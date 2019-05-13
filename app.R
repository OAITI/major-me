library(tidyverse)
library(shiny)
library(ggrepel)
library(shinycssloaders)

ui <- fluidPage(

    titlePanel("Smart Match"),
    
    wellPanel(
        #a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
        HTML("Welcome to the smart major matching application. The algorithm suggests a list of college majors after every question, converging when there is a consistent major found in the list or all the questions are exhausted."),
        HTML("Follow along the diagnostics see the quick match results!"),
        hr(),
        actionButton("goButton", "Take the Quiz", icon = icon("play-circle"))
    ),

    fluidRow(
        column(7,
               h3("Quiz"),
               div(style ="border-right:1px solid grey;height:700px;", 
                   uiOutput("quiz_ui"))
                ),
        column(5,
               h3("Diagnostics"), 
               uiOutput("stat_ui")
               )
        )
)

qs_data <- read_csv("data/major_qs_data.csv") %>%
    rename(Major = 1) # rename the first column
dev_data <- qs_data %>%
    mutate_if(is.numeric, function(x) {
        abs(x - mean(x, na.rm = TRUE))
    })
trait_data <- read_csv("data/input_data/questions.csv") %>%
    filter(!is.na(Question)) %>%
    gather(key = Construct, value, 2:ncol(.), na.rm = TRUE) %>%
    # replace 1 with trait name and -1 with not
    mutate(Trait = ifelse(value == 1, Construct, paste0("Not ", Construct)))
# for mds plot
major_avg <- colMeans(qs_data[,-1])
qs_useralgo <- qs_data %>%
    rbind(c("My Major", major_avg)) # in the beginning user answrs are set to average of all majors

# Define server logic required 
server <- function(input, output, session) {
    
    ## Reactive Values to hold User Data
    user <- reactiveValues(user = NULL, if_finish_quiz = FALSE, 
                           user_response = numeric(), pred_label = NULL)
    ## Reactive Values for Input
    data <- reactiveValues(qs = NULL, qs_dev = NULL)
    ## Reactive Values to track for next steps/convergence
    track <- reactiveValues(cur_qs = NULL, selected_qs = NULL, dist_calc = NULL,
                            iter = numeric(), rank1 = NULL, user_algo = NULL, prev_qs = NULL)
    
    ## Read data and Initialize when Take Quiz is clicked
    observeEvent(input$goButton, {
        data$qs <- qs_data
        # find deviations from mean of observations for each question
        data$qs_dev <- dev_data
        # Initialize/Set all vars to default
        user$user <- tempfile(pattern = "User", tmpdir = "")
        user$if_finish_quiz <- FALSE
        user$user_response <- numeric()
        # first question is the one with max deviation (most distinguishable)
        track$cur_qs <- data$qs_dev %>%
            select(-Major) %>%
            select(max.col(.)) %>% # selects columns with rowmax s(doing this to make subset so it's faster?)
            filter_all(any_vars(. == !!max(.))) %>%
            select(max.col(.)) %>%
            select(1) %>%
            colnames()
        track$dist_calc <- data$qs %>% select(Major) %>% mutate(Distance = 0)
        track$rank1 <- tibble()
        track$iter <- 0
        track$selected_qs <- NULL
        track$prev_qs <- NULL
        track$user_algo <- qs_useralgo
    })
    
    output$quiz_ui <- renderUI({
        ## If start and ask questions
        if(!is.null(user$user) & !user$if_finish_quiz) {
            list(
                ## Question UI
                HTML(paste0("<h3>", track$cur_qs, "</h3>")),
                radioButtons("ans_choice", label="", selected = 0,
                             inline = FALSE,
                             width="400px", 
                             choices = c("Strongly Agree" = 2,"Agree" = 1,"Sometimes" = 0,
                                         "Disagree" = -1,"Strongly Disagree" = -2)),
                actionButton("ansButton", label = "Submit")
            )
        } else if (user$if_finish_quiz) {
                list(
                    HTML(paste0("<h3>", "Your Major is <b>", user$pred_label, "</b></h3>"))
                )
            }
        else {
            list(
                h4(paste0("Ready.."))
            )
        }
    })
    
    # Submit answer - find minmajors, update tracking vars, data vars, user vars, and mdsplot 
    observeEvent(input$ansButton,{
        user$user_response <- as.numeric(input$ans_choice)
        track$prev_qs <- track$cur_qs
        # Algo steps to update vars
        track$dist_calc <- track$dist_calc %>%
            mutate(Distance = Distance + abs(data$qs[[track$cur_qs]] - user$user_response)) 
        
        # get majors with the smallest distance now
        min_dist_majors <- track$dist_calc %>%
            filter(Distance == min(Distance)) %>%
            select(Major)
        # store them as rank 1 majors
        track$rank1 <- track$rank1 %>%
            bind_rows(min_dist_majors)
        cq <- track$cur_qs
        # remove completed qs
        data$qs <- data$qs %>%
            select(-cq)
        data$qs_dev <- data$qs_dev %>%
            select(-cq)

        # terminate if done with questions
        if (ncol(data$qs_dev) == 1) { # just has major column
            mymajor <- track$dist_calc$Major[which.min(track$dist_calc$Distance)]
            user$pred_label <- mymajor
            user$if_finish_quiz <- TRUE
        }
        # Convergence criteria
        if(track$iter >= 5) {
            check <- track$rank1 %>%
                count(Major, sort = TRUE) %>%
                slice(1) 
            if (check$n > 4) {
                user$pred_label <- check$Major
                user$if_finish_quiz <- TRUE
            } 
        }
        # 2. max deviation questions  - What if there are ties in this step, choosing first
        max_dist_qs <- data$qs_dev %>%
            filter(Major %in% min_dist_majors$Major) %>%
            select(-Major) %>%
            select(max.col(.)) %>% # selects columns with rowmax s(doing this to make subset so it's faster?)
            filter_all(any_vars(. == !!max(.))) %>%
            select(max.col(.))
        # add cur qs to selected
        track$selected_qs <- c(track$selected_qs, track$cur_qs)
        # choose 1 in case there are many, this becomes cur_qs
        track$cur_qs <- max_dist_qs %>%
            select(1) %>%
            colnames()
        track$iter <- track$iter + 1
    })
    qs_trait <- reactive({
        if(!is.null(user$user) & !user$if_finish_quiz) {
            trait_data %>%
                filter(Question == track$cur_qs) %>%
                select(Construct)
        }})
    output$stat_ui <- renderUI({
        list(
            h4("Trait Measured by current Question: ", qs_trait()),
            hr(),
            h4("Map of Possible Majors (based on previous answer)"),
            withSpinner(plotOutput("mdsplot"))
        )
    })
    plotdata <- reactive({
        d <- dist(track$user_algo[,-1])
        if(!is.null(track$selected_qs)){
            # if the user has answered qs, update the answer
            track$user_algo[track$user_algo$Major == "My Major", track$prev_qs] <- user$user_response
            # take distance between selected qs vectors and mymajor for mds
            d <- dist(track$user_algo[,track$selected_qs, drop = FALSE])
        }
        fit <- cmdscale(d, eig=TRUE, k=2)
        
        # plot data 
        data.frame(track$user_algo[,1], x = fit$points[,1], y = fit$points[,2])
    })
    output$mdsplot <- renderPlot({
        if(!is.null(user$user)) {
            ggplot(plotdata(), aes(x, y)) +
                geom_point(color = '#F78E24') +
                geom_point(data = plotdata() %>% filter(Major == "My Major"), size = 3, color = "red4") +
                geom_text_repel(aes(label = Major)) +
                theme_minimal(base_size = 16) +
                theme(axis.text = element_text(color = "#aaaaaa"))
            }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
