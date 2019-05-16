library(tidyverse)
library(lubridate)
library(ggrepel)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(bsplus)

## Utility functions
source("code/functions.R")

## Set images resource path
addResourcePath("images", "images")

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
qs_useralgo <- qs_data %>%
    rbind(c("My Major", colMeans(.[,-1]))) # in the beginning user answrs are set to average of all majors


ui <- fluidPage(theme = shinytheme("cerulean"),
                
    useShinyjs(),
    use_bs_popover(),
                
    includeCSS("css/styles.css"),

    titlePanel("Major Me"),
    
    wellPanel(
        fluidRow(
            tabsetPanel(id = "tabs1",
                tabPanel("Quiz",
                         column(width = 10,
                                h4("About"),
                                HTML("Welcome to the major me matching application. The algorithm suggests a list of college majors after every question, converging when there is a consistent major found in the list or all the questions are exhausted."),
                                HTML("Follow along the diagnostics see the quick match results!")
                         ),
                         column(width = 2,
                                div(style = "text-align: right", a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")))
                         ),
                         
                         hr(),
                         fluidRow(
                             column(width = 2,
                                    div(style = "padding-left: 15px", actionButton("goButton", "Take the Quiz", icon = icon("play-circle")))
                             ),
                             column(width = 3,
                                    checkboxInput("advanced", "Show Advanced Options")
                             ),
                             column(width = 3,
                                    conditionalPanel(condition = "input.advanced",
                                                     selectInput("mode", "Survey Mode", choices = c("Reduced", "Complete")) %>%
                                                         shinyInput_label_embed(
                                                             shiny_iconlink() %>%
                                                                 bs_embed_popover(
                                                                     title = "Survey Mode", content = "Reduced (default) will intelligently select questions and terminate when a major is identified. Complete will ask all 61 questions in our database.", placement = "left"
                                                                 )
                                                         )
                                    )
                             ),
                             column(width = 3,
                                    conditionalPanel(condition = "input.advanced",
                                                     selectInput("start", "Survey Start", choices = c("Trait Optimized", "Optimized", "Random")) %>%
                                                         shinyInput_label_embed(
                                                             shiny_iconlink() %>%
                                                                 bs_embed_popover(
                                                                     title = "Survey Mode", content = "Trait Optimized (default) will randomly choose a question that most has the most deviation between majors within a particular trait or construct. Optimized will ignore traits and constructs and choose the highest deviation question overall. Random will randomly select the first question from the full list.", placement = "left"
                                                                 )
                                                         )
                                    )
                             )
                         )
                ),
                tabPanel("Information",
                         column(width = 10,
                                h4("Purpose"),
                                HTML("Though surveys and quizzes can do a great job at matching individuals to a particular skill, major, or career, one issue with them is the length. A good survey has a large variety of questions, but users may get bored or disinterested before completion of a large survey.<br><br>
                                      
                                      This application implemented a <b>reduced questionnaire</b> algorithm to intelligently select questions, and speed the process of matching to a major. While the Complete survey is over 60 questions, a typical reduced version will be 5-10 questions before termination."),
                                h4("Directions"),
                                HTML("<ol><li>Read the information and disclaimers on the quiz</li>
                                          <li>On the Quiz tab, click 'Take the Quiz'</li>
                                          <li>Answer the question and click 'Submit'</li>
                                          <li>Diagnostics will appear in the right-hand panel. Answer the next question to continue</li>
                                          <li>When the algorithm terminates, you will have the option to submit your real major to help us improve the results</li></ol>"),
                                h4("Disclaimer"),
                                HTML("This quiz and algorithm is provided 'as-is' and we make no guarantees about the accuracy or performance. By submitting your real major, you consent to have your anonymized data used for purposes of improving this algorithm.")
                         ),
                         column(width = 2,
                                div(style = "text-align: right", a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")))
                         )
                )
            )
        )
    ),

    conditionalPanel(condition = "input.tabs1 == 'Quiz'",
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
)

# Define server logic required 
server <- function(input, output, session) {
    
    ## Reactive Values to hold User Data
    user <- reactiveValues(if_finish_quiz = NULL, user_response = NULL, pred_label = NULL,
                           pred_others = NULL, feedback_submitted = FALSE)
    
    ## Reactive Values for Input
    data <- reactiveValues(qs = NULL, qs_dev = NULL)
    
    ## Reactive Values to track for next steps/convergence
    track <- reactiveValues(cur_qs = NULL, selected_qs = NULL, selected_ans = NULL, dist_calc = NULL,
                            iter = numeric(), rank1 = NULL, user_algo = NULL, prev_qs = NULL)
    
    ## Read data and Initialize when Take Quiz is clicked
    observeEvent(input$goButton, {
        
        # Disable take quiz button
        shinyjs::enable("submit_actual")
        shinyjs::disable("goButton")
        shinyjs::hide("mode")
        shinyjs::hide("start")

        # Copy the data as a reactive
        data$qs <- qs_data
        # find deviations from mean of observations for each question
        data$qs_dev <- dev_data
        
        # Initialize/Set all vars to default
        user$user <- tempfile(pattern = "User", tmpdir = "")
        user$if_finish_quiz <- FALSE
        user$user_response <- numeric()
        user$feedback_submitted <- FALSE

        # Set all the track variables including starting question
        track$cur_qs <- get_next_question(data$qs_dev, trait_data, start = input$start)
        track$dist_calc <- data$qs %>% select(Major) %>% mutate(Distance = 0)
        track$rank1 <- tibble()
        track$iter <- 0
        track$selected_qs <- NULL
        track$selected_ans <- NULL
        track$prev_qs <- NULL
        track$user_algo <- qs_useralgo
    })
    
    output$quiz_ui <- renderUI({
        ## If start and ask questions
        if (is.null(user$if_finish_quiz)) {
            list(
                h4(paste0("Click 'Take the Quiz' to begin!"))
            )
        } else if (!user$if_finish_quiz) {
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
            shinyjs::enable("goButton")
            shinyjs::show("mode")
            shinyjs::show("start")
            
            list(
                HTML(paste0("<h3>", "Your Major is <b>", user$pred_label, "</b></h3>")),
                hr(),
                HTML(paste0("<h4>", "Other Possibilities are <b>", paste(user$pred_others, collapse = " and "), "</b></h3>")),
                hr(),
                selectInput("actual", "Actual Major", choices = qs_data$Major, selected = NULL),
                actionButton("submit_actual", "Submit Feedback"),
                textOutput("feedback")
            )
        }
    })
    
    output$feedback <- renderText({ 
        if (!user$feedback_submitted) return(NULL)
        
        "Thanks for submitting feedback! If you'd like to take the quiz again, just click Take the Quiz above!" 
    })
    
    observeEvent(input$submit_actual, {
        shinyjs::disable("submit_actual")
        
        load("data/feedback.RData")

        mytbl <- tibble(
            Time = now(),
            Predicted = user$pred_label,
            Actual = input$actual,
            Vector = list(track$selected_ans),
            Questions = list(track$selected_qs)
        )
        
        feedback <- feedback %>%
            rbind(mytbl)
        
        save(feedback, file = "data/feedback.RData")
        
        user$feedback_submitted <- TRUE
    })
    
    # Submit answer - find minmajors, update tracking vars, data vars, user vars, and mdsplot 
    observeEvent(input$ansButton,{
        user$user_response <- as.numeric(input$ans_choice)
        track$selected_ans <- c(track$selected_ans, user$user_response)
        
        # Store the previous questions
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
        
        # Convergence criteria
        term_criteria <- FALSE
        if(track$iter >= 5) {
            check <- track$rank1 %>%
                count(Major, sort = TRUE) %>%
                slice(1) 
            
            # Either 5 consecutive min dist, or there's no more questions
            term_criteria <- ncol(data$qs_dev) == 1
            if (input$mode == "Reduced") term_criteria <- term_criteria || check$n > 4
            
            if (term_criteria) {
                #mymajor <- track$dist_calc$Major[which.min(track$dist_calc$Distance)]
                mymajors <- track$dist_calc$Major[order(track$dist_calc$Distance)][1:3]
                user$pred_label <- mymajors[1]
                user$pred_others <- mymajors[2:3]
                user$if_finish_quiz <- TRUE
            } 
        }
        
        # add cur qs to selected
        track$selected_qs <- c(track$selected_qs, track$cur_qs)
        
        if (!term_criteria) {
            
            # 2. max deviation questions  - What if there are ties in this step, choosing first
            # choose 1 in case there are many, this becomes cur_qs
            track$cur_qs <- get_next_question(data$qs_dev, start = "Optimized", major_list = min_dist_majors$Major)
            
            # Increase the iteration
            track$iter <- track$iter + 1
        }
    })
    
    qs_trait <- reactive({
        if(!user$if_finish_quiz) {
            trait_data %>%
                filter(Question == track$cur_qs) %>%
                select(Construct)
        }})
    
    output$stat_ui <- renderUI({
        if (is.null(user$if_finish_quiz)) return(NULL)
        
        list(
            h4("Questions Answered: ", length(track$selected_qs)),
            h4("Trait Measured by current Question: ", qs_trait()),
            hr(),
            h4("Map of Possible Majors (based on previous answer)"),
            withSpinner(plotOutput("mdsplot")),
            hr(),
            h4(paste0("Top 5 Majors After Question ", track$iter)),
            withSpinner(tableOutput("top5"))
        )
    })
    
    output$top5 <- renderTable({
        if (track$iter == 0) return(NULL)
            
        tibble(
            Rank = paste0("#", 1:5),
            Major = track$dist_calc$Major[order(track$dist_calc$Distance)][1:5]
        )
    })
    
    plotdata <- reactive({
        req(user$user_response)
        
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
        ggplot(plotdata(), aes(x, y)) +
            geom_point(color = '#F78E24') +
            geom_point(data = plotdata() %>% filter(Major == "My Major"), size = 3, color = "red4") +
            geom_text_repel(aes(label = Major)) +
            theme_minimal(base_size = 16) +
            theme(axis.text = element_text(color = "#aaaaaa"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
