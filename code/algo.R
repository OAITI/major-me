# load libraries
library(tidyverse)

## Utility functions
source("functions.R")

# Parameters for the script
if (!exists("simulated")) simulated <- FALSE
if (!exists("random_start")) random_start <- "Trait Optimized"
if (!exists("game_mode")) game_mode <- "Reduced"

# read the data
qs <- read_csv("../data/major_qs_data.csv")

# rename the first column
qs_algo <- qs %>%
    rename(Major = 1)

# find deviations from mean of observations for each question
qs_dev <- qs_algo %>%
    mutate_if(is.numeric, function(x) {
        abs(x - mean(x, na.rm = TRUE))
    })

# data on the traits
trait_data <- read_csv("../data/input_data/questions.csv") %>%
    filter(!is.na(Question)) %>%
    gather(key = Construct, value, 2:ncol(.), na.rm = TRUE) %>%
    # replace 1 with trait name and -1 with not
    mutate(Trait = ifelse(value == 1, Construct, paste0("Not ", Construct)))

# Initialize
# first question is the one with max deviation(most distinguishable)
cur_qs <- get_next_question(qs_dev, trait_data, start = random_start)
dist_calc <- qs_algo %>% select(Major) %>% mutate(Distance = 0)
rank1 <- tibble()

# store the questions and answers
all_answers <- NULL
all_questions <- NULL

# Serve Question and get response
for(i in 1:(ncol(qs_algo) - 1)) {
    
    # print current question
    if (!simulated) print(cur_qs)
    
    # provide the score
    if (simulated) {
        user <- sample(-2:2, size = 1)
    } else {
        user <- as.numeric(readline(prompt = "Enter answer (-2 to 2): ") )
    }
    
    all_answers <- c(all_answers, user)
    all_questions <- c(all_questions, cur_qs)

    # Iteration 1
    # 1. Find min distance observation for Q1
    # 2. For that observation, find the max deviated question and ask it next
    # 1. min dist observations
    dist_calc <- dist_calc %>%
        mutate(Distance = Distance + abs(qs_algo[[cur_qs]] - user)) 
    
    # get majors with the smallest distance now
    min_dist_majors <- dist_calc %>%
        filter(Distance == min(Distance)) %>%
        select(Major) %>%
        mutate(Iteration = i)
    
    # get majors responding identically to that question
    prev_majors <- dist_calc %>%
        mutate(Distance = abs(qs_algo[[cur_qs]] - user)) %>%
        filter(Distance == min(Distance)) %>%
        .$Major
    
    # store as rank 1 major
    rank1 <- rank1 %>%
        bind_rows(min_dist_majors)
    
    # remove completed qs
    qs_algo <- qs_algo %>%
        select(-cur_qs)
    
    # remove completed qs
    qs_dev <- qs_dev %>%
        select(-cur_qs)

    # Convergence criteria
    term_criteria <- FALSE
    if(i >= 5) {
        check <- rank1 %>%
            filter(Iteration %in% (i-4):i) %>%
            count(Major, sort = TRUE) %>%
            filter(n == 5)
        
        # Either 5 consecutive min dist, or there's no more questions
        term_criteria <- ncol(qs_dev) == 1
        if (game_mode == "Reduced") term_criteria <- term_criteria || (nrow(check) == 1 && nrow(min_dist_majors) == 1)
        
        if (term_criteria) {
            mymajor <- dist_calc$Major[which.min(dist_calc$Distance)]

            if (!simulated) print(paste0("Your major is: ", mymajor))
            
            final <- list(Major = mymajor, Vector = all_answers, Questions = all_questions)
            
            break;
        } 
    }
    
    if (!term_criteria) {
        cur_qs <- get_next_question(qs_dev, start = "Optimized", major_list = prev_majors)
    }
}

