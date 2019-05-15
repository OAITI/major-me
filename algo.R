# Simulated flag
simulated <- TRUE

# load libraries
library(tidyverse)

# read the data
#qs <- read_csv("data/s_q_demo.csv")
#qs <- read_csv("data/algonquin.csv")
qs <- read_csv("data/major_qs_data.csv")

# rename the first column
qs_algo <- qs %>%
    rename(Major = 1)

# find deviations from mean of observations for each question
qs_dev <- qs_algo %>%
    mutate_if(is.numeric, function(x) {
        abs(x - mean(x, na.rm = TRUE))
    })

# Initialize
# first question is the one with max deviation(most distinguishable)
cur_qs <- qs_dev %>%
    select(-Major) %>%
    select(max.col(.)) %>% # selects columns with rowmax s(doing this to make subset so it's faster?)
    filter_all(any_vars(. == !!max(.))) %>%
    select(max.col(.)) %>%
    select(1) %>%
    colnames()
dist_calc <- qs_algo %>% select(Major) %>% mutate(Distance = 0)
rank1 <- tibble()

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
        user <- as.numeric(readline(prompt = "Enter answer: ") )
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
        select(Major)
    
    # store as rank 1 major
    rank1 <- rank1 %>%
        bind_rows(min_dist_majors)
    
    # remove completed qs
    qs_algo <- qs_algo %>%
        select(-cur_qs)
    
    # remove completed qs
    qs_dev <- qs_dev %>%
        select(-cur_qs)
    
    # terminate if done with questions
    if (ncol(qs_dev) == 1) { # just has major column
        mymajor <- dist_calc$Major[which.min(dist_calc$Distance)]

        if (!simulated) print(paste0("Your major is: ", mymajor))
        
        final <- list(Major = mymajor, Vector = all_answers, Questions = all_questions)
        
        break;
    }
    
    # Convergence criteria
    # The highest ranked school does not change after 5 iterations
    # A school remains in the top ranked group after 5 iterations
    # There is only one school in the rank 1 group
    # All questions are answered
    if(i >= 5) {
        check <- rank1 %>%
            count(Major, sort = TRUE) %>%
            slice(1) 
        if (check$n > 4) {
            if (!simulated) print(paste0("Your major is: ", check$Major))
            
            final <- list(Major = check$Major, Vector = all_answers, Questions = all_questions)
            
            break;
        } 
    }
    
    # 2. max deviation questions  - What if there are ties in this step, choosing first
    max_dist_qs <- qs_dev %>%
        filter(Major %in% min_dist_majors$Major) %>%
        select(-Major) %>%
        select(max.col(.)) %>% # selects columns with rowmax s(doing this to make subset so it's faster?)
        filter_all(any_vars(. == !!max(.))) %>%
        select(max.col(.))
    
    # choose 1 in case there are many, this becomes cur_qs
    cur_qs <- max_dist_qs %>%
        select(1) %>%
        colnames()
}

