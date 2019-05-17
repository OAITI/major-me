get_next_question <- function(deviation_data, trait_data = NULL, start = "Trait Optimized", major_list = unique(deviation_data$Major)) {
    
    if (start == "Trait Optimized") {
        # first question is the one in a random trait with the most deviation
        cur_qs <- deviation_data %>%
            gather(key = Question, value = Value, 2:ncol(.)) %>%
            left_join(trait_data) %>%
            group_by(Construct) %>%
            summarise(Deviation = max(Value),
                      Question = Question[which.max(Value)]) %>%
            sample_n(1) %>%
            .$Question
    } else if (start == "Optimized") {
        # first question is the one with max deviation (most distinguishable)
        cur_qs <- deviation_data %>%
            filter(Major %in% major_list) %>%
            gather(key = Question, value = Value, 2:ncol(.)) %>%
            group_by(Question) %>%
            summarise(Deviation = max(abs(diff(Value)))) %>%
            filter(Deviation == max(Deviation)) %>%
            sample_n(1) %>%
            .[[1]]
    } else if (start == "Random") {
        cur_qs <- sample(names(deviation_data)[-1], size = 1)
    }
    
    return(cur_qs)
}
