library(tidyverse)
library(readxl)

qs <- read_csv("../data/input_data/questions.csv") %>%
    filter(!is.na(Question)) # qs to Traits
means <- read_excel("../data/input_data/UpdatedMeanVectors.xlsx", sheet = 3)

## Combining Questions to Traits and Traits to Broad Majors data, to form a 
# Broad Majors to questions dataset, which will be our test input
question_mapping <- qs %>%
    gather(key = Trait, value = Value, 2:ncol(.)) %>%
    left_join(
        means %>%
            gather(key = Trait, value = MeanValue, 2:ncol(.)), by = "Trait"
    ) %>%
    mutate(Value = Value * MeanValue) %>% # get one value for Major for each question
    group_by(Question, Major) %>%
    summarise(Value = mean(Value, na.rm = TRUE)) %>%
    filter(!is.na(Question), !is.na(Major), !is.na(Value)) %>%
    ungroup() %>%
    spread(key = Question, value = Value)

write_csv(question_mapping, "../data/major_qs_data.csv")
