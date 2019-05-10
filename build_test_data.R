library(tidyverse)
library(readxl)

# algonquin.csv has sample question-answers from different fields and we want to check 
# whether the means match the means given by someone initialmeanvectors.xlsx. 
algo <- read_csv("data/input_data/algonquin_orig.csv") # specific occupation to question
qs <- read_csv("data/input_data/questions.csv") %>%
    filter(!is.na(Question)) # qs to Traits
means <- read_excel("data/input_data/InitialMeanVectors.xlsx") %>%
    filter(!is.na(`...1`)) %>% # Broad Major to Traits(mean vectors)
    mutate_if(is.numeric, function(x){
        x-3
    })
mapping <- read_csv("data/input_data/major_mapping.csv") # specific occupation to broad major

# answers to 1-5 level in tall format
algo_tall <- algo %>%
    rename(Major = X1) %>%
    gather(key = Question, value = Answer, 2:ncol(.)) %>%
    mutate(Answer = factor(Answer, levels = c("Strongly Disagree", "Disagree", "Sometimes", "Agree", "Strongly Agree"))) %>%
    filter(!is.na(Answer)) %>%
    mutate(AnswerNum = as.numeric(Answer) -3)
length(unique(qs$Question))
length(unique(algo_tall$Question)) # only has a sample of qs and occupations

# add the Traits to qs-ans data
algo_join <- algo_tall %>%
    left_join(qs) %>%
    gather(key = Trait, value = Value, 5:ncol(.)) %>%
    mutate(Value = AnswerNum * Value)
# find average trait value for the major
algo_group <- algo_join %>%
    group_by(Major, Trait) %>%
    summarise(Value = mean(Value, na.rm = TRUE))

# map majors to broad major by averaging Trait
algo_map <- algo_join %>%
    left_join(mapping, by = c("Major" = "Specific Major")) %>%
    group_by(`Broad Major`, Trait) %>%
    summarise(Value = mean(Value, na.rm = TRUE))

# Now, get the other data in same format (it is already just need to gather)
means_clean <- means %>%
    rename(`Broad Major` = `...1`) %>%
    gather(key = Trait, value = Value, 2:ncol(.))
# join both data to compare
joined <- algo_map %>%
    left_join(means_clean %>% rename(Expected = Value)) %>%
    ungroup() %>%
    rename(Actual = Value) %>%
    mutate(Difference = Actual - Expected) %>%
    filter(!is.na(Expected))

ggplot(data = joined, aes(x = abs(Difference))) +
    geom_histogram()

## Output
final <- joined %>%
    select(`Broad Major`, Trait, Expected) %>%
    spread(key = Trait, value = Expected)

write_csv(final, "data/algonquin.csv")


## Combining Questions to Traits and Traits to Broad Majors data, to form a 
# Broad Majors to questions dataset, which will be our test input

question_mapping <- qs %>%
    gather(key = Trait, value = Value, 2:ncol(.)) %>%
    left_join(
        means %>%
            rename(Major = `...1`) %>%
            gather(key = Trait, value = MeanValue, 2:ncol(.)), by = "Trait"
    ) %>%
    mutate(Value = Value * MeanValue) %>% # get one value for Major for each question
    group_by(Question, Major) %>%
    summarise(Value = mean(Value, na.rm = TRUE)) %>%
    filter(!is.na(Question), !is.na(Major), !is.na(Value)) %>%
    ungroup() %>%
    spread(key = Question, value = Value)

write_csv(question_mapping, "data/major_qs_data.csv")


