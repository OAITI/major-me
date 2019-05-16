## Distribution of majors, average number of questions answered by major
## distribution of questions to see which ones are being asked most and least
library(tidyverse)

load("result.RData")
trait_data <- read_csv("../data/input_data/questions.csv") %>%
    filter(!is.na(Question)) %>%
    gather(key = Construct, value, 2:ncol(.), na.rm = TRUE) %>%
    # replace 1 with trait name and -1 with not
    mutate(Trait = ifelse(value == 1, Construct, paste0("Not ", Construct)))

maj <- result %>%
    count(Major, sort = TRUE) 
ggplot(maj, aes(x = forcats::fct_reorder(Major, n, .desc = TRUE), y = n)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(hjust = 1, angle = 20)) +
    labs(title = "Distribution of Majors",
         x = "")
# questions

qs <- result %>%
    unnest(Questions) %>%
    count(Questions, sort = TRUE) %>%
    # top and bottom
    slice(c(1:10, (nrow(.)-10):nrow(.)))
    
ggplot(qs, aes(x = forcats::fct_reorder(Questions, n), y = n)) +
    geom_bar(stat = "identity") + 
    coord_flip() +
    labs(title = "Top and Bottom 10 Questions by Frequency",
         x = "")

# Table of Q and A for least common Majors

#least common 
lc_maj <- maj %>%
    arrange(n) %>%
    slice(1:5)

qs_ans <- result %>%
    filter(Major %in% lc_maj$Major) %>%
    group_by(Major) %>%
    mutate(Trial = seq(1:length(Major))) %>%
    arrange(Major) %>%
    unnest() %>%
    group_by(Trial) %>%
    mutate(QNo = seq(1:length(Questions))) 
