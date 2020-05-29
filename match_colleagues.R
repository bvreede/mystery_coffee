library(tidyverse)

# read in the data
df <- read_csv("data/collegas_continued.csv")

# select participants for this week
participants = df %>% filter(!is.na(Week_23)) %>% pull(Name)

# make the division
how_many <- as.integer(length(participants)/2)
first_set <- sample(participants, how_many)
participants <- participants[!participants %in% first_set]
second_set <- sample(participants, how_many)
participants <- participants[!participants %in% second_set]
matches <- tibble(Collega_1 = first_set, Collega_2 = second_set)

# add a third colleague in case of an uneven number
if(length(participants) > 0){
  third_set <- c(rep(NA, how_many-length(participants)), participants)
  matches$Colleague_3 <- sample(third_set, how_many)
}

write_csv(matches, paste0("matches/matches_",lubridate::today(),".csv"))
