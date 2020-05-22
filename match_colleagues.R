library(tidyverse)

# read in the data
df <- read_csv("data/testdata.csv")

# make the division
how_many <- as.integer(nrow(df)/2)
first_set <- sample(df$Name, how_many)
left <- df %>% filter(!Name %in% first_set)
second_set <- sample(left$Name, how_many)
left <- left %>% filter(!Name %in% second_set)
matches <- tibble(Colleague_1 = first_set, Colleague_2 = second_set)

# add a third colleague in case of an uneven number
if(nrow(left) > 0){
  third_set <- c(rep(NA, how_many-nrow(left)), left$Name)
  matches$Colleague_3 <- sample(third_set, how_many)
}