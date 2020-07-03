library(tidyverse)

previous_matches <- function(name){
  previous <- existing %>% 
    filter(Colleague == name) %>%
    unlist() %>%
    unname()
  previous <- previous[!is.na(previous)]
  return(previous)
}

remove_previous <- function(name, participants){
  previous <- previous_matches(name)
  available <- participants[!participants %in% c(name,previous)]
  if(length(available) == 0){
    available <- participants[participants != name]
    warning("No available matches for ", name, ". Removing condition.")
  }
  return(available)
}


# read in the data
df <- read_csv("data/collegas_continued.csv")
existing <- read_csv("matches/allmatches.csv")

# select participants for this week
participants = df %>% filter(!is.na(Week_28)) %>% pull(Name)

# make the data frame to fill in
matches <- NULL


# make the division
while(length(participants) > 1){
  # draw name 1
  name <- base::sample(participants, 1)
  # get available matches
  available <- remove_previous(name, participants)
  # select match
  match <- base::sample(available, 1)
  # remove from participants
  participants <- participants[!participants %in% c(name,match)]
  # save to df
  matches <- rbind(matches, c(name,match))
}

names(matches) <- c("first", "second") # why does this not survive as_tibble?
matches <- as_tibble(matches)
names(matches) <- c("first", "second")

# last match
if(length(participants > 0)){
name <- participants[1]
previous <- previous_matches(name)

matches <- matches %>%
  mutate(
    third = case_when(!first %in% previous & !second %in% previous ~ name),
    third = case_when(!duplicated(third) ~ third)
  )
}

write_csv(matches, paste0("matches/matches_",lubridate::today(),".csv"))
