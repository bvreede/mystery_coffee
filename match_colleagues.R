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

# remove the week from six weeks ago
existing <- existing %>%
  select(-starts_with("22_Match"))


# select participants for this week
participants = df %>% filter(!is.na(Week_29)) %>% pull(Name)

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

matches <- as.data.frame(matches)
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


# add matches to the previous matches document
first <- matches %>%
  rename(Colleague = first,
         match = second)

second <- matches %>%
  rename(Colleague = second,
         match = first)

matches_for_existing <- bind_rows(first,second) %>%
  rename(`29_Match1` = match)

# TODO: if there is a three-person match, the third needs to be added
# if(ncol(matches) > 2){
#   
# }


existing <- full_join(existing, matches_for_existing, by="Colleague")

write_csv(existing, "matches/allmatches.csv")
