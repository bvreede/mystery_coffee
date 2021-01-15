library(tidyverse)

# running variable: use "test" for test data, "real" for real data, if present
running <- "test" # "real"

# week number
weekno <- 3


  



###### FUNCTIONS ######
load_user_data <- function(type = "real"){
  url_test <- "https://docs.google.com/spreadsheets/d/1I-8IG0FqaIGwx7ZVEbbTXzshXbvJUM6kOHk2y3eA528/"
  url_real <- ""
    if(type == "real"){
      df <- read_sheet(url_real)
    }else if(type == "test"){
      df <- read_sheet(url_test)  
    }else{
      stop("No valid option given (function load_user_data()).")
    }
  return(df)
}

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
    stop("No available matches for ", name, ".")
  }
  return(available)
}


divide_participants <- function(participants){
  # make the data frame to fill in
  matchdata <- NULL
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
    matchdata <- rbind(matchdata, c(name,match))
  }
  
  matchdata <- as.data.frame(matchdata)
  names(matchdata) <- c("Collega_1", "Collega_2")
  
  # last match
  if(length(participants > 0)){
    name <- participants[1]
    previous <- previous_matches(name)
    
    matchdata <- matchdata %>%
      mutate(
        Collega_3 = case_when(!Collega_1 %in% previous & !Collega_2 %in% previous ~ name),
        Collega_3 = case_when(!duplicated(Collega_3) ~ Collega_3)
      )
    # if the last match did not find a good spot...
    if(!name%in%matchdata$Collega_3){
      stop(paste0("Could not find a good group for last remaining participant, ", name, "."))
    }
  }
  return(matchdata)
}


###### PIPELINE ######

# read in the data
df <- load_user_data(type = running)

if(running == "test"){
  cat(paste0("Running the colleague matching script with TEST data for week ",weekno,".\n"))
  existing <- read_csv("data/testdata_matches.csv")
} else if(running == "real"){
  cat(paste0("Running the colleague matching script with REAL data for week ",weekno,".\n"))
  existing <- read_csv("data/realdata_matches.csv")
} else{
  stop("Make a choice between 'real' and 'test' in the variable 'running'.")
}



# select participants for this week
participants <- df %>% 
  select(Name, contains(as.character(weekno)))
participants <- participants[,"Name"][!is.na(participants[,2])] %>% unname()

matchdata <- NULL
while(is.null(matchdata)){
  tryCatch({matchdata <- divide_participants(participants)},
          error = function(e){print(e)})
}

# make directory if it does not yet exists
if(!dir.exists("matches")){
  dir.create("matches")
}
write_csv(matchdata, paste0("matches/matches_",lubridate::today(),".csv"))


# this week's match name
thisweek1 <- paste0(weekno,"_Match1")
thisweek2 <- paste0(weekno,"_Match2")

# add matches to the previous matches document
Collega_1 <- matchdata %>%
  rename(Colleague = Collega_1,
         match = Collega_2)

Collega_2 <- matchdata %>%
  rename(Colleague = Collega_2,
         match = Collega_1)

matches_for_existing <- bind_rows(Collega_1,Collega_2) %>%
  rename(!!thisweek1 := match)

# if there is one set of three, a second column needs to be added
if(ncol(matchdata) > 2){
  matches_for_existing <- matches_for_existing %>%
    rename(!!thisweek2 := Collega_3)
  
  third_match <- matchdata %>%
    filter(!is.na(Collega_3)) %>%
    rename(Colleague = Collega_3,
           !!thisweek1 := Collega_1,
           !!thisweek2 := Collega_2)
  
  matches_for_existing <- bind_rows(matches_for_existing,third_match)
}

existing <- full_join(existing, matches_for_existing, by="Colleague")

# save the matches to 'existing matches' data
if(running == "test"){
  cat(paste0("Saving the matches with TEST data."))
  write_csv(existing, "data/testdata_matches.csv")
} else if(running == "real"){
  cat(paste0("Saving the matches with REAL data."))
  write_csv(existing, "data/realdata_matches.csv")
} else{
  stop("Make a choice between 'real' and 'test' in the variable 'running'.")
}
