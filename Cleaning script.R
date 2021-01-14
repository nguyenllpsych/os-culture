##############################################################
##    Open Science in Ethnic Minority/Cultural Psychology   ##
##                    Cleaning Script                       ##
##                                                          ##
## Linh Nguyen - nguy4006@umn.edu                           ##
##                                                          ##
## Created: 13-Jan-2021                                     ##
## Updated: 13-Jan-2021                                     ##
## To navigate: Edit - Folder - Collapse All                ##
##############################################################

# META =======================================================
# > Libraries ----

library("groundhog") #package version control
libDate <- "2021-01-01" #dates for package versions

groundhog.library("tidyverse", libDate) #general data wrangling
groundhog.library("codebook", libDate) #code book cleaning functions 
groundhog.library("rio", libDate) #import export files
groundhog.library("future", libDate) #reliability
groundhog.library("ufs", libDate) #reliability
groundhog.library("GGally", libDate) #reliability
groundhog.library("GPArotation", libDate) #reliability

# > Data ----
dict <- import("Dictionary.xlsx") #dictionary

names <- read_csv("./Raw data/Raw data.csv") %>% #change working directory as needed
  names()

data <- read_csv("./Raw data/Raw data.csv",
                 col_names = names,
                 skip = 3, #skip 3 Qualtrics rows
                 col_types = cols(prrp1_5 = col_character(), #force list variables to be characters
                                  prrp2_5 = col_character(),
                                  prrp3_5 = col_character(),
                                  rep_2 = col_character(), 
                                  lead = col_character()))

data <- data[, c(-1:-10)] #delete unnecessary Qualtrics data


# CLEANING =================================================
# > Basic cleaning ----

## special NA values
experience <- dict %>% 
  filter(scale == "Positive" | scale == "Negative") %>% 
  pull(variable)
data <- data %>%
  mutate_at(c(experience), na_if, 11)

prrp <- dict %>% 
  filter(value_label == "0 = No, 1 = Yes, 2 = I have not published an empirical paper") %>% 
  pull(variable)
data <- data %>%
  mutate_at(c(prrp), na_if, 2)

rm(experience, prrp)


## variable types
names <- dict %>% 
  filter(type == "character") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.character)

names <- dict %>% 
  filter(type == "factor") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.factor)

names <- dict %>% 
  filter(type == "numeric") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.numeric)

rm(names)

# > List variables ----

## each number is surrounded by 2 commas
data$prrp1_5 <- paste0(",", data$prrp1_5, ",")
data$prrp2_5 <- paste0(",", data$prrp2_5, ",")
data$prrp3_5 <- paste0(",", data$prrp3_5, ",")
data$rep_2 <- paste0(",", data$rep_2, ",")
data$lead <- paste0(",", data$lead, ",")

## prrp1_5 reasons for not posting data
data <- data %>% 
  mutate(
    prrp1_5_1 = ifelse(
      str_detect(data$prrp1_5, fixed(",1,")), #detect when 1 is included
      1,0),
    prrp1_5_2 = ifelse(
      str_detect(data$prrp1_5, fixed(",2,")), #detect when 2 is included
      1,0),
    prrp1_5_3 = ifelse(
      str_detect(data$prrp1_5, fixed(",3,")), #detect when 3 is included
      1,0),      
    prrp1_5_4 = ifelse(
      str_detect(data$prrp1_5, fixed(",4,")), #detect when 4 is included
      1,0),
    prrp1_5_5 = ifelse(
      str_detect(data$prrp1_5, fixed(",5,")), #detect when 5 is included
      1,0),
    prrp1_5_6 = ifelse(
      str_detect(data$prrp1_5, fixed(",6,")), #detect when 6 is included
      1,0),
    prrp1_5_7 = ifelse(
      str_detect(data$prrp1_5, fixed(",7,")), #detect when 7 is included
      1,0),
    prrp1_5_8 = ifelse(
      str_detect(data$prrp1_5, fixed(",8,")), #detect when 8 is included
      1,0),
    prrp1_5_9 = ifelse(
      str_detect(data$prrp1_5, fixed(",9,")), #detect when 9 is included
      1,0),
    prrp1_5_10 = ifelse(
      str_detect(data$prrp1_5, fixed(",10,")), #detect when 10 is included
      1,0))

var_label(data$prrp1_5_1) <- "Posting data - The data were propritary or access was restricted"
var_label(data$prrp1_5_2) <- "Posting data - It was too time-consuming"
var_label(data$prrp1_5_3) <- "Posting data - I didn't want to be scooped"
var_label(data$prrp1_5_4) <- "Posting data - The data were too difficult to deidentify"
var_label(data$prrp1_5_5) <- "Posting data - I didn't perceive my field to be in favor of this"
var_label(data$prrp1_5_6) <- "Posting data - The data were already publicly available"
var_label(data$prrp1_5_7) <- "Posting data - I was nervous that someone would discover a mistake"
var_label(data$prrp1_5_8) <- "Posting data - I was unfamiliar with the practice"
var_label(data$prrp1_5_9) <- "Posting data - I am planning to but haven't done it yet"
var_label(data$prrp1_5_10) <- "Posting data - Other reason"

## prrp2_5 reasons for not posting instruments
data <- data %>% 
  mutate(
    prrp2_5_1 = ifelse(
      str_detect(data$prrp2_5, fixed(",1,")), #detect when 1 is included
      1,0),
    prrp2_5_2 = ifelse(
      str_detect(data$prrp2_5, fixed(",2,")), #detect when 2 is included
      1,0),
    prrp2_5_3 = ifelse(
      str_detect(data$prrp2_5, fixed(",3,")), #detect when 3 is included
      1,0),      
    prrp2_5_4 = ifelse(
      str_detect(data$prrp2_5, fixed(",4,")), #detect when 4 is included
      1,0),
    prrp2_5_5 = ifelse(
      str_detect(data$prrp2_5, fixed(",5,")), #detect when 5 is included
      1,0),
    prrp2_5_6 = ifelse(
      str_detect(data$prrp2_5, fixed(",6,")), #detect when 6 is included
      1,0),
    prrp2_5_7 = ifelse(
      str_detect(data$prrp2_5, fixed(",7,")), #detect when 7 is included
      1,0),
    prrp2_5_8 = ifelse(
      str_detect(data$prrp2_5, fixed(",8,")), #detect when 8 is included
      1,0),
    prrp2_5_9 = ifelse(
      str_detect(data$prrp2_5, fixed(",9,")), #detect when 9 is included
      1,0))

var_label(data$prrp2_5_1) <- "Posting instruments - The materials were proprietary or access was restricted"
var_label(data$prrp2_5_2) <- "Posting instruments - It was too time-consuming"
var_label(data$prrp2_5_3) <- "Posting instruments - I didn't want to be scooped"
var_label(data$prrp2_5_4) <- "Posting instruments - Materials were too difficult to deidentify"
var_label(data$prrp2_5_5) <- "Posting instruments - I didn't perceive my field to be in favor of this"
var_label(data$prrp2_5_6) <- "Posting instruments - The materials were already publicly available"
var_label(data$prrp2_5_7) <- "Posting instruments - I was unfamiliar with the practice"
var_label(data$prrp2_5_8) <- "Posting instruments - I am planning to but haven't done it yet"
var_label(data$prrp2_5_9) <- "Posting instruments - Other reason"

## prrp3_5 reasons for not preregistering
data <- data %>% 
  mutate(
    prrp3_5_1 = ifelse(
      str_detect(data$prrp3_5, fixed(",1,")), #detect when 1 is included
      1,0),
    prrp3_5_2 = ifelse(
      str_detect(data$prrp3_5, fixed(",2,")), #detect when 2 is included
      1,0),
    prrp3_5_3 = ifelse(
      str_detect(data$prrp3_5, fixed(",3,")), #detect when 3 is included
      1,0),      
    prrp3_5_4 = ifelse(
      str_detect(data$prrp3_5, fixed(",4,")), #detect when 4 is included
      1,0),
    prrp3_5_5 = ifelse(
      str_detect(data$prrp3_5, fixed(",5,")), #detect when 5 is included
      1,0),
    prrp3_5_6 = ifelse(
      str_detect(data$prrp3_5, fixed(",6,")), #detect when 6 is included
      1,0))

var_label(data$prrp3_5_1) <- "Preregistration - It was too time-consuming"
var_label(data$prrp3_5_2) <- "Preregistration - I didn't want to be scooped"
var_label(data$prrp3_5_3) <- "Preregistration - I didn't perceive my field to be in favor of this"
var_label(data$prrp3_5_4) <- "Preregistration - The materials were already publicly available"
var_label(data$prrp3_5_5) <- "Preregistration - I was unfamiliar with the practice"
var_label(data$prrp3_5_6) <- "Preregistration - Other reason"

## rep_2 reasons for replication failures
data <- data %>% 
  mutate(
    rep_2_1 = ifelse(
      str_detect(data$rep_2, fixed(",1")), #detect when 1 is included
      1,0),
    rep_2_2 = ifelse(
      str_detect(data$rep_2, fixed(",2")), #detect when 2 is included
      1,0),
    rep_2_3 = ifelse(
      str_detect(data$rep_2, fixed(",3")), #detect when 3 is included
      1,0),
    rep_2_4 = ifelse(
      str_detect(data$rep_2, fixed(",4")), #detect when 4 is included
      1,0),
    rep_2_5 = ifelse(
      str_detect(data$rep_2, fixed(",5")), #detect when 5 is included
      1,0),
    rep_2_6 = ifelse(
      str_detect(data$rep_2, fixed(",6")), #detect when 6 is included
      1,0),
    rep_2_7 = ifelse(
      str_detect(data$rep_2, fixed(",7")), #detect when 7 is included
      1,0),
    rep_2_8 = ifelse(
      str_detect(data$rep_2, fixed(",8")), #detect when 8 is included
      1,0),
    rep_2_9 = ifelse(
      str_detect(data$rep_2, fixed(",9")), #detect when 9 is included
      1,0))

var_label(data$rep_2_1) <- "Replication failure - Outdated methods"
var_label(data$rep_2_2) <- "Replication failure - Low quality data"
var_label(data$rep_2_3) <- "Replication failure - Lack of external validity"
var_label(data$rep_2_4) <- "Replication failure - Lack of internal validity"
var_label(data$rep_2_5) <- "Replication failure - Publication bias"
var_label(data$rep_2_6) <- "Replication failure - Author bias"
var_label(data$rep_2_7) <- "Replication failure - Fraud"
var_label(data$rep_2_8) <- "Replication failure - I don't think there is a replication problem"
var_label(data$rep_2_9) <- "Replication failure - Other explanation"

## lead leadership positions
data <- data %>% 
  mutate(
    lead_1 = ifelse(
      str_detect(data$lead, fixed(",1")), #detect when 1 is included
      1,0),
    lead_2 = ifelse(
      str_detect(data$lead, fixed(",2")), #detect when 2 is included
      1,0),
    lead_3 = ifelse(
      str_detect(data$lead, fixed(",3")), #detect when 3 is included
      1,0),
    lead_4 = ifelse(
      str_detect(data$lead, fixed(",4")), #detect when 4 is included
      1,0))

var_label(data$lead_1) <- "Leadership position - Your institution"
var_label(data$lead_2) <- "Leadership position - Journal editorialship"
var_label(data$lead_3) <- "Leadership position - Professional societies"
var_label(data$lead_4) <- "Leadership position - None of the above"

# > Labels ----
## variable labels
var_label(data) <- dict %>% 
  dplyr::select(variable, label) %>% 
  dict_to_list()

## value labels demographics
val_labels(data$career) <- c("Undergraduate student" = 1,
                             "Graduate student" = 2,
                             "Post-doctoral fellow" = 3,
                             "Assistant professor/lecturer" = 4,
                             "Associate/full professor" = 5,
                             "Research scientist" = 6, 
                             "Other" = 7) 

val_labels(data$continent) <- c("Africa" = 1,
                                "Asia" = 2,
                                "Australia/Oceania" = 3,
                                "Europe" = 4,
                                "North America" = 5,
                                "South America" = 6)

val_labels(data$politics) <- c("Extremely liberal" = 1,
                               "Liberal" = 2, 
                               "Slightly liberal" = 3,
                               "Moderate" = 4,
                               "Sligthly conservative" = 5,
                               "Conservative" = 6,
                               "Extremely conservative" = 7)

val_labels(data$group) <- c("Flat" = 1,
                            "Raffle" = 2)

val_labels(data$age) <- c("18-22" = 1,
                          "23-29" = 2,
                          "30-39" = 3,
                          "40-49" = 4,
                          "50-59" = 5,
                          "60-69" = 6,
                          "70+" = 7)

val_labels(data$c19stress) <- c("Low stress" = 1,
                                "High stress" = 10)

## value labels 0 = No, 1 = Yes
likert <- dict %>% 
  filter(value_label == "0 = No, 1 = Yes" 
         | value_label == "0 = No, 1 = Yes, 2 = I have not published an empirical paper" #2 special NA
         | value_label == "0 = No, 1 = Yes, 2 = Maybe") %>%
  pull(variable)
list <- data %>% 
  select(starts_with(c("prrp1_5_", "prrp2_5_", "prrp3_5_", "rep_2_", "lead_"))) %>% 
  select(-prrp1_5_o, -prrp2_5_o, -prrp3_5_o, -rep_2_o) %>% 
  names()

add_likert <- function(x) {
  val_labels(x) <- c("No" = 0,
                     "Yes" = 1,
                     "Maybe" = 2)
  x
}
data <- data %>%
  mutate_at(c(likert,list), 
            add_likert)  
rm(list)

## value labels 0 = Not at all, 10 = Extremely
likert <- dict %>% 
  filter(value_label == "0 = Not at all, 10 = Extremely, 11 = NA") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("Not at all" = 0,
                     "Extremely" = 10)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## value labels 1 = It should never be used, 4 = It should be used almost always
likert <- dict %>% 
  filter(value_label == "1 = It should never be used, 2 = It should only be used rarely, 3 = It should be used often, 4 = It should be used almost always") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("It should never be used" = 1,
                     "It should only be used rarely" = 2,
                     "It should be used often" = 3,
                     "It should be used almost always" = 4)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## value labels 1 = Never, 3 = Often
likert <- dict %>% 
  filter(value_label == "1 = Never, 2 = Once or Twice, 3 = Often") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("Never" = 1,
                     "Once or Twice" = 2,
                     "Often" = 3)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## value labels 1 = Never, 5 = Almost always
likert <- dict %>% 
  filter(value_label == "1 = Never, 2 = Once, 3 = Occasionally, 4 = Frequently, 5 = Almost always") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("Never" = 1,
                     "Once" = 2,
                     "Occasionally" = 3,
                     "Frequently" = 4,
                     "Almost always" = 5)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## value labels 1 = Not at all, 7 = Extremely
likert <- dict %>% 
  filter(value_label == "1 = Not at all, 7 = Extremely") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("Not at all" = 1,
                     "Extremely" = 7)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## value labels 1 = Strongly disagree 5 = Strongly agree
likert <- dict %>% 
  filter(value_label == "1 = Strongly disagree, 3 = Neither agree nor disagree, 5 = Strongly agree") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("Strongly disagree" = 1,
                     "Neither agree nor disagree" = 3,
                     "Strongly agree" = 5)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## value labels 1 = Very low confidence, 5 = High confidence
likert <- dict %>% 
  filter(value_label == "1 = Very low confidence, 2 = Low confidence, 3 = Neither high nor low confidence, 4 = Moderate confidence, 5 = High confidence") %>%
  pull(variable)
add_likert <- function(x) {
  val_labels(x) <- c("Very low confidence" = 1,
                     "Low confidence" = 2,
                     "Neither high nor low confidence" = 3,
                     "Moderate confidence" = 4, 
                     "High confidence" = 5)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

rm(likert, add_likert)

