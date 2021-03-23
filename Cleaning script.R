##############################################################
##    Open Science in Ethnic Minority/Cultural Psychology   ##
##                    Cleaning Script                       ##
##                                                          ##
## Linh Nguyen - nguy4006@umn.edu                           ##
##                                                          ##
## Created: 13-Jan-2021                                     ##
## Updated: 23-Mar-2021                                     ##
## To navigate: Edit - Folder - Collapse All                ##
##############################################################

# META =======================================================
# > Libraries ----

library("groundhog") #package version control
libDate <- "2021-01-01" #dates for package versions

groundhog.library("tidyverse", libDate) #general data wrangling
groundhog.library("rio", libDate) #import export files
groundhog.library("future", libDate) #reliability
groundhog.library("ufs", libDate) #reliability
groundhog.library("GGally", libDate) #reliability
groundhog.library("GPArotation", libDate) #reliability

library("codebook") #code book generation

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

data <- subset(data, select = -c(ID, Status, RecordedDate, ResponseId, 
                  DistributionChannel, UserLanguage)) #delete unnecessary Qualtrics data
data$id <- 1:nrow(data)

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

## attention check variable recode experience_19
data <- data %>% 
  mutate(experience_19 = ifelse
         (data$experience_19 == 3, 1, 0)) #3 is correct answer

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
  lapply(data[,names], as.numeric) #factor variables coded as numeric for codebook 

names <- dict %>% 
  filter(type == "numeric") %>% 
  pull(variable)
data[,names] <- 
  lapply(data[,names], as.numeric)

rm(names)

# > Categorize demographic variables ----

## Gender 
female <- data %>% 
  filter(str_detect(gender, fixed("fem", ignore_case=TRUE))|
           str_detect(gender, fixed("wom", ignore_case=TRUE))|
           str_detect(gender, fixed("she", ignore_case=TRUE))|
           gender == "f" |
           gender == "F")%>% 
  pull(id)

male <- data %>% 
  filter(gender == "male"|
           gender == "Male"|
           gender == "m"|
           gender == "M"|
           gender == "Man"|
           gender == "Male cisgender"|
           gender == "cis-man"|
           gender == "cisgender male"|
           gender == "MALE"|
           gender == "cisgender man") %>% 
  pull(id)

data <- data %>% 
  mutate(gender_f = ifelse(
    id %in% female, # select when ID corresponds to "female"
    1,
    ifelse(
      id %in% male, # select when ID corresponds to "male"
      2, NA)))

data$gender_f <- as.numeric(data$gender_f)

rm(female, male)

val_labels(data$gender_f) <- c("female" = 1,
                               "male" = 2)

var_label(data$gender_f) <- "Gender Coded as Factor"

## Race/ethnicity
mixed <- data %>% 
  filter(race == "Asian and Hispanic/Latino"|
           race == "multi-racial black"|
           race == "mixed Latina and white"|
           race == "Multiracial Black"|
           race == "French & Guadeloupean"|
           race == "White and Choctaw"|
           race == "Mixed-latinx"|
           race == "white; jewish; black"|
           race == "Asian American (White and Asian)"|
           race == "Mixed race Latina"|
           race == "Multiracial: Asian and White"|
           race == "white 4/8, native american 3/8, 1/8 unknown (probably white)"|
           race == "Multiracial"|
           race == "Biracial- White and Black American"|
           race == "multiracial"|
           race == "Mixed race/Latina"|
           race == "Native American/American Indian/Latinx"|
           race == "American Indian, Alaska Native or Native Hawaiian. Non-Hispanic.") %>% 
  pull(id)

black <- data %>% 
  filter(!id %in% mixed &
           str_detect(race, fixed("black", ignore_case = T))|
           str_detect(race, fixed("african", ignore_case = T))) %>% 
  pull(id)

hispanic <- data %>% 
  filter(!id %in% mixed &
           str_detect(race, fixed("latin", ignore_case = T))|
           str_detect(race, fixed("mexi", ignore_case = T))|
           race == "White, Hispanic"|
           race == "Hispanic"|
           race == "White Hispanic"|
           race == "Hispanic, white"|
           race == "White, of Hispanic Origin"|
           race == "Chicana") %>% 
  pull(id)

white <- data %>% 
  filter(!id %in% mixed &
           !id %in% hispanic &
           str_detect(race, fixed("white", ignore_case = T))|
           str_detect(race, fixed("cauca", ignore_case = T))|
           str_detect(race, fixed("euro", ignore_case = T))|
           str_detect(race, fixed("irish", ignore_case = T))|
           str_detect(race, fixed("italian", ignore_case = T))|
           race == "MENA"|
           race == "W"|
           race == "Write"|
           race == "Middle Eastern") %>% 
  pull(id)

asian <- data %>% 
  filter(!id %in% mixed &
           !id %in% white &
           str_detect(race, fixed("asian", ignore_case = T))|
           str_detect(race, fixed("viet", ignore_case = T))|
           str_detect(race, fixed("korean", ignore_case = T))|
           str_detect(race, fixed("chinese", ignore_case = T))) %>% 
  pull(id)

native <- data %>% 
  filter(race == "american indian"|
           race == "American Indian") %>% 
  pull(id)

data <- data %>% 
  mutate(race_f = ifelse(
    id %in% asian, # select when ID corresponds to "asian"
    1,
    ifelse(
      id %in% black, # select when ID corresponds to "black"
      2, 
      ifelse(
        id %in% hispanic, #select when ID corresponds to "hispanic"
        3, 
        ifelse(
          id %in% native, #select when ID corresponds to "native"
          4, ifelse(
            id %in% white, #select when ID corresponds to "white"
            5, ifelse(
              id %in% mixed, #select when ID corresponds to "mixed"
              6, NA)))))))

data$race_f <- as.numeric(data$race_f)

rm(asian, black, hispanic, native, white, mixed)

val_labels(data$race_f) <- c("Asian" = 1,
                             "Black" = 2,
                             "Hispanic" = 3,
                             "Native American" = 4,
                             "Non-Hispanic White" = 5,
                             "Mixed" = 6)

var_label(data$race_f) <- "Race/Ethnicity Coded as Factor"

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
      1,0))

var_label(data$lead_1) <- "Leadership position - Your institution"
var_label(data$lead_2) <- "Leadership position - Journal editorialship"
var_label(data$lead_3) <- "Leadership position - Professional societies"

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

## value labels 0 = No, 1 = Yes, 2 = Maybe
likert <- dict %>% 
  filter(value_label == "0 = No, 1 = Yes, 2 = Maybe") %>%
  pull(variable)

add_likert <- function(x) {
  val_labels(x) <- c("No" = 0,
                     "Yes" = 1,
                     "Maybe" = 2)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

## value labels 0 = No, 1 = Yes 
likert <- dict %>% 
  filter(value_label == "0 = No, 1 = Yes" 
         | value_label == "0 = No, 1 = Yes, 2 = I have not published an empirical paper") %>% 
  #2 special NA 
  pull(variable)
list <- data %>% 
  select(starts_with(c("prrp1_5_", "prrp2_5_", "prrp3_5_", "rep_2_", "lead_"))) %>% 
  select(-prrp1_5_o, -prrp2_5_o, -prrp3_5_o, -rep_2_o) %>% 
  names()

add_likert <- function(x) {
  val_labels(x) <- c("No" = 0,
                     "Yes" = 1)
  x
}
data <- data %>%
  mutate_at(c(likert,list), 
            add_likert)  
rm(list)

## value labels 0 = Incorrect, 1 = Correct
likert <- dict %>% 
  filter(value_label == "0 = Incorrect, 1 = Correct") %>% #attention check variables
  pull(variable)

add_likert <- function(x) {
  val_labels(x) <- c("Incorrect" = 0,
                     "Correct" = 1)
  x
}
data <- data %>%
  mutate_at(likert, 
            add_likert)  

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

# > Scale scoring ----
## reverse scoring
reversed_items <- dict %>%  #make a list of reversed items
  filter (keying == -1) %>% 
  pull(variable)

data <- data %>%  #reverse values in data
  mutate_at(reversed_items,
            reverse_labelled_values)

rm(reversed_items)

# >> Questionable research practices (Fraser, 2018) ----
## create list of items
selfQRP <- dict %>% 
  filter(scale == "Self QRP") %>% 
  pull(variable)
peerQRP <- dict %>% 
  filter(scale == "Peer QRP") %>% 
  pull(variable)
opinionQRP <- dict %>% 
  filter(scale == "Opinion QRP") %>% 
  pull(variable)

## create aggregated variables 
data$selfQRP <- data %>% 
  dplyr::select(all_of(selfQRP)) %>% 
  aggregate_and_document_scale()
data$peerQRP <- data %>% 
  dplyr::select(all_of(peerQRP)) %>% 
  aggregate_and_document_scale()
data$opinionQRP <- data %>% 
  dplyr::select(all_of(opinionQRP)) %>% 
  aggregate_and_document_scale()

## variable label for aggregated variables 
var_label(data$selfQRP) <- "Have you engaged in this practice? - 10 questionable research practices aggregated by rowMeans"
var_label(data$peerQRP) <- "Percentage of researchers in ethnic minority/cultural psychology who you believe have
engaged in this practice - 10 questionable research practices aggregated by rowMeans"
var_label(data$opinionQRP) <- "What is your opinion of this practice? - 10 questionable research practices aggregated by rowMeans"

rm(selfQRP, peerQRP, opinionQRP)

# >> Proposed reforms to researcher practices (Christensen, 2019 + Fraser, 2018) ----
## create list of items
awarePRRP <- dict %>% 
  filter(scale == "Aware PRRP") %>% 
  pull(variable)
selfPRRP <- dict %>% 
  filter(scale == "Self PRRP") %>% 
  pull(variable)
peerPRRP <- dict %>% 
  filter(scale == "Peer PRRP") %>% 
  pull(variable)
recentPRRP <- dict %>% 
  filter(scale == "Recent PRRP") %>% 
  pull(variable)
opinionPRRP <- dict %>% 
  filter(scale == "Opinion PRRP") %>% 
  pull(variable)

## create aggregated variables 
data$awarePRRP <- data %>% 
  dplyr::select(all_of(awarePRRP)) %>% 
  aggregate_and_document_scale()
data$selfPRRP <- data %>% 
  dplyr::select(all_of(selfPRRP)) %>% 
  aggregate_and_document_scale()
data$peerPRRP <- data %>% 
  dplyr::select(all_of(peerPRRP)) %>% 
  aggregate_and_document_scale()
data$recentPRRP <- data %>% 
  dplyr::select(all_of(recentPRRP)) %>% 
  aggregate_and_document_scale()
data$opinionPRRP <- data %>% 
  dplyr::select(all_of(opinionPRRP)) %>% 
  aggregate_and_document_scale()

## variable label for aggregated variables 
var_label(data$awarePRRP) <- "Have you ever heard of this practice? - 3 proposed reforms to research practices aggregated by rowMeans"
var_label(data$selfPRRP) <- "Have you engaged in this practice? - 3 proposed reforms to research practices aggregated by rowMeans"
var_label(data$peerPRRP) <- "Percentage of researchers in ethnic minority/cultural psychology who you believe have
engaged in this practice - 3 proposed reforms to research practices aggregated by rowMeans"
var_label(data$recentPRRP) <- "Did you engage in this practice for your last empirical paper? - 3 proposed reforms to research practices aggregated by rowMeans"
var_label(data$opinionPRRP) <- "What is your opinion of this practice? - 3 proposed reforms to research practices aggregated by rowMeans"

rm(awarePRRP, selfPRRP, peerPRRP, recentPRRP, opinionPRRP)

# >> Conversation (Koyama, 2020) ----
## create list of items
participate <- dict %>% 
  filter(scale == "Participate") %>% 
  pull(variable)
comfort <- dict %>% 
  filter(scale == "Comfort") %>% 
  pull(variable)
positive <- dict %>% 
  filter(scale == "Positive") %>% 
  pull(variable)
negative <- dict %>% 
  filter(scale == "Negative") %>% 
  pull(variable)
entity <- dict %>% 
  filter(scale == "Entity") %>% 
  pull(variable)

## create aggregated variables 
data$participate <- data %>% 
  dplyr::select(all_of(participate)) %>% 
  aggregate_and_document_scale()
data$comfort <- data %>% 
  dplyr::select(all_of(comfort)) %>% 
  aggregate_and_document_scale()
data$positive <- data %>% 
  dplyr::select(all_of(positive)) %>% 
  aggregate_and_document_scale()
data$negative <- data %>% 
  dplyr::select(all_of(negative)) %>% 
  aggregate_and_document_scale()
data$entity <- data %>% 
  dplyr::select(all_of(entity)) %>% 
  aggregate_and_document_scale()

## variable label for aggregated variables 
var_label(data$participate) <- "To what extent do you engage in discussions about open science in the following environment? - 4 items aggregated by rowMeans"
var_label(data$comfort) <- "How comfortable would you feel participating in a discussion about open science with other psychology researchers in each of the following environments? - 4 items aggregated by rowMeans"
var_label(data$positive) <- "How would you describe your experience participating in or observing discussions about open science and/or the improvement of psychological science? - 12 positive emotions items aggregated by rowMeans"
var_label(data$negative) <- "How would you describe your experience participating in or observing discussions about open science and/or the improvement of psychological science? - 6 negative emotions aggregated by rowMeans"
var_label(data$entity) <- "To what extent do you perceive discussions about open science to be occurring... - 2 items aggregated by rowMeans"

rm(participate, comfort, positive, negative, entity)

# >> Replications (Christensen, 2019) ----
## create list of items
repConf <- dict %>% 
  filter(scale == "Replication confidence") %>% 
  pull(variable)

## create aggregated variables 
data$repConf <- data %>% 
  dplyr::select(all_of(repConf)) %>% 
  aggregate_and_document_scale()

## variable label for aggregated variables 
var_label(data$repConf) <- "How confident are you that research findings in your field would replicate? - 4 items aggregated by rowMeans"

rm(repConf)

# >> Integrity (Fraser, 2018) ----
## create list of items
integrity <- dict %>% 
  filter(scale == "Integrity") %>% 
  pull(variable)
intMild <- dict %>% 
  filter(subscale == "Integrity mild") %>% 
  pull(variable)
intSerious <- dict %>% 
  filter(subscale == "Integrity serious") %>% 
  pull(variable)

## create aggregated variables 
data$integrity <- data %>% 
  dplyr::select(all_of(integrity)) %>% 
  aggregate_and_document_scale()
data$intMild <- data %>% 
  dplyr::select(all_of(intMild)) %>% 
  aggregate_and_document_scale()
data$intSerious <- data %>% 
  dplyr::select(all_of(intSerious)) %>% 
  aggregate_and_document_scale()

## variable label for aggregated variables 
var_label(data$integrity) <- "Have you ever had doubts about the scientific integrity of researchers in ethnic minority/cultural psychology research? - 10 items aggregated by rowMeans"
var_label(data$intMild) <- "Have you ever had doubts about the scientific integrity of researchers in ethnic minority/cultural psychology research for mild issues/questionable research practices? - 5 items aggregated by rowMeans"
var_label(data$intSerious) <- "Have you ever had doubts about the scientific integrity of researchers in ethnic minority/cultural psychology research for serious issues/scientific misconducts? - 5 items aggregated by rowMeans"

rm(integrity, intMild, intSerious)

# >> Personality TIPI (Gosling, 2003) ----
## create list of items
agree <- dict %>% 
  filter(scale == "Agreeableness") %>% 
  pull(variable)
consci <- dict %>% 
  filter(scale == "Conscientiousness") %>% 
  pull(variable)
extra <- dict %>% 
  filter(scale == "Extraversion") %>% 
  pull(variable)
neuro <- dict %>% 
  filter(scale == "Neuroticism") %>% 
  pull(variable)
open <- dict %>% 
  filter(scale == "Openness") %>% 
  pull(variable)

## create aggregated variables 
data$agree <- data %>% 
  dplyr::select(all_of(agree)) %>% 
  aggregate_and_document_scale()
data$consci <- data %>% 
  dplyr::select(all_of(consci)) %>% 
  aggregate_and_document_scale()
data$extra <- data %>% 
  dplyr::select(all_of(extra)) %>% 
  aggregate_and_document_scale()
data$neuro <- data %>% 
  dplyr::select(all_of(neuro)) %>% 
  aggregate_and_document_scale()
data$open <- data %>% 
  dplyr::select(all_of(open)) %>% 
  aggregate_and_document_scale()

## variable label for aggregated variables 
var_label(data$agree) <- "Agreeableness - 2 TIPI items aggregated by rowMeans"
var_label(data$consci) <- "Conscientiousness - 2 TIPI items aggregated by rowMeans"
var_label(data$extra) <- "Extraversion - 2 TIPI items aggregated by rowMeans"
var_label(data$neuro) <- "Neurotism - 2 TIPI items aggregated by rowMeans"
var_label(data$open) <- "Openness - 2 TIPI items aggregated by rowMeans"

rm(agree, consci, extra, neuro, open)

# >> List variables ----
## PRRP1_5 Reasons for not posting data
prrp1_5 <- c("prrp1_5_1", "prrp1_5_2", "prrp1_5_3", "prrp1_5_4", "prrp1_5_5", "prrp1_5_6",
          "prrp1_5_7", "prrp1_5_8", "prrp1_5_9", "prrp1_5_10")

data$prrp1_5 <- data %>% 
  dplyr::select(all_of(prrp1_5)) %>% 
  aggregate_and_document_scale()

var_label(data$prrp1_5) <- "10 Reasons for not posting data"

## PRRP2_5 Reasons for not posting instruments
prrp2_5 <- c("prrp2_5_1", "prrp2_5_2", "prrp2_5_3", "prrp2_5_4", "prrp2_5_5", "prrp2_5_6",
          "prrp2_5_7", "prrp2_5_8", "prrp2_5_9")

data$prrp2_5 <- data %>% 
  dplyr::select(all_of(prrp2_5)) %>% 
  aggregate_and_document_scale()

var_label(data$prrp1_5) <- "9 Reasons for not posting instruments"

## PRRP3_5 Reasons for not preregistering
prrp3_5 <- c("prrp3_5_1", "prrp3_5_2", "prrp3_5_3", "prrp3_5_4", "prrp3_5_5", "prrp3_5_6")

data$prrp3_5 <- data %>% 
  dplyr::select(all_of(prrp3_5)) %>% 
  aggregate_and_document_scale()

var_label(data$prrp3_5) <- "6 Reasons for not preregistering"

## rep_2 Reasons for replication failures
rep_2 <- c("rep_2_1", "rep_2_2", "rep_2_3", "rep_2_4", "rep_2_5", "rep_2_6",
           "rep_2_7", "rep_2_8", "rep_2_9")

data$rep_2 <- data %>% 
  dplyr::select(all_of(rep_2)) %>% 
  aggregate_and_document_scale()

var_label(data$rep_2) <- "9 Reasons for replication failures"

## lead Leadership positions in the past 5 years 
lead <- c("lead_1", "lead_2", "lead_3")

data$lead <- data %>% 
  dplyr::select(all_of(lead)) %>% 
  aggregate_and_document_scale()

var_label(data$lead) <- "3 Leadership positions in the past 5 years"

# EXPORT =====================================================
export(data, "cleaned.csv")
