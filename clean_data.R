library(plyr)
library(tidyverse)
library(magrittr)
options(dplyr.width = Inf)
library(here)
library(readr)
library(forcats)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)




# Download raw data-----------
raw_data_start_path <- read_csv(here("data_raw", "AV_formr_conjoint_start_v3.csv"))
raw_data_P1 <- read_csv(here("data_raw", "AV_formr_conjoint_P1_v3.csv"))
raw_data_P2 <- read_csv(here("data_raw", "AV_formr_conjoint_P2_v3.csv"))
raw_data_P3 <- read_csv(here("data_raw", "AV_formr_conjoint_P3_v3.csv"))
raw_data_P4 <- read_csv(here("data_raw", "AV_formr_conjoint_P4_v3.csv"))


#Format start survey
raw_data_start <- raw_data_start_path %>%
  filter(!is.na(ended)) %>% 
  #calc time to do start survey
  mutate(
    start_created = ymd_hms(created),
    start_ended =  ymd_hms(ended),
    sec_elapsed_start = as.numeric(start_ended - start_created, units = "secs")) %>%
  #select important col from start survey
  select(session, respondentID, psid, start_ended, sec_elapsed_start, everything()) %>% 
  filter(!is.na(sec_elapsed_start))

raw_data_P1 <- raw_data_P1 %>% 
  mutate(
    P1_created = ymd_hms(created),
    P1_ended = ymd_hms(ended)
  ) %>%
  select(-respondentID) %>% 
  filter(!is.na(P1_ended))

# Join start and P1 data sets
raw_data_StartP1 <- raw_data_start %>%
  inner_join(raw_data_P1, by = c("session")) %>% 
  select(session, start_ended, pageTime3, everything())

# Join P1 and P2 data sets

raw_data_P2 <- raw_data_P2 %>% 
  mutate(
    P2_created = ymd_hms(created),
    P2_ended = ymd_hms(ended)
  ) %>% 
  select(-respondentID) %>% 
  filter(!is.na(P2_ended))


raw_data <- raw_data_StartP1 %>% 
  inner_join(raw_data_P2, by = c("session")) %>% 
  select(session, sec_elapsed_start, start_ended, P1_ended, P2_created, everything())

# Add in P3 data

raw_data_P3 <- raw_data_P3 %>% 
  mutate(
    P3_created = ymd_hms(created),
    P3_ended = ymd_hms(ended)
  ) %>% 
  select(-respondentID) %>% 
  filter(!is.na(P3_ended))

raw_data <- raw_data %>% 
  inner_join(raw_data_P3, by = c("session")) %>% 
  select(session, respondentID, sec_elapsed_start, start_ended, P2_ended, P3_ended, everything())


# Add in P4 data

raw_data_P4 <- raw_data_P4 %>% 
  mutate(
    P4_created = ymd_hms(created),
    P4_ended = case_when(
      is.na(ended) ~ ymd_hms(pageTime23), 
      TRUE ~ ymd_hms(ended))  # change to pageTime24 for future surveys
  ) %>% 
  filter(!is.na(P4_ended))


raw_data <- raw_data %>% 
  inner_join(raw_data_P4, by = c("session")) %>% 
  select(session, psid, respondentID, sec_elapsed_start, everything())

#check Dynata data

check <- raw_data %>% 
  select(psid, screenout, attentionScreenout, speedCheckScreenout, cbcSameScreenout, start_created, P4_ended)

dynataList <- check %>% 
  select(psid, P4_ended) %>% 
  distinct(psid, P4_ended)

write_csv(dynataList, here::here('data_processed', 'dynataList.csv')) 

# Clean data-----------------

data_clean <- raw_data %>%
  #calc times for survey
  mutate(
    pageTime3 = ymd_hms(pageTime3, tz = "EST"), # calculate time on video page
    pageTime4 = ymd_hms(pageTime4, tz = "EST"),
    pageTime12 = ymd_hms(pageTime12, tz = "EST"), 
    pageTime13 = ymd_hms(pageTime13, tz = "EST"), # practice conjoint Q
    pageTime14 = ymd_hms(pageTime14, tz = "EST"), #start of conjoint Qs
    pageTime15 = ymd_hms(pageTime15, tz = "EST"), 
    pageTime16 = ymd_hms(pageTime16, tz = "EST"),
    pageTime17 = ymd_hms(pageTime17, tz = "EST"),
    pageTime18 = ymd_hms(pageTime18, tz = "EST"),
    pageTime19 = ymd_hms(pageTime19, tz = "EST"),
    pageTime20 = ymd_hms(pageTime20, tz = "EST"), 
    pageTime21 = ymd_hms(pageTime21, tz = "EST"),
    pageTime22 = ymd_hms(pageTime22, tz = "EST"), 
    pageTime23 = ymd_hms(pageTime23, tz = "EST"), #end of conjoint Qs
    sec_elapsed_P1 = as.numeric(P1_ended - start_ended, units = "secs"), #time for part 1
    sec_elapsed_P2 = as.numeric(P2_ended - P1_ended, units = "secs"), #time for part 2
    sec_elapsed_P3 = as.numeric(P3_ended - P2_ended, units = "secs"), #time for part 3
    sec_elapsed_P4 = as.numeric(P4_ended - P3_ended, units = "secs"), #time for part 4
    sec_elapsed_total = as.numeric(sec_elapsed_start + sec_elapsed_P1 + sec_elapsed_P2 + sec_elapsed_P3 + sec_elapsed_P4, units = "secs"),
    sec_practice_conjoint = as.numeric(pageTime13 - pageTime12, units = "secs"), # time on practice conjoint Q
    sec_introVid = as.numeric(pageTime4 - pageTime3, units = "secs"), # time on video briefing page
    sec_cbc1 = as.numeric(pageTime15 - pageTime14, units = "secs"), #time for conjoint Qs
    sec_cbc2 = as.numeric(pageTime16 - pageTime15, units = "secs"), 
    sec_cbc3 = as.numeric(pageTime17 - pageTime16, units = "secs"),
    sec_cbc4 = as.numeric(pageTime18 - pageTime17, units = "secs"),
    sec_cbc5 = as.numeric(pageTime19 - pageTime18, units = "secs"),
    sec_cbc6 = as.numeric(pageTime20 - pageTime19, units = "secs"),
    sec_cbc7 = as.numeric(pageTime21 - pageTime20, units = "secs"),
    sec_cbc8 = as.numeric(pageTime22 - pageTime21, units = "secs"),
    avg_sec_cbc = as.numeric(((sec_cbc1 + sec_cbc2 + sec_cbc3 + sec_cbc4 + sec_cbc5 + sec_cbc6 + sec_cbc7 + sec_cbc8)/8), units = "secs"),
    avg_sec_cbcFirstFour = as.numeric(((sec_cbc1 + sec_cbc2 + sec_cbc3 + sec_cbc4)/4), units = "secs"),
    avg_sec_cbcLastFour = as.numeric(((sec_cbc5 + sec_cbc6 + sec_cbc7 + sec_cbc8)/4), units = "secs")
  )

data_clean$id = seq(nrow(data_clean)) 

write_csv(data_clean, here::here('data_processed', 'data_clean.csv')) 


data <- read_csv(here::here('data_processed', 'data_clean.csv'))

# Filter out responses------------

#starting dimensions
dim(data) 


#1 - check if all conjoint responses were the same

data <- data %>% 
  mutate(cbcValid = ifelse(
    (cbc1 == cbc2 & cbc2 == cbc3 & cbc3 == cbc4 & cbc4 == cbc5 & cbc5 == cbc6 & cbc6 == cbc7 & cbc7 == cbc8), 0, 1)) %>% 
  filter(cbcValid == 1 ) 

dim(data)


#2 - Remove missing conjoint responses 
#Note: Only removing the skipped question, not the entire respondent.

data <- data %>% 
  select(session, respondentID, cbc1, cbc2, cbc3, cbc4, cbc5, cbc6, cbc7, cbc8, everything()) %>% 
  gather(
    key = "conjointQ",
    value = "response",
    cbc1:cbc8
  ) %>% 
  select(session, respondentID, response, everything()) %>% 
  filter(!is.na(response)) %>% 
  spread(
    key = conjointQ,
    value = response
  )

dim(data)

#3 - Remove duplicated session IDs

data <- data %>%   
  distinct(session, .keep_all = TRUE) 

dim(data)

#4 - Remove too short total survey times - less than 10th percentile

data <- data %>%     
  filter(sec_elapsed_total > floor(quantile(sec_elapsed_total, .1))) 
#filter(sec_elapsed_total > 420) 
dim(data)



#5 - Remove too short conjoint question times


data <- data %>%
  gather(
    key = "conjointQ",
    value = "conjointTime",
    sec_cbc1:sec_cbc8
  ) %>%
  filter(conjointTime > 5) %>%
  spread(
    key = conjointQ,
    value = conjointTime
  )

dim(data)

#6 - Remove respondents that got the attention check question wrong

data <- data %>% 
  filter(attentionCheck == "attendant_no")

dim(data)

data_filtered <- data

# Demographic groupings ---------

minorityRace <- data_filtered %>% 
  select(race) %>% 
  filter(!(race %in% c("white", "prefer_not_say", "black")), !is.na(race)) %>% 
  unique()


minorityRace <- as.list(minorityRace)

data_filtered <- data_filtered %>%
  mutate(
    raceGroup = case_when(
      race %in% minorityRace$race ~ "mixedRace",
      (race == "prefer_not_say" |is.na(race) ) ~ "unknown",
      TRUE ~ race),
    genderGroup = case_when(
      gender == "male" ~ "A", 
      gender %in% c("female", "transMale", "transFemale", "genderNonconform") ~ "B",
      TRUE ~ "unknown"),
    incomeGroup = case_when(
      income %in% c("under10", "inc_10to15", "inc_15to25", "inc_25to35", "inc_35to50") ~ "B",
      (income == "prefer_not_say" | is.na(income)) ~ "unknown",
      TRUE ~ "A") #high income
  ) 

write_csv(data_filtered, here::here('data_processed', 'data_filtered.csv'))



# Creating df of choices---------------

# Load the survey data set:

survey <- read_csv(here::here('survey.csv'))
data_filtered <- read_csv(here::here('data_processed', 'data_filtered.csv')) %>% 
  select(session, respondentID, id, cbc1, cbc2, cbc3, cbc4, cbc5, cbc6, cbc7, cbc8, everything())


# Remove participants who are missing demographic info

data_filtered <- data_filtered %>%
  filter(genderGroup != "unknown")


dim(data_filtered)

# Merge responses with survey designs to get choiceData 

choiceData <- data_filtered %>%  
  select(respondentID, cbc1:cbc8, genderGroup, id) %>% 
  mutate(
    weights = ifelse(genderGroup == "B", 1.15, .75) # added in weights for gender
  ) %>% 
  gather(
    key = "qID",
    value = "selection",
    cbc1:cbc8
  ) %>% 
  mutate(
    qID = str_replace(qID, "cbc", ""),
    qID = as.numeric(qID),
    respondentID = as.numeric(respondentID)
  ) %>% 
  left_join(survey, by = c("respondentID" = "respID", "qID")) %>% 
  mutate(
    choice = ifelse(selection == altID, 1, 0)
  ) 



# Re-number obsID and respondentID
#choiceData$obsID = rep(seq(nrow(choiceData) / 4), each = 4)

id <-  sort(unique(choiceData$id))

temp <- data.frame(id = id, newID = seq(1:length(id)))

choiceData <- choiceData %>% 
  left_join(temp, by = "id") %>% 
  select(-id) %>% 
  rename(id = newID)

# Save formatted response data
write_csv(choiceData, here::here('data_processed', 'choiceData.csv'))



