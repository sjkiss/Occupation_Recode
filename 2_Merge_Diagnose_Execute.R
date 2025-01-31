## -----------------------------------------------------------
#| label: load-libraries
#| echo: false
library(readxl)
library(here)
library(tidyverse)
library(dplyr)
library(haven)
library(skimr)
library(labelled)
library(stringr)


## -----------------------------------------------------------
#| label: import-noc
#| warning: false
#read in the data file
read_excel(path=here("Data/occupations_coded.xlsx")) %>% 
  #Select only the columns with the unique job title and the NOC codes
  select("title", "NOC21_4", "NOC21_5") %>% 
  #Set 0s to be missing in the two columns containing the NOC code
mutate(across(2:3, function(x) car::Recode(x, "0=NA", as.factor=T)))->
  # Store as object noc
  noc



## -----------------------------------------------------------
#| label: diagnostics-noc
#| echo: false


#This slightly modifies skim() to report counts of both missing and valid cases side-by-side
my_skim <- skim_with(
  base = sfl(n_missing = n_missing, 
             valid = n_complete, pct_complete=complete_rate),
  append = TRUE
)

#
noc %>% 
  #Deal only with the NOC4 and NOC5 variables
  select(2:3) %>% 
my_skim() %>% 
  select(skim_variable:pct_complete)


## -----------------------------------------------------------
#| label: import-2019-phone
ces19phone<-read_dta(here(file="Data/2019 Canadian Election Study - Phone Survey v1.0.dta"),encoding="utf-8")


## ----lower-p52-merge----------------------------------------

ces19phone %>% 
  #Convert p52 to lower case
  mutate(p52_lower=str_to_lower(p52)) %>% 
  #merge with noc keying on p52_lower and title
    left_join(., noc, by=join_by("p52_lower"=="title"))->ces19phone


## -----------------------------------------------------------
#| label:  employment-ces19phone
ces19phone %>% 
  mutate(employment=case_when(
    #If Q68 is between 0 and 4 employed
    q68>0 & q68< 4~ "Employed",
    # If Q68 is between 8 and 12 employed
    q68> 8 &q68<12~ "Employed",
    #If Q68 is 4 set Retired
    q68==4~ "Retired",
    #All other responses (e.g. student, family caregiver, don't know )
    # Set to other
    TRUE ~ "Other"
  )) %>% 
  #Set the factor levels for the new employment variable
  mutate(employment=factor(employment, levels=c("Employed", "Retired", "Other")))->
  #Save back into ces19phone
  ces19phone
#Show results 
table(ces19phone$employment)


## -----------------------------------------------------------
#| label: skim-2019-noc-employment

ces19phone %>% 
  select(employment, contains("NOC")) %>% 
  group_by(employment) %>% 
  my_skim() %>% 
    select(employment, skim_variable, n_missing:pct_complete) %>% 
  arrange(employment)


## -----------------------------------------------------------
#| label: ces19phone-NOC-diagnostics
set.seed(50)
ces19phone %>% 
  select(p52_lower, NOC21_4, NOC21_5) %>% 
  #This filters to include only those who responded with the following options
  filter(., str_detect(p52_lower, "carpenter|plumber|camionneur|infirmière|electricien|enseignant|menuisier|plombier|driver|nurse|doctor|teacher|electrician")) %>% 
  slice_sample(n=50) %>% 
  print(n=50)




## ----import-ces19web----------------------------------------
ces19web <- read_dta(file=here("Data/2019 Canadian Election Study - Online Survey v1.0.dta"), encoding="latin1")
source("1_problem_with_encodings.R")


## -----------------------------------------------------------
#| label: show-accented-characters-fix
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text)


## -----------------------------------------------------------
#| label: merge-2019-web
ces19web %>% 
  mutate(pes19_occ_text_lower=str_to_lower(pes19_occ_text)) %>% 
  left_join(., noc, join_by("pes19_occ_text_lower"=="title"))->ces19web


## -----------------------------------------------------------
#| label: recode-employment-2019-web
#Recode employment variable
ces19web %>% 
  mutate(employment=case_when(
    #Anything less than 4 is employed
    cps19_employment<4 ~ "Employed",
    #4 
    cps19_employment ==4 ~ "Retired",
   cps19_employment > 8 & cps19_employment<12~ "Employed",
    13==cps19_employment~ "Other",
    TRUE ~ "Other"
  ))->ces19web
#Show results of recode
table(ces19web$employment)



## -----------------------------------------------------------
#| label: skim-2019-web
ces19web %>%
  #Filter in only those who completed the PES
  filter(pes19_consent==1) %>% 
  #Form groups by employment status
  group_by(employment) %>%
  #Select only the two NOC codes
  select(contains("NOC")) %>% 
my_skim() %>% 
  select(employment, skim_variable, n_missing:pct_complete) %>% 
  arrange(employment)
  


## -----------------------------------------------------------
set.seed(50)
ces19web %>% 
  filter(pes19_consent==1) %>% 
  select(employment, pes19_occ_text_lower, NOC21_4, NOC21_5) %>% 
  slice_sample(n=50) %>% 
  print(n=50)


## -----------------------------------------------------------
#|label: ces19-show-diagnostics-of-specific-titles
set.seed(50)
ces19web %>% 
  select(employment, pes19_occ_text_lower, NOC21_4, NOC21_5) %>% 
  filter(str_detect(pes19_occ_text_lower, "carpenter|plumber|driver|nurse|doctor|teacher|electrician")) %>% 
  slice_sample(n=50) %>% 
  print(n=50)


## -----------------------------------------------------------
#| label: import-ces21
ces21<-read_dta(file=here("Data/CES21.dta"))


## -----------------------------------------------------------
#| label: ces21-employment

ces21 %>% 
  mutate(employment=case_when(
    cps21_employment< 4 ~ "Employed",
    cps21_employment==4 ~ "Retired",
    cps21_employment > 8 & cps21_employment<12 ~ "Employed",
    TRUE ~ 'Other'
  ))->ces21
#Show the results
table(ces21$employment)


## -----------------------------------------------------------
#| label: ces21-lower-merge-with-noc
ces21 %>% 
  mutate(pes21_occ_text_lower=str_to_lower(pes21_occ_text)) %>% 
  left_join(., noc, join_by("pes21_occ_text_lower"=="title"))->ces21



## -----------------------------------------------------------
#| label: skim-2021-web
ces21 %>%
  #Filter in only those who completed the PES
  filter(pes21_consent==1) %>% 
  #Form groups by employment status
  group_by(employment) %>%
  #Select only the two NOC codes
  select(contains("NOC")) %>% 
my_skim() %>% 
  select(employment, skim_variable, n_missing:pct_complete) %>% 
  arrange(employment)
  


## -----------------------------------------------------------
#| label: diagnostics-ces21
set.seed(50)
ces21 %>% 
  filter(pes21_consent==1) %>% 
  select(employment, pes21_occ_text_lower, NOC21_4, NOC21_5) %>% 
  slice_sample(n=50) %>% 
  print(n=50)


## -----------------------------------------------------------
#|label: ces21-show-diagnostics-of-specific-titles
set.seed(50)
ces21 %>% 
  select(employment, pes21_occ_text_lower, NOC21_4, NOC21_5) %>% 
  filter(str_detect(pes21_occ_text_lower, "carpenter|plumber|driver|nurse|doctor|teacher|electrician")) %>% 
  slice_sample(n=50) %>% 
  print(n=50)


## -----------------------------------------------------------
#| label: comprehensive-table
#select pes19 consent variable and NOC
ces19web %>% 
  select(consent=pes19_consent, contains("NOC"),
         #Store in object ces19
        employment)->ces19

#Select ces21 consent variable and NOC variables
ces21 %>% 
  select(consent=pes21_consent, contains("NOC"),
         #store in object ces21_2
         employment)->ces21_2

#Add variables showing which survey they came from 
ces19 %>% 
  mutate(Survey=rep("CES19Web", nrow(.)))->ces19
ces21_2 %>% 
  mutate(Survey=rep("CES21", nrow(.))) %>% 
  #bind ces21-2 to ces19
  bind_rows(ces19) %>% 
  #filter only those rs who participated in PES
  filter(consent==1) %>% 
  #unselect consent
  select(-consent) ->ces19_21
 
#Now prepare ces19phone
  ces19phone %>% 
    #Select only employment and NOC variables
  select(contains("NOC"), employment) %>% 
    #Add variable indicated where responses came frpm
mutate(Survey=rep("CES19Phone", nrow(.))) %>% 
    #bind with ces19-21
    bind_rows(ces19_21) %>% 
    #form groups of survey and employ,ent
    group_by(Survey, employment) %>%
    #skim to report statistics
    my_skim() %>% 
    select(Survey, employment, skim_variable, n_missing:pct_complete) %>% 
    arrange(Survey, employment)




## -----------------------------------------------------------
#| label: adding-value-labels
# run the script to scrape the NOC codes
source("2_get_NOC_codes.R")

#Merge the 5-digit NOC codes and 4-digit codes with ces19phone
ces19phone %>% 
  left_join(., noc_2021, by=c("NOC21_5")) %>% 
 left_join(., NOC21_4_titles) %>% 
 # select(contains("NOC21")) %>% 
  #Drop the detailed job description
  select(-contains('Description')) ->ces19phone
#repeat for ces19web
ces19web %>% 
  left_join(., noc_2021, by=c("NOC21_5")) %>% 
 left_join(., NOC21_4_titles) %>% 
 # select(contains("NOC21")) %>% 
  select(-contains('Description')) ->ces19web

ces21 %>% 
    left_join(., noc_2021, by=c("NOC21_5")) %>% 
 left_join(., NOC21_4_titles) %>% 
 # select(contains("NOC21")) %>% 
  select(-contains('Description')) ->ces21
  

# 
# # ## -----------------------------------------------------------
# # #| label: export-files
# #Export Ces19phone Stata
# ces19phone %>%
#   write_dta(path=here("Data/ces2019_phone_noc.dta"))
# #Export Ces19phone Rdata
ces19phone %>%
  save(file=here("Data/ces2019_phone_noc.RData"))
# 
# #Export CES19phone SPSS
# ces19phone %>%
#   write_sav(path=here("Data/ces2019_phone_noc.sav"))
# 
# #Export Ces19web Stata
# ces19web %>%
#   write_dta(path=here("Data/ces2019_web_noc.dta"))
# #Export Ces19phone Rdata
ces19web %>%
  save(file=here("Data/ces2019_web_noc.RData"))

# #Export Ces21 Stata
# ces21 %>% 
#   write_dta(path=here("Data/ces2021_web.dta"))
# #Export CES21 Rdata
ces21 %>%
  save(file=here("Data/ces2021_noc.RData"))
# #Export CES21 SPSS
# ces21 %>% 
#   write_sav(path=here("Data/ces2021_web_noc.sav"))


