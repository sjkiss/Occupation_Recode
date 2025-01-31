#load the ces19phone
#import ces19phone
library(haven)
library(here)
library(tidyverse)
library(labelled)
#Read in ces19phone
ces19phone<-read_dta(file=here("Data/2019 Canadian Election Study - Phone Survey v1.0.dta"), encoding="utf-8")
#Necessary for ces19web because of problems with encoding
#Sys.setlocale(locale = "fr_CA.UTF-8")
#ces19web<-read_dta(file=here("Data/CES-E-2019-online_F1.dta"), encoding="")
ces19web<-read_dta(file=here("Data/CES-E-2019-online_F1.dta"), encoding="latin1")
ces19web
#Load ces21 occupations
#ces21<-read_dta(file=here("Data/CES 2021 Occupation.dta"), encoding="latin1")
#### Diagnostic section for accented characters ####
ces19web$cps19_prov_id
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text)
#ces21 %>% 
  #filter(str_detect(pes21_occ_text,"assembleur-m")) %>% 
  #select(cps21_ResponseId, pes21_occ_text)
ces19web$encoding<-Encoding(ces19web$pes19_occ_text)
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding) 
ces19web %>% 
  filter(str_detect(pes19_occ_text,"Ã|©")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding) %>% 
  write_csv(file=here("Data/messy.csv"))
#ces21 %>% 
 # filter(str_detect(pes21_occ_text,"Ã")) %>% 
  #select(pes21_occ_text)

#Try to fix
source("fix_encodings.R")
messy<-ces19web$pes19_occ_text
library(stringi)
ces19web$pes19_occ_text_cleaned<-stri_replace_all_fixed(messy, names(fixes), fixes, vectorize_all = F)

ces19web %>% 
  filter(str_detect(pes19_occ_text_cleaned,"Ã|©")) %>% 
  select(cps19_ResponseId, pes19_occ_text, pes19_occ_text_cleaned, encoding) %>% 
  View()

#Check the value labels for provincial party id

lookfor(ces19web, "employment")

ces19web %>% 
  select(cps19_ResponseId,pes19_occ_text, encoding) %>% 
  filter(str_detect(pes19_occ_text, "inval")) 


ces21 %>% 
  slice(271:280)
#Look for occupations
library(tidyverse)


library(openxlsx)
#Write out ces19phone occupations
lookfor(ces19phone, "occupation")

lookfor(ces19phone, "employment")
ces19phone %>%   
  select(q68,p52) %>% 
  #This filters in 
  # full time, part time, self-employed student working for pay, caring for family and working and retired and working
  filter(q68==1|q68==2|q68==3|q68==9|q68==10|q68==11) %>% 
  select(p52) ->ces19phone_occupation
#Write out ces19_web_occupations
lookfor(ces19web, "employment")
lookfor(ces19web, "occupation")
table(ces19web$cps19_employment, ces19web$pes19_employment)
table(ces19web$pes19_employment)
ces19web %>% 
  #This filters in 
  # full time, part time, self-employed student working for pay, caring for family and working and retired and working
 filter(cps19_employment==1|cps19_employment==2|cps19_employment==3|cps19_employment==9|cps19_employment==10|cps19_employment==11) %>%       
  select(pes19_occ_text) ->ces19web_occupation
lookfor(ces19web, "employment")

#Wrote out ces21 occupations
lookfor(ces21, "employment")
lookfor(ces21, "occupation")

ces21 %>% 
  filter(cps21_employment==1|cps21_employment==2|cps21_employment==3|cps21_employment==9|cps21_employment==10|cps21_employment==11) %>%       
  select(pes21_occ_text)->ces21web_occupation

#Join all three variables

occupations<-c(ces19phone_occupation$p52, ces19web_occupation$pes19_occ_text, ces21web_occupation$pes21_occ_text)
occupations
#Turn to lower case
occupations<-str_to_lower(occupations)
#Filter out duplicates
occupations<-unique(occupations)
occupations
data.frame(occupations)
write.xlsx(data.frame(title=occupations), file="Data/occupations.xlsx")

### Duplicate without filtering out employment status

ces19phone %>%   
  select(q68,p52) %>% 
  #This filters in 
  # full time, part time, self-employed student working for pay, caring for family and working and retired and working
  #filter(q68==1|q68==2|q68==3|q68==9|q68==10|q68==11) %>% 
  select(p52) ->ces19phone_occupation
#Write out ces19_web_occupations
lookfor(ces19web, "employment")
lookfor(ces19web, "")
table(ces19web$cps19_employment, ces19web$pes19_employment)
table(ces19web$pes19_employment)
ces19web %>% 
  #This filters in 
  # full time, part time, self-employed student working for pay, caring for family and working and retired and working
  #filter(cps19_employment==1|cps19_employment==2|cps19_employment==3|cps19_employment==9|cps19_employment==10|cps19_employment==11) %>%       
  select(pes19_occ_text) ->ces19web_occupation
lookfor(ces19web, "employment")

#Wrote out ces21 occupations
lookfor(ces21, "employment")
ces21 %>% 
  #filter(cps21_employment==1|cps21_employment==2|cps21_employment==3|cps21_employment==9|cps21_employment==10|cps21_employment==11) %>%       
  select(pes21_occ_text)->ces21web_occupation

#Join all three variables

occupations<-c(ces19phone_occupation$p52, ces19web_occupation$pes19_occ_text, ces21web_occupation$pes21_occ_text)
occupations
#Turn to lower case
occupations<-str_to_lower(occupations)
#Filter out duplicates
occupations<-unique(occupations)
occupations
data.frame(occupations)
write.xlsx(data.frame(title=occupations), file="Data/occupations_all_employment.xlsx")
