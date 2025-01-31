
library(haven)
library(here)
library(tidyverse)
library(labelled)
#Download file
temp <- tempfile()
temp2 <- tempfile()

download.file("https://github.com/sjkiss/Occupation_Recode/raw/main/Data/CES-E-2019-online_F1.dta.zip", temp)
unzip(zipfile = temp, exdir = temp2)
ces19web <- read_dta(file.path(temp2, "CES-E-2019-online_F1.dta"), encoding="latin1")

#Try with encoding set to blank, it won't work. 
#ces19web <- read_dta(file.path(temp2, "CES-E-2019-online_F1.dta"), encoding="")

unlink(c(temp, temp2))

#### Diagnostic section for accented characters ####
ces19web$cps19_prov_id
#Note value labels are cut-off at accented characters in Quebec. 
#I know this occupation has messed up characters
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text)
#Check the encodings of the occupation titles and store in a variable encoding
ces19web$encoding<-Encoding(ces19web$pes19_occ_text)
#Check encoding of problematic characters
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding) 
#Write out messy occupation titles
ces19web %>% 
  filter(str_detect(pes19_occ_text,"Ã|©")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding) %>% 
  write_csv(file=here("Data/messy.csv"))

#Try to fix
#Run this function from Stack overflow
source("fix_encodings.R")
#store the messy variables in messy
messy<-ces19web$pes19_occ_text
library(stringi)
#Try to clean with the function fix_encodings
ces19web$pes19_occ_text_cleaned<-stri_replace_all_fixed(messy, names(fixes), fixes, vectorize_all = F)

#Examine
ces19web %>% 
  filter(str_detect(pes19_occ_text_cleaned,"Ã|©")) %>% 
  select(cps19_ResponseId, pes19_occ_text, pes19_occ_text_cleaned, encoding) %>% 
head()

