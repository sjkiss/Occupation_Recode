
library(haven)
library(here)
library(tidyverse)
library(labelled)
# #Download file
# temp <- tempfile()
# temp2 <- tempfile()
# 
# download.file("https://github.com/sjkiss/Occupation_Recode/raw/main/Data/CES-E-2019-online_F1.dta.zip", temp)
# unzip(zipfile = temp, exdir = temp2)
# ces19web <- read_dta(file.path(temp2, "CES-E-2019-online_F1.dta"), encoding="latin1")
#ces19web <- read_dta(file=here("Data/2019 Canadian Election Study - Online Survey v1.0.dta"), encoding="latin1")

#ces19web <- read_sav(file=here("Data/CES-E-2019-online_F1.sav"), encoding="latin1")

#Try with encoding set to blank, it won't work. 
#ces19web <- read_dta(file.path(temp2, "CES-E-2019-online_F1.dta"), encoding="")

#unlink(c(temp, temp2))

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


#Try to fix
#Run this function from Stack overflow

for (n in names(ces19web)) {
  v <- ces19web[[n]]
  if (is.character(v)) {
    v <- iconv(v, from = "UTF-8", to = "latin1") 
    Encoding(v) <- "UTF-8"
  }
  ces19web[[n]] <- v
}

#Examine
ces19web %>% 
  filter(str_detect(pes19_occ_text,"assembleur-m")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding) 


ces19web %>% 
  filter(str_detect(pes19_occ_text,"Ã|©")) %>% 
  select(cps19_ResponseId, pes19_occ_text, encoding)
