#source("2_get_NOC_codes.R")
library(openxlsx)
noc<-read.xlsx(xlsxFile="Data/noc_english_complete.xlsx")
head(noc)
names(noc)
noc %>% 
group_by(NOC) %>% 
  summarise(text=str_c("Examples include: ", Sample_job, collapse="; ")) %>% 
  left_join(., noc) %>% 
  mutate(prompt=str_c(description, text)) %>% 
  select(-Sample_job) %>% 
distinct() %>% 
rename(completion=NOC) %>% 
  select(-c(2:6))->NOC_prompt_classification_english
NOC_prompt_classification_english %>% 
  relocate(prompt, .before=completion)->NOC_prompt_classification_english
library(rjson)
noc_english_json<-toJSON(NOC_prompt_classification_english)
write(noc_english_json, file="noc_english_json")
library(openai)

validation<-read.xlsx("Data/occupations_coded.xlsx")
names(validation)
validation$title
