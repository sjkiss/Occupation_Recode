library(rvest)
library(tidyverse)
#This sets the url that contains the NOC21
myurl<-"https://noc.esdc.gc.ca/Structure/Hierarchy?objectid=%2Fd0IGA6qD8JPRfoj5UCjpg%3D%3D"
noc<-read_html(myurl)
#this gets the five digit nocCode
html_nodes(noc, ".nocCode") %>% 
html_text()->noc_codes
#This gets the title associated with the five digit nocCode
html_nodes(noc, ".nocTitle") %>% 
  html_text()->noc_titles
#This code gets the 4-digit NOC codes and 4-digit group titles
html_elements(noc, "details summary") %>% 
  html_text() %>% 
  data.frame() %>% 
  filter(., str_detect(., "^[0-9]{4} ")) %>% 
  separate(., col=".", into=c("NOC21_4", "NOC21_4_Title"), sep=4) %>% 
  mutate(NOC21_4_Title=str_trim(NOC21_4_Title))->NOC21_4_titles

#View(noc_2021)
#This gets the job description for each five digit NOC code
html_nodes(noc, ".nocLI") %>% 
 # html_children() %>% 
  html_elements("p") %>% 
  html_text()->noc_description

myurl_fr<-"https://noc.esdc.gc.ca/LaStructure/Hierarchie?objectid=%2fd0IGA6qD8JPRfoj5UCjpg%3d%3d&GoCTemplateCulture=fr-CA"
noc_fr<-read_html(myurl_fr)
html_nodes(noc_fr, ".nocCode") %>% 
  html_text()->noc_codes_fr
html_nodes(noc_fr, ".nocTitle") %>% 
  html_text()->noc_titles_fr
html_elements(noc_fr, "details summary") %>% 
  html_text() %>% 
  data.frame() %>% 
  filter(., str_detect(., "^[0-9]{4} ")) %>% 
  separate(., col=".", into=c("NOC21_4", "NOC21_4_Title_fr"), sep=4) %>% 
  mutate(NOC21_4_Title_fr=str_trim(NOC21_4_Title_fr))->NOC21_4_titles_fr
#View(noc_2021)
html_nodes(noc_fr, ".nocLI") %>% 
  # html_children() %>% 
  html_elements("p") %>% 
  html_text()->noc_description_fr

noc_2021<-data.frame(NOC21_5=noc_codes, 
                     NOC21_5_Title=noc_titles, 
                     NOC21_5_Description=noc_description, 
                     NOC21_5_Title_fr=noc_titles_fr, 
                     NOC21_5_Description_fr=noc_description_fr)
#Combine the 4-digit english codes and titles with the 4-digit french codes and titles
NOC21_4_titles %>% 
  left_join(NOC21_4_titles_fr) ->NOC21_4_titles

#View(noc_2021)
library(openxlsx)


write.xlsx(noc_2021, file="Data/NOC_2021_5_job_titles.xlsx", overwrite=T)
write.xlsx(NOC21_4_titles, file="Data/NOC_2021_4_job_titles.xlsx", overwrite=T)

