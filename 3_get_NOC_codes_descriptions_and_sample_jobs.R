library(rvest)
myurl<-"https://noc.esdc.gc.ca/Structure/Hierarchy?objectid=%2Fd0IGA6qD8JPRfoj5UCjpg%3D%3D"
noc_html<-read_html(myurl)
#Scrape all English NOC Codes
# All information on each individual NOC code 
# is stored in an html tag with class nocLI
#First get the codes
noc_html %>% 
  html_elements(., ".nocLI .nocCode") %>% 
  html_text()->noc_codes
#Then get the job titles for each 5 digit code 
noc_html %>% html_elements(., ".nocTitle") %>% 
  html_text()->noc_titles
#Now get each link
noc_html %>% 
  #This gets the "a" tags in each nocLI
  html_elements(".nocLI a") %>% 
  #This grabs the actual value of href
  html_attr("href")  %>% 
  #This loops over each href stub and pastes the core URL onto it
  map(., ~ str_c("https://noc.esdc.gc.ca", .)) %>% 
unlist()->noc_urls
#Now get each job description
noc_html %>% html_elements(., ".nocLI p") %>% 
  html_text()->noc_descriptions
noc_descriptions
#Now get the big shebang sample job titles
#THIS IS TIME CONSUMING
noc_urls %>% 
  map(., . %>% read_html() %>% 
        html_node('#IndexTitles ul') %>% 
        html_children() %>% 
        html_text())->noc_specific_job_titles_raw
noc_specific_job_titles_raw
# This returns a list of noc specific job titles
# each list item corresponds to one noc code
# and contains a vector of sample job titles
# I think we can safely use the noc codes we have already gathered to just match
# the sample job titles to an noc code. Theyu were gathered sequentially, so they should match up

length(noc_codes)
names(noc_specific_job_titles_raw)<-noc_codes
#Turn this into a data frame
noc_specific_job_titles_raw %>% 
  unlist(.) %>% 
  data.frame() %>% 
    rownames_to_column() %>%
rename(NOC=1, `Sample_job`=2) %>% 
  #Note: the code above produces an NOC code that is 
  #. more than 5 digits because there are like 10, 15, example
  # job titles for each 5-digit NOC code
  # When the data frame is created and the 5-digit names for each list 
  # item are converted to a dataframe
  # rownames_to_columns makes sure that each new value 
  # For the variable rownames is unique
  # So it appends an extra digit to each name of each list item
  #all we need to do is take the first 5 digits of each rowname
  mutate(NOC=str_sub(NOC, start=1, end=5)) ->noc_specific_job_titles

head(noc_specific_job_titles)
#Tie this all into a nice data frame
noc<-data.frame(
  NOC=noc_codes,
  Title=noc_titles,
  url=noc_urls,
  description=noc_descriptions
)

#Add language variable
noc$language<-rep("English", nrow(noc))
#Check
noc %>% 
  left_join(., noc_specific_job_titles, by="NOC") ->noc

  # Complete for French
myurl_fr<-"https://noc.esdc.gc.ca/LaStructure/Hierarchie?objectid=%2fd0IGA6qD8JPRfoj5UCjpg%3d%3d&GoCTemplateCulture=fr-CA"
noc_html_fr<-read_html(myurl_fr)
#Scrape all French NOC Codes
#First get the codes
noc_html_fr %>% 
  html_elements(., ".nocLI .nocCode") %>% 
  html_text()->noc_codes_fr
#Then get the french job titles for each 5 digit code 
noc_html_fr %>% html_elements(., ".nocTitle") %>% 
  html_text()->noc_titles_fr
#Now get each link
noc_html_fr %>% 
  #This gets the "a" tags in each nocLI
  html_elements(".nocLI a") %>% 
  #This grabs the actual value of href
  html_attr("href") %>% 
  #This loops over each href stub and pastes the core URL onto it
  map(., ~ str_c("https://noc.esdc.gc.ca", .)) %>% 
  unlist()->noc_urls_fr

#Now get each french job description
noc_html_fr %>% html_elements(., ".nocLI p") %>% 
  html_text()->noc_descriptions_fr
noc_descriptions_fr
#Now get the big shebang sample job titles
#THIS IS TIME CONSUMING
noc_urls_fr %>% 
  map(., . %>% read_html() %>% 
        html_node('#IndexTitles ul') %>% 
        html_children() %>% 
        html_text())->noc_specific_job_titles_raw_fr
# This returns a list of noc specific job titles
# each list item corresponds to one noc code
# and contains a vector of sample job titles
# I think we can safely use the noc codes we have already gathered to just match
# the sample job titles to an noc code. Theyu were gathered sequentially, so they should match up

length(noc_codes_fr)
length(noc_specific_job_titles_raw_fr)
names(noc_specific_job_titles_raw_fr)<-noc_codes_fr
#Turn this into a data frame
noc_specific_job_titles_raw_fr %>% 
  unlist(.) %>% 
  data.frame() %>% 
  rownames_to_column() %>%
  rename(NOC=1, `Sample_job`=2) %>% 
  #Note: the code above produces an NOC code that is 
  #. more than 5 digits because there are like 10, 15, example
  # job titles for each 5-digit NOC code
  # When the data frame is created and the 5-digit names for each list 
  # item are converted to a dataframe
  # rownames_to_columns makes sure that each new value 
  # For the variable rownames is unique
  # So it appends an extra digit to each name of each list item
  #all we need to do is take the first 5 digits of each rowname
  mutate(NOC=str_sub(NOC, start=1, end=5)) ->noc_specific_job_titles_fr

head(noc_specific_job_titles_fr)
#Tie this all into a nice data frame
noc_fr<-data.frame(
  NOC=noc_codes_fr,
  Title=noc_titles_fr,
  url=noc_urls_fr,
  description=noc_descriptions_fr
)
#Add language variable
noc_fr$language<-rep("French", nrow(noc_fr))
noc_fr %>% 
  left_join(., noc_specific_job_titles_fr, by="NOC") ->noc_fr
glimpse(noc_fr)


# Write outthe files
library(openxlsx)

# Write out english with specific job titles
write.xlsx(noc, file="Data/noc_english_complete.xlsx")
# Write out french with specific job titles
write.xlsx(noc_fr, file="Data/noc_french_complete.xlsx")
# Write out english with without specific job titles
noc %>% 
  select(-Sample_job) %>% 
  distinct() %>% 
write.xlsx(., file="Data/noc_english_without_job_titles.xlsx",
           overwrite = T)
# Write out french with specific job titles
noc_fr %>% 
  select(-Sample_job) %>% 
  distinct() %>% 
write.xlsx(., file="Data/noc_french_without_job_titles.xlsx", 
           overwrite=T)

head(noc)
#View(noc)
