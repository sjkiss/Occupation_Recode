
library(haven)
library(tidyverse)
library(estimatr)
library(broom)
library(marginaleffects)
library(nnet)

# I have stored these data files in my own personal package that is stored on GitHub. 
#library(devtools)
#devtools::install_github("sjkiss/cesdata2", force=T)
library(cesdata2)

CES_2019_web <- ces19web %>% 
  select(pes19_conf_inst1_2, pes19_conf_inst1_1, cps19_issue_handle_1, pes19_conf_inst2_8, 
         pes19_age, cps19_gender, pes19_lang, cps19_province, pes19_votechoice2019,degree,
         #Would it be better if we used the NOC21_4 codes? Would we have a bigger sample size?
         NOC21_5, NOC21_4) 

# Clean 2019 CES 

CES_2019_web <- CES_2019_web %>% 
  mutate(Year = "2019",
         pes19_age = case_when(pes19_age < 35 ~ "18-34",
                               pes19_age > 34 & pes19_age < 55 ~ "35-54",
                               pes19_age > 54 ~ "55+"),
         cps19_gender = ifelse(cps19_gender == 2, 1, 0),
         pes19_votechoice2019 = replace(pes19_votechoice2019, pes19_votechoice2019 %in% c(8, 9), NA), 
         Degree=as_factor(degree),
         Province=as_factor(cps19_province)) %>% 
  rename(Conf_Province = pes19_conf_inst1_2,
         Conf_Federal = pes19_conf_inst1_1,
         Handle_healthcare = cps19_issue_handle_1, 
         Conf_Police = pes19_conf_inst2_8,
         Age = pes19_age,
         Gender = cps19_gender,
         #Province = cps19_province,
         Vote_Choice = pes19_votechoice2019
  )
table(CES_2019_web$Province)
table(CES_2019_web$Age, useNA = "ifany")
# Select Only Key Variables
table(as_factor(ces21$provcode), as_factor(ces21$prov), useNA = "ifany")

CES_2021 <- ces21 %>% 
  select(pes21_conf_inst1_2, pes21_conf_inst1_1, cps21_issue_handle_1, pes21_conf_inst2_6,
         age, cps21_genderid, Q_Language, provcode, cps21_votechoice,
         NOC21_5, NOC21_4, degree)
table(ces21$province, as_factor(ces21$prov), useNA = "ifany")
# Clean 2021 CES 
val_labels(CES_2021$prov)
val_labels(CES_2019_web$Province)
CES_2021 <- CES_2021 %>%  
  mutate(Degree=as_factor(degree),Year = "2021",
        Age = case_when(age < 35 ~ "18-34",
                               age > 34 & age < 55 ~ "35-54",
                               age > 54 ~ "55+"),
         cps21_issue_handle_1 = replace(cps21_issue_handle_1, cps21_issue_handle_1 == 6, 7),
         cps21_genderid = ifelse(cps21_genderid == 2, 1, 0),
         Province = as_factor(provcode),
         cps21_votechoice = replace(cps21_votechoice, cps21_votechoice == 7, NA),
         cps21_votechoice = replace(cps21_votechoice, cps21_votechoice == 6, 7)) %>% 
  rename(Conf_Province = pes21_conf_inst1_2,
         Conf_Federal = pes21_conf_inst1_1,
         Handle_healthcare = cps21_issue_handle_1,
         Conf_police = pes21_conf_inst2_6,
         Gender = cps21_genderid,
         Vote_Choice = cps21_votechoice
  )

# Merge 2019 and 2021 CES
library(labelled)
#Value labels for these are different; 
val_labels(CES_2019_web$Vote_Choice)
val_labels(CES_2021$Vote_Choice)
#Convert to fators for recoding
CES_2019_web$Vote_Choice<-as_factor(CES_2019_web$Vote_Choice)
CES_2021$Vote_Choice<-as_factor(CES_2021$Vote_Choice)
table(CES_2019_web$Vote_Choice)
table(CES_2021$Vote_Choice)
CES_2019_web$Handle_healthcare<-as_factor(CES_2019_web$Handle_healthcare)
CES_2021$Handle_healthcare<-as_factor(CES_2021$Handle_healthcare)
CES <- bind_rows(CES_2021, CES_2019_web)
table(CES$Year, CES$Province)
table(CES$Vote_Choice)
val_labels(CES_2019_web$Province)
#Convert in one go

CES %>% 
  mutate(Vote_Choice=case_when(
    str_detect( Vote_Choice,"ndp|NDP")~"NDP",
    str_detect(Vote_Choice,"Liberal|liberal")~"Liberal",
    str_detect(Vote_Choice,"Conservative")~"Conservative",
    str_detect(Vote_Choice,"Bloc|bloc")~"BQ",
    str_detect(Vote_Choice,"People")~"People's Party",
    str_detect(Vote_Choice,"Green")~"Green",
    str_detect(Vote_Choice,"know|Know")~NA_character_,
TRUE~"Other"
  ))->CES
table(CES$Vote_Choice)
CES %>% 
  mutate(Handle_healthcare=case_when(
    str_detect( Handle_healthcare,"ndp|NDP")~"NDP",
    str_detect(Handle_healthcare,"Liberal|liberal")~"Liberal",
    str_detect(Handle_healthcare,"Conservative")~"Conservative",
    str_detect(Handle_healthcare,"Bloc|bloc")~"BQ",
    str_detect(Handle_healthcare,"Green")~"Green",
    str_detect(Handle_healthcare,"know|Know")~NA_character_,
    TRUE~"Other"
  ))->CES
table(CES$Handle_healthcare)
CES %>% 
  mutate(Region=case_when(
    str_detect(Province, "Newfo|Prince|Nova|New Brun")~"Atlantic",
    str_detect(Province, "Quebec")~"Quebec",
    str_detect(Province, "Ontario")~"Ontario",
    str_detect(Province, "Manit|Sask|British|Alberta")~"West",
  ))->CES
CES$Region<-factor(CES$Region, levels=c("Atlantic", "Quebec", "Ontario", "West"))
# Create a right_wing vote choice variable 

CES <- CES %>% 
  mutate(Right_wing = ifelse(Vote_Choice %in% c("Conservative"), 1, ifelse(Vote_Choice %in% c("BQ", "Liberal", "NDP", "Green", "Other"), 0, NA)))
CES %>% 
  count(Right_wing, Year)
##### SAMPLE ANALYSIS USING ANOTHER TABLE ####

labour_market_summary <- read.csv("Data/RLMC_CRMT_2021_2023_NOC2021.csv")

labour_market_summary <- labour_market_summary %>% 
  slice(-(1:17))

labour_market_summary <- labour_market_summary %>% 
  mutate(Recent_Labour_Market_Conditions = case_when(Recent_Labour_Market_Conditions == 
                                                       "Moderate signs of Shortage" ~ "Shortage",
                                                     Recent_Labour_Market_Conditions == 
                                                       "Strong signs of Shortage" ~ "Shortage",
                                                     Recent_Labour_Market_Conditions == 
                                                       "Moderate signs of Surplus" ~ "Surplus",
                                                     Recent_Labour_Market_Conditions == 
                                                       "Strong signs of Surplus" ~ "Surplus",
                                                     TRUE ~ Recent_Labour_Market_Conditions
  ))

future_labour_market <- read.csv("Data/Summary_sommaire_2024_2033_NOC2021.csv")

future_labour_market <- future_labour_market %>% 
  slice(-(1:17))


table(future_labour_market$Future_Labour_Market_Conditions)

future_labour_market <- future_labour_market %>% 
mutate(Future_Labour_Market_Conditions = case_when(Future_Labour_Market_Conditions == 
                                                     "Moderate risk of Shortage" ~ "Shortage",
                                                   Future_Labour_Market_Conditions == 
                                                     "Strong risk of Shortage" ~ "Shortage",
                                                   Future_Labour_Market_Conditions == 
                                                     "Moderate risk of Surplus" ~ "Surplus",
                                                   Future_Labour_Market_Conditions == 
                                                     "Strong risk of Surplus" ~ "Surplus",
                                                   TRUE ~ Future_Labour_Market_Conditions
)) %>% 
  select(Code, Future_Labour_Market_Conditions)


job_market_conditons <- left_join(labour_market_summary, future_labour_market, by = "Code")

table(job_market_conditons$Future_Labour_Market_Conditions)

job_market_conditons <- job_market_conditons %>% 
  mutate(Code = as.numeric(Code))

job_market_conditons <- job_market_conditons %>%  
  mutate(across(c(Recent_Labour_Market_Conditions, Future_Labour_Market_Conditions), \(x)replace(x, x == "N/A", NA)))
  
CES <- left_join(CES, job_market_conditons, by = c("NOC21_5" = "Code"))
# CES$Province
# table(CES$Future_Labour_Market_Conditions)
# table(CES$Recent_Labour_Market_Conditions)
# Create a Vector with the names of the control variables

CES %>% 
  count(Year, Recent_Labour_Market_Conditions,degree)
CONTROLS <- c("Age", "Gender", "Region", "Year", "Degree")

Recent_conditions<- lm(reformulate(c("Recent_Labour_Market_Conditions", "Year"), response = "Right_wing"), data = CES) 

Recent_conditions_2 <- lm(reformulate(c("Recent_Labour_Market_Conditions", CONTROLS), response = "Right_wing"), data = CES) 
modelsummary::modelsummary(list(Recent_conditions, Recent_conditions_2), stars = TRUE)

Future_conditions <- lm(reformulate(c("Future_Labour_Market_Conditions", "Year"), response = "Right_wing"), data = CES) 
Future_conditions_2 <- lm(reformulate(c("Future_Labour_Market_Conditions", CONTROLS), response = "Right_wing"), data = CES) 


modelsummary::modelsummary(list(Future_conditions, Future_conditions_2), stars = TRUE)


Recent_conditions_df <- Recent_conditions %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% c("Recent_Labour_Market_ConditionsShortage", "Recent_Labour_Market_ConditionsSurplus")) %>% 
  mutate(Controls = "No Controls",
         Model = "Recent Labour Conditions") %>% 
  as.data.frame() 

Recent_conditions_2_df <- Recent_conditions_2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% c("Recent_Labour_Market_ConditionsSurplus", "Recent_Labour_Market_ConditionsShortage")) %>% 
  mutate(Controls = "Controls",
         Model = "Recent Labour Conditions") %>% 
  as.data.frame()


Future_conditions_df <- Future_conditions %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% c("Future_Labour_Market_ConditionsShortage", "Future_Labour_Market_ConditionsSurplus")) %>% 
  mutate(Controls = "No Controls",
         Model = "Future Labour Conditions") %>% 
  as.data.frame() 

Future_conditions_2_df <- Future_conditions_2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% c("Future_Labour_Market_ConditionsSurplus", "Future_Labour_Market_ConditionsShortage")) %>% 
  mutate(Controls = "Controls",
         Model = "Future Labour Conditions") %>% 
  as.data.frame()


labour_conditons_df <- rbind(Recent_conditions_df,
                             Recent_conditions_2_df) %>% 
  rbind(Future_conditions_df) %>% 
  rbind(Future_conditions_2_df)

labour_conditons_df <- labour_conditons_df %>% 
  mutate(term = case_when(str_detect(term, "Shortage") ~ "Shortage",
                          str_detect(term, "Surplus") ~ "Surplus"),
         Model = factor(Model, levels = c("Recent Labour Conditions", "Future Labour Conditions")))
CES %>% count(Year, Province) %>% view()

labour_market_conditions_plot <- labour_conditons_df %>% 
  ggplot(aes(x = estimate, y = term, col = Controls,
             shape = Controls, xmin = conf.low, xmax = conf.high)) + 
  geom_point(position = position_dodge(width = 0.4)) +
  geom_linerange(position = position_dodge(width = 0.4)) + 
  facet_wrap(~Model, ncol = 1) + 
  geom_vline(xintercept = 0, lty = 4, col = "red") +
  labs(x = NULL,
       y = NULL) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("orange", "darkblue"))
labour_market_conditions_plot
ggsave("plots/labour_market_conditions.png", labour_market_conditions_plot,  width = 6, height = 3)  


