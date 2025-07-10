
library(haven)
library(tidyverse)
library(estimatr)
library(broom)
library(marginaleffects)
library(nnet)

# I have stored these data files in my own personal package that is stored on GitHub. 
#library(devtools)
#devtools::install_github("sjkiss/cesdata2")
library(cesdata2)

CES_2019_web <- ces19web %>% 
  select(pes19_conf_inst1_2, pes19_conf_inst1_1, cps19_issue_handle_1, pes19_conf_inst2_8, 
         pes19_age, cps19_gender, pes19_lang, cps19_province, pes19_votechoice2019,
         #Would it be better if we used the NOC21_4 codes? Would we have a bigger sample size?
         NOC21_5, NOC21_4) 

# Clean 2019 CES 

CES_2019_web <- CES_2019_web %>% 
  mutate(Year = "2019",
         pes19_age = case_when(pes19_age < 35 ~ "18-34",
                               pes19_age > 34 & pes19_age < 55 ~ "35-54",
                               pes19_age > 54 ~ "55+"),
         cps19_gender = ifelse(cps19_gender == 2, 1, 0),
         pes19_votechoice2019 = replace(pes19_votechoice2019, pes19_votechoice2019 %in% c(8, 9), NA)) %>% 
  rename(Conf_Province = pes19_conf_inst1_2,
         Conf_Federal = pes19_conf_inst1_1,
         Handle_healthcare = cps19_issue_handle_1, 
         Conf_Police = pes19_conf_inst2_8,
         Age = pes19_age,
         Gender = cps19_gender,
         Province = cps19_province,
         Vote_Choice = pes19_votechoice2019
  )

# Select Only Key Variables

CES_2021 <- ces21 %>% 
  select(pes21_conf_inst1_2, pes21_conf_inst1_1, cps21_issue_handle_1, pes21_conf_inst2_6,
         Age, cps21_genderid, Q_Language, prov, cps21_votechoice,
         NOC21_5, NOC21_4)

# Clean 2021 CES 

CES_2021 <- CES_2021 %>%  
  mutate(Year = "2021",
         cps21_issue_handle_1 = replace(cps21_issue_handle_1, cps21_issue_handle_1 == 6, 7),
         cps21_genderid = ifelse(cps21_genderid == 2, 1, 0),
         prov = case_when(prov == 1 ~ 14,
                          prov %in% c(2, 3) ~ 15,
                          prov == 4 ~ 16,
                          prov %in% c(5, 8) ~17,
                          prov %in% c(6, 17) ~ 18,
                          prov == 7 ~ 19,
                          prov %in% c(9, 10) ~ 20,
                          prov == 11 ~ 21,
                          prov == 12 ~ 22,
                          prov == 13 ~ 23,
                          prov %in% c(14, 15) ~ 24,
                          prov == 16 ~ 25,
                          prov == 18 ~ 26
         ),
         cps21_votechoice = replace(cps21_votechoice, cps21_votechoice == 7, NA),
         cps21_votechoice = replace(cps21_votechoice, cps21_votechoice == 6, 7)) %>% 
  rename(Conf_Province = pes21_conf_inst1_2,
         Conf_Federal = pes21_conf_inst1_1,
         Handle_healthcare = cps21_issue_handle_1,
         Conf_police = pes21_conf_inst2_6,
         Gender = cps21_genderid,
         Province = prov,
         Vote_Choice = cps21_votechoice
  )

# Merge 2019 and 2021 CES

CES <- bind_rows(CES_2021, CES_2019_web)

# Classify Jobs by stability and type of stability

CES <- CES %>% 
  mutate(stability = case_when(NOC21_5 %in% c(20010, 20011, 20012, 21300, 21321, 21322, 21311,
                                              21200, 21201, 21202, 21203, 21210, 21211, 21223,
                                              21230, 21231, 21232, 21234, 31300, 31301, 31100, 
                                              31101, 31102, 31103, 31111, 31201, 31303, 32103,
                                              31209, 31303, 32103, 31121, 31202, 32109, 32120,
                                              32110, 32111, 32112, 33100, 32102, 32101, 33102,
                                              32109, 33103, 33109, 31200, 41301, 63200, 63201,
                                              65202, 72106, 73200, 84120, 85100, 85101, 85103, 
                                              85102, 85120, 94141, 94142, 
                                              
                                              32104, 44101, 62020, 62200, 63100, 65200, 64100,
                                              65310, 65200, 65201, 72310, 72311, 72400, 72402,
                                              72405, 72406, 72420, 72421, 72422, 72422, 72429, 
                                              73300, 94210, 94211, 94142, 95100, 95101, 95102, 
                                              95102, 95104, 95106, 95107
  ) ~ "Shortages",
  NOC21_5 %in% c(1411, 14112, 14300, 14301, 14110, 52100, 53100, 74102, 75201,
                 12103, 72600, 72601, 72602, 72603, 72604, 64320, 64322, 64321) ~ "Surplus",
  is.na(NOC21_5) ~ NA,
  TRUE ~ "Stable Occupations"
  ),
  Structural = case_when(NOC21_5 %in% c(20010, 20011, 20012, 21300, 21321, 21322, 21311,
                                        21200, 21201, 21202, 21203, 21210, 21211, 21223,
                                        21230, 21231, 21232, 21234, 31300, 31301, 31100, 
                                        31101, 31102, 31103, 31111, 31201, 31303, 32103,
                                        31209, 31303, 32103, 31121, 31202, 32109, 32120,
                                        32110, 32111, 32112, 33100, 32102, 32101, 33102,
                                        32109, 33103, 33109, 31200, 41301, 63200, 63201,
                                        65202, 72106, 73200, 84120, 85100, 85101, 85103, 
                                        85102, 85120, 94141, 94142, 
                                        1411, 14112, 14300, 14301, 14110, 52100, 53100, 74102, 75201) ~ "Strucutral",
                         NOC21_5 %in% c( 32104, 44101, 62020, 62200, 63100, 65200, 64100,
                                         65310, 65200, 65201, 72310, 72311, 72400, 72402,
                                         72405, 72406, 72420, 72421, 72422, 72422, 72429, 
                                         73300, 94210, 94211, 94142, 95100, 95101, 95102, 
                                         95102, 95104, 95106, 95107, 
                                         12103, 72600, 72601, 72602, 72603, 72604, 64320, 64322, 64321
                         ) ~ "Frictional"
  ),
  stability2 = factor(stability, levels = c("Stable Occupations", "Surplus", "Shortages")))

# Create a right_wing vote choice variable 

CES <- CES %>% 
  mutate(Right_wing = ifelse(Vote_Choice %in% c(2, 6), 1, ifelse(Vote_Choice %in% c(1, 3, 4, 5), 0, NA)))

#### EXPLORATORY ANALYSIS MODELS ####

# Create a Vector with the names of the control variables

CONTROLS <- c("Age", "Gender", "Province", "Year")

# Fit main models for stable occupation 
# LPM models coefficient represents the probability of voting for a right wing candidate 

Stability <- lm(reformulate(c("stability2"), response = "Right_wing"), data = CES) 
Stability_2 <- lm(reformulate(c("stability2", CONTROLS), response = "Right_wing"), data = CES) 

# Fit models with interactions for stability and and type of challenge for occupations

Structural_Stability <- lm(reformulate(c("stability*Structural"), response = "Right_wing"), data = CES) 
Structural_Stability_2 <- lm(Right_wing ~ stability*Structural + Age + Gender + Province + Year, data = CES) 

# Display models

modelsummary::modelsummary(list(Stability, Stability_2,
                                Structural_Stability,
                                Structural_Stability_2),
                           stars = TRUE)

#### CREATE FIGURE 1 ####

# Create dfs of models
Stability_df <- Stability %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% c("stability2Surplus", "stability2Shortages")) %>% 
  mutate(Controls = "No Controls",
         Model = "Labour Conditions") %>% 
  as.data.frame() 

Stability_2_df <- Stability_2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% c("stability2Surplus", "stability2Shortages")) %>% 
  mutate(Controls = "Controls",
         Model = "Labour Conditions") %>% 
  as.data.frame()


Structural_Stability_df <- slopes(Structural_Stability,
                                  variable = "stability", by = "Structural") %>% 
  mutate(Controls = "No Controls",
         Model = "Marginal Effect of Having a Surplus Occupation") %>% 
  as.data.frame()

Structural_Stability_2_df <-slopes(Structural_Stability_2,
                                   variable = "stability", by = "Structural") %>% 
  mutate(Controls = "Controls",
         Model = "Marginal Effect of Having a Surplus Occupation") %>% 
  as.data.frame()

# Combine all models 

Stability_df <- Stability_df %>% 
  rbind(Stability_2_df) 


Structural_Stability_df <-  Structural_Stability_df %>% 
  rbind(Structural_Stability_2_df) 

# Create the first panel of Figure 1

Stability_plot <- Stability_df %>% 
  mutate(term = case_match(term, "stability2Surplus" ~ "Surplus \n (Ref. Stable Occupations)",
                           "stability2Shortages" ~ "Shortage")) %>% 
  ggplot(aes(x = estimate, y = term,  col = Controls,
             shape = Controls, xmin = conf.low, xmax = conf.high)) + 
  geom_point(position = position_dodge(width = 0.4)) +
  geom_linerange(position = position_dodge(width = 0.4)) + 
  facet_wrap(~Model) + 
  geom_vline(xintercept = 0, lty = 4, col = "red") +
  labs(x = NULL,
       y = NULL) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("orange", "darkblue"))

# Create the second panel of figure 1 

Structural_Stability_plot <- Structural_Stability_df  %>% 
  ggplot(aes(x = estimate, y = Structural, col = Controls,
             shape = Controls, xmin = conf.low, xmax = conf.high)) + 
  geom_point(position = position_dodge(width = 0.4)) +
  geom_linerange(position = position_dodge(width = 0.4)) + 
  facet_wrap(~Model) + 
  geom_vline(xintercept = 0, lty = 4, col = "red") +
  labs(x = NULL,
       y = NULL) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("orange", "darkblue"))

# Save figure 1 

ggpubr::ggarrange(Stability_plot, Structural_Stability_plot, common.legend = TRUE,
                  nrow = 2, align = "hv", legend = "bottom") %>% 
  ggsave("plots/ocupation_stability.png", ., width = 6, height = 3)


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

table(labour_market_summary$Recent_Labour_Market_Conditions)


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

job_market_conditons %>% 
  filter(Recent_Labour_Market_Conditions == "Surplus")

job_market_conditons <- job_market_conditons %>%  
  mutate(across(c(Recent_Labour_Market_Conditions, Future_Labour_Market_Conditions), \(x)replace(x, x == "N/A", NA)))
  
CES <- left_join(CES, job_market_conditons, by = c("NOC21_5" = "Code"))

table(CES$Future_Labour_Market_Conditions)
 
Recent_conditions <- lm(reformulate(c("Recent_Labour_Market_Conditions", "Year"), response = "Right_wing"), data = CES) 
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

ggsave("plots/labour_market_conditions.png", labour_market_conditions_plot,  width = 6, height = 3)  


