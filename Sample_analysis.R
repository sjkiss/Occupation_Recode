
library(haven)
library(tidyverse)
library(estimatr)
library(broom)
library(marginaleffects)
library(nnet)

# CES_2019_web <- read_dta("Data/ces2019_web_noc.dta")
# 
# CES_2021 <- read_dta("Data/ces2021_web.dta")

# I have stored these data files in my own personal package that is stored on GitHub. 
 #library(devtools)
 #install_github("sjkiss/cesdata2")
 library(cesdata2)

CES_2019_web <- ces19web %>% 
  select(pes19_conf_inst1_2, pes19_conf_inst1_1, cps19_issue_handle_1, pes19_conf_inst2_8, 
         pes19_age, cps19_gender, pes19_lang, cps19_province, pes19_votechoice2019,
         NOC21_5)


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


CES_2021 <- ces21 %>% 
  select(pes21_conf_inst1_2, pes21_conf_inst1_1, cps21_issue_handle_1, pes21_conf_inst2_6,
         Age, cps21_genderid, Q_Language, prov, cps21_votechoice,
         NOC21_5)


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


CES <- bind_rows(CES_2021, CES_2019_web)

CES <- CES %>% 
  mutate(stability = case_when(NOC21_5 %in% c(68:70, 84:89, 93, 94, 95, 96, 98, 99, 102, 104, 
                                              176, 177, 137:140, 142, 145, 146, 147, 148, 151,
                                              155, 160, 162:165, 180, 200, 292:294, 354, 394,
                                              435, 436:439, 468,
                                              161, 231, 278, 286, 289, 321, 322, 364, 365, 368,
                                              370, 373, 376, 377, 378, 380, 397, 476, 477, 468, 
                                              480, 481, 483, 484, 485
                                              ) ~ "Shortages",
                               NOC21_5 %in% c(57, 58, 62, 63, 56, 248, 257, 408, 419,
                                              27, 381:386, 309, 310, 311) ~ "Surplus",
                               is.na(NOC21_5) ~ NA,
                               TRUE ~ "Stable Occupations"
                               ),
         Structural = case_when(NOC21_5 %in% c(68:70, 84:89, 93, 94, 95, 96, 98, 99, 102, 104, 
                                               176, 177, 137:140, 142, 145, 146, 147, 148, 151,
                                               155, 160, 162:165, 180, 200, 292:294, 354, 394,
                                               435, 436:439, 468, 57, 58, 62, 63, 56, 248, 257, 408, 419) ~ "Strucutral",
                                NOC21_5 %in% c( 161, 231, 278, 286, 289, 321, 322, 364, 365, 368,
                                                370, 373, 376, 377, 378, 380, 397, 476, 477, 468, 
                                                480, 481, 483, 484, 485, 27, 381:386, 309, 310, 311) ~ "Frictional"),
         stability = factor(stability, levels = c("Stable Occupations", "Surplus", "Shortages")))

CES <- CES %>% 
  mutate(health_care = ifelse(NOC21_5 %in% c(137, 138, 139, 151, 153, 154,
                                             155, 158, 159, 160, 177, 178, 180),
                              1, ifelse(is.na(NOC21_5), NA, 0)),
         Police = ifelse(NOC21_5 %in% c(189, 202, 215), 1, ifelse(is.na(NOC21_5),
                                                                  NA, 0)),
         frontline_workers = ifelse(NOC21_5 %in% c(273, 274, 277, 278, 285, 290,
                                                   292, 298, 321, 322, 323), 1,
                                    ifelse(is.na(NOC21_5), NA, 0)
                                    ))



CES <- CES %>% 
  mutate(across(c(Conf_Province, Conf_Federal, Conf_police),
                \(x)replace(x, x == 5, NA)),
across(c(Conf_Province, Conf_Federal, Conf_police),
       \(x)case_match(x, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)),
         Handle_healthcare = case_when(Handle_healthcare == 1 ~ 1,
                                       Handle_healthcare %in% c(2, 3, 4, 5) ~ 0,
                                       Handle_healthcare == 6 ~ NA)
)


Stability <- multinom(Vote_Choice ~ stability, data = CES) 

Stability %>% 
  stargazer::stargazer(type = "text")


Stability %>% 
  avg_predictions(type = "probs", by = "stability") %>% 
as.data.frame() %>% 
  ggplot(aes(x = estimate, col = stability,
             y = group, xmin = conf.low,
             xmax = conf.high
             )) +
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_linerange(position = position_dodge(width = 0.5)) + 
  theme_bw() + geom_vline(xintercept = 0, col = "grey", lty = 4)
  
  
  

structual_stability <- multinom(Vote_Choice ~ stability*Structural,
                                data = CES) 

structual_stability %>%  
  stargazer::stargazer(type = "text")

structual_stability %>% 
  avg_predictions(by = c("stability", "Structural")) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high,
             col = Structural, y = stability)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_linerange(position = position_dodge(width = 0.5)) + 
  facet_wrap(~group) + 
  theme_bw()

summary(model)
Handle_HealthcareC <- lm(Conf_Federal ~ health_care*Year + Age + as.factor(Province) + Gender,
              data = CES) 

Handle_Healthcare <- lm(Conf_Federal ~ health_care*Year,
                                data = CES) 

FrontlineC <- lm(Conf_Federal ~ frontline_workers*Year + Age + as.factor(Province) + Gender,
                        data = CES) 
 
Frontline <- lm(Conf_Federal ~ frontline_workers*Year,
                        data = CES) 
  
PoliceC <- lm(Conf_Federal ~ Police*Year + Age + as.factor(Province) + Gender,
                      data = CES)

Police <- lm(Conf_Federal ~ Police*Year,
                     data = CES) 





Handle_HealthcareC_df <- slopes(Handle_HealthcareC, 
                             variables = "health_care", 
                             by = "Year") %>% 
  mutate(Model = "Health Care Workers",
         Controls = "Controls")  


Handle_Healthcare_df <- slopes(Handle_Healthcare, 
                                variables = "health_care", 
                                by = "Year") %>% 
  mutate(Model = "Health Care Workers",
         Controls = "No Controls")  %>% 
  as.data.frame()


FrontlineC_df <- slopes(FrontlineC, 
                               variables = "frontline_workers", 
                               by = "Year") %>% 
  mutate(Model = "Essential Workers",
         Controls = "Controls")  %>% 
  as.data.frame()

Frontline_df <- slopes(Frontline, 
                        variables = "frontline_workers", 
                        by = "Year") %>% 
  mutate(Model = "Essential Workers",
         Controls = "No Controls")  %>% 
  as.data.frame()

PoliceC_df <- slopes(PoliceC, variables = "Police", by = "Year") %>% 
  mutate(Model = "Police Officers",
         Controls = "Controls")  %>% 
  as.data.frame()

Police_df <- slopes(Police, variables = "Police", by = "Year") %>% 
  mutate(Model = "Police Officers",
         Controls = "No Controls")  %>% 
  as.data.frame()

# police <- rbind(PoliceC_df, Police_df)
# police <- data.frame(term = c("Police", "Police"),
#                      contrast = NA, 
#                      Year = c(2021, 2021),
#                      estimate = police$estimate[c(2, 19)],
#                      conf.low = police$conf.low[c(2, 19)],
#                      conf.high = police$conf.high[c(2, 19)],
#                      Model = c("Police Officers", "Police Officers"),
#                      Controls = c("Controls", "No Controls")
#                      )

df <- rbind(PoliceC_df, Police_df,
            Handle_HealthcareC_df, Handle_Healthcare_df,
            FrontlineC_df, Frontline_df)

# df <- df %>% 
#   select(term, contrast, Year, estimate, conf.low, conf.high, Model, Controls)
# 
# df <- rbind(police, as.data.frame(df))

df %>% 
  ggplot(aes(x = estimate, y = Year,
             xmin = conf.low, xmax = conf.high,
             col = Controls)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_linerange(position = position_dodge(width = 0.5)) + 
  geom_vline(xintercept = 0, col = "grey", lty = 4) + 
  scale_colour_manual(values = c("darkblue", "orange")) +
  facet_wrap(~ Model, ncol = 2) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL)



  
  

