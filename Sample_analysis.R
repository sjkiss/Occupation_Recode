
library(haven)
library(tidyverse)
library(estimatr)
library(broom)
library(marginaleffects)
library(nnet)

# CES_2019_web <- read_dta("Data/ces2019_web_noc.dta")
# 
# CES_2021 <- read_dta("Data/ces2021_noc.dta")

# I have stored these data files in my own personal package that is stored on GitHub. 
#library(devtools)
#install_github("sjkiss/cesdata2")
library(cesdata2)

CES_2019_web <- ces19web %>% 
  select(pes19_conf_inst1_2, pes19_conf_inst1_1, cps19_issue_handle_1, pes19_conf_inst2_8, 
         pes19_age, cps19_gender, pes19_lang, cps19_province, pes19_votechoice2019,
         #Would it be better if we used the NOC21_4 codes? Would we have a bigger sample size?
         NOC21_5, NOC21_4) 


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
         NOC21_5, NOC21_4)


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




CES <- CES %>% 
  mutate(across(c(Conf_Province, Conf_Federal, Conf_police),
                \(x)replace(x, x == 5, NA)),
         across(c(Conf_Province, Conf_Federal, Conf_police),
                \(x)case_match(x, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)),
         Handle_healthcare = case_when(Handle_healthcare == 1 ~ 1,
                                       Handle_healthcare %in% c(2, 3, 4, 5) ~ 0,
                                       Handle_healthcare == 6 ~ NA),
         Right_wing = ifelse(Vote_Choice %in% c(2, 6), 1, ifelse(Vote_Choice %in% c(1, 3, 4, 5), 0, NA))
  )

CONTROLS <- c("Age", "Gender", "Province", "Year")

Stability <- lm(reformulate(c("stability2"), response = "Right_wing"), data = CES) 
Stability_2 <- lm(reformulate(c("stability2", CONTROLS), response = "Right_wing"), data = CES) 

library(modelsummary)
modelsummary(list(Stability, Stability_2), stars=T)
#stargazer::stargazer(list(Stability, Stability_2), type = "text")

library(marginaleffects)
Stability %>% 
  avg_predictions(type = "response", by = "stability") %>% 
  as_tibble() %>% 
  # mutate(Party = case_match(group,
  #                           "1" ~ "Liberal Party",
  #                           "2" ~ "Conservative Party",
  #                           "3" ~ "NDP",
  #                           "4" ~ "Bloc Quebecois",
  #                           "5" ~ "Green Party",
  #                           "6" ~ "People's Party (2019 Only)",
  #                           "7" ~ "Another Party"
  #                           
  # ),
  # Party = factor(Party, levels = c("Liberal Party", "Conservative Party", "NDP",
  #                                  "Bloc Quebecois", "Green Party", "People's Party (2019 Only)", "Another Party"))) %>%
  # filter(Party %in% c("Liberal Party", "Conservative Party", "NDP",
  #                     "Bloc Quebecois", "Green Party")) %>% 
  ggplot(aes(x = estimate, y = stability, xmin = conf.low,
             xmax = conf.high
  )) +
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_linerange(position = position_dodge(width = 0.5)) + 
  theme_bw() + geom_vline(xintercept = 0, col = "grey", lty = 4) + 
  theme(legend.position = "bottom") 
#  scale_color_manual(values=c("red", "darkblue", "orange", "lightblue", "green", "purple", "pink"))


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

Structural_Stability <- lm(reformulate(c("stability*Structural"), response = "Right_wing"), data = CES) 
Structural_Stability_2 <- lm(Right_wing ~ stability*Structural + Age + Gender + Province + Year, data = CES) 

modelsummary::modelsummary(list(Structural_Stability, Structural_Stability_2),
                           stars = TRUE)


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

Stability_df <- Stability_df %>% 
  rbind(Stability_2_df) 


Structural_Stability_df <-  Structural_Stability_df %>% 
  rbind(Structural_Stability_2_df) 


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

ggpubr::ggarrange(Stability_plot, Structural_Stability_plot, common.legend = TRUE,
                  nrow = 2, align = "hv", legend = "bottom") %>% 
  ggsave("plots/ocupation_stability.png", ., width = 6, height = 3)


Structural_Stability %>% 
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

