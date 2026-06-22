
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

##### Marriage and Occupation ####
female_atypical <- c("13",
  "70","80","90","213","22","421","72","73","74","75",
  "82","83","84","85","92","93"
)

male_atypical <- c(
  "313","32","33","422","441","45", "131"
)

ces21 <- ces21 %>%
  mutate(
    NOC_chr = as.character(NOC21_4),
    gender_atypical = case_when(
      cps21_genderid == 1 &
        str_starts(NOC_chr, str_c("^(", str_c(male_atypical, collapse = "|"), ")")) ~ 1,
      cps21_genderid == 2 &
        str_starts(NOC_chr, str_c("^(", str_c(female_atypical, collapse = "|"), ")")) ~ 1,
      TRUE ~ 0
    ),
    gender_stereotypical = case_when(
      cps21_genderid == 2 &
        str_starts(NOC_chr, str_c("^(", str_c(male_atypical, collapse = "|"), ")")) ~ 1,
      cps21_genderid == 1 &
        str_starts(NOC_chr, str_c("^(", str_c(female_atypical, collapse = "|"), ")")) ~ 1,
      TRUE ~ 0
    ),
    
    Gendered_occupation = case_when(gender_atypical == 1 ~ "Atypical",
                                    gender_stereotypical == 1 ~ "Stereotypical",
                                    TRUE ~ "Balanced"),
    Gendered_occupation = factor(Gendered_occupation, levels = c("Balanced", "Atypical", "Stereotypical"))
  )

ces21_straight <- ces21 %>% 
  filter(cps21_sexuality == 1)

ces21_straight <- ces21_straight %>% 
  filter(cps21_genderid %in% c(1, 2))

ces21_straight <- ces21_straight %>% 
  mutate(Married = ifelse(cps21_marital %in% c(1, 2), 1, 0))

ces19web <- ces19web %>% 
  mutate(
    NOC_chr = as.character(NOC21_4),
    gender_atypical = case_when(
      cps19_gender == 1 &
        str_starts(NOC_chr, str_c("^(", str_c(male_atypical, collapse = "|"), ")")) ~ 1,
      cps19_gender == 2 &
        str_starts(NOC_chr, str_c("^(", str_c(female_atypical, collapse = "|"), ")")) ~ 1,
      TRUE ~ 0
    ),
    gender_stereotypical = case_when(
      cps19_gender == 2 &
        str_starts(NOC_chr, str_c("^(", str_c(male_atypical, collapse = "|"), ")")) ~ 1,
      cps19_gender == 1 &
        str_starts(NOC_chr, str_c("^(", str_c(female_atypical, collapse = "|"), ")")) ~ 1,
      TRUE ~ 0
    ),
    
    Gendered_occupation = case_when(gender_atypical == 1 ~ "Atypical",
                                    gender_stereotypical == 1 ~ "Stereotypical",
                                    TRUE ~ "Balanced"),
    Gendered_occupation = factor(Gendered_occupation, levels = c("Balanced", "Atypical", "Stereotypical"))
  )

ces19_straight <- ces19web %>% 
  filter(cps19_sexuality == 1)

ces19_straight <- ces19_straight %>% 
  filter(cps19_gender %in% c(1, 2))


ces19_straight <- ces19_straight %>% 
  mutate(Married = ifelse(cps19_marital %in% c(1, 2), 1, 0))

#### Models ####

#### Base Models ####

base_gender_control_19 <- lm(Married ~ as.factor(gender_atypical) + cps19_gender, ces19_straight) 
base_gender_control_21 <- lm(Married ~ as.factor(gender_atypical) + cps21_genderid, ces21_straight) 


base_all_controls_19 <- lm(Married ~ as.factor(gender_atypical) + cps19_gender  + as.factor(income) + degree + age, ces19_straight) 
base_all_controls_21 <- lm(Married ~ as.factor(gender_atypical) + cps21_genderid  + as.factor(income) + degree + age, ces21_straight) 

base_gender_control_19_df <- tidy(base_gender_control_19, conf.int = TRUE) %>% 
  mutate(Year = 2019)
base_gender_control_21_df <- tidy(base_gender_control_21, conf.int = TRUE) %>% 
  mutate(Year = 2021)
base_all_controls_19_df <- tidy(base_all_controls_19, conf.int = TRUE) %>% 
  mutate(Year = 2019)
base_all_controls_21_df <- tidy(base_all_controls_21, conf.int = TRUE) %>% 
  mutate(Year = 2021)

base_models <- rbind(base_gender_control_19_df %>% mutate(Controls = "Gender Only"),
                     base_gender_control_21_df %>% mutate(Controls = "Gender Only"),
                     base_all_controls_19_df %>% mutate(Controls = "Demographic Controls"),
                     base_all_controls_21_df %>% mutate(Controls = "Demographic Controls"))

#### Graph the Base Model ####

base_models %>% 
  filter(term == "as.factor(gender_atypical)1") %>%
  mutate(term = replace(term, term == "as.factor(gender_atypical)1", "Employed in a Gender \n Atypical Occupation")) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, col = Controls, y = term)) +
    geom_point(position = position_dodge(width = 0.6)) + 
    geom_linerange(position = position_dodge(width = 0.6)) + 
  facet_wrap(~Year) + 
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_colour_manual(values = c("darkblue", "orange")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey30") +
  labs(x = "OLS Coefficent Estimate and 95% Confidence Intervals",
       y = NULL) + 
  guides(color = guide_legend(reverse = TRUE))

#### Gender Interaction Model ####

interact_gender_19 <- lm(Married ~ as.factor(gender_atypical) * cps19_gender, ces19_straight) 
interact_gender_21 <- lm(Married ~ as.factor(gender_atypical) * cps21_genderid, ces21_straight) 

interact_gender_19_df <- avg_slopes(interact_gender_19, variables = "gender_atypical", by = "cps19_gender") %>% 
  mutate(Controls = "No Controls",
         Year = 2019)
interact_gender_21_df <- avg_slopes(interact_gender_21, variables = "gender_atypical", by = "cps21_genderid") %>% 
  mutate(Controls = "No Controls",
         Year = 2021)  %>% 
  rename(cps19_gender = cps21_genderid)

interact_gender_control_19 <- lm(Married ~ as.factor(gender_atypical) * cps19_gender + as.factor(income) + degree + age, ces19_straight) 
interact_gender_control_21 <- lm(Married ~ as.factor(gender_atypical) * cps21_genderid + as.factor(income) + degree + age, ces21_straight) 


interact_gender_control_19_df <- avg_slopes(interact_gender_control_19, variables = "gender_atypical", by = "cps19_gender") %>% 
  mutate(Controls = "Demographic Controls",
         Year = 2019) 
interact_gender_control_21_df <- avg_slopes(interact_gender_control_21, variables = "gender_atypical", by = "cps21_genderid") %>% 
  mutate(Controls = "Demographic Controls",
         Year = 2021) %>% 
  rename(cps19_gender = cps21_genderid)

interaction_df <- rbind(interact_gender_19_df, 
                        interact_gender_21_df,
                        interact_gender_control_19_df,
                        interact_gender_control_21_df
                        ) %>% 
  as.data.frame()

#### Interaction Plot ####

interaction_df %>% 
  mutate(Gender = case_when(cps19_gender == 1 ~ "Man",
                            cps19_gender == 2 ~ "Woman")) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, col = Controls, y = Gender)) +
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  theme_bw() +
  facet_wrap(~Year) + 
  theme(legend.position = "bottom") + 
  scale_colour_manual(values = c("darkblue", "orange")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey30") +
  labs(x = "Marginal Effect of Being Employed in a Gender Atypical Role \n on Marriage Prospects by Gender",
       y = NULL) + 
  guides(color = guide_legend(reverse = TRUE))


#### Gender Stereotypical ####


base_stereo_19 <- lm(Married ~ Gendered_occupation + cps19_gender, ces19_straight) 
base_stereo_21 <- lm(Married ~ Gendered_occupation + cps21_genderid, ces21_straight) 


base_stereo_controls_19 <- lm(Married ~ Gendered_occupation + cps19_gender  + as.factor(income) + degree + age, ces19_straight) 
base_stereo_controls_21 <- lm(Married ~ Gendered_occupation + cps21_genderid  + as.factor(income) + degree + age, ces21_straight) 

base_stereo_19_df <- tidy(base_stereo_19, conf.int = TRUE) %>% 
  mutate(Year = 2019)
base_stereo_21_df <- tidy(base_stereo_21, conf.int = TRUE) %>% 
  mutate(Year = 2021)
base_stereo_controls_19_df <- tidy(base_stereo_controls_19, conf.int = TRUE) %>% 
  mutate(Year = 2019)
base_stereo_controls_21_df <- tidy(base_stereo_controls_21, conf.int = TRUE) %>% 
  mutate(Year = 2021)

base_stereo_models <- rbind(base_stereo_19_df %>% mutate(Controls = "Gender Only"),
                     base_stereo_21_df %>% mutate(Controls = "Gender Only"),
                     base_stereo_controls_19_df %>% mutate(Controls = "Demographic Controls"),
                     base_stereo_controls_21_df %>% mutate(Controls = "Demographic Controls"))

#### Graph the Base Model ####

base_stereo_models %>% 
  filter(term %in% c("Gendered_occupationAtypical",
                           "Gendered_occupationStereotypical")) %>%
  mutate(term = case_match(term, "Gendered_occupationAtypical" ~ "Employed in a Gender \n  Atypical Occupation \n (Ref. Balanced)",
                           "Gendered_occupationStereotypical" ~ "Employed in a Gender \n  Stereotypical Occupation"),
         term = factor(term, levels = rev(c("Employed in a Gender \n  Atypical Occupation \n (Ref. Balanced)",
                                            "Employed in a Gender \n  Stereotypical Occupation")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, col = Controls, y = term)) +
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  facet_wrap(~Year) + 
  theme_bw() +
  theme(legend.position = "bottom") + 
  scale_colour_manual(values = c("darkblue", "orange")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey30") +
  labs(x = "OLS Coefficent Estimate and 95% Confidence Intervals",
       y = NULL) + 
  guides(color = guide_legend(reverse = TRUE))

#### Gender Interaction Model ####

interact_stereo_19 <- lm(Married ~ Gendered_occupation * cps19_gender, ces19_straight) 
interact_stereo_21 <- lm(Married ~ Gendered_occupation * cps21_genderid, ces21_straight) 

interact_stereo_19_df <- avg_slopes(interact_stereo_19, variables = "Gendered_occupation", by = "cps19_gender") %>% 
  mutate(Controls = "No Controls",
         Year = 2019)
interact_stereo_21_df <- avg_slopes(interact_stereo_21, variables = "Gendered_occupation", by = "cps21_genderid") %>% 
  mutate(Controls = "No Controls",
         Year = 2021)  %>% 
  rename(cps19_gender = cps21_genderid)

interact_stereo_control_19 <- lm(Married ~ Gendered_occupation * cps19_gender + as.factor(income) + degree + age, ces19_straight) 
interact_stereo_control_21 <- lm(Married ~ Gendered_occupation * cps21_genderid + as.factor(income) + degree + age, ces21_straight) 

modelsummary::modelsummary(list(base_stereo_controls_19, base_stereo_controls_21,
                           interact_stereo_19, interact_stereo_21), stars = TRUE)

interact_stereo_control_19_df <- avg_slopes(interact_stereo_control_19, variables = "Gendered_occupation", by = "cps19_gender") %>% 
  mutate(Controls = "Demographic Controls",
         Year = 2019) 
interact_stereo_control_21_df <- avg_slopes(interact_stereo_control_21, variables = "Gendered_occupation", by = "cps21_genderid") %>% 
  mutate(Controls = "Demographic Controls",
         Year = 2021) %>% 
  rename(cps19_gender = cps21_genderid)

intereact_stereo_df <- rbind(interact_stereo_19_df, 
                        interact_stereo_21_df,
                        interact_stereo_control_19_df,
                        interact_stereo_control_21_df
) %>% 
  as.data.frame()

#### Interaction Plot ####

intereact_stereo_df %>% 
  #filter(Controls == "Demographic Controls") %>% 
  mutate(Gender = case_when(cps19_gender == 1 ~ "Man",
                            cps19_gender == 2 ~ "Woman")) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, col = Gender, y = contrast)) +
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  theme_bw() +
  facet_wrap(~Year + Controls) + 
  theme(legend.position = "bottom") + 
  scale_colour_manual(values = c("darkblue", "orange")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey30") +
  labs(x = "Marginal Effect of Being Employed in a Gender Atypical/Stereotypical Role \n on Marriage Prospects by Gender",
       y = NULL) + 
  guides(color = guide_legend(reverse = TRUE))


#### Gender Hostility ####

ces21_straight <- ces21_straight %>% 
  mutate(pes21_hostile2 = replace(pes21_hostile2, pes21_hostile2 == 6, NA),
         pes21_hostile4 = replace(pes21_hostile4, pes21_hostile4 == 6, NA),
         Gender_hostile = (pes21_hostile2 + pes21_hostile4) / 2 )


base_hostile <- lm(Gender_hostile ~ Gendered_occupation + cps21_genderid, ces21_straight) 
base_hostile_controls <- lm(Gender_hostile ~ Gendered_occupation + cps21_genderid  + as.factor(income) + degree + age, ces21_straight) 

base_hostile_df <- tidy(base_hostile, conf.int = TRUE)
base_hostile_controls_df <- tidy(base_hostile_controls, conf.int = TRUE)


base_hostile_models <- bind_rows(base_hostile_df %>% mutate(Controls = "Gender Only"),
                            base_hostile_controls_df %>% mutate(Controls = "Demographic Controls"))


base_hostile_models %>% 
  filter(term %in% c("Gendered_occupationAtypical",
                     "Gendered_occupationStereotypical")) %>%
  mutate(term = case_match(term, "Gendered_occupationAtypical" ~ "Employed in a Gender \n  Atypical Occupation \n (Ref. Balanced)",
                           "Gendered_occupationStereotypical" ~ "Employed in a Gender \n  Stereotypical Occupation"),
         term = factor(term, levels = rev(c("Employed in a Gender \n  Atypical Occupation \n (Ref. Balanced)",
                                            "Employed in a Gender \n  Stereotypical Occupation")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, col = Controls, y = term)) +
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  #facet_wrap(~Year) + 
  theme_bw() +
  theme(legend.position = "bottom") + 
  scale_colour_manual(values = c("darkblue", "orange")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey30") +
  labs(x = "OLS Coefficent Estimate and 95% Confidence Intervals",
       y = NULL) + 
  guides(color = guide_legend(reverse = TRUE))


interact_hostile <- lm(Gender_hostile ~ Gendered_occupation * cps21_genderid, ces21_straight) 


interact_hostile_df <- avg_slopes(interact_hostile, variables = "Gendered_occupation", by = "cps21_genderid") %>% 
  mutate(Controls = "No Controls")  %>% 
  rename(cps19_gender = cps21_genderid)

interact_hostile_control <- lm(Gender_hostile ~ Gendered_occupation * cps21_genderid + as.factor(income) + degree + age, ces21_straight) 


interact_hostile_control_df <- avg_slopes(interact_hostile_control, variables = "Gendered_occupation", by = "cps21_genderid") %>% 
  mutate(Controls = "Demographic Controls") %>% 
  rename(cps19_gender = cps21_genderid)

intereact_host_df <- rbind(interact_hostile_df, 
                           interact_hostile_control_df
) %>% 
  as.data.frame()

#### Interaction Plot ####

intereact_host_df %>% 
  #filter(Controls == "Demographic Controls") %>% 
  mutate(Gender = case_when(cps19_gender == 1 ~ "Man",
                            cps19_gender == 2 ~ "Woman")) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, col = Controls, y = contrast)) +
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  theme_bw() +
  facet_wrap(~Gender) + 
  theme(legend.position = "bottom") + 
  scale_colour_manual(values = c("darkblue", "orange")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey30") +
  labs(x = "Marginal Effect of Being Employed in a Gender Atypical/Stereotypical Role \n on Marriage Prospects by Gender",
       y = NULL) + 
  guides(color = guide_legend(reverse = TRUE))

##### Masculinity/Feminitiy ####


base_fem <- lm(pes21_feminine_1 ~ Gendered_occupation + cps21_genderid, ces21_straight) 
base_fem_controls <- lm(pes21_feminine_1 ~ Gendered_occupation + cps21_genderid  + as.factor(income) + degree + age, ces21_straight) 

base_fem_df <- tidy(base_fem, conf.int = TRUE) %>% mutate(Outcome = "Femininity")
base_fem_controls_df <- tidy(base_fem_controls, conf.int = TRUE) %>% mutate(Outcome = "Femininity")



base_masc <- lm(pes21_masculine_1 ~ Gendered_occupation + cps21_genderid, ces21_straight) 
base_masc_controls <- lm(pes21_masculine_1 ~ Gendered_occupation + cps21_genderid  + as.factor(income) + degree + age, ces21_straight) 

base_masc_df <- tidy(base_masc, conf.int = TRUE) %>% mutate(Outcome = "Masculinity")
base_masc_controls_df <- tidy(base_masc_controls, conf.int = TRUE)  %>% mutate(Outcome = "Masculinity")


base_gender_models <- bind_rows(base_fem_df %>% mutate(Controls = "Gender Only"),
                             base_fem_controls_df %>% mutate(Controls = "Demographic Controls"),
                             base_masc_df %>% mutate(Controls = "Gender Only"),
                             base_masc_controls_df %>% mutate(Controls = "Demographic Controls"))

base_gender_models %>% 
  filter(term %in% c("Gendered_occupationAtypical",
                     "Gendered_occupationStereotypical")) %>%
  mutate(term = case_match(term, "Gendered_occupationAtypical" ~ "Employed in a Gender \n  Atypical Occupation \n (Ref. Balanced)",
                           "Gendered_occupationStereotypical" ~ "Employed in a Gender \n  Stereotypical Occupation"),
         term = factor(term, levels = rev(c("Employed in a Gender \n  Atypical Occupation \n (Ref. Balanced)",
                                            "Employed in a Gender \n  Stereotypical Occupation")))) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, col = Controls, y = term)) +
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  facet_wrap(~Outcome) + 
  theme_bw() +
  theme(legend.position = "bottom") + 
  scale_colour_manual(values = c("darkblue", "orange")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey30") +
  labs(x = "OLS Coefficent Estimate and 95% Confidence Intervals",
       y = NULL) + 
  guides(color = guide_legend(reverse = TRUE))


interact_fem <- lm(pes21_feminine_1 ~ Gendered_occupation * cps21_genderid, ces21_straight) 
interact_masc <- lm(pes21_masculine_1 ~ Gendered_occupation * cps21_genderid, ces21_straight) 


interact_fem_df <- avg_slopes(interact_fem, variables = "Gendered_occupation", by = "cps21_genderid") %>% 
  mutate(Controls = "No Controls",
         Outcome = "Femininity")  %>% 
  rename(cps19_gender = cps21_genderid)

interact_masc_df <- avg_slopes(interact_masc, variables = "Gendered_occupation", by = "cps21_genderid") %>% 
  mutate(Controls = "No Controls",
         Outcome = "Masculinity")  %>% 
  rename(cps19_gender = cps21_genderid)

interact_fem_control <- lm(pes21_feminine_1 ~ Gendered_occupation * cps21_genderid + as.factor(income) + degree + age, ces21_straight) 
interact_masc_control <- lm(pes21_masculine_1 ~ Gendered_occupation * cps21_genderid + as.factor(income) + degree + age, ces21_straight) 


interact_fem_control_df <- avg_slopes(interact_fem_control, variables = "Gendered_occupation", by = "cps21_genderid") %>% 
  mutate(Controls = "Demographic Controls",
         Outcome = "Femininity") %>% 
  rename(cps19_gender = cps21_genderid)

interact_masc_control_df <- avg_slopes(interact_masc_control, variables = "Gendered_occupation", by = "cps21_genderid") %>% 
  mutate(Controls = "Demographic Controls",
         Outcome = "Masculinity") %>% 
  rename(cps19_gender = cps21_genderid)

intereact_gendered_df <- rbind(interact_masc_df, 
                           interact_fem_df,
                           interact_fem_control_df,
                           interact_masc_control_df
) %>% 
  as.data.frame()

#### Interaction Plot ####

intereact_gendered_df %>% 
  #filter(Controls == "Demographic Controls") %>% 
  mutate(Gender = case_when(cps19_gender == 1 ~ "Man",
                            cps19_gender == 2 ~ "Woman")) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, col = Controls, y = contrast)) +
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_linerange(position = position_dodge(width = 0.6)) + 
  theme_bw() +
  facet_grid(Gender ~ Outcome) + 
  theme(legend.position = "bottom") + 
  scale_colour_manual(values = c("darkblue", "orange")) +
  geom_vline(xintercept = 0, lty = 4, col = "grey30") +
  labs(x = "Marginal Effect of Being Employed in a Gender Atypical/Stereotypical Role \n on Marriage Prospects by Gender",
       y = NULL) + 
  guides(color = guide_legend(reverse = TRUE))

  