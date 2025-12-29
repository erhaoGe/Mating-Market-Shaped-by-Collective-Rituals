### These scripts reproduce the results reported in the main text of
### Ge et al., "Mating Markets Shaped by Collective Religious Rituals"

# Import the cleaned and processed dataset
Data_publication <- readRDS("Data_publication.RDS")

# Install and load required packages
packages <- c(
  "tidyverse",   
  "ggalluvial",
  "modelsummary",
  "survival",
  "survminer",
  "brms",
  "loo",
  "posterior",
  "bayesplot",
  "broom",
  "flextable",
  "officer",
  "marginaleffects",
  "patchwork"
)

install.packages(setdiff(packages, rownames(installed.packages())))

lapply(packages, library, character.only = TRUE)

###################################################################
### Part 1: Collective ritual participation and age at first marriage
###################################################################


########## 
# Figure 1
##########

freq_diff_long <- Data_publication %>%
  filter(MarriStatus == 1) %>% 
  mutate(
    Diff_Mouth    = if_else(FrequencyMouth    > 0,
                            Frequency_beforeMarriage_Mouth    - Frequency_afterMarriage_Mouth,
                            NA_real_),
    Diff_Back     = if_else(FrequencyBack     > 0,
                            Frequency_beforeMarriage_Back     - Frequency_afterMarriage_Back,
                            NA_real_),
    Diff_Cutting  = if_else(FrequencyCutting  > 0,
                            Frequency_beforeMarriage_Cutting  - Frequency_afterMarriage_Cutting,
                            NA_real_),
    Diff_Carrying = if_else(FrequencyCarrying > 0,
                            Frequency_beforeMarriage_Carrying - Frequency_afterMarriage_Carrying,
                            NA_real_)
  ) %>%
  select(starts_with("Diff_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Ritual",
    values_to = "Diff"
  ) %>%
  mutate(
    Ritual = gsub("Diff_", "", Ritual),
    Ritual = factor(Ritual,
                    levels = c("Mouth", "Back", "Cutting", "Carrying"),
                    labels = c("Mouth-piercing", "Back-piercing", 
                               "Forehead-cutting", "Statue-carrying"))
  )

ritual_means <- freq_diff_long %>%
  group_by(Ritual) %>%
  summarise(mean_diff = mean(Diff, na.rm = TRUE))


ggplot(freq_diff_long, aes(x = Diff)) +
  geom_bar(fill = "#1f77b4", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(data = ritual_means %>%
               filter(Ritual != "Gar-dancing"), aes(x = mean_diff, y = 0), 
             color = "red", size = 3) +
  facet_wrap(~ Ritual, ncol = 2, scales = "free_y") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10)) +
  labs(
    title = "Differences in Ritual Participation Frequency Before and After Marriage",
    x = "Difference in participation frequency",
    y = "Count"
  )


########## 
# Figure 2
##########

# Kaplan–Meier mean
fit_sex <- survfit(Surv(Surv_time, Surv_event) ~ Gender, 
                   data = Data_publication)

mean_ages <- summary(fit_sex)$table %>%
  as.data.frame() %>%
  mutate(Gender = rownames(summary(fit_sex)$table)) %>%
  mutate(Gender = gsub("Gender=", "", Gender)) %>% 
  select(Gender, rmean) %>%
  rename(mean_age = rmean)

ritual_sex <- tibble(
  Variable = c("Age_of_first_Mouth", 
               "Age_of_first_Back",
               "Age_of_first_Cutting", 
               "Age_of_first_Carrying"),
  Sex = "M"
)

facet_labels <- c(
  "Age_of_first_Mouth"    = "Mouth-piercing",
  "Age_of_first_Back"     = "Back-piercing",
  "Age_of_first_Cutting"  = "Forehead-cutting",
  "Age_of_first_Carrying" = "Statue-carrying"
)

ritual_order <- c(
  "Age_of_first_Mouth",
  "Age_of_first_Back",
  "Age_of_first_Cutting",
  "Age_of_first_Carrying"
)

Data_publication%>%
  pivot_longer(
    cols = all_of(ritual_order),
    names_to = "Variable",
    values_to = "Age_value"
  ) %>%
  filter(!is.na(Age_value)) %>%
  left_join(ritual_sex, by = "Variable") %>%
  left_join(mean_ages, by = "Gender") %>%   
  mutate(
    Variable = factor(Variable, levels = ritual_order)
  ) %>%
  ggplot(aes(x = Age_value)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "white") +
  geom_vline(
    aes(xintercept = mean_age),
    linetype = "dashed",
    size = 1,
    color = "blue"
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free_y",
    labeller = as_labeller(facet_labels)
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of the Age at First Ritual Participation Across Ritual Types",
    x = "Age at first ritual participation",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 10)
  )


########## 
# Table 1
##########

Suheri_survival_final4_cox_time_dependent_variable <- 
  Data_publication %>% 
  filter(Gender == "Male") %>% 
  mutate(
    Age_of_Mouth = strsplit(as.character(Age_of_Mouth), ","),
    Age_of_Back = strsplit(as.character(Age_of_Back), ","),
    Age_of_Cutting = strsplit(as.character(Age_of_Cutting), ","),
    Age_of_Carrying = strsplit(as.character(Age_of_Carrying), ","),
    Mouth_age = map(Age_of_Mouth, ~ as.numeric(.x)),
    Back_age = map(Age_of_Back, ~ as.numeric(.x)),
    Cutting_age = map(Age_of_Cutting, ~ as.numeric(.x)),
    Carrying_age = map(Age_of_Carrying, ~ as.numeric(.x)),
    
    Age_seq = map(Surv_time, ~ seq(14, .x - 1))
  ) %>%
  unnest(Age_seq) %>%
  mutate(
    start = Age_seq,
    stop = Age_seq + 1,
    event = ifelse(stop == Surv_time & Surv_event == 1, 1, 0),
    
    Participated_Mouth = map2_lgl(Mouth_age, start, ~ .y %in% .x),
    Participated_Back = map2_lgl(Back_age, start, ~ .y %in% .x),
    Participated_Cutting = map2_lgl(Cutting_age, start, ~ .y %in% .x),
    Participated_Carrying = map2_lgl(Carrying_age, start, ~ .y %in% .x)
  )

surv_obj <- Surv(time = Suheri_survival_final4_cox_time_dependent_variable$start,
                 time2 = Suheri_survival_final4_cox_time_dependent_variable$stop,
                 event = Suheri_survival_final4_cox_time_dependent_variable$event)

# Control model
cox_model8 <- coxph(surv_obj ~ 
                      WhichTribe +
                      EducationLevel+Totalsibling+Eco_Rank1+
                      cluster(ID),
                    ties = "efron", 
                    data = Suheri_survival_final4_cox_time_dependent_variable)

# Concurrent model
cox_model9 <- coxph(surv_obj ~ 
                      WhichTribe +
                      EducationLevel+Totalsibling+Eco_Rank1+
                      Participated_Mouth+
                      Participated_Back+
                      Participated_Cutting+
                      Participated_Carrying+
                      cluster(ID),
                    ties = "efron" , 
                    data = Suheri_survival_final4_cox_time_dependent_variable)

# Delayed effect of time-dependent variables
Suheri_survival_final4_cox_time_dependent_variable <- 
  Suheri_survival_final4_cox_time_dependent_variable %>% 
  mutate(
    Participated_Mouth_lag1 = map2_lgl(Mouth_age, start, ~ (.y - 1) %in% .x),
    Participated_Back_lag1 = map2_lgl(Back_age, start, ~ (.y - 1) %in% .x),
    Participated_Cutting_lag1 = map2_lgl(Cutting_age, start, ~ (.y - 1) %in% .x),
    Participated_Carrying_lag1 = map2_lgl(Carrying_age, start, ~ (.y - 1) %in% .x),
    Participated_Mouth_lag2 = map2_lgl(Mouth_age, start, ~ (.y - 2) %in% .x),
    Participated_Back_lag2= map2_lgl(Back_age, start, ~ (.y - 2) %in% .x),
    Participated_Cutting_lag2 = map2_lgl(Cutting_age, start, ~ (.y - 2) %in% .x),
    Participated_Carrying_lag2 = map2_lgl(Carrying_age, start, ~ (.y - 2) %in% .x),
    Participated_Mouth_lag3 = map2_lgl(Mouth_age, start, ~ (.y - 3) %in% .x),
    Participated_Back_lag3= map2_lgl(Back_age, start, ~ (.y - 3) %in% .x),
    Participated_Cutting_lag3 = map2_lgl(Cutting_age, start, ~ (.y - 3) %in% .x),
    Participated_Carrying_lag3 = map2_lgl(Carrying_age, start, ~ (.y - 3) %in% .x))

# 1 year delayed
cox_model_10<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + Totalsibling + Eco_Rank1 +
                       Participated_Mouth_lag1 +
                       Participated_Back_lag1 +
                       Participated_Cutting_lag1 +
                       Participated_Carrying_lag1+
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)

# 2 year delayed
cox_model_11<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + Totalsibling + Eco_Rank1 +
                       Participated_Mouth_lag2 +
                       Participated_Back_lag2 +
                       Participated_Cutting_lag2 +
                       Participated_Carrying_lag2+
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)

# 3 year delayed
cox_model_12<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + Totalsibling + Eco_Rank1 +
                       Participated_Mouth_lag3 +
                       Participated_Back_lag3 +
                       Participated_Cutting_lag3 +
                       Participated_Carrying_lag3+
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)
summary(cox_model_12)


# The AIC values of cox_model_11 and cox_model_12 are similar,
# suggesting no meaningful difference between the second- and third-year lag effects.
AIC(cox_model8,cox_model9,cox_model_10,cox_model_11,cox_model_12)


Suheri_survival_final4_cox_time_dependent_variable <- 
  Suheri_survival_final4_cox_time_dependent_variable %>%
  group_by(ID) %>%
  arrange(start, .by_group = TRUE) %>%
  mutate(
    Cum_Mouth = cumsum(Participated_Mouth),
    Cum_Back = cumsum(Participated_Back),
    Cum_Cutting = cumsum(Participated_Cutting),
    Cum_Carrying = cumsum(Participated_Carrying)
  ) %>%
  mutate(Delayed_Cum_Mouth = lag(Cum_Mouth, n = 2, default = 0),
         Delayed_Cum_Back = lag(Cum_Back, n = 2, default = 0),
         Delayed_Cum_Cutting = lag(Cum_Cutting, n = 2, default = 0),
         Delayed_Cum_Carrying = lag(Cum_Carrying, n = 2, default = 0)) %>% 
  ungroup()

# Cumulative effect
cox_model_13<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + Totalsibling + Eco_Rank1 +
                       Cum_Mouth +
                       Cum_Back +
                       Cum_Cutting +
                       Cum_Carrying+
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)
summary(cox_model_13)

# Cumulative effect + 2 year delayed
cox_model_14<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + Totalsibling + Eco_Rank1 +
                       Delayed_Cum_Mouth +
                       Delayed_Cum_Back +
                       Delayed_Cum_Cutting +
                       Delayed_Cum_Carrying +
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)
summary(cox_model_14)


### Model comparison
AIC(cox_model8,cox_model9,cox_model_10,
    cox_model_11,cox_model_13,cox_model_14)


# List of Cox proportional hazards models to be tested
cox_models <- list(
  cox_model8,
  cox_model9,
  cox_model_10,
  cox_model_11,
  cox_model_13,
  cox_model_14
)

# Proportional hazards (PH) assumption tests using Schoenfeld residuals
ph_tests <- lapply(cox_models, cox.zph)

# Print PH test results for all models
lapply(ph_tests, print)

# Plot Schoenfeld residuals for visual inspection
lapply(ph_tests, plot)

#Evaluation of the predictive performance of the selected
#best-fitting Cox model (cox_model_14).

lp <- predict(cox_model_14, type = "lp", 
              newdata = Suheri_survival_final4_cox_time_dependent_variable)

df_lp <- Suheri_survival_final4_cox_time_dependent_variable %>%
  mutate(lp = lp) %>%
  group_by(ID) %>%
  arrange(ID, stop) %>%
  summarise(lp_last = last(lp),      
            time_event = last(stop),  
            event = max(event)) %>%
  ungroup()

concordance_ind <- concordance(Surv(time_event, event) ~ lp_last, data = df_lp)
concordance_ind



###################################################################
### Part 2: Pre-marital collective ritual participation 
### and spouse geographic origin
###################################################################

# Assess missingness in the individual's birthplace variable by gender
# among married individuals
# (Male: 149 non-missing; Female: 162 non-missing)
Data_publication %>% 
  filter(MarriStatus == 1) %>% 
  count(Gender,is.na(Birthplace))

# Assess missingness in the spouse's birthplace variable by gender
# among married individuals
# (Male: 129 non-missing; Female: 131 non-missing)
Data_publication %>% 
  filter(MarriStatus == 1) %>% 
  count(Gender,is.na(Mate_BirthPlace))

Data_publication %>% 
  filter(MarriStatus == 1) %>%  
  count(Gender,is.na(Birthplace),is.na(Mate_BirthPlace))

 
# A total of 129 men have complete data on both the spouse’s birthplace
# and ritual participation measures
Data_publication %>% 
  filter(Gender == "Male") %>% 
  filter(MarriStatus == 1) %>% 
  count(is.na(BinaryMouth),is.na(Mate_BirthPlace))

# Most men are from the home village
Data_publication %>% 
  filter(Gender == "Male") %>% 
  filter(MarriStatus == 1) %>% 
  count(Birthplace) 


########## 
# Figure 3
##########
freq_vars <- c("Frequency_beforeMarriage_Mouth",
               "Frequency_beforeMarriage_Back",
               "Frequency_beforeMarriage_Cutting",
               "Frequency_beforeMarriage_Carrying")

ritual_order <- c("Mouth", "Back", "Cutting", "Carrying")


Data_publication %>%
  filter(Gender == "Male", MarriStatus == 1) %>%
  drop_na(Mate_BirthPlace) %>%  
  pivot_longer(
    cols = all_of(freq_vars),
    names_to = "Ritual",
    values_to = "Frequency"
  ) %>%
  mutate(
    Ritual = str_remove(Ritual, "Frequency_"),
    Ritual = str_remove(Ritual, "beforeMarriage_"),
    Ritual = recode(
      Ritual,
      "Mouth" = "Mouth-piercing",
      "Back" = "Back-piercing",
      "Cutting" = "Forehead-cutting",
      "Carrying" = "Statue-carrying"
    ),
    Ritual = factor(Ritual, levels = c(
      "Mouth-piercing",
      "Back-piercing",
      "Forehead-cutting",
      "Statue-carrying"
    ))
  ) %>% 
  ggplot(., aes(x = Mate_BirthPlace, y = Frequency, fill = Mate_BirthPlace)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) +  # Optional: show individual points
  facet_wrap(~ Ritual, nrow = 1) +
  labs(
    title = "Men’s Premarital Ritual Participation Across Spouses’ Birthplace Categories",
    x = "Spouse’s birthplace",
    y = "Premarital ritual participation frequency",
    fill = "Spouse’s birthplace"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(
    values = c(
      "Home village" = "#edf8fb",
      "Nearby villages" = "#b2e2e2",
      "Outside region" = "#2c7fb8"
    )
  )


# Significance tests
Data_publication %>%
  filter(Gender == "Male", MarriStatus == 1) %>%  
  drop_na(Mate_BirthPlace) %>%  
  pivot_longer(
    cols = all_of(freq_vars),
    names_to = "Ritual",
    values_to = "Frequency"
  ) %>%
  mutate(
    Ritual = str_remove(Ritual, "Frequency_"),
    Ritual = str_remove(Ritual, "beforeMarriage_"),
    Ritual = factor(Ritual, levels = ritual_order),  
  )  %>%
  group_split(Ritual) %>%
  map(~{
    cat("\n====", unique(.$Ritual), "====\n")
    print(kruskal.test(Frequency ~ Mate_BirthPlace, data = .))
  })

# Pairwise comparisons
Data_publication %>%
  filter(Gender == "Male", MarriStatus == 1) %>%  
  drop_na(Mate_BirthPlace) %>%  
  pivot_longer(
    cols = all_of(freq_vars),
    names_to = "Ritual",
    values_to = "Frequency"
  ) %>%
  mutate(
    Ritual = str_remove(Ritual, "Frequency_"),
    Ritual = str_remove(Ritual, "beforeMarriage_"),
    Ritual = factor(Ritual, levels = ritual_order),  
  )  %>%
  group_split(Ritual) %>%
  map(~{
    cat("\n====", unique(.$Ritual), "====\n")
    print(pairwise.wilcox.test(.$Frequency, .$Mate_BirthPlace,
                               p.adjust.method = "BH"))
  })


##########
# Bayesian ordinal regression
##########

# Check the outcome levels (ordered spouse birthplace categories)
levels(Data_publication$Mate_BirthPlace)

priors_ordinal <- c(
  # 1) Fixed-effect priors
  prior(normal(0, 1), class = "b"),  
  prior(normal(0, 0.312), class = "b", coef = "Frequency_beforeMarriage_Mouth"),
  prior(normal(0, 1.3),   class = "b", coef = "Frequency_beforeMarriage_Back"),
  prior(normal(0, 0.878), class = "b", coef = "Frequency_beforeMarriage_Cutting"),
  prior(normal(0, 0.253), class = "b", coef = "Frequency_beforeMarriage_Carrying"),
  
  # 2) Cutpoint (threshold) priors
  prior(student_t(3, 0, 10), class = "Intercept"))

# Prior predictive check to assess whether the priors are reasonable
fit_prior <- brm(
  formula = Mate_BirthPlace ~ Frequency_beforeMarriage_Mouth+
    Frequency_beforeMarriage_Back+
    Frequency_beforeMarriage_Cutting+
    Frequency_beforeMarriage_Carrying+
    Age + I(Age^2) +
    EducationLevel+Totalsibling+Eco_Rank1,
  family = cumulative("logit"),   
  data = Data_publication %>%
    filter(Gender == "Male", MarriStatus == 1),
  prior = priors_ordinal,
  sample_prior = "only", iter = 2000, chains = 4
)

pp_check(fit_prior)  



Mate_BirthPlace_Control_model <- brm(
  formula = Mate_BirthPlace ~ 
    Age + I(Age^2) +
    EducationLevel+
    Totalsibling+
    Eco_Rank1,
  family = cumulative("logit"),   
  data = Data_publication %>%
    filter(Gender == "Male", MarriStatus == 1),
  chains = 4, cores = 4, iter = 4000,
  prior = priors_ordinal, 
  control = list(adapt_delta = 0.95)
)

Mate_BirthPlace_model <- brm(
  formula = Mate_BirthPlace ~ 
    Frequency_beforeMarriage_Mouth+
    Frequency_beforeMarriage_Back+
    Frequency_beforeMarriage_Cutting+
    Frequency_beforeMarriage_Carrying+
    Age + I(Age^2) +
    EducationLevel+Totalsibling+Eco_Rank1,
  family = cumulative("logit"),  
  data = Data_publication %>%
    filter(Gender == "Male", MarriStatus == 1),
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95),
  prior = priors_ordinal, 
)

# Model diagnostics: trace plots for MCMC chains
mcmc_plot(Mate_BirthPlace_model, type = "trace")

# Compare against a model that relaxes the proportional-odds assumption
priors_ordinal2 <- c(
  prior(normal(0, 1), class = "b"),  
  prior(student_t(3, 0, 10), class = "Intercept")
)


Mate_BirthPlace_model_No_Proportional <- brm(
  formula = Mate_BirthPlace ~ mo(Frequency_beforeMarriage_Mouth)+
    mo(Frequency_beforeMarriage_Back)+
    mo(Frequency_beforeMarriage_Cutting)+
    mo(Frequency_beforeMarriage_Carrying)+
    Age + I(Age^2) +
    EducationLevel+Totalsibling+Eco_Rank1,
  family = cumulative("logit"),  
  data = Data_publication %>%
    filter(Gender == "Male", MarriStatus == 1),
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95),
  prior = priors_ordinal2)

# Model comparison using LOO cross-validation
Mate_BirthPlace_model <- add_criterion(Mate_BirthPlace_model, "loo")
Mate_BirthPlace_Control_model <- add_criterion(Mate_BirthPlace_Control_model, "loo")
Mate_BirthPlace_model_No_Proportional <- add_criterion(Mate_BirthPlace_model_No_Proportional, "loo")

loo_compare(loo(Mate_BirthPlace_model), loo(Mate_BirthPlace_Control_model))
loo_compare(loo(Mate_BirthPlace_model), loo(Mate_BirthPlace_model_No_Proportional))

# Inspect Pareto-k diagnostics for influential observations (PSIS-LOO)
loo1 <- loo(Mate_BirthPlace_model)
loo2 <- loo(Mate_BirthPlace_Control_model)
table(cut(loo1$diagnostics$pareto_k, breaks=c(-Inf,0.5,0.7,Inf)))
table(cut(loo2$diagnostics$pareto_k, breaks=c(-Inf,0.5,0.7,Inf)))

########## 
# Figure 4
##########
param_order <- c(
  "Intercept[1]",
  "Intercept[2]",
  "b_Frequency_beforeMarriage_Mouth",
  "b_Frequency_beforeMarriage_Back",
  "b_Frequency_beforeMarriage_Cutting",
  "b_Frequency_beforeMarriage_Carrying",
  "b_Age",
  "b_IAgeE2",       
  "b_EducationLevel",
  "b_Totalsibling",
  "b_Eco_Rank1L",
  "b_Eco_Rank1M"
)

pretty_labels <- c(
  "Intercept — threshold 1",
  "Intercept — threshold 2",
  "Freq: Mouth (pre-marriage)",
  "Freq: Back (pre-marriage)",
  "Freq: Cutting (pre-marriage)",
  "Freq: Carrying (pre-marriage)",
  "Age (linear)",
  "Age² (quadratic)",
  "Education level",
  "Total siblings",
  "Eco rank: Low (vs High)",
  "Eco rank: Medium (vs High)"
)


mcmc_intervals(
  Mate_BirthPlace_model,
  pars = param_order,
  prob = 0.5,
  prob_outer = 0.9,
  point_est = "mean"
) +
  scale_y_discrete(
    labels = setNames(pretty_labels, param_order),
    limits = rev(param_order)
  ) +
  labs(x = "Posterior estimate", y = NULL) +
  theme_minimal(base_size = 13)





