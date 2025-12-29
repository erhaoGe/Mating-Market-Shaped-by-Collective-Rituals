### The following code reproduces the analyses and results presented in the
### Supplementary Information of Ge et al., "Mating Markets Shaped by Collective
### Religious Rituals".
###
### Figure S1 is not produced by R code.

### IMPORTANT — EXECUTION ORDER:
### These Supplementary Information (SI) scripts depend on objects generated
### in the main-text analyses.
### Please run all main-text scripts FIRST, and only then run the SI scripts.

########## 
# Figure S2
##########

#p1
Suheri_survival_final4_long_Mouth <- Data_publication %>%
  filter(Gender == "Male") %>% 
  filter(BinaryMouth == 1) %>% 
  mutate(Age_of_Mouth = strsplit(as.character(Age_of_Mouth), ",")) %>%
  unnest(Age_of_Mouth) %>%
  mutate(Age_of_Mouth = as.numeric(Age_of_Mouth)) %>%
  mutate(Participated = 1)

df_heatmap1 <- expand.grid(
  ID = unique(Suheri_survival_final4_long_Mouth$ID),
  Age = min(Suheri_survival_final4_long_Mouth$Age_of_Mouth,
            Suheri_survival_final4_long_Mouth$FirstMarriageAge,na.rm = T):
    max(Suheri_survival_final4_long_Mouth$Age_of_Mouth,
        Suheri_survival_final4_long_Mouth$FirstMarriageAge,na.rm = T)
) %>%
  left_join(Suheri_survival_final4_long_Mouth %>% 
              select(ID, Age = Age_of_Mouth, Participated),
            by = c("ID", "Age")) %>%
  left_join(Data_publication %>% 
              select(ID, FirstMarriageAge),
            by = "ID") %>%
  mutate(
    Participated = ifelse(is.na(Participated), 0, Participated),
    MarriageMark = ifelse(Age == FirstMarriageAge, 1, 0), 
    Group = "Participated")


df_heatmap2 <- Data_publication %>%
  filter(Gender == "Male", 
         BinaryMouth == 0, !is.na(FirstMarriageAge)) %>%
  mutate(Age = FirstMarriageAge,
         Participated = 0,  
         MarriageMark = 1) %>% 
  mutate(Group = "Never participated")

df_heatmap_all <- bind_rows(df_heatmap1, df_heatmap2)

df_heatmap_all <- df_heatmap_all %>%
  mutate(ID_ordered = paste(Group, ID, sep = "_")) 

df_heatmap_all <- df_heatmap_all %>%
  mutate(ID_ordered = factor(ID_ordered, 
                             levels = c(
                               unique(df_heatmap_all %>% filter(Group=="Never participated") %>% pull(ID_ordered)),
                               unique(df_heatmap_all %>% filter(Group=="Participated") %>% pull(ID_ordered))
                             )))

ggplot(df_heatmap_all, aes(x = Age, y = ID_ordered)) +
  geom_tile(aes(fill = factor(Participated)), color = "white") +
  geom_tile(data = df_heatmap_all %>% filter(MarriageMark == 1),
            fill = NA, color = "red") +
  scale_fill_manual(values = c("0" = "white", "1" = "steelblue"),
                    name = "Participation",
                    labels = c("0" = "No", "1" = "Yes") ) +
  scale_x_continuous(
    breaks = seq(15, 45, by = 5)
  )  + 
  labs(
    title = "Mouth-Piercing",
    x = "Age",
    y = "Individuals",
    fill = "Participation"
  ) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5) 
  ) +
  guides(fill = guide_legend(override.aes = list(color = "black"))) -> p1

# p2
Suheri_survival_final4_long_Back <- Data_publication %>%
  filter(Gender == "Male") %>% 
  filter(BinaryBack == 1) %>% 
  mutate(Age_of_Back = strsplit(as.character(Age_of_Back), ",")) %>%
  unnest(Age_of_Back) %>%
  mutate(Age_of_Back = as.numeric(Age_of_Back)) %>%
  mutate(Participated = 1)

df_heatmap3 <- expand.grid(
  ID = unique(Suheri_survival_final4_long_Back$ID),
  Age = min(Suheri_survival_final4_long_Back$Age_of_Back,
            Suheri_survival_final4_long_Back$FirstMarriageAge,na.rm = T):
    max(Suheri_survival_final4_long_Back$Age_of_Back,
        Suheri_survival_final4_long_Back$FirstMarriageAge,na.rm = T)
) %>% 
  left_join(Suheri_survival_final4_long_Back %>% 
              select(ID, Age = Age_of_Back, Participated),
            by = c("ID", "Age")) %>%
  left_join(Data_publication %>% 
              select(ID, FirstMarriageAge),
            by = "ID") %>%
  mutate(
    Participated = ifelse(is.na(Participated), 0, Participated),
    MarriageMark = ifelse(Age == FirstMarriageAge, 1, 0), 
    Group = "Participated")

df_heatmap4 <- Data_publication %>%
  filter(Gender == "Male", 
         BinaryBack == 0, !is.na(FirstMarriageAge)) %>%
  mutate(Age = FirstMarriageAge,
         Participated = 0,  
         MarriageMark = 1) %>% 
  mutate(Group = "Never participated")

df_heatmap_all <- bind_rows(df_heatmap3, df_heatmap4)

df_heatmap_all <- df_heatmap_all %>%
  mutate(ID_ordered = paste(Group, ID, sep = "_")) 

df_heatmap_all <- df_heatmap_all %>%
  mutate(ID_ordered = factor(ID_ordered, 
                             levels = c(
                               unique(df_heatmap_all %>% filter(Group=="Never participated") %>% pull(ID_ordered)),
                               unique(df_heatmap_all %>% filter(Group=="Participated") %>% pull(ID_ordered))
                             )))

ggplot(df_heatmap_all, aes(x = Age, y = ID_ordered)) +
  geom_tile(aes(fill = factor(Participated)), color = "white") +
  geom_tile(data = df_heatmap_all %>% filter(MarriageMark == 1),
            fill = NA, color = "red") +
  scale_fill_manual(values = c("0" = "white", "1" = "steelblue"),
                    name = "Participation",
                    labels = c("0" = "No", "1" = "Yes") ) +
  scale_x_continuous(
    breaks = seq(15, 45, by = 5)
  )  + 
  labs(
    title = "Back-Piercing",
    x = "Age",
    y = "Individuals",
    fill = "Participation"
  ) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5) 
  ) +
  guides(fill = guide_legend(override.aes = list(color = "black"))) -> p2


#p3
Suheri_survival_final4_long_Cutting <- Data_publication %>%
  filter(Gender == "Male") %>% 
  filter(BinaryCutting == 1) %>% 
  mutate(Age_of_Cutting = strsplit(as.character(Age_of_Cutting), ",")) %>%
  unnest(Age_of_Cutting) %>%
  mutate(Age_of_Cutting = as.numeric(Age_of_Cutting)) %>%
  mutate(Participated = 1)

df_heatmap5 <- expand.grid(
  ID = unique(Suheri_survival_final4_long_Cutting$ID),
  Age = min(Suheri_survival_final4_long_Cutting$Age_of_Cutting,
            Suheri_survival_final4_long_Cutting$FirstMarriageAge,na.rm = T):
    max(Suheri_survival_final4_long_Cutting$Age_of_Cutting,
        Suheri_survival_final4_long_Cutting$FirstMarriageAge,na.rm = T)
) %>% 
  left_join(Suheri_survival_final4_long_Cutting %>% 
              select(ID, Age = Age_of_Cutting, Participated),
            by = c("ID", "Age")) %>%
  left_join(Data_publication %>% 
              select(ID, FirstMarriageAge),
            by = "ID") %>%
  mutate(
    Participated = ifelse(is.na(Participated), 0, Participated),
    MarriageMark = ifelse(Age == FirstMarriageAge, 1, 0),  
    Group = "Participated")

df_heatmap6 <- Data_publication %>%
  filter(Gender == "Male", 
         BinaryCutting == 0, !is.na(FirstMarriageAge)) %>%
  mutate(Age = FirstMarriageAge,
         Participated = 0,  
         MarriageMark = 1) %>% 
  mutate(Group = "Never participated")

df_heatmap_all <- bind_rows(df_heatmap5, df_heatmap6)

df_heatmap_all <- df_heatmap_all %>%
  mutate(ID_ordered = paste(Group, ID, sep = "_")) 

df_heatmap_all <- df_heatmap_all %>%
  mutate(ID_ordered = factor(ID_ordered, 
                             levels = c(
                               unique(df_heatmap_all %>% filter(Group=="Never participated") %>% pull(ID_ordered)),
                               unique(df_heatmap_all %>% filter(Group=="Participated") %>% pull(ID_ordered))
                             )))

ggplot(df_heatmap_all, aes(x = Age, y = ID_ordered)) +
  geom_tile(aes(fill = factor(Participated)), color = "white") +
  geom_tile(data = df_heatmap_all %>% filter(MarriageMark == 1),
            fill = NA, color = "red") +
  scale_fill_manual(values = c("0" = "white", "1" = "steelblue"),
                    name = "Participation",
                    labels = c("0" = "No", "1" = "Yes") ) +
  scale_x_continuous(
    breaks = seq(15, 45, by = 5)
  )  + 
  labs(
    title = "Forehead-Cutting",
    x = "Age",
    y = "Individuals",
    fill = "Participation"
  ) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5) 
  ) +
  guides(fill = guide_legend(override.aes = list(color = "black"))) -> p3

#p4
Suheri_survival_final4_long_Carrying <- Data_publication %>%
  filter(Gender == "Male") %>% 
  filter(BinaryCarrying == 1) %>% 
  mutate(Age_of_Carrying = strsplit(as.character(Age_of_Carrying), ",")) %>%
  unnest(Age_of_Carrying) %>%
  mutate(Age_of_Carrying = as.numeric(Age_of_Carrying)) %>%
  mutate(Participated = 1)

df_heatmap7 <- expand.grid(
  ID = unique(Suheri_survival_final4_long_Carrying$ID),
  Age = min(Suheri_survival_final4_long_Carrying$Age_of_Carrying,
            Suheri_survival_final4_long_Carrying$FirstMarriageAge,na.rm = T):
    max(Suheri_survival_final4_long_Carrying$Age_of_Carrying,
        Suheri_survival_final4_long_Carrying$FirstMarriageAge,na.rm = T)
) %>% 
  left_join(Suheri_survival_final4_long_Carrying %>% 
              select(ID, Age = Age_of_Carrying, Participated),
            by = c("ID", "Age")) %>%
  left_join(Data_publication %>% 
              select(ID, FirstMarriageAge),
            by = "ID") %>%
  mutate(
    Participated = ifelse(is.na(Participated), 0, Participated),
    MarriageMark = ifelse(Age == FirstMarriageAge, 1, 0),  
    Group = "Participated")


df_heatmap8 <- Suheri_survival_final4 %>%
  filter(Gender == "Male", 
         BinaryCarrying == 0, !is.na(FirstMarriageAge)) %>%
  mutate(Age = FirstMarriageAge,
         Participated = 0,  
         MarriageMark = 1) %>% 
  mutate(Group = "Never participated")

df_heatmap_all <- bind_rows(df_heatmap7, df_heatmap8)

df_heatmap_all <- df_heatmap_all %>%
  mutate(ID_ordered = paste(Group, ID, sep = "_")) 

df_heatmap_all <- df_heatmap_all %>%
  mutate(ID_ordered = factor(ID_ordered, 
                             levels = c(
                               unique(df_heatmap_all %>% filter(Group=="Never participated") %>% pull(ID_ordered)),
                               unique(df_heatmap_all %>% filter(Group=="Participated") %>% pull(ID_ordered))
                             )))

ggplot(df_heatmap_all, aes(x = Age, y = ID_ordered)) +
  geom_tile(aes(fill = factor(Participated)), color = "white") +
  geom_tile(data = df_heatmap_all %>% filter(MarriageMark == 1),
            fill = NA, color = "red") +
  scale_fill_manual(values = c("0" = "white", "1" = "steelblue"),
                    name = "Participation",
                    labels = c("0" = "No", "1" = "Yes") ) +
  scale_x_continuous(
    breaks = seq(15, 45, by = 5)
  )  + 
  labs(
    title = "Statue-Carrying",
    x = "Age",
    y = "Individuals",
    fill = "Participation"
  ) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5) 
  ) +
  guides(fill = guide_legend(override.aes = list(color = "black"))) -> p4


(p1 + p2)/(p3 + p4) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")  

########## 
# Figure S3
##########
ggplot(freq_long %>% filter(!grepl("Gar", as.character(Ritual))), 
       aes(x = Frequency, fill = Period)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 20, color = NA) +  
  facet_wrap(~ Ritual, 
             nrow = 2,    
             labeller = labeller(Ritual = ritual_labels)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "top"
  ) +
  labs(
    title = "Ritual frequency before vs after marriage",
    x = "Frequency",
    y = "Count",
    fill = "Period"
  ) +
  scale_fill_manual(values = c("Before marriage" = "#4e79a7",
                               "After marriage"  = "#f28e2b"))


########## 
# Figure S4
##########
ritual_labels <- c(
  "Mouth"    = "Mouth piercing",
  "Back"     = "Back piercing",
  "Cutting"  = "Forehead cutting",
  "Carrying" = "Statue carrying",
  "Gar"      = "Gar dance"
)

ritual_order <- c("Mouth", "Back", "Cutting", 
                  "Carrying", "Gar")

freq_long <- Data_publication %>%
  filter(MarriStatus == 1) %>%
  pivot_longer(
    cols = starts_with("Frequency_beforeMarriage") |
      starts_with("Frequency_afterMarriage"),
    names_to = "Variable",
    values_to = "Frequency"
  ) %>%
  
  mutate(
    Period = ifelse(grepl("beforeMarriage", Variable), 
                    "Before marriage", "After marriage"),
    Ritual = gsub("Frequency_beforeMarriage_|Frequency_afterMarriage_", "", Variable)
  ) %>%
  
  filter(
    (Ritual == "Mouth"    & FrequencyMouth    > 0) |
      (Ritual == "Back"     & FrequencyBack     > 0) |
      (Ritual == "Cutting"  & FrequencyCutting  > 0) |
      (Ritual == "Carrying" & FrequencyCarrying > 0) |
      (Ritual == "Gar"      & FrequencyGar      > 0)
  ) %>%
  
  mutate(
    Ritual = factor(
      Ritual,
      levels = ritual_order,
      labels = c("Mouth-piercing", "Back-piercing", 
                 "Forehead-cutting", "Statue-carrying", 
                 "Gar-dancing")
    )
  )

data_flow <- Data_publication %>%
  filter(Gender == "Male", MarriStatus == 1) %>%
  drop_na(Birthplace, Mate_BirthPlace) %>%
  mutate(
    Birthplace = factor(Birthplace, levels = c("Home village", "Nearby villages", "Outside region")),
    Mate_BirthPlace = factor(Mate_BirthPlace, levels = c("Home village", "Nearby villages", "Outside region"))
  ) %>%
  count(Birthplace, Mate_BirthPlace) 

data_flow$Birthplace <- factor(data_flow$Birthplace,
                               levels = c("Home village", "Nearby villages", "Outside region"))
data_flow$Mate_BirthPlace <- factor(data_flow$Mate_BirthPlace,
                                    levels = c("Home village", "Nearby villages", "Outside region"))

ggplot(data_flow,
       aes(axis1 = Birthplace,
           axis2 = Mate_BirthPlace,
           y = n)) +
  geom_alluvium(aes(fill = Birthplace), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 0.17, fill = "grey90", color = "grey30") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 4, family = "Times") +
  scale_x_discrete(
    limits = c("Men's birthplace", "Spouses' birthplace"),
    expand = c(0.15, 0.05)
  ) +
  scale_y_continuous(name = "Count") +
  scale_fill_brewer(
    type = "qual",
    palette = "Set2",
    name = "Men's birthplace"
  ) +
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    legend.position = "right",
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 13),
    plot.title = element_text(size = 16, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Marriage Flow Between Men's Birthplaces and Their Spouses' Birthplaces"
  )

########## 
# Figure S5
##########
ritual_vars <- c("whether_Mouth_beforeMarriage",
                 "whether_Back_beforeMarriage",
                 "whether_Cutting_beforeMarriage",
                 "whether_Carrying_beforeMarriage")

ritual_order <- c("Mouth", "Back", "Cutting", "Carrying")

Data_publication %>%
  filter(Gender == "Male", MarriStatus == 1) %>%  
  drop_na(Mate_BirthPlace) %>%  
  pivot_longer(
    cols = all_of(ritual_vars),
    names_to = "Ritual",
    values_to = "Participated"
  ) %>%
  mutate(
    Ritual = str_remove(Ritual, "whether_"),
    Ritual = str_remove(Ritual, "_beforeMarriage"),
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
    )),
    Participated = ifelse(Participated == 1, "Yes", "No")
  ) %>%
  ggplot(aes(x = Mate_BirthPlace, fill = Participated)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Ritual, nrow = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Men’s Proportional Participation in Premarital Rituals Across Spouses’ Birthplace Categories",
    x = "Spouse’s birthplace",
    y = "Proportion",
    fill = "Premarital participation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(
    values = c("No" = "#1f77b4", "Yes" = "#ff7f0e")
  )

# Significance tests:
Data_publication %>%
  filter(Gender == "Male", MarriStatus == 1) %>%  
  drop_na(Mate_BirthPlace) %>%  
  pivot_longer(
    cols = all_of(ritual_vars),
    names_to = "Ritual",
    values_to = "Participated"
  ) %>%
  mutate(
    Ritual = str_remove(Ritual, "whether_"),
    Ritual = str_remove(Ritual, "_beforeMarriage"),
    Ritual = factor(Ritual, levels = ritual_order),  
    Participated = ifelse(Participated == 1, "Yes", "No")
  ) %>%
  group_split(Ritual) %>%
  purrr::map(~{
    tab <- table(.$Mate_BirthPlace, .$Participated)
    cat("\n====", unique(.$Ritual), "====\n")
    print(fisher.test(tab))
  })


########## 
# Figure S6
##########
Mate_BirthPlace_model_adding_AFB <- brm(
  formula = Mate_BirthPlace ~ 
    Frequency_beforeMarriage_Mouth+
    Frequency_beforeMarriage_Back+
    Frequency_beforeMarriage_Cutting+
    Frequency_beforeMarriage_Carrying+
    Age + I(Age^2) + 
    FirstMarriageAge +
    EducationLevel+Totalsibling+Eco_Rank1,
  family = cumulative("logit"), 
  data = Data_publication %>%
    filter(Gender == "Male", MarriStatus == 1),
  chains = 4, cores = 4, iter = 4000,
  control = list(adapt_delta = 0.95),
  prior = priors_ordinal, 
)

mcmc_intervals(
  Mate_BirthPlace_model_adding_AFB,
  prob = 0.5,
  prob_outer = 0.9,
  point_est = "mean"
)

param_order <- c(
  "Intercept[1]",
  "Intercept[2]",
  "b_Frequency_beforeMarriage_Mouth",
  "b_Frequency_beforeMarriage_Back",
  "b_Frequency_beforeMarriage_Cutting",
  "b_Frequency_beforeMarriage_Carrying",
  "b_Age",
  "b_IAgeE2",  
  "b_FirstMarriageAge",
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
  "Age at first marriage",
  "Education level",
  "Total siblings",
  "Eco rank: Low (vs High)",
  "Eco rank: Medium (vs High)"
)

mcmc_intervals(
  Mate_BirthPlace_model_adding_AFB,
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


########## 
# Table S1-S2
##########
modelsummary(list(
  "Control" = cox_model8,
  "concurrent" = cox_model9,
  "1 year lag" = cox_model_10,
  "2 year lag" = cox_model_11,
  "3 year lag" = cox_model_12), stars = TRUE) %>% 
  tinytable::tt_save(., "cox_models_control_concurrent_3yearslag.docx")

########## 
# Table S3
##########
modelsummary(list(
  "Control" = cox_model8,
  "concurrent" = cox_model9,
  "1 year lag" = cox_model_10,
  "2 year lag" = cox_model_11,
  "Culmulative" = cox_model_13,
  "Culmulative + 2 year lag" = cox_model_14), stars = TRUE) %>% 
  tinytable::tt_save(., "cox_models_Culmulative_2yearlag.docx")


########## 
# Table S4
##########
cox_model_22<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + 
                       Totalsibling + Eco_Rank1 +
                       Delayed_Cum_Mouth +
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)
summary(cox_model_22)

cox_model_23<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + 
                       Totalsibling + Eco_Rank1 +
                       Delayed_Cum_Back +
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)
summary(cox_model_23)

cox_model_24<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + 
                       Totalsibling + Eco_Rank1 +
                       Delayed_Cum_Cutting +
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)
summary(cox_model_24)

cox_model_25<- coxph(Surv(start, stop, event) ~ 
                       WhichTribe + EducationLevel + 
                       Totalsibling + Eco_Rank1 +
                       Delayed_Cum_Carrying +
                       cluster(ID),
                     ties = "efron",
                     data = Suheri_survival_final4_cox_time_dependent_variable)
summary(cox_model_25)


########## 
# Table S5
##########
# Extract full posterior summaries from the brms model (including Rhat and ESS)

tidy_fixed2 <- as_draws_df(Mate_BirthPlace_model) %>%
  summarise_draws(
    mean, sd, q5 = ~quantile2(.x, 0.05),
    q95 = ~quantile2(.x, 0.95),
    rhat, ess_bulk, ess_tail
  ) %>%
  as_tibble() %>%
  filter(grepl("^b_", variable)) %>%
  mutate(
    `90% HPDI` = paste0(round(q5, 3), " , ", round(q95, 3))) %>%
  select(
    Variable = variable,
    Mean = mean,
    SD = sd,
    `90% HPDI`,
    Rhat = rhat,
    Bulk_ESS = ess_bulk,
    Tail_ESS = ess_tail
  )

ft <- tidy_fixed2 %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  flextable() %>%
  autofit() %>%
  set_caption("Posterior summaries for Bayesian ordinal logistic regression (fixed effects only)")

read_docx() %>%
  body_add_par("Table X. Posterior summaries (90% HPDIs)", style = "heading 2") %>%
  body_add_flextable(ft) %>%
  print(target = "Bayesian_ordinal_model_spouse_birthplace_premarital_ritual_frequency.docx")


########## 
# Table S6
##########
avg_slopes(
  Mate_BirthPlace_model,
  variables = "Frequency_beforeMarriage_Mouth",
  conf_level = 0.9)

avg_slopes(
  Mate_BirthPlace_model,
  variables = "Frequency_beforeMarriage_Back",
  conf_level = 0.9)

avg_slopes(
  Mate_BirthPlace_model,
  variables = "Frequency_beforeMarriage_Cutting",
  conf_level = 0.9)

avg_slopes(
  Mate_BirthPlace_model,
  variables = "Frequency_beforeMarriage_Carrying",
  conf_level = 0.9)