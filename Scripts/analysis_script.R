# Installing or loading packages
if (!require(pacman)) {
  install.packages(pacman)
  load("pacman")
}

p_load(dplyr, tidyr, here, afex, emmeans)

# Run this line of code to load data and helper functions
source(here::here("scripts/load_data.R"))

# In the functions.R script, each helper to perform the analysis function is explained in detail.
# experiment 1 ----

# N and % of trials excluded
report_trials(raw_all, experiment = "exp1")

# Irrelevant distraction
# RT:
aov_results(sum_data(raw_all, F, Type = "Irrelevant", experiment = "exp1") %>% select(-Manipulation),
            "RT", contrasts = T, Manipulation = F, with = F)

# ACC:
aov_results(sum_data(raw_all, T, Type = "Irrelevant", experiment = "exp1"),
            "ACC", contrasts = T, Manipulation = F, with = F)


# Relevant distraction
# RT
aov_results(sum_data(raw_all, F, Type = "Relevant", experiment = "exp1"),
            "RT", contrasts = T, Manipulation = F, with = F)
# ACC
aov_results(sum_data(raw_all, T, Type = "Relevant", experiment = "exp1"),
            "ACC", contrasts = T, Manipulation = F, with = F)

# experiment 2 ----
# N and % of trials excluded
report_trials(raw_all, experiment = "exp2")

# Subgroup
report_trials(raw_all[raw_all$Manipulation == "100",],
              experiment = "exp2") # 100 ms group
report_trials(raw_all[raw_all$Manipulation == "200",],
              experiment = "exp2") # 200 ms group


# Irrelevant distraction
# RT:
aov_results(sum_data(raw_all, F, Type = "Irrelevant", experiment = "exp2"),
            "RT", contrasts = T, Manipulation = T, with = F)
# ACC
aov_results(sum_data(raw_all, T, Type = "Irrelevant", experiment = "exp2"),
            "ACC", contrasts = T, Manipulation = T, with = F)


# Relevant distraction
# RT
aov_results(sum_data(raw_all, F, Type = "Relevant", experiment = "exp2"),
            "RT", contrasts = T, Manipulation = T, with = F)
# ACC
aov_results(sum_data(raw_all, T, Type = "Relevant", experiment = "exp2"),
            "ACC", contrasts = T, Manipulation = T, with = F)


# experiment 3 ----

# N and % of trials excluded
report_trials(raw_all, experiment = "exp3")

# Irrelevant distraction
# RT
(aovIrr_2_RT <- aov_results(sum_data(raw_all, F, Type = "Irrelevant",
                                     experiment = "exp3"),"RT", contrasts = T)) # Three-way interaction close to significance

# Mixed load
(aovIrr_2_RT <- aov_results(sum_data(raw_all, F, Type = "Irrelevant",
                                     experiment = "exp3") %>% filter(Manipulation == "Mixed"),
                            "RT", contrasts = T, Manipulation = F))

(aovIrr_2_RT <- aov_results(sum_data(raw_all, F, Type = "Irrelevant",
                                     experiment = "exp3") %>% filter(Manipulation == "Blocked"),
                            "RT", contrasts = T, Manipulation = F))


# Load x Manipulation interaction:
# Effect of manipulation on load 
emmeans(aovIrr_2_RT[["anova"]], ~ load | Manipulation) %>% pairs() 
emmeans(aovIrr_2_RT[["anova"]], ~ load | Manipulation) %>% pairs() %>% confint()

# Effect of load on manipulation:
emmeans(aovIrr_2_RT[["anova"]], ~ Manipulation | load) %>% pairs()
emmeans(aovIrr_2_RT[["anova"]], ~ Manipulation | load) %>% pairs() %>% confint()


# ACC
aov_results(sum_data(raw_all, T, Type = "Irrelevant", experiment = "exp3"),
            "ACC", contrasts = T)


# Load x Manipulation interaction:
emmeans(aovIrr_2_ACC[["anova"]], ~ load | Manipulation) %>% pairs()
emmeans(aovIrr_2_ACC[["anova"]], ~ load | Manipulation) %>% pairs() %>% confint()

emmeans(aovIrr_2_ACC[["anova"]], ~ Manipulation | Load) %>% pairs()
emmeans(aovIrr_2_ACC[["anova"]], ~ Manipulation | Load) %>% pairs() %>% confint()

# Relevant distraction
# RT
# Invalid vs valid
(aovR_2_RT_1 <- aov_results(sum_data(raw_all, F, Type = "Relevant", experiment = "exp3") %>% filter(Condition != "AbsentG"),
            "RT", contrasts = T, Manipulation = T, with = F))


# load x Manipulation interaction:
emmeans(aovR_2_RT_1[["anova"]], ~ load | Manipulation) %>% pairs()
emmeans(aovR_2_RT_1[["anova"]], ~ load | Manipulation) %>% pairs() %>% confint()

emmeans(aovR_2_RT_1[["anova"]], ~ Manipulation |load) %>% pairs()
emmeans(aovR_2_RT_1[["anova"]], ~ Manipulation | load) %>% pairs() %>% confint()

emmeans(aovR_2_RT_1[["anova"]], ~ Condition |Manipulation) %>% pairs()
emmeans(aovR_2_RT_1[["anova"]], ~ Condition | Manipulation) %>% pairs() %>% confint()

emmeans(aovR_2_RT_1[["anova"]], ~ Manipulation |Condition) %>% pairs()
emmeans(aovR_2_RT_1[["anova"]], ~ Manipulation |Condition) %>% pairs() %>% confint()


# Invalid vs absent
(aovR_2_RT_2 <- aov_results(sum_data(raw_all, F, Type = "Relevant", experiment = "exp3") %>% filter(Condition != "Valid"),
            "RT", contrasts = T,  Manipulation = T, with = F))

# load x Manipulation interaction:
emmeans(aovR_2_RT_2[["anova"]], ~ load | Manipulation) %>% pairs()
emmeans(aovR_2_RT_2[["anova"]], ~ load | Manipulation) %>% pairs() %>% confint()

emmeans(aovR_2_RT_2[["anova"]], ~ Manipulation |load) %>% pairs()
emmeans(aovR_2_RT_2[["anova"]], ~ Manipulation | load) %>% pairs() %>% confint()

# Valid vs absent
(aovR_2_RT_3 <- aov_results(sum_data(raw_all, F, Type = "Relevant", experiment = "exp3") %>% filter(Condition != "Invalid"),
            "RT", contrasts = T, Manipulation = T, with = F))

emmeans(aovR_2_RT_3[["anova"]], ~ load | Manipulation) %>% pairs()
emmeans(aovR_2_RT_3[["anova"]], ~ load | Manipulation) %>% pairs() %>% confint()

emmeans(aovR_2_RT_3[["anova"]], ~ Manipulation |load) %>% pairs()
emmeans(aovR_2_RT_3[["anova"]], ~ Manipulation | load) %>% pairs() %>% confint()


# ACC
# Invalid - Valid contrast
aov_results(sum_data(raw_all, T, Type = "Relevant", experiment = "exp3") %>% filter(Condition != "AbsentG"),
            "ACC", contrasts = T, Manipulation = T, with = F)

# Invalid - Absent contrast
aov_results(sum_data(raw_all, T, Type = "Relevant", experiment = "exp3") %>% filter(Condition != "Valid"),
            "ACC", contrasts = T, Manipulation = T, with = F)

# Valid - Absent contrast
aov_results(sum_data(raw_all, T, Type = "Relevant", experiment = "exp3") %>% filter(Condition != "Invalid"),
            "ACC", contrasts = T, Manipulation = T, with = F)

# experiment 4 ----

# N and % of trials excluded
report_trials(raw_all,experiment = "exp4")

# Irrelevant distraction
# RT
aov_results(sum_data(raw_all, F, Type = "Irrelevant", experiment = "exp4"), "RT", contrasts = T)

# Sig three-way interaction: ANOVA for each level of Manipulation
aov_results(sum_data(raw_all, F, Type = "Irrelevant", experiment = "exp4") %>% filter(Manipulation == "Diff"), "RT", contrasts = T,
            Manipulation = F) # Until response

aov_results(sum_data(raw_all, F, Type = "Irrelevant", experiment = "exp4") %>% filter(Manipulation == "Same"), "RT", contrasts = T,
            Manipulation = F) # 200 ms

# ACC
aov_results(sum_data(raw_all, T, Type = "Irrelevant", experiment = "exp4"), "ACC", contrasts = F)


# Relevant distraction
# RT
aov_results(sum_data(raw_all, F, Type = "Relevant", experiment = "exp4") %>% filter(Condition != "AbsentG"),
            "RT", contrasts = T, Manipulation = F) # Invalid - Valid contrast

aov_results(sum_data(raw_all, F, Type = "Relevant", experiment = "exp4") %>% filter(Condition != "Valid"),
            "RT", contrasts = T, Manipulation = F) # Invalid - Absent contrast

aov_results(sum_data(raw_all, F, Type = "Relevant", experiment = "exp4") %>% filter(Condition != "Invalid"),
            "RT", contrasts = T, Manipulation = F) # Absent - Valid contrast

# ACC
aov_results(sum_data(raw_all, T, Type = "Relevant", experiment = "exp4") %>% filter(Condition != "AbsentG"),
            "ACC", contrasts = F, Manipulation = F) # Invalid - Valid contrast

aov_results(sum_data(raw_all, T, Type = "Relevant", experiment = "exp4") %>% filter(Condition != "Valid"),
            "ACC", contrasts = F, Manipulation = F) # Invalid - Absent contrast

aov_results(sum_data(raw_all, T, Type = "Relevant", experiment = "exp4") %>% filter(Condition != "Invalid"),
            "ACC", contrasts = F, Manipulation = F) # Absent - Valid contrast


# Comparing irrelevant vs. relevant distraction:
# Generating the combined data frame. Peripheral is coded as "invalid" only for the purposes of this analysis. 
exp4_sum_rel_irr <- bind_rows(
  sum_data(raw_all, F, Type = "Relevant", experiment = "exp4", Manipulation = F) %>% filter(Condition != "Valid") %>%
    mutate(Type = "Relevant", Condition = ifelse(Condition == "AbsentG", "Absent", "Invalid")),
  sum_data(raw_all, F, Type = "Irrelevant", experiment = "exp4", Manipulation = F) %>% mutate(Type = "Irrelevant",
                                                                            Condition = ifelse(Condition == "Peripheral",
                                                                                               "Invalid", "Absent"))
)

(aov_irr_rel <- aov_ez("ID", "RT", exp4_sum_rel_irr, within = c("Type", "load", "Condition"),
       anova_table = list(es = "pes"))) # Three-way interaction is significant. 

emmeans(aov_irr_rel, pairwise~load) # Main effect of Load
emmeans(aov_irr_rel, pairwise~Condition) # Main effect of Condition (Distractor)