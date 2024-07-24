# loading all necessary packages ----
if (!require(pacman)) {
  install.packages(pacman)
  load("pacman")
}

p_load(rempsyc, dplyr, here, ggplot2, ggrain, tidyr, Rmisc, ggpubr)

# loading helper functions
source(here::here("scripts/functions.R"))

# globals ----
rel_colors <- c("darkblue", "darkorange")
bc_colors <- c("#ff3800", "#ffc000")
breaks <- seq(-300, 300, 100)
limits <- c(-300, 300)

# Experiment 1 ----
# This script creates all the figures that depend on the data. ?rl
rel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Relevant", experiment = "exp1"), "RT") %>% 
  mutate(Distraction = Invalid - Valid) %>% select(ID, load, Distraction)
irrel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Irrelevant", experiment = "exp1"), "RT") %>% 
  mutate(Distraction = Peripheral - Absent) %>% select(ID, load, Distraction)
all_effs <- rbind(rel_effs, irrel_effs) %>% mutate(Type = rep(c("Relevant", "Irrelevant"), each = nrow(.)/2)) 

all_effs$load <- factor(all_effs$load, levels = c("low", "high"))

#summarySEwithin(data = all_effs, measurevar = "Distraction", idvar = "ID", withinvars = c("load", "Type")) -> se
summarySE(data = all_effs, measurevar = "Distraction", groupvars  = c("load", "Type")) -> se

all_effs$ID <- paste0(all_effs$ID, all_effs$Type)

ggplot(all_effs, aes(load, Distraction, fill = Type, color = Type)) +
  geom_rain(rain.side = 'f2x2', id.long.var = "ID",
            point.args = list(alpha = .2),
            violin.args = list(adjust = 1.2, alpha = .5, trim = F),
            boxplot.args = list(alpha = .5)) +
  geom_errorbar(data = se, aes(x = load, ymin = Distraction - ci, ymax = Distraction + ci, color = Type), width = .05,
                position = position_dodge(width = .05))+
  geom_point(data = se, aes(load, Distraction, color = Type), position = position_dodge(width = .05)) +
  geom_line(data = se, aes(load, Distraction, color = Type, group = Type), position = position_dodge(width = .05)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Load", y = "Distractor effect (ms)")+
  scale_x_discrete(labels = c("Low", "High")) +
  scale_fill_manual(values=rel_colors) +
  scale_color_manual(values=rel_colors) +
  scale_y_continuous(limits = limits, breaks = breaks) +
  guides(fill=guide_legend(title="Type of Distractor"),
         color=guide_legend(title="Type of Distractor"))+
  theme_Publication(base_size = 18) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 20))

ggsave(here::here("plots/figure3.png"), height = 7, width = 10, dpi = 1200)


# Experiment 2 -----
rel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Relevant", experiment = "exp2"), "RT")%>% 
  mutate(Distraction = Invalid - Valid) %>% select(ID,Manipulation, load, Distraction)
irrel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Irrelevant", experiment = "exp2"), "RT") %>% 
  mutate(Distraction = Peripheral - Absent) %>% select(ID,Manipulation, load, Distraction)
all_effs <- rbind(rel_effs, irrel_effs) %>% mutate(Type = rep(c("Relevant", "Irrelevant"), each = nrow(.)/2)) 

all_effs$load <- factor(all_effs$load, levels = c("low", "high"))

#summarySEwithin(data = all_effs, measurevar = "Distraction", idvar = "ID", withinvars = c("load", "Type")) -> se
summarySE(data = all_effs, measurevar = "Distraction", groupvars  = c("Manipulation", "load", "Type")) -> se

all_effs$ID <- paste0(all_effs$ID, all_effs$Type, all_effs$Manipulation)
all_effs$`Time exposure` <- paste(all_effs$Manipulation, "ms")
se$`Time exposure` <- paste(se$Manipulation, "ms")

ggplot(all_effs, aes(x=load, y=Distraction, fill = Type, color = Type)) +
  geom_rain(rain.side = 'f', id.long.var = "ID",
            point.args = list(alpha = .2),
            violin.args = list(adjust = 1.2, alpha = .5, trim = F),
            boxplot.args = list(alpha = .5, outlier.shape = NA),
            boxplot.args.pos = list(width = .1, 
                                    # A hack to use rainclouds with facets
                                    position = ggpp::position_dodgenudge(x = c(-.13, -.13, .13, .13, 
                                                                               -.13, -.13, .13, .13))), 
            violin.args.pos = list(width = .7,
                                   position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2), 
                                                                   rep(-.2, 256*2), rep(-.2, 256*2),
                                                                   rep(.2, 256*2), rep(.2, 256*2),
                                                                   rep(.2, 256*2), rep(.2, 256*2))))) + 
  geom_errorbar(data = se, aes(x = load, ymin = Distraction - ci, ymax = Distraction + ci, color = Type), width = .05,
                position = position_dodge(width = .05))+
  geom_point(data = se, aes(load, Distraction, color = Type), position = position_dodge(width = .05)) +
  geom_line(data = se, aes(load, Distraction, color = Type, group = Type), position = position_dodge(width = .05)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Load", y = "Distractor effect (ms)")+
  scale_x_discrete(labels = c("Low", "High")) +
  scale_fill_manual(values=rel_colors) +
  scale_color_manual(values=rel_colors) +
  scale_y_continuous(limits = limits, breaks = breaks) +
  guides(fill=guide_legend(title="Type of Distractor"),
         color=guide_legend(title="Type of Distractor"))+
  facet_wrap(~`Time exposure`) +
  theme_Publication(base_size = 15) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 15))

ggsave(here::here("plots/figure4.png"), height = 7, width = 10, dpi = 1200)

# Experiment 3 ----

rel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Relevant", experiment = "exp3"), "RT")%>% 
  mutate(Distraction = Invalid - Valid, Type = "Relevant") %>% select(ID,Manipulation, load, Distraction, Type)
irrel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Irrelevant", experiment = "exp3"), "RT") %>% 
  mutate(Distraction = Peripheral - Absent, Type = "Irrelevant") %>% select(ID,Manipulation, load, Distraction, Type)
all_effs <- rbind(rel_effs, irrel_effs)

all_effs$load <- factor(all_effs$load, levels = c("low", "high"))

summarySE(data = all_effs, measurevar = "Distraction", groupvars  = c("Manipulation", "load", "Type")) -> se

all_effs$Load <- ifelse(all_effs$Manipulation == "Blocked", "Blocked load", "Mixed load")
all_effs$ID <- paste0(all_effs$ID, all_effs$Type, all_effs$Load)
se$Load <- ifelse(se$Manipulation == "Blocked", "Blocked load", "Mixed load")

ggplot(all_effs, aes(x=load, y=Distraction, fill = Type, color = Type)) +
  geom_rain(rain.side = 'f', id.long.var = "ID",
            point.args = list(alpha = .2),
            violin.args = list(adjust = 1.2, alpha = .5, trim = F),
            boxplot.args = list(alpha = .5, outlier.shape = NA),
            boxplot.args.pos = list(width = .1,
                                    # The same hack as figure 4
                                    position = ggpp::position_dodgenudge(x = c(-.13, -.13, .13, .13, 
                                                                               -.13, -.13, .13, .13))), 
            violin.args.pos = list(width = .7,
                                   position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2), 
                                                                   rep(-.2, 256*2), rep(-.2, 256*2),
                                                                   rep(.2, 256*2), rep(.2, 256*2),
                                                                   rep(.2, 256*2), rep(.2, 256*2))))) + 
  geom_errorbar(data = se, aes(x = load, ymin = Distraction - ci, ymax = Distraction + ci, color = Type), width = .05,
                position = position_dodge(width = .05))+
  geom_point(data = se, aes(load, Distraction, color = Type), position = position_dodge(width = .05)) +
  geom_line(data = se, aes(load, Distraction, color = Type, group = Type), position = position_dodge(width = .05)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Load", y = "Distractor effect (ms)")+
  scale_x_discrete(labels = c("Low", "High")) +
  scale_fill_manual(values=rel_colors) +
  scale_color_manual(values=rel_colors) +
  scale_y_continuous(limits = limits, breaks = breaks) +
  guides(fill=guide_legend(title="Type of Distractor"),
         color=guide_legend(title="Type of Distractor"))+
  facet_wrap(~Load) +
  theme_Publication(base_size = 15) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 15))

ggsave(here::here("plots/figure5.png"), height = 7, width = 10, dpi = 1200)

# Absent baseline:
rel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Relevant", experiment = "exp3"), "RT")%>% 
  mutate(`valid vs. absent` = Valid-AbsentG, Type = "Relevant",
         `invalid vs. absent` = Invalid-AbsentG) %>% select(ID, Manipulation, load, `valid vs. absent`, `invalid vs. absent`, Type)%>%
  pivot_longer(cols = 4:5, names_to = "Contrast", values_to = "Distraction") %>%
  mutate(Contrast = ifelse(Contrast == "invalid vs. absent", "Costs", "Benefits"),
         grouping_var = interaction(ID, Manipulation))
rel_effs$load <- factor(rel_effs$load, levels = c("low", "high"))

summarySE(data = rel_effs, measurevar = "Distraction", groupvars  = c("Manipulation", "load", "Contrast")) -> se

rel_effs$load_line <- (as.numeric(factor(rel_effs$load))) # This works!
rel_effs$load_line <- jitter(rel_effs$load_line, .3)
se$Load <- se$Manipulation


block_p <- ggplot(rel_effs[rel_effs$Manipulation == "Blocked",], aes(x=load, y=Distraction, color = Contrast, fill = Contrast))+
  geom_blank()+
  geom_point(aes(x=load_line), alpha = .3)+
  geom_line(aes(x = load_line, group = grouping_var),
            alpha = .3) +
  geom_bar(data = se[se$Manipulation == "Blocked",], aes(x = load, y = Distraction), stat="identity",
           position = position_dodge(.6), width = .5, alpha = .3)+
  geom_line(data = se[se$Manipulation == "Blocked",], aes(x = load, y = Distraction, group = Manipulation), stat="identity",
            position = position_dodge(.6))+
  facet_wrap(~factor(Contrast, levels = c("Costs", "Benefits")), scales = "free_x")+
  geom_errorbar(data = se, aes(x = load, ymin = Distraction - ci, ymax = Distraction + ci), width = .05,
                position = position_dodge(width = .6))+
  labs(x = "Load", y = "Difference from absent condition (ms)", linetype = "Load type")+
  scale_x_discrete(labels = c("Low", "High")) +
  scale_fill_manual(values=bc_colors) +
  scale_color_manual(values=bc_colors) +
  scale_y_continuous(breaks = breaks) +
  coord_cartesian(ylim = limits) +
  theme_Publication(base_size = 15)+
  theme(strip.text.x = element_text(size = 15),
        legend.position = "none")

mixed_p <- ggplot(rel_effs[rel_effs$Manipulation == "Mixed",], aes(x=load, y=Distraction, color = Contrast, fill = Contrast))+
  geom_blank()+
  geom_point(aes(x=load_line), alpha = .3)+
  geom_line(aes(x = load_line, group = grouping_var),
            alpha = .3) +
  geom_bar(data = se[se$Manipulation == "Mixed",], aes(x = load, y = Distraction), stat="identity",
           position = position_dodge(.6), width = .5, alpha = .3)+
  geom_line(data = se[se$Manipulation == "Mixed",], aes(x = load, y = Distraction, group = Manipulation), stat="identity",
            position = position_dodge(.6))+
  facet_wrap(~factor(Contrast, levels = c("Costs", "Benefits")), scales = "free_x")+
  geom_errorbar(data = se, aes(x = load, ymin = Distraction - ci, ymax = Distraction + ci), width = .05,
                position = position_dodge(width = .6))+
  labs(x = "Load", y = "Difference from absent condition (ms)", linetype = "Load type")+
  scale_x_discrete(labels = c("Low", "High")) +
  scale_fill_manual(values=bc_colors) +
  scale_color_manual(values=bc_colors) +
  scale_y_continuous(breaks = breaks) +
  coord_cartesian(ylim = limits) +
  theme_Publication(base_size = 15)+
  theme(strip.text.x = element_text(size = 15),
        legend.position = "none")

ggarrange(block_p, mixed_p, nrow = 2, labels = c("Blocked Load", "Mixed Load"), label.x = c(0.37, 0.39), # Adjust labels for central position 
          font.label = list(size = 18))

ggsave(here::here("plots/figure7.png"), height = 12, width = 10, dpi = 1200)

# Experiment 4 ----
rel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Relevant", experiment = "exp4"), "RT")%>% 
  mutate(Distraction = Invalid - Valid, Type = "Relevant") %>% select(ID,Manipulation, load, Distraction, Type)
irrel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Irrelevant", experiment = "exp4"), "RT") %>% 
  mutate(Distraction = Peripheral - Absent, Type = "Irrelevant") %>% select(ID,Manipulation, load, Distraction, Type)
all_effs <- rbind(rel_effs, irrel_effs)

all_effs$load <- factor(all_effs$load, levels = c("low", "high"))
#summarySEwithin(data = all_effs, measurevar = "Distraction", idvar = "ID", withinvars = c("load", "Type")) -> se
summarySE(data = all_effs, measurevar = "Distraction", groupvars  = c("Manipulation", "load", "Type")) -> se

all_effs$Load <- all_effs$Manipulation
all_effs$ID <- paste0(all_effs$ID, all_effs$Type, all_effs$Load)
se$Load <- se$Manipulation

all_effs$`Distractor time exposure` <- ifelse(all_effs$Load == "No" | all_effs$Load == "Same", "200 ms", "Until response")
se$`Distractor time exposure` <- ifelse(se$Load == "No" | se$Load == "Same","200 ms", "Until response")
all_effs$`Distractor time exposure` <- factor(all_effs$`Distractor time exposure`, levels = c("200 ms", "Until response"))
se$`Distractor time exposure` <- factor(se$`Distractor time exposure`, levels = c("200 ms", "Until response"))

ggplot(all_effs, aes(x=load, y=Distraction, fill = Type, color = Type)) +
  geom_rain(rain.side = 'f', id.long.var = "ID", 
            point.args = list(alpha = .2),
            violin.args = list(adjust = 1.2, alpha = .5, trim = F),
            boxplot.args = list(alpha = .5, outlier.shape = NA),
            boxplot.args.pos = list(width = .1, 
                                    position = ggpp::position_dodgenudge(x = c(-.13, -.13, .13, .13, 
                                                                               -.13, .13, -.13, -.13))), 
            violin.args.pos = list(width = .7,
                                   position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2), 
                                                                   rep(-.2, 256*2), rep(.2, 256*2),
                                                                   rep(.2, 256*2), rep(.2, 256*2))))) + 
  geom_errorbar(data = se, aes(x = load, ymin = Distraction - ci, ymax = Distraction + ci, color = Type), width = .05,
                position = position_dodge(width = .05))+
  geom_point(data = se, aes(load, Distraction, color = Type), position = position_dodge(width = .05)) +
  geom_line(data = se, aes(load, Distraction, color = Type, group = Type), position = position_dodge(width = .05)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Load", y = "Distractor effect (ms)")+
  scale_x_discrete(labels = c("Low", "High")) +
  scale_fill_manual(values=rel_colors) +
  scale_color_manual(values=rel_colors) +
  scale_y_continuous(limits = limits, breaks = breaks) +
  guides(fill=guide_legend(title="Type of Distractor"),
         color=guide_legend(title="Type of Distractor"))+
  facet_wrap(~`Distractor time exposure`) +
  theme_Publication(base_size = 15)+
  theme(strip.text = element_text(size = 15))


ggsave(here::here("plots/figure8.png"), height = 7, width = 12, dpi = 1200)

# Absent baseline:
rel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Relevant", experiment = "exp4"), "RT")%>% 
  mutate(`valid vs. absent` = Valid-AbsentG, Type = "Relevant",
         `invalid vs. absent` = Invalid-AbsentG) %>% select(ID, load, `valid vs. absent`, `invalid vs. absent`, Type)%>%
  pivot_longer(cols = 3:4, names_to = "Contrast", values_to = "Distraction") %>%
  mutate(Contrast = ifelse(Contrast == "invalid vs. absent", "Costs", "Benefits"))
rel_effs$load <- factor(rel_effs$load, levels = c("low", "high"))

summarySE(data = rel_effs, measurevar = "Distraction", groupvars  = c("load", "Contrast")) -> se

rel_effs$load_line <- as.numeric(factor(rel_effs$load)) # This works!
rel_effs$load_line <- jitter(rel_effs$load_line, .3)

ggplot(rel_effs, aes(x=load, y=Distraction, color = Contrast, fill = Contrast))+
  geom_blank()+
  geom_line(aes(x = load_line, group = ID),
            alpha = .3) +
  geom_point(aes(x=load_line), alpha = .3)+
  geom_bar(data = se, aes(x = load, y = Distraction), stat="identity", width = .5, alpha = .3)+
  geom_line(data = se, aes(x = load, y = Distraction, group = Contrast), stat="identity")+
  facet_wrap(~factor(Contrast, levels = c("Costs", "Benefits")))+
  geom_errorbar(data = se, aes(x = load, ymin = Distraction - ci, ymax = Distraction + ci), width = .05)+
  labs(x = "Load", y = "Difference from absent condition (ms)")+
  scale_x_discrete(labels = c("Low", "High")) +
  scale_fill_manual(values=bc_colors) +
  scale_color_manual(values=bc_colors) +
  scale_y_continuous(limits = limits, breaks = breaks) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_Publication(base_size = 15)+
  theme(strip.text = element_text(size = 15),
        legend.position  = "none")

ggsave(here::here("plots/figure9.png"), height = 7, width = 10, dpi = 1200)

# Relevant cost vs irrelevant cost:
rel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Relevant", experiment = "exp4"), "RT")%>% 
  mutate(`valid vs. absent` = Valid-AbsentG, Type = "Relevant",
         `invalid vs. absent` = Invalid-AbsentG) %>% select(ID, load, `valid vs. absent`, `invalid vs. absent`, Type)%>%
  pivot_longer(cols = 3:4, names_to = "Contrast", values_to = "Distraction") %>%
  mutate(Contrast = ifelse(Contrast == "invalid vs. absent", "Relevant costs", "Benefits"))

irrel_effs <- get_effects(sum_data(raw_all, ACC = F, Type = "Irrelevant", experiment = "exp4", Manipulation = F), "RT") %>% 
  mutate(Distraction = Peripheral - Absent,Type = "Relevant", Contrast = "Irrelevant costs") %>% select(ID, load,Type, Contrast,Type, Distraction)

effs <- rbind(rel_effs, irrel_effs) %>% filter(Contrast != "Benefits")
effs$load <- factor(effs$load, levels = c("low", "high"))

summarySE(data = effs, measurevar = "Distraction", groupvars  = c("load", "Contrast")) -> se

effs$load_line <- as.numeric(factor(effs$load)) # This works!
effs$load_line <- jitter(effs$load_line, .3)


ggplot(effs, aes(x=load, y=Distraction, color = Contrast, fill = Contrast))+
  geom_blank()+
  geom_line(aes(x = load_line, group = ID),
            alpha = .3) +
  geom_point(aes(x=load_line), alpha = .3)+
  geom_bar(data = se, aes(x = load, y = Distraction), stat="identity", width = .5, alpha = .3)+
  geom_line(data = se, aes(x = load, y = Distraction, group = Contrast), stat="identity")+
  facet_wrap(~factor(Contrast, levels = c("Irrelevant costs", "Relevant costs")))+
  geom_errorbar(data = se, aes(x = load, ymin = Distraction - ci, ymax = Distraction + ci), width = .05)+
  labs(x = "Load", y = "Difference from absent condition (ms)")+
  scale_x_discrete(labels = c("Low", "High")) +
  scale_fill_manual(values=bc_colors) +
  scale_color_manual(values = bc_colors) +
  scale_y_continuous(limits = limits, breaks = breaks) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_Publication(base_size = 15)+
  theme(strip.text = element_text(size = 15),
        legend.position  = "none")

ggsave(here::here("plots/figure10.png"), height = 7, width = 10, dpi = 1200)
