# Author: Francisco Garre-Frutos

# Loading packages
if (!require(pacman)) {
  install.packages(pacman)
  load("pacman")
}

p_load(dplyr, afex, emmeans, ggplot2, tidyr, Rmisc)


# Helper functions ----

### Function to filter raw data: 
# This function is used to apply the following filters to the data: RT > 200, RT < 1200, 
# and only correct answers are used (ACC == 1) if the accuracy argument is set to TRUE (default). 
# In addition, it filters out participants who have less than a certain task accuracy with the 
# acc_min argument (default is 0.7).The type argument is used to filter the type of distraction in the task (by default "Irrelevant").

filter_data <- function(data, accuracy = T, Type = "Irrelevant", experiment = NULL, sd_acc = 2) {
  if (is.null(experiment)) stop("Please, set a value for 'Experiment'")
  library(dplyr)
  
  out <- data %>%
    filter(Phase %in% c("Relevant", "Irrelevant"), !ID %in% excluded[[experiment]],
           RT > 200, RT < 1200)

  if (experiment != "all") out <- out[out$Experiment == experiment,]
  if (Type == "Irrelevant" | Type == "Relevant") out <- out[out$Phase == Type,]
  if (accuracy) out <- out[which(out$ACC == 0),]
  
  return(out)
}


# Function to report:
report_trials <- function(d, experiment){
  d_f <- d[d$Phase %in% c("Relevant", "Irrelevant") &
             d$Experiment %in% experiment & 
             !d$ID %in% excluded[[experiment]],]
  per_corr <- 100 - (nrow(d_f[which(d_f$ACC == 0),])/nrow(d_f))*100
  per_RT <- 100 - (nrow(d_f[which(d_f$ACC == 0 & (d_f$RT > 200 & d_f$RT < 1200)),])/nrow(d_f[which(d_f$ACC == 0),]))*100

  print(paste0("Number of participants: ", length(unique(d_f$ID))))
  print(paste0("Incorrect responses: ", round(per_corr, 2), "%"))
  print(paste0("Too fast or too slow responses: ", round(per_RT, 2), "%"))
}


### Function to aggregate data by load, condition and subject:
# This function calculates the average DV per Subject, load and Condition. It is built on top of the 
# filter data function. If ACC is set to TRUE, the data will be filtered for accuracy analysis, 
# otherwise reaction times will be used. 

sum_data <- function(data, ACC, Type = "Irrelevant", experiment = NULL, sd_acc = 2, Manipulation = T) {
  library(dplyr)
  if(Type == "Irrelevant") {
    levels <- c("Peripheral", "Absent") 
  } else {
    if (experiment %in% c("exp3", "exp4")) levels <- c("Invalid", "Valid", "AbsentG")
    else levels <- c("Invalid", "Valid") 
  }
  if (Manipulation) {
    if (ACC) {
      out <- filter_data(data, !ACC, Type = Type, experiment = experiment, sd_acc = sd_acc) %>%
        group_by(ID, Manipulation, load, Condition) %>%
        dplyr::summarise(ACC = mean(ACC)) %>%
        ungroup()
      out$Condition <- factor(out$Condition, levels = levels)
      return(out)
    }
    out <- filter_data(data,Type = Type, sd_acc = sd_acc, experiment = experiment)%>%
      group_by(ID,Manipulation, load, Condition) %>%
      dplyr::summarise(RT = mean(RT)) %>%
      ungroup()
    out$Condition <- factor(out$Condition, levels = levels)
    return(out)
  }
  if (ACC) {
    out <- filter_data(data, !ACC, Type = Type, experiment = experiment, sd_acc = sd_acc) %>%
      group_by(ID, load, Condition) %>%
      dplyr::summarise(ACC = mean(ACC)) %>%
      ungroup()
    out$Condition <- factor(out$Condition, levels = levels)
    return(out)
  }
  out <- filter_data(data,Type = Type, sd_acc = sd_acc, experiment = experiment) %>%
    group_by(ID, load, Condition) %>%
    dplyr::summarise(RT = mean(RT)) %>%
    ungroup()
  #out$Condition <- factor(out$Condition, levels = levels)
  return(out)
}

### Function to perform ANOVAs and contrasts:
# The function aov_results() take a summarized data with the VD for each Subject, Condition and Load and performs
# a repeated measure ANOVA for the Condition x Load interaction. if contrasts is set to TRUE (default), the interaction is further 
# analyzed for the contrasts of theoretical interest: the effect of condition for each load level.
aov_results <- function(sum_dat, vd = "RT", contrasts = T, main = T, Manipulation = T, with = T) {
  emm_options(opt.digits = TRUE)  # revert to optimal digits
  if (Manipulation & with) aov_var <- c("Condition", "load", "Manipulation")
  else aov_var <- c("Condition", "load")
  list_res <- list()
  if (Manipulation & with) {
    list_res[["anova"]] <- aov_ez(data = sum_dat, id = "ID", dv = vd, within = aov_var, anova_table = list("es" = "pes"),
                                  include_aov = T)
  } else if (Manipulation) {
    list_res[["anova"]] <- aov_ez(data = sum_dat, id = "ID", dv = vd, within = aov_var, between = "Manipulation",
                                  anova_table = list("es" = "pes"), include_aov = T)
  } else {
    list_res[["anova"]] <- aov_ez(data = sum_dat, id = "ID", dv = vd, within = aov_var, anova_table = list("es" = "pes"),
                                  include_aov = T)
  }
  if (main) {
    list_res[["condition_main"]] <- emmeans(list_res[["anova"]],pairwise~Condition)
    list_res[["load_main"]] <- emmeans(list_res[["anova"]],pairwise~load)
    if (Manipulation) list_res[["Manipulation_main"]] <- emmeans(list_res[["anova"]],pairwise~Manipulation)
  }
  list_res[["marginal_means"]] <- emmeans(list_res[["anova"]], ~Condition|load)
  if (contrasts) {
    list_res[["contrasts"]] <- list_res[["marginal_means"]] %>% pairs()
    list_res[["contrasts_CI"]] <- list_res[["marginal_means"]] %>% pairs() %>% confint()
  }

  return(list_res)
}

### Helper function for plots: 
# The function get_effects() take a summarized data with the VD for each Subject, Condition and Load, and returns the difference of the vd
# for each Condition. The effects are computed separately for each Load condition.
get_effects <- function(sum_dat, vd = "RT") {
  out <- sum_dat %>% pivot_wider(names_from = c("Condition"),
                                 values_from = vd)
  return(out)
}

### Custom theme for plots
theme_Publication <- function(base_size=12, base_family="helvetica") {
  # Function adapted from: 
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            #legend.margin = margin(0, "cm"),
            #legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour=NA,fill=NA),
            strip.text = element_text(face="bold")
    ))
  
}

