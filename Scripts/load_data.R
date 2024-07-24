if (!require(pacman)) {
  install.packages(pacman)
  load("pacman")
}

p_load(readxl, openxlsx, here, dplyr, stringr)

setwd(here::here())

source(here::here("scripts/functions.R")) # Load relevant functions to the workspace

# Formatting raw data for analysis
if (!file.exists(("Output/combined_exps.xlsx"))) {
  # Experiment 1 ----
  
  # If formatted data does not exists, this code reads the raw data and formatted it.
  # If formatted data exists, it is loaded in the environment
  dlist <- list()
  if (!file.exists("Output/data1.xlsx")) {
    dlist[["exp1"]] <- read_excel(here::here("Input/raw_T1.xlsx"))%>%
      select(ExperimentName, Subject, Condition, flanker.ACC,
             flanker.RT, load, Trial, `Procedure[Trial]`, Trial, Sex, Age, matches("^p\\d+$"), CaptureL, CaptureR) %>%
      dplyr::rename("Experiment" = "ExperimentName",
                    "ID" = "Subject",
                    "ACC" =  "flanker.ACC",
                    "RT" = "flanker.RT",
                    "Phase" = "Procedure[Trial]")%>% 
      mutate(Phase = case_when(
        grepl("^(HighB|LowB)", .$Phase)~"Irrelevant",
        grepl("^(HighR|LowR)", .$Phase)~"Relevant",
        T~"Practice"
      ),
      Experiment = "exp1",
      Manipulation = "No") %>%
      mutate(TargetPos = case_when(
        p5 %in% c("X", "N") | p6 %in% c("X", "N") | p1 %in% c("X", "N") ~ "Up",
        p4 %in% c("X", "N") | p2 %in% c("X", "N") | p3 %in% c("X", "N") ~ "Down",
      ),
      DistPos = case_when(
        CaptureL != "blank.bmp" ~ "Up", 
        CaptureR != "blank.bmp" ~ "Down", 
        CaptureL == "blank.bmp" & CaptureL == "blank.bmp"~"Absent"
      ),
      Distance = case_when(
        DistPos == "Absent"~"Absent",
        TargetPos == DistPos ~ "Near",
        TargetPos != DistPos ~"Far"
        )
      )
    
    write.xlsx(dlist[["exp1"]], file = here::here("Output/data1.xlsx"))
  } else {
    dlist[["exp1"]] <- read_excel("Output/data2.xlsx")[, -1]
  }
  
  dlist[["exp1"]]$load <- factor(dlist[["exp1"]]$load, levels = c("high", "low"))
  
  # Experiment 2 ----
  if (!file.exists("Output/data2.xlsx")) {
    dlist[["exp2"]] <- read_excel(here::here("Input/raw_T2.xlsx"))%>%
      select(ExperimentName, Subject, Condition, flanker.ACC,
             flanker.RT, load, `Procedure[SubTrial]`, SubTrial, TimeExpo,
             `Running[Block]`, Texto, Item, RatingEVEA, RatingSTAI, Sex, Age, matches("^p\\d+$"), CaptureL, CaptureR) %>%
      dplyr::rename("Experiment" = "ExperimentName",
                    "ID" = "Subject",
                    "ACC" =  "flanker.ACC",
                    "RT" = "flanker.RT",
                    "Trial" = "SubTrial",
                    "Phase" = "Procedure[SubTrial]",
                    "Manipulation" = "TimeExpo")%>% 
      mutate(Phase = case_when(
        grepl("^(HighB|LowB)", .$Phase)~"Irrelevant",
        grepl("^(HighR|LowR)", .$Phase)~"Relevant",
        `Running[Block]` == "antesSTAIe" |`Running[Block]` == "despuesSTAIe"~"STAIe",
        `Running[Block]` == "antesEVEALIST" | `Running[Block]` == "despuesEVEALIST"~"EVEA",
        T~"Practice"
      ),
      Manipulation = case_when(
        Manipulation == 100 ~ "100",
        Manipulation == 200 ~ "200",
        T~"No"
      ),
      Item = ifelse(is.na(Item), "", Item),
      Texto = ifelse(is.na(Texto), "", Texto),
      Item = paste0(Item, Texto),
      Rating = ifelse(is.na(RatingSTAI), 0, RatingSTAI) +
        ifelse(is.na(RatingEVEA), 0, RatingEVEA),
      Experiment = "exp2") %>%
      select(-c(Texto, `Running[Block]`, RatingSTAI, RatingEVEA)) %>%
      mutate(TargetPos = case_when(
        p5 %in% c("X", "N") | p6 %in% c("X", "N") | p1 %in% c("X", "N") ~ "Up",
        p4 %in% c("X", "N") | p2 %in% c("X", "N") | p3 %in% c("X", "N") ~ "Down",
      ),
      DistPos = case_when(
        CaptureL != "blank.bmp" ~ "Up", 
        CaptureR != "blank.bmp" ~ "Down", 
        CaptureL == "blank.bmp" & CaptureL == "blank.bmp"~"Absent"
      ),
      Distance = case_when(
        DistPos == "Absent"~"Absent",
        TargetPos == DistPos ~ "Near",
        TargetPos != DistPos ~"Far"
        )
      )
    
    write.xlsx(dlist[["exp2"]], file = here::here("Output/data2.xlsx"))
  } else {
    dlist[["exp2"]] <- read_excel("Output/data2.xlsx")[, -1]
  }
  
  dlist[["exp2"]]$load <- factor(dlist[["exp2"]]$load, levels = c("high", "low"))
  
  
  # Experiment 3 ----
  if (!file.exists("Output/data3.xlsx")) {
    dlist[["exp3"]] <- read_excel(here::here("Input/raw_T3.xlsx"))%>%
      select(ExperimentName, Subject, Condition, flanker.ACC,
             flanker.RT, `load[SubTrial]`, Trial, `Procedure[Trial]`, Trial, Order,
             `Running[Block]`, Texto, Item, RatingEVEA, RatingSTAI, Sex, Age, matches("^p\\d+$"), CaptureL, CaptureR) %>%
      dplyr::rename("Experiment" = "ExperimentName",
                    "ID" = "Subject",
                    "ACC" =  "flanker.ACC",
                    "RT" = "flanker.RT",
                    "Phase" = "Procedure[Trial]",
                    "load" = "load[SubTrial]",
                    "Manipulation" = "Order")%>% 
      mutate(
        Manipulation = case_when(
          Phase == "MixedL" | Phase == "MixedG"~"Mixed",
          grepl("^(HighR|LowR|HighB|LowB)", .$Phase)~"Blocked",
          T~"No"
        ),
        Phase = case_when(
          grepl("^(HighB|LowB|MixedL)", .$Phase)~"Irrelevant",
          grepl("^(HighR|LowR|MixedG)", .$Phase)~"Relevant",
          `Running[Block]` == "antesSTAIe" |`Running[Block]` == "despuesSTAIe"~"STAIe",
          `Running[Block]` == "antesEVEALIST" | `Running[Block]` == "despuesEVEALIST"~"EVEA",
          T~"Practice"
        ),
        Item = ifelse(is.na(Item), "", Item),
        Texto = ifelse(is.na(Texto), "", Texto),
        Item = paste0(Item, Texto),
        Rating = ifelse(is.na(RatingSTAI), 0, RatingSTAI) +
          ifelse(is.na(RatingEVEA), 0, RatingEVEA),
        Experiment = "exp3") %>%
      select(-c(Texto, `Running[Block]`, RatingSTAI, RatingEVEA)) %>%
      mutate(TargetPos = case_when(
        p5 %in% c("X", "N") | p6 %in% c("X", "N") | p1 %in% c("X", "N") ~ "Up",
        p4 %in% c("X", "N") | p2 %in% c("X", "N") | p3 %in% c("X", "N") ~ "Down",
      ),
      DistPos = case_when(
        CaptureL != "blank.bmp" ~ "Up", 
        CaptureR != "blank.bmp" ~ "Down", 
        CaptureL == "blank.bmp" & CaptureL == "blank.bmp"~"Absent"
      ),
      Distance = case_when(
        DistPos == "Absent"~"Absent",
        TargetPos == DistPos ~ "Near",
        TargetPos != DistPos ~"Far"
        )
      )
    
    write.xlsx(dlist[["exp3"]], file = here::here("Output/data3.xlsx"))
  } else {
    dlist[["exp3"]] <- read_excel("Output/data3.xlsx")[, -1]
  }
  
  
  dlist[["exp3"]]$load <- factor(dlist[["exp3"]]$load, levels = c("high", "low"))
  
  # Experiment 4 ----
  
  if (!file.exists("Output/data4.xlsx")) {
    dlist[["exp4"]] <- read.csv(here::here("Input/raw_T4.csv")) %>%
      select(ExperimentName, Subject, Running.Block., Condition, flanker.ACC, flanker.RT,
             flanker1.RT, flanker1.ACC, flanker2.RT, flanker2.ACC,  SubTrial, Procedure.Trial., load, Condition, 
             Item, Texto, RatingSTAIE, RatingSTAIR, RatingEVEA, Sex, Age, matches("^p\\d+$"), CaptureL, CaptureR) %>% 
      mutate(
        RT = ifelse(is.na(flanker.RT), 0, flanker.RT) +
          ifelse(is.na(flanker2.RT), 0, flanker2.RT),
        ACC = ifelse(is.na(flanker.ACC), 0, flanker.ACC) +
          ifelse(is.na(flanker2.ACC), 0, flanker2.ACC) 
      ) %>% 
      dplyr::rename("Experiment" = "ExperimentName",
                    "ID" = "Subject",
                    "Phase" = "Procedure.Trial.",
                    "Trial" = "SubTrial",
      ) %>% 
      #select(-c(flanker.RT, flanker.ACC, flanker1.RT, flanker1.ACC)) %>%
      mutate(Manipulation = case_when(
        Phase == "MixedLsame"~"Same",
        Phase == "MixedLdiff"~"Diff",
        T~"No"
      ),
      Phase = case_when(
        Phase == "MixedLsame" | Phase == "MixedLdiff"~"Irrelevant",
        Phase == "MixedG"~"Relevant",
        Running.Block. == "antesSTAIe" |Running.Block. == "despuesSTAIe"~"STAIe",
        Running.Block. == "antesSTAIr" | Running.Block. == "despuesSTAIr"~"STAIr",
        Running.Block. == "antesEVEALIST" | Running.Block. == "despuesEVEALIST"~"EVEA",
        T~"Practice"
      ),
      Item = ifelse(is.na(Item), "", Item),
      Texto = ifelse(is.na(Texto), "", Texto),
      Item = paste0(Item, Texto),
      Rating = ifelse(is.na(RatingSTAIE), 0, RatingSTAIE) + ifelse(is.na(RatingSTAIR), 0, RatingSTAIR)+
        ifelse(is.na(RatingEVEA), 0, RatingEVEA),
      Experiment = "exp4") %>%
      select(-c(Texto, Running.Block., RatingSTAIE, RatingSTAIR, RatingEVEA))%>%
      mutate(TargetPos = case_when(
        p3 %in% c("X", "N") | p6 %in% c("X", "N") ~ "Center", # Center
        p1 %in% c("X", "N") | p2 %in% c("X", "N") ~ "Right", # Left
        p4 %in% c("X", "N") | p5 %in% c("X", "N") ~ "Left" # Right
      ),
      DistPos = case_when(
        CaptureL != "blank.bmp" ~ "Left", # Left
        CaptureR != "blank.bmp" ~ "Right", # Right
        CaptureL == "blank.bmp" & CaptureL == "blank.bmp"~"Absent"
      ),
      TargetDist_distance = case_when(
        DistPos == "Absent"~"Absent",
        TargetPos == DistPos ~ "Near",
        TargetPos == "Center" ~ "Middle",
        TargetPos != "Center" & TargetPos != DistPos ~"Far"
        )
      )
    
    write.xlsx(dlist[["exp4"]], file = here::here("Output/data4.xlsx"))
  } else {
    dlist[["exp4"]] <- read_excel("Output/data4.xlsx")[, -1]
  }
  
  dlist[["exp4"]]$load <- factor(dlist[["exp4"]]$load, levels = c("high", "low"))
  
  # Combine 4 experiments ----
  
  raw_all <- do.call(bind_rows, dlist)
  raw_all$ACC <- ifelse(raw_all$ACC == 1, 0, 1) # Changing accuracy for errors
  raw_all$ACC <- raw_all$ACC*100 # Chaining proportion to percentage
  write.xlsx(raw_all, file = here::here("Output/combined_exps.xlsx"))
} else {
  raw_all <- read_excel("Output/combined_exps.xlsx")
}

# Check data ----
# Excluded participants by Experiment
excluded <- list()
for (exp in unique(raw_all$Experiment)) {
  excluded[[exp]] <- raw_all[raw_all$Experiment == exp,] %>%
    filter(Phase %in% c("Relevant", "Irrelevant"))%>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(mean_Err = mean(ACC)) %>%
    ungroup() %>%
    mutate(cut_off = mean(mean_Err)+sd(mean_Err)*2) %>%
    filter(mean_Err > cut_off) %>%
    pull(ID)
}

# Participant with missing observations in one condition
excluded[["exp2"]] <- c(excluded[["exp2"]], 36)

# Number of participants excluded by experiments
lapply(excluded, \(x) length(x)) 

# Object used to filter excluded participants when filtering data:
to_exclude <- unlist(excluded)

