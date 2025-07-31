library(tidyverse)
library(ggpubr)
source("D:/personal onedrive/OneDrive/1. Projects/2023-2025_Sprint locomotion of autotomy grass lizards/2025.05_ Quantify the lateral undulation/[Code] Autotomy and Control/2025.05_Stage 2-1. Run a sprint video.R")



#
f_estimate_all_video <-
  function(files, sprint_phase, group_grass) {
    if (!sprint_phase %in% c("acceleration", "maintenance")){
      message("sprint_phase should be acceleration or maintenance.")
      stop()
    }
    
    
    files <- dir(path = ".", pattern = "DLC_colnamefixed.csv")
    res <- vector("list", length = length(files))
    
    for (i in 1:length(files)) {
      res[[i]] <- f_analyze_all(
        files[i],
        DLC_lkd = 0.9,
        length_track = 90,
        num_classic_sprint = 60,
        threshold_edge = 1,
        dist_nearzero = 0.02,
        sprint_phase,
        plot = TRUE
      )
    }
    
    saveRDS(object = res, 
            file = paste0(sprint_phase,"_", group_grass, ".rds"))
    message(paste0("Save results as ", sprint_phase,"_", group_grass, ".rds"))
  }


# remember to change the name of rds
f_estimate_all_video(files, sprint_phase = "acceleration", group_grass = "Control_nG")
f_estimate_all_video(files, sprint_phase = "maintenance", group_grass = "Control_nG")
# res
