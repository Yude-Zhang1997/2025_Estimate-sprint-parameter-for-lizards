library(tidyverse)
library(ggpubr)
source("Stage 2. Run a sprint video.R")
files <- dir(path = ".", pattern = "DLC_colnamefixed.csv")
res <- vector("list", length = length(files))


for (i in 1:length(files)) {
  res[[i]] <- f_analyze_all(files[i], 
                            num_classic_sprint = 50, 
                            dist_nearzero = 0.025, 
                            plot = TRUE)
}



res



