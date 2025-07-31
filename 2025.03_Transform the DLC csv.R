##### Transform the DLC csv #####
library(tidyverse)

### set the column name
cN <-c("frame" ,expand.grid(
  c("x", "y", "likelihood"),
  c("Head", "Body1", "Body2", "Body3", "Tail", "LF", "RF", "LB", "RB",
    "edge_LT", "edge_RT", "edge_LB", "edge_RB")
) %>% 
  apply(1, function(x){
    paste0(x[2], "_", x[1])
  }))
print(cN)

### 
filenames <- dir(pattern = "\\.csv$", full.names = TRUE)

dir.create("transformed data", showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(filenames)) {
  if (str_detect(
    filenames[i],
    "_resnet50_autotomy in grassFeb6shuffle2_150000.csv"
  )) {
    filename_out <- filenames[i] %>%
      str_replace(
        "_resnet50_autotomy in grassFeb6shuffle2_150000.csv",
        ""
      ) %>%
      paste0("transformed data/", ., "_colnamefixed.csv")
    read_csv(filenames[i], , col_names = FALSE, skip = 3) %>%
      `colnames<-`(cN) %>%
      write_csv(filename_out)
  } else if (str_detect(
    filenames[i],
    "_resnet50_autotomy in grassFeb6shuffle2_150000.csv"
  )) {
    filename_out <- filenames[i] %>%
      str_replace(
        "_resnet50_autotomy in grassFeb6shuffle2_150000.csv",
        ""
      ) %>%
      paste0("transformed data/", ., "_colnamefixed.csv")
    read_csv(filenames[i], , col_names = FALSE, skip = 3) %>%
      `colnames<-`(cN) %>%
      write_csv(filename_out)
  }
}
