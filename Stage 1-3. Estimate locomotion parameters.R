##### Stage 1-3. Estimate locomotion parameters


## Function to find start ID and end ID each swing
# input: velocity data of the limb
# return: (tibble) with start ID and end ID of each swing
f_find_ID_swing <- function(d_vel_limb){
  ## Find the start ID
  # criteria: dang[i] < 0 & dang[i-1] >= 0 & dang[(i+1):(i+2)] < 0
  f_find_ID_swing_start <- function(d_vel_limb){
    ID_swing_start <- integer()
    for (i in seq_along(d_vel_limb[["ID"]])) {
      if(i > 1 && i < (length(d_vel_limb[["ID"]]) - 2)){
        if(d_vel_limb[["dang_mod"]][i] < 0 &&
           d_vel_limb[["dang_mod"]][i - 1] >= 0 &&
           all(d_vel_limb[["dang_mod"]][(i + 1):(i + 2)] < 0)){
          ID_swing_start <- c(ID_swing_start, d_vel_limb[["ID"]][i])
        }
      }
    }
    return(ID_swing_start)
  }
  ID_swing_start <- f_find_ID_swing_start(d_vel_limb = d_vel_limb)
  
  ## Find the end ID
  # criteria: dang[i] < 0 & dang[(i-2):(i-1)] < 0 & dang[i + 1] >= 0
  f_find_ID_swing_end <- function(d_vel_limb){
    ID_swing_end <- integer()
    for (i in seq_along(d_vel_limb[["ID"]])) {
      if(i > 2 && i < (length(d_vel_limb[["ID"]]) - 1)){
        if(d_vel_limb[["dang_mod"]][i] < 0 &&
           all(d_vel_limb[["dang_mod"]][(i - 2):(i - 1)] < 0) &&
           d_vel_limb[["dang_mod"]][i + 1] >= 0){
          ID_swing_end <- c(ID_swing_end, d_vel_limb[["ID"]][i])
        }
      }
    }
    return(ID_swing_end)
  }
  ID_swing_end <- f_find_ID_swing_end(d_vel_limb = d_vel_limb)
  
  ## if ID_swing_start[1] > ID_swing_end[1]，刪除ID_swing_end[1]
  if(ID_swing_start[1] > ID_swing_end[1]){
    message("remove the first ID_swing_end")
    ID_swing_end <- ID_swing_end[-1]
  }
  if(length(ID_swing_start) > length(ID_swing_end)){
    message("remove the last ID_swing_start")
    ID_swing <- 
      tibble(ID_swing_start = ID_swing_start[1:(length(ID_swing_start) - 1)],
             ID_swing_end = ID_swing_end)
  } else {
    ID_swing <- 
      tibble(ID_swing_start = ID_swing_start,
             ID_swing_end = ID_swing_end)}
  return(ID_swing)
}

## Function to estimate the step length of each swing
# input: (tibble) selected sprint data
# return: (vector) step length of each swing
f_estimate_step_length <- function(d_sprint_select, 
                                   ID_swing, 
                                   limb = "LF"){
  # Validate limb input
  if (!(limb %in% c("LF", "RF", "LB", "RB"))){
    stop("Invalid limb input. Choose from 'LF', 'RF', 'LB','RB'.")}
  
  # Initialize an empty vector for storing step lengths
  step_length <- numeric()
  
  # loop through each swing
  for (i in seq_along(ID_swing$ID_swing_start)) {
    # Get the coordinate of swing start
    d_sprint_swing_start <- 
      d_sprint_select %>% 
      filter(ID == ID_swing$ID_swing_start[i])
    xs <- d_sprint_swing_start[[paste0(limb,"_x_rm")]]
    ys <- d_sprint_swing_start[[paste0(limb, "_y_rm")]]
    
    # Get the coordinate of swing end
    d_sprint_swing_end <- 
      d_sprint_select %>%
      filter(ID == (ID_swing$ID_swing_end[i] + 1))
    xe <- d_sprint_swing_end[[paste0(limb, "_x_rm")]]
    ye <- d_sprint_swing_end[[paste0(limb, "_y_rm")]]
    dist <- sqrt((xe-xs)^2 + (ye-ys)^2)
    step_length <- c(step_length, dist)
  }
  return(step_length)
}

## Function to estimate the limb angle of each swing
# input: (tibble) selected sprint data
# return: (vector) limb angle of each swing
f_estimate_limb_angle <- function(d_sprint_select, ID_swing, limb = "LF"){
  # Validate limb input
  if (!(limb %in% c("LF", "RF", "LB", "RB"))){
    stop("Invalid limb input. Choose from 'LF', 'RF', 'LB','RB'.")}
    
  # initialize an empty vector for storing angles
  limb_angle <- numeric()
  
  # loop through each swing
  for (i in seq_along(ID_swing$ID_swing_start)) {
    d_sprint_swing_start <-
      d_sprint_select %>% 
      filter(ID == ID_swing$ID_swing_start[i])
    ang_start <- d_sprint_swing_start[[paste0("Angle_", limb)]]
    d_sprint_swing_end <- 
      d_sprint_select %>%
      filter(ID == ID_swing$ID_swing_end[i])
    ang_end <- d_sprint_swing_end[[paste0("Angle_", limb)]]
    limb_angle_i <- ang_end - ang_start
    limb_angle <- c(limb_angle, limb_angle_i)
  }
  return(limb_angle)
}

##### 13. Estimate spine angle
## Function to estimate spine angle of each move
# input: (tibble) selected sprint data
# return: (vector) spine angle of each move
# position = c("right", "left") <- choose the body side
f_estimate_spine_angle <- function(d_sprint_select,
                                   position = "right") {
  # Validate position input
  if (!(position %in% c("right", "left"))){
    stop("Invalid position input. Choose from 'right' and 'left'.")}
  
  
  Angle_spine <- integer()
  # 身體往右為正
  for (i in seq_along(d_sprint_select$ID)) {
    if (position == "right") {
      # 身體往右 <- 前後皆小於自己
      if (i > 2 &&
          i < (length(d_sprint_select$ID) - 2) &&
          all(d_sprint_select[["Angle_Spine"]][(i -
                                                2):(i - 1)] < d_sprint_select[["Angle_Spine"]][i]) &&
          all(d_sprint_select[["Angle_Spine"]][(i +
                                                1):(i + 2)] < d_sprint_select[["Angle_Spine"]][i])) {
        Angle_spine <- c(Angle_spine, d_sprint_select[["Angle_Spine"]][i])
      }
    } else if (position == "left") {
      # 身體往左 <-前後皆大於自己
      if (i > 2 &&
          i < (length(d_sprint_select$ID) - 2) &&
          all(d_sprint_select[["Angle_Spine"]][(i - 2):(i - 1)] > d_sprint_select[["Angle_Spine"]][i]) &&
          all(d_sprint_select[["Angle_Spine"]][(i + 1):(i + 2)] > d_sprint_select[["Angle_Spine"]][i])) {
        Angle_spine <- c(Angle_spine, (d_sprint_select[["Angle_Spine"]][i]))
      }
    }
  }
  return(Angle_spine)
}

##### 14. Estimate tail angle
## Function to estimate tail angle

f_estimate_tail_angle <- function(d_sprint_select, 
                                  position = "right"){
  # Validate position input
  if (!(position %in% c("right", "left"))){
    stop("Invalid position input. Choose from 'right' and 'left'.")}
  
  Angle_tail <- integer()
  # 身體右側為正
  for (i in seq_along(d_sprint_select[["ID"]])) {
    # 身體右側 <- 前後兩格小於自己
    if (position == "right") {
      if (i > 2 &&
          i < length(d_sprint_select[["ID"]]) - 2 &&
          all(d_sprint_select[["Angle_Tail"]][(i-2):(i-1)] < d_sprint_select[["Angle_Tail"]][i]) &&
          all(d_sprint_select[["Angle_Tail"]][(i+1):(i+2)] < d_sprint_select[["Angle_Tail"]][i])) {
        Angle_tail <- c(Angle_tail, d_sprint_select[["Angle_Tail"]][i])
      }
    } else if (position == "left") {
      # 身體左側 <- 前後兩格大於自己
      if (i > 2 &&
          i < length(d_sprint_select$ID) - 2 &&
          all(d_sprint_select[["Angle_Tail"]][(i-2):(i-1)] > d_sprint_select[["Angle_Tail"]][i]) &&
          all(d_sprint_select[["Angle_Tail"]][(i+1):(i+2)] > d_sprint_select[["Angle_Tail"]][i])) {
        Angle_tail <- c(Angle_tail, d_sprint_select[["Angle_Tail"]][i])
      }
    }
  }
  return(Angle_tail)
}
