##### Stage 1-2. Find the classic sprint

#### 1. 找出所有的sprint段落 ####
f_find_sprint <- function(data, ID, velocity, num_classic_sprint = 50, plot = TRUE){
  # Find the starting ID of each sprint
  # 針對速度>0的影格，找出誰前1格速度== 0 且 後20格速度> 0
  find_sprint_start <- function(data, ID, velocity, num_classic_sprint = 50) {
    # ensure data is not empty and has required columns
    if(nrow(data) == 0 || !all(c(ID, velocity) %in% colnames(data))){
      stop("Input data is empty or does not contain specified columns.")
    }
    
    # initialize the vector to store
    ID_start <- integer() 
    
    # Loop the frame number in the velocity data
    for (i in seq_along(data[[ID]])) {
      # Check if V_i > 0
      if (data[[velocity]][i] > 0) {
        # Check if the index is within range
        # V_(i-1) == 0
        # V_(i+1):V_(i+50) > 0
        if (i > 1 && i < length(data[[ID]]) - num_classic_sprint &&
            data[[velocity]][i - 1] == 0 &&
            all(data[[velocity]][(i + 1):(i + num_classic_sprint)] > 0)) {
          ID_start <- c(ID_start, data[[ID]][i])
        }
      }
    }
    return(ID_start)
  }
  
  # Find the ending frame ID of each sprint
  # 針對速度>0的影格，找出誰前20格速度 > 0 且後1格速度 == 0
  find_sprint_end <- function(data, ID, velocity, num_classic_sprint = 50){
    # ensure data is not empty and has required columns
    if(nrow(data) == 0 || !all(c(ID, velocity) %in% colnames(data))){
      stop("Input data is empty or does not contain specified columns.")
    }
    
    # initialize an empty vector to store ending frames
    ID_end <- integer()
    
    # Loop through the frame number
    for (i in seq_along(data[[ID]])) {
      # Check if the velocity is more than 0
      if (data[[velocity]][i] > 0) {
        # Check if the index is within range
        # Check if the velocity from the previous 50 to 1 frames are more than 0
        # Check if the velocity of the next 1 frame is equal to 0
        if (i > num_classic_sprint && i < length(data[[ID]]) &&
            all(data[[velocity]][(i - num_classic_sprint):(i - 1)] > 0) &&
            data[[velocity]][i + 1] == 0) {
          ID_end <- c(ID_end, data[[ID]][i])
        }
      }
    }
    return(ID_end)
  }
  
  # 若nrow(velocity data) < 0，ID_sprint = NULL
  if (nrow(data) == 0){
    ID_sprint <- NULL
    print("velocity data is unestimated.")} else {
      ID_start <- find_sprint_start(data, ID, velocity, num_classic_sprint = 50)
      ID_end <- find_sprint_end(data, ID, velocity, num_classic_sprint = 50)
      
      # decide whether add the first or last id
      if (length(ID_start) == 0 & length(ID_end) == 0) {
        if (all(data[[velocity]] > 0)) {
          ID_sprint <- tibble(ID_start = data[[ID]][1], 
                              ID_end = data[[ID]][length(data[[ID]])])
        }
        else {
          ID_sprint <- NULL
        }
      } else if (length(ID_start) < length(ID_end)){
        ID_sprint <- tibble(ID_start = c(data[[ID]][1], ID_start),
                            ID_end = ID_end) %>% 
          mutate(Num_sprint = 1:nrow(.))
      } else if (length(ID_start) > length(ID_end)){
        ID_sprint <- tibble(ID_start = ID_start,
                            ID_end = c(ID_end, data[[ID]][length(data[[ID]])])) %>% 
          mutate(Num_sprint = 1:nrow(.))
      } else if (length(ID_start) == length(ID_end)) {
        if (ID_start[1] < ID_end[1]){
          ID_sprint <- tibble(ID_start = ID_start,
                              ID_end = ID_end) %>% 
            mutate(Num_sprint = 1:nrow(.))
        } else if (ID_start[1] > ID_end[1]){
          ID_sprint <- tibble(ID_start = c(data[[ID]][1], ID_start),
                              ID_end = c(ID_end, data[[ID]][length(data[[ID]])])) %>% 
            mutate(Num_sprint = 1:nrow(.))
        }
      }
    }
  return(ID_sprint)
  
  # plot to check
  if (plot == TRUE){
    if (!is.null(ID_sprint)){
      Plot_vt_each_sprint <-
        ggplot() +
        geom_rect(
          data = ID_sprint,
          aes(
            ymin = 0,
            ymax = max(d_vel_mod$vel_mod),
            xmin = ID_start,
            xmax = ID_end
          ),
          fill = "lightgray"
        ) +
        geom_text(
          data = ID_sprint,
          aes(
            x = ID_start,
            y = max(d_vel_mod$vel_mod) + 1,
            label = Num_sprint
          ),
          size = 12,
          size.unit = "pt"
        ) +
        geom_hline(
          aes(yintercept = 0),
          linewidth = 0.5,
          linetype = 2,
          color = "darkgray"
        ) +
        geom_line(
          data = d_vel_mod,
          mapping = aes(x = ID, y = vel_mod),
          linewidth = 0.75
        ) +
        geom_segment(
          data = d_vel_mod,
          mapping = aes(
            x = ID,
            y = vel_mod,
            xend = ID + dist_mod * cos(Angle_sprint / 180 * pi) * 50,
            yend = vel_mod + dist_mod * sin(Angle_sprint / 180 * pi) * 50,
            color = cut(dist_mod, breaks = seq(0, dist_mod %>% max, length.out = 7))
          ),
          arrow = arrow(
            type = "closed",
            ends = "last",
            length = unit(0.15, "cm")
          ),
          linetype = 2,
          linewidth = 0.5,
          show.legend = T
        ) +
        scale_y_continuous("Velocity (cm/s)") +
        scale_color_manual(
          values = c(
            '#00429d',
            '#5681b9',
            '#93c4d2',
            '#ffa59e',
            '#dd4c65',
            '#93003a'
          ) %>%
            rev,
          name = "sprint vector"
        ) +
        labs(title = "Vel-t (Head)") +
        theme_pubr(base_size = 12,
                   border = T,
                   margin = T)
      ggsave(
        vFN_07_plot_v_t_each_sprint.png,
        width = 16,
        height = 8,
        units = "cm"
      )
    }
  }
}

### 2. 去除有急轉彎的段落
## target: 找出3格以內角度差總和超過30度的轉彎起始frame

# Function to find the start of turns where sum angle change is greater than 30
f_find_turn <- function(ID_sprint, d_vel_mod) {
  # Initialize an empty vector to store turn start ID
  ID_turn_start <- c()
  
  # Loop through each sprint number
  for (i in ID_sprint$Num_sprint) {
    # Loop through each frame in the data
    for (j in d_vel_mod$ID) {
      # Check if the frame falls within the start and end range of the sprint
      if (j %in% ID_sprint$ID_start[i]:ID_sprint$ID_end[i]) {
        # Check if the frame is at least 2 frames away from the sprint end
        # Check if the sum of difference in angles over these 3 frames is greater than 30 degrees
        if (j < ID_sprint$ID_end[i] - 2 &&
            d_vel_mod %>% filter(ID %in% j:(j + 2)) %>%
            .$Angle_sprint %>% diff %>% sum %>% abs > 30) {
          # Store the frame index if the condition above is met
          ID_turn_start <- c(ID_turn_start, j)
        }
      }
    }
  }
  # return the tibble including turn start and turn end
  ID_turn <-
    tibble(ID_turn_start = ID_turn_start, 
           ID_turn_end = ID_turn_start + 2)
  return(ID_turn)
}

##### 08. Split sharp turns from sprints
## Function to remove the turns then return new start and end of each new sprint
f_remove_turn_from_sprint <- function(ID_sprint, ID_turn){
  # 排列出從最小到最大所有的frame
  x0 <- seq.int(min(ID_sprint %>% select(ID_start, ID_end), ID_turn),
                max(ID_sprint %>% select(ID_start, ID_end), ID_turn))
  
  # 保留在ID_sprint中的frame
  remianed <- 
    apply(ID_sprint %>% select(ID_start, ID_end) %>% as.matrix, 1, function(x){
      x0 %in% seq(x[1], x[2])
    }) |> 
    apply(1, any)
  x0.remained <- x0[remianed]
  
  # 去除ID_turn的frame
  if(nrow(ID_turn) == 0){
    cutted <- 
      apply(ID_sprint, 1, function(x){
        x0.remained %in% seq(x[1], x[2])
      }) |> 
      apply(1, any)
  } else {
    cutted <- 
      apply(ID_turn, 1, function(x){
        x0.remained %in% seq(x[1], x[2])
      }) |> 
      apply(1, any) |> 
      (function(x) !x)()
  }
  x0.cutted.remained <- x0.remained[cutted]
  
  # 找出所有連續數字的起始點與結束點
  index <- 
    c(1,
      which(diff(x0.cutted.remained) > 1),
      which(diff(x0.cutted.remained) > 1) + 1,
      length(x0.cutted.remained)) |>
    sort() |>
    matrix(ncol = 2, byrow = T, dimnames = list(NULL, c("f1", "f2")))
  x0.cutted.remained[index] |>
    matrix(ncol = 2, byrow = F, dimnames = list(NULL, c("f1", "f2")))
  ID_sprint_no_turn <- x0.cutted.remained[index] |>
    matrix(ncol = 2, byrow = F, dimnames = list(NULL, c("ID_start", "ID_end"))) %>% 
    as_tibble %>%
    mutate(Num_sprint = seq_along(ID_start))
  return(ID_sprint_no_turn)
}


##### 09. Choose long sprints
## Function to choose long sprints
# default frame number is 50
f_choose_long_sprint <- function(ID_sprint_no_turn, num_classic_sprint = 50) {
  # calculate sprint duration and filter for long sprints
  ID_sprint_long <- ID_sprint_no_turn %>%
    mutate(sprint_duration = ID_end - ID_start) %>%
    filter(sprint_duration >= num_classic_sprint)
  
  # Check if there are no long sprints
  if (nrow(ID_sprint_long) == 0) {
    message("There is no long sprint.")
    return(NULL)
  }
  return(ID_sprint_long)
}

##### 10. Select the sprint with the max speed
## Function to select the sprint with the max speed
# input: (tibble) with start ID and end ID of each sprint
# return: (tibble) only one row of the selected sprint, including its start ID and end ID
f_select_sprint_max_speed <- function(ID_sprint_long, d_vel_mod){
  # Calculate max speed for each sprint
  ID_max_speed <- ID_sprint_long %>%
    rowwise() %>%
    mutate(
      Max_speed = d_vel_mod %>%
        filter(ID >= ID_start & ID <= ID_end) %>%
        pull(vel_mod) %>%
        max(na.rm = TRUE) # Ensure NAs are ignored
    ) %>%
    ungroup()
  
    # Select the sprint with the highest max speed
    ID_sprint_select <- ID_max_speed %>%
      filter(Max_speed == max(Max_speed, na.rm = TRUE)) %>%
      slice(1) # To handle ties, take the first
  
  # Output the selected sprint
  return(ID_sprint_select)
}