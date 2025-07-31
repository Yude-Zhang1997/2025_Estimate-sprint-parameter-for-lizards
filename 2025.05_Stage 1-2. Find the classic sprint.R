##### Stage 1-2. Find the classic sprint

#### 1. 找出所有的sprint段落 ####
f_find_sprint <- function(data, ID, velocity, num_classic_sprint) {
  # Find the starting ID of each sprint
  # 針對速度=0的影格，找出誰後num_classic_sprint格速度> 0
  find_sprint_start <- function(data, ID, velocity, num_classic_sprint) {
    # ensure data is not empty and has required columns
    if (nrow(data) == 0 ||
        !all(c(ID, velocity) %in% colnames(data))) {
      stop("Input data is empty or does not contain specified columns.")
    }
    
    # initialize the vector to store
    ID_start <- integer()
    
    # Loop the frame number in the velocity data
    for (i in seq_along(data[[ID]])) {
      # Check if V_i = 0
      if (data[[velocity]][i] == 0) {
        # Check if the index is within range
        # V_(i+1) > 0
        if (i < (length(data[[ID]]) - num_classic_sprint) &&
            all((data[[velocity]][(i + 1):(i + num_classic_sprint)]) > 0)) {
          ID_start <- c(ID_start, data[[ID]][i])
        }
      }
    }
    return(ID_start)
  }
  
  # Find the ending frame ID of each sprint
  # 針對速度 = 0的影格，找出誰前1格速度 > 0
  find_sprint_end <- function(data, ID, velocity, num_classic_sprint) {
    # ensure data is not empty and has required columns
    if (nrow(data) == 0 ||
        !all(c(ID, velocity) %in% colnames(data))) {
      stop("Input data is empty or does not contain specified columns.")
    }
    
    # initialize an empty vector to store ending frames
    ID_end <- integer()
    
    # Loop through the frame number
    for (i in seq_along(data[[ID]])) {
      # Check if the velocity is more than 0
      if (data[[velocity]][i] == 0) {
        # Check if the index is within range
        # Check if the velocity from the previous 50 to 1 frames are more than 0
        # Check if the velocity of the next 1 frame is equal to 0
        if (i > num_classic_sprint &&
            all((data[[velocity]][(i - num_classic_sprint):(i - 1)]) > 0)) {
          ID_end <- c(ID_end, data[[ID]][i])
        }
      }
    }
    return(ID_end)
  }
  
  # 若nrow(velocity data) < 0，ID_sprint = NULL
  if (nrow(data) == 0) {
    ID_sprint <- NULL
    print("velocity data is unestimated.")
  } else {
    ID_start <- find_sprint_start(data, ID, velocity, num_classic_sprint)
    ID_end <- find_sprint_end(data, ID, velocity, num_classic_sprint)
    
    # decide whether add the first or last id
    if (length(ID_start) == 0 |
        length(ID_end) == 0) {
      ID_sprint <- NULL
      message("There is no available sprint in the video.")
    } else if (length(ID_start) < length(ID_end)) {
      ID_sprint <-
        tibble(ID_start = ID_start, ID_end = ID_end[-1]) %>%
        mutate(Num_sprint = 1:nrow(.))
    } else if (length(ID_start) > length(ID_end)) {
      ID_sprint <-
        tibble(ID_start = ID_start[-length(ID_start)], ID_end = ID_end) %>%
        mutate(Num_sprint = 1:nrow(.))
    } else if (length(ID_start) == length(ID_end)) {
      if (ID_start[1] < ID_end[1]) {
        ID_sprint <-
          tibble(ID_start = ID_start, ID_end = ID_end) %>%
          mutate(Num_sprint = 1:nrow(.))
      } else if (ID_start[1] > ID_end[1]) {
        ID_sprint <-
          tibble(ID_start = ID_start[-length(ID_start)], ID_end = ID_end[-1])
        if (nrow(ID_sprint) == 0) {
          ID_sprint <- NULL
        } else {
          ID_sprint <-
            ID_sprint %>%
            mutate(Num_sprint = 1:nrow(.))
        }
      } else if (ID_start[1] == ID_end[1]){
        if(length(ID_end[- 1]) == 0){
          ID_sprint <- NULL
        } else {
          ID_sprint <-
            tibble(ID_start = ID_start[1:length(ID_end[-1])], 
                   ID_end = ID_end[-1]) %>%
            mutate(Num_sprint = 1:nrow(.))
        }
      }
    }
  }
  return(ID_sprint)
}

#### 2.2 plot all sprints in the video
f_plot_all_sprints_in_the_video <-
  function(ID_sprint,
           d_vel_mod, 
           title = ""){
    plot_vt_each_sprint <-
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
        color = "gray"
      ) +
      geom_line(
        data = d_vel_mod,
        mapping = aes(x = ID, y = vel_mod),
        linewidth = 0.75
      ) +
      geom_segment(
        data = d_vel_mod,
        aes(
          x = ID,
          y = vel,
          xend = ID + dist * cos(Angle_dxdy  / 180 * pi) * 50,
          yend = vel + dist * sin(Angle_dxdy  / 180 * pi) * 50,
          color = cut(dist, breaks = seq(0, dist %>% max, length.out = 7))
        ),
        arrow = arrow(
          type = "closed",
          ends = "last",
          length = unit(0.15, "cm")
        ),
        linetype = 2,
        linewidth = 0.5,
        # color = "darkblue",
        show.legend = T
      ) +
      scale_x_continuous(name = "ID") +
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
      labs(title = title) +
      theme_pubr(base_size = 12,
                 border = T,
                 margin = T)
    return(plot_vt_each_sprint)
  }




##### 2.2 Choose long sprints
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

##### 2.3 remove sprints including blank rows
f_remove_sprints_including_blank_rows <-
  function(ID_sprint_long, d_vel_mod) {
    # find sprints_including_blank_rows
    f_find_sprint_including_blank_rows <-
      function(ID_sprint_long, d_vel_mod) {
        i_sprint_including_blank_rows <- integer()
        
        for (i in seq_along(ID_sprint_long$ID_start)) {
          d <- d_vel_mod %>%
            filter(ID %in% ID_sprint_long$ID_start[i]:ID_sprint_long$ID_end[i])
          ID_diff <- diff(d$ID)
          if (any(ID_diff > 1)) {
            i_sprint_including_blank_rows <-
              c(i_sprint_including_blank_rows, i)
          }
        }
        return(i_sprint_including_blank_rows)
      }
    
    i_sprint_including_blank_rows <-
      f_find_sprint_including_blank_rows(ID_sprint_long, d_vel_mod)
    
    if(length(i_sprint_including_blank_rows) == 0){
      ID_without_blank_rows <- ID_sprint_long
    } else {
      ID_without_blank_rows <- 
      ID_sprint_long[- i_sprint_including_blank_rows, ]
    }
    
    return(ID_without_blank_rows)
  }

##### 2.4 remove sprints including long interpolations
f_remove_sprints_including_long_interpolations <-
  function(ID_without_blank_rows, d_vel_mod) {
    
    # find sprints_including_long_interpolations
    f_find_sprint_including_long_interpolations <-
      function(ID_without_blank_rows, d_vel_mod) {
        i_sprint_including_long_interpolations <- integer()
        
        for (i in seq_along(ID_without_blank_rows$ID_start)) {
          d <- d_vel_mod %>%
            filter(ID %in%
                     ID_without_blank_rows$ID_start[i]:ID_without_blank_rows$ID_end[i])
          vel_mod_diff <- diff(d$vel_mod)
          
          
          is_zero_vel_mod_diff <- as.integer(vel_mod_diff == 0)
          
          # 使用滑動窗口檢查每一段長度為 5 的區間總和是否為 5
          f_check_is_zero_vel_mod_diff <-
            function(is_zero_vel_mod_diff) {
              for (i in seq_along(is_zero_vel_mod_diff)) {
                if ((i < (length(is_zero_vel_mod_diff) - 4) &&
                     sum(is_zero_vel_mod_diff[i:(i + 4)]) == 5) |
                    (i > 4 &&
                     sum(is_zero_vel_mod_diff[(i - 4):i]) == 5)) {
                  return(TRUE)  # 找到一段連續五個 0
                }
              }
              return(FALSE)
            }
          
          if (f_check_is_zero_vel_mod_diff(is_zero_vel_mod_diff) == TRUE) {
            i_sprint_including_long_interpolations <-
              c(i_sprint_including_long_interpolations, i)
          }
        }
        return(i_sprint_including_long_interpolations)
      }
    
    
    i_sprint_including_long_interpolations <- f_find_sprint_including_long_interpolations(ID_without_blank_rows, d_vel_mod)
    
    if(length(i_sprint_including_long_interpolations) == 0) {
      ID_without_long_interpolations <- ID_without_blank_rows
    } else {
      ID_without_long_interpolations <-
        ID_without_blank_rows[-i_sprint_including_long_interpolations, ]
    }
    
    return(ID_without_long_interpolations)
  }

##### 2.5 remove sprints direction is skew
#### criteria: the angle between the vector start to end and the vector should be less than 60
f_remove_skew_sprints <-
  function(ID_without_long_interpolations, d_sprint_t, d_vel_mod, angle_skew_sprint = 60){
    d <-
      d_vel_mod %>%
      mutate(mean_body_x = (Head_x_rm + Body1_x_rm + Body2_x_rm + Body3_x_rm + Tail_x_rm) / 5) %>%
      mutate(mean_body_y = (Head_y_rm + Body1_y_rm + Body2_y_rm + Body3_y_rm + Tail_y_rm) / 5) %>% 
      select(c(ID,mean_body_x, mean_body_y))
    
    f_find_i_skew_sprint <-
      function(ID_without_long_interpolations,
               d,
               d_sprint_t,
               angle_skew_sprint) {
        i_skew_sprint <- integer()
        
        # compute the track vector
        track_x <- (
          median(d_sprint_t$edge_RT_x) - median(d_sprint_t$edge_LT_x) +
            median(d_sprint_t$edge_RB_x) - median(d_sprint_t$edge_LB_x)
        ) / 2
        track_y <- (
          median(d_sprint_t$edge_RT_y) - median(d_sprint_t$edge_LT_y) +
            median(d_sprint_t$edge_RB_y) - median(d_sprint_t$edge_LB_y)
        ) / 2
        V2 <- c(track_x, track_y)
        
        for (i in seq_along(ID_without_long_interpolations$Num_sprint)) {
          ID_start <- 
            ID_without_long_interpolations$ID_start[i]
          ID_end <- 
            ID_without_long_interpolations$ID_end[i]
          
          point_start <-
            d %>%
            filter(ID == ID_start) %>%
            select(mean_body_x, mean_body_y)
          point_end <-
            d %>%
            filter(ID == ID_end) %>%
            select(mean_body_x, mean_body_y)
          
          
          
          V1 <- c((point_end$mean_body_x - point_start$mean_body_x), 
                  (point_end$mean_body_y - point_start$mean_body_y))
          
          # check for degenerate zero-length vectors
          if (sqrt(sum(V1^2)) == 0 || sqrt(sum(V2^2)) == 0) next
          
          
          # Compute angle in degrees
          angle <- acos(sum(V1 * V2) / (sqrt(sum(V1^2)) * sqrt(sum(V2^2)))) * 180 / pi
          
          if (!is.na(angle) && angle > angle_skew_sprint) {
            i_skew_sprint <- c(i_skew_sprint, i)
          }
        }
        return(i_skew_sprint)
      }
    
    i_skew_sprint <-
      f_find_i_skew_sprint(ID_without_long_interpolations,
                           d,
                           d_sprint_t,
                           angle_skew_sprint)
    
    if (length(i_skew_sprint) == 0) {
      ID_remove_skew_sprint <-
        ID_without_long_interpolations
    } else {
      ID_remove_skew_sprint <-
        ID_without_long_interpolations[-i_skew_sprint, ]
    }
    
    return(ID_remove_skew_sprint)
  }

##### 2.6 remove sprints including too large acceleration 
### criteria: difference between two sequential velocity > 20
f_remove_sprints_including_too_large_acceleration <-
  function(ID_remove_skew_sprint, d_vel_mod, acc_too_large = 20){
    d <-
      tibble(
        ID = d_vel_mod$ID[-length(d_vel_mod$ID)],
        acc = diff(d_vel_mod$vel_mod)
      )
    
    # the sprint including any acc larger than acc_too_large
    i_acc_too_large <- integer()
    for (i in seq_along(ID_remove_skew_sprint$Num_sprint)) {
      d_i <- 
        d %>%
        filter(ID %in% ID_remove_skew_sprint$ID_start[i]:ID_remove_skew_sprint$ID_end[i])
      if(any(d_i$acc > acc_too_large)){
        i_acc_too_large <- c(i_acc_too_large, i)
      }
    }
    
    #
    if (length(i_acc_too_large) == 0) {
      ID_remove_sprint_acc_too_large <-
        ID_remove_skew_sprint
    } else {
      ID_remove_sprint_acc_too_large <-
        ID_remove_skew_sprint[-i_acc_too_large, ]
    }
    
    return(ID_remove_sprint_acc_too_large)
  }

##### 2.7 Select the sprint with the max speed
## Function to select the sprint with the max speed
# input: (tibble) with start ID and end ID of each sprint
# return: (tibble) only one row of the selected sprint, including its start ID and end ID
f_select_sprint_max_speed <- function(ID_without_long_interpolations, d_vel_mod){
  # Calculate max speed for each sprint
  ID_max_speed <- ID_without_long_interpolations %>%
    rowwise() %>%
    mutate(
      Max_speed = d_vel_mod %>%
        filter(ID %in% ID_start:ID_end) %>%
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

# ##### Separte the starting phase, running phase, and ending phase #####
# ##
# f_find_ID_end_of_starting_phase <-
#   function(d_acc_select){
#     ID_match <- integer()
#     
#     for (i in seq_along(d_acc_select$ID)) {
#       if(all(d_acc_select$acceleration[1:i] > 0)){
#         ID_match <- c(d_acc_select$ID[i] ,ID_match)
#       }
#     }
#     ID_end_of_starting_phase <- max(ID_match)
#     return(ID_end_of_starting_phase)
#   }
# 
# ## 
# f_find_ID_start_of_ending_phase <-
#   function(d_acc_select){
#     ID_match <- integer()
#     
#     for (i in seq_along(d_acc_select$ID)) {
#       if(all(d_acc_select$acceleration[i:length(d_acc_select$ID)] < 0)){
#         ID_match <- c(d_acc_select$ID[i] ,ID_match)
#       }
#     }
#     ID_start_of_ending_phase <- min(ID_match)
#     return(ID_start_of_ending_phase)
#   }

#### function to plot the trajectory of the selected sprint ####
f_plot_trajectory_select <-
  function(d_sprint_select, ID_sprint_select, vFN) {
    plot_trajectory_select <-
      ggplot(data = d_sprint_select %>%
               filter(
                 ID %in% seq(d_sprint_select$ID %>% first, d_sprint_select$ID %>% last, by = 5)
               )) +
      geom_line(
        data = d_sprint_select,
        mapping = aes(x = Mean_x_all_body_parts, y = sprint_group_sprint_y_predict, group = sprint_group_id),
        linetype = 2,
        linewidth = 0.75
      ) +
      geom_segment(
        aes(
          x = Body1_x_rm,
          y = Body1_y_rm,
          xend = Head_x_rm,
          yend = Head_y_rm
        ),
        color = '#93003a',
        linewidth = 0.5
        # arrow = arrow(
        #   type = "closed",
        #   ends = "last",
        #   length = unit(0.15, "cm")
        # )
      ) +
      geom_segment(
        aes(
          x = Body2_x_rm,
          y = Body2_y_rm,
          xend = Body1_x_rm,
          yend = Body1_y_rm
        ),
        color = '#f4777f',
        linewidth = 0.5
        # arrow = arrow(
        #   type = "closed",
        #   ends = "last",
        #   length = unit(0.15, "cm")
        # )
      ) +
      geom_segment(
        aes(
          x = Body3_x_rm,
          y = Body3_y_rm,
          xend = Body2_x_rm,
          yend = Body2_y_rm
        ),
        color = '#6b93b2',
        linewidth = 0.5
        # arrow = arrow(
        #   type = "closed",
        #   ends = "last",
        #   length = unit(0.15, "cm")
        # )
      ) +
      geom_segment(
        aes(
          x = Tail_x_rm,
          y = Tail_y_rm,
          xend = Body3_x_rm,
          yend = Body3_y_rm
        ),
        color = '#002a7f',
        linewidth = 0.5
        # arrow = arrow(
        #   type = "closed",
        #   ends = "last",
        #   length = unit(0.15, "cm")
        # )
      ) +
      # geom_point(mapping = aes(x = Head_x_rm, y = Head_y_rm),
      #            size = 1) +
      # geom_point(mapping = aes(x = Body1_x_rm, y = Body1_y_rm),
      #            size = 0.5) +
      # geom_point(mapping = aes(x = Body2_x_rm, y = Body2_y_rm),
      #            size = 0.5) +
      # geom_point(mapping = aes(x = Body3_x_rm, y = Body3_y_rm),
      #            size = 0.5) +
      geom_point(mapping = aes(x = Tail_x_rm, y = Tail_y_rm),
                 size = 0.5) +
      geom_text(
        mapping = aes(x = Head_x_rm, y = Head_y_rm, label = ID),
        size = 6,
        size.unit = "pt"
      ) +
      # annotate(
      #   "point",
      #   x = mean(c(d_sprint_select$Body1_x_rm, d_sprint_select$Body3_x_rm)),
      #   y = mean(c(d_sprint_select$Body1_y_rm, d_sprint_select$Body3_y_rm)),
      #   color = "darkred"
      # ) +
      # annotate(
      #   "segment",
      #   x = mean(c(
      #     d_sprint_select$Body1_x_rm, d_sprint_select$Body3_x_rm
      #   )),
      #   y = mean(c(
      #     d_sprint_select$Body1_y_rm, d_sprint_select$Body3_y_rm
      #   )),
      #   xend = mean(c(
      #     d_sprint_select$Body1_x_rm, d_sprint_select$Body3_x_rm
      #   )) + mean(d_sprint_select$Body1_x_rm - d_sprint_select$Body3_x_rm),
      #   yend = mean(c(
      #     d_sprint_select$Body1_y_rm, d_sprint_select$Body3_y_rm
      #   )) + mean(d_sprint_select$Body1_y_rm - d_sprint_select$Body3_y_rm),
      #   linewidth = 0.5,
      #   color = "darkred"
      # ) +
      labs(title = paste0(
        "Trajectory of the selected sprint",
        " (",
        sub("DLC.*", "", vFN),
        ")"
      )) +
      scale_x_continuous(
        name = "Head_x_rm (cm)",
        breaks = seq(0, 100, by = 10),
        limits = c(
          d_sprint_select$Tail_x_rm %>% first - 0.5,
          d_sprint_select$Head_x_rm %>% last + 0.5
        )
      ) +
      scale_y_continuous(
        name = "Head_y_rm (cm)",
        breaks = seq(0, 100, by = 2),
        limits = c(
          c(d_sprint_select$Head_y_rm, d_sprint_select$Tail_y_rm) %>% min - 0.5,
          c(d_sprint_select$Head_y_rm, d_sprint_select$Tail_y_rm) %>% max + 0.5
        )
      ) +
      theme_pubr(base_size = 8,
                 border = T,
                 margin = T) +
      coord_equal()
    
    plot_trajectory_select_mean_bodyparts <-
      ggplot(data = d_sprint_select,
             aes(x = Mean_x_all_body_parts, y = Mean_y_all_body_parts)) +
      # geom_line(linewidth = 0.5) +
      geom_point(size = 0.5) +
      geom_line(
        data = d_sprint_select,
        mapping = aes(x = Mean_x_all_body_parts, y = sprint_group_sprint_y_predict, group = sprint_group_id),
        linetype = 2,
        linewidth = 0.75,
        color = "darkred"
      ) +
      geom_line(
        data = d_sprint_select %>%
          filter(
            ID %in% c(d_sprint_select$ID %>% first, d_sprint_select$ID %>% last)
          ),
        aes(x = Mean_x_all_body_parts, y = Mean_y_all_body_parts),
        linetype = 2,
        color = "darkblue"
      ) +
      scale_x_continuous(
        name = "Mean_x_rm (cm)",
        breaks = seq(0, 100, by = 10),
        limits = c(
          d_sprint_select$Tail_x_rm %>% first - 0.5,
          d_sprint_select$Head_x_rm %>% last + 0.5
        )
      ) +
      scale_y_continuous(
        name = "Mean_y_rm (cm)",
        breaks = seq(0, 100, by = 2),
        limits = c(
          c(d_sprint_select$Head_y_rm, d_sprint_select$Tail_y_rm) %>% min - 0.5,
          c(d_sprint_select$Head_y_rm, d_sprint_select$Tail_y_rm) %>% max + 0.5
        )
      ) +
      theme_pubr(base_size = 8,
                 border = T,
                 margin = T) +
      coord_equal()
    
    # plot the trajectory of the head and tail
    plot_trajectory_select_Head <-
      ggplot() +
      geom_line(data = d_sprint_select, 
                mapping = aes(x = Head_x_rm, y = Head_y_rm),
                linewidth = 0.5,
                color = "darkred") +
      geom_point(data = d_sprint_select, 
                 mapping = aes(x = Head_x_rm, y = Head_y_rm),
                 size = 0.5,
                 color = "darkred") +
      geom_line(data = d_sprint_select, 
                mapping = aes(x = Tail_x_rm, y = Tail_y_rm),
                linewidth = 0.5,
                color = "darkblue") +
      geom_point(data = d_sprint_select, 
                 mapping = aes(x = Tail_x_rm, y = Tail_y_rm),
                 size = 0.5,
                 color = "darkblue") +
      scale_x_continuous(
        name = "x_rm (cm)",
        breaks = seq(0, 100, by = 10),
        limits = c(
          d_sprint_select$Tail_x_rm %>% first - 0.5,
          d_sprint_select$Head_x_rm %>% last + 0.5
        )
      ) +
      scale_y_continuous(
        name = "y_rm (cm)",
        breaks = seq(0, 100, by = 2),
        limits = c(
          c(d_sprint_select$Head_y_rm, d_sprint_select$Tail_y_rm) %>% min - 0.5,
          c(d_sprint_select$Head_y_rm, d_sprint_select$Tail_y_rm) %>% max + 0.5
        )
      ) +
      theme_pubr(base_size = 8,
                 border = T,
                 margin = T) +
      coord_equal()
    
    # vertically arrange two plots
    plot_trajectory_select_arranged <-
      plot_trajectory_select /
      plot_trajectory_select_mean_bodyparts /
      plot_trajectory_select_Head
    return(plot_trajectory_select_arranged)
  }


#### function to plot the vt plot of the selected sprint ####
# output: (plot)plot_v_t_select
f_plot_vt_select <-
  function(d_vel_select, d_sprint_select, ID_sprint_select, vFN) {
    plot_vt_select <-
      ggplot() +
      geom_rect(
        data = ID_sprint_select,
        aes(
          ymin = 0,
          ymax = max(d_vel_select$vel_mod),
          xmin = ID_start,
          xmax = ID_end
        ),
        fill = "lightgray"
      ) +
      geom_text(
        data = ID_sprint_select,
        aes(
          x = ID_start,
          y = max(d_vel_select$vel_mod) + 1,
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
        data = d_vel_select,
        mapping = aes(x = ID, y = vel_mod),
        linewidth = 0.75
      ) +
      geom_segment(
        data = d_vel_select,
        mapping = aes(
          x = ID,
          y = vel_mod,
          xend = ID + dist_mod * cos(Angle_dxdy  / 180 * pi) * 50,
          yend = vel_mod + dist_mod * sin(Angle_dxdy  / 180 * pi) * 50,
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
      labs(title =
             paste0("Vel-t (Head)", " (", sub("DLC.*", "", vFN), ")")) +
      theme_pubr(base_size = 8,
                 border = T,
                 margin = T)
    
    plot_angt_Body1Head_select <-
      ggplot() +
      geom_rect(
        data = ID_sprint_select,
        aes(
          ymin = min(d_sprint_select$Angle_Body1Head),
          ymax = max(d_sprint_select$Angle_Body1Head),
          xmin = ID_start,
          xmax = ID_end
        ),
        fill = "lightgray"
      ) +
      geom_text(
        data = ID_sprint_select,
        aes(
          x = ID_start,
          y = max(d_sprint_select$Angle_Body1Head) + 1,
          label = Num_sprint
        ),
        size = 8,
        size.unit = "pt"
      ) +
      geom_hline(
        aes(yintercept = 0),
        linetype = 2,
        linewidth = 0.5,
        color = "gray"
      ) +
      geom_point(
        data = d_sprint_select,
        mapping = aes(x = ID, y = Angle_Body1Head),
        size = 1
      ) +
      geom_line(
        data = d_sprint_select,
        mapping = aes(x = ID, y = Angle_Body1Head),
        linewidth = 0.5
      ) +
      scale_y_continuous("angle") +
      labs(title = "Angle_Body1Head_track - t") +
      theme_pubr(base_size = 8,
                 border = T,
                 margin = T)
    
    plot_angt_Body3Tail_select <-
      ggplot() +
      geom_rect(
        data = ID_sprint_select,
        aes(
          ymin = min(d_sprint_select$Angle_Body3Tail),
          ymax = max(d_sprint_select$Angle_Body3Tail),
          xmin = ID_start,
          xmax = ID_end
        ),
        fill = "lightgray"
      ) +
      geom_text(
        data = ID_sprint_select,
        aes(
          x = ID_start,
          y = max(d_sprint_select$Angle_Body3Tail) + 1,
          label = Num_sprint
        ),
        size = 8,
        size.unit = "pt"
      ) +
      geom_hline(
        aes(yintercept = 0),
        linetype = 2,
        linewidth = 0.5,
        color = "gray"
      ) +
      geom_point(
        data = d_sprint_select,
        mapping = aes(x = ID, y = Angle_Body3Tail),
        size = 1
      ) +
      geom_line(
        data = d_sprint_select,
        mapping = aes(x = ID, y = Angle_Body3Tail),
        linewidth = 0.5
      ) +
      scale_y_continuous("angle") +
      labs(title = "Angle_Body3Tail_track - t") +
      theme_pubr(base_size = 8,
                 border = T,
                 margin = T)
    
    plot_v_t_select <-
      ggarrange(plot_vt_select,
                plot_angt_Body1Head_select,
                plot_angt_Body3Tail_select,
                nrow = 3)
    return(plot_v_t_select)
  }
