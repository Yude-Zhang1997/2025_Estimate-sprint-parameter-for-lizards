###### Stage 2. Run a sprint video
##### 0. Load packages
library(tidyverse)
library(ggpubr)

# num_classic_sprint <- 標準跑步需要多餘幾個影格
# dist_nearzero <- dist-t離0多近的被看做0
f_analyze_all <- function(filenames, 
                          num_classic_sprint = 50, 
                          dist_nearzero = 0.025, 
                          plot = TRUE) {
  ##### 00. Organize the filenames
  ### ancestral filename
  ## vFN <- filenames
  vFN <- filenames
  print(paste0("Start analysis ", filenames, "."))
  
  ## create folder to save figures and tables
  vFN_folder <- str_replace(vFN, "_colnamefixed.csv", "")
  
  # Check if the folder exists. If not, create the folder
  if (!file.exists(file.path(vFN_folder))) {
    dir.create(file.path(vFN_folder), recursive = TRUE) # Create the folder
    message("The folder did not exist and has been created.")
  } else {
    message("The folder already exists.")
  }
  
  ## filename of exported figures and tables
  vFN_d_sprint_inedge.csv <-
    paste0(vFN_folder, "/d_sprint_inedge.csv")
  vFN_03_plot_trajectory.png <-
    paste0(vFN_folder, "/03_plot_trajectory.png")
  vFN_04_plot_dist_t_remove_replicates.png <-
    paste0(vFN_folder, "/04_plot_dist_t_remove_replicates.png")
  vFN_05_d_sprint_interpolated.csv <-
    paste0(vFN_folder, "/05_d_sprint_interpolated.csv")
  vFN_05_plot_dist_t_interpolated.png <-
    paste0(vFN_folder, "/05_plot_dist_t_interpolated.png")
  vFN_06_d_sprint_rm.csv <-
    paste0(vFN_folder, "/06_d_sprint_rm.csv")
  vFN_06_plot_vt_rm.png <-
    paste0(vFN_folder, "/06_plot_vt_rm.png")
  vFN_06_plot_angt_sprint_rm.png <-
    paste0(vFN_folder, "/06_plot_angt_sprint_rm.png")
  vFN_06_plot_v_t_rm.png <-
    paste0(vFN_folder, "/06_plot_v_t_rm.png")
  vFN_07_plot_v_t_each_sprint.png <-
    paste0(vFN_folder, "/07_plot_v_t_each_sprint.png")
  vFN_08_plot_v_t_each_sprint_no_turn.png <-
    paste0(vFN_folder, "/08_plot_v_t_each_sprint_no_turn.png")
  vFN_10_v_t_select.png <-
    paste0(vFN_folder, "/10_v_t_select.png")
  vFN_12_plot_LF.png <-
    paste0(vFN_folder, "/12_plot_LF.png")
  vFN_12_plot_RF.png <-
    paste0(vFN_folder, "/12_plot_RF.png")
  vFN_12_plot_LB.png <-
    paste0(vFN_folder, "/12_plot_LB.png")
  vFN_12_plot_RB.png <-
    paste0(vFN_folder, "/12_plot_RB.png")
  vFN_13_plot_ang_t_select.png <-
    paste0(vFN_folder, "/13_plot_ang_t_spine.png")
  vFN_14_plot_ang_t_tail.png <-
    paste0(vFN_folder, "/14_plot_ang_t_tail.png")
  
  ##### 01. Read data
  d_sprint <- read_csv(vFN, show_col_types = FALSE)
  d_edge <- read_csv("the ratio and the edge of sprint videos.csv", show_col_types = FALSE)
  
  ##### 02. Change the coordinate system
  # change edge data
  
  d_edge_f <-
    d_edge %>%
    filter(ID == str_replace(vFN, "DLC_colnamefixed.csv", "")) %>%
    mutate(across(ends_with("_y"), ~ 1080 - .)) %>%
    mutate(across(ends_with("_x"), ~ . / Scale)) %>%
    mutate(across(ends_with("_y"), ~ . / Scale))
  
  # change sprint data
  d_sprint_t <-
    d_sprint %>%
    mutate(ID = 1:nrow(.)) %>%
    relocate(ID, .before = "frame") %>%
    mutate(across(ends_with("_y"), ~ 1080 - .)) %>%
    mutate(across(ends_with("_x"), ~ . / d_edge_f$Scale)) %>%
    mutate(across(ends_with("_y"), ~ . / d_edge_f$Scale)) %>%
    select(- frame)
  
  ###### Part I. Construct a reliable data
  source("Stage 1-1. Construct a reliable data.R")
  
  ##### 03. Remove the sprint beside edges #####
  #### target: remove the sprint beside edges
  
  ID_inedge <- f_find_ID_in_edge(data = d_sprint_t, d_edge_f, threshold_edge = 1.5)
  d_sprint_inedge <-
    tibble(ID = d_sprint_t$ID) %>%
    full_join(d_sprint_t %>% filter(ID %in% ID_inedge), by = "ID")
  
  # save
  write_csv(d_sprint_inedge, file = vFN_d_sprint_inedge.csv)
  
  ## To debug, plot the trajectory
  # Trajectory of the whole video
  plot_trajectory_all <-
    ggplot(data = d_sprint_t, aes(x = Head_x, y = Head_y)) +
    geom_point(size = 1, shape = 20) +
    geom_line(linewidth = 0.5) +
    geom_line(
      mapping = aes(x = Head_x, 
                    y = f_edge_top(Head_x, d_edge_f, threshold_edge = 1.5)),
      linewidth = 1,
      linetype = 2,
      color = "gray"
    ) +
    geom_line(
      mapping = aes(x = Head_x, 
                    y = f_edge_bottom(Head_x, d_edge_f, threshold_edge = 1.5)),
      linewidth = 1,
      linetype = 2,
      color = "gray"
    ) +
    geom_line(
      aes(x = f_edge_left(Head_y, d_edge_f, threshold_edge = 1.5), 
          y = Head_y),
      linewidth = 1,
      linetype = 2,
      color = "gray"
    ) +
    coord_equal() +
    theme_pubr(border = T, margin = T)
  
  # Trajectory of the sprint in edges
  plot_trajectory_inedge <-
    ggplot(data = d_sprint_inedge, aes(x = Head_x, y = Head_y)) +
    geom_point(size = 1, shape = 20) +
    geom_line(linewidth = 0.5) +
    geom_line(
      mapping = aes(x = Head_x, 
                    y = f_edge_top(Head_x, d_edge_f, threshold_edge = 1.5)),
      linewidth = 1,
      linetype = 2,
      color = "gray"
    ) +
    geom_line(
      mapping = aes(x = Head_x, 
                    y = f_edge_bottom(Head_x, d_edge_f, threshold_edge = 1.5)),
      linewidth = 1,
      linetype = 2,
      color = "gray"
    ) +
    geom_line(
      aes(x = f_edge_left(Head_y, d_edge_f, threshold_edge = 1.5), 
          y = Head_y),
      linewidth = 1,
      linetype = 2,
      color = "gray"
    ) +
    coord_equal() +
    theme_pubr(border = T, margin = T)
  
  ggarrange(plot_trajectory_all, plot_trajectory_inedge, nrow = 2)
  ggsave(
    vFN_03_plot_trajectory.png,
    width = 16,
    height = 8,
    units = "cm"
  )
  
  ##### 04. Replace the replicated frame with NA #####
  ## Construct velocity data
  # diff <- 第n+1項 - 第n項
  d_vel <- d_sprint_inedge %>%
    select(ID, Head_x, Head_y) %>%
    mutate(
      d_x = if_else(
        row_number() == 1 | ID - lag(ID, default = first(ID)) > 1,
        0,
        Head_x - lag(Head_x, default = first(Head_x))
      ),
      d_y = if_else(
        row_number() == 1 | ID - lag(ID, default = first(ID)) > 1,
        0,
        Head_y - lag(Head_y, default = first(Head_y))
      )
    ) %>%
    mutate(ID = ID - 1) %>%
    slice(-1) %>%
    mutate(dist = sqrt(d_x^2 + d_y^2)) %>%
    mutate(vel = dist * 240)

  
  ## Find frames that were replicated once
  ID_replicate1 <- f_find_ID_replicate1(d_vel, dist_threshold = 0.025)
  
  ## Find frames that were replicated twice
  ID_replicate2 <- f_find_ID_replicate2(d_vel, dist_threshold = 0.025)
  
  # combine replicate 1 frame and 2 frame
  ID_replicate <- c(ID_replicate1, ID_replicate2)
  
  ## Turn the replicated data into NA_real_
  d_sprint_removerep <- f_turn_rep_to_NA(d_sprint_inedge, ID_replicate)
  
  ## Plot to check the progress
  plot_dist_t_rep <-
    ggplot(data = d_vel, aes(x = ID, y = dist)) +
    geom_path() +
    geom_point() +
    theme_pubr(border = T, margin = T)
  
  plot_dist_t_remove_rep <-
    ggplot(
      data = tibble(
        d_x = d_sprint_removerep$Head_x %>% diff,
        d_y = d_sprint_removerep$Head_y %>% diff,
        ID = d_sprint_removerep$ID[1:length(d_sprint_removerep$ID) -
                                     1]
      ) %>%
        mutate(dist = sqrt(d_x^2 + d_y^2)),
      aes(x = ID, y = dist)
    ) +
    geom_path() +
    geom_point() +
    theme_pubr(border = T, margin = T)
  
  ggarrange(plot_dist_t_rep, plot_dist_t_remove_rep, nrow = 2)
  ggsave(
    vFN_04_plot_dist_t_remove_replicates.png,
    width = 16,
    height = 8,
    units = "cm"
  )
  
  
  ##### 05. Replace values whose likelihood less than 0.95 with NA & interpolate NAs
  ## Replace values whose likelihood < 0.95 with NA
  lkd <- 0.95 # modify the likelihood threshold
  d_sprint_lkd <- d_sprint_removerep %>%
    mutate(across(ends_with("_x"), ~ case_when(get(
      sub("_x", "_likelihood", cur_column())
    ) < lkd ~ NA, TRUE ~ .), .names = "{.col}_mod")) %>%
    mutate(across(ends_with("_y"), ~ case_when(get(
      sub("_y", "_likelihood", cur_column())
    ) < lkd ~ NA , TRUE ~ .), .names = "{.col}_mod")) %>%
    select(ID, ends_with("_mod"))
  
  # Check the sprint data with NA
  d_sprint_lkd %>%
    filter(if_any(everything(), is.na))
  
  ## Interpolate only NA values for rows where ID is in selected_IDs
  d_sprint_mod <- d_sprint_lkd %>%
    mutate(across(-ID, ~ {
      if (all(is.na(.)))
        return(.)  # Skip columns with all NA values
      interpolated <- approx(
        x = which(!is.na(.)),
        # Indices of non-NA values
        y = .[!is.na(.)],
        # Non-NA values
        xout = seq_along(.),
        # Indices for all rows
        method = "linear",
        rule = 2                   # Rule to handle edges
      )$y
      replace(., is.na(.) & ID %in% ID_inedge, interpolated[is.na(.) &
                                                              ID %in% ID_inedge])
    }))
  write_csv(d_sprint_mod, vFN_05_d_sprint_interpolated.csv)
  
  ## plot for checking
  ggarrange(
    ggplot(
      data = tibble(
        d_x = d_sprint_lkd$Head_x_mod %>% diff,
        d_y = d_sprint_lkd$Head_y_mod %>% diff
      ) %>%
        mutate(ID = 1:nrow(.)) %>%
        mutate(dist = sqrt(d_x^2 + d_y^2)),
      aes(x = ID, y = dist)
    ) +
      geom_path() +
      geom_point() +
      theme_pubr(border = T, margin = T),
    
    ggplot(
      data = tibble(
        d_x = d_sprint_mod$Head_x_mod %>% diff,
        d_y = d_sprint_mod$Head_y_mod %>% diff
      ) %>%
        mutate(ID = 1:nrow(.)) %>%
        mutate(dist = sqrt(d_x^2 + d_y^2)),
      aes(x = ID, y = dist)
    ) +
      geom_path() +
      geom_point() +
      theme_pubr(border = T, margin = T),
    nrow = 2
  )
  ggsave(
    vFN_05_plot_dist_t_interpolated.png,
    width = 16,
    height = 16,
    units = "cm"
  )
  
  ##### 06. Rolling average and estimate angles
  
  #### Rolling average the sprint parameter
  k <- 5 # Define window size for rolling mean
  
  # List of x and y columns
  columns_to_smooth <- c(
    "Head_x_mod",
    "Head_y_mod",
    "Body1_x_mod",
    "Body1_y_mod",
    "Body2_x_mod",
    "Body2_y_mod",
    "Body3_x_mod",
    "Body3_y_mod",
    "Tail_x_mod",
    "Tail_y_mod",
    "LF_x_mod",
    "LF_y_mod",
    "RF_x_mod",
    "RF_y_mod",
    "LB_x_mod",
    "LB_y_mod",
    "RB_x_mod",
    "RB_y_mod"
  )
  
  # Rolling mean calculation and filtering
  d_sprint_mod1 <- d_sprint_mod %>%
    # Apply rolling mean to all columns dynamically
    mutate(across(ends_with("_mod"), 
                  ~ zoo::rollmean(., k, fill = NA), 
                  .names = "{.col}_rm")) %>%
    rename_with( ~ gsub("_mod_rm", "_rm", .), .cols = everything()) %>%
    select(ID, ends_with("_rm")) %>%
    # Filter rows where all rolling mean columns are > 0
    filter(if_any(ends_with("_rm"), ~ !is.na(.)))
  
  ### Calculate angles
  d_sprint_rm <- d_sprint_mod1 %>%
    mutate(Angle_LF = pmap_dbl(., function(LF_x_rm,
                                           LF_y_rm,
                                           Body1_x_rm,
                                           Body1_y_rm,
                                           Body3_x_rm,
                                           Body3_y_rm,
                                           ...) {
      f_calculate_angle(
        LF_x_rm,
        LF_y_rm,
        Body1_x_rm,
        Body1_y_rm,
        Body3_x_rm,
        Body3_y_rm,
        flip = TRUE,
        cross = FALSE
      )
    })) %>%
    mutate(Angle_RF = pmap_dbl(., function(RF_x_rm,
                                           RF_y_rm,
                                           Body1_x_rm,
                                           Body1_y_rm,
                                           Body3_x_rm,
                                           Body3_y_rm,
                                           ...) {
      f_calculate_angle(
        RF_x_rm,
        RF_y_rm,
        Body1_x_rm,
        Body1_y_rm,
        Body3_x_rm,
        Body3_y_rm,
        flip = T,
        cross = F
      )
    })) %>%
    mutate(Angle_LB = pmap_dbl(., function(LB_x_rm,
                                           LB_y_rm,
                                           Body3_x_rm,
                                           Body3_y_rm,
                                           Body1_x_rm,
                                           Body1_y_rm,
                                           ...) {
      f_calculate_angle(
        LB_x_rm,
        LB_y_rm,
        Body3_x_rm,
        Body3_y_rm,
        Body1_x_rm,
        Body1_y_rm,
        flip = F,
        cross = F
      )
    })) %>%
    mutate(Angle_RB = pmap_dbl(., function(RB_x_rm,
                                           RB_y_rm,
                                           Body3_x_rm,
                                           Body3_y_rm,
                                           Body1_x_rm,
                                           Body1_y_rm,
                                           ...) {
      f_calculate_angle(
        RB_x_rm,
        RB_y_rm,
        Body3_x_rm,
        Body3_y_rm,
        Body1_x_rm,
        Body1_y_rm,
        flip = F,
        cross = F
      )
    })) %>%
    mutate(Angle_Tail = pmap_dbl(., function(Tail_x_rm,
                                             Tail_y_rm,
                                             Body3_x_rm,
                                             Body3_y_rm,
                                             Body1_x_rm,
                                             Body1_y_rm,
                                             ...) {
      #往身體右側為正
      f_calculate_angle(
        Tail_x_rm,
        Tail_y_rm,
        Body3_x_rm,
        Body3_y_rm,
        Body1_x_rm,
        Body1_y_rm,
        flip = T,
        cross = T
      )
    })) %>%
    mutate(Angle_Spine = pmap_dbl(., function(Body1_x_rm,
                                              Body1_y_rm,
                                              Body2_x_rm,
                                              Body2_y_rm,
                                              Body3_x_rm,
                                              Body3_y_rm,
                                              ...) {
      # 往身體右側為正
      f_calculate_angle(
        Body1_x_rm,
        Body1_y_rm,
        Body2_x_rm,
        Body2_y_rm,
        Body3_x_rm,
        Body3_y_rm,
        flip = T,
        cross = T
      )
    }))
  write_csv(d_sprint_rm, vFN_06_d_sprint_rm.csv)
  
  ## Construct the velocity data
  ## todo: not calculate na diff

  d_vel_move <- d_sprint_rm %>%
    select(ID, Head_x_rm, Head_y_rm) %>%
    mutate(
      dx = if_else(
        row_number() == 1 | ID - lag(ID, default = first(ID)) > 1,
        0,
        Head_x_rm - lag(Head_x_rm, default = first(Head_x_rm))
      ),
      dy = if_else(
        row_number() == 1 | ID - lag(ID, default = first(ID)) > 1,
        0,
        Head_y_rm - lag(Head_y_rm, default = first(Head_y_rm))
      )
    ) %>% 
    mutate(ID = ID - 1) %>% 
    slice(- 1) %>%
    mutate(dist = sqrt(dx^2 + dy^2)) %>%
    mutate(cum_dist = cumsum(dist)) %>%
    mutate(vel = dist * 240) %>%
    mutate(Angle_sprint = pmap_dbl(., function(dx, dy, ...) {
      V1 <- c(dx, dy) %>% as.vector
      V2 <- c((
        d_edge_f$Corner_RT_x - d_edge_f$Corner_LT_x +
          d_edge_f$Corner_RB_x - d_edge_f$Corner_LB_x
      ) / 2,
      (
        d_edge_f$Corner_RT_y - d_edge_f$Corner_LT_y +
          d_edge_f$Corner_RB_y - d_edge_f$Corner_LB_y
      ) / 2
      ) %>% as.vector
      angle = acos(sum(V1 * V2) / (sqrt(sum(V1 * V1)) * sqrt(sum(V2 * V2)))) * 180 / pi
      V1_slope <- ifelse(V1[1] == 0, Inf, V1[2] / V1[1])
      V2_slope <- ifelse(V2[1] == 0, Inf, V2[2] / V2[1])
      if (dx >= 0) {
        if (V1_slope >= V2_slope) angle else - angle
      } else {
        if (V1_slope >= V2_slope) -angle else angle
      }
    }))
  
  
  ### Explore the data
  Plot_trajectory_rm <-
    ggplot(data = d_sprint_rm,
           mapping = aes(x = Head_x_rm, y = Head_y_rm)) +
    geom_point(size = 1) +
    labs(title = "Trajectory") +
    scale_x_continuous(name = "Head_x_rm (cm)", breaks = seq(0, 100, by = 10)) +
    scale_y_continuous(name = "Head_y_rm (cm)") +
    theme_pubr(border = T, margin = T) +
    coord_equal()
  ggsave(
    vFN_06_plot_vt_rm.png,
    width = 16,
    height = 8,
    units = "cm"
  )

  
  if (nrow(d_vel_move) > 0){
    Plot_angt_sprint_rm <-
      ggplot(data = d_vel_move, mapping = aes(x = ID, y = Angle_sprint)) +
      geom_hline(
        aes(yintercept = 0),
        linetype = 2,
        linewidth = 0.5,
        color = "gray"
      ) +
      geom_point(size = 1) +
      geom_line(linewidth = 0.5) +
      scale_y_continuous("angle") +
      labs(title = "Moving direction-t") +
      theme_pubr(border = T, margin = T)
    ggsave(
      vFN_06_plot_angt_sprint_rm.png,
      width = 16,
      height = 8,
      units = "cm"
    )
    
    Plot_vt_rm <-
      ggplot(data = d_vel_move, mapping = aes(x = ID, y = vel)) +
      geom_hline(
        aes(yintercept = 0),
        linewidth = 0.5,
        linetype = 2,
        color = "gray"
      ) +
      geom_line(linewidth = 0.75) +
      geom_segment(
        aes(
          x = ID,
          y = vel,
          xend = ID + dist * cos(Angle_sprint / 180 * pi) * 50,
          yend = vel + dist * sin(Angle_sprint / 180 * pi) * 50,
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
    vFN_06_plot_v_t_rm.png,
    width = 16,
    height = 8,
    units = "cm"
  )}
  
  
  
  
  ###### Part II. Find the classic sprint
  source("Stage 1-2. Find the classic sprint.R")
  
  ##### 07. Find every sprint
  ## remove small error near 0
  d_vel_mod <- d_vel_move %>%
    mutate(dist_mod = if_else(dist < dist_nearzero, 0, dist)) %>%
    mutate(vel_mod = dist_mod * 240)
  
  ### Find all sprints
  ID_sprint <- f_find_sprint(
    data = d_vel_mod,
    ID = "ID",
    velocity = "vel_mod",
    num_classic_sprint = 50,
    plot = TRUE
  )

  #### Determine whether nrow(ID_sprint) > 0. IF not, parameters are "no work"
  if (!is.null(ID_sprint)){
    ##### 08. Split sharp turns from sprints
    ID_turn <- f_find_turn(ID_sprint, d_vel_mod)
    
    ### Remove the turns then return new start and end of each new sprint
    ID_sprint_no_turn <- f_remove_turn_from_sprint(ID_sprint, ID_turn)
    
    ## plot to check
    Plot_vt_each_sprint_no_turn <-
      ggplot() +
      geom_rect(
        data = ID_sprint_no_turn,
        aes(
          ymin = 0,
          ymax = max(d_vel_mod$vel_mod),
          xmin = ID_start,
          xmax = ID_end
        ),
        fill = "lightgray"
      ) +
      geom_text(
        data = ID_sprint_no_turn,
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
      scale_x_continuous("ID") +
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
      vFN_08_plot_v_t_each_sprint_no_turn.png,
      width = 16,
      height = 8,
      units = "cm"
    )
    
    ##### 09. Choose long sprints
    ## frame number should be more than 50.
    ID_sprint_long <- f_choose_long_sprint(ID_sprint_no_turn, num_classic_sprint = 50)
    is_at_least_num_classic_sprint <- !is.null(ID_sprint_long)
    
    ##### decide whether there is at least one classic sprint #####
    if (is_at_least_num_classic_sprint) {
      ##### 10. Select the sprint with the max speed
      ## select the sprint with highest speed
      ID_sprint_select <- f_select_sprint_max_speed(ID_sprint_long, d_vel_mod)
      
      d_vel_select <-
        d_vel_mod %>% filter(ID %in% (ID_sprint_select$ID_start):(ID_sprint_select$ID_end))
      
      d_sprint_select <-
        d_sprint_rm %>% filter(ID %in% (ID_sprint_select$ID_start):(ID_sprint_select$ID_end + 1))
      
      ## Plot to check
      Plot_vt_select <-
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
        vFN_10_v_t_select.png,
        width = 16,
        height = 8,
        units = "cm"
      )
      
      ###### Part III. Estimate locomotion parameters
      source("Stage 1-3. Estimate locomotion parameters.R")
      
      ##### 11. Estimate speed parameter
      speed <- d_vel_select$vel_mod # return

      ##### 12. Estimate step rate, step length, and step angle #####
      #### Estimate step rate, step length, and limb angle of the left forelimb (LF) ####
      ## Construct velocity data
      d_vel_LF <-
        tibble(
          dx = d_sprint_select$LF_x_rm %>% diff,
          dy = d_sprint_select$LF_y_rm %>% diff,
          dang = d_sprint_select$Angle_LF %>% diff
        ) %>%
        mutate(dist = sqrt(dx^2 + dy^2)) %>%
        mutate(ID = d_sprint_select$ID[1:(length(d_sprint_select$ID) - 1)]) %>%
        mutate(vel = dist * 240) %>%
        mutate(dist_mod = if_else(dist < 0.02, 0, dist)) %>%
        mutate(vel_mod = dist_mod * 240) %>%
        mutate(dang_mod = if_else(abs(dang) < 1, 0, dang)) %>%
        mutate(phase = if_else(dang_mod >= 0, "stance", "swing"))
      
      ## plot to check
      Plot_vt_LF <-
        ggplot(data = d_vel_LF, mapping = aes(x = ID, y = vel_mod)) +
        geom_line() +
        geom_point(aes(color = phase)) +
        scale_y_continuous("Velocity (cm / s)") +
        scale_color_manual(values = c("blue", "red")) +
        labs(title = "V-t (Left forelimb)") +
        theme_pubr(border = T,
                   margin = T,
                   legend = "top")
      Plot_dangt_LF <-
        ggplot(data = d_vel_LF, mapping = aes(x = ID, y = dang_mod)) +
        geom_hline(
          aes(yintercept = 0),
          linetype = 3,
          color = "gray",
          linewidth = 1
        ) +
        geom_line() +
        geom_point(aes(color = phase)) +
        scale_y_continuous("Degree") +
        scale_color_manual(values = c("blue", "red")) +
        labs(title = "dAng-t (Left forelimb)") +
        theme_pubr(border = T,
                   margin = T,
                   legend = "top")
      Plot_angt_LF <-
        ggplot(data = d_sprint_select, aes(x = ID, y = Angle_LF)) +
        geom_hline(
          aes(yintercept = mean(Angle_LF)),
          linewidth = 0.5,
          color = "darkgray",
          linetype = 2
        ) +
        geom_point(size = 1) +
        geom_line(linewidth = 0.6) +
        scale_y_continuous(name = "Degree") +
        labs(title = "Ang-t (Lef forelimb)") +
        theme_pubr(border = T, margin = T)
      
      ggarrange(
        Plot_vt_LF,
        Plot_dangt_LF,
        Plot_angt_LF,
        nrow = 3,
        common.legend = T
      )
      ggsave(
        vFN_12_plot_LF.png,
        width = 16,
        height = 24,
        units = "cm"
      )
      
      ### Find the ID of each swing
      ID_swing_LF <- f_find_ID_swing(d_vel_LF)
      
      # step_rate_LF
      step_rate_LF <-
        nrow(ID_swing_LF) / ((ID_swing_LF$ID_swing_end[nrow(ID_swing_LF)] - ID_swing_LF$ID_swing_start[1]) / 240) # return

      
      ## step_length_LF
      step_length_LF <- f_estimate_step_length(d_sprint_select, ID_swing = ID_swing_LF, limb = "LF") # return

      ## limb_angle_LF
      limb_angle_LF <- f_estimate_limb_angle(d_sprint_select, ID_swing = ID_swing_LF, limb = "LF") # return
      
      #### Estimate step rate, step length, and limb angle of the right forelimb (RF) ####
      ## Construct velocity data
      d_vel_RF <-
        tibble(
          dx = d_sprint_select$RF_x_rm %>% diff,
          dy = d_sprint_select$RF_y_rm %>% diff,
          dang = d_sprint_select$Angle_RF %>% diff
        ) %>%
        mutate(dist = sqrt(dx^2 + dy^2)) %>%
        mutate(ID = d_sprint_select$ID[1:(length(d_sprint_select$ID) - 1)]) %>%
        mutate(vel = dist * 240) %>%
        mutate(dist_mod = if_else(dist < 0.02, 0, dist)) %>%
        mutate(vel_mod = dist_mod * 240) %>%
        mutate(dang_mod = if_else(abs(dang) < 1, 0, dang)) %>%
        mutate(phase = if_else(dang_mod >= 0, "stance", "swing"))

      ## plot to check
      Plot_vt_RF <-
        ggplot(data = d_vel_RF, mapping = aes(x = ID, y = vel_mod)) +
        geom_line() +
        geom_point(aes(color = phase)) +
        scale_y_continuous("Velocity (cm / s)") +
        scale_color_manual(values = c("blue", "red")) +
        labs(title = "V-t (Right forelimb)") +
        theme_pubr(border = T,
                   margin = T,
                   legend = "top")
      Plot_dangt_RF <-
        ggplot(data = d_vel_RF, mapping = aes(x = ID, y = dang_mod)) +
        geom_hline(
          aes(yintercept = 0),
          linetype = 3,
          color = "gray",
          linewidth = 1
        ) +
        geom_line() +
        geom_point(aes(color = phase)) +
        scale_y_continuous("Degree") +
        scale_color_manual(values = c("blue", "red")) +
        labs(title = "dAng-t (Right forelimb)") +
        theme_pubr(border = T,
                   margin = T,
                   legend = "top")
      Plot_angt_RF <-
        ggplot(data = d_sprint_select, aes(x = ID, y = Angle_RF)) +
        geom_hline(
          aes(yintercept = mean(Angle_RF)),
          linewidth = 0.5,
          color = "darkgray",
          linetype = 2
        ) +
        geom_point(size = 1) +
        geom_line(linewidth = 0.6) +
        scale_y_continuous(name = "Degree") +
        labs(title = "Ang-t (Right forelimb)") +
        theme_pubr(border = T, margin = T)
      
      ggarrange(
        Plot_vt_RF,
        Plot_dangt_RF,
        Plot_angt_RF,
        nrow = 3,
        common.legend = T
      )
      ggsave(
        vFN_12_plot_RF.png,
        width = 16,
        height = 24,
        units = "cm"
      )
      
      ### Find the ID of each swing of RF
      ID_swing_RF <- f_find_ID_swing(d_vel_RF)

      # step_rate_RF
      step_rate_RF <-
        nrow(ID_swing_RF) / ((ID_swing_RF$ID_swing_end[nrow(ID_swing_RF)] - ID_swing_RF$ID_swing_start[1]) / 240) #return
      
      ## step_length_RF
      step_length_RF <- f_estimate_step_length(d_sprint_select, ID_swing = ID_swing_RF, limb = "RF") #return
      
      ## limb_angle_RF
      limb_angle_RF <- f_estimate_limb_angle(d_sprint_select, ID_swing = ID_swing_RF, limb = "RF") #return
      
      #### Estimate step rate, step length, and limb angle of the left hind limb (LB) ####
      ## Construct velocity data
      d_vel_LB <-
        tibble(
          dx = d_sprint_select$LB_x_rm %>% diff,
          dy = d_sprint_select$LB_y_rm %>% diff,
          dang = d_sprint_select$Angle_LB %>% diff
        ) %>%
        mutate(dist = sqrt(dx^2 + dy^2)) %>%
        mutate(ID = d_sprint_select$ID[1:(length(d_sprint_select$ID) - 1)]) %>%
        mutate(vel = dist * 240) %>%
        mutate(dist_mod = if_else(dist < 0.02, 0, dist)) %>%
        mutate(vel_mod = dist_mod * 240) %>%
        mutate(dang_mod = if_else(abs(dang) < 1, 0, dang)) %>%
        mutate(phase = if_else(dang_mod >= 0, "stance", "swing"))
      
      ## plot to check
      Plot_vt_LB <-
        ggplot(data = d_vel_LB, mapping = aes(x = ID, y = vel_mod)) +
        geom_line() +
        geom_point(aes(color = phase)) +
        scale_y_continuous("Velocity (cm / s)") +
        scale_color_manual(values = c("blue", "red")) +
        labs(title = "V-t (Left hind limb)") +
        theme_pubr(border = T,
                   margin = T,
                   legend = "top")
      
      Plot_dangt_LB <-
        ggplot(data = d_vel_LB, mapping = aes(x = ID, y = dang_mod)) +
        geom_hline(
          aes(yintercept = 0),
          linetype = 3,
          color = "gray",
          linewidth = 1
        ) +
        geom_line() +
        geom_point(aes(color = phase)) +
        scale_y_continuous("Degree") +
        scale_color_manual(values = c("blue", "red")) +
        labs(title = "dAng-t (Left hind limb)") +
        theme_pubr(border = T,
                   margin = T,
                   legend = "top")
      Plot_angt_LB <-
        ggplot(data = d_sprint_select, aes(x = ID, y = Angle_LB)) +
        geom_hline(
          aes(yintercept = mean(Angle_LB)),
          linewidth = 0.5,
          color = "darkgray",
          linetype = 2
        ) +
        geom_point(size = 1) +
        geom_line(linewidth = 0.6) +
        scale_y_continuous(name = "Degree") +
        labs(title = "Ang-t (Left hind limb)") +
        theme_pubr(border = T, margin = T)
      
      ggarrange(
        Plot_vt_LB,
        Plot_dangt_LB,
        Plot_angt_LB,
        nrow = 3,
        common.legend = T
      )
      ggsave(
        vFN_12_plot_LB.png,
        width = 16,
        height = 24,
        units = "cm"
      )
      
      ### Find the ID of each swing of LB
      ID_swing_LB <- f_find_ID_swing(d_vel_LB)

      # step rate of LB
      step_rate_LB <-
        nrow(ID_swing_LB) / ((ID_swing_LB$ID_swing_end[nrow(ID_swing_LB)] - ID_swing_LB$ID_swing_start[1]) / 240) #return
      
      ## step_length_LB
      step_length_LB <- f_estimate_step_length(d_sprint_select, 
                                               ID_swing = ID_swing_LB,
                                               limb = "LB") #return
      
      ## limb_angle_LB
      limb_angle_LB <- f_estimate_limb_angle(d_sprint_select, 
                                             ID_swing = ID_swing_LB, 
                                             limb = "LB") #return
      
      #### Estimate step rate, step length, and limb angle of the right hind limb (RB) ####
      ## Construct velocity data
      d_vel_RB <-
        tibble(
          dx = d_sprint_select$RB_x_rm %>% diff,
          dy = d_sprint_select$RB_y_rm %>% diff,
          dang = d_sprint_select$Angle_RB %>% diff
        ) %>%
        mutate(dist = sqrt(dx^2 + dy^2)) %>%
        mutate(ID = d_sprint_select$ID[1:(length(d_sprint_select$ID) - 1)]) %>%
        mutate(vel = dist * 240) %>%
        mutate(dist_mod = if_else(dist < 0.02, 0, dist)) %>%
        mutate(vel_mod = dist_mod * 240) %>%
        mutate(dang_mod = if_else(abs(dang) < 1, 0, dang)) %>%
        mutate(phase = if_else(dang_mod >= 0, "stance", "swing"))
      
      ## plot to check
      Plot_vt_RB <-
        ggplot(data = d_vel_RB, mapping = aes(x = ID, y = vel_mod)) +
        geom_line() +
        geom_point(aes(color = phase)) +
        scale_y_continuous("Velocity (cm / s)") +
        scale_color_manual(values = c("blue", "red")) +
        labs(title = "V-t (Right hind limb)") +
        theme_pubr(border = T,
                   margin = T,
                   legend = "top")
      Plot_dangt_RB <-
        ggplot(data = d_vel_RB, mapping = aes(x = ID, y = dang_mod)) +
        geom_hline(
          aes(yintercept = 0),
          linetype = 3,
          color = "gray",
          linewidth = 1
        ) +
        geom_line() +
        geom_point(aes(color = phase)) +
        scale_y_continuous("Degree") +
        scale_color_manual(values = c("blue", "red")) +
        labs(title = "dAng-t (Right hind limb)") +
        theme_pubr(border = T,
                   margin = T,
                   legend = "top")
      Plot_angt_RB <-
        ggplot(data = d_sprint_select, aes(x = ID, y = Angle_RB)) +
        geom_hline(
          aes(yintercept = mean(Angle_RB)),
          linewidth = 0.5,
          color = "darkgray",
          linetype = 2
        ) +
        geom_point(size = 1) +
        geom_line(linewidth = 0.6) +
        scale_y_continuous(name = "Degree") +
        labs(title = "Ang-t (Right hind limb)") +
        theme_pubr(border = T, margin = T)
      
      ggarrange(
        Plot_vt_RB,
        Plot_dangt_RB,
        Plot_angt_RB,
        nrow = 3,
        common.legend = T
      )
      ggsave(
        vFN_12_plot_RB.png,
        width = 16,
        height = 24,
        units = "cm"
      )
      
      ### Find the ID of each swing of LB
      ID_swing_RB <- f_find_ID_swing(d_vel_RB)
      
      # step rate of RB
      step_rate_RB <-
        nrow(ID_swing_RB) / ((ID_swing_RB$ID_swing_end[nrow(ID_swing_RB)] - ID_swing_RB$ID_swing_start[1]) / 240) #return
      
      ## step_length_RB
      step_length_RB <- f_estimate_step_length(d_sprint_select, 
                                               ID_swing = ID_swing_RB, 
                                               limb = "RB") #return
      
      ## limb_angle_RB
      limb_angle_RB <- f_estimate_limb_angle(d_sprint_select, 
                                             ID_swing = ID_swing_RB, 
                                             limb = "RB") #return
      
      ##### 13. Estimate spine angle
      Plot_Ang_t_select <-
        ggplot(data = d_sprint_select, aes(x = ID, y = Angle_Spine)) +
        geom_point(size = 1) +
        geom_line(linewidth = 0.75) +
        geom_hline(aes(yintercept = 0),
                   color = "gray",
                   linetype = 2) +
        scale_x_continuous(name = "ID") +
        scale_y_continuous(name = "Degree") +
        labs(title = "Ang_Spine - t") +
        theme_pubr(border = T, margin = T)
      ggsave(
        vFN_13_plot_ang_t_select.png,
        width = 16,
        height = 8,
        units = "cm"
      )
      
      # Ang_spine_right
      Angle_spine_right <- f_estimate_spine_angle(d_sprint_select, position = "right") #return
      
      # Ang_spine_left
      Angle_spine_left <- f_estimate_spine_angle(d_sprint_select, position = "left") #return
      
      ##### 14. Estimate tail angle
      Plot_Ang_t_tail <-
        ggplot(data = d_sprint_select, aes(x = ID, y = Angle_Tail)) +
        geom_point(size = 1) +
        geom_line(linewidth = 0.6) +
        geom_hline(aes(yintercept = 0),
                   color = "gray",
                   linetype = 2) +
        scale_x_continuous(name = "ID") +
        scale_y_continuous(name = "Degree") +
        labs(title = "Ang_Tail - t") +
        theme_pubr(border = T, margin = T)
      ggsave(
        vFN_14_plot_ang_t_tail.png,
        width = 16,
        height = 8,
        units = "cm"
      )
      
      # Angle_tail_right
      Angle_tail_right <- f_estimate_tail_angle(d_sprint_select, position = "right") #return
      
      # Angle_tail_left
      Angle_tail_left <- f_estimate_tail_angle(d_sprint_select, position = "left") #return
    } else {
      speed <- "no work"
      step_rate_LF <- "no work"
      step_length_LF <- "no work"
      limb_angle_LF <- "no work"
      step_rate_RF <- "no work"
      step_length_RF <- "no work"
      limb_angle_RF <- "no work"
      step_rate_LB <- "no work"
      step_length_LB <- "no work"
      limb_angle_LB <- "no work"
      step_rate_RB <- "no work"
      step_length_RB <- "no work"
      limb_angle_RB <- "no work"
      Angle_spine_right <- "no work"
      Angle_spine_left <- "no work"
      Angle_tail_right <- "no work"
      Angle_tail_left <- "no work"
    }
  } else {
    speed <- "no work"
    step_rate_LF <- "no work"
    step_length_LF <- "no work"
    limb_angle_LF <- "no work"
    step_rate_RF <- "no work"
    step_length_RF <- "no work"
    limb_angle_RF <- "no work"
    step_rate_LB <- "no work"
    step_length_LB <- "no work"
    limb_angle_LB <- "no work"
    step_rate_RB <- "no work"
    step_length_RB <- "no work"
    limb_angle_RB <- "no work"
    Angle_spine_right <- "no work"
    Angle_spine_left <- "no work"
    Angle_tail_right <- "no work"
    Angle_tail_left <- "no work"
  }
  res <-
    list(
      speed,
      step_rate_LF,
      step_length_LF,
      limb_angle_LF,
      step_rate_RF,
      step_length_RF,
      limb_angle_RF,
      step_rate_LB,
      step_length_LB,
      limb_angle_LB,
      step_rate_RB,
      step_length_RB,
      limb_angle_RB,
      Angle_spine_right,
      Angle_spine_left,
      Angle_tail_right,
      Angle_tail_left
    )
  print(paste0("End analysis ", filenames, "."))
  return(res)
}

#### test ####
# filenames <- "211112MG3DLC_colnamefixed.csv" # DLC predict有誤
# filenames <- "211052MnG1DLC_colnamefixed.csv" # success
# filenames <- "211116MnG1DLC_colnamefixed.csv" # success
# filenames <- "211252FnG2DLC_colnamefixed.csv" # success
# filenames <- "220371FG3DLC_colnamefixed.csv" # ?
# filenames <- "2112124FnG1DLC_colnamefixed.csv" # success
# filenames <- "211111FnG2DLC_colnamefixed.csv" # success
# filenames <- "211116MnG2DLC_colnamefixed.csv" # success
# filenames <- "2112122MG1DLC_colnamefixed.csv" # failed (error in DLC)
# filenames <- "211172FnG2DLC_colnamefixed.csv" # all coordinates beside the edge
# filenames <- "211176MnG2DLC_colnamefixed.csv" # 邊界data有誤
# filenames <- "2112124FnG3DLC_colnamefixed.csv" # 無停止從頭跑到尾

f_analyze_all(filenames)


