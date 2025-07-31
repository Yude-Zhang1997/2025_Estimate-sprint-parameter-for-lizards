###### Stage 2. Run a sprint video
##### 0. Load packages

library(tidyverse)
library(ggpubr)
library(zoo)
library(patchwork)

# prevent the conflict of "filter"
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")




# num_classic_sprint <- 標準跑步需要多餘幾個影格
# dist_nearzero <- dist-t離0多近的被看做0
f_analyze_all <-
  function(filenames,
           DLC_lkd = 0.9,
           length_track = 90,
           num_classic_sprint = 60,
           threshold_edge = 1,
           dist_nearzero = 0.02,
           plot = TRUE) {
    ##### 00. Organize the filenames
    ### ancestral filename
    ## vFN <- filenames
    vFN <- filenames
    print(paste0("Start analysis ", filenames, "."))
    
    ## create folder to save figures and tables
    vFN_folder <- paste0("analysis/", str_replace(vFN, "_colnamefixed.csv", ""))
    
    
    # Check if the folder exists. If not, create the folder
    if (!file.exists(file.path(vFN_folder))) {
      dir.create(file.path(vFN_folder), recursive = TRUE) # Create the folder
      message("The folder did not exist and has been created.")
    } else {
      message("The folder already exists.")
    }
    
    ## filename of exported figures and tables
    vFN_03_plot_trajectory.png <-
      paste0(vFN_folder, "/1_plot_trajectory.png")
    vFN_04_plot_dist_t_remove_replicates.png <-
      paste0(vFN_folder, "/1_plot_dist_t_remove_replicates.png")
    vFN_05_d_sprint_interpolated.csv <-
      paste0(vFN_folder, "/1_d_sprint_interpolated.csv")
    vFN_05_plot_dist_t_interpolated.png <-
      paste0(vFN_folder, "/1_plot_dist_t_interpolated.png")
    vFN_06_d_sprint_rm.csv <-
      paste0(vFN_folder, "/1_d_sprint_rm.csv")
    vFN_06_plot_trajectory_rm.png <-
      paste0(vFN_folder, "/1_plot_trajectory_rm.png")
    vFN_06_plot_v_t_rm.png <-
      paste0(vFN_folder, "/1_plot_v_t_rm.png")
    
    vFN_2_plot_vt_select_standard_sprint.png <-
      paste0(vFN_folder, "/2_plot_vt_select_standard_sprint.png")
    
    vFN_3.1_trajectory_select.png <-
      paste0(vFN_folder, "/3.1_trajectory_select.png")
    vFN_10_v_t_select_raw.png <-
      paste0(vFN_folder, "/3.1_v_t_select_all.png")
    
    vFN_plot_amplitude.png <-
      paste0(vFN_folder, "/3.2_plot_amplitude.png")
    vFN_plot_lateral_undulation_angle.png <-
      paste0(vFN_folder, "/3.2_plot_lateral_undulation_angle.png")
    
    vFN_plot_velocity_limb.png <-
      paste0(vFN_folder, "/3.3_plot_velocity_limb.png")
    vFN_plot_diff_angle_limb.png <-
      paste0(vFN_folder, "/3.3_plot_diff_angle_limb.png")
    vFN_plot_angle_limb.png <-
      paste0(vFN_folder, "/3.3_plot_angle_limb.png")
    
    ##### 01. Read data
    d_sprint <- read_csv(vFN, show_col_types = FALSE)
    
    ##### 02. Change the coordinate system
    # change sprint data
    Ratio_pixel_cm <-
      sqrt((median(d_sprint[["edge_RT_x"]]) -
              median(d_sprint[["edge_LT_x"]]))^2 +
             (median(d_sprint[["edge_RT_y"]]) -
                median(d_sprint[["edge_LT_y"]]))^2) / length_track
    
    d_sprint_t <-
      d_sprint %>%
      mutate(ID = 1:nrow(.)) %>%
      relocate(ID, .before = "frame") %>%
      mutate(across(ends_with("_y"), ~ 1080 - .)) %>%
      mutate(across(ends_with("_x"), ~ . / Ratio_pixel_cm)) %>%
      mutate(across(ends_with("_y"), ~ . / Ratio_pixel_cm)) %>%
      select(-frame)
    
    ###### Part I. Construct a reliable data
    source(
      "D:/personal onedrive/OneDrive/1. Projects/2023-2025_Sprint locomotion of autotomy grass lizards/2025.05_ Quantify the lateral undulation/[Code] Autotomy and Control/2025.03_Stage 1-1. Construct a reliable data.R"
    )
    print("Construct a reliable data.")
    
    ##### 03. Remove the sprint beside edges #####
    #### target: remove the sprint beside edges
    print("Remove the sprint beside edges.")
    
    ID_inedge <-
      f_find_ID_in_edge(d = d_sprint_t, threshold_edge)
    d_sprint_inedge <-
      tibble(ID = d_sprint_t$ID) %>%
      full_join(d_sprint_t %>%
                  filter(ID %in% ID_inedge), by = "ID")
    
    ## To debug, plot the trajectory
    if (plot == TRUE) {
      # Trajectory of the whole video
      plot_trajectory_all <-
        ggplot(data = d_sprint_t, aes(x = Head_x, y = Head_y)) +
        geom_point(size = 1, shape = 20,
                   na.rm = T) +
        geom_line(linewidth = 0.5,
                  na.rm = T) +
        geom_line(
          aes(
            x = Head_x,
            y = f_edge_top(Head_x, d = d_sprint_t, threshold_edge = 1)
          ),
          linewidth = 1,
          linetype = 2,
          color = "gray",
          na.rm = T
        ) +
        geom_line(
          aes(
            x = Head_x,
            y = f_edge_bottom(Head_x, d = d_sprint_t, threshold_edge = 1)
          ),
          linewidth = 1,
          linetype = 2,
          color = "gray",
          na.rm = T
        ) +
        geom_line(
          aes(
            x = f_edge_left(Head_y, d = d_sprint_t, threshold_edge = 1),
            y = Head_y
          ),
          linewidth = 1,
          linetype = 2,
          color = "gray",
          na.rm = T
        ) +
        coord_equal() +
        theme_pubr(border = T, margin = T)
      
      # Trajectory of the sprint in edges
      plot_trajectory_inedge <-
        ggplot(data = d_sprint_inedge, aes(x = Head_x, y = Head_y)) +
        geom_point(size = 1, 
                   shape = 20,
                   na.rm = T) +
        geom_line(linewidth = 0.5,
                  na.rm = T) +
        geom_line(
          aes(
            x = Head_x,
            y = f_edge_top(Head_x, d = d_sprint_inedge, threshold_edge = 1)
          ),
          linewidth = 1,
          linetype = 2,
          color = "gray",
          na.rm = T
        ) +
        geom_line(
          aes(
            x = Head_x,
            y = f_edge_bottom(Head_x, d = d_sprint_inedge, threshold_edge = 1)
          ),
          linewidth = 1,
          linetype = 2,
          color = "gray",
          na.rm = T
        ) +
        geom_line(
          aes(
            x = f_edge_left(Head_y, d = d_sprint_inedge, threshold_edge = 1),
            y = Head_y
          ),
          linewidth = 1,
          linetype = 2,
          color = "gray",
          na.rm = T
        ) +
        coord_equal() +
        theme_pubr(border = T, margin = T)
      
      plot_trajectory <- 
        ggarrange(plot_trajectory_all, plot_trajectory_inedge, nrow = 2)
      
      ggsave(
        vFN_03_plot_trajectory.png,
        width = 16,
        height = 8,
        units = "cm"
      )
    }
    
    
    ##### 04. Replace the replicated frame with NA #####
    print("Replace the replicated frame with NA.")
    
    ## Construct velocity data
    # diff <- 第n+1項 - 第n項
    d_vel <- d_sprint_inedge %>%
      select(ID, Body1_x, Body1_y, Body3_x, Body3_y) %>%
      mutate(
        d_x = if_else(row_number() == 1 |
                        ID - lag(ID, default = first(ID)) > 1, 0, ((
                          Body1_x - lag(Body1_x, default = first(Body1_x))
                        ) +
                          (
                            Body3_x - lag(Body3_x, default = first(Body3_x))
                          )) / 2),
        d_y = if_else(row_number() == 1 |
                        ID - lag(ID, default = first(ID)) > 1, 0, ((
                          Body1_y - lag(Body1_y, default = first(Body1_y))
                        ) +
                          (
                            Body3_y - lag(Body3_y, default = first(Body3_y))
                          )) / 2)
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
    if (plot == TRUE) {
      plot_dist_t_rep <-
        ggplot(data = d_vel, aes(x = ID, y = dist)) +
        geom_path(na.rm = T) +
        geom_point(na.rm = T) +
        theme_pubr(border = T, margin = T)
      
      plot_dist_t_remove_rep <-
        ggplot(
          data = tibble(
            d_x = ((d_sprint_removerep$Body1_x %>% diff) + (d_sprint_removerep$Body3_x %>% diff)
            ) / 2,
            d_y = ((d_sprint_removerep$Body1_y %>% diff) + (d_sprint_removerep$Body3_y %>% diff)
            ) / 2,
            ID = d_sprint_removerep$ID[1:length(d_sprint_removerep$ID) -
                                         1]
          ) %>%
            mutate(dist = sqrt(d_x^2 + d_y^2)),
          aes(x = ID, y = dist)
        ) +
        geom_path(na.rm = T) +
        geom_point(na.rm = T) +
        theme_pubr(border = T, margin = T)
      
      ggarrange(plot_dist_t_rep, plot_dist_t_remove_rep, nrow = 2)
      ggsave(
        vFN_04_plot_dist_t_remove_replicates.png,
        width = 16,
        height = 8,
        units = "cm"
      )
    }
    
    ##### 05. Replace values whose likelihood less than 0.95 with NA & interpolate NAs
    print("Replace values whose likelihood less than 0.95 with NA and interploate all NAs.")
    
    ## Replace values whose likelihood < 0.95 with NA
    d_sprint_lkd <- d_sprint_removerep %>%
      mutate(across(
        ends_with("_x"),
        ~ case_when(get(sub(
          "_x", "_likelihood", cur_column()
        )) < DLC_lkd ~ NA, TRUE ~ .),
        .names = "{.col}_mod"
      )) %>%
      mutate(across(
        ends_with("_y"),
        ~ case_when(get(sub(
          "_y", "_likelihood", cur_column()
        )) < DLC_lkd ~ NA , TRUE ~ .),
        .names = "{.col}_mod"
      )) %>%
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
        replace(., is.na(.) &
                  ID %in% ID_inedge, interpolated[is.na(.) &
                                                    ID %in% ID_inedge])
      }))
    
    ## plot for checking
    if (plot == TRUE) {
      ggarrange(
        ggplot(
          data = tibble(
            d_x = (
              d_sprint_lkd$Body1_x_mod %>% diff + d_sprint_lkd$Body3_x_mod %>% diff
            ) / 2,
            d_y = (
              d_sprint_lkd$Body1_y_mod %>% diff + d_sprint_lkd$Body3_y_mod %>% diff
            ) / 2
          ) %>%
            mutate(ID = 1:nrow(.)) %>%
            mutate(dist = sqrt(d_x^2 + d_y^2)),
          aes(x = ID, y = dist)
        ) +
          geom_path(na.rm = T) +
          geom_point(na.rm = T) +
          theme_pubr(border = T, margin = T),
        
        ggplot(
          data = tibble(
            d_x = (
              d_sprint_mod$Body1_x_mod %>% diff + d_sprint_mod$Body3_x_mod %>% diff
            ) / 2,
            d_y = (
              d_sprint_mod$Body1_x_mod %>% diff + d_sprint_mod$Body3_x_mod %>% diff
            ) / 2
          ) %>%
            mutate(ID = 1:nrow(.)) %>%
            mutate(dist = sqrt(d_x^2 + d_y^2)),
          aes(x = ID, y = dist)
        ) +
          geom_path(na.rm = T) +
          geom_point(na.rm = T) +
          theme_pubr(border = T, margin = T),
        nrow = 2
      )
      ggsave(
        vFN_05_plot_dist_t_interpolated.png,
        width = 16,
        height = 16,
        units = "cm"
      )
    }
    
    
    ##### 06. Rolling average and estimate angles
    print("Rolling average and estiamte angles.")
    
    #### Rolling average the sprint parameter
    k <- 5 # Define window size for rolling mean
    
    # Rolling mean calculation and filtering
    d_sprint_mod1 <- d_sprint_mod %>%
      # Apply rolling mean to all columns dynamically
      mutate(across(ends_with("_mod"), ~ zoo::rollmean(., k, fill = NA), .names = "{.col}_rm")) %>%
      rename_with(~ gsub("_mod_rm", "_rm", .), .cols = everything()) %>%
      select(ID, ends_with("_rm")) %>%
      # Filter rows where all rolling mean columns are > 0
      filter(if_any(ends_with("_rm"), ~ !is.na(.)))
    
    ### Calculate angle
    # Define a helper function to calculate angles
    calculate_angle_mutate <- function(data,
                                       new_col,
                                       col1,
                                       col2,
                                       col3,
                                       col4,
                                       col5,
                                       col6,
                                       flip = FALSE,
                                       cross = FALSE) {
      data %>%
        mutate(!!new_col := pmap_dbl(list(.[[col1]], .[[col2]], .[[col3]], .[[col4]], .[[col5]], .[[col6]]), function(x1, y1, x2, y2, x3, y3) {
          f_calculate_angle(x1, y1, x2, y2, x3, y3, flip = flip, cross = cross)
        }))
    }
    
    # Apply the function to different angles
    d_sprint_rm <- d_sprint_mod1 %>%
      calculate_angle_mutate(
        "Angle_LF",
        "LF_x_rm",
        "LF_y_rm",
        "Body1_x_rm",
        "Body1_y_rm",
        "Body3_x_rm",
        "Body3_y_rm",
        flip = TRUE
      ) %>%
      calculate_angle_mutate(
        "Angle_RF",
        "RF_x_rm",
        "RF_y_rm",
        "Body1_x_rm",
        "Body1_y_rm",
        "Body3_x_rm",
        "Body3_y_rm",
        flip = TRUE
      ) %>%
      calculate_angle_mutate(
        "Angle_LB",
        "LB_x_rm",
        "LB_y_rm",
        "Body3_x_rm",
        "Body3_y_rm",
        "Body1_x_rm",
        "Body1_y_rm"
      ) %>%
      calculate_angle_mutate(
        "Angle_RB",
        "RB_x_rm",
        "RB_y_rm",
        "Body3_x_rm",
        "Body3_y_rm",
        "Body1_x_rm",
        "Body1_y_rm"
      ) %>%
      calculate_angle_mutate(
        "Angle_Tail",
        "Tail_x_rm",
        "Tail_y_rm",
        "Body3_x_rm",
        "Body3_y_rm",
        "Body1_x_rm",
        "Body1_y_rm",
        flip = TRUE,
        cross = TRUE
      ) %>%
      calculate_angle_mutate(
        "Angle_Spine",
        "Body1_x_rm",
        "Body1_y_rm",
        "Body2_x_rm",
        "Body2_y_rm",
        "Body3_x_rm",
        "Body3_y_rm",
        flip = TRUE,
        cross = TRUE
      )
    
    ## Construct the velocity data
    d_vel_move <-
      d_sprint_rm %>%
      select(
        ID,
        Head_x_rm,
        Head_y_rm,
        Body1_x_rm,
        Body1_y_rm,
        Body2_x_rm,
        Body2_y_rm,
        Body3_x_rm,
        Body3_y_rm,
        Tail_x_rm,
        Tail_y_rm
      ) %>%
      mutate(
        dx = if_else(row_number() == 1 |
                       ID - lag(ID, default = first(ID)) > 1, 
                     0, 
                     ((Head_x_rm - lag(Head_x_rm)) + 
                        (Body1_x_rm - lag(Body1_x_rm)) + 
                        (Body2_x_rm - lag(Body2_x_rm)) + 
                        (Body3_x_rm - lag(Body3_x_rm)) + 
                        (Tail_x_rm - lag(Tail_x_rm))
                     ) / 5),
        dy = if_else(row_number() == 1 |
                       ID - lag(ID, default = first(ID)) > 1, 
                     0, 
                     ((Head_y_rm - lag(Head_y_rm)) + 
                        (Body1_y_rm - lag(Body1_y_rm)) + 
                        (Body2_y_rm - lag(Body2_y_rm)) + 
                        (Body3_y_rm - lag(Body3_y_rm)) + 
                        (Tail_y_rm - lag(Tail_y_rm))
                     ) / 5),
        dist = sqrt(dx^2 + dy^2),
        cum_dist = cumsum(dist),
        vel = dist * 240
      )%>%
      mutate(Angle_dxdy = pmap_dbl(list(
        dx, dy
      ), function(dx,
                  dy
      ) {
        track_x_start <- (median(d_sprint_t$edge_LT_x) + median(d_sprint_t$edge_LB_x))/2
        track_x_end <- (median(d_sprint_t$edge_RT_x) + median(d_sprint_t$edge_RB_x))/2
        track_y_start <- (median(d_sprint_t$edge_LT_y) + median(d_sprint_t$edge_LB_y))/2
        track_y_end <- (median(d_sprint_t$edge_RT_y) + median(d_sprint_t$edge_RB_y))/2
        
        V1 <- c(dx, dy)
        V2 <- c(track_x_end - track_x_start, track_y_end - track_y_start)
        
        # Compute angle in degrees
        angle <- acos(sum(V1 * V2) / (sqrt(sum(V1^2)) * sqrt(sum(V2^2)))) * 180 / pi
        
        # Compute slopes (handling division by zero safely)
        V1_slope <- ifelse(V1[1] == 0, Inf, V1[2] / V1[1])
        V2_slope <- ifelse(V2[1] == 0, Inf, V2[2] / V2[1])
        
        # Determine angle direction
        if (dx >= 0) {
          if (V1_slope >= V2_slope)
            angle
          else
            - angle
        } else {
          if (V1_slope >= V2_slope)
            - angle
          else
            angle
        }
      }))
    
    
    ### Explore the data
    # plot the trajectory after rolling mean.
    if (plot == TRUE) {
      Plot_trajectory_rm <-
        ggplot(data = d_sprint_rm, mapping = aes(
          x = (Head_x_rm + Body1_x_rm + Body2_x_rm + Body3_x_rm + Tail_x_rm) / 5,
          y = (Head_y_rm + Body1_y_rm + Body2_y_rm + Body3_y_rm + Tail_y_rm) / 5
        )) +
        geom_point(size = 1,
                   na.rm = T) +
        labs(title = "(1.6)Trajectory") +
        scale_x_continuous(name = "Mean_body_x (cm)", breaks = seq(0, 100, by = 10)) +
        scale_y_continuous(name = "Mean_body_y (cm)", breaks = seq(0, 100, by = 2)) +
        theme_pubr(border = T, margin = T) +
        coord_equal()
      ggsave(
        vFN_06_plot_trajectory_rm.png,
        width = 16,
        height = 8,
        units = "cm"
      )
    }
    
    
    # To prevent the ggplot error when all labels are NAs, I set a statement that plot is able to be drew when nrow(d_vel_move) > 0.
    if (nrow(d_vel_move) > 0) {
      # plot
      if (plot == TRUE) {
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
              xend = ID + dist * cos(Angle_dxdy / 180 * pi) * 50,
              yend = vel + dist * sin(Angle_dxdy / 180 * pi) * 50,
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
            name = "facing direction"
          ) +
          labs(title = paste0("(06)Velocity - t")) +
          theme_pubr(base_size = 12,
                     border = T,
                     margin = T)
        ggsave(
          vFN_06_plot_v_t_rm.png,
          width = 16,
          height = 8,
          units = "cm"
        )
      }
    }
    
    ###### Part II. Find the classic sprint
    print("Section 2. Find the classic sprint from the video.")
    source(
      "D:/personal onedrive/OneDrive/1. Projects/2023-2025_Sprint locomotion of autotomy grass lizards/2025.05_ Quantify the lateral undulation/[Code] Autotomy and Control/2025.05_Stage 1-2. Find the classic sprint.R"
    )
    
    ##### 2-1. Find every sprint
    print("Find every sprint.")
    
    ## remove small error near 0
    d_vel_move1 <- 
      d_vel_move %>%
      mutate(dist_mod = if_else(dist < dist_nearzero, 
                                0, 
                                dist)) %>%
      mutate(vel_mod = dist_mod * 240)
    
    ## Since diff(x) is x[i] - x[(i-1)], value would be 0 when the previous x is absent. This causes the algorithm find pseudo-starting sprint ID. I remove these pseudo-starting ID with the function down below.
    f_remove_discontinuous_ID <- 
      function(df) {
      # Ensure it's sorted by ID first
      df <- df[order(df$ID), ]
      
      # Logical vector: TRUE if ID is 1 greater than previous
      keep <- c(TRUE, diff(df$ID) == 1)
      
      # Keep only continuous rows
      df_continuous <- df[keep, ]
      
      return(df_continuous)
    }
    
    d_vel_mod <- 
      f_remove_discontinuous_ID(df = d_vel_move1) %>% slice(-1)
    
    
    ### Find all sprints
    ## criteria:
    # ID_start <- velocity_ID == 0 && velocity_(ID+1) > 0
    # ID_end <- velocity_ID == 0 && velocity_(ID-1) > 0
    ## result: a tibble including Num.sprint, ID_start, and ID_end
    ID_sprint <-
      f_find_sprint(
        data = d_vel_mod,
        ID = "ID",
        velocity = "vel_mod",
        num_classic_sprint
      )
    
    #### Check1. Determine whether nrow(ID_sprint) > 0. IF not, parameters are "no work"
    if (!is.null(ID_sprint)) {
    
    ##### 2.2 Choose long sprints #####
    print("Choose long sprints.")
    
    ## frame number should be more than 50.
    ID_sprint_long <- 
      f_choose_long_sprint(ID_sprint, num_classic_sprint)
    
    ##### 2.3 remove sprints including blank rows (ID is not continuous.)
    print("Remove sprints including blank rows")
    
    ID_without_blank_rows <-
      f_remove_sprints_including_blank_rows(ID_sprint_long, d_vel_mod)
    
    ##### 2.4 remove sprints including long interpolations
    print("Remove sprints including long interpolations")
    
    ID_without_long_interpolations <- 
      f_remove_sprints_including_long_interpolations(ID_without_blank_rows, d_vel_mod)
   
    ##### 2.5 remove sprints direction is skew
    #### criteria: the angle between the vector start to end and the vector should be less than 45
    print("Remove sprints direction is skew")
    
    ID_remove_skew_sprint <- 
      f_remove_skew_sprints(ID_without_long_interpolations, 
                            d_sprint_t, 
                            d_vel_mod, 
                            angle_skew_sprint = 60)
    
    ##### 2.6 remove sprints including too large acceleration 
    ### criteria: difference between two sequential velocity > 20
    print("Remove sprints including too large acceleration")
    
    ID_remove_sprint_acc_too_large <-
      f_remove_sprints_including_too_large_acceleration(ID_remove_skew_sprint, 
                                                        d_vel_mod, 
                                                        acc_too_large = 30)
    
    # plot the progress of select standard sprint in the video
    if(plot == TRUE) {
      plot_vt_each_sprint <-
        f_plot_all_sprints_in_the_video(ID_sprint, 
                                        d_vel_mod, 
                                        title = "2.1 plot_v-t_each sprint")
      plot_vt_long_sprints <- 
        f_plot_all_sprints_in_the_video(ID_sprint_long,
                                        d_vel_mod,
                                        title = "2.2 plot long sprints")
      plot_sprints_without_blank_rows <- 
        f_plot_all_sprints_in_the_video(ID_without_blank_rows,
                                        d_vel_mod,
                                        title = "2.3 remove sprints including blank rows")
      plot_sprints_without_long_interpolations <- 
        f_plot_all_sprints_in_the_video(ID_without_long_interpolations,
                                        d_vel_mod,
                                        title = "2.4 remove sprints including long interpolations")
      plot_sprints_without_skew_directions <- 
        f_plot_all_sprints_in_the_video(ID_remove_skew_sprint,
                                        d_vel_mod,
                                        title = "2.5 remove sprints with skew directions")
      plot_sprints_without_too_large_acceleration <- 
        f_plot_all_sprints_in_the_video(ID_remove_sprint_acc_too_large,
                                        d_vel_mod,
                                        title = "2.6 remove sprints including too large acceleration")
      plot_vt_select_standard_sprint <-
        ggarrange(plot_vt_each_sprint,
                    plot_vt_long_sprints,
                    plot_sprints_without_blank_rows,
                    plot_sprints_without_long_interpolations,
                    plot_sprints_without_skew_directions,
                  plot_sprints_without_too_large_acceleration,
                  nrow = 6,
                  common.legend = T)
        
      ggsave(
        vFN_2_plot_vt_select_standard_sprint.png,
        plot = plot_vt_select_standard_sprint,
        width = 16,
        height = 48,
        units = "cm"
      )
    }
    
    #### Check2. Determine whether there is at least one classic sprint ####
    is_at_least_num_classic_sprint <- 
      nrow(ID_remove_sprint_acc_too_large) > 0
    
    if (is_at_least_num_classic_sprint) {
      ##### 2.7 Select the sprint with the max speed
      print("select the sprint with highest speed")
      
      ID_sprint_select <- f_select_sprint_max_speed(ID_remove_sprint_acc_too_large, d_vel_mod)
      
      d_vel_select <-
        d_vel_mod %>%
        filter(ID %in% (ID_sprint_select$ID_start):(ID_sprint_select$ID_end))
      
      d_sprint_select <-
        d_sprint_rm %>%
        filter(ID %in%
                 (ID_sprint_select$ID_start):(ID_sprint_select$ID_end + 1)) %>%
        mutate(
          Mean_x_all_body_parts = 
            (Head_x_rm + Body1_x_rm + Body2_x_rm + Body3_x_rm + Tail_x_rm) / 5,
          Mean_y_all_body_parts = 
            (Head_y_rm + Body1_y_rm + Body2_y_rm + Body3_y_rm + Tail_y_rm) / 5,
          sprint_group_id = 
            ceiling(row_number() / 20)
        ) %>%
        group_by(sprint_group_id) %>%
        mutate(
          sprint_group_mean_slope =
            mean(diff(Mean_y_all_body_parts) / diff(Mean_x_all_body_parts)),
          sprint_group_mean_x =
            mean(Mean_x_all_body_parts),
          sprint_group_mean_y =
            mean(Mean_y_all_body_parts),
          sprint_group_intercept =
            sprint_group_mean_y -
            sprint_group_mean_slope * sprint_group_mean_x
        ) %>%
        ungroup() %>%
        mutate(sprint_group_sprint_y_predict =
                 sprint_group_intercept + sprint_group_mean_slope * Mean_x_all_body_parts) %>%
        drop_na() %>%
        mutate(Angle_Body1Head = pmap_dbl(list(
          Head_x_rm, Head_y_rm, Body1_x_rm, Body1_y_rm, sprint_group_mean_slope
        ), function(Head_x_rm,
                    Head_y_rm,
                    Body1_x_rm,
                    Body1_y_rm, 
                    sprint_group_mean_slope
        ) {
          dx = Head_x_rm - Body1_x_rm
          dy = Head_y_rm - Body1_y_rm
          V1 <- c(dx, dy)
          V2 <- c(1, sprint_group_mean_slope)
          
          # Compute angle in degrees
          angle <- acos(sum(V1 * V2) / (sqrt(sum(V1^2)) * sqrt(sum(V2^2)))) * 180 / pi
          
          # Compute slopes (handling division by zero safely)
          V1_slope <- ifelse(V1[1] == 0, Inf, V1[2] / V1[1])
          V2_slope <- ifelse(V2[1] == 0, Inf, V2[2] / V2[1])
          
          # Determine angle direction
          if(is.na(sprint_group_mean_slope)){
            angle = NA_real_
          } else if (dx >= 0) {
            if (V1_slope >= V2_slope)
              angle
            else -angle
          } else {
            if (V1_slope >= V2_slope)
              - angle
            else
              angle
          }
        })) %>%
        mutate(Angle_Body3Tail = pmap_dbl(list(
          Body3_x_rm, Body3_y_rm, Tail_x_rm, Tail_y_rm, sprint_group_mean_slope
        ), function(Body3_x_rm,
                    Body3_y_rm,
                    Tail_x_rm,
                    Tail_y_rm,
                    sprint_group_mean_slope
        ) {
          dx = Body3_x_rm - Tail_x_rm
          dy = Body3_y_rm - Tail_y_rm
          V1 <- c(dx, dy)
          V2 <- c(1, sprint_group_mean_slope)
          
          # Compute angle in degrees
          angle <- acos(sum(V1 * V2) / (sqrt(sum(V1^2)) * sqrt(sum(V2^2)))) * 180 / pi
          
          # Compute slopes (handling division by zero safely)
          V1_slope <- ifelse(V1[1] == 0, Inf, V1[2] / V1[1])
          V2_slope <- ifelse(V2[1] == 0, Inf, V2[2] / V2[1])
          
          # Determine angle direction
          if(is.na(sprint_group_mean_slope)){
            angle = NA_real_
          } else if (dx >= 0) {
            if (V1_slope >= V2_slope)
              angle
            else -angle
          } else {
            if (V1_slope >= V2_slope)
              - angle
            else
              angle
          }
        })) %>%
        mutate(Angle_Body3Body1 = pmap_dbl(list(
          Body1_x_rm, Body1_y_rm, Body3_x_rm, Body3_y_rm, sprint_group_mean_slope
        ), function(Body1_x_rm,
                    Body1_y_rm,
                    Body3_x_rm,
                    Body3_y_rm,
                    sprint_group_mean_slope
        ) {
          dx = Body1_x_rm - Body3_x_rm
          dy = Body1_y_rm - Body3_y_rm
          V1 <- c(dx, dy)
          V2 <- c(1, sprint_group_mean_slope)
          
          # Compute angle in degrees
          angle <- acos(sum(V1 * V2) / (sqrt(sum(V1^2)) * sqrt(sum(V2^2)))) * 180 / pi
          
          # Compute slopes (handling division by zero safely)
          V1_slope <- ifelse(V1[1] == 0, Inf, V1[2] / V1[1])
          V2_slope <- ifelse(V2[1] == 0, Inf, V2[2] / V2[1])
          
          # Determine angle direction
          if(is.na(sprint_group_mean_slope)){
            angle = NA_real_
          } else if (dx >= 0) {
            if (V1_slope >= V2_slope)
              angle
            else
              - angle
          } else {
            if (V1_slope >= V2_slope)
              - angle
            else
              angle
          }
        }))
      
      ## plot trajectory of the selected sprint
      if (plot == TRUE) {
        plot_trajectory_select_arranged <-
          f_plot_trajectory_select(d_sprint_select,
                                   ID_sprint_select, 
                                   vFN)
        
        ggsave(
          filename = vFN_3.1_trajectory_select.png,
          plot = plot_trajectory_select_arranged,
          width = 16,
          height = 16,
          units = "cm"
        )
      }
      
      ## plot vt of the selected sprint
      if (plot == TRUE) {
        plot_v_t_select_raw <-
          f_plot_vt_select(d_vel_select, 
                           d_sprint_select,
                           ID_sprint_select, 
                           vFN)
        
        ggsave(
          plot = plot_v_t_select_raw,
          filename = vFN_10_v_t_select_raw.png,
          width = 16,
          height = 24,
          units = "cm"
        )
      }
      
      #### Part III. Estimate locomotion parameters ####
      source(
        "D:/personal onedrive/OneDrive/1. Projects/2023-2025_Sprint locomotion of autotomy grass lizards/2025.05_ Quantify the lateral undulation/[Code] Autotomy and Control/2025.03_Stage 1-3. Estimate locomotion parameters.R"
      )
      print("Stage 3. Estiamte locomotion parameters.")
      
      #### 3.1 Estimate speed parameter ####
      print("Estimate speed parameters.")
      
      ## frame-wise speed
      speed <- d_vel_select$vel_mod # return
      
      ## frame-wise acceleration
      acceleration <- diff(d_vel_select$vel_mod) * 240 # return
      
      ## sprint distance
      sprint_distance <-
        f_estimate_sprint_distance(
          d_sprint_select,
          x1 = Mean_x_all_body_parts,
          y1 = Mean_y_all_body_parts,
          x2 = Mean_x_all_body_parts,
          y2 = Mean_y_all_body_parts
        ) # return
      
      ## path length
      path_length <- 
        d_vel_select$dist_mod %>% sum # return
      path_length_hd <- 
        sqrt(diff(d_vel_select$Head_x_rm)^2 + diff(d_vel_select$Head_y_rm)^2) %>% sum # return
      path_length_tl <- 
        sqrt(diff(d_vel_select$Tail_x_rm)^2 + diff(d_vel_select$Tail_y_rm)^2) %>% sum # return
      
      #### 3.2 Lateral undulation of spine ####
      ### output: frame-wise amplitude of bodyparts
      print("Estimate amplitudes of body parts")
      
      # head
      amplitude_head <-
        f_estimate_displacement(
          d_sprint_select,
          bodypart_x = Head_x_rm,
          bodypart_y = Head_y_rm,
          slope = sprint_group_mean_slope,
          intercept = sprint_group_intercept
        ) # return
      
      # body1
      amplitude_shoulder <-
        f_estimate_displacement(
          d_sprint_select,
          bodypart_x = Body1_x_rm,
          bodypart_y = Body1_y_rm,
          slope = sprint_group_mean_slope,
          intercept = sprint_group_intercept
        ) # return
      
      # body2
      amplitude_body <-
        f_estimate_displacement(
          d_sprint_select,
          bodypart_x = Body2_x_rm,
          bodypart_y = Body2_y_rm,
          slope = sprint_group_mean_slope,
          intercept = sprint_group_intercept
        ) # return
      
      # body3
      amplitude_hip <-
        f_estimate_displacement(
          d_sprint_select,
          bodypart_x = Body3_x_rm,
          bodypart_y = Body3_y_rm,
          slope = sprint_group_mean_slope,
          intercept = sprint_group_intercept
        ) # return
      
      # tail
      amplitude_tail <-
        f_estimate_displacement(
          d_sprint_select,
          bodypart_x = Tail_x_rm,
          bodypart_y = Tail_y_rm,
          slope = sprint_group_mean_slope,
          intercept = sprint_group_intercept
        ) # return
      
      # plot
      if (plot == TRUE) {
        plot_amplitude <-
          f_plot_amplitude(
              d_sprint_select,
              amplitude_head,
              amplitude_shoulder,
              amplitude_body,
              amplitude_hip,
              amplitude_tail,
              ID_maintenance_phase,
              vFN
            )
        ggsave(
          vFN_plot_amplitude.png,
          width = 16,
          height = 40,
          units = "cm"
        )
      }
      
      #### Estimate lateral undulation angle ####
     
      ### Estimate spine angle
      print("Estiamte spine angle")
      spine_angle <- d_sprint_select$Angle_Spine #return
      spine_angle_acceleration <- d_sprint_select_acceleration$Angle_Spine #return
      spine_angle_maintenance <- d_sprint_select_maintenance$Angle_Spine #return
      
      ### Estimate tail angle
      print("Estimate tail angle.")
      tail_angle <- d_sprint_select$Angle_Tail #return
      tail_angle_acceleration <- d_sprint_select_acceleration$Angle_Tail #return
      tail_angle_maintenance <- d_sprint_select_maintenance$Angle_Tail #return
      
      ### Estimate head sprint angle
      print("Estimate head sprint angle.")
      head_sprint_angle <- d_sprint_select$Angle_Body1Head #return
      head_sprint_angle_acceleration <- d_sprint_select_acceleration$Angle_Body1Head #return
      head_sprint_angle_maintenance <- d_sprint_select_maintenance$Angle_Body1Head #return
      
      ### Estimate body sprint angle
      print("Estimate body sprint angle.")
      body_sprint_angle <- d_sprint_select$Angle_Body3Body1 #return
      body_sprint_angle_acceleration <- d_sprint_select_acceleration$Angle_Body3Body1 #return
      body_sprint_angle_maintenance <- d_sprint_select_maintenance$Angle_Body3Body1 #return
      
      ### Estimate tail sprint angle
      print("Estimate tail sprint angle.")
      tail_sprint_angle <- d_sprint_select$Angle_Body3Tail #return
      tail_sprint_angle_acceleration <- d_sprint_select_acceleration$Angle_Body3Tail #return
      tail_sprint_angle_maintenance <- d_sprint_select_maintenance$Angle_Body3Tail #return
      
      ## plot
      if (plot == TRUE) {
        plot_lateral_undulation_angle <-
            f_plot_lateral_undulation_angle(d_sprint_select,
                                            spine_angle, 
                                            tail_angle,
                                            head_sprint_angle,
                                            body_sprint_angle,
                                            tail_sprint_angle,
                                            ID_maintenance_phase,
                                            vFN)
        ggsave(
          plot = plot_lateral_undulation_angle,
          filename = vFN_plot_lateral_undulation_angle.png,
          width = 16,
          height = 40,
          units = "cm"
        )
      }
      
      ##### 3. Estimate step rate, step length, and step angle #####
      print("Estimate step rate, step length, and range of motion of limbs.")
      #### Constuct velocity data for the four limbs.
      print("Constuct velocity data for the four limbs.")
      
      ## Criteria to define the phase of step
      # In the stance phase, the velocity of limb < the velocity of body
      # In the swing phase, the velocity of limb > the velocity of body
      
      ### velocity data of the left forelimb
      d_vel_LF <-
        f_create_velocity_data_of_limbs(
          d_sprint_select = d_sprint_select,
          limb = "LF",
          k_s = 11,
          dist_nearzero
        )
      d_vel_LF_acceleration <-
        d_vel_LF %>% 
        filter(ID %in% .$ID[1:20])
      d_vel_LF_maintenance <- 
        d_vel_LF %>%
        filter(ID %in% ID_maintenance_phase$ID_start:ID_maintenance_phase$ID_end)
      
      ### velocity data of the right forelimb
      d_vel_RF <-
        f_create_velocity_data_of_limbs(
          d_sprint_select = d_sprint_select,
          limb = "RF",
          k_s = 11,
          dist_nearzero
        )
      d_vel_RF_acceleration <-
        d_vel_RF %>% 
        filter(ID %in% .$ID[1:20])
      d_vel_RF_maintenance <- 
        d_vel_RF %>%
        filter(ID %in% ID_maintenance_phase$ID_start:ID_maintenance_phase$ID_end)
      
      ### velocity data of the left hindlimb
      d_vel_LB <-
        f_create_velocity_data_of_limbs(
          d_sprint_select = d_sprint_select,
          limb = "LB",
          k_s = 11,
          dist_nearzero
        )
      d_vel_LB_acceleration <-
        d_vel_LB %>% 
        filter(ID %in% .$ID[1:20])
      d_vel_LB_maintenance <- 
        d_vel_LB %>%
        filter(ID %in% ID_maintenance_phase$ID_start:ID_maintenance_phase$ID_end)
      
      ### velocity data of the right hindlimb
      d_vel_RB <-
        f_create_velocity_data_of_limbs(
          d_sprint_select = d_sprint_select,
          limb = "RB",
          k_s = 11,
          dist_nearzero
        )
      d_vel_RB_acceleration <-
        d_vel_RB %>% 
        filter(ID %in% .$ID[1:20])
      d_vel_RB_maintenance <- 
        d_vel_RB %>%
        filter(ID %in% ID_maintenance_phase$ID_start:ID_maintenance_phase$ID_end)
      
      ## plot the velocity of the four limbs
      # f_plot_velocity_limb (in stage1-3) is the function to plot the velocity of limbs
      plot_velocity_limb_arrange <-
        ggarrange(
          f_plot_velocity_limb(d_vel = d_vel_LF, 
                               ID_maintenance_phase, limb = "LF"),
          f_plot_velocity_limb(d_vel = d_vel_RF, 
                               ID_maintenance_phase, limb = "RF"),
          f_plot_velocity_limb(d_vel = d_vel_LB, 
                               ID_maintenance_phase, limb = "LB"),
          f_plot_velocity_limb(d_vel = d_vel_RB, 
                               ID_maintenance_phase, limb = "RB"),
          nrow = 4,
          common.legend = TRUE
        )
      plot_velocity_limb <-
        annotate_figure(plot_velocity_limb_arrange,
                        top = text_grob(
                          paste0("velocity of limbs", " (", sub("DLC.*", "", vFN), ")"),
                          color = "black",
                          face = "bold",
                          size = 12
                        ))
      ggsave(
        plot = plot_velocity_limb,
        filename = vFN_plot_velocity_limb.png,
        width = 16,
        height = 32,
        unit = "cm"
      )
      
      
      # plot diff_angle of limbs
      if (plot == TRUE) {
        plot_diff_angle_arrange <-
          ggarrange(
            f_plot_diff_angle_limb(d_vel = d_vel_LF, ID_maintenance_phase, limb = "LF"),
            f_plot_diff_angle_limb(d_vel = d_vel_RF, ID_maintenance_phase, limb = "RF"),
            f_plot_diff_angle_limb(d_vel = d_vel_LB, ID_maintenance_phase, limb = "LB"),
            f_plot_diff_angle_limb(d_vel = d_vel_RB, ID_maintenance_phase, limb = "RB"),
            nrow = 4,
            common.legend = TRUE
          )
        plot_diff_angle_limb <-
          annotate_figure(plot_diff_angle_arrange,
                          top = text_grob(
                            paste0("difference of angle of limbs", " (", sub("DLC.*", "", vFN), ")"),
                            color = "black",
                            face = "bold",
                            size = 12
                          ))
        ggsave(
          plot = plot_diff_angle_limb,
          filename = vFN_plot_diff_angle_limb.png,
          width = 16,
          height = 32,
          unit = "cm"
        )
      }
      
      # plot angle of limbs
      if (plot == TRUE) {
        plot_angle_limb_arrange <-
          ggarrange(
            f_plot_angle_limb(
              d_sprint_select = d_sprint_select,
              d_vel_limb = d_vel_LF,
              ID_maintenance_phase, 
              limb = "LF"
            ),
            f_plot_angle_limb(
              d_sprint_select = d_sprint_select,
              d_vel_limb = d_vel_RF,
              ID_maintenance_phase, 
              limb = "RF"
            ),
            f_plot_angle_limb(
              d_sprint_select = d_sprint_select,
              d_vel_limb = d_vel_LB,
              ID_maintenance_phase, 
              limb = "LB"
            ),
            f_plot_angle_limb(
              d_sprint_select = d_sprint_select,
              d_vel_limb = d_vel_RB,
              ID_maintenance_phase, 
              limb = "RB"
            ),
            nrow = 4,
            common.legend = TRUE
          )
        plot_angle_limb <-
          annotate_figure(plot_angle_limb_arrange,
                          top = text_grob(
                            paste0("angle of limbs", " (", sub("DLC.*", "", vFN), ")"),
                            color = "black",
                            face = "bold",
                            size = 12
                          ))
        ggsave(
          plot = plot_angle_limb,
          filename = vFN_plot_angle_limb.png,
          width = 16,
          height = 32,
          unit = "cm"
        )
      }
      
      #### 20250528 Find the ID of stance and swing
      ### Estimate step rate, step length, and limb angle of the left forelimb (LF)
      print(" Estimate step rate, step length, and limb angle of the left forelimb (LF).")
      
      ## find the stance ID
      ID_stance_LF <- f_find_ID_stance(d_vel_limb = d_vel_LF)
      
      ## estimate step parameters
      if (all(is.na(ID_stance_LF[["ID_stance_start"]])) &
          all(is.na(ID_stance_LF[["ID_stance_end"]]))) {
        step_rate_LF <- "no work"
        step_length_LF <- "no work"
        limb_angle_LF <- "no work"
      } else {
        ## estimate step_rate_LF
        step_rate_LF <-
          nrow(ID_stance_LF) / ((
            ID_stance_LF$ID_stance_end[nrow(ID_stance_LF)] - ID_stance_LF$ID_stance_start[1]
          ) / 240) # return
        
        ## step_length_LF
        step_length_LF <-
          f_estimate_step_length(d_sprint_select, ID_stance = ID_stance_LF, limb = "LF") # return
        
        ## limb_angle_LF
        limb_angle_LF <-
          f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_LF, limb = "LF") # return
      }
      
      ### Estimate step rate, step length, and limb angle of the right forelimb (RF)
      print(" Estimate step rate, step length, and limb angle of the right forelimb (RF).")
      
      ## find the stance ID
      ID_stance_RF <- f_find_ID_stance(d_vel_limb = d_vel_RF)
      
      ## estimate step parameters
      if (all(is.na(ID_stance_RF[["ID_stance_start"]])) &
          all(is.na(ID_stance_RF[["ID_stance_end"]]))) {
        step_rate_RF <- "no work"
        step_length_RF <- "no work"
        limb_angle_RF <- "no work"
      } else {
        ## estimate step_rate_RF
        step_rate_RF <-
          nrow(ID_stance_RF) / ((
            ID_stance_RF$ID_stance_end[nrow(ID_stance_RF)] - ID_stance_RF$ID_stance_start[1]
          ) / 240) # return
        
        ## step_length_RF
        step_length_RF <-
          f_estimate_step_length(d_sprint_select, ID_stance = ID_stance_RF, limb = "RF") # return
        
        ## limb_angle_RF
        limb_angle_RF <-
          f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_RF, limb = "RF") # return
      }
      
      ### Estimate step rate, step length, and limb angle of the left hindlimb (LB)
      print(" Estimate step rate, step length, and limb angle of the left hindlimb (LB).")
      
      ## find the stance ID
      ID_stance_LB <- f_find_ID_stance(d_vel_limb = d_vel_LB)
      
      ## estimate step parameters
      if (all(is.na(ID_stance_LB[["ID_stance_start"]])) &
          all(is.na(ID_stance_LB[["ID_stance_end"]]))) {
        step_rate_LB <- "no work"
        step_length_LB <- "no work"
        limb_angle_LB <- "no work"
      } else {
        ## estimate step_rate_LB
        step_rate_LB <-
          nrow(ID_stance_LB) / ((
            ID_stance_LB$ID_stance_end[nrow(ID_stance_LB)] - ID_stance_LB$ID_stance_start[1]
          ) / 240) # return
        
        ## step_length_LB
        step_length_LB <-
          f_estimate_step_length(d_sprint_select,
                                 ID_stance = ID_stance_LB,
                                 limb = "LB") # return
        
        ## limb_angle_LB
        limb_angle_LB <-
          f_estimate_limb_angle(d_sprint_select,
                                ID_stance = ID_stance_LB,
                                limb = "LB") # return
      }
      
      
      ### Estimate step rate, step length, and limb angle of the right hindlimb (RB)
      print("Estimate step rate, step length, and limb angle of the right hindlimb (RB).")
      
      ## find the stance ID
      ID_stance_RB <- f_find_ID_stance(d_vel_limb = d_vel_RB)
      
      ## estimate step parameters
      if (all(is.na(ID_stance_RB[["ID_stance_start"]])) &
          all(is.na(ID_stance_RB[["ID_stance_end"]]))) {
        step_rate_RB <- "no work"
        step_length_RB <- "no work"
        limb_angle_RB <- "no work"
      } else {
        ## estimate step_rate_RB
        step_rate_RB <-
          nrow(ID_stance_RB) / ((
            ID_stance_RB$ID_stance_end[nrow(ID_stance_RB)] - ID_stance_RB$ID_stance_start[1]
          ) / 240) # return
        
        ## step_length_RB
        step_length_RB <-
          f_estimate_step_length(
            d_sprint_select, 
            ID_stance = ID_stance_RB, 
            limb = "RB") # return
        
        ## limb_angle_RB
        limb_angle_RB <-
          f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_RB, limb = "RB") # return
      }
      
      ### Estimate step rate, step length, and limb angle of the left forelimb (LF)
      print(" Estimate step rate, step length, and limb angle of the left forelimb (LF) in the acceleration phase.")
      
      ## find the stance ID
      ID_stance_LF <- f_find_ID_stance(d_vel_limb = d_vel_LF_acceleration)
      
      ## estimate step parameters
      if (all(is.na(ID_stance_LF[["ID_stance_start"]])) &
          all(is.na(ID_stance_LF[["ID_stance_end"]]))) {
        step_rate_LF_acceleration <- "no work"
        step_length_LF_acceleration <- "no work"
        limb_angle_LF_acceleration <- "no work"
      } else {
        ## estimate step_rate_LF_acceleration
        step_rate_LF_acceleration <-
          nrow(ID_stance_LF) / ((
            ID_stance_LF$ID_stance_end[nrow(ID_stance_LF)] - ID_stance_LF$ID_stance_start[1]
          ) / 240) # return
        
        ## step_length_LF
        step_length_LF_acceleration <-
          f_estimate_step_length(d_sprint_select, ID_stance = ID_stance_LF, limb = "LF") # return
        
        ## limb_angle_LF
        limb_angle_LF_acceleration <-
          f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_LF, limb = "LF") # return
      }
      
      ### Estimate step rate, step length, and limb angle of the right forelimb (RF)
      print(" Estimate step rate, step length, and limb angle of the right forelimb (RF) in the acceleration phase.")
      
      ## find the stance ID
      ID_stance_RF <- f_find_ID_stance(d_vel_limb = d_vel_RF_acceleration)
      
      ## estimate step parameters
      if (all(is.na(ID_stance_RF[["ID_stance_start"]])) &
          all(is.na(ID_stance_RF[["ID_stance_end"]]))) {
        step_rate_RF_acceleration <- "no work"
        step_length_RF_acceleration <- "no work"
        limb_angle_RF_acceleration <- "no work"
      } else {
        ## estimate step_rate_RF
        step_rate_RF_acceleration <-
          nrow(ID_stance_RF) / ((
            ID_stance_RF$ID_stance_end[nrow(ID_stance_RF)] - ID_stance_RF$ID_stance_start[1]
          ) / 240) # return
        
        ## step_length_RF
        step_length_RF_acceleration <-
          f_estimate_step_length(d_sprint_select, ID_stance = ID_stance_RF, limb = "RF") # return
        
        ## limb_angle_RF
        limb_angle_RF_acceleration <-
          f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_RF, limb = "RF") # return
        }
        
        ### Estimate step rate, step length, and limb angle of the left hindlimb (LB)
        print(" Estimate step rate, step length, and limb angle of the left hindlimb (LB) in the acceleration phase.")
        
        ## find the stance ID
        ID_stance_LB <- f_find_ID_stance(d_vel_limb = d_vel_LB_acceleration)
        
        ## estimate step parameters
        if (all(is.na(ID_stance_LB[["ID_stance_start"]])) &
            all(is.na(ID_stance_LB[["ID_stance_end"]]))) {
          step_rate_LB_acceleration <- "no work"
          step_length_LB_acceleration <- "no work"
          limb_angle_LB_acceleration <- "no work"
        } else {
          ## estimate step_rate_LB
          step_rate_LB_acceleration <-
            nrow(ID_stance_LB) / ((
              ID_stance_LB$ID_stance_end[nrow(ID_stance_LB)] - ID_stance_LB$ID_stance_start[1]
            ) / 240) # return
          
          ## step_length_LB
          step_length_LB_acceleration <-
            f_estimate_step_length(d_sprint_select,
                                   ID_stance = ID_stance_LB,
                                   limb = "LB") # return
          
          ## limb_angle_LB
          limb_angle_LB_acceleration <-
            f_estimate_limb_angle(d_sprint_select,
                                  ID_stance = ID_stance_LB,
                                  limb = "LB") # return
        }
        
        
        ### Estimate step rate, step length, and limb angle of the right hindlimb (RB)
        print("Estimate step rate, step length, and limb angle of the right hindlimb (RB) in the acceleration phase.")
        
        ## find the stance ID
        ID_stance_RB <- f_find_ID_stance(d_vel_limb = d_vel_RB_acceleration)
        
        ## estimate step parameters
        if (all(is.na(ID_stance_RB[["ID_stance_start"]])) &
            all(is.na(ID_stance_RB[["ID_stance_end"]]))) {
          step_rate_RB_acceleration <- "no work"
          step_length_RB_acceleration <- "no work"
          limb_angle_RB_acceleration <- "no work"
        } else {
          ## estimate step_rate_RB
          step_rate_RB_acceleration <-
            nrow(ID_stance_RB) / ((
              ID_stance_RB$ID_stance_end[nrow(ID_stance_RB)] - ID_stance_RB$ID_stance_start[1]
            ) / 240) # return
          
          ## step_length_RB
          step_length_RB_acceleration <-
            f_estimate_step_length(
              d_sprint_select, 
              ID_stance = ID_stance_RB, 
              limb = "RB") # return
          
          ## limb_angle_RB
          limb_angle_RB_acceleration <-
            f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_RB, limb = "RB") # return
        }
        
        ### Estimate step rate, step length, and limb angle of the left forelimb (LF)
        print(" Estimate step rate, step length, and limb angle of the left forelimb (LF) in the maintenance phase.")
        
        ## find the stance ID
        ID_stance_LF <- f_find_ID_stance(d_vel_limb = d_vel_LF_maintenance)
        
        ## estimate step parameters
        if (all(is.na(ID_stance_LF[["ID_stance_start"]])) &
            all(is.na(ID_stance_LF[["ID_stance_end"]]))) {
          step_rate_LF_maintenance <- "no work"
          step_length_LF_maintenance <- "no work"
          limb_angle_LF_maintenance <- "no work"
        } else {
          ## estimate step_rate_LF_acceleration
          step_rate_LF_maintenance <-
            nrow(ID_stance_LF) / ((
              ID_stance_LF$ID_stance_end[nrow(ID_stance_LF)] - ID_stance_LF$ID_stance_start[1]
            ) / 240) # return
          
          ## step_length_LF
          step_length_LF_maintenance <-
            f_estimate_step_length(d_sprint_select, ID_stance = ID_stance_LF, limb = "LF") # return
          
          ## limb_angle_LF
          limb_angle_LF_maintenance <-
            f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_LF, limb = "LF") # return
        }
        
        ### Estimate step rate, step length, and limb angle of the right forelimb (RF)
        print(" Estimate step rate, step length, and limb angle of the right forelimb (RF) in the acceleration phase.")
        
        ## find the stance ID
        ID_stance_RF <- f_find_ID_stance(d_vel_limb = d_vel_RF_maintenance)
        
        ## estimate step parameters
        if (all(is.na(ID_stance_RF[["ID_stance_start"]])) &
            all(is.na(ID_stance_RF[["ID_stance_end"]]))) {
          step_rate_RF_maintenance <- "no work"
          step_length_RF_maintenance <- "no work"
          limb_angle_RF_maintenance <- "no work"
        } else {
          ## estimate step_rate_RF
          step_rate_RF_maintenance <-
            nrow(ID_stance_RF) / ((
              ID_stance_RF$ID_stance_end[nrow(ID_stance_RF)] - ID_stance_RF$ID_stance_start[1]
            ) / 240) # return
          
          ## step_length_RF
          step_length_RF_maintenance <-
            f_estimate_step_length(d_sprint_select, 
                                   ID_stance = ID_stance_RF, 
                                   limb = "RF") # return
          
          ## limb_angle_RF
          limb_angle_RF_maintenance <-
            f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_RF, limb = "RF") # return
        }
        
        ### Estimate step rate, step length, and limb angle of the left hindlimb (LB)
        print(" Estimate step rate, step length, and limb angle of the left hindlimb (LB) in the acceleration phase.")
        
        ## find the stance ID
        ID_stance_LB <- f_find_ID_stance(d_vel_limb = d_vel_LB_maintenance)
        
        ## estimate step parameters
        if (all(is.na(ID_stance_LB[["ID_stance_start"]])) &
            all(is.na(ID_stance_LB[["ID_stance_end"]]))) {
          step_rate_LB_maintenance <- "no work"
          step_length_LB_maintenance <- "no work"
          limb_angle_LB_maintenance <- "no work"
        } else {
          ## estimate step_rate_LB
          step_rate_LB_maintenance <-
            nrow(ID_stance_LB) / ((
              ID_stance_LB$ID_stance_end[nrow(ID_stance_LB)] - ID_stance_LB$ID_stance_start[1]
            ) / 240) # return
          
          ## step_length_LB
          step_length_LB_maintenance <-
            f_estimate_step_length(d_sprint_select,
                                   ID_stance = ID_stance_LB,
                                   limb = "LB") # return
          
          ## limb_angle_LB
          limb_angle_LB_maintenance <-
            f_estimate_limb_angle(d_sprint_select,
                                  ID_stance = ID_stance_LB,
                                  limb = "LB") # return
        }
        
        
        ### Estimate step rate, step length, and limb angle of the right hindlimb (RB)
        print("Estimate step rate, step length, and limb angle of the right hindlimb (RB) in the acceleration phase.")
        
        ## find the stance ID
        ID_stance_RB <- f_find_ID_stance(d_vel_limb = d_vel_RB_maintenance)
        
        ## estimate step parameters
        if (all(is.na(ID_stance_RB[["ID_stance_start"]])) &
            all(is.na(ID_stance_RB[["ID_stance_end"]]))) {
          step_rate_RB_maintenance <- "no work"
          step_length_RB_maintenance <- "no work"
          limb_angle_RB_maintenance <- "no work"
        } else {
          ## estimate step_rate_RB
          step_rate_RB_maintenance <-
            nrow(ID_stance_RB) / ((
              ID_stance_RB$ID_stance_end[nrow(ID_stance_RB)] - ID_stance_RB$ID_stance_start[1]
            ) / 240) # return
          
          ## step_length_RB
          step_length_RB_maintenance <-
            f_estimate_step_length(d_sprint_select, ID_stance = ID_stance_RB, limb = "RB") # return
          
          ## limb_angle_RB
          limb_angle_RB_maintenance <-
            f_estimate_limb_angle(d_sprint_select, ID_stance = ID_stance_RB, limb = "RB") # return
        }
        
        
        ## Estimate the ratio that both limbs are stance.
        print("Estimate the ratio that both limbs are stance.")
        
        f_estimate_both_stance_ratio <- 
          function(d_vel_L, d_vel_R){
            
            i_both_stance <- integer()
          
            for (i in seq_along(d_vel_L$ID)) {
              if(d_vel_L[["phase"]][i] == "stance" &&
                 d_vel_R[["phase"]][i] == "stance"){
                i_both_stance <- c(i_both_stance, i)
              }
            }
            both_stance_ratio <- 
              length(i_both_stance)/ length(d_vel_L$ID)
            return(both_stance_ratio)
          }
        
        both_stance_ratio_front <- 
          f_estimate_both_stance_ratio(d_vel_LF, d_vel_RF) #return
        both_stance_ratio_hind <- 
          f_estimate_both_stance_ratio(d_vel_LB, d_vel_RB) #return
        
        ## export the first stance ID of hindlimbs
        ## find the stance ID
        ID_stance_LB <- f_find_ID_stance(d_vel_limb = d_vel_LB)
        ID_stance_RB <- f_find_ID_stance(d_vel_limb = d_vel_RB)
        
        if (all(is.na(ID_stance_RB[["ID_stance_start"]])) &
            all(is.na(ID_stance_RB[["ID_stance_end"]]))) {
          first_stance_start_LB <- "no work"
          first_stance_end_LB <- "no work"
          first_stance_start_RB <- "no work"
          first_stance_end_RB <- "no work"
        } else {
          first_stance_start_LB <- ID_stance_LB$ID_stance_start[1]
          first_stance_end_LB <- ID_stance_LB$ID_stance_end[1]
          first_stance_start_RB <- ID_stance_RB$ID_stance_start[1]
          first_stance_end_RB <- ID_stance_RB$ID_stance_end[1]
        }
        
        
        
      # end estimation
    } else {
      filenames <- filenames
      
      # speed parameters
      speed <- "no work"
      acceleration <- "no work"
      sprint_distance <- "no work"
      path_length <- "no work"
      
      ## lateral undulation parameters
      # all
      amplitude_head <- "no work"
      amplitude_shoulder <- "no work"
      amplitude_body <- "no work"
      amplitude_hip <- "no work"
      amplitude_tail <- "no work"
      spine_angle <- "no work"
      tail_angle <- "no work"
      head_sprint_angle <- "no work"
      body_sprint_angle <- "no work"
      tail_sprint_angle <- "no work"
      # # accerleration phase
      # amplitude_head_acceleration <- "no work"
      # amplitude_shoulder_acceleration <- "no work"
      # amplitude_body_acceleration <- "no work"
      # amplitude_hip_acceleration <- "no work"
      # amplitude_tail_acceleration <- "no work"
      # spine_angle_acceleration <- "no work"
      # tail_angle_acceleration <- "no work"
      # head_sprint_angle_acceleration <- "no work"
      # body_sprint_angle_acceleration <- "no work"
      # tail_sprint_angle_acceleration <- "no work"
      # # maintenance phase
      # amplitude_head_maintenance <- "no work"
      # amplitude_shoulder_maintenance <- "no work"
      # amplitude_body_maintenance <- "no work"
      # amplitude_hip_maintenance <- "no work"
      # amplitude_tail_maintenance <- "no work"
      # spine_angle_maintenance <- "no work"
      # tail_angle_maintenance <- "no work"
      # head_sprint_angle_maintenance <- "no work"
      # body_sprint_angle_maintenance <- "no work"
      # tail_sprint_angle_maintenance <- "no work"
      
      ## step parameters
      # all
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
      # # acceleration phase
      # step_rate_LF_acceleration <- "no work"
      # step_length_LF_acceleration <- "no work"
      # limb_angle_LF_acceleration <- "no work"
      # step_rate_RF_acceleration <- "no work"
      # step_length_RF_acceleration <- "no work"
      # limb_angle_RF_acceleration <- "no work"
      # step_rate_LB_acceleration <- "no work"
      # step_length_LB_acceleration <- "no work"
      # limb_angle_LB_acceleration <- "no work"
      # step_rate_RB_acceleration <- "no work"
      # step_length_RB_acceleration <- "no work"
      # limb_angle_RB_acceleration <- "no work"
      # # maintenance phase
      # step_rate_LF_maintenance <- "no work"
      # step_length_LF_maintenance <- "no work"
      # limb_angle_LF_maintenance <- "no work"
      # step_rate_RF_maintenance <- "no work"
      # step_length_RF_maintenance <- "no work"
      # limb_angle_RF_maintenance <- "no work"
      # step_rate_LB_maintenance <- "no work"
      # step_length_LB_maintenance <- "no work"
      # limb_angle_LB_maintenance <- "no work"
      # step_rate_RB_maintenance <- "no work"
      # step_length_RB_maintenance <- "no work"
      # limb_angle_RB_maintenance <- "no work"
      #
      both_stance_ratio_front <- "no work"
      both_stance_ratio_hind <- "no work"
      first_stance_start_LB <- "no work"
      first_stance_end_LB <- "no work"
      first_stance_start_RB <- "no work"
      first_stance_end_RB <- "no work"
    }
  } else {
    filenames <- filenames
    
    # speed parameters
    speed <- "no work"
    acceleration <- "no work"
    sprint_distance <- "no work"
    path_length <- "no work"
    
    ## lateral undulation parameters
    # all
    amplitude_head <- "no work"
    amplitude_shoulder <- "no work"
    amplitude_body <- "no work"
    amplitude_hip <- "no work"
    amplitude_tail <- "no work"
    spine_angle <- "no work"
    tail_angle <- "no work"
    head_sprint_angle <- "no work"
    body_sprint_angle <- "no work"
    tail_sprint_angle <- "no work"
    # # accerleration phase
    # amplitude_head_acceleration <- "no work"
    # amplitude_shoulder_acceleration <- "no work"
    # amplitude_body_acceleration <- "no work"
    # amplitude_hip_acceleration <- "no work"
    # amplitude_tail_acceleration <- "no work"
    # spine_angle_acceleration <- "no work"
    # tail_angle_acceleration <- "no work"
    # head_sprint_angle_acceleration <- "no work"
    # body_sprint_angle_acceleration <- "no work"
    # tail_sprint_angle_acceleration <- "no work"
    # # maintenance phase
    # amplitude_head_maintenance <- "no work"
    # amplitude_shoulder_maintenance <- "no work"
    # amplitude_body_maintenance <- "no work"
    # amplitude_hip_maintenance <- "no work"
    # amplitude_tail_maintenance <- "no work"
    # spine_angle_maintenance <- "no work"
    # tail_angle_maintenance <- "no work"
    # head_sprint_angle_maintenance <- "no work"
    # body_sprint_angle_maintenance <- "no work"
    # tail_sprint_angle_maintenance <- "no work"
    
    ## step parameters
    # all
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
    # # acceleration phase
    # step_rate_LF_acceleration <- "no work"
    # step_length_LF_acceleration <- "no work"
    # limb_angle_LF_acceleration <- "no work"
    # step_rate_RF_acceleration <- "no work"
    # step_length_RF_acceleration <- "no work"
    # limb_angle_RF_acceleration <- "no work"
    # step_rate_LB_acceleration <- "no work"
    # step_length_LB_acceleration <- "no work"
    # limb_angle_LB_acceleration <- "no work"
    # step_rate_RB_acceleration <- "no work"
    # step_length_RB_acceleration <- "no work"
    # limb_angle_RB_acceleration <- "no work"
    # # maintenance phase
    # step_rate_LF_maintenance <- "no work"
    # step_length_LF_maintenance <- "no work"
    # limb_angle_LF_maintenance <- "no work"
    # step_rate_RF_maintenance <- "no work"
    # step_length_RF_maintenance <- "no work"
    # limb_angle_RF_maintenance <- "no work"
    # step_rate_LB_maintenance <- "no work"
    # step_length_LB_maintenance <- "no work"
    # limb_angle_LB_maintenance <- "no work"
    # step_rate_RB_maintenance <- "no work"
    # step_length_RB_maintenance <- "no work"
    # limb_angle_RB_maintenance <- "no work"
    #
    both_stance_ratio_front <- "no work"
    both_stance_ratio_hind <- "no work"
    first_stance_start_LB <- "no work"
    first_stance_end_LB <- "no work"
    first_stance_start_RB <- "no work"
    first_stance_end_RB <- "no work"
  }
res <-
  list(
    filenames = filenames,
    
    # speed parameters
    speed = speed,
    acceleration = acceleration,
    sprint_distance = sprint_distance,
    path_length = path_length,
    
    ## lateral undulation parameters
    # all
    amplitude_head = amplitude_head,
    amplitude_shoulder = amplitude_shoulder,
    amplitude_body = amplitude_body,
    amplitude_hip = amplitude_hip,
    amplitude_tail = amplitude_tail,
    spine_angle = spine_angle,
    tail_angle = tail_angle,
    head_sprint_angle = head_sprint_angle,
    body_sprint_angle = body_sprint_angle,
    tail_sprint_angle = tail_sprint_angle,
    # # accerleration phase
    # amplitude_head_acceleration = amplitude_head_acceleration,
    # amplitude_shoulder_acceleration = amplitude_shoulder_acceleration,
    # amplitude_body_acceleration = amplitude_body_acceleration,
    # amplitude_hip_acceleration = amplitude_hip_acceleration,
    # amplitude_tail_acceleration = amplitude_tail_acceleration,
    # spine_angle_acceleration = spine_angle_acceleration,
    # tail_angle_acceleration = tail_angle_acceleration,
    # head_sprint_angle_acceleration = head_sprint_angle_acceleration,
    # body_sprint_angle_acceleration = body_sprint_angle_acceleration,
    # tail_sprint_angle_acceleration = tail_sprint_angle_acceleration,
    # # maintenance phase
    # amplitude_head_maintenance = amplitude_head_maintenance,
    # amplitude_shoulder_maintenance = amplitude_shoulder_maintenance,
    # amplitude_body_maintenance = amplitude_body_maintenance,
    # amplitude_hip_maintenance = amplitude_hip_maintenance,
    # amplitude_tail_maintenance = amplitude_tail_maintenance,
    # spine_angle_maintenance = spine_angle_maintenance,
    # tail_angle_maintenance = tail_angle_maintenance,
    # head_sprint_angle_maintenance = head_sprint_angle_maintenance,
    # body_sprint_angle_maintenance = body_sprint_angle_maintenance,
    # tail_sprint_angle_maintenance = tail_sprint_angle_maintenance,
    
    ## step parameters
    # all
    step_rate_LF = step_rate_LF,
    step_length_LF = step_length_LF,
    limb_angle_LF = limb_angle_LF,
    step_rate_RF = step_rate_RF,
    step_length_RF = step_length_RF,
    limb_angle_RF = limb_angle_RF,
    step_rate_LB = step_rate_LB,
    step_length_LB = step_length_LB,
    limb_angle_LB = limb_angle_LB,
    step_rate_RB = step_rate_RB,
    step_length_RB = step_length_RB,
    limb_angle_RB = limb_angle_RB,
    # # acceleration phase
    # step_rate_LF_acceleration = step_rate_LF_acceleration,
    # step_length_LF_acceleration = step_length_LF_acceleration,
    # limb_angle_LF_acceleration = limb_angle_LF_acceleration,
    # step_rate_RF_acceleration = step_rate_RF_acceleration,
    # step_length_RF_acceleration = step_length_RF_acceleration,
    # limb_angle_RF_acceleration = limb_angle_RF_acceleration,
    # step_rate_LB_acceleration = step_rate_LB_acceleration,
    # step_length_LB_acceleration = step_length_LB_acceleration,
    # limb_angle_LB_acceleration = limb_angle_LB_acceleration,
    # step_rate_RB_acceleration = step_rate_RB_acceleration,
    # step_length_RB_acceleration = step_length_RB_acceleration,
    # limb_angle_RB_acceleration = limb_angle_RB_acceleration,
    # # maintenance phase
    # step_rate_LF_maintenance = step_rate_LF_maintenance,
    # step_length_LF_maintenance = step_length_LF_maintenance,
    # limb_angle_LF_maintenance = limb_angle_LF_maintenance,
    # step_rate_RF_maintenance = step_rate_RF_maintenance,
    # step_length_RF_maintenance = step_length_RF_maintenance,
    # limb_angle_RF_maintenance = limb_angle_RF_maintenance,
    # step_rate_LB_maintenance = step_rate_LB_maintenance,
    # step_length_LB_maintenance = step_length_LB_maintenance,
    # limb_angle_LB_maintenance = limb_angle_LB_maintenance,
    # step_rate_RB_maintenance = step_rate_RB_maintenance,
    # step_length_RB_maintenance = step_length_RB_maintenance,
    # limb_angle_RB_maintenance = limb_angle_RB_maintenance,
    #
    both_stance_ratio_front = both_stance_ratio_front,
    both_stance_ratio_hind = both_stance_ratio_hind,
    first_stance_start_LB = first_stance_start_LB,
    first_stance_end_LB = first_stance_end_LB,
    first_stance_start_RB = first_stance_start_RB,
    first_stance_end_RB = first_stance_end_RB
  )
print(paste0("End analysis ", vFN, "."))
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
# filenames <- "211051FBG(F1)DLC_colnamefixed.csv"
# filenames <- "211117MAG(M1)DLC_colnamefixed.csv" # 無法計算step
# filenames <- "211117MBG(M1)DLC_colnamefixed.csv"



## 2025.05
# filenames <- "211052MAG(M1)DLC_colnamefixed.csv" # autotomy G
# filenames <- "211113FAG(F1)DLC_colnamefixed.csv" # autotomy G (bug)
# filenames <- "211174MAG(M3)DLC_colnamefixed.csv" # autotomy G
# filenames <- "211114MAG(M1)DLC_colnamefixed.csv" # autotomy G
# filenames <- "220375FAG(F3)DLC_colnamefixed.csv" # autotomy G
# filenames <- "211053MAG(M1)DLC_colnamefixed.csv" # autotomy G
# filenames <- "211051FAG(F1)DLC_colnamefixed.csv" # autotomy G
# filenames <- "2111719FAG(F3)DLC_colnamefixed.csv" # autotomy G (long interpolation)
# filenames <- "211051FAG(F3)DLC_colnamefixed.csv" # autotomy G



filenames <- "211173MAnG(M3)DLC_colnamefixed.csv" # autotomy nG
# filenames <- "211051FAnG(F2)DLC_colnamefixed.csv" # autotomy nG
# filenames <- "2111715FAnG(F3)DLC_colnamefixed.csv" # autotomy nG
# filenames <- "211053MAnG(M3)DLC_colnamefixed.csv" # autotomy nG
# filenames <- "220372MAnG(M2)DLC_colnamefixed.csv" # autotomy nG
# filenames <- "211051FAnG(F3)DLC_colnamefixed.csv" # autotomy nG
# filenames <- "211053MAnG(M1)DLC_colnamefixed.csv" # autotomy nG # no sprint
# filenames <- "2111719FAnG(F1)DLC_colnamefixed.csv" # autotomy nG # no sprint

# filenames <- "211052MBG(M2)DLC_colnamefixed.csv" # control G
# filenames <- "211112MBG(M2)DLC_colnamefixed.csv" # control G
# filenames <- "211176MBG(M3)DLC_colnamefixed.csv" # control G
# filenames <- "211252FBG(F1)DLC_colnamefixed.csv" # control G
# filenames <- "211179FBG(F1)DLC_colnamefixed.csv" # control G
# filenames <- "220378FAG(F1)DLC_colnamefixed.csv" # control G
# filenames <- "220379MAG(M1)DLC_colnamefixed.csv" # control G

# filenames <- "211173MBnG A(M2)DLC_colnamefixed.csv" # control nG
# filenames <- "211114MBnG(M2)DLC_colnamefixed.csv" # control nG
# filenames <- "211116MBnG A(M3)DLC_colnamefixed.csv" # control nG, straight
# filenames <- "211056FBnG A(F1)DLC_colnamefixed.csv" # control nG
# filenames <- "211052MBnG(M2)DLC_colnamefixed.csv" # control nG
# filenames <- "211054MAnG(M1)DLC_colnamefixed.csv" # control nG
# filenames <- "211051FBnG(F1)DLC_colnamefixed.csv" # control nG






#
res <-
  f_analyze_all(
    filenames,
    DLC_lkd = 0.9,
    length_track = 90,
    num_classic_sprint = 60,
    threshold_edge = 1,
    dist_nearzero = 0.02,
    plot = TRUE
  )
res
