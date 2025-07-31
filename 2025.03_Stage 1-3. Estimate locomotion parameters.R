##### Stage 1-3. Estimate locomotion parameters

#### 3.1 Estimate speed parameter
## function to measure the sprint distance of the selected sprint
f_estimate_sprint_distance <-
  function(d_sprint_select, x1, y1, x2, y2) {
    x1_vec <- d_sprint_select %>% pull({{x1}})
    y1_vec <- d_sprint_select %>% pull({{y1}})
    x2_vec <- d_sprint_select %>% pull({{x2}})
    y2_vec <- d_sprint_select %>% pull({{y2}})
    
    x_mean_first <- (first(x1_vec) + first(x2_vec)) / 2
    y_mean_first <- (first(y1_vec) + first(y2_vec)) / 2
    x_mean_last <- (last(x1_vec) + last(x2_vec)) / 2
    y_mean_last <- (last(y1_vec) + last(y2_vec)) / 2
    
    sprint_distance <-
      sqrt((x_mean_last - x_mean_first)^2 + (y_mean_last - y_mean_first)^2)
    return(sprint_distance)
  }

#### 3.2 Lateral undulation of spine ####
## function to estimate amplitude of bodyparts
f_estimate_displacement <-
  function(d_sprint_select,
           bodypart_x,
           bodypart_y,
           slope,
           intercept) {
    displacement <-
      d_sprint_select %>%
      mutate(
        x_val = {{bodypart_x}},
        y_val = {{bodypart_y}},
        slope_val = {{slope}},
        intercept_val = {{intercept}}
      ) %>%
      mutate(
        displacement_initial =
          abs(slope_val * x_val - y_val + intercept_val) / sqrt(slope_val^2 + 1),
        sign =
          y_val - (slope_val * x_val + intercept_val),
        displacement =
          case_when(
            sign >= 0 ~ displacement_initial,
            sign < 0 ~ -displacement_initial
          )
      ) %>%
      pull(displacement)
    
    return(displacement)
  }

## function to plot amplitude of bodyparts
# output: plot of amplitude
f_plot_amplitude <-
  function(d_sprint_select, 
           amplitude_head, 
           amplitude_shoulder, 
           amplitude_body, 
           amplitude_hip, 
           amplitude_tail, 
           ID_maintenance_phase,
           vFN) {
    plot_amplitude_head <-
      ggplot(data = tibble(ID = d_sprint_select$ID, amplitude_head = amplitude_head)) +
      geom_point(aes(x = ID, y = amplitude_head)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_vline(aes(xintercept = d_sprint_select$ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      labs(title = "head") +
      scale_y_continuous(
        name = "amplitude",
        breaks = seq(-5, 5, 0.5),
        limits = c(min(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) - 0.1, 
                   max(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) + 0.1)
      ) +
      theme_pubr(border = T, margin = T)
    
    plot_amplitude_shoulder <-
      ggplot(data = tibble(ID = d_sprint_select$ID, amplitude_shoulder = amplitude_shoulder)) +
      geom_point(aes(x = ID, y = amplitude_shoulder)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_vline(aes(xintercept = d_sprint_select$ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      labs(title = "shoulder") +
      scale_y_continuous(
        name = "amplitude",
        breaks = seq(-5, 5, 0.5),
        limits = c(min(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) - 0.1, 
                   max(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) + 0.1)
      ) +
      theme_pubr(border = T, margin = T)
    
    plot_amplitude_body <-
      ggplot(data = tibble(ID = d_sprint_select$ID, amplitude_body = amplitude_body)) +
      geom_point(aes(x = ID, y = amplitude_body)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_vline(aes(xintercept = d_sprint_select$ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      labs(title = "body") +
      scale_y_continuous(
        name = "amplitude",
        breaks = seq(-5, 5, 0.5),
        limits = c(min(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) - 0.1, 
                   max(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) + 0.1)
      ) +
      theme_pubr(border = T, margin = T)
    
    plot_amplitude_hip <-
      ggplot(data = tibble(ID = d_sprint_select$ID, amplitude_hip = amplitude_hip)) +
      geom_point(aes(x = ID, y = amplitude_hip)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_vline(aes(xintercept = d_sprint_select$ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      labs(title = "hip") +
      scale_y_continuous(
        name = "amplitude",
        breaks = seq(-5, 5, 0.5),
        limits = c(min(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) - 0.1, 
                   max(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) + 0.1)
      ) +
      theme_pubr(border = T, margin = T)
    
    plot_amplitude_tail <-
      ggplot(data = tibble(ID = d_sprint_select$ID, amplitude_tail = amplitude_tail)) +
      geom_point(aes(x = ID, y = amplitude_tail)) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_vline(aes(xintercept = d_sprint_select$ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      labs(title = "tail") +
      scale_y_continuous(
        name = "amplitude",
        breaks = seq(-5, 5, 0.5),
        limits = c(min(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) - 0.1, 
                   max(c(amplitude_head, 
                         amplitude_shoulder,
                         amplitude_body,
                         amplitude_hip,
                         amplitude_tail)) + 0.1)
      ) +
      theme_pubr(border = T, margin = T)
    
    plot_amplitude_arrange <-
      ggarrange(
        plot_amplitude_head,
        plot_amplitude_shoulder,
        plot_amplitude_body,
        plot_amplitude_hip,
        plot_amplitude_tail,
        nrow = 5,
        common.legend = T
      )
    plot_amplitude <-
      annotate_figure(plot_amplitude_arrange,
                      top = text_grob(
                        paste0("amplitude", " (", sub("DLC.*", "", vFN), ")"),
                        color = "black",
                        face = "bold",
                        size = 12
                      ))
    
    return(plot_amplitude)
  }

## function to plot lateral undulation angles
f_plot_lateral_undulation_angle <- function(d_sprint_select, 
                                            spine_angle, 
                                            tail_angle,
                                            head_sprint_angle,
                                            body_sprint_angle,
                                            tail_sprint_angle,
                                            ID_maintenance_phase,
                                            vFN) {
          
    # spine angle
  plot_spine_angle <-
    ggplot(data = d_sprint_select, aes(x = ID, y = Angle_Spine)) +
    geom_point(size = 1) +
    geom_line(linewidth = 0.75) +
    geom_hline(aes(yintercept = 0),
               color = "gray",
               linetype = 2) +
    geom_vline(aes(xintercept = ID[20]), linetype = 2) +
    geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
    scale_x_continuous(name = "ID") +
    scale_y_continuous(name = "Degree", limits = c(min(
      c(
        spine_angle,
        tail_angle,
        head_sprint_angle,
        body_sprint_angle,
        tail_sprint_angle
      )
    ) - 0.1, max(
      c(
        spine_angle,
        tail_angle,
        head_sprint_angle,
        body_sprint_angle,
        tail_sprint_angle
      )
    ) + 0.1)) +
    labs(title = paste0("spine angle", " (", sub("DLC.*", "", vFN), ")")) +
    theme_pubr(border = T, margin = T)
      
    
    # tail angle
    plot_tail_angle <-
      ggplot(data = d_sprint_select, aes(x = ID, y = Angle_Tail)) +
      geom_point(size = 1) +
      geom_line(linewidth = 0.6) +
      geom_hline(aes(yintercept = 0),
                 color = "gray",
                 linetype = 2) +
      geom_vline(aes(xintercept = ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      scale_x_continuous(name = "ID") +
      scale_y_continuous(name = "Degree",
                         limits = c(min(c(spine_angle,
                                          tail_angle, 
                                          head_sprint_angle,
                                          body_sprint_angle,
                                          tail_sprint_angle)) - 0.1, 
                                    max(c(spine_angle,
                                          tail_angle, 
                                          head_sprint_angle,
                                          body_sprint_angle,
                                          tail_sprint_angle)) + 0.1)) +
      labs(title = paste0("tail angle", " (", sub("DLC.*", "", vFN), ")")) +
      theme_pubr(border = T, margin = T)
    
    # head_sprint_angle
    plot_head_sprint_angle <-
      ggplot(data = d_sprint_select, aes(x = ID, y = Angle_Body1Head)) +
      geom_point(size = 1) +
      geom_line(linewidth = 0.6) +
      geom_hline(aes(yintercept = 0),
                 color = "gray",
                 linetype = 2) +
      geom_vline(aes(xintercept = ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      scale_x_continuous(name = "ID") +
      scale_y_continuous(name = "Degree",
                         limits = c(min(c(spine_angle,
                                          tail_angle, 
                                          head_sprint_angle,
                                          body_sprint_angle,
                                          tail_sprint_angle)) - 0.1, 
                                    max(c(spine_angle,
                                          tail_angle, 
                                          head_sprint_angle,
                                          body_sprint_angle,
                                          tail_sprint_angle)) + 0.1)) +
      labs(title = paste0("head sprint angle", " (", sub("DLC.*", "", vFN), ")")) +
      theme_pubr(border = T, margin = T)
    
    # body_sprint_angle
    plot_body_sprint_angle <-
      ggplot(data = d_sprint_select, aes(x = ID, y = Angle_Body3Body1)) +
      geom_point(size = 1) +
      geom_line(linewidth = 0.6) +
      geom_hline(aes(yintercept = 0),
                 color = "gray",
                 linetype = 2) +
      geom_vline(aes(xintercept = ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      scale_x_continuous(name = "ID") +
      scale_y_continuous(name = "Degree",
                         limits = c(min(c(spine_angle,
                                          tail_angle, 
                                          head_sprint_angle,
                                          body_sprint_angle,
                                          tail_sprint_angle)) - 0.1, 
                                    max(c(spine_angle,
                                          tail_angle, 
                                          head_sprint_angle,
                                          body_sprint_angle,
                                          tail_sprint_angle)) + 0.1)) +
      labs(title = paste0("body sprint angle", " (", sub("DLC.*", "", vFN), ")")) +
      theme_pubr(border = T, margin = T)
    
    # tail_sprint_angle
    plot_tail_sprint_angle <-
      ggplot(data = d_sprint_select, aes(x = ID, y = Angle_Body3Tail)) +
      geom_point(size = 1) +
      geom_line(linewidth = 0.6) +
      geom_hline(aes(yintercept = 0),
                 color = "gray",
                 linetype = 2) +
      geom_vline(aes(xintercept = ID[20]), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end), linetype = 2) +
      scale_x_continuous(name = "ID") +
      scale_y_continuous(name = "Degree",
                         limits = c(min(c(spine_angle,
                                          tail_angle, 
                                          head_sprint_angle,
                                          body_sprint_angle,
                                          tail_sprint_angle)) - 0.1, 
                                    max(c(spine_angle,
                                          tail_angle, 
                                          head_sprint_angle,
                                          body_sprint_angle,
                                          tail_sprint_angle)) + 0.1)) +
      labs(title = paste0("tail sprint angle", " (", sub("DLC.*", "", vFN), ")")) +
      theme_pubr(border = T, margin = T)
    
    plot_lateral_undulation_angle <-
      ggarrange(
        plot_spine_angle,
        plot_tail_angle,
        plot_head_sprint_angle,
        plot_body_sprint_angle,
        plot_tail_sprint_angle,
        nrow = 5
      )
    
    return(plot_lateral_undulation_angle)
  }

#### 3.3 Step rate, step length, and Rang of motion for limbs ####

## function to create the velocity data of limbs
f_create_velocity_data_of_limbs <-
  function(d_sprint_select,
           limb,
           k_s = 11,
           dist_nearzero) {
    # check limb
    if (!limb %in% c("LF", "RF", "LB", "RB")) {
      stop()
      message("limb should be LF, RF, LB, or RB.")
    }
    
    # create data
    d_vel_limb <-
      tibble(
        dx = d_sprint_select[[paste0(limb, "_x_rm")]] %>% diff,
        dy = d_sprint_select[[paste0(limb, "_y_rm")]] %>% diff,
        dang = d_sprint_select[[paste0("Angle_", limb)]] %>% diff
      ) %>%
      mutate(
        dist = sqrt(dx^2 + dy^2),
        vel = dist * 240,
        vel_limb_sgolayfilt = gsignal::sgolayfilt(vel, n = k_s),
        dist_mod = if_else(dist < dist_nearzero, 0, dist),
        vel_mod = dist_mod * 240,
        dang_mod = if_else(abs(dang) < 1, 0, dang)
      ) %>%
      mutate(
        ID = d_sprint_select$ID[-length(d_sprint_select$ID)],
        vel_body = sqrt((d_sprint_select$Mean_x_all_body_parts %>% diff)^2 + (d_sprint_select$Mean_y_all_body_parts %>% diff)^2
        ) * 240,
        vel_body_sgolayfilt = gsignal::sgolayfilt(vel_body, n = k_s),
        phase =
          if_else(
            vel_body_sgolayfilt - vel_limb_sgolayfilt > 0,
            "stance",
            "swing"
          )
      )
    
    return(d_vel_limb)
  }

## Function to plot the velocity of four limbs with ggplot
f_plot_velocity_limb <-
  function(d_vel,
           ID_maintenance_phase, 
           limb){
    if(!limb %in% c("LF", "RF","LB", "RB")){
      stop()
      warning("limb should be LF, RF, LB, or RB.")
    }
    
    # define the label of limb on the plot
    if(limb == "LF"){
      limb_label <- "left forelimb"
    } else if(limb == "RF"){
      limb_label <- "right forelimb"
    } else if(limb == "LB"){
      limb_label <- "left hindlimb"
    } else if(limb == "RB"){
      limb_label <- "right hindlimb"
    }
    
    plot_velocity_limb <-
      ggplot(data = d_vel,
             mapping = aes(x = ID, y = vel_limb_sgolayfilt)) +
      geom_line() +
      geom_point(aes(color = phase)) +
      geom_line(aes(x = ID, y = vel_body_sgolayfilt), linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_start),
                 linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end),
                 linetype = 2) +
      scale_x_continuous(name = "ID", breaks = seq(0, last(d_vel$ID), by = 10)) +
      scale_y_continuous("velocity (cm / s)") +
      scale_color_manual(values = c("blue", "red")) +
      labs(title = limb_label) +
      theme_pubr(border = T,
                 margin = T,
                 legend = "top")
    
    return(plot_velocity_limb)
  }

## Function to plot the diff angle of four limbs with ggplot
f_plot_diff_angle_limb <-
  function(d_vel, 
           ID_maintenance_phase, 
           limb) {
    if (!limb %in% c("LF", "RF", "LB", "RB")) {
      stop()
      warning("limb should be LF, RF, LB, or RB.")
    }
    
    # define the label of limb on the plot
    if (limb == "LF") {
      limb_label <- "left forelimb"
    } else if (limb == "RF") {
      limb_label <- "right forelimb"
    } else if (limb == "LB") {
      limb_label <- "left hindlimb"
    } else if (limb == "RB") {
      limb_label <- "right hindlimb"
    }
    
    plot_dangt <-
      ggplot(data = d_vel, 
             aes(x = ID, y = dang)) +
      geom_hline(
        aes(yintercept = 0),
        linetype = 3,
        color = "gray",
        linewidth = 1
      ) +
      geom_line() +
      geom_point(aes(color = phase)) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_start),
                 linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end),
                 linetype = 2) +
      scale_y_continuous("Degree") +
      scale_color_manual(values = c("blue", "red")) +
      labs(title = limb_label) +
      theme_pubr(border = T,
                 margin = T,
                 legend = "top")
    
    return(plot_dangt)
  }

## Function to plot the angle of four limbs with ggplot
f_plot_angle_limb <-
  function(d_sprint_select, 
           d_vel_limb, 
           ID_maintenance_phase, 
           limb) {
    if (!limb %in% c("LF", "RF", "LB", "RB")) {
      stop()
      warning("limb should be LF, RF, LB, or RB.")
    }
    
    # define the label of limb on the plot
    if (limb == "LF") {
      limb_label <- "left forelimb"
    } else if (limb == "RF") {
      limb_label <- "right forelimb"
    } else if (limb == "LB") {
      limb_label <- "left hindlimb"
    } else if (limb == "RB") {
      limb_label <- "right hindlimb"
    }
    d_plot <-
      tibble(ID = d_sprint_select$ID[- length(d_sprint_select$ID)], 
             angle = d_sprint_select[[paste0("Angle_", limb)]][- length(d_sprint_select$ID)]) %>%
      mutate(phase = d_vel_limb[["phase"]])
    
    plot_angle_limb <-
      ggplot(data = d_plot, aes(x = ID, y = angle)) +
      geom_hline(
        aes(yintercept = mean(angle)),
        linewidth = 0.5,
        color = "darkgray",
        linetype = 2
      ) +
      geom_point(size = 1,
                 aes(color = phase)) +
      geom_line(linewidth = 0.6) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_start),
                 linetype = 2) +
      geom_vline(aes(xintercept = ID_maintenance_phase$ID_end),
                 linetype = 2) +
      scale_y_continuous(name = "Degree") +
      scale_color_manual(
        values = c("blue", "red")
      ) +
      labs(title = limb_label) +
      theme_pubr(border = T, margin = T)
    
    return(plot_angle_limb)
  }

## Function to find start ID and end ID each swing
# input: velocity data of the limb
# return: (tibble) with start ID and end ID of each stance
f_find_ID_stance <- function(d_vel_limb) {
  ## Find the start ID
  # 前一格為swing且後兩格為stance
  f_find_ID_stance_start <- function(d_vel_limb) {
    ID_stance_start <- integer()
    for (i in seq_along(d_vel_limb[["ID"]])) {
      if (i > 1 && i < (length(d_vel_limb[["ID"]]) - 2)) {
        if (!is.na(d_vel_limb[["phase"]][i]) &&
            !is.na(d_vel_limb[["phase"]][i - 1]) &&
            !any(is.na(d_vel_limb[["phase"]][(i + 1):(i + 2)])) &&
            d_vel_limb[["phase"]][i] == "stance" &&
            d_vel_limb[["phase"]][i - 1] == "swing" &&
            all(d_vel_limb[["phase"]][(i + 1):(i + 2)] == "stance")) {
          ID_stance_start <- c(ID_stance_start, d_vel_limb[["ID"]][i])
        }
      }
    }
    return(ID_stance_start)
  }
  ID_stance_start <- f_find_ID_stance_start(d_vel_limb = d_vel_limb)
  
  ## Find the end ID
  # criteria: dang[i] < 0 & dang[(i-2):(i-1)] < 0 & dang[i + 1] >= 0
  f_find_ID_stance_end <- function(d_vel_limb) {
    ID_stance_end <- integer()
    for (i in seq_along(d_vel_limb[["ID"]])) {
      if (i > 2 && i < (length(d_vel_limb[["ID"]]) - 1)) {
        if (!is.na(d_vel_limb[["phase"]][i]) &&
            !is.na(d_vel_limb[["phase"]][i + 1]) &&
            !any(is.na(d_vel_limb[["phase"]][(i - 2):(i - 1)])) &&
            d_vel_limb[["phase"]][i] == "stance" &&
            all(d_vel_limb[["phase"]][(i - 2):(i - 1)] == "stance") &&
            d_vel_limb[["phase"]][i + 1] == "swing") {
          ID_stance_end <- c(ID_stance_end, d_vel_limb[["ID"]][i])
        }
      }
    }
    return(ID_stance_end)
  }
  ID_stance_end <- f_find_ID_stance_end(d_vel_limb = d_vel_limb)
  
  ## If no swing is able to be estimated, return "NA".
  if (length(ID_stance_start) == 0 && 
      length(ID_stance_end) == 0) {
    if (all(d_vel_limb[["phase"]] == "stance")){
      ID_stance <-
        tibble(ID_stance_start = first(d_vel_limb[["ID"]]), ID_stance_end = last(d_vel_limb[["ID"]]))
    } else {
      ID_stance <-
        tibble(ID_stance_start = NA, ID_stance_end = NA)
    }
  } else if (length(ID_stance_start) < length(ID_stance_end)) {
    message("add the first ID into ID_stance_start")
    ID_stance <-
      tibble(
        ID_stance_start = c(first(d_vel_limb[["ID"]]), ID_stance_start),
        ID_stance_end = ID_stance_end
      )
  } else if (length(ID_stance_start) > length(ID_stance_end)) {
    message("add the last ID into ID_stance_end")
    ID_stance <-
      tibble(ID_stance_start = ID_stance_start,
             ID_stance_end = c(ID_stance_end, last(d_vel_limb[["ID"]])))
  } else if (length(ID_stance_start) == length(ID_stance_end)) {
    if (first(ID_stance_start) < first(ID_stance_end)) {
      ID_stance <-
        tibble(ID_stance_start = ID_stance_start, ID_stance_end = ID_stance_end)
    } else {
      message("add the first ID into ID_stance_start")
      message("add the last ID into ID_stance_end")
      ID_stance <-
        tibble(
          ID_stance_start = c(first(d_vel_limb[["ID"]]), ID_stance_start),
          ID_stance_end = c(ID_stance_end, last(d_vel_limb[["ID"]]))
        )
    }
  }
  
  return(ID_stance)
}

## Function to estimate the step length of each swing
# input: (tibble) selected sprint data
# return: (vector) step length of each swing
f_estimate_step_length <- function(d_sprint_select, 
                                   ID_stance, 
                                   limb){
  # Validate limb input
  if (!(limb %in% c("LF", "RF", "LB", "RB"))){
    stop("Invalid limb input. Choose from 'LF', 'RF', 'LB', and 'RB'.")}
  
  # Initialize an empty vector for storing step lengths
  step_length <- integer()
  
  # loop through each swing
  for (i in seq_along(ID_stance$ID_stance_start)) {
    # Get the coordinate of swing start
    d_sprint_stance_start <- 
      d_sprint_select %>% 
      filter(ID == ID_stance$ID_stance_start[i])
    
    xs <- d_sprint_stance_start[["Mean_x_all_body_parts"]]
    ys <- d_sprint_stance_start[["Mean_y_all_body_parts"]]
    
    # Get the coordinate of swing end
    d_sprint_stance_end <- 
      d_sprint_select %>%
      filter(ID == (ID_stance$ID_stance_end[i]))
    xe <- d_sprint_stance_end[["Mean_x_all_body_parts"]]
    ye <- d_sprint_stance_end[["Mean_y_all_body_parts"]]
    dist <- sqrt((xe-xs)^2 + (ye-ys)^2)
    step_length <- c(step_length, dist)
  }
  return(step_length)
}

## Function to estimate the limb angle of each swing
# input: (tibble) selected sprint data
# return: (vector) limb angle of each swing
f_estimate_limb_angle <- 
  function(d_sprint_select, ID_stance, limb){
  # Validate limb input
  if (!(limb %in% c("LF", "RF", "LB", "RB"))){
    stop("Invalid limb input. Choose from 'LF', 'RF', 'LB','RB'.")}
    
  # initialize an empty vector for storing angles
  limb_angle <- numeric()
  
  # loop through each swing
  for (i in seq_along(ID_stance$ID_stance_start)) {
    d_sprint_stance_start <-
      d_sprint_select %>% 
      filter(ID == ID_stance$ID_stance_start[i])
    ang_start <- d_sprint_stance_start[[paste0("Angle_", limb)]]
    d_sprint_stance_end <- 
      d_sprint_select %>%
      filter(ID == ID_stance$ID_stance_end[i])
    ang_end <- d_sprint_stance_end[[paste0("Angle_", limb)]]
    limb_angle_i <- ang_end - ang_start
    limb_angle <- c(limb_angle, limb_angle_i)
  }
  return(limb_angle)
}

##### Estimate spine angle
## Function to estimate spine angle of each move
# input: (tibble) selected sprint data
# return: (vector) spine angle of each move
# position = c("right", "left") <- choose the body side
f_estimate_spine_angle <-
  function(d_sprint_select) {
    Angle_spine_match_condition <- integer()
    
    # 身體往右為正
    for (i in seq_along(d_sprint_select$ID)) {
      if (i > 2 &&
          i < (length(d_sprint_select$ID) - 2) &&
          
          (
            # 身體往右 <- 前後皆小於自己
            all(d_sprint_select[["Angle_Spine"]][(i - 2):(i - 1)] <
                d_sprint_select[["Angle_Spine"]][i]) &&
            all(d_sprint_select[["Angle_Spine"]][(i + 1):(i + 2)] <
                d_sprint_select[["Angle_Spine"]][i])
            ||
            # 身體往左 <-前後皆大於自己
            all(d_sprint_select[["Angle_Spine"]][(i - 2):(i - 1)] >
                d_sprint_select[["Angle_Spine"]][i]) &&
            all(d_sprint_select[["Angle_Spine"]][(i + 1):(i + 2)] >
                d_sprint_select[["Angle_Spine"]][i])
          )
      ) {
        Angle_spine_match_condition <- 
          c(Angle_spine_match_condition, d_sprint_select[["Angle_Spine"]][i])
      }
    }
    Angle_spine <- c(first(d_sprint_select[["Angle_Spine"]]),
                     Angle_spine_match_condition,
                     last(d_sprint_select[["Angle_Spine"]]))
    
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
