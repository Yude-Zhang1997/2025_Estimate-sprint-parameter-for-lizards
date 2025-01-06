##### Stage 1-1. Construct a reliable data

#### 03. Remove the sprint beside edges
### Functions to predict the edges
## default threshold is set as 1.5 cm
# Function to predict the top edge
f_edge_top <- function(x, d_edge_f, threshold_edge = 1.5) {
  # Extract coordinates for the top edge
  x_top <- c(d_edge_f$Corner_LT_x, d_edge_f$Corner_RT_x)
  y_top <- c(d_edge_f$Corner_LT_y, d_edge_f$Corner_RT_y)
  
  # Calculate slope and intercept
  slope <- (y_top[2] - y_top[1]) / (x_top[2] - x_top[1])
  intercept <- y_top[1] - slope * x_top[1]
  
  # Compute the predicted y-coordinate
  edge_top <- intercept + slope * x - threshold_edge
  return(edge_top)
}

## Function to predict the bottom edge
f_edge_bottom <- function(x, d_edge_f,  threshold_edge = 1.5) {
  # Extract coordinates for the bottom edge
  x_bottom = c(d_edge_f$Corner_LB_x, d_edge_f$Corner_RB_x)
  y_bottom = c(d_edge_f$Corner_LB_y, d_edge_f$Corner_RB_y)
  
  # Calculate slope and intercept
  slope <- (y_bottom[2] - y_bottom[1]) / (x_bottom[2] - x_bottom[1])
  intercept <- y_bottom[1] - slope * x_bottom[1]
  
  # predict the edge
  edge_bottom <- intercept + slope * x + threshold_edge
  return(edge_bottom)
}

## Function to predict the left edge (starting line)
f_edge_left <- function(y, d_edge_f, threshold_edge = 1.5) {
  # Extract coordinates for the left edge
  x_left = c(d_edge_f$Corner_LT_x, d_edge_f$Corner_LB_x)
  y_left = c(d_edge_f$Corner_LT_y, d_edge_f$Corner_LB_y)
  
  # Calculate slope and intercept
  slope <- (x_left[2] - x_left[1]) / (y_left[2] - y_left[1])
  intercept <- x_left[1] - slope * y_left[1]
  
  # predict the edge
  edge_left <- intercept + slope * y + threshold_edge
  return(edge_left)
}

## Function to find the frame ID of sprint in the track edge
# return: (vector) ID that sprints within the track edge
f_find_ID_in_edge <- function(data, d_edge_f, threshold_edge = 1.5){
  d1_edge <- data %>%
    mutate(edge_top_y = f_edge_top(x = Head_x, d_edge_f, threshold_edge = 1.5)) %>%
    mutate(edge_bottom_y = f_edge_bottom(x = Head_x, d_edge_f, threshold_edge = 1.5)) %>%
    mutate(edge_left_x = f_edge_left( y = Head_y, d_edge_f, threshold_edge = 1.5))
  
  # Initialize an empty vector to store in edge frames
  ID_inedge <- integer()
  
  for (i in seq_along(d1_edge$ID)) {
    if (d1_edge$Head_x[i] > d1_edge$edge_left_x[i]) {
      if (d1_edge$Head_y[i] < d1_edge$edge_top_y[i] &&
          d1_edge$edge_bottom_y[i] < d1_edge$Head_y[i]) {
        ID_inedge <- append(ID_inedge, data$ID[i])
      }
    }
  }
  return(ID_inedge)
}


#### IV. Replace the replicated frame with NA
## Function to find the frame ID of frame replicated once
# d_vel for velocity data not sprint data
# default dist_threshold is 0.025
f_find_ID_replicate1 <- function(d_vel, dist_threshold = 0.025){
  valid_ID <- 3:(length(d_vel$dist) - 2) # to allow surrounding values check
  # When dist at i is less than dist_threshold , which i previous two and next two dist are greater than dist_threshold?
  surrounding_check <- (abs(d_vel$dist[(valid_ID - 2)]) > dist_threshold &
                          abs(d_vel$dist[(valid_ID - 1)]) > dist_threshold &
                          abs(d_vel$dist[(valid_ID + 1)]) > dist_threshold &
                          abs(d_vel$dist[(valid_ID + 2)]) > dist_threshold
  )
  
  ID_replicate1 <- valid_ID[
    surrounding_check &
      !is.na(d_vel$dist[valid_ID]) &
      abs(d_vel$dist[valid_ID]) < dist_threshold
  ]
  if (length(ID_replicate1) == 0) {
    return(NULL)
  } else {
    # Assign names to ID
    names(ID_replicate1) <- paste("ID", ID_replicate1)
    return(ID_replicate1)
  }
}

## Function to find the frame ID of frame replicated twice
f_find_ID_replicate2 <- function(d_vel, dist_threshold = 0.025) {
  valid_ID <- 3:(length(d_vel$dist) - 3) # to allow surrounding values check
  # When dist at i and i+1 is less than dist_threshold , which i previous two and (i+1) next two dist are greater than dist_threshold?
  surrounding_check <- (
    abs(d_vel$dist[(valid_ID - 2)]) > dist_threshold &
      abs(d_vel$dist[(valid_ID - 1)]) > dist_threshold &
      abs(d_vel$dist[(valid_ID + 2)]) > dist_threshold &
      abs(d_vel$dist[(valid_ID + 3)]) > dist_threshold
  )
  
  ID_replicate2_i <- valid_ID[surrounding_check &
                                !is.na(d_vel$dist[valid_ID]) &
                                abs(d_vel$dist[valid_ID]) < dist_threshold &
                                abs(d_vel$dist[(valid_ID + 1)]) < dist_threshold]
  ID_replicate2 <- c(ID_replicate2_i, (ID_replicate2_i + 1)) %>% sort
  
  if (length(ID_replicate2) == 0) {
    return(NULL)
  } else {
    # Assign names to ID
    names(ID_replicate2) <- paste("ID", ID_replicate2)
    return(ID_replicate2)
  }
}

## Function to turn the replicated data to NA_real_
# input data = sprint data
# return = (tibble) sprint data which replicated data has been replaced with NA_real_
f_turn_rep_to_NA <- function(d_sprint_inedge, ID_replicate) {
  if (is.null(ID_replicate)) {
    d_sprint_removerep <- d_sprint_inedge  # No changes in the data
    message("There's no replicated data.")
    return(d_sprint_removerep)
  } else {
    d_sprint_removerep <- d_sprint_inedge %>%
      mutate(across(ends_with(c("_x", "_y")), 
                    ~ if_else(ID %in% (ID_replicate + 1), NA_real_, .)))
    message("The replicated data has been replaced with NA.")
    return(d_sprint_removerep)
  }
}

##### 06. Rolling average and calculate angles
## Function to calculate the angle between two vectors a and b
# angle <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
# flip = T <- (180 - angle)
# cross = T <- Consider the cross product to decide positive or negative
f_calculate_angle <- function(x1, y1, x2, y2, x3, y3, flip = FALSE, cross = FALSE) {
  V1 <- c(x1 - x2, y1 - y2) %>% as.vector
  V2 <- c(x3 - x2, y3 - y2) %>% as.vector
  angle <- acos(sum(V1 * V2) / (sqrt(sum(V1^2)) * sqrt(sum(V2^2)))) * 180 / pi
  if (flip) {angle <- 180 - angle}
  if (cross){
    cross_product_V1V2 <- (x1 - x2) * (y3 - y2) - (y1 - y2) * (x3 - x2)
    if (cross_product_V1V2 < 0) {angle <- -angle}}
  return(angle)
} 