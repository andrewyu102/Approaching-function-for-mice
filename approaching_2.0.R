
raw_data <- read.csv(file.choose())

num_mice <- 5
moving_thred <- 2
is_moving <- matrix(0, nrow = nrow(raw_data), ncol = num_mice)
moving <- matrix(0, nrow = nrow(raw_data)-1, ncol = num_mice)
dist_thred <- 200
dist_close_all <- matrix(0, nrow = nrow(raw_data), ncol = num_mice - 1)

# Detect whether an agent is still or not
for (i in 1:num_mice) {
  moving[, i] <- abs(diff(raw_data[, i * 2])) + abs(diff(raw_data[, i * 2 + 1]))
  index <- which(moving [, i] > moving_thred)
  is_moving[index, i] <- 1
}
# Calculate distance from each mouse to mouse 1
dist_to_mouse1 <- matrix(0, nrow = nrow(raw_data), ncol = num_mice - 1)

for (i in 2:num_mice) {
  dist_to_mouse1[, i - 1] <- sqrt((raw_data[, i * 2] - raw_data[, 1 * 2])^2 + (raw_data[, i * 2 + 1] - raw_data[, 1 * 2 + 1])^2)
}

# Calculate change in distance from each mouse to mouse 1
change_in_distance_to_mouse1 <- matrix(0, nrow = nrow(raw_data), ncol = num_mice - 1)

for (i in 1:(num_mice - 1)) {
  change_in_distance_to_mouse1[, i] <- c(0, diff(dist_to_mouse1[, i]))
}
approaching <- matrix(0, nrow = nrow(raw_data), ncol = num_mice - 1)

for (i in 1:(num_mice - 1)) {
  index <- which(!is.na(change_in_distance_to_mouse1[, i]))
  if (any(change_in_distance_to_mouse1[index, i] < 0) && any(dist_to_mouse1[index, i] < dist_thred)) {
    approaching[index, i] <- 1
  }
}


#creating a results matrix
results <- matrix(0, nrow = num_mice, ncol = 3)

#finding the proportion of time mouse is moving more than 10 pixels per frame
for (i in 1:num_mice) {
  results[i, 1] <- length(which(moving[, i] > 10)) / length(which(moving[, i] > 0))
  # proportion of dataframe has valid data
  results[i, 2] <- length(which(moving[, i] > 0)) / nrow(moving)
  # proportion of data frame where mouse is moving less than 0.5 pixels per frame
  results[i, 3] <- length(which(moving[, i] < 0.5)) / length(which(moving[, i] > 0))
}


# Who did is approaching who
moving_approach_self <- numeric(num_mice - 1)
moving_approach_other <- numeric(num_mice - 1)

for (i in 1:(num_mice - 1)) {
  index <- which(approaching[, i] == 1)
  
  if (length(index) > 0) {
    moving_approach_self[i] <- mean(is_moving[index, 1])
    moving_approach_other[i] <- mean(is_moving[index, i + 1])
  } else {
    moving_approach_self[i] <- 0
    moving_approach_other[i] <- 0
  }
}

results_self <- sapply(moving_approach_self, mean)
results_other <- sapply(moving_approach_other, mean)

results_both <- matrix(0, nrow = num_mice - 1, ncol = 3)

for (i in 1:(num_mice - 1)) {
  moving_approach_both <- is_moving[index, 1]*2 + is_moving[index, i + 1]
  results_both[i, 1] <- sum(moving_approach_both == 1) / length(moving_approach_both)
  results_both[i, 2] <- sum(moving_approach_both == 2) / length(moving_approach_both)
  results_both[i, 3] <- sum(moving_approach_both == 3) / length(moving_approach_both)
}
results_both

