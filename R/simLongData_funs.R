library(MASS)       # For mvrnorm
library(splines)    # For B-splines
library(ggplot2)
library(dplyr)
library(MASS)


# Random time effect function: ri(t) = bi0 + bi1*t + bi2*t^2
generate_random_curve <- function(times, 
                                  sigma_b = diag(c(2, 0.3, 0.06)), 
                                  rho_b
) {
  if (missing(rho_b)) {
      # As per Section 3.2
    rho_b <- matrix(c(
      1.0,  0.4, -0.3,
      0.4,  1.0, -0.2,
      -0.3, -0.2, 1.0
    ), nrow = 3, byrow = TRUE)
  }
  # Full covariance matrix
  Sigma_b <- sigma_b %*% rho_b %*% sigma_b
  b <- mvrnorm(1, mu = rep(0, 3), Sigma = Sigma_b)
  b[1] + b[2]*times + b[3]*times^2
}

# ---- Random time effect function (from Case 1) ----
generate_random_curve_old <- function(times, sigma_b = c(2, 0.3, 0.06), rho = 0.5) {
  B <- cbind(
    rep(1, length(times)),
    times,
    times^2
  )
  cov_b <- diag(sigma_b^2)
  b <- mvrnorm(1, mu = rep(0, 3), Sigma = cov_b)
  B %*% b
}


plot_multivariate_trajectories <- function(data) {
  # Assumes: columns = subject, time, outcome, y, cluster
  data$outcome <- factor(data$outcome, labels = paste0("Y", 1:5))
  data$cluster <- as.factor(data$cluster)
  
  # Compute average trajectory per cluster/outcome/time
  avg_data <- data %>%
    group_by(cluster, outcome, time) %>%
    summarise(mean_y = mean(y), .groups = 'drop')
  if (0)
  ggplot(data, aes(x = time, y = y, group = subject, color = cluster)) +
    geom_line(alpha = 0.3, linewidth = 0.3) +  # Individual trajectories
    geom_line(data = avg_data, aes(x = time, y = mean_y, group = cluster),
              linewidth = 1.2, alpha = 0.9) +  # Cluster mean
    facet_wrap(~ outcome, scales = "free_y") +
    theme_minimal(base_size = 14) +
    labs(title = "Simulated Multivariate Longitudinal Data",
         x = "Time", y = "Outcome Value") +
    scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "bottom")
  
  ggplot(data, aes(x = time, y = y, color = cluster)) +
    geom_line(aes(group = subject), alpha = 0.3, linewidth = 0.3) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
    facet_wrap(~ factor(outcome, labels = paste0("Y", 1:5)), scales = "free_y") +
    theme_minimal(base_size = 14) +
    labs(title = "Smoothed Multivariate Longitudinal Trajectories",
         x = "Time", y = "Outcome Value") +
    scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "bottom")
  
}

smooth_plot_bspline <- function(data, time_grid = seq(0, 11, length.out = 100), spline_df = 6) {
  data$outcome <- factor(data$outcome, labels = paste0("Y", 1:5))
  data$cluster <- as.factor(data$cluster)
  
  smoothed_list <- list()
  
  # Loop through each subject × outcome
  for (subj in unique(data$subject)) {
    for (h in unique(data$outcome)) {
      df_sub <- data %>% filter(subject == subj, outcome == h)
      
      if (nrow(df_sub) < 4) next  # Skip very sparse subjects
      
      fit <- lm(y ~ bs(time, df = spline_df), data = df_sub)
      y_pred <- predict(fit, newdata = data.frame(time = time_grid))
      
      smoothed_list[[length(smoothed_list) + 1]] <- data.frame(
        subject = subj,
        outcome = h,
        cluster = unique(df_sub$cluster),
        time = time_grid,
        y = y_pred
      )
    }
  }
  
  smooth_df <- do.call(rbind, smoothed_list)
  
  # Average across subjects by cluster × outcome × time
  avg_df <- smooth_df %>%
    group_by(cluster, outcome, time) %>%
    summarise(mean_y = mean(y), .groups = 'drop')
  
  # Plot
  ggplot() +
    geom_line(data = smooth_df, aes(x = time, y = y, group = subject, color = cluster),
              alpha = 0.2, linewidth = 0.3) +
    geom_line(data = avg_df, aes(x = time, y = mean_y, color = cluster),
              linewidth = 1.4) +
    facet_wrap(~ outcome, scales = "free_y") +
    theme_minimal(base_size = 14) +
    labs(title = "B-spline Smoothed Trajectories with Cluster Means",
         x = "Time", y = "Outcome Value") +
    scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "bottom")
}
