source("simLongData_funs.R")


# ---- Simulation settings ----
set.seed(123)
n_total <- 200         # total subjects
K <- 4                 # number of clusters
outcomes <- 5
eta <- 3               # noise level; also set to 6 in some settings
cluster_sizes <- rep(n_total / K, K)  # Balanced setting (S0)

# ---- Fixed effect mean functions ----
# Each cluster has its own mean trajectory per outcome
# For simplicity, using some simple functional forms
mean_functions_hallucinated <- list(
  list(function(t) 2*t, function(t) sin(t), function(t) 0.5*t, function(t) 0.3*t^2, function(t) 0),
  list(function(t) 1 + 0.5*t, function(t) cos(t), function(t) 0.4*t, function(t) 0.2*t^2, function(t) 0),
  list(function(t) 2 - 0.3*t, function(t) -sin(t), function(t) 0.6*t, function(t) 0.1*t^2, function(t) 0),
  list(function(t) t^0.5, function(t) 0.5*cos(t), function(t) 0.2*t, function(t) 0.05*t^2, function(t) 0)
)

# Define correct mean functions from Table 2 (LaTeX source)
mean_functions <- list(
  # Cluster 1
  list(
    function(t) 8*t - 0.6*t^2,
    function(t) t,
    function(t) -10 + 6*t - 0.4*t^2,
    function(t) -1 + t,
    function(t) -2*t + 0.1*t^2
  ),
  # Cluster 2
  list(
    function(t) 20 - 6*t + 0.3*t^2,
    function(t) -t,
    function(t) -10 + 6*t - 0.4*t^2,
    function(t) -1 + t,
    function(t) -2*t + 0.1*t^2
  ),
  # Cluster 3
  list(
    function(t) 0,
    function(t) -7*t + 0.5*t^2,
    function(t) 0.2*t,
    function(t) -1 + t,
    function(t) -2*t + 0.1*t^2
  ),
  # Cluster 4
  list(
    function(t) 20,
    function(t) -20 + t,
    function(t) 0.2*t,
    function(t) 10 + 2*t - 0.2*t^2,
    function(t) -2*t + 0.1*t^2
  )
)

# ---- Covariance structure for random subject-level outcome effects ----
sigma_diag <- rep(3, outcomes)
R <- matrix(c(
  1,   0.5, 0.3, -0.1, 0,
  0.5, 1,   0.2,  0.1, 0,
  0.3, 0.2, 1,    0.1, 0,
  -0.1, 0.1, 0.1,  1,   0,
  0,   0,   0,    0,   1
), nrow = outcomes)
Sigma_sigma <- diag(sigma_diag) %*% R %*% diag(sigma_diag)

# ---- Simulate data ----
sim_data <- list()
subject_id <- 1

for (k in 1:K) {
  for (i in 1:cluster_sizes[k]) {
    n_i <- sample(4:12, 1)
    times <- sort(c(0, sort(runif(n_i - 1, min = 0.5, max = 11))))
    
    # Subject-specific outcome effects
    u_i <- mvrnorm(1, mu = rep(0, outcomes), Sigma = Sigma_sigma)
    
    for (j in 1:n_i) {
      t_ij <- times[j]
      random_effect_t <- generate_random_curve(t_ij)
      
      for (h in 1:outcomes) {
        mu <- mean_functions[[k]][[h]](t_ij)
        y_ijh <- mu + random_effect_t + u_i[h] +
          rnorm(1, mean = 0, sd = eta)  # You can change this to runif if needed
        
        sim_data[[length(sim_data) + 1]] <- data.frame(
          subject = subject_id,
          time = t_ij,
          outcome = h,
          y = y_ijh,
          cluster = k
        )
      }
    }
    subject_id <- subject_id + 1
  }
}

# ---- Combine into a data frame ----
sim_df <- do.call(rbind, sim_data)
head(sim_df)


plot_multivariate_trajectories(sim_df)

#smooth_plot_bspline(sim_df)
