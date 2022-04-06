library(targets)

list(
# Set the number of monte carlo simulations -------------------
tar_targets(n_runs, 10000),

# 1. alpha ----------------
tar_target(
  alpha,
  {
    rnorm(n_runs, mean = 0.5,
      sd = 1)
  }),

# 2. beta ----------------
tar_target(
  beta,
  {
    rnorm(n_runs, mean = alpha,
      sd = 1)
  }),

# 3. gamma ----------------
tar_target(
  gamma,
  {
    rnorm(n_runs, mean = alpha + sqrt(beta^4 * alpha^2),
      sd = 1)
  }),

# 4. eta ----------------
tar_target(
  eta,
  {
    rgamma(n_runs, shape = 3,
      rate = 1)
  }),

NULL
)