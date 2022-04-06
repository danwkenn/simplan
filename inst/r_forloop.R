# Set the number of monte carlo simulations -------------------
n_runs = 10000

# Instantiate the nodes -------------------
alpha <- rep(NA,n_runs)
beta <- rep(NA,n_runs)
gamma <- rep(NA,n_runs)
eta <- rep(NA,n_runs)

for(iter in 1:n_runs){
  # 1. alpha ----------------
  alpha[[iter]] <- rnorm(1, mean = 0.5, sd = 1)

  # 2. beta ----------------
  beta[[iter]] <- rnorm(1, mean = alpha[[iter]], sd = 1)

  # 3. gamma ----------------
  gamma[[iter]] <- rnorm(1, mean = alpha[[iter]] + sqrt(beta[[iter]]^4 * alpha[[iter]]^2), sd = 1)

  # 4. eta ----------------
  eta[[iter]] <- rgamma(1, shape = 3, rate = 1)

  # end loop
}