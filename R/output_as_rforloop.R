#' Output a simplan into an R script.
#' @param file Output file path.
#' @param plan Simplan plan.
#' @param add_nsims Add the n-sims directly.
#' @examples
#' plan <- yaml::read_yaml('~/git-projects/simplan/inst/test.yaml')
#' \dontrun{output_as_rforloop(plan,file = 'results.R',add_nsims = TRUE)}
#' @export
output_as_rforloop <- function(file, plan, add_nsims = TRUE){

  # Create data-table for plan.
  plan_dt <-
    data.table::data.table(
      name = names(plan$nodes),
      distribution = sapply(plan$nodes, function(x){x$distribution}),
      parameters = lapply(plan$nodes, function(x){x[["parameters"]]})
    )

  plan_dt$inputs <- lapply(extract_variables_2.Internal,X = plan_dt$parameters)

  score <- rep(NA, nrow(plan_dt))
  arguments <- do.call("c",lapply(plan_dt$inputs, unique))
  plan_dt$order <- order(-sapply(plan_dt$name, function(x){sum(x == arguments)}))

  data.table::setkey(plan_dt, order)
  plan_dt$expression <- sapply(X = plan_dt$name,
                               FUN = construct_r_forloop.Internal,
                               plan = plan,
                               plan_dt = plan_dt,
                               add_nsims = TRUE)

  plan_dt$comment <- paste0("# ", 1:nrow(plan_dt), ". ", plan_dt$name," ----------------")

  plan_dt$instantiate <- paste0(plan_dt$name," <- list(NA,n_runs)")

  sink(file = file)

  if(any(sapply(r_dists[plan_dt$distribution], function(x){x %in% simplan_dists}))){
    cat("# Simplan Distributions required ---------------------\n")
    cat("library(simplan)\n\n")
  }

  if(add_nsims){
    cat("# Set the number of monte carlo simulations -------------------\n")
    cat(paste0("n_runs = ", plan$n_runs,"\n\n"))
  }

  cat("# Instantiate the nodes -------------------\n")
  for(i in 1:nrow(plan_dt)){
    cat(plan_dt$instantiate[i])
    cat("\n")
  }

  cat("\nfor(iter in 1:n_runs){\n\n  ")

  for(i in 1:nrow(plan_dt)){

    cat(plan_dt$comment[i])
    cat('\n  ')
    cat(plan_dt$expression[i])
    cat('\n\n  ')

  }

  cat("# end loop\n")
  cat("}")
  sink(file = NULL)
  file
}
