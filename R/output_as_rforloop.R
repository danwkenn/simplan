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
      parameters = lapply(plan$nodes, function(x){x[["parameters"]]}),
      contortion = lapply(plan$nodes, function(x){x[["contortion"]]})
    )

  # Determine the inputs for each node:
  inputs <- list()
  for(i in 1:nrow(plan_dt)){
    if(!is.null(plan_dt$contortion[[i]])){
      inputs[[i]] <- extract_variables_2.Internal(
        do.call("c",c(plan_dt$parameters[[i]],
                      do.call("c", do.call("c",(lapply(1:length(plan_dt$contortion[[i]]),
                                                       FUN = function(j){plan_dt$contortion[[i]][[j]]$parameters}))))
        )))
    }else{
      inputs[[i]] <- extract_variables_2.Internal(plan_dt$parameters[[i]])
    }
  }

  plan_dt$inputs <- inputs

  score <- sapply(plan_dt$name,FUN = add_count, plan_dt = plan_dt)
  plan_dt$order <- rank(score,ties.method = "first")

  data.table::setkey(plan_dt, order)
  plan_dt$expression <- sapply(X = plan_dt$name,
                               FUN = construct_r_forloop.Internal,
                               plan = plan,
                               plan_dt = plan_dt,
                               add_nsims = TRUE)

  plan_dt$comment <- paste0("# ", 1:nrow(plan_dt), ". ", plan_dt$name," ----------------")

  plan_dt$instantiate <- paste0(plan_dt$name," <- list()")

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
