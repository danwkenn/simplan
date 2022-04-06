#' Output a simplan into an R script.
#' @param file Output file path.
#' @param plan Simplan plan.
#' @param add_nsims Add the n-sims directly.
#' @examples
#' plan <- yaml::read_yaml('~/git-projects/simplan/inst/test.yaml')
#' \dontrun{output_as_rscript(plan,file = 'results.R',add_nsims = TRUE)}
#' @export
output_as_rscript <- function(file, plan, add_nsims = TRUE){

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
                               FUN = construct_r_code.Internal,
                               plan = plan,
                               add_nsims = TRUE)

  plan_dt$comment <- paste0("# ", 1:nrow(plan_dt), ". ", plan_dt$name," ----------------")

  sink(file = file)

  if(add_nsims){
    cat("# Set the number of monte carlo simulations -------------------\n")
    cat(paste0("n_runs = ", plan$n_runs,"\n\n"))
  }

  for(i in 1:nrow(plan_dt)){

    cat(plan_dt$comment[i])
    cat('\n')
    cat(plan_dt$expression[i])
    cat('\n\n')

  }

  sink(file = NULL)
  file
}
