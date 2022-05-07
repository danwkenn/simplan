#' Read in a simplan from a YAML file
#' @param plan unbaked plan.
#' @export
unbake_plan <- function(plan){

  plan$compute_chunks <- NULL
  plan$r_packages <- NULL
  plan$baked <- FALSE
  plan$nodes$rank <- NULL
  plan$nodes$inputs <- NULL

  plan
}
