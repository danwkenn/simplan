#' Construct R code for an expression.
#' @param plan Simplan plan.
#' @param node Name of the node.
#' @param add_nsims Add the n-sims directly.
construct_r_code.Internal <- function(plan, node, add_nsims = FALSE){
  code <- "__NAME__ <- r__DISTRIBUTION__(__NSIMS__, __PARAMS__)"
  dist_name <- r_dists[plan$nodes[[node]]$distribution]
  PARAMS <- plan$nodes[[node]]$parameters
  PARAMS <- paste0(paste0(names(PARAMS)," = ",PARAMS),collapse  =", ")
  code <- sub("__NAME__",node,code)
  code <- sub("__DISTRIBUTION__",dist_name,code)
  if(!add_nsims){
    code <- sub("__NSIMS__",plan$n_runs,code)
  }else{
    code <- sub("__NSIMS__","n_runs",code)
  }
  code <- sub("__PARAMS__",PARAMS,code)
  code
}

