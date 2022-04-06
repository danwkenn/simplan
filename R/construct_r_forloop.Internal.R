#' Construct R code for an expression.
#' @param plan Simplan plan.
#' @param node Name of the node.
#' @param plan_dt Data-table representation of the simplan.
#' @param add_nsims Add the n-sims directly.
#' @import data.table
construct_r_forloop.Internal <- function(plan, plan_dt, node, add_nsims = FALSE){
  code <- "__NAME__[[iter]] <- r__DISTRIBUTION__(1, __PARAMS__)"
  if(!is.null(plan_dt[plan_dt$name == node,"contortion"][[1]][[1]])){
    contortion <- plan_dt[plan_dt$name == node,"contortion"][[1]][[1]][[1]]
    if(contortion$type == "truncation"){
      code = "__NAME__[[iter]] <- rtrunc(
      n = 1,
      spec = '__INPUT_DIST__', __CONTORT_PARAMS__,__PARAMS__)"
    }
  }

  dist_name <- r_dists[plan$nodes[[node]]$distribution]
  PARAMS <- plan$nodes[[node]]$parameters
  # PARAMS <- lapply(PARAMS, function(x){if(length(x) == 1){x}else{paste0(x,"[[iter]]")}})
  inputs <- name <- NULL
  for(i in 1:length(PARAMS)){
    # if(grepl(PARAMS[[i]], pattern = "^(VECTOR|MATRIX)")){
      PARAMS[[i]] <- translate_arrays(PARAMS[[i]])
    # }
  }
  PARAMS <- paste0(paste0(names(PARAMS)," = ",PARAMS),collapse  =", ")

  nodes <- unique(plan_dt[plan_dt$name == node,]$inputs[[1]])
  nodes <- nodes[nodes %in% plan_dt$name]
  PARAMS <- add_iters(
    string = PARAMS,
    inputs = nodes)
  code <- sub("__NAME__",node,code)
  code <- sub("__DISTRIBUTION__",dist_name,code)
  code <- sub("__INPUT_DIST__",dist_name,code)
  if(!add_nsims){
    code <- sub("__NSIMS__",plan$n_runs,code)
  }else{
    code <- sub("__NSIMS__","n_runs",code)
  }
  code <- sub("__PARAMS__",PARAMS,code)

  # Add contortion parameters (if required)
  if(!is.null(plan_dt[plan_dt$name == node,"contortion"][[1]][[1]])){
    contortion <- plan_dt[plan_dt$name == node,"contortion"][[1]][[1]][[1]]
    if(contortion$type == "truncation"){
      CONTORT_PARAMS <- contortion$parameters
      inputs <- name <- NULL
      for(i in 1:length(CONTORT_PARAMS)){
        # if(grepl(PARAMS[[i]], pattern = "^(VECTOR|MATRIX)")){
        CONTORT_PARAMS[[i]] <- translate_arrays(CONTORT_PARAMS[[i]])
        # }
      }
      CONTORT_PARAMS <- paste0(paste0(names(CONTORT_PARAMS)," = ",CONTORT_PARAMS),collapse  =", ")

      CONTORT_PARAMS <- add_iters(
        string = CONTORT_PARAMS,
        inputs = unique(plan_dt[plan_dt$name == node,]$inputs[[1]]))
      CONTORT_PARAMS <- sub(CONTORT_PARAMS,pattern = "minimum = ", replacement = "a = ")
      CONTORT_PARAMS <- sub(CONTORT_PARAMS,pattern = "maximum = ", replacement = "b = ")
      code <- sub("__CONTORT_PARAMS__",CONTORT_PARAMS,code)
    }
  }

  code
}

