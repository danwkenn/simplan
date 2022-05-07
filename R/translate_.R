translate_simple_chunk_expr.Internal <- function(plan, id){

  nodes <- NULL

  node_name <- plan$compute_chunks[which(plan$compute_chunks$id == id),nodes[[1]]]
  details <- plan$nodes[which(plan$nodes$name == node_name)]
  code <- "__NAME__ <- r__DISTRIBUTION__(__NSIMS__, __PARAMS__)"
  dist_name <- r_dists[details$distribution]
  PARAMS <- details$parameters[[1]]
  PARAMS <- paste0(paste0(names(PARAMS)," = ",PARAMS),collapse  =", ")
  code <- sub("__NAME__",node_name,code)
  code <- sub("__DISTRIBUTION__",dist_name,code)
  code <- sub("__NSIMS__","n_runs",code)
  code <- sub("__PARAMS__",PARAMS,code)
  code
}

#' Construct R code for an expression.
#' @param plan Simplan plan.
#' @param id Add the n-sims directly.
#' @import data.table
translate_simple_chunk.Internal <- function(plan, id){

  node_name <- plan$compute_chunks$nodes[plan$compute_chunks$id == id][[1]]
  details <- plan$nodes[which(plan$nodes$name == node_name)]

  code <- "__NAME__[[iter]] <- r__DISTRIBUTION__(1, __PARAMS__)"
  if(!is.null(details[1,"contortion"][[1]][[1]])){
    contortion <- details[1,"contortion"][[1]][[1]][[1]]
    if(contortion$type == "truncation"){
      code = "__NAME__[[iter]] <- rtrunc(
      n = 1,
      spec = '__INPUT_DIST__', __CONTORT_PARAMS__,__PARAMS__)"
    }
  }

  dist_name <- r_dists[details$distribution]
  PARAMS <- details$parameters[[1]]
  # PARAMS <- lapply(PARAMS, function(x){if(length(x) == 1){x}else{paste0(x,"[[iter]]")}})
  inputs <- name <- NULL
  for(i in 1:length(PARAMS)){
    # if(grepl(PARAMS[[i]], pattern = "^(VECTOR|MATRIX)")){
    PARAMS[[i]] <- translate_arrays(PARAMS[[i]])
    # }
  }
  PARAMS <- paste0(paste0(names(PARAMS)," = ",PARAMS),collapse  =", ")

  nodes <- plan$nodes[name == node_name, inputs[[1]]]
  PARAMS <- add_iters(
    string = PARAMS,
    inputs = nodes)
  code <- sub("__NAME__",node_name,code)
  code <- sub("__DISTRIBUTION__",dist_name,code)
  code <- sub("__INPUT_DIST__",dist_name,code)
  code <- sub("__NSIMS__","n_runs",code)
  code <- sub("__PARAMS__",PARAMS,code)

  # Add contortion parameters (if required)
  if(!is.null(details[1,"contortion"][[1]][[1]])){
    contortion <- details[1,"contortion"][[1]][[1]][[1]]
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
        inputs = details$inputs[[1]])
      CONTORT_PARAMS <- sub(CONTORT_PARAMS,pattern = "minimum = ", replacement = "a = ")
      CONTORT_PARAMS <- sub(CONTORT_PARAMS,pattern = "maximum = ", replacement = "b = ")
      code <- sub("__CONTORT_PARAMS__",CONTORT_PARAMS,code)
    }
  }

  c(glue::glue("# {node_name}-----------------"),
    code)
}


