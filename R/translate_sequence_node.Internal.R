#' Construct R code for an expression.
#' @param plan Simplan plan.
#' @param chunk_id Computing Chunk ID
#' @param node_name Name of the node.
#' @import data.table
translate_sequence_node.Internal <- function(plan, chunk_id, node_name){

  details <- plan$nodes[which(plan$nodes$name == node_name)]
  max_initial_ind <- length(details$initials[[1]])
  code <- "if(__ITERATOR__ > __MAXINIT__){__NAME__[[iter]][__ITERATOR__] <- r__DISTRIBUTION__(1, __PARAMS__)}"
  if(!is.null(details[1,"contortion"][[1]][[1]])){
    contortion <- details[1,"contortion"][[1]][[1]][[1]]
    if(contortion$type == "truncation"){
      code = "if(__ITERATOR__ > __MAXINIT__){__NAME__[[iter]][__ITERATOR__] <- rtrunc(
      n = 1,
      spec = '__INPUT_DIST__', __CONTORT_PARAMS__,__PARAMS__)}"
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

  PARAMS <- add_iters(
    string = PARAMS,
    inputs = plan$nodes$inputs[which(plan$nodes$name == node_name)][[1]])
  code <- sub("__NAME__",node_name,code)
  code <- sub("__DISTRIBUTION__",dist_name,code)
  code <- sub("__INPUT_DIST__",dist_name,code)
  code <- sub("__NSIMS__","n_runs",code)
  code <- sub("__PARAMS__",PARAMS,code)
  code <- gsub("__ITERATOR__",chunk_id, code)
  code <- sub("__MAXINIT__",max_initial_ind, code)

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

  code
}

#' Internal function for converting sequence chunks.
#' @param plan Simplan plan.
#' @param chunk_id Computing Chunk ID
#' @export
translate_sequence.Internal <- function(plan, chunk_id){

  id <- name <- NULL
  # For each node in the chunk:
  chunk_details <- plan$compute_chunks[id == chunk_id,]
  nodes <- chunk_details$nodes[[1]]
  suborder <- chunk_details$compute_suborder[[1]]

  exprs <- character()
  for(i in 1:length(suborder)){

    # Identify node:
    curr_node <- nodes[which(suborder == i)]

    exprs[length(exprs) + 1] <- glue::glue("# {curr_node}-----------------")
    exprs[length(exprs) + 1] <- translate_sequence_node.Internal(
      plan,chunk_id, node_name = curr_node)

  }

  first_line <- "for(__ITERATOR__ in 1:__LENGTH__){"
  first_line <- sub("__ITERATOR__", chunk_details$id, first_line)
  first_line <- sub("__LENGTH__",
                    plan$nodes[name == chunk_details$nodes[[1]][1],]$
                      sequence_parameters[[1]]$
                      length,
                    first_line)
  result <- list(
    first_line,
    exprs,
    "}"
  )

  return(result)
}
