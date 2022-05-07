translate_sequence_initial_node.Internal <- function(plan, chunk_id, node_name){

  index <- NULL

  details <- plan$nodes[which(plan$nodes$name == node_name)]

  code_template <- "__NAME__[[iter]][__ITERATOR__] <- r__DISTRIBUTION__(1, __PARAMS__)"
  initials <- details$initials[[1]]
  initials_dt <- data.table::data.table(
    index = sapply(initials,FUN = function(x){x$index}),
    distribution = sapply(initials,FUN = function(x){x$distribution}),
    parameters = lapply(initials,FUN = function(x){x$parameters})
  )
  initial_code <- c()
  for(i in 1:initials_dt[,max(index)]){
    code <- code_template
    if(!is.null(initials[[i]][["contortion"]])){
      contortion <- initials[[i]][["contortion"]]
      if(contortion$type == "truncation"){
        code = "__NAME__[[iter]][__ITERATOR__] <- rtrunc(
      n = 1,
      spec = '__INPUT_DIST__', __CONTORT_PARAMS__,__PARAMS__)"
      }
    }

    dist_name <- r_dists[initials[[i]][["distribution"]]]
    PARAMS <- initials[[i]][["parameters"]]
    # PARAMS <- lapply(PARAMS, function(x){if(length(x) == 1){x}else{paste0(x,"[[iter]]")}})
    inputs <- name <- NULL
    for(j in 1:length(PARAMS)){
      # if(grepl(PARAMS[[i]], pattern = "^(VECTOR|MATRIX)")){
      PARAMS[[j]] <- translate_arrays(PARAMS[[j]])
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
    code <- sub("__ITERATOR__",initials[[i]]$index, code)
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
    initial_code <- c(initial_code, code)

  }
  initial_code
}

translate_sequence_initials.Internal <- function(plan, chunk_id){

  id <- NULL
  # For each node in the chunk:
  chunk_details <- plan$compute_chunks[id == chunk_id,]
  nodes <- chunk_details$nodes[[1]]
  suborder <- chunk_details$compute_suborder[[1]]

  initial_codes <- lapply(
    nodes,
    translate_sequence_initial_node.Internal,
    plan = plan,
    chunk_id = chunk_id
  )

  max_length <- max(sapply(initial_codes, length))
  initials_code_list <- c()
  for(i in 1:max_length){
    for(j in 1:max(suborder)){
      node_curr <- nodes[which(suborder == j)]
      node_ind <- which(suborder == j)
      if(i == 1){
        initials_code_list[[length(initials_code_list)+1]] <- as.character(glue::glue("{node_curr}[[iter]] <- numeric()"))
      }
      if(length(initial_codes[[node_ind]]) >= i){
        initials_code_list[[length(initials_code_list)+1]] <-
          initial_codes[[node_ind]][[i]]
      }
    }
  }

  initials_code_list
}
