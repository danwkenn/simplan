#' Extract lags from a sequence expression:
#' @param expr Expression as a character string.
#' @param index_variable Variable name for indexing.
extract_lags_from_seq_expr <- function(expr, index_variable){

  if(!grepl(index_variable,expr,fixed = TRUE)){
    return(NULL)
  }

  starts <- gregexpr(pattern = paste0(name_grep_pattern,"\\["), text = expr)[[1]]
  lengths <- attr(x = starts, which = 'match.length')
  lags <- rep(NA, length.out = length(starts))
  names(lags) <- sapply(1:length(starts),function(i){
    substr(expr, start = starts[i], stop = starts[i] + lengths[i]-2)
  })

  for(i in seq.int(1,length(starts), length.out = length(starts))){
    remainder <- strsplit(substr(expr,starts[i] + lengths[i],nchar(expr)), split = character())[[1]]
    end_of_index <- which(
      cumsum(data.table::fcase(remainder == "]",-1,remainder == "[", +1, default = 0))==-1
    )[[1]] - 1
    indexing_expr <- sub("\\s","",paste0(remainder[1:end_of_index],collapse = ""))
    if(indexing_expr == index_variable){
      lags[i] <- 0
    }else{
      if(grepl(index_variable, indexing_expr)){
        lags[i] <- as.numeric(sub(paste0(index_variable,"-(\\d+)"),"\\1",x = indexing_expr))
      }
    }
  }
  lags
}

#' Extract all lags for a given node:
#' @param node name of the node
#' @param nodes_dt Node-set as a data.table.
#' @param index_variable Variable name for indexing.
extract_all_lags <- function(node,nodes_dt, index_variable){
  exprs <- extract_all_sequence_params(node, nodes_dt)
  lags <- (lapply(exprs, extract_lags_from_seq_expr, index_variable = index_variable))
  names(lags) <- NULL
  lags <- unlist(lags)
  lags
}

#' Extract the parameters for a sequence type variable
#' @param node Name of the sequence-type node.
#' @param nodes_dt Node-set as a data.table.
extract_all_sequence_params <- function(node, nodes_dt){
  name <- NULL
  initials <- nodes_dt[name == node,  initials][[1]]
  parameters <- nodes_dt[name == node,  parameters][[1]]
  c(
    unlist(lapply(nodes_dt[name == node,  initials][[1]],
                  function(x){x[["parameters"]]}),recursive = TRUE),
    unlist(lapply(nodes_dt[name == node,  initials][[1]],
                  function(x){x[["contortion"]]}),recursive = TRUE),
    unlist(parameters)
  )
}

#' Determine the set of interdependent sets of variables for computing.
#' @param nodes_dt Node set as data-table.
create_compute_chunks <- function(nodes_dt){

  type <- name <- index <- nodes <- NULL

simplan_compute_chunks <-
  nodes_dt[type %in% c("SIMPLE"),
           list(nodes = list(name),
             compute_suborder = list(1)),list(id = name)]

sequence_parameters <- compute_suborder <- name <- NULL

# Identify close interdependencies:
if(nodes_dt[type %in% c("SEQUENCE"),.N]> 0){
sequence_compute_chunks <- nodes_dt[type %in% c("SEQUENCE"),
         list(nodes = list(name)),
         list(index = sapply(sequence_parameters, function(x){x[["index"]]}))]

sequence_indices <- sequence_compute_chunks$index

for(i in seq.int(1,length(sequence_indices),length.out = length(sequence_indices))){

  # For each node we need a list of the input nodes and the lags.
  nodes_in_chunk <- sequence_compute_chunks[
    index == sequence_indices[[i]],
    nodes[[1]]]

  lags <- lapply(FUN = extract_all_lags, X = nodes_in_chunk,
         nodes_dt = nodes_dt,
         index_variable = sequence_indices[[i]])

  lags <- lapply(lags, function(x){x[names(x) %in% nodes_in_chunk]})
  names(lags) <- nodes_in_chunk

  # Get the max lag:
  max_lag <- max(sapply(lags,max))

  suborder <- rep(NA,length.out = length(nodes_in_chunk))
  names(suborder) <- nodes_in_chunk
  if(length(nodes_in_chunk) == 1){
    sequence_compute_chunks[
      index == sequence_indices[[i]],
      compute_suborder := list(1)]
  }else{
  lag_curr <- 0
  lag_penalty <- 0
  COMPLETE <- FALSE
  while(lag_curr <= max_lag){
  tmp <- lapply(lags, function(x){x[x == lag_curr]})
  tmp <- lapply(tmp, names)
  for(j in seq.int(1,length(tmp), length.out = length(tmp))){
    tmp[[j]] <- tmp[[j]][tmp[[j]] != names(tmp)[j]]
  }
  tmp <- tmp[sapply(tmp, function(x) length(x) > 0)]
  in_running <- unique(unlist(tmp))
  in_running <- in_running[(in_running %in% names(suborder[is.na(suborder)]))]
  if(!is.null(in_running)){
    ranks <- rank(get_node_rank(tmp,nodes = nodes_in_chunk)[in_running])
    suborder[names(ranks)] <- ranks + lag_penalty
    lag_penalty <-max(c(0,suborder), na.rm = TRUE)
  }
  lag_curr <- lag_curr + 1
  }
  if(any(is.na(suborder))){
    suborder[is.na(suborder)] <- 1:sum(is.na(suborder)) + max(suborder,na.rm = T)
  }

  sequence_compute_chunks[
    index == sequence_indices[[i]],
    compute_suborder := list(suborder)]
  }
}

simplan_compute_chunks <- rbind(simplan_compute_chunks,
                                sequence_compute_chunks[
                                  ,list(id = index, nodes, compute_suborder)])
}
simplan_compute_chunks
}
