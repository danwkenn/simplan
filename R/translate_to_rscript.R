#' Translate the result into R code:
#' @param plan Simplan object
#' @param print_file File path to print to.
#' @export
translate_to_rscript <- function(plan, print_file){
plan$compute_chunks$comment <- paste0(
  "# ", plan$compute_chunks$rank,
  ". ", plan$compute_chunks$id," ----------------")
plan$nodes$instantiate <- paste0(plan$nodes$name," <- list()")

lines <- list()

if(any(sapply(r_dists[plan$nodes$distribution], function(x){x %in% simplan_dists}))){
  lines[[length(lines)+1]] <- ("# Simplan Distributions required ---------------------")
  lines[[length(lines)+1]] <- ("library(simplan)")
}

if(length(plan$r_packages) > 0){
  for(pack in plan$r_packages){
  lines[[length(lines)+1]] <- paste0("library(",pack,")")
  }
}

lines[[length(lines)+1]] <- ("# Set the number of monte carlo simulations -------------------")
lines[[length(lines)+1]] <- (paste0("n_runs = ", plan$n_runs))


lines[[length(lines)+1]] <- ("# Instantiate the nodes -------------------")
lines[[length(lines)+1]] <- paste0(
  "nodes_list <- c(",
  paste0(paste0("\"",plan$nodes$name,"\""),collapse = ", "),
  ")"
)
for(i in 1:nrow(plan$nodes)){
  lines[[length(lines)+1]] <- (plan$nodes$instantiate[i])
}

lines[[length(lines)+1]] <- "for(iter in 1:n_runs){"
node_calc_lines <- list()
for(i in 1:nrow(plan$compute_chunks)){
  ind <- which(plan$compute_chunks$rank == i)
  if(plan$compute_chunks[ind,"type"] == "SEQUENCE"){
    node_calc_lines[[ind]] <- c(
      plan$compute_chunks[ind,"comment"],
      translate_sequence_initials.Internal(
        plan,
        chunk_id = plan$compute_chunks[ind,"id"][[1]]),
      translate_sequence.Internal(
      plan = plan,
      chunk_id = plan$compute_chunks[ind,"id"][[1]]))
  }else{
    node_calc_lines[[ind]] <- translate_simple_chunk.Internal(
      plan = plan,
      id = plan$compute_chunks[ind,"id"][[1]])
  }
}
lines[[length(lines)+1]] <- node_calc_lines
lines[[length(lines)+1]] <- "}"
sink(file = print_file)
cat(paste0(convert_list_to_lines.Internal(lines),collapse = "\n"))
sink(file = NULL)
}

