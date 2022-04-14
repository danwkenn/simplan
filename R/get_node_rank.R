#' Recursively determine the number of inputs (direct and indirect) into a node.
#' @param node Node in question
#' @param node_inputs A list of character vectors with each of the inputs into the node.
#' @param nodes Names of the nodes in the list.
#' @param remove_selfreference Remove self-references?
get_node_count.Internal <- function(
  node,
  node_inputs,
  nodes = names(node_inputs),
  remove_selfreference = TRUE){

  direct_inputs <- unique(intersect(node_inputs[[node]], nodes))
  if(remove_selfreference){direct_inputs <- setdiff(direct_inputs, node)}
  if(length(direct_inputs) == 0){
    return(1)
  }else{
    result <- sum(sapply(direct_inputs,
                         FUN = get_node_count.Internal,
                         node_inputs = node_inputs,
                         nodes = nodes))+1
    return(result)
  }

}


name_grep_pattern <- "(?:\\.|[a-z,A-Z])_*(?:\\.|_|[a-z,A-Z,0-9])*"



#' Determine the build order for a set of nodes
#'
#' @param node_inputs A list of character vectors with each of the inputs into the node.
#' @param nodes Names of the nodes in the list.
#' @examples
#' node_inputs <- list(
#'   alpha = c("pi","f", "alpha"),
#'   beta = c("alpha","epsilon"),
#'   delta = c("d","beta"),
#'   epsilon = c("2")
#' )
#' get_node_rank(node_inputs)
get_node_rank <- function(node_inputs, nodes = names(node_inputs)){
  rank(sapply(nodes,
              FUN = get_node_count.Internal,
              node_inputs = node_inputs,
              nodes = nodes),ties.method = "first")
}


