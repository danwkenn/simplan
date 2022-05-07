#' Create a basic plot for a sequence
#' @param node Node to plot
#' @param plan Simplan object
#' @param result_type Unused currently.
plot_node_sequence_raw_base <- function(node,plan,result_type = NULL){

  name <- type <- NULL
  node_type <- plan$node[name == node, type]

  node_samples <- get(node)

  # Determine the max and min values.
  range <- c(
    min(sapply(node_samples, min)),
    max(sapply(node_samples, max))
  )

  x_range <- c(
    1,
    max(sapply(node_samples, length))
  )

  graphics::plot.new()
  graphics::plot.window(xlim = x_range, ylim = range)
  graphics::axis(2)
  graphics::axis(1)
  for(i in seq.int(
    1,
    length(node_samples),
    length.out = length(node_samples))){
    graphics::lines(x = seq.int(
        1,
        length(node_samples[[i]]),
        length.out = length(node_samples[[1]])),
        y = node_samples[[i]])
  }
  graphics::title(node,xlab = "Sequence Iteration",
        ylab = "Value")
}

#' Create a basic plot for a sequence
#' @param node Node to plot
#' @param plan Simplan object
#' @param result_type Unused currently.
plot_node_simple_raw_base <- function(node,plan,result_type = NULL){

  node_type <- plan$node[name == node, type]

  node_samples <- get(node)

  name <- type <- NULL
  # Determine the max and min values.
  range <- c(
    min(sapply(node_samples, min)),
    max(sapply(node_samples, max))
  )

  x_range <- c(
    1,
    max(sapply(node_samples, length))
  )

  if(max(x_range) == 1){
    graphics::hist(unlist(node_samples),breaks = min(10, sqrt(length(node_samples))),
         main = node,
         xlab = c("Value"))
  }else{
    hist_objs <- lapply(
      1:x_range[2],
      FUN = function(x){
        res <- sapply(node_samples, FUN = function(y){y[x]})
        res <- res[!is.na(res)]
        hists <- graphics::hist(res, breaks = max(10, sqrt(length(res))),plot = FALSE)
        hists$xname <- paste0(node,"[[",x,"]]")
        hists
      })
    lapply(hist_objs,plot)
  }
}

#' Create a basic plot for a sequence
#' @param plan Simplan object
#' @param result_type Unused currently.
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' @param facet Facet the plots or single plots?
#' @export
plot_nodes_raw_base <- function(plan,result_type = NULL, nrow = 2,ncol = 2, facet = TRUE){
  name <- type <- NULL
  n_nodes <- nrow(plan$nodes)

  if(facet){
  graphics::par(mfrow = c(nrow,ncol))
  }
  for(i in seq.int(1,n_nodes,length.out = n_nodes)){
    node <- plan$nodes[i, name]
    if(plan$nodes[i, type == "SEQUENCE"]){
      plot_node_sequence_raw_base(node,plan,result_type = NULL)
    }else{
    plot_node_simple_raw_base(node,plan,result_type = NULL)
    }
  }

}
