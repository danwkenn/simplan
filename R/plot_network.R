

#' Extract the edges between nodes for visualising:
#' @param plan A baked simplan
extract_edges <- function(plan){

  name <- NULL

  # Create a list of edges:
  edges <- data.frame(
    start = character(), finish = character()
  )

  for(i in 1:nrow(plan$nodes)){
    curr_inputs <- plan$nodes$inputs[[i]]
    var_inputs <- plan$nodes[,which(name %in% curr_inputs)]

    edges <- rbind(
      edges,
      data.frame(
        start = var_inputs,
        finish = rep(i,length(var_inputs))
      )
    )
  }

  return(edges)
}


#' Get the node heirarchy for visualising:
#' @param plan A baked simplan
determine_variable_heirarchy <- function(plan){

  edges<- extract_edges(plan)

  # Determine order:
  # Determine order:
  all_ids <- 1:nrow(plan$nodes)
  ids <- all_ids
  full_dependencies <- edges
  dependencies <- edges
  dependencies <- dependencies[dependencies$start != dependencies$finish,]
  points <- rep(0,length(all_ids))

  inc <- 0
  while(nrow(dependencies) > 0){
    inc <- inc + 1
    lowest_level <- ids[(!ids %in% dependencies$finish)]

    points[which(!(all_ids %in% lowest_level) & all_ids %in% ids)] <-
      points[which(!(all_ids %in% lowest_level) & all_ids %in% ids)] + inc

    dependencies <- dependencies[!(dependencies$start %in% lowest_level),]
    ids <- ids[!(ids %in% lowest_level)]
  }

  as.numeric(factor(-rank(points)))
}

#' Plot the directed acyclic graph of the nodes.
#' @param plan A baked simplan
#' @import ggplot2
#' @export
plot_simulation_DAG <- function(plan){

  data <- x_pos <- name <- x_pos_finish <- NULL
  height_finish <- line_length <- x_pos_adj <- NULL
  height_adj <- x_pos_finish_adj <- height <- NULL
  height_finish_adj <- node_id <- NULL

  node_data <- data.table::setDT(data.frame(
    node_id = 1:nrow(plan$nodes),
    name = plan$nodes$name,
    height = determine_variable_heirarchy(plan)
  ))
  node_data[,`:=`(x_pos = 1:.N - .N/2),by = list(height)]
  node_data$height <- node_data$height / max(node_data$height) * (diff(range(node_data$x_pos)))
  ggplot2::ggplot() +
    ggplot2::geom_text(data = node_data, ggplot2::aes(x = x_pos, y = height, label = name))

  edge_data <- extract_edges(plan)
  edge_data <- node_data[edge_data, on = c(node_id = 'start')]
  edge_data[,`:=`(start = node_id)]
  edge_data <- node_data[edge_data, on = c(node_id = 'finish')]
  edge_data[,`:=`(finish = node_id)]
  names(edge_data) <- sub("^i\\.(.*)$","\\1_finish",names(edge_data))
 data.table::setDT(edge_data)
  edge_data[,`:=`(
    line_middle_x = 0.5 * (x_pos + x_pos_finish),
    line_middle_y = 0.5 * (height + height_finish),
    line_length = sqrt((x_pos - x_pos_finish)^2 + (height - height_finish)^2)
  )]

  edge_data[,`:=`(
    x_pos_adj = x_pos - 0.03 * (x_pos - x_pos_finish)/line_length,
    height_adj = height - 0.03 * (height - height_finish)/line_length,
    x_pos_finish_adj = x_pos_finish + 0.03 * (x_pos - x_pos_finish)/line_length,
    height_finish_adj = height_finish + 0.03 * (height - height_finish)/line_length
  )]

  ggplot() +
    geom_label(data = node_data, aes(x = x_pos, y = height, label = name)) +
    geom_segment(data = edge_data, aes(x = x_pos_adj, y = height_adj,
                                       xend = x_pos_finish_adj,yend = height_finish_adj),
                 arrow = ggplot2::arrow(ends = "first")) +
    coord_equal() +
    theme_void()
}

#' Visualise the directed acyclic graph of the nodes.
#' @param plan A baked simplan
#' @export
visualise_node_network <- function(plan){

  name <- start <- finish <- arrows <- NULL

  temp_d <- tempdir()


  # for(i in 1:nrow(plan$nodes)){
  #   p <- ggplot() + geom_density(
  #     aes(x = plan$nodes[[i]]),
  #     fill = "blue",
  #     size = 0.1) +
  #     theme_void()
  #   ggsave(filename = paste0(temp_d,"/network-density-icon-",i,".png"),plot =  p,width = 0.2,height = 0.2)
  # }
  #
  # nodes <- plan$nodes %>% rename(label = name) #%>%
  nodes <- data.table::copy(plan$nodes)
  data.table::setnames(nodes, "name", "label")
    # mutate(title = paste0(
    #   "<p><strong>Node <em>",
    #   label,
    #   "</em></strong></p>"),
    #   shape = "image",
    #   image = paste0(temp_d,"/network-density-icon-",id,".png"))
    #
  nodes <- nodes# %>% group_by(id) %>%
    # mutate(
    #   image = paste('data:image/png;base64',
    #                 RCurl::base64Encode(readBin(image,
    #                                             'raw',
    #                                             file.info(image)[1, 'size']),
    #                                     'txt'), sep = ',')
    # ) %>% ungroup()
    #
  edges <- data.table::setDT(extract_edges(plan))
  nodes$id <- 1:nrow(nodes)
  # edges <- edges %>%
  #   rename(from = start, to = finish) %>%
  #   mutate(arrows = "to")
  data.table::setnames(edges , old = c("start","finish"), new = c("from", "to"))
  edges[,arrows := "to"]
  netw <- visNetwork::visNetwork(nodes, edges, width = "100%")
  netw <- visNetwork::visNodes(netw, shapeProperties = list(useBorderWithImage = TRUE))
  netw <- visNetwork::visLayout(netw,randomSeed = 12)
  netw
}


# determine_draw_order <- function(plan){
#
#   edges<- extract_edges(plan)
#   # Determine order:
#   all_ids <- 1:nrow(plan$nodes)
#   ids <- all_ids
#   full_dependencies <- edges
#   dependencies <- edges
#   dependencies <- dependencies[dependencies$start != dependencies$finish,]
#   points <- rep(0,length(all_ids))
#
#   inc <- 0
#   while(nrow(dependencies) > 0){
#     inc <- inc + 1
#     lowest_level <- ids[(!ids %in% dependencies$finish)]
#
#     points[which(!(all_ids %in% lowest_level) & all_ids %in% ids)] <-
#       points[which(!(all_ids %in% lowest_level) & all_ids %in% ids)] + inc
#
#     dependencies <- dependencies[!(dependencies$start %in% lowest_level),]
#     ids <- ids[!(ids %in% lowest_level)]
#   }
#   draw_order <- order(points)
#   draw_order
# }

