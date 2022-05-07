#' Read in a simplan from a YAML file
#' @param plan unbaked plan.
#' @export
bake_plan <- function(plan){

  id <- type <- name <- NULL

  # Convert to data.table
  nodes_dt <- plan$nodes

  nodes <- nodes_dt$name
  nodes_dt$inputs <- lapply(nodes_dt$name, FUN = get_direct_inputs,
                            nodes_dt = nodes_dt)
  node_inputs <- nodes_dt$inputs
  names(node_inputs) <- nodes_dt$name
  nodes_dt$rank <- get_node_rank(node_inputs = node_inputs,nodes = nodes_dt$name)

  simplan_code_chunks <- create_compute_chunks(nodes_dt)
  simplan_code_chunks[,`:=`(rank = nodes_dt[name %in% nodes[[1]], min(rank)]),id]
  simplan_code_chunks[,`:=`(rank = rank(rank))]
  simplan_code_chunks[,`:=`(type = data.table::fcase(
    all(nodes_dt[name %in% nodes[[1]],type == "SEQUENCE"]), "SEQUENCE",
    default = "SIMPLE"
  )),id]

  # Determine additional packages required for R translation:
  contortion <- NULL
  contortion_types <- sapply(
    nodes_dt[sapply(contortion,function(x) length(x) == 1),contortion],
    function(x){x[[1]]$type})
  contortion_packages <- c()
  if("truncation" %in% contortion_types){
    contortion_packages <- c(
      contortion_packages,
      "truncdist"
    )
  }

  return(
    list(
      baked = TRUE,
      r_packages = c(contortion_packages),
      n_runs = plan$n_runs,
      nodes = nodes_dt,
      compute_chunks = simplan_code_chunks
    )
  )
}
