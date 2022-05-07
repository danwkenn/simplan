#' Add a node to a plan
#' @param plan Unbaked plan
#' @param name Name of the node.
#' @param type Node type.
#' @param distribution Probability distribution.
#' @param parameters Parameters of the probability distribution.
#' @param sequence_parameters Sequence parameters.
#' @param contortion Contortions.
#' @param initials Sequence initials.
#' @export
add_node <- function(
  plan,
  name,
  type = "SIMPLE",
  distribution,
  parameters,
  sequence_parameters = NULL,
  contortion = NULL,
  initials = NULL
){

  # Check if baked:
  if(plan$baked){
   plan <- unbake_plan(plan)
  }

  new_node <- data.table::data.table(
    name = name,
    type = type,
    distribution = distribution,
    parameters = list(parameters),
    sequence_parameters = list(sequence_parameters),
    contortion = list(contortion),
    initials = list(initials)
  )

  plan$nodes <- data.table::rbindlist(
    list(
      plan$nodes,
      new_node
    )
  )

  plan
}
