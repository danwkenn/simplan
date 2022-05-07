#' Read in a simplan from a YAML file
#' @param file YAML file with a simplan.
#' @export
read_simplan <- function(file){

  id <-name <- type <- starts <- NULL
  # Read in YAML
  simplan_yml <- yaml::read_yaml(file)

  # Convert to data.table
  nodes_dt <- data.table::data.table(
    name = names(simplan_yml$nodes),
    type = sapply(simplan_yml$nodes,
                  FUN = function(x){
                    if(is.null(x[['type']])){
                      "SIMPLE"}else{
                      x[["type"]]
                    }}),
    distribution = sapply(simplan_yml$nodes,function(x){x[["distribution"]]}),
    parameters = lapply(simplan_yml$nodes,function(x){x[["parameters"]]}),
    sequence_parameters = lapply(simplan_yml$nodes,function(x){
      list(length = x[["length"]],
           index = x[["index"]])}),
    contortion = lapply(simplan_yml$nodes,function(x){x[["contortion"]]}),
    initials = lapply(simplan_yml$nodes,function(x){x[["initials"]]})
    )

  return(
    list(
      baked = FALSE,
      n_runs = simplan_yml$n_runs,
      nodes = nodes_dt
    )
  )
}
