#' for each input get the count of dependent nodes.
#' @param plan_dt `data.table` object describing a simplan.
#' @param node Name of the node.
add_count <- function(plan_dt, node){
  inputs <- unique(plan_dt[plan_dt$name == node,"inputs"][[1]][[1]])
  inputs <- inputs[inputs %in% plan_dt$name]
  if(length(inputs) == 0){
    return(0)
  }else{
    return(
      sum(sapply(FUN = add_count, X = inputs, plan_dt = plan_dt))+1
    )
  }
}
