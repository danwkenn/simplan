get_direct_inputs <- function(node, nodes_dt){
  name <- parameters <- contortion <- type <- NULL
  exprs <- unlist(nodes_dt[name == node, c(parameters[[1]])])
  if(!is.null(nodes_dt[name == node, contortion[[1]]])){
    exprs <- c(
      exprs,
      unlist(nodes_dt[name == node, "contortion"][[1]][[1]][[1]]$parameters)
    )
  }
  if(nodes_dt[name == node, type == "SEQUENCE"]){
    initials <- nodes_dt[name == node,"initials"][[1]][[1]]
    for(i in 1:length(initials)){
      exprs <- c(
        exprs,
        unlist(initials[[i]]$parameters)
      )
     }
  }
  names_in_expr <- lapply(exprs, function(x){
    unlist(regmatches(x = x,
                      gregexpr(pattern = name_grep_pattern,
                               text = x)))})
  names_in_expr <- do.call(c,names_in_expr)
  unique(names_in_expr[names_in_expr %in% nodes_dt$name])
}
