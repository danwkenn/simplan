convert_list_to_lines.Internal <- function(lines, level = 0){
  if(length(lines) == 1){
    result <- paste0(
      paste0(rep("  ",times = level),collapse = ""),
      unlist(lines,recursive = TRUE))
    return(result)
  }else{
    tmp <- c()
    for(i in 1:length(lines)){
      tmp[[i]] <- convert_list_to_lines.Internal(lines = lines[[i]], level = level + 1)
    }
    return(
      do.call("c",tmp)
    )
  }
}
