#' Add iteration indexing for for loops.
#' @param string string of parameters and values
#' @param inputs vector of names of input nodes.
add_iters <- function(string, inputs){

  for(i in inputs){
    string <- gsub(pattern = paste0("(",i,")",expression_components),
        replacement = "\\1[[iter]]\\2",
        x = string)
  }
  string
}


expression_components <- paste0(
  "(",
  paste0(c("!","&","\\,",
           "\\|","\\^","[[:alnum:]]*\\(","\\)",
           "\\{","\\}","\\[","\\]",
           "-",
           "\\+",
           "\\*","\\s","\\,",":","%","$"),collapse = "|"),")")
