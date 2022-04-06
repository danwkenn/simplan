#' Extract the variables from an expression.
#' @param x A character string representing an expression.
extract_variables.Internal <- function(x){
  x <- as.character(x)
  if(grepl(pattern = "^(\\d|\\.)+$",x)){
    return(NULL)
  }
  result <- trimws(strsplit(
    x = x,
    split = expression_components)[[1]])

  result <- result[nchar(result) > 0]
  result <- result[!grepl(pattern = "^(\\d|\\.)+$",result)]
  result
}

#' Extract the variables from multiple expressions and convert to a character vector.
#' @param x List of character strings representing expressions.
extract_variables_2.Internal <- function(x){do.call("c",
                                                    lapply(
                                                      x,
                                                      FUN = extract_variables.Internal))
}

