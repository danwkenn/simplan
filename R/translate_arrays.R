#' Identify the index of the closing bracket
#' @param x Character string with brackets.
get_first_bracket_end <- function(x){
  coding <- (sapply(X = strsplit(x,split = "")[[1]],FUN = switch,
                    `(` = 1,
                    `)` = -1,
                    0))
  which(cumsum(coding) == 0)[[1]]
}

array_types <- c("VECTOR","MATRIX", "DIAGONAL")

#' Identify all substrings of the form MATRIX|VECTOR(...)
#' @param string String of simplan code to be translated into R.
translate_arrays <- function(string){

  # string <- "2 * MATRIX((1,0),(0,1)) + MATRIX((2,0),(0,1))"
  while(grepl(pattern = paste0("(",paste0(array_types,collapse = "|"),")\\("), x = string)){
  pattern_1 <- gregexpr(
    pattern = paste0(
      "(",paste0(array_types,collapse = "|"),")\\("),
    text = string)[[1]]

  rest_of_string <- substr(
    string,
    start = pattern_1[1] + attr(pattern_1,"match.length")[[1]]-1,
    stop = nchar(string))

  first_array_substring <- paste0(
    substr(string,pattern_1[[1]],
           pattern_1[[1]]+attr(pattern_1,'match.length')[[1]]-2),
    substr(rest_of_string,
           start = 1,
           stop = get_first_bracket_end(rest_of_string))
  )

  replacement <- express_array_param(first_array_substring)
  string <- gsub(
    pattern = first_array_substring,
    replacement = replacement,
    x = string, fixed = TRUE)
  }
  string
}
