#' Internal function for expressing heirarchical string lists as R list() objects.
#' @param string string
listify <- function(string){

  inside <- sub("^\\((.*)\\)$", "\\1", string)
  if(grepl(inside,pattern = "(^|,)\\(")){
    subs <- strsplit(inside, split = "),")[[1]]
    if(length(subs) == 1){
      stop("Redundant levels in heirarchical list. Check brackets.")
    }else{
      subs[1:(length(subs)-1)] <- paste0(subs[1:(length(subs)-1)], ")")
      return(lapply(X = subs, FUN = listify))
    }
  }else{
    return(paste0("c(",inside, ")"))
  }
}

#' Internal function for converting an R-list into an R-object of the specified type.
#' @param list R List object created by `listify()`.
#' @param type Type of simplan array (currently only matrix and vector accepted)).
combine <- function(list, type){
  switch(type,
         MATRIX = glue::glue("rbind({paste0(unlist(list), collapse = ',')})"),
         VECTOR = list[[1]])
}

#' Internal function for converting a simplan array string into a R-expression (still string.)
#' @param string string
express_array_param <- function(string){
  # Identify type.
  string_type <- sub("^([[:alnum:]]*)\\(.*$","\\1", string)

  string_main <- sub("^([[:alnum:]]*)(\\(.*)$","\\2", string)
  string_main <- gsub(x = string_main, "\\s", "")

  # Listify
  list <- listify(string_main)

  # Convert to R-friendly object.
  expr <- combine(list, type = string_type)
  expr
}
