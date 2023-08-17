#' Tools: Enter integer
#'
#' @param range a vector with 2 numbers
#'
#' @return An integer
#' @keywords internal
#'
#' @examples enter_integer()
enter_integer = function(range = c(-Inf, Inf)){
  flag = TRUE
  while(flag){
    input = suppressWarnings(as.integer(readline(prompt = "Enter number: ")))
    if(!is.na(input)){
      if(is.integer(input) & min(range) <= input & input <= max(range)){
        flag = FALSE
      }
    }
  }
  return(invisible(input))
}
