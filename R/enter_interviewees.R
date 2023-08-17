#' Tools: "How many interviewees?" Confirmation
#'
#' @return A list of numbers, from 1 to the designated amount
#' @keywords internal
#'
#' @examples enter_interviewee
enter_interviewee = function(){
  cli::cli_text("How many interviewees in this round?")
  input = PLS:::enter_integer(range = c(1, Inf))
  i = (1:input)
  return(i)
}
