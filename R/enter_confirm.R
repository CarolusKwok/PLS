#' Tools: "Are you sure?" Confirmation
#'
#' @return A logical value, with `TRUE` as "Yes" and `FALSE` as "No"
#' @keywords internal
#'
#' @examples enter_confirm
enter_confirm = function(){
  flag = TRUE
  while(flag){
    cli::cli_text(cli::style_bold("Are you sure?"))
    input = readline(prompt = "Enter y/Y/n/N: ")
    if(length(input) == 1 & sum(input %in% c("Y", "y", "N", "n"))){flag = FALSE}
  }
  input = (input %in% c("Y", "y"))
  return(invisible(input))
}
