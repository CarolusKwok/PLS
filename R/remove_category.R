#' Remove choices from list
#'
#' @param data Choices in dataframe, consisting of `category` and `word`
#' @param category Category to be removed, in character
#'
#' @return A dataframe, without the choices in `words`
#' @keywords internal
#'
#' @examples remove_category(data, words)
remove_category = function(data, category){
  sel_category = category
  data = dplyr::filter(.data = data,
                       !(category %in% sel_category))
  return(invisible(data))
}
