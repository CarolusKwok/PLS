#' Remove choices from list
#'
#' @param data Choices in dataframe, consisting of `category` and `word`
#' @param words Choices *to be removed* in dataframe, consisting of `category` and `word`
#'
#' @return A dataframe, without the choices in `words`
#' @keywords internal
#'
#' @examples remove_choice(data, words)
remove_choice = function(data, words){
  for(i in 1:nrow(words)){
    sel_cat = words$category[i]
    sel_word = words$word[i]
    data = dplyr::mutate(.data = data,
                         pass_cat = (category == sel_cat),
                         pass_word= (word == sel_word),
                         pass     = pass_cat & pass_word) %>%
      dplyr::filter(!pass)
  }
  return(invisible(data))
}
