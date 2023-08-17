#' Sampling based on pseudo-random selection
#'
#' @param data Choices in dataframe, consisting of `category` and `word`
#' @param force Can the interview reject the choice? By default (`FALSE`), the interviewee *can* reject the choice.
#'
#' @return A vector of 2 words, as follow
#' * Category
#' * Word
#' @keywords internal
#'
#' @examples sample_random(data, seed = 1)
sample_random = function(data, seed = NULL, force = FALSE){
  flag = TRUE
  while(flag){
    choice = sample(1:nrow(data), 1)
    cat = data$category[choice]
    word = data$word[choice]

    cli::cli_text('Random word from {.code {cat}}')
    cli::cli_text("> Word: {.strong {.code {word}}}")

    if(force){flag = FALSE} else if(PLS:::enter_confirm()){flag = FALSE}
  }
  return(invisible(c(cat, word)))
}
