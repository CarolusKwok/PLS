#' Sampling based on categories presented
#'
#' @param data Choices in dataframe, consisting of `category` and `word`
#' @param seed Seed for the pseudo-random selection. By default (`NULL`), seed will be selected based on `Sys.time()`
#' @param force Can the interview reject the choice? By default (`FALSE`), the interviewee *can* reject the choice
#'
#' @return A vector of 2 words, as follow
#' * Category
#' * Word
#' @keywords internal
#'
#' @examples sample_categorized(data, seed = 1)
sample_categorized = function(data, seed = NULL, force = FALSE){
  flag = TRUE
  if(is.null(seed)){seed = as.numeric(Sys.time())}

  #Find the categories ####
  data_category = dplyr::group_by(.data = data, category) %>%
    dplyr::summarise(count = dplyr::n())

  #Select the category ####
  cli::cli_text("Select your category")
  print(data_category)

  ###Select category ####
  while(flag){
    input = PLS:::enter_integer(range = c(1, nrow(data_category)))
    cat = data_category$category[input]
    cli::cli_text("> Selection: {.strong {.code {cat}}}")
    if(PLS:::enter_confirm()){flag = FALSE}
  }

  ###Select the word based on selected category####
  choice = PLS:::sample_random(data = dplyr::filter(.data = data, category == cat), seed = seed, force = force)
  return(invisible(choice))
}
