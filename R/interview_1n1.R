#' Create an 1+1 interview list
#'
#' A "1+1" interview refers to "1 chosen term, 1 non-chosen term". This is to ensure that interviewees are capable of handling non-major topics or seeking help.
#' The dataframe inserted within this function must contain 2 columns, "category" and "word".
#' This function also returns a dataframe, that consists of all the terms chosen by the interviewees.
#'
#' @param data Choices in dataframe, consisting of `category` and `word`
#' @param seed Seed for the pseudo-random selection. By default (`NULL`), seed will be selected based on `Sys.time()`
#' @param i List of Interviewee. By default (`NULL`), a pop-up will appear asking you how many interviewees there are, and will fill in accordingly as numbers.
#'
#' @return A dataframe, containing all terms choosen by the interviewees.
#' @export
#'
#' @examples interview_1n1(df)
interview_1n1 = function(data, i = NULL, seed = NULL){
  #Set global variables for loops
  flag = TRUE
  NumInterviewees = 0
  data_selections = tibble::tibble(Person = as.character(NA),
                                   Cat_1 = as.character(NA), Word_1 = as.character(NA),
                                   Cat_2 = as.character(NA), Word_2 = as.character(NA),
                                   .rows = 0)

  #Categorize algorithm ####
  categorize = function(data){
    data = dplyr::summarise(.data = dplyr::group_by(.data = data, category), count = dplyr::n())
    return(invisible(data))
  }

  #Check data input ####
  if(!hasArg(data)){return(invisible(cli::cli_warn(message = "Please input data into `data.`")))}
  if(!is.data.frame(data)){return(invisible(cli::cli_warn(message = "Please input dataframe into `data`.")))}
  if(!("category" %in% colnames(data))){
    return(invisible(cli::cli_warn(message = "Column `category` doesn't exist in `data`")))
  }
  if(!("word" %in% colnames(data))){
    return(invisible(cli::cli_warn(message = "Column `word` doesn't exist in `data`")))
  }

  #START ####
  ##Choose number of interviewees in this round ####
  cli::cli_text(cli::bg_yellow(cli::col_black("========== PLS word selection system ==========")))

  if(is.null(i)){
    cli::cli_text("How many interviewees in this round?")
    input = PLS:::enter_integer(range = c(1, Inf))
    i = (1:input)
    flag = T
  }

  i = as.character(i)
  ##Starting to ask interviewee for numbers ####
  for(j in i){
    if(is.null(seed)){seed = as.numeric(Sys.time())}

    #Categorize data and count how much word is in each category ####
    cli::cli_text(cli::bg_br_white(cli::col_black("> Interviewee {j} <")))

    ###Select category and choose word ####
    Choice_1 = PLS:::sample_categorized(data = data, seed = seed, force = FALSE)
    Cat_1 = Choice_1[1]
    Word_1 = Choice_1[2]

    ###Randomly select the word from list ####
    Choice_2 = dplyr::filter(.data = data, word != Word_1 & category != Cat_1) %>%
      PLS:::sample_random(seed = seed, force = TRUE)
    Cat_2 = Choice_2[1]
    Word_2 = Choice_2[2]

    ###Remove the word from `data` ####
    data = dplyr::filter(.data = data, !(word %in% c(Word_1, Word_2)))

    ###Format data_selection by binding ###
    tmp_selection = data.frame(Person = j,
                               Cat_1 = Cat_1, Word_1 = Word_1,
                               Cat_2 = Cat_2, Word_2 = Word_2)
    data_selections = dplyr::bind_rows(data_selections, tmp_selection)
    cli::cli_text("")
  }
  #Print out the data_selection
  cli::cli_text("{.empha {.strong Recall}}")
  print(data_selections)

  #Return data_selection
  rm(list = ls()[ls() != "data_selections"])
  return(invisible(data_selections))
}
