#' Create an 1+1 interview list
#'
#' A "1+1" interview refers to "1 chosen term, 1 non-chosen term". This is to ensure that interviewees are capable in handling non-major topics or seek help.
#' The dataframe inserted within this function must contain 2 columns, "category" and "word".
#' This function also returns a dataframe, that consist of all the terms choose by the interviewees.
#'
#' @param data_org A dataframe, containing column "category" and "word"
#'
#' @return
#' @export
#'
#' @examples interview_1_1(df)
interview_1_1 = function(data_org){
  suppressMessages(suppressWarnings(library(tidyverse)))

  #input data
  data = data_org
  if(!("category" %in% colnames(data))){
    return(message('ERROR: Column "category" not exist in data'))
  }
  if(!("word" %in% colnames(data))){
    return(message('ERROR: Column "word" not exist in data'))
  }

  #categorize data and count how much word is in each category
  data_category = data %>%
    select(category) %>%
    distinct() %>%
    mutate(count = NA)

  for(i in 1:nrow(data_category)){
    data_temp = data %>%
      filter(category == data_category$category[i])
    data_category$count[i] = nrow(data_temp)
  }

  #START
  message("Welcome to the PLS random word selection system")
  message("How many interviewees are within this round?")

  flag_int = F
  while(flag_int == F){
    interview_num = readline(prompt="Enter number: ")
    interview_num = as.integer(interview_num)
    if(!is.na(interview_num) & is.integer(interview_num)){
      flag_int = T
    }
  }
  flag_int = F

  interview_df = data.frame(Num        = 1:interview_num,
                            Word_1_cat = NA,
                            Word_1     = NA,
                            Word_2_cat = NA,
                            Word_2     = NA)

  for(i in 1:interview_num){
    message(paste("For interviewee", i))
    flag_confirm = F
    flag_int     = F

    #Ask for interviewee field selection
    while(flag_confirm == F){
      message(paste("Please choose a field of your selection"))
      print(data_category)
      while(flag_int == F){
        choose_num = readline(prompt = "Enter ROW number: ")
        choose_num = suppressWarnings(as.integer(choose_num))
        if(!is.na(choose_num) & is.integer(choose_num) & choose_num > 0 & choose_num <= nrow(data_category)){
          flag_int = T
        }
      }
      flag_int = F
      choose_cat = data_category$category[choose_num]
      message(paste("You have selected:", choose_cat))
      message("Are you sure?")
      sure = readline(prompt = "Enter Y/N: ")
      if(sure == "Y" | sure == "y"){
        flag_confirm = T
      }
    }
    flag_confirm = F

    interview_df$Word_1_cat[i] = choose_cat

    #Sample a random word that is from interviewee choice
    data_sample = data %>%
      filter(category == choose_cat)
    random_str = data_sample$word[sample(1:length(data_sample$word), 1)]

    message(paste0('The random word chosen from "', choose_cat, '" is .....'))
    cat("     ", random_str)

    interview_df$Word_1[i] = random_str

    #Remove used word from list
    data = data %>%
      filter(word != random_str)

    #Sample a random word that is NOT from interviewee choice
    data_sample = data %>%
      filter(category != choose_cat)
    random_str = data_sample$word[sample(1:length(data_sample$word), 1)]
    random_cat = data_sample %>%
      filter(word == random_str)
    random_cat = random_cat$category[1]
    cat("\n")
    message(paste0('The second word chosen is from "', random_cat, '". It is ....'))
    cat("     ", random_str)

    interview_df$Word_2_cat[i] = random_cat
    interview_df$Word_2[i]     = random_str

    #Remove used word from list
    data = data %>%
      filter(word != random_str)

    cat("\n")
    message("---===---===---===---===---===---")
    cat("\n")

    #Recount!
    for(i in 1:nrow(data_category)){
      data_temp = data %>%
        filter(category == data_category$category[i])
      data_category$count[i] = nrow(data_temp)
    }
  }

  message("RECALL")
  print(interview_df)
  invisible(interview_df)
}
