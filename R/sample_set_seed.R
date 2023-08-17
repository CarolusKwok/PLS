#' Set seed before random selection
#'
#' Do once only!
#'
#' @param seed Seed for the pseudo-random selection. By default (`NULL`), seed will be selected based on `Sys.time()`
#'
#' @return Random seed set
#' @export
#'
#' @examples set_seed()
set_seed = function(seed = NULL){
  set.seed(seed = seed)
}
