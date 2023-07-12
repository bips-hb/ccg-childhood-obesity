#' Recode food variables
#'
#' @param x food variable from FFQ to be recoded
#'
#' @return
#' @export
#'
#' @examples
#' recodeFood(fruit_1)
#' recodeFastsnack(fastsnack_t1)
#'
recodeFood <- function(x)
{
  new <- vector(length = length(x), mode = "integer")
  new[x == 1] <- 0
  new[x == 2] <- 2
  new[x == 3] <- 5
  new[x == 4] <- 7
  new[x == 5] <- 14
  new[x == 6] <- 21
  new[x == 7] <- 30
  new[x == 8] <- NA
  new[is.na(x)] <- NA
  x <- new
}


#' @export
recodeFastsnack <- function(x){
  new <- vector(length = length(x), mode = "numeric")
  new[x == 1] <- 0
  new[x == 2] <- 0.2
  new[x == 3] <- 0.8
  new[x == 4] <- 1.5
  new[x == 5] <- 4
  new[is.na(x)] <- NA
  x <- new
}
