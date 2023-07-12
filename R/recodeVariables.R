#' Recoding of variables
#'
#' @param x variable to be recoded
#'
#' @return
#' @export
#'
#' @examples
#' recode71(homebreakf)
#' recode07(breakfast_T3)
#' recodeMedia(tv_dur_wd)
#'
# recode2
recode71 <- function(x){
  new <- vector(length = length(x), mode = "integer")
  new[x == 1] <- 7
  new[x == 2] <- 5
  new[x == 3] <- 2
  new[x == 4] <- 3
  new[x == 5] <- 1
  new[is.na(x)] <- NA
  x <- new
}


# recode3
#' @export
recode07 <- function(x){
  new <- vector(length = length(x), mode = "integer")
  new[x == 1] <- 0
  new[x == 2] <- 0.5
  new[x == 3] <- 1.5
  new[x == 4] <- 4.5
  new[x == 5] <- 7
  new[is.na(x)] <- NA
  x <- new
}


#' @export
recodeMedia <- function(x){
  new <- vector(length = length(x), mode = "numeric")
  new[x == 1] <- 0.25
  new[x == 2] <- 0.75
  new[x == 3] <- 1.5
  new[x == 4] <- 2.5
  new[x == 5] <- 2.5
  new[x == 6] <- 4
  new[is.na(x)] <- NA
  x <- new
}

