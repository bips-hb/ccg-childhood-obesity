#' Spread a key-value pair across multiple columns for many values
#'
#' @param df  A data frame
#' @param key  Column names or position.
#' @param value Variable names to be spread
#'
#' @export
#'
#' @examples
#' tmp_1 <- spread_with_many_values(ui, survey, c(energy_day_exmr, prot_day_exmr))
#'
spread_with_many_values <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)

  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)

  df %>% gather(variable, value, !!!s) %>%
    unite(temp, variable, !!keyq) %>%
    spread(temp, value)
 }

