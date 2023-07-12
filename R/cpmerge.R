#' Create Consumer Price Score
#'
#' @param daten dataset where the cpscore should be merged to
#' @param id  name of id variable
#' @param mergeby name of merge by variable
#' @param cp.daten name of dataset with consumer prices including year and country
#' @param cntry name of country variable
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' head(a <- cpmerge(idefics$core_t0, id = "id_no", mergeby = "year_t0"))
#' head(a <- cpmerge(idefics$core_t1, id = "id_no", mergeby = "year_t1",
#'           cntry = "country_t1"))
#' head(a <- cpmerge(idefics$core_t3, id = "ifam_no", mergeby = "year_t3"))
#' }

cpmerge <- function(daten, id, mergeby, cp.daten = cp, cntry = "country")
{
  daten <- subset(daten, select = c(id, cntry, mergeby))
  daten <- merge(daten, cp.daten,
               by.x = c(cntry, mergeby), by.y = c("country", "year"))

  daten$cpscore <- apply(as.matrix(daten[, 4:11]), 1, sum, na.rm = TRUE)
  daten[, c(3,12)]
}



