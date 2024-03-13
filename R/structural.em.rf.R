#' Structure learning from missing data
#'
#' The \code{bnlear::\link[bnlearn]{structural.em}} function was adapted
#' to our data since it could not impute some missing values for bmi_m.2
#'
#' @return
#' @export
#'

structural.em.rf <- function (x, maximize = "hc", maximize.args = list(), fit = "mle",
          fit.args = list(), impute, impute.args = list(), return.all = FALSE,
          start = NULL, max.iter = 5, debug = FALSE)
{
  ntests = 0
  data.info = bnlearn:::check.data(x, allow.levels = TRUE, allow.missing = TRUE,
                         warn.if.no.missing = TRUE, stop.if.all.missing = !is(start,
                                                                              "bn.fit"))
  max.iter = bnlearn:::check.max.iter(max.iter)
  bnlearn:::check.logical(debug)
  bnlearn:::check.logical(return.all)
  bnlearn:::check.learning.algorithm(algorithm = maximize, class = "score")
  critical.arguments = c("x", "heuristic", "start", "debug")
  bnlearn:::check.unused.args(intersect(critical.arguments, names(maximize.args)),
                    character(0))
  maximize.args[critical.arguments] = list(x = NULL, heuristic = maximize,
                                           start = NULL, debug = debug)
  bnlearn:::check.fitting.method(method = fit, data = x)
  fit.args = bnlearn:::check.fitting.args(method = fit, network = NULL,
                                data = x, extra.args = fit.args)
  impute = bnlearn:::check.imputation.method(impute, x)
  impute.args = bnlearn:::check.imputation.extra.args(impute, impute.args)

  dag = empty.graph(nodes = names(x))
  fitted = bnlearn:::bn.fit.backend(dag, data = x, method = fit,
                            extra.args = fit.args, data.info = data.info)

  data.info$complete.nodes[names(x)] = TRUE
  for (i in seq(max.iter)) {
    print(i)
    complete = bnlearn:::impute.backend(fitted = fitted, data = x,
                              method = impute, extra.args = impute.args, debug = debug)

# new: start: BMI Mother at FU2 could not be estimated. We imputed here the mean
#            value. The number of missing values were < 10.
    if(any(is.na(complete))){
      ml <- which(is.na(complete$bmi_m.2))
      complete$bmi_m.2[ml] <- mean(complete$bmi_m.2, na.rm = TRUE)
    }
# end
    maximize.args$x = complete
    maximize.args$start = dag
    dag = do.call(bnlearn:::greedy.search, maximize.args)
    fitted.new = bnlearn:::bn.fit.backend(dag, data = complete, method = fit,
                                extra.args = fit.args, data.info = data.info)

    ntests = ntests + dag$learning$ntests
    if (isTRUE(all.equal(fitted, fitted.new)))
      break
    else fitted = fitted.new
  }
  dag$learning$algo = "sem"
  dag$learning$maximize = maximize
  dag$learning$impute = impute
  dag$learning$fit = fit
  dag$learning$ntests = ntests
  if (return.all)
    invisible(list(dag = dag, imputed = complete, fitted = fitted))
  else invisible(dag)
}



