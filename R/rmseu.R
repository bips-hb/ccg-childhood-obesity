#' Graph uncertainty measures
#'
#' Graph uncertainty measures such as the root mean squared edge uncertainty
#'
#' @param edges a vector of edge frequencies
#' @param vertices number of nodes
#' @param threshold default = 0.5
#' @param epsilon defalta = 1e-100
#'
#' @return A list including the meu (mean edge uncertainty), mseu (mean squared
#' edge uncertainty), rmseu (root mean squared edge uncertainty) and mee
#' (mean edge entropy).
#'
#' @export
#'
#' @examples
gum <- function( edges, vertices=NULL, threshold=0.5, epsilon=1e-10000)
  ###### edges ist Vektor von Kantenh?ufigkeiten
  ###### falls notwendig kann aus einer Kantenmatrix dieser so erzeugt werden:
  ###### edges <- t(edgematrix)[lower.tri(t(edgematrix))]
  ###### vertices ist Anzahl von Knoten
{
  if( is.vector(edges) && is.null(vertices)){
    stop("number of 'vertices' must be specified")
  }
  if( is.matrix(edges) ){
    vertices <- nrow(edges)
    edges <- t(edges)[lower.tri(t(edges))]
  }

  e <- cbind( threshold*edges, (1-threshold)*(1-edges))
  edgeS <- apply(e,1,min)
  edgeS

  meu  <- sum(edgeS/ (threshold*(1-threshold)))/choose(vertices,2)
  mseu <- sum((edgeS/(threshold*(1-threshold)))^2)/choose(vertices,2)
  H    <- -1/log(2) * (edges * log(edges + epsilon) +
                      (1 - edges)*log(1 - edges + epsilon) )
  mee  <- sum(H, na.rm = TRUE) / choose(vertices,2)
  out  <- list(meu=meu, mseu=mseu, rmseu=sqrt(mseu), mee=mee)
  out
}



