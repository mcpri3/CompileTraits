#' Get genus level from a list of species name
#'
#' @param species.vec vector of species name
#'
#' @return Vector of genius associated to species name  
#' @export
#'
#' @examples
get_genus <- function(species.vec) {
  interm <- strsplit(species.vec, ' ')
  interm <- lapply(interm, get.first <- function(x){return(x[1])})
  return(unlist(interm))
}