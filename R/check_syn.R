#' Function to identify synonyms of species' name that is absent in a database
#'
#' @param myvec vector of species' name that is compared to dbvec
#' @param dbvec vector of species' name available in a database
#' @param db.syn database to query to find synonyms, either itis, tropicos, nbn, worms, or pow (see taxize::synonyms)
#'
#' @return A dataframe with one column indicating the missing species' name in the database and one colum indicating its potential synonyms
#' @export
#'
#' @examples
check_syn <- function(myvec, dbvec, db.syn = 'itis', api.key = NULL) {
  missing <- dplyr::setdiff(myvec, dbvec)
  
  syn <- taxize::synonyms(missing, db = db.syn, key = api.key)
  final <- data.frame()
  
  for (l in 1:length(syn)) {
    x = syn[[l]]
    if (sum(class(x) == 'data.frame') != 0) {
       if (nrow(x) > 0) {
      ttry <- try(rbind(final, data.frame(Species = names(syn[l]), Synonym = (x)[, 'syn_name'])))
      if (class(ttry) == 'try-error') {ttry = rbind(final, data.frame(Species = names(syn[l]), Synonym = (x)[, 'synonym']))}
      final <- ttry
       } else {
         final <- rbind(final, data.frame(Species = names(syn[l]), Synonym = NA))
       }
    } else {
      final <- rbind(final, data.frame(Species = names(syn[l]), Synonym = NA))
    }
  }
  
  return(final)
}
