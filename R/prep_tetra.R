#' Transformation of TETRA_EU general diet file into a binary database 
#'
#' @return A dataframe with each line being one species and columns indicating whether the species eats what is specified in column name
#' @export
#'
#' @examples
prep_tetra = function() {
  tetra <- readr::read_delim(here::here('data/Original/Traits/TETRA_EU/TetraEU_generic_diet.csv'), 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
  tetra.db <- data.frame() 
  lb <- unique(tetra$targetGenericItemName)
  
  for (id in unique(tetra$sourceTaxonName)) {
    subdata <- tetra[tetra$sourceTaxonName == id,]
    tetra.db <- rbind(tetra.db, data.frame(sourceTaxonName = id, t(lb %in% subdata$targetGenericItemName)))
  }
  lb <- gsub(' ', '_', lb, fixed = T)
  lb = paste0('eats_', lb)
  colnames(tetra.db) <- c('Species', lb)
  return(tetra.db)
}