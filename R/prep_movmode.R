#' Transformation of TAXREF mobility file into a binary database 
#'
#' @return
#' @export
#'
#' @examples
prep_movmode <- function() {
  movmode <- readr::read_csv(here::here("data/Original/Traits/TAXREF/Vertebrates/TAXREF_VERTEBRATES_Mobility.csv"))
  tax.clean <- reshape2::colsplit(movmode$Taxon, '</i>', c('taxname', 'author'))
  movmode$Taxon <- gsub('<i>','', tax.clean$taxname)
  movmode <- movmode[!is.na(movmode$`Stade biologique`),]
  movmode.db <- data.frame(matrix(data = 0, ncol = (length(unique(movmode$Mobilité))+1), nrow = length(unique(movmode$Taxon))))
  colnames(movmode.db) <- c('Taxon', unique(movmode$Mobilité))
  movmode.db$Taxon = unique(movmode$Taxon)
  
  for (s in unique(movmode$Taxon)) {
    
    subdata <- movmode[movmode$Taxon == s, ]
    movmode.db[movmode.db$Taxon == s, which(colnames(movmode.db) %in% subdata$Mobilité)] <- 1  
  }
  return(movmode.db)
  
}