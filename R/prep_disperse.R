#' Transformation of DISPERSE general database into a unique trait-species relationship 
#'
#' @return
#' @export
#'
#' @examples
prep_disperse <- function() {
  disp <- readxl::read_excel(here::here("data/Original/Traits/DISPERSE/DISPERSE database Revision_02_10_2020.xlsx"), 
                             sheet = "Data", skip = 2)
  trait.def <- readxl::read_excel(here::here("data/Original/Traits/DISPERSE/DISPERSE database Revision_02_10_2020.xlsx"), 
                                  sheet = "DataKey")
  trait.col <- data.frame(Trait = unique(trait.def$Trait), lb = c('bs', 'cy_dur', 'cy_num', 'disp','ls','fwl','wp_type', 'num_egg','prop_drift'))
  trait.def <- dplyr::left_join(trait.def, trait.col, by = 'Trait')
  
  tobind <- data.frame(matrix(data = NA, ncol = length(unique(trait.def$lb)), nrow = nrow(disp)))
  colnames(tobind) <- unique(trait.def$lb)
  disp <- cbind(disp, tobind)
  
  
  for (t in unique(trait.def$lb)) {
    
    code <- trait.def$Code[trait.def$lb == t]
    code <- code[-grep('Ref', code)]
    if(length(grep('info', code))> 0) {code <- code[-grep('info', code)]} #fwl
    subdisp <- disp[, code]
    max.lst <- apply(subdisp, 1, max)
    
    for (n in c(1:nrow(subdisp))) {
      
      if (max.lst[n] != 'NA') {
        
        idx <- which(subdisp[n, ] == max.lst[n])
        val.trait <- trait.def$`Trait category/description`[trait.def$Code %in% code[idx]]
        
        if (length(val.trait)>1) {val.trait <- val.trait[length(val.trait)]}
        disp[n , t] <- val.trait
        
      }
    }
  }
  disp <- disp[, c(1:6, 40, 57:65)]
  return(disp)
}