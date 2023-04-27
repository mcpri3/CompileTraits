#' Function to combine trait databases on mammals, birds, reptiles and amphibians of Europe.
#'
#' @return A database with common traits from the four groups. See W. Thuiller et al., 2015, Philosophical Transactions of the Royal Society B: Biological Sciences
#' for details on trait identification
#' @export
#'
#' @examples
combine_vert_traitDB = function() {
# Read tables of traits 
traits_A <- read.table(here::here("data/Original/Traits/VERTEBRATES_THUILLER2015/Traits_interet_Amphibian.txt"), sep=" ", header = TRUE)
traits_B <- read.table(here::here("data/Original/Traits/VERTEBRATES_THUILLER2015/traits_interet_birds_jan2018.txt"), sep="\t", header = TRUE)
traits_M <- read.table(here::here("data/Original/Traits/VERTEBRATES_THUILLER2015/Traits_interet_Mammals.txt"), sep="\t", header = TRUE)
traits_R <- read.table(here::here("data/Original/Traits/VERTEBRATES_THUILLER2015/Traits_interet_Reptiles.txt"),sep=" ", header = TRUE)

traits_M$Body_length_max_.cm. = NA

# Identify common traits 
common_traits <- intersect(names(traits_A), intersect(names(traits_B), intersect(names(traits_R), names(traits_M))))

# New dataframe with common traits for all classes
trait_matrix <- rbind(traits_A[, colnames(traits_A) %in% common_traits],
                              traits_B[, colnames(traits_B) %in% common_traits] ,
                              traits_M[, colnames(traits_M) %in% common_traits] ,
                              traits_R[, colnames(traits_R) %in% common_traits])

# Add species name
sp.code.old <- readr::read_delim("data/Original/Traits/TETRA_EU/species_codes_and_taxonomy.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
sp.code.old <- sp.code.old[, c('Species ID', 'Species')]
colnames(sp.code.old)[2] <- 'OldSpeciesName_Maiorano'
sp.code <- readr::read_delim("data/Original/Taxa/SpeciesList_Correspondances.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
sp.code <- dplyr::left_join(sp.code, sp.code.old, by = c("Code_old" = "Species ID"))
trait_matrix <- dplyr::left_join(sp.code, trait_matrix, by = c("Code_old" = "Sp.code" ))
}