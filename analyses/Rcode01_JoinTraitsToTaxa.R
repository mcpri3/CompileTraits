
# rredlist package 
#######################################
## General functions
#######################################
# Prep. TETRA_EU
prep.tetra = function() {
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

#######################################
######### lst of SNAP taxa ############
#######################################
myData <- readxl::read_excel(here::here("data/Original/Taxa/SNAPTaxons_Representativite_Mlx.xlsx"), 
                             sheet = "Liste_Taxons")
colnames(myData)[colnames(myData) == 'CD_REF'] = 'CD_NOM'

# TAXREF to add the rank
taxref.full <- utils::read.csv('data/Original/Taxa/TAXREFv16.csv', header = T, sep = ";")
taxref <- taxref.full[taxref.full$CD_NOM %in% myData$CD_NOM, ]
taxref <- taxref[, c('CD_NOM','CD_REF', 'RANG')]
colnames(taxref)[colnames(taxref) == "CD_REF"] <- "CD_REF_V16TAXREF"

myData <- dplyr::left_join(myData, taxref, by = 'CD_NOM')

# update to the species levels for under-species taxa
toupdate <- myData[myData$RANG != 'ES',]
taxref <- taxref.full[taxref.full$CD_NOM %in% toupdate$CD_NOM,]
taxref <- taxref[, c('CD_NOM', 'CD_TAXSUP')]
taxref.parent <- taxref.full[taxref.full$CD_NOM %in% taxref$CD_TAXSUP, ]
taxref.parent <- taxref.parent[, c('CD_REF', 'LB_NOM')]
colnames(taxref.parent) <- c('CD_REF_SPE_LEVEL', 'LB_NOM_VALIDE_SPE_LEVEL')
taxref.parent <- dplyr::left_join(taxref.parent, taxref, by = c('CD_REF_SPE_LEVEL' = 'CD_TAXSUP'))

toupdate$Parent <- T  
toupdate <- dplyr::left_join(toupdate, taxref.parent, by = 'CD_NOM')

myData.sp <- myData[myData$RANG == 'ES', ]
myData.sp$Parent <- F 
myData.sp$CD_REF_SPE_LEVEL <- myData.sp$CD_REF_V16TAXREF
myData.sp$LB_NOM_VALIDE_SPE_LEVEL <- myData.sp$LB_NOM_VALIDE

myData <- rbind(myData.sp, toupdate)

############################
# Amphibians (N = 28)
############################
# Trait DB: amphiBIO & Tetra_EU, AnimalTraits for missing body mass (but nada...)

# Prep. amphiBIO
amphibio <- readr::read_csv(here::here("data/Original/Traits/AmphiBIO/AmphiBIO_v1.csv"))
idx <- colnames(amphibio)[c(5:23, 26, 29, 38)] #removed reproductive traits, ok?
amphibio <- amphibio[, colnames(amphibio) %in% idx]

# Prep. TETRA_EU
tetra.db = prep.tetra()

# Prep. AnimalTraits
animalT <- readxl::read_excel("data/Original/Traits/AnimalTraits/observations.xlsx")
animalT <- animalT[, c('species', 'body mass', 'body mass - units')]

# Prep. CESTE 
ceste.md <- readxl::read_excel("data/Original/Traits/CESTES/286_2_CESTES_metadata.xlsx")
ceste.md <- ceste.md[ceste.md$`Taxonomic group` == 'Amphibians',]
files = paste0(ceste.md$DatasetName, '_AJ.xlsx')

# Join to species list
amphi <- myData[myData$GROUP2_INPN == 'Amphibiens',]
amphi <- dplyr::left_join(amphi, amphibio, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'Species'))
amphi <- dplyr::left_join(amphi, tetra.db, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'Species'))

animalT <- animalT[animalT$species %in% amphi$LB_NOM_VALIDE_SPE_LEVEL[is.na(amphi$Body_mass_g)],]
# nada... 

########################
# Reptiles (N = 27)
########################
# Trait DB: ReptileTraits & Tetra_EU, AnimalTraits for missing body mass 

## Generic function for loading data, see https://leowong.ca/blog/connect-to-microsoft-access-database-via-r/
fn.importData <- function(MDBPATH, TABLES, DROP_VARS=c(), ORDER_BY=c(), PWD="") {

  ## Set up driver info and database path
  library(RODBC)
  DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH, ";PWD=", PWD)

  ## Establish connection
  channel <- odbcDriverConnect(PATH)

  ## Iterate through list of tables
  for (tbl in TABLES) {

    ## Retrieve all variable names from table tbl
    tbl_vars <- sqlColumns(channel, tbl)["COLUMN_NAME"]
    ## Exclude variables based on input parameters
    tbl_vars <- subset(tbl_vars, !(tbl_vars$COLUMN_NAME %in% DROP_VARS))
    ## Add brackets to each variable (ie. [variable]) to maintain ACCESS syntax
    tbl_vars$COLUMN_NAME <- paste0("[", tbl_vars$COLUMN_NAME, "]")
    ## Transform dataframe column into string separated by comma
    cols <- paste0(tbl_vars[1:nrow(tbl_vars), ], collapse=",")
    ## Create ORDER BY string
    if (length(ORDER_BY) > 0) {
      order <- paste0("ORDER BY", paste0(paste0("[", ORDER_BY, "]"), collapse=", "))
    }
    order <- ""
  }

  ## Extract table of interest as dataframe
  df <- sqlQuery(channel,
                 paste0("SELECT ", cols, " FROM [", tbl, "]", order, ";"),
                 stringsAsFactors=FALSE)

  ## Replace dash with underscore
  new_tbl_name <- gsub("-", "_", tbl)

  ## Assign dataframe to environment
  assign(new_tbl_name, df, envir=.GlobalEnv)
}

fn.importData(MDBPATH=here::here("data/Original/Traits/ReptileTraits/Grimm_etal_Reptile_DB_Update_2015.accdb"),
              TABLES=c("Morphometry"))

## Close and remove channel
close(channel)
rm(channel)

########################
# Mammals (N = 61)
########################
# COMBINE, Tetra_EU, HerbiTraits, DarkCiseS, AnimalTraits (nada), EltonTraits (already in COMBINE) 

mammal = myData[myData$GROUP2_INPN == 'Mammifères',]

# Prep. COMBINE 
comb <- readr::read_csv(here::here("data/Original/Traits/COMBINE/trait_data_imputed.csv"))
comb <- comb[, colnames(comb) != c('order', 'family', 'genus', 'species', 'phylacine_binomial')]

# Prep. AnimalTraits
animalT <- readxl::read_excel("data/Original/Traits/AnimalTraits/observations.xlsx")
animalT <- animalT[, c('species', 'body mass', 'body mass - units')]
animalT <- animalT[animalT$species %in% mammal$LB_NOM_VALIDE_SPE_LEVEL[is.na(mammal$adult_mass_g)],] 
# nada ... 

# Prep. HerbiTraits 
herbi <- readr::read_csv(here::here("data/Original/Traits/HerbiTraits/HerbiTraits_1.2.csv"))
herbi[herbi$Binomial %in% mammal$LB_NOM_VALIDE_SPE_LEVEL,]
# less info than in COMBINE and only 5 species in HerbiTraits from our list

# Prep. TETRA_EU
tetra.db = prep.tetra()

# Prep. DarkCiseS
darkcise <- readr::read_csv(here::here("data/Original/Traits/DarkCiseS/V1/Dataset1.csv"))
darkcise <- darkcise[, c(4, 7:12, 30:51)]
colnames(darkcise) = gsub(' ', '_', colnames(darkcise))

# Join to species list
mammal <- dplyr::left_join(mammal, comb, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'iucn2020_binomial'))
mammal <- dplyr::left_join(mammal, tetra.db, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'Species'))
mammal <- dplyr::left_join(mammal, darkcise, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'Species.name'))

###########################
# Birds (N = 174)
###########################
# GABB (no need?), AVONET, TETRA_EU, EltonTraits (already in AVONET), AnimalTraits (nada)

birds <- myData[myData$GROUP2_INPN %in% 'Oiseaux', ]

# Prep. AVONET
avonet1 <-readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET1_Birdlife.xlsx"), 
                             sheet = "AVONET1_BirdLife")
avonet2 <- readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET2_eBird.xlsx"), 
                              sheet = "AVONET2_eBird")
avonet3 <- readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET3_BirdTree.xlsx"), 
                              sheet = "AVONET3_BirdTree")
col.tokeep <- c( "Beak.Length_Culmen", "Beak.Length_Nares", "Beak.Width", "Beak.Depth", 
                 "Tarsus.Length", "Wing.Length", "Kipps.Distance", "Secondary1", "Hand-Wing.Index",  "Tail.Length",       
                 "Mass", "Habitat", "Habitat.Density", "Migration", 
                 "Trophic.Level", "Trophic.Niche", "Primary.Lifestyle")
avonet1 <- avonet1[, c('Species1', col.tokeep)]
avonet2 <- avonet2[, c('Species2', col.tokeep)]
avonet3 <- avonet3[, c('Species3', col.tokeep)]

# Join to species list
birds1 <- birds[birds$LB_NOM_VALIDE_SPE_LEVEL %in% avonet1$Species1, ]
birds2 <- birds[!(birds$LB_NOM_VALIDE_SPE_LEVEL %in% avonet1$Species1) & birds$LB_NOM_VALIDE_SPE_LEVEL %in% avonet2$Species2,]
birds3 <- birds[!(birds$LB_NOM_VALIDE_SPE_LEVEL %in% avonet1$Species1) & !(birds$LB_NOM_VALIDE_SPE_LEVEL %in% avonet2$Species2) & birds$LB_NOM_VALIDE_SPE_LEVEL %in% avonet3$Species3,]
birds1<- dplyr::left_join(birds1, avonet1, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'Species1'))
birds2<- dplyr::left_join(birds2, avonet2, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'Species2'))
birds3<- dplyr::left_join(birds3, avonet3, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'Species3'))
toupdate <- rbind(birds1, birds2, birds3)

missing <- birds[!(birds$LB_NOM_VALIDE_SPE_LEVEL %in% toupdate$LB_NOM_VALIDE_SPE_LEVEL), ]
col.toadd.lb = colnames(toupdate)[!(colnames(toupdate) %in% colnames(missing))]
col.toadd <- matrix(data = NA, ncol = length(col.toadd.lb))
col.toadd <- as.data.frame(col.toadd)
colnames(col.toadd) = col.toadd.lb
missing <- cbind(missing, col.toadd)

birds = rbind(toupdate, missing)

# Prep. TETRA_EU
tetra.db = prep.tetra()

# Prep. AnimalTraits
animalT <- readxl::read_excel(here::here("data/Original/Traits/AnimalTraits/observations.xlsx"))
animalT <- animalT[, c('species', 'body mass', 'body mass - units')]
animalT <- animalT[animalT$species %in% birds$LB_NOM_VALIDE_SPE_LEVEL[is.na(birds$Mass)],] 
# nada ... 

# Prep. GABB ; maybe for nest info?
gabb <- readxl::read_excel(here::here("data/Original/Traits/GABB/GABB_Global_Alpine_Breeding_Bird_dataset.xlsx"), 
                           sheet = "Global alpine breeding birds")
sum(birds$LB_NOM_VALIDE_SPE_LEVEL %in% gabb$`Scientific name`)

birds <- dplyr::left_join(birds, tetra.db, by = c('LB_NOM_VALIDE_SPE_LEVEL' = 'Species'))

##########################
# Insects (N = 684)
##########################
