################################################
######### Prep. lst of SNAP species ############
################################################
myData <- readxl::read_excel(here::here("data/Original/Taxa/SNAPTaxons_Representativite_Mlx.xlsx"), 
                             sheet = "Liste_Taxons")
colnames(myData)[colnames(myData) == 'CD_REF'] = 'CD_NOM'

# TAXREF to add the rank
taxref.full <- utils::read.csv(here::here('data/Original/Taxa/TAXREFv16.csv'), header = T, sep = ";")
taxref <- taxref.full[taxref.full$CD_NOM %in% myData$CD_NOM, ]
taxref <- taxref[, c('CD_NOM','CD_REF', 'RANG', 'GROUP3_INPN', 'NOM_VERN', 'PHYLUM', 'CLASSE', 'FAMILLE')]
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
# openxlsx::write.xlsx(myData, here::here('data/Modified/SNAPTaxons_Representativite_Mlx_SpeciesLevel.xlsx'))
myData <- myData[, c("PHYLUM","CLASSE","FAMILLE","ORDRE","GROUP2_INPN", "GROUP3_INPN","CD_REF_SPE_LEVEL","LB_NOM_VALIDE_SPE_LEVEL")]
myData <- dplyr::distinct(myData)
myData <- myData[!is.na(myData$CD_REF_SPE_LEVEL),]

########################################
# Thuiller et al. 2015 trait DB
########################################
# Read and combine vertebrate databases
vert.trait <- combine_vert_traitDB()
vert.trait <- vert.trait[, !(colnames(vert.trait) %in% c('Order', 'Family'))]

vert.trait <- vert.trait[!is.na(vert.trait$Mushrooms),]
vert.trait <- vert.trait[!is.na(vert.trait$Opportunistic),]
vert.trait <- vert.trait[!is.na(vert.trait$Arithmic),]
vert.trait <- vert.trait[!is.na(vert.trait$Ground),]
sum(is.na(vert.trait$Body_length_max_.cm.) & is.na(vert.trait$BodyMass))

colnames(vert.trait)[colnames(vert.trait) %in% c("Mushrooms", "Mosses.Lichens","Seeds.Nuts.Grains","Fruits.Berries" ,               
                                                 "Vegitative","Invert","Fish" ,"Small_Mam","Large_Mam","Herptile" ,                      
                                                 "Bird_eggs","Small_bird", "Large_Bird" ,                    
                                                 "Vertebrate" ,"Bones","Carrion","Coprofagus" )] <- 
                                                  paste0('diet.', c("Mushrooms", "Mosses.Lichens","Seeds.Nuts.Grains",
                                                                    "Fruits.Berries" ,"Vegitative","Invert","Fish" ,
                                                                    "Small_Mam","Large_Mam","Herptile" , "Bird_eggs",
                                                                    "Small_bird", "Large_Bird" , "Vertebrate" ,
                                                                    "Bones","Carrion","Coprofagus" ))

colnames(vert.trait)[colnames(vert.trait) %in% c("Opportunistic","Hunting", "Browser", "Grazer")] <- paste0('forag.strat.', c("Opportunistic","Hunting", "Browser", "Grazer"))
colnames(vert.trait)[colnames(vert.trait) %in% c("Nocturnal", "Crepuscolar", "Diurnal", "Arithmic")] <- paste0('act.time.', c("Nocturnal", "Crepuscolar", "Diurnal", "Arithmic"))
colnames(vert.trait)[colnames(vert.trait) %in% c("Body_length_max_.cm.")] <- 'morpho.Body_length_max_.cm.'
colnames(vert.trait)[colnames(vert.trait) %in% c("BodyMass")] <- 'morpho.BodyMass'
colnames(vert.trait)[colnames(vert.trait) %in% c("Tree_hole.fissure_in_the_bark", "Ground", "Rocks", "Building.Artificial" , "Underground_water",              
                                                 "Cave.Fissures.Borrows", "Lodge", "Temporary_water" ,"Brooks.springs.small_rivers", 
                                                 "Puddles.ponds.pools.small_lakes", "Brackish_waters" )] <- 
                                                  paste0('nest.hab.', c("Tree_hole.fissure_in_the_bark", "Ground", "Rocks", 
                                                                       "Building.Artificial" , "Underground_water", "Cave.Fissures.Borrows", 
                                                                       "Lodge", "Temporary_water" ,"Brooks.springs.small_rivers", 
                                                                        "Puddles.ponds.pools.small_lakes", "Brackish_waters" ))




#################################
# Habitat preference
#################################
hab.pref <- readr::read_csv("data/Original/Traits/HabitatPreference/SpeciesHabitatsPreferences_Yue.csv")
corresp.code <- readr::read_delim("data/Original/Traits/HabitatPreference/LandUseCodesCorrespondance_Yue_HabitatPreferences.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
hab.pref.up <- hab.pref
for (i in c(6:34)) {
  colnames(hab.pref.up)[i] <- corresp.code$YueRaster[corresp.code$Louise == colnames(hab.pref.up)[i]]
} 

hab.pref.up <- hab.pref.up[, c(5:34)]
colnames(hab.pref.up)[2:30] <- paste0('hab.pref.',colnames(hab.pref.up)[2:30])

# Join to trait DB 
vert.trait <- dplyr::left_join(vert.trait, hab.pref.up, by = c('Code_old' = 'SppID'))

###########################################################
# Dispersal distance, movement mode & reproductive traits
###########################################################

        ###############
########### MAMMALS ###############
        ###############

mammals <- vert.trait[vert.trait$Class == 'Mammalia',]

# Prep. COMBINE ; mammals
comb <- readr::read_csv(here::here("data/Original/Traits/COMBINE/trait_data_imputed.csv"))
comb$crawler <- 0
comb$flier <- ifelse(comb$terrestrial_volant == 1, 1, 0)
comb$walker <- ifelse(comb$`terrestrial_non-volant` == 1, 1, 0)
comb$runner <- ifelse(comb$`terrestrial_non-volant` == 1, 1, 0)
comb$swimmer <- ifelse(comb$`terrestrial_non-volant` == 1 & (comb$marine == 1 |comb$freshwater == 1), 1, 0)
comb$jumper <- NA
comb$climber <- NA

comb1 <- comb[, c('iucn2020_binomial', "dispersal_km", 'crawler', 'flier', 'walker','runner','swimmer','jumper','climber')]
colnames(comb1) <- c('species', 'dispersal_km', 'crawler', 'flier', 'walker','runner','swimmer','jumper','climber')
comb2 <- comb[, c('phylacine_binomial', "dispersal_km", 'crawler', 'flier', 'walker','runner','swimmer','jumper','climber' )]
colnames(comb2) <- c('species', 'dispersal_km', 'crawler', 'flier', 'walker','runner','swimmer','jumper','climber')
comb <- rbind(comb1, comb2)
comb <- dplyr::distinct(comb)

# Check if synonyms are available and could help adding some traits 
# syn <- check_syn(mammals$SpeciesName, comb$species)
# syn$inDB <- syn$Synonym %in% comb$species
# syn <- syn[syn$inDB, ]
# 
# syn <- check_syn(mammals$NAME_IUCN, comb$species)
# syn$inDB <- syn$Synonym %in% comb$species
# syn <- syn[syn$inDB, ]

syn <- check_syn(mammals$OldSpeciesName_Maiorano, comb$species)
syn$inDB <- syn$Synonym %in% comb$species
syn <- syn[syn$inDB, ]

mammals <- dplyr::left_join(mammals, syn[,c('Species','Synonym')], by = c('OldSpeciesName_Maiorano' = 'Species'))
mammals$Species_Syn <- ifelse(is.na(mammals$Synonym), mammals$OldSpeciesName_Maiorano, mammals$Synonym)

# Check other name columns
idx = !(mammals$Species_Syn %in% comb$species)
sum(mammals$SpeciesName[idx] %in% comb$species)
sum(mammals$NAME_IUCN[idx] %in% comb$species)
# update
mammals$Species_Syn[idx] <- mammals$NAME_IUCN[idx]

# Join to species list
mammals <- dplyr::left_join(mammals, comb, by = c("Species_Syn"="species"))

# Add bats info 
bats <- readr::read_csv(here::here("data/Original/Traits/EuroBaTrait/09_spatial_behaviour.csv"))
morpho <- readr::read_csv("data/Original/Traits/EuroBaTrait/03_morphology.csv")

# try to find correlation between traits
df <- bats[bats$verbatimTraitName == 'DispersalDistances', ]
df <- df[, c('verbatimScientificName', 'verbatimTraitValue')]
colnames(df) <- c('species', 'DD')
test <- morpho[morpho$verbatimTraitName == "WingLoadingIndex", ]
test <- test[, c('verbatimScientificName', 'verbatimTraitValue')]
colnames(test) <- c('species', 'var.test')
test$var.test <- as.numeric(test$var.test)
df <- dplyr::left_join(df, test, by = 'species')
mod <- lm(DD~ var.test, data = df)
summary(mod)

bats <- bats[bats$verbatimTraitName %in% c("DispersalDistances"), ]
bats$verbatimScientificName <- gsub('_',' ',bats$verbatimScientificName)
morpho <- morpho[morpho$verbatimTraitName == 'WingLoadingIndex', ]
morpho$verbatimScientificName <- gsub('_',' ',morpho$verbatimScientificName)

for (s in unique(c(bats$verbatimScientificName, morpho$verbatimScientificName))) {
  val <- bats$verbatimTraitValue[(bats$verbatimScientificName %in% s)]
  if (length(val) == 0) { 
    val <- morpho$verbatimTraitValue[(morpho$verbatimScientificName %in% s)]
    val <- -176.2 + 29.6 * val
    }
  mammals$dispersal_km[mammals$SpeciesName %in% s] <- val
}

#### Add reproductive traits ####
comb <- readr::read_csv(here::here("data/Original/Traits/COMBINE/trait_data_imputed.csv"))
comb1 <- comb[, c('iucn2020_binomial', 'max_longevity_d', 'maturity_d', 'age_first_reproduction_d' , 'litter_size_n', 'litters_per_year_n')]
colnames(comb1) <- c('species', 'max_longevity_d', 'maturity_d', 'age_first_reproduction_d' , 'litter_size_n', 'litters_per_year_n')
comb2 <- comb[, c('phylacine_binomial', 'max_longevity_d', 'maturity_d', 'age_first_reproduction_d' , 'litter_size_n', 'litters_per_year_n')]
colnames(comb2) <- c('species', 'max_longevity_d', 'maturity_d', 'age_first_reproduction_d' , 'litter_size_n', 'litters_per_year_n')
comb <- rbind(comb1, comb2)
comb <- dplyr::distinct(comb)
double <- table(comb$species)
subcomb <- comb[comb$species %in% names(double)[double > 1], ]
comb <- comb[!(comb$species %in% names(double)[double > 1]), ]

for (s in unique(subcomb$species)) {
  ssubcomb <- subcomb[subcomb$species == s, ]
  val = apply(ssubcomb[, 2:6], 2, mean)
  ssubcomb <- cbind(data.frame(species = s), as.data.frame(t(val)))
  comb <- rbind(comb, ssubcomb)
}
mammals <- dplyr::left_join(mammals, comb, by = c("Species_Syn"="species"))

            ###############
############## REPTILES ##################
            ###############

reptiles <- vert.trait[vert.trait$Class == 'Reptilia',]

# Prep. Escoriza 
escoriza <- readxl::read_excel(here::here("data/Original/Traits/Escoriza_2021/Modeled_trajectories.xlsx"))
# escoriza <- escoriza[, c('Species', "Total travel1", "Total travel2", "Total travel 3")]
# escoriza$dispersal_km <- apply(escoriza[, c(2:4)], 1 ,mean)
escoriza$dispersal_km <- escoriza$`Total travel 3`
escoriza <- escoriza[, c("Species", "dispersal_km")]

# Check if synonyms are available and could help adding some traits 
idx <- is.na(reptiles$NAME_IUCN)
reptiles$NAME_IUCN[idx] <- reptiles$SpeciesName[idx]
syn <- check_syn(reptiles$NAME_IUCN, escoriza$Species)
syn$inDB <- syn$Synonym %in% escoriza$Species
syn <- syn[syn$inDB, ]
reptiles <- dplyr::left_join(reptiles, syn[, c('Species','Synonym')], by = c('NAME_IUCN' = 'Species') )
reptiles$Species_Syn <- ifelse(is.na(reptiles$Synonym), reptiles$NAME_IUCN, reptiles$Synonym)
reptiles$NAME_IUCN[idx] <- NA

# Check other name columns
idx = !(reptiles$Species_Syn %in% escoriza$Species)
sum(reptiles$SpeciesName[idx] %in% escoriza$Species)
sum(reptiles$OldSpeciesName_Maiorano[idx] %in% escoriza$Species)

# Join to species list
reptiles <- dplyr::left_join(reptiles, escoriza, by = c("Species_Syn"="Species"))

#### Add movement mode from another DB Grimm et al. 2015 #####
mmode <- readxl::read_excel(here::here("data/Original/Traits/ReptileTraits/Movement Query.xlsx"))
mmode <- mmode[!is.na(mmode$Locomotion),]
loco <- reshape2::colsplit(mmode$Locomotion, '; ', c('loco1','loco2'))
loco <- rbind(data.frame(sp.nme = mmode$`Species name SEH`, loco), data.frame(sp.nme = mmode$`Current species name`, loco))
loco <- dplyr::distinct(loco)
loco$crawler <- NA
loco$flier <- 0
loco$walker <- NA
loco$runner <- NA
loco$swimmer <- NA
loco$jumper <- NA
loco$climber <- NA
mmodes <- c('crawler', 'flier','walker','runner','swimmer','jumper','climber')

for (i in 1:nrow(loco)) {
  val <- (mmodes  %in% loco$loco1[i]) + (mmodes %in% loco$loco2[i]) 
  loco[i, colnames(loco) %in% mmodes] <- val
}
loco <- loco[, !colnames(loco) %in% c('loco1','loco2')]

final = data.frame()
for (s in unique(loco$sp.nme)) {
  sub <- loco[loco$sp.nme %in% s, mmodes]
  sub <- sapply(sub, sum)
  final <- rbind(final, data.frame(sp.nme = s, t(sub)))
}

# Synonyms ok 
reptiles <- dplyr::left_join(reptiles, final, by = c("Species_Syn"="sp.nme"))


#### Add reproductive traits #####
# To do manually 
reptiles$max_longevity_d <- NA
reptiles$maturity_d <- NA
reptiles$age_first_reproduction_d <- NA
reptiles$litter_size_n <- NA
reptiles$litters_per_year_n <- NA

        ###############
############ BIRDS ################
        ###############

birds <- vert.trait[vert.trait$Class == 'Aves',]

# Prep. AVONET
avonet1 <-readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET1_Birdlife.xlsx"), 
                             sheet = "AVONET1_BirdLife")
avonet2 <- readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET2_eBird.xlsx"), 
                              sheet = "AVONET2_eBird")
avonet3 <- readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET3_BirdTree.xlsx"), 
                              sheet = "AVONET3_BirdTree")
col.tokeep <- c( "Hand-Wing.Index", "Habitat")
avonet1 <- avonet1[, c('Species1', col.tokeep)]
avonet2 <- avonet2[, c('Species2', col.tokeep)]
avonet3 <- avonet3[, c('Species3', col.tokeep)]
newcolnms <- c('Species', 'HWI', 'Habitat')
colnames(avonet1) <- newcolnms
colnames(avonet2) <- newcolnms
colnames(avonet3) <- newcolnms
avonet <- rbind(avonet1, avonet2, avonet3)
avonet <- dplyr::distinct(avonet)
avonet$dispersal_km <- exp(2.70 + 0.86 * (avonet$HWI - mean(avonet$HWI))/(2*sd(avonet$HWI)))
avonet <- avonet[, c('Species', 'dispersal_km', 'Habitat')]

# No need to check syn because they are all in avonet
birds$Species_Syn <- birds$SpeciesName

# Join to species list
birds <- dplyr::left_join(birds, avonet, by = c("Species_Syn"="Species"))

#### Add movement mode #####
birds$crawler <- 0
birds$flier <- 1
birds$walker <- 1
birds$runner <- NA
birds$swimmer <- ifelse(birds$Habitat %in% c('Wetland','Coastal','Marine','Riverine'), 1, 0)
birds$jumper <- NA
birds$climber <- NA

#### Add reproductive traits #####
euroB <- readr::read_delim(here::here('data/Original/Traits/EuroBirds/Life-history characteristics of European birds.txt'))
euroB <- euroB[, c('Species', 'Life span', 'Age of first breeding', 'Clutch_MEAN', 'Broods per year')]
euroB$max_longevity_d <-  euroB$`Life span`*365.25
euroB$maturity_d <- NA
euroB$age_first_reproduction_d <- euroB$`Age of first breeding` * 365.25
euroB$litter_size_n <- euroB$Clutch_MEAN
euroB$litters_per_year_n <- euroB$`Broods per year`
euroB <- euroB[, c("Species",  "max_longevity_d" , "maturity_d", "age_first_reproduction_d" ,
                   "litter_size_n" , "litters_per_year_n")]
birds <- dplyr::left_join(birds, euroB, by = c("SpeciesName" = "Species"))

          ###############
############ AMPHIBIANS ################
          ###############

amphi <- vert.trait[vert.trait$Class == 'Amphibia',]

# Prep. Trochet DB
trochet <- readxl::read_excel(here::here("data/Original/Traits/Trochet_et_al_2014_Amphibians/biodiversity_data_journal-2-e4123-s001.xlsx"), 
                              skip = 3)
mmode <- trochet[, c('Species', 'Walker', 'Jumper','Runner', 'Climber','Swimmer', 'Crawler')]
trochet <- trochet[trochet$`Maximum dispersal distance`!= 'DD',]
trochet$`Maximum dispersal distance` <- as.numeric(trochet$`Maximum dispersal distance`)/1000
trochet <- trochet[, c("Species", "Maximum dispersal distance")]
colnames(trochet)[2] <- 'dispersal_km'

# Check if synonyms are available and could help adding some traits 
idx <- is.na(amphi$NAME_IUCN)
amphi$NAME_IUCN[idx] <- amphi$SpeciesName[idx]
syn <- check_syn(amphi$NAME_IUCN, trochet$Species)
2
syn$inDB <- syn$Synonym %in% trochet$Species
syn <- syn[syn$inDB, ]
amphi <- dplyr::left_join(amphi, syn[, c('Species','Synonym')], by = c('NAME_IUCN'='Species'))
amphi$Species_Syn <- ifelse(is.na(amphi$Synonym), amphi$NAME_IUCN, amphi$Synonym)
amphi$NAME_IUCN[idx] <- NA

# Check other name columns
idx = !(amphi$Species_Syn %in% trochet$Species)
sum(amphi$SpeciesName[idx] %in% trochet$Species)
sum(amphi$OldSpeciesName_Maiorano[idx] %in% trochet$Species)

# Join to species list
amphi <- dplyr::left_join(amphi, trochet, by = c("Species_Syn"="Species"))

#### Add movement mode
amphi <- dplyr::left_join(amphi, mmode, by = c("Species_Syn" = "Species"))
amphi$crawler <- ifelse(amphi$Crawler == 'DD', NA, amphi$Crawler)
amphi$flier <- 0
amphi$walker <- ifelse(amphi$Walker == 'DD', NA, amphi$Walker)
amphi$runner <- ifelse(amphi$Runner == 'DD', NA, amphi$Runner)
amphi$swimmer <- ifelse(amphi$Swimmer == 'DD', NA, amphi$Swimmer)
amphi$jumper <- ifelse(amphi$Jumper == 'DD', NA, amphi$Jumper)
amphi$climber <- ifelse(amphi$Climber == 'DD', NA, amphi$Climber)

#### Add reproductive trait
amphibio <- readr::read_csv(here::here("data/Original/Traits/AmphiBIO/AmphiBIO_v1.csv"))
amphibio <- amphibio[, c('Species', "Age_at_maturity_min_y", "Age_at_maturity_max_y", 'Longevity_max_y', "Litter_size_min_n", "Litter_size_max_n" ,"Reproductive_output_y" )]
amphibio$max_longevity_d <- amphibio$Longevity_max_y *365.25
amphibio$maturity_d <- 365.25*(amphibio$Age_at_maturity_min_y + amphibio$Age_at_maturity_max_y)/2
amphibio$age_first_reproduction_d <- NA
amphibio$litter_size_n <- (amphibio$Litter_size_min_n + amphibio$Litter_size_max_n)/2
amphibio$litters_per_year_n <- amphibio$Reproductive_output_y
amphibio <- amphibio[, c('Species', "max_longevity_d" , "maturity_d", "age_first_reproduction_d" ,
                         "litter_size_n" , "litters_per_year_n")]
amphi <- dplyr::left_join(amphi, amphibio, by = c("SpeciesName"="Species"))

####### COMBINE THE FOUR DATAFRAMES ########
col.tokeep <- intersect(colnames(mammals), colnames(birds))
final <- rbind(mammals[, col.tokeep], birds[, col.tokeep], amphi[, col.tokeep], reptiles[, col.tokeep])
final <- final[, !(colnames(final) %in% 'Species_Syn')]
# Update mov mode
colnames(final)[colnames(final) %in% c('crawler', 'flier','walker','runner','swimmer','jumper','climber')] <- paste0('mov.mode.', colnames(final)[colnames(final) %in% c('crawler', 'flier','walker','runner','swimmer','jumper','climber')])
for (lb in grep('mov.mode', colnames(final))) {
  final[,lb] <- as.numeric(unlist(final[, lb]))
}
# Update colnames of life history traits
colnames(final)[colnames(final) %in% c("max_longevity_d" , "maturity_d",  "age_first_reproduction_d", "litter_size_n", "litters_per_year_n")] <- 
  paste0('life.hist.', colnames(final)[colnames(final) %in% c("max_longevity_d" , "maturity_d",  "age_first_reproduction_d", "litter_size_n", "litters_per_year_n")])
# Calculate litter_size_per_year 
final$life.hist.offspring_per_year_n <- final$life.hist.litter_size_n * final$life.hist.litters_per_year_n

# Calculate averaged disp. dist. when several available
idx <- table(final$SpeciesName)
toclean <- final[!(final$SpeciesName %in% names(idx)[idx == 1]),]
final <- final[final$SpeciesName %in% names(idx)[idx == 1],]

for (s in names(idx)[idx != 1]) {
  subdata <- toclean[toclean$SpeciesName == s, ]
  tokeep <- subdata[, colnames(subdata) != 'dispersal_km']
  tokeep <- dplyr::distinct(tokeep)
  tokeep$dispersal_km <- mean(na.omit(subdata$dispersal_km))
  final <- rbind(final, tokeep)
  }

# Dispersal distance estimated by Dirk Karger for FutureWeb
lst.files <- list.files(here::here('data/Original/Traits/futureweb_ImputedDD/data/'))
lst.files <- lst.files[grep('imputation', lst.files)]
lst.files <- lst.files[-grep('statistics', lst.files)]
dd.imput <- data.frame()
for (l in lst.files) {
  dta <- read.delim(here::here(paste0('data/Original/Traits/futureweb_ImputedDD/data/', l)), sep = ' ')
  dd.imput <- rbind(dd.imput, dta)
}
idx <- table(dd.imput$spec_name)
idx <- idx[idx>1]
dd.imput.ok <- dd.imput[!(dd.imput$spec_name %in% names(idx)), ]

for (nme in names(idx)) {
  tocheck <- dd.imput[dd.imput$spec_name == nme, ]
  tocheck <- tocheck[!is.na(tocheck$Sp.code),]
  dd.imput.ok <- rbind(dd.imput.ok, tocheck)  
}

dd.missing <- final[is.na(final$dispersal_km),]
for (i in 1:nrow(dd.missing)){
  sp.nme <- dd.missing$SpeciesName[i]
  val <- dd.imput.ok$max_disp_m[dd.imput.ok$spec_name == sp.nme]/1000
  if (length(val)!= 0) {dd.missing$dispersal_km[i] <- val}
}
final <- final[!is.na(final$dispersal_km),]
final <- rbind(final, dd.missing)

summary(final)

# Calculate diet breadth and habitat breadth 
val <- final[, grep('diet.', colnames(final))]
final$diet.breadth <- apply(val, 1, sum)
val <- final[, grep('hab.pref.', colnames(final))]
final$hab.pref.breadth <- apply(val, 1, sum)
val <- final[, grep('nest.hab.', colnames(final))]
final$nest.hab.breadth <- apply(val, 1, sum)

######## COMPARE TO SNAP SPECIES ##########
# Select vertebrates
vert.snap <- myData[myData$GROUP2_INPN %in% c('Mammifères', 'Reptiles', 'Amphibiens','Oiseaux'),]

# Check if synonyms are available and could help adding some traits 
# lst.sp <- unique(c(final$SpeciesName, final$NAME_IUCN, final$OldSpeciesName_Maiorano))
# syn <- check_syn(vert.snap$LB_NOM_VALIDE_SPE_LEVEL, lst.sp)

# Manually updated species names from comparing 'SpeciesName', 'NAME_IUCN' and 'OldSpeciesName_Maiorano'
toreplace <- data.frame(LB_NOM_VALIDE_SPE_LEVEL = c("Dendrocopos medius", "Dendrocopos minor", "Pelophylax kl. grafi", "Lyrurus tetrix", 'Ovis gmelinii',
                                                   'Bonasa bonasia', 'Muscicapa tyrrhenica', 'Psammodromus edwarsianus', 'Natrix helvetica'), 
                       LB_NOM_VALIDE_SPE_LEVEL_SYN = c("Dendrocoptes medius", "Dryobates minor", "Pelophylax grafi", "Tetrao tetrix", 'Ovis gmelini', 'Tetrastes bonasia', 
                                                       'Muscicapa striata', 'Psammodromus edwardsianus', 'Natrix natrix'))

vert.snap <- dplyr::left_join(vert.snap, toreplace, by = c("LB_NOM_VALIDE_SPE_LEVEL"))
vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN <- ifelse(is.na(vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN), vert.snap$LB_NOM_VALIDE_SPE_LEVEL, vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN)

# Join trait data to SNAP vertebrates 
vert.snap <- dplyr::left_join(vert.snap, final, by = c("LB_NOM_VALIDE_SPE_LEVEL_SYN" = "SpeciesName"))

##############################
### Add pressures 
##############################

# get national red list 
# iucn.nat <- openxlsx::read.xlsx(here::here('data/Original/Pressures/LISTE_ROUGE_FAUNE_200916_Table_Menaces_LRN_v2.xlsx'))
# # TAXREF to add species name and CD_REF
# taxref.full <- utils::read.csv(here::here('data/Original/Taxa/TAXREFv16.csv'), header = T, sep = ";")
# taxref <- taxref.full[taxref.full$CD_NOM %in% iucn.nat$CD_NOM, ]
# taxref <- taxref[, c('CD_NOM','CD_REF')]
# colnames(taxref)[colnames(taxref) == "CD_REF"] <- "CD_REF_V16TAXREF"
# iucn.nat <- dplyr::left_join(iucn.nat, taxref, by = 'CD_NOM')
# iucn.nat <- iucn.nat[!is.na(iucn.nat$CD_REF_V16TAXREF),]
# iucn.nat <- iucn.nat[iucn.nat$Code_Menaces != '', ]
# iucn.nat <- iucn.nat[iucn.nat$CD_REF_V16TAXREF %in% c(vert.snap$CD_REF_SPE_LEVEL), ]
# 
# # get European red list (to run once)
# lst.sp <- unique(vert.snap$NAME_IUCN[!is.na(vert.snap$NAME_IUCN)])
# press.euro <- data.frame()
# pb <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
#                             max = length(lst.sp), # Maximum value of the progress bar
#                             style = 3,    # Progress bar style (also available style = 1 and style = 2)
#                             width = 50,   # Progress bar width. Defaults to getOption("width")
#                             char = "=")
# 
# for (s in lst.sp) {
# 
#   thre <- rredlist::rl_threats(name = s, region = 'europe', key = 'd434eb1c13e1c936fe9d9961f7f197ebf4b4c5d2cbd4a2e53608ea965c3f052e')
#   thre <- thre$result
#   thre$NAME_IUCN <- s
#   if (length(thre)>1) {
#     press.euro <- rbind(press.euro, thre)
#   }
#   utils::setTxtProgressBar(pb, which(s == lst.sp))
# }
# 
# openxlsx::write.xlsx(press.euro, here::here('data/Modified/EuropeanRedList_IUCNPressures.xlsx'))

press.euro <- openxlsx::read.xlsx(here::here('data/Modified/EuropeanRedList_IUCNPressures.xlsx'))
# Extract pressures for SNAP species 
agri <- c('2.1','2.1.1','2.1.2','2.1.3','2.1.4','2.3','2.3.1','2.3.2','2.3.3','2.3.4')
sylvi <- c('5.3','5.3.1','5.3.2','5.3.3','5.3.4', '2.2','2.2.1','2.2.2','2.2.3')
urba <- c('1','1.1','1.2','1.3')
lin.struc <- c('4.1','4.3')
cc <- c('11.2', '11.3','11.4')

vert.snap$pressure.agri <- NA
vert.snap$pressure.sylvi <- NA
vert.snap$pressure.urba <- NA
vert.snap$pressure.lin_struc <- NA
vert.snap$pressure.cc <- NA

for (s in vert.snap$CD_REF_SPE_LEVEL) {
  
  subdata <- iucn.nat[iucn.nat$CD_REF_V16TAXREF == s,]
  
  if (nrow(subdata)>0) {
    
    vert.snap$pressure.agri[vert.snap$CD_REF_SPE_LEVEL %in% s] <- sum(subdata$Code_Menaces %in% agri)
    vert.snap$pressure.agri[vert.snap$CD_REF_SPE_LEVEL %in% s] <- ifelse(vert.snap$pressure.agri[vert.snap$CD_REF_SPE_LEVEL %in% s]>0, 1, 0)
    vert.snap$pressure.sylvi[vert.snap$CD_REF_SPE_LEVEL %in% s] <- sum(subdata$Code_Menaces %in% sylvi)
    vert.snap$pressure.sylvi[vert.snap$CD_REF_SPE_LEVEL %in% s] <- ifelse(vert.snap$pressure.sylvi[vert.snap$CD_REF_SPE_LEVEL %in% s]>0, 1, 0)
    vert.snap$pressure.urba[vert.snap$CD_REF_SPE_LEVEL %in% s] <- sum(subdata$Code_Menaces %in% urba)
    vert.snap$pressure.urba[vert.snap$CD_REF_SPE_LEVEL %in% s] <- ifelse(vert.snap$pressure.urba[vert.snap$CD_REF_SPE_LEVEL %in% s]>0, 1, 0)
    vert.snap$pressure.lin_struc[vert.snap$CD_REF_SPE_LEVEL %in% s] <- sum(subdata$Code_Menaces %in% lin.struc)
    vert.snap$pressure.lin_struc[vert.snap$CD_REF_SPE_LEVEL %in% s] <- ifelse(vert.snap$pressure.lin_struc[vert.snap$CD_REF_SPE_LEVEL %in% s]>0, 1, 0)
    vert.snap$pressure.cc[vert.snap$CD_REF_SPE_LEVEL %in% s] <- sum(subdata$Code_Menaces %in% cc)
    vert.snap$pressure.cc[vert.snap$CD_REF_SPE_LEVEL %in% s] <- ifelse(vert.snap$pressure.cc[vert.snap$CD_REF_SPE_LEVEL %in% s]>0, 1, 0)
    
  }
    
  nme <- vert.snap$NAME_IUCN[vert.snap$CD_REF_SPE_LEVEL == s]
    
    if (!is.na(nme)) {
      
    subdata <- press.euro[press.euro$NAME_IUCN == nme,]
    
    if (nrow(subdata)>0) {
      
      vert.snap$pressure.agri[vert.snap$NAME_IUCN %in% nme] <- sum(na.omit(c(subdata$code %in% agri, vert.snap$pressure.agri[vert.snap$NAME_IUCN %in% nme])))
      vert.snap$pressure.agri[vert.snap$NAME_IUCN %in% nme] <- ifelse(vert.snap$pressure.agri[vert.snap$NAME_IUCN %in% nme]>0, 1, 0)
      vert.snap$pressure.sylvi[vert.snap$NAME_IUCN %in% nme] <- sum(na.omit(c(subdata$code %in% sylvi, vert.snap$pressure.sylvi[vert.snap$NAME_IUCN %in% nme])))
      vert.snap$pressure.sylvi[vert.snap$NAME_IUCN %in% nme] <- ifelse(vert.snap$pressure.sylvi[vert.snap$NAME_IUCN %in% nme]>0, 1, 0)
      vert.snap$pressure.urba[vert.snap$NAME_IUCN %in% nme] <- sum(na.omit(c(subdata$code %in% urba, vert.snap$pressure.urba[vert.snap$NAME_IUCN %in% nme])))
      vert.snap$pressure.urba[vert.snap$NAME_IUCN %in% nme] <- ifelse(vert.snap$pressure.urba[vert.snap$NAME_IUCN %in% nme]>0, 1, 0)
      vert.snap$pressure.lin_struc[vert.snap$NAME_IUCN %in% nme] <- sum(na.omit(c(subdata$code %in% lin.struc, vert.snap$pressure.lin_struc[vert.snap$NAME_IUCN %in% nme])))
      vert.snap$pressure.lin_struc[vert.snap$NAME_IUCN %in% nme] <- ifelse(vert.snap$pressure.lin_struc[vert.snap$NAME_IUCN %in% nme]>0, 1, 0)
      vert.snap$pressure.cc[vert.snap$NAME_IUCN %in% nme] <- sum(na.omit(c(subdata$code %in% cc, vert.snap$pressure.cc[vert.snap$NAME_IUCN %in% nme])))
      vert.snap$pressure.cc[vert.snap$NAME_IUCN %in% nme] <- ifelse(vert.snap$pressure.cc[vert.snap$NAME_IUCN %in% nme]>0, 1, 0)
      
    }
    
    }
  }


##########################
# Final manual updates 
########################## 

# Move mode
vert.snap$mov.mode.crawler[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Discoglossus sardus'] <- 0 # https://doris.ffessm.fr/Especes/Discoglossus-spp.-Discoglosse-sarde-et-discoglosse-corse-3629 ; proche de Alytes obstetricans et Bombina variegata 
vert.snap$mov.mode.swimmer[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Discoglossus sardus'] <- 1
vert.snap$mov.mode.walker[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Discoglossus sardus'] <- 1

vert.snap$mov.mode.crawler[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Salamandra lanzai'] <- 0 #https://inpn.mnhn.fr/espece/cd_nom/699127
vert.snap$mov.mode.swimmer[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Salamandra lanzai'] <- 1
vert.snap$mov.mode.walker[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Salamandra lanzai'] <- 1

vert.snap$mov.mode.crawler[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Salamandra atra'] <- 0 #https://inpn.mnhn.fr/espece/cd_nom/701819
vert.snap$mov.mode.swimmer[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Salamandra atra'] <- 1
vert.snap$mov.mode.walker[vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN == 'Salamandra atra'] <- 1

# Life-hist (Numb. offspring) & pressure (species for which we know we have occurrence data but missing traits)
updates <- openxlsx::read.xlsx(here::here('data/Original/Traits/ManualImplementation-of-missing-traits.xlsx'))
updates <- updates[, colnames(updates) %in% colnames(vert.snap)]
vert.snap <- vert.snap[!(vert.snap$CD_REF_SPE_LEVEL %in% updates$CD_REF_SPE_LEVEL),]
vert.snap <- rbind(vert.snap, updates) 

# Age first repro
mod <- lm(vert.snap$life.hist.age_first_reproduction_d ~ vert.snap$life.hist.maturity_d)
summary(mod)
vert.snap$life.hist.age_first_reproduction_d[is.na(vert.snap$life.hist.age_first_reproduction_d)] <- 85.6 + 1.03*vert.snap$life.hist.maturity_d[is.na(vert.snap$life.hist.age_first_reproduction_d)]

# Save final DF 
openxlsx::write.xlsx(vert.snap, here::here('data/Modified/SNAP-Vertebrate-Species_ActT-Diet-ForagS-NestH-Morpho-HabPref-DispD-MoveMod-LifeHist-PressureTraits.xlsx'))


#######################################
# OTHER DATABASES NOT USED 
#######################################
# Prep. ReptileTraits 
rept.mov <- readxl::read_excel(here::here("data/Original/Traits/ReptileTraits/Movement Query.xlsx"))
rept.mov <- rept.mov[!is.na(rept.mov$`max movement high`),]

# Prep. GABB 
gabb <- readxl::read_excel(here::here("data/Original/Traits/GABB/GABB_Global_Alpine_Breeding_Bird_dataset.xlsx"), 
                           sheet = "Global alpine breeding birds")

# Prep. amphiBIO
amphibio <- readr::read_csv(here::here("data/Original/Traits/AmphiBIO/AmphiBIO_v1.csv"))
idx <- colnames(amphibio)[c(5:23, 26, 29, 38)] #removed reproductive traits, ok?
amphibio <- amphibio[, colnames(amphibio) %in% idx]

# Prep. TETRA_EU
tetra.db = prep_tetra()

# Prep. AnimalTraits
animalT <- readxl::read_excel("data/Original/Traits/AnimalTraits/observations.xlsx")
animalT <- animalT[, c('species', 'body mass', 'body mass - units')]

# Prep. CESTE 
ceste.md <- readxl::read_excel("data/Original/Traits/CESTES/286_2_CESTES_metadata.xlsx")
ceste.md <- ceste.md[ceste.md$`Taxonomic group` == 'Amphibians',]
files = paste0(ceste.md$DatasetName, '_AJ.xlsx')

# Prep. DarkCiseS ; bats 
darkcise <- readr::read_csv(here::here("data/Original/Traits/DarkCiseS/V1/Dataset1.csv"))
darkcise <- darkcise[, c(4, 7:12, 30:51)]
colnames(darkcise) = gsub(' ', '_', colnames(darkcise))

# Prep. HerbiTraits 
herbi <- readr::read_csv(here::here("data/Original/Traits/HerbiTraits/HerbiTraits_1.2.csv"))
herbi[herbi$Binomial %in% mammal$LB_NOM_VALIDE_SPE_LEVEL,]

##########################
# Insects (N = 684)
##########################
  # DISPERSE (genus level), LepTraits, EuropeButterfly, GlobalAnts, ARTHRO8, Carabids, BETSI 

insect <- myData[myData$GROUP2_INPN %in% 'Insectes', ]

# Prep. DISPERSE 
disp = prep_disperse()
disp <- disp[, !(colnames(disp) %in% c('Group', "Family", "Tribe/sub-family (if coded at this level)", "Synonyms", "Lowest taxonomic level"))]

# Prep. LepTraits
lep <- readr::read_csv(here::here("data/Original/Traits/LepTraits/V1/consensus/consensus.csv"))
lep <- lep[, colnames(lep) %in% c("Species","WS_L", "WS_U", "FW_L", "FW_U",
                                  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                                  "FlightDuration", "DiapauseStage", "Voltinism", "OvipositionStyle",         
                                  "CanopyAffinity", "EdgeAffinity" , "MoistureAffinity", "DisturbanceAffinity", 
                                  "NumberOfHostplantFamilies", "SoleHostplantFamily", "PrimaryHostplantFamily", "SecondaryHostplantFamily", 
                                  "EqualHostplantFamily","NumberOfHostplantAccounts")]
# 2 species from our list with multiple traits, Minois dryas and Pyrgus cirsii

# Prep. EuropeButterfly 
euro.butt <- readxl::read_excel(here::here("data/Original/Traits/EuropeButterfly/European_&_Maghreb_Butterfly_Trait_data_v1.2.xlsx"), 
                               sheet = "Traits table")
euro.butt$Taxon <- gsub('_', ' ',euro.butt$Taxon)
trait.code <- readxl::read_excel(here::here("data/Original/Traits/EuropeButterfly/European_&_Maghreb_Butterfly_Trait_data_v1.2.xlsx"), 
                                 sheet = "Trait_codes")

# Join to species list
insect.lep <- dplyr::left_join(insect[insect$GROUP3_INPN == 'Lépidoptères',], lep, by = c('LB_NOM_VALIDE_SPE_LEVEL'='Species'))
insect.lep <- dplyr::left_join(insect.lep, euro.butt, by = c("LB_NOM_VALIDE_SPE_LEVEL" = "Taxon"))
insect$genus <- get_genus(insect$LB_NOM_VALIDE_SPE_LEVEL)
disp <- disp[disp$`Genus (if coded at this level)` %in% unique(insect$genus),]
disp <- disp[!is.na(disp$`Genus (if coded at this level)`),]
insect <- dplyr::left_join(insect, disp, by = c("genus" = "Genus (if coded at this level)"))


# Prep. GlobalAnts 
# JDD requested 

# Prep. ARTHRO8
arthro8 <- readxl::read_excel(here::here('data/Original/Traits/ARTHRO8/ArthropodSpeciesTraits.xlsx'))
arthro8 <- arthro8[, !(colnames(arthro8)%in% c('Order','Suborder','Family','Author','Remark'))]
idx = (insect$LB_NOM_VALIDE_SPE_LEVEL %in% arthro8$SpeciesID) #only 2... 


#################################
# SPIDERS (N = 37)
#################################
  #WorldSpiderDatabase, EuroSubSpiders

spider <- myData[myData$GROUP2_INPN %in% 'Arachnides',]

# Prep. WorldSpiderDatabase 
check <- arakno::checknames(tax = spider$LB_NOM_VALIDE_SPE_LEVEL)
wsd <- arakno::traits(tax = spider$LB_NOM_VALIDE_SPE_LEVEL[!(spider$LB_NOM_VALIDE_SPE_LEVEL %in% check[,1])])
wsd <- wsd[!is.na(wsd$value), ]
wsd <- wsd[!(wsd$lifeStage.name %in% 'Juvenile'), ]

# Check what traits are available 
wsd.db <- as.data.frame(matrix(ncol = length(unique(wsd$trait.abbrev))+1))
colnames(wsd.db) <- c('Species', unique(wsd$trait.abbrev))
for (sp in unique(wsd$originalName)) {
  subdata <- wsd[wsd$originalName == sp, ]
  subdata <- subdata[!is.na(subdata$value), ]
  tobind <- data.frame(Species = sp, t(unique(wsd$trait.abbrev) %in% unique(subdata$trait.abbrev)))
  colnames(tobind) = c('Species', unique(wsd$trait.abbrev))
  wsd.db <- rbind(wsd.db, tobind)
}
wsd.db = wsd.db[-1, ]

apply(wsd.db[, -1],1, sum)
sort(apply(wsd.db[, -1],2, sum))

lst.trait = c( 'moi2', 'lig2',  'bole', 'guil')
trait.code <- wsd[, c("trait.abbrev", "trait.name")]
trait.code <- dplyr::distinct(trait.code)
trait.code[trait.code$trait.abbrev %in% colnames(wsd.db),]
trait.code[trait.code$trait.abbrev %in% lst.trait,]

# The most common 
wsd <- wsd[wsd$trait.abbrev %in% lst.trait,]
final.db <- data.frame()
for (sp in unique(wsd$originalName)) {
  subdata <- wsd[wsd$originalName == sp, ]
  
  bole = mean(as.numeric(subdata$value[subdata$trait.abbrev == 'bole']))
  if (length(bole) == 0) {bole = NA}
  guil = unique(subdata$value[subdata$trait.abbrev == 'guil'])
  if (length(guil) == 0) {guil = NA}
  moi2 = unique(subdata$value[subdata$trait.abbrev == 'moi2'])
  if (length(moi2) == 0) {moi2 = NA}
  lig2 = unique(subdata$value[subdata$trait.abbrev == 'lig2'])
  if (length(lig2) == 0) {lig2 = NA}
  
  final.db <- rbind(final.db, expand.grid(Species = sp, bole = bole, guil = guil, moi2 = moi2, lig2 = lig2))  
}

spider = dplyr::left_join(spider, final.db, by = c("LB_NOM_VALIDE_SPE_LEVEL" = "Species"))

# Prep. EuroSubSpiders
euro.subspi = readxl::read_excel(here::here("data/Original/Traits/EuroSubSpiders/Database_Mammola_et_al_2022_Figshare_V2.xlsx"))
sum(spider$LB_NOM_VALIDE_SPE_LEVEL %in% euro.subspi$Genus_species)
# Que les espèces qui vivent dans des caves ... 


############################
## Other invertebrates 
############################
   # DISPERSE, Carabids?, BETSI  
inver <- myData[myData$GROUP2_INPN %in% c('Bivalves', 'Crustacés', 'Entognathes', 'Gastéropodes',  
                                          'Annélides', 'Myriapodes', 'Plathelminthes'), ]

# Prep. DISPERSE 
disp = prep_disperse()
disp <- disp[, !(colnames(disp) %in% c('Group', "Family", "Tribe/sub-family (if coded at this level)", "Synonyms", "Lowest taxonomic level"))]

# Join to species list 
inver$genus <- get_genus(inver$LB_NOM_VALIDE_SPE_LEVEL)
disp <- disp[disp$`Genus (if coded at this level)` %in% unique(inver$genus),]
inver <- dplyr::left_join(inver, disp, by = c("genus" = "Genus (if coded at this level)"))

############################
# Plants
############################
# Bryidae, Angiospermes, Gymnospermes, Hépatiques et Anthocérotes, Ptéridophytes,
# Lichens

############################
# Fungus
############################
   # FungalTraits 

fungus <- myData[myData$GROUP2_INPN %in% c('Autres'), ]

# Prep. FungalTraits
fung.t1 <- microeco::fungi_func_FungalTraits
fung.t2 <- microeco::fungi_func_FUNGuild
fung.t2 <- fung.t2[, c('taxon', 'trophicMode', 'guild','growthForm')]

# Join to species list 
fungus$genus <- get_genus(fungus$LB_NOM_VALIDE_SPE_LEVEL)
fungus <- dplyr::left_join(fungus, fung.t2, by = c("genus"="taxon"))
