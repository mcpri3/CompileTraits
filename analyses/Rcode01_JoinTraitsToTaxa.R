
# rredlist package 

################################################
######### Prep. lst of SNAP species ############
################################################
myData <- readxl::read_excel(here::here("data/Original/Taxa/SNAPTaxons_Representativite_Mlx.xlsx"), 
                             sheet = "Liste_Taxons")
colnames(myData)[colnames(myData) == 'CD_REF'] = 'CD_NOM'

# TAXREF to add the rank
taxref.full <- utils::read.csv(here::here('data/Original/Taxa/TAXREFv16.csv'), header = T, sep = ";")
taxref <- taxref.full[taxref.full$CD_NOM %in% myData$CD_NOM, ]
taxref <- taxref[, c('CD_NOM','CD_REF', 'RANG', 'GROUP3_INPN', 'NOM_VERN')]
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
openxlsx::write.xlsx(myData, here::here('data/Original/Taxa/SNAPTaxons_Representativite_Mlx_SpeciesLevel.xlsx'))
myData <- myData[, c("GROUP2_INPN", "GROUP3_INPN","CD_REF_SPE_LEVEL","LB_NOM_VALIDE_SPE_LEVEL")]
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

#################################
# Dispersal distance
#################################

########### MAMMALS ###############
mammals <- vert.trait[vert.trait$Class == 'Mammalia',]

# Prep. COMBINE ; mammals
comb <- readr::read_csv(here::here("data/Original/Traits/COMBINE/trait_data_imputed.csv"))
comb1 <- comb[, c('iucn2020_binomial', "dispersal_km" )]
colnames(comb1) <- c('species', 'dispersal_km')
comb2 <- comb[, c('phylacine_binomial', "dispersal_km" )]
colnames(comb2) <- c('species', 'dispersal_km')
comb <- rbind(comb1, comb2)
comb <- dplyr::distinct(comb)

# Check if synonyms are available and could help adding some traits 
syn <- check_syn(mammals$SpeciesName, comb$species)
syn$inDB <- syn$Synonym %in% comb$species
syn <- syn[syn$inDB, ]

syn <- check_syn(mammals$NAME_IUCN, comb$species)
syn$inDB <- syn$Synonym %in% comb$species
syn <- syn[syn$inDB, ]

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


############## REPTILES ##################
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
reptiles$NAME_IUCN[idx] <- NA
syn$inDB <- syn$Synonym %in% escoriza$Species
syn <- syn[syn$inDB, ]
reptiles <- dplyr::left_join(reptiles, syn[, c('Species','Synonym')], by = c('NAME_IUCN' = 'Species') )
reptiles$Species_Syn <- ifelse(is.na(reptiles$Synonym), reptiles$NAME_IUCN, reptiles$Synonym)

# Check other name columns
idx = !(reptiles$Species_Syn %in% escoriza$Species)
sum(reptiles$SpeciesName[idx] %in% escoriza$Species)
sum(reptiles$OldSpeciesName_Maiorano[idx] %in% escoriza$Species)

# Join to species list
reptiles <- dplyr::left_join(reptiles, escoriza, by = c("Species_Syn"="Species"))

############ BIRDS ################
birds <- vert.trait[vert.trait$Class == 'Aves',]

# Prep. AVONET
avonet1 <-readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET1_Birdlife.xlsx"), 
                             sheet = "AVONET1_BirdLife")
avonet2 <- readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET2_eBird.xlsx"), 
                              sheet = "AVONET2_eBird")
avonet3 <- readxl::read_excel(here::here("data/Original/Traits/AVONET/AVONET3_BirdTree.xlsx"), 
                              sheet = "AVONET3_BirdTree")
col.tokeep <- c( "Hand-Wing.Index")
avonet1 <- avonet1[, c('Species1', col.tokeep)]
avonet2 <- avonet2[, c('Species2', col.tokeep)]
avonet3 <- avonet3[, c('Species3', col.tokeep)]
newcolnms <- c('Species', 'HWI')
colnames(avonet1) <- newcolnms
colnames(avonet2) <- newcolnms
colnames(avonet3) <- newcolnms
avonet <- rbind(avonet1, avonet2, avonet3)
avonet <- dplyr::distinct(avonet)
avonet$dispersal_km <- exp(2.70 + 0.86 * (avonet$HWI - mean(avonet$HWI))/(2*sd(avonet$HWI)))
avonet <- avonet[, c('Species', 'dispersal_km')]

# No need to check syn because they are all in avonet
birds$Species_Syn <- birds$SpeciesName

# Join to species list
birds <- dplyr::left_join(birds, avonet, by = c("Species_Syn"="Species"))

############ AMPHIBIANS ################
amphi <- vert.trait[vert.trait$Class == 'Amphibia',]

# Prep. Trochet DB
trochet <- readxl::read_excel(here::here("data/Original/Traits/Trochet_et_al_2014_Amphibians/biodiversity_data_journal-2-e4123-s001.xlsx"), 
                              skip = 3)
trochet <- trochet[trochet$`Maximum dispersal distance`!= 'DD',]
trochet$`Maximum dispersal distance` <- as.numeric(trochet$`Maximum dispersal distance`)/1000
trochet <- trochet[, c("Species", "Maximum dispersal distance")]
colnames(trochet)[2] <- 'dispersal_km'

# Check if synonyms are available and could help adding some traits 
idx <- is.na(amphi$NAME_IUCN)
amphi$NAME_IUCN[idx] <- amphi$SpeciesName[idx]
syn <- check_syn(amphi$NAME_IUCN, trochet$Species)
amphi$NAME_IUCN[idx] <- NA
syn$inDB <- syn$Synonym %in% trochet$Species
syn <- syn[syn$inDB, ]
amphi <- dplyr::left_join(amphi, syn[, c('Species','Synonym')], by = c('NAME_IUCN'='Species'))
amphi$Species_Syn <- ifelse(is.na(amphi$Synonym), amphi$NAME_IUCN, amphi$Synonym)

# Check other name columns
idx = !(amphi$Species_Syn %in% trochet$Species)
sum(amphi$SpeciesName[idx] %in% trochet$Species)
sum(amphi$OldSpeciesName_Maiorano[idx] %in% trochet$Species)
# update
amphi$Species_Syn[amphi$SpeciesName == "Pelophylax esculentus"] <- "Pelophylax esculentus"

# Join to species list
amphi <- dplyr::left_join(amphi, trochet, by = c("Species_Syn"="Species"))

####### COMBINE THE FOUR DATAFRAMES ########
col.tokeep <- intersect(colnames(mammals), colnames(birds))
final <- rbind(mammals[, col.tokeep], birds[, col.tokeep], amphi[, col.tokeep], reptiles[, col.tokeep])
final <- final[, !(colnames(final) %in% 'Species_Syn')]

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

summary(final)

######## COMPARE TO SNAP SPECIES ##########
# Select vertebrates
vert.snap <- myData[myData$GROUP2_INPN %in% c('Mammifères', 'Reptiles', 'Amphibiens','Oiseaux'),]

# Check if synonyms are available and could help adding some traits 
# lst.sp <- unique(c(final$SpeciesName, final$NAME_IUCN, final$OldSpeciesName_Maiorano))
# syn <- check_syn(vert.snap$LB_NOM_VALIDE_SPE_LEVEL, lst.sp)

# Manually updated species names from comparing 'SpeciesName', 'NAME_IUCN' and 'OldSpeciesName_Maiorano'
toreplace <- data.frame(LB_NOM_VALIDE_SPE_LEVEL = c("Dryobates minor", "Dendrocopos minor", "Pelophylax kl. grafi", "Lyrurus tetrix", 'Ovis gmelinii',
                                                   'Bonasa bonasia', 'Muscicapa tyrrhenica', 'Psammodromus edwarsianus'), 
                       LB_NOM_VALIDE_SPE_LEVEL_SYN = c("Dendrocopos minor", "Dryobates minor", "Pelophylax grafi", "Tetrao tetrix", 'Ovis gmelini', 'Tetrastes bonasia', 
                                                       'Muscicapa striata', 'Psammodromus edwardsianus'))
vert.snap <- dplyr::left_join(vert.snap, toreplace, by = c("LB_NOM_VALIDE_SPE_LEVEL"))
vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN <- ifelse(is.na(vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN), vert.snap$LB_NOM_VALIDE_SPE_LEVEL, vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN)

# Join trait data to SNAP vertebrates 
vert.snap <- dplyr::left_join(vert.snap, final, by = c("LB_NOM_VALIDE_SPE_LEVEL_SYN" = "SpeciesName"))

# # Compare to European species list for NaturaConnect 
# # Vertebrates only 
# euro <- readr::read_csv("data/Original/Taxa/EU_species_RedList_Art12_Art17.csv")
# euro <- euro[euro$Class %in% c('MAMMALIA', 'REPTILIA', 'AMPHIBIA','AVES'),]
# syn <- check_syn(vert.snap$LB_NOM_VALIDE_SPE_LEVEL, euro$speciesname.EU.RedList)
# syn$inDB <- syn$Synonym %in% euro$speciesname.EU.RedList
# syn <- syn[syn$inDB, ]
# vert.snap <- dplyr::left_join(vert.snap, syn[, c('Species','Synonym')], by = c("LB_NOM_VALIDE_SPE_LEVEL" = "Species"))
# vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN <- ifelse(is.na(vert.snap$Synonym), vert.snap$LB_NOM_VALIDE_SPE_LEVEL, vert.snap$Synonym)
# vert.snap$inEuropeList <- vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN %in% euro$speciesname.EU.RedList
# 
# # All taxa
# euro <- readr::read_csv("data/Original/Taxa/EU_species_RedList_Art12_Art17.csv")
# syn <- check_syn(myData$LB_NOM_VALIDE_SPE_LEVEL, euro$speciesname.EU.RedList)
# syn$inDB <- syn$Synonym %in% euro$speciesname.EU.RedList
# syn <- syn[syn$inDB, ]
# myData <- dplyr::left_join(myData, syn[, c('Species','Synonym')], by = c("LB_NOM_VALIDE_SPE_LEVEL" = "Species"))
# myData$LB_NOM_VALIDE_SPE_LEVEL_SYN <- ifelse(is.na(myData$Synonym), myData$LB_NOM_VALIDE_SPE_LEVEL, myData$Synonym)
# myData$inEuropeList <- myData$LB_NOM_VALIDE_SPE_LEVEL_SYN %in% euro$speciesname.EU.RedList

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
