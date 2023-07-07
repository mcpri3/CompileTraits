source(here::here('analyses/Rscript_SearchTraits_NLG.R'))

full.tt.1 <- get.traits(sciName = vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN[1:100])
full.tt.2 <- get.traits(sciName = vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN[101:200])
full.tt.3 <- get.traits(sciName = vert.snap$LB_NOM_VALIDE_SPE_LEVEL_SYN[201:285])
full.tt <- rbind(full.tt.1, full.tt.2, full.tt.3)

mean.dd <- full.tt[full.tt$trait %in% 'mean dispersal distance',]
mean.dd$trait_value <- as.numeric(mean.dd$trait_value)
mean.dd$trait_value[mean.dd$unit == 'meter'] <- mean.dd$trait_value[mean.dd$unit == 'meter']/1000
mean.dd <- mean.dd[, c("queryName", "trait_value")]

final = data.frame()
for (s in unique(mean.dd$queryName)) {
  sub <- mean.dd[mean.dd$queryName == s, ]
  if (nrow(sub)>1) {
    sub <- data.frame(queryName = s, trait_value = mean(sub$trait_value))
  }
  final <- rbind(final, sub)
}

test = vert.snap[, c("LB_NOM_VALIDE_SPE_LEVEL_SYN", "dispersal_km")]
test <- left_join(test, final, by = c("LB_NOM_VALIDE_SPE_LEVEL_SYN"="queryName"))
length(unique(mean.dd$queryName))


max.dd <- full.tt[full.tt$trait %in% 'maximum dispersal distance',]
max.dd$trait_value <- as.numeric(max.dd$trait_value)
max.dd <- max.dd[, c("queryName", "trait_value")]
c <- c[, c('LB_NOM_VALIDE_SPE_LEVEL_SYN', 'dispersal_km')]
c <- left_join(c, max.dd, by = c("LB_NOM_VALIDE_SPE_LEVEL_SYN"="queryName"))
