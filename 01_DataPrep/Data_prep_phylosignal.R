library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(plyr)
library(picante)
library(geiger)
library(ape)
library(phytools)
library(TNRS)

#Elevation shifts for all communities combined-----------
#bring in data
elev_shift <- read.csv("Data/elevation_shift_signal.csv")

##make sure all species names match S&B-------------
#add underscores to species names
elev_shift$Species <- gsub(" ", "_", elev_shift$Species)

#bring in S&B taxa list
SBtree <- read.tree("Data/ALLMB.tre")

SB_list <- SBtree$tip.label
SB_list <- as.data.frame(SB_list)

#show missing species
missing_species <- elev_shift %>% filter(!Species %in% SB_list$SB_list)
missing_to_check <- as.character(missing_species$Species)

#check for alternate names in TNRS
TNRS(missing_to_check)

#replace species names
elev_shift$Species[elev_shift$Species == 'Agoseris_glauca'] <- 'Agoseris_glauca_var._dasycephala'
elev_shift$Species[elev_shift$Species == 'Acomastylis_rossii'] <- 'Geum_rossii'
elev_shift$Species[elev_shift$Species == 'Chrysothamnus_spp.'] <- 'Chrysothamnus_viscidiflorus'
elev_shift$Species[elev_shift$Species == 'Castilleja_linariaefolia'] <- 'Castilleja_linariifolia'
elev_shift$Species[elev_shift$Species == 'Tolmachevia_integrifolia'] <- 'Rhodiola_integrifolia'
elev_shift$Species[elev_shift$Species == 'Mahonia_repens'] <- 'Berberis_repens'
elev_shift$Species[elev_shift$Species == 'Bromelica_spectabilis'] <- 'Melica_spectabilis'
elev_shift$Species[elev_shift$Species == 'Pseudocymopterus_montanus'] <- 'Cymopterus_planosus'
elev_shift$Species[elev_shift$Species == 'Bromopsis_ciliata'] <- 'Bromus_ciliatus'
elev_shift$Species[elev_shift$Species == 'Seriphidium_tridentatum'] <- 'Artemisia_tridentata'
elev_shift$Species[elev_shift$Species == 'Trollius_albiflorus'] <- 'Trollius_laxus'
elev_shift$Species[elev_shift$Species == 'Chamerion_danielsii'] <- 'Chamerion_angustifolium'
elev_shift$Species[elev_shift$Species == 'Rydbergia_grandiflora'] <- 'Hymenoxys_grandiflora'
elev_shift$Species[elev_shift$Species == 'Psychrophila_leptosepala'] <- 'Caltha_leptosepala'
elev_shift$Species[elev_shift$Species == 'Calamagrostis_purpurascens'] <- 'Calamagrostis_purpurascens_subsp._purpurascens'
elev_shift$Species[elev_shift$Species == 'Ivesia_gordonii'] <- 'Potentilla_gordonii'
elev_shift$Species[elev_shift$Species == 'Helianthella_quinquenervis'] <- 'Helianthella_uniflora'
elev_shift$Species[elev_shift$Species == 'Hymenoxys_grandiflora'] <- 'Hymenoxys_hoopesii'

#show missing species again
missing_species2 <- elev_shift %>% filter(!Species %in% SB_list$SB_list)

#remove Erigerons and s. crassulus
elev_shift <- elev_shift %>% filter(!Species %in% c("Erigeron_coulteri",
                                                  "Erigeron_glacialis",
                                                  "Erigeron_elatior",
                                                  "Senecio_crassulus"))

shift_df <- elev_shift %>% filter(!Species %in% c("Erigeron_coulteri",
                                  "Erigeron_glacialis",
                                  "Erigeron_elatior",
                                  "Senecio_crassulus"))

##reformat for signal calculation--------
#match order of species in trait data with order in phylogeny
shift_df <- shift_df[ order(match(shift_df$Species, 
                                      SB_list$SB_list)), ]

#match order of species in trait data with order in phylogeny
shift_df <- shift_df[ order(match(shift_df$Species, 
                                  SB_list$SB_list)), ]

#reformat
shift_df <- shift_df %>% remove_rownames %>% column_to_rownames(var="Species")
shift_vec <- df2vec(shift_df, colID=1)


#as vector for trimming phylo
sp_list <- as.vector(elev_shift$Species)

##prune tree------
pruned.tree <- drop.tip(SBtree,SBtree$tip.label[-match(sp_list, SBtree$tip.label)])

plot(pruned.tree)
length(pruned.tree$tip.label)
is.rooted(pruned.tree)
is.binary(pruned.tree)

## remove all lumped genera ---------------
shift_df_removed <- elev_shift %>% filter(!Species %in% "Chrysothamnus_viscidiflorus")

# match order of species in trait data with order in phylogeny
shift_df_removed <- shift_df_removed[ order(match(shift_df_removed$Species, 
                                  SB_list$SB_list)), ]

# reformat
shift_vec_removed <- shift_df_removed %>% remove_rownames %>% column_to_rownames(var="Species")
shift_vec_removed <- df2vec(shift_vec_removed, colID=1)

#as vector for trimming phylo
sp_list <- as.vector(shift_df_removed$Species)

### prune tree ------
pruned.tree.removed <- drop.tip(SBtree,SBtree$tip.label[-match(sp_list, SBtree$tip.label)])

plot(pruned.tree.removed)
length(pruned.tree.removed$tip.label)
is.rooted(pruned.tree)
is.binary(pruned.tree)

# Individual communities -------

# bring in dataset
shift_comm <- read.csv("Data/elevation_shift_communities.csv", header=TRUE, 
                       stringsAsFactors=FALSE, fileEncoding="latin1")


# add underscores to species names
shift_comm$Species <- gsub(" ", "_", shift_comm$Species)

# replace species names
shift_comm$Species[shift_comm$Species == 'Agoseris_glauca'] <- 'Agoseris_glauca_var._dasycephala'
shift_comm$Species[shift_comm$Species == 'Acomastylis_rossii'] <- 'Geum_rossii'
shift_comm$Species[shift_comm$Species == 'Chrysothamnus_spp.'] <- 'Chrysothamnus_viscidiflorus'
shift_comm$Species[shift_comm$Species == 'Castilleja_linariaefolia'] <- 'Castilleja_linariifolia'
shift_comm$Species[shift_comm$Species == 'Tolmachevia_integrifolia'] <- 'Rhodiola_integrifolia'
shift_comm$Species[shift_comm$Species == 'Mahonia_repens'] <- 'Berberis_repens'
shift_comm$Species[shift_comm$Species == 'Bromelica_spectabilis'] <- 'Melica_spectabilis'
shift_comm$Species[shift_comm$Species == 'Pseudocymopterus_montanus'] <- 'Cymopterus_planosus'
shift_comm$Species[shift_comm$Species == 'Bromopsis_ciliata'] <- 'Bromus_ciliatus'
shift_comm$Species[shift_comm$Species == 'Seriphidium_tridentatum'] <- 'Artemisia_tridentata'
shift_comm$Species[shift_comm$Species == 'Trollius_albiflorus'] <- 'Trollius_laxus'
shift_comm$Species[shift_comm$Species == 'Chamerion_danielsii'] <- 'Chamerion_angustifolium'
shift_comm$Species[shift_comm$Species == 'Rydbergia_grandiflora'] <- 'Hymenoxys_grandiflora'
shift_comm$Species[shift_comm$Species == 'Psychrophila_leptosepala'] <- 'Caltha_leptosepala'
shift_comm$Species[shift_comm$Species == 'Calamagrostis_purpurascens'] <- 'Calamagrostis_purpurascens_subsp._purpurascens'
shift_comm$Species[shift_comm$Species == 'Ivesia_gordonii'] <- 'Potentilla_gordonii'
shift_comm$Species[shift_comm$Species == 'Helianthella_quinquenervis'] <- 'Helianthella_uniflora'
shift_comm$Species[shift_comm$Species == 'Hymenoxys_grandiflora'] <- 'Hymenoxys_hoopesii'


# remove Erigerons and s. crassulus
shift_comm <- shift_comm %>% filter(!Species %in% c("Erigeron_coulteri",
                                                    "Erigeron_glacialis",
                                                    "Erigeron_elatior",
                                                    "Senecio_crassulus"))

## add column for change for each plot
shift_comm$Elevation_change <- shift_comm$Mean_elevation_2014-shift_comm$Mean_elevation_1950

## make vectors to calculate signal in elevation shift for each community----------
# these are grouped by 1950 community occupancy, not 2014

## SB #####

# filter for only sagebrush
sb_shift <- shift_comm %>% filter(occupancy_1950 %in% "SB")

# get rid of extra columns
sb_shift <- subset(sb_shift, select = -c(Mean_elevation_1950,
                                         Mean_elevation_2014, 
                                         occupancy_2014, 
                                         occupancy_1950) )

# match order of species in trait data with order in phylogeny
sb_shift <- sb_shift[ order(match(sb_shift$Species, 
                                  SB_list$SB_list)), ]

# reformat
sb_shift_vec <- sb_shift %>% remove_rownames %>% column_to_rownames(var="Species")
sb_shift_vec <- df2vec(sb_shift_vec, colID=1)


# as vector for trimming phylo
sage_list <- as.vector(sb_shift$Species)

## prune tree
pruned.tree.sagebrush <- drop.tip(SBtree,SBtree$tip.label[-match(sage_list, SBtree$tip.label)])
plot(pruned.tree.sagebrush)

### remove lumped genera -------
sb_shift_removed <- sb_shift %>% filter(!Species %in% "Chrysothamnus_viscidiflorus")

# reformat
sb_shift_vec_removed <- sb_shift_removed %>% remove_rownames %>% column_to_rownames(var="Species")
sb_shift_vec_removed <- df2vec(sb_shift_vec_removed, colID=1)

# as vector for trimming phylo
sage_list_rm <- as.vector(sb_shift_removed$Species)

## prune tree
pruned.tree.sagebrush.rm <- drop.tip(SBtree,SBtree$tip.label[-match(sage_list_rm, SBtree$tip.label)])
plot(pruned.tree.sagebrush.rm)

## SF ####

# filter for only spruce-fir
sf_shift <- shift_comm %>% filter(occupancy_1950 %in% "SF")

# get rid of extra columns
sf_shift <- subset(sf_shift, select = -c(Mean_elevation_1950,
                                         Mean_elevation_2014, 
                                         occupancy_2014, 
                                         occupancy_1950) )

# match order of species in trait data with order in phylogeny
sf_shift <- sf_shift[ order(match(sf_shift$Species, 
                                  SB_list$SB_list)), ]

# reformat
sf_shift_vec <- sf_shift %>% remove_rownames %>% column_to_rownames(var="Species")
sf_shift_vec <- df2vec(sf_shift_vec, colID=1)


# as vector for trimming phylo
spruce_list <- as.vector(sf_shift$Species)

## prune tree
pruned.tree.sprucefir <- drop.tip(SBtree,SBtree$tip.label[-match(spruce_list, SBtree$tip.label)])
plot(pruned.tree.sprucefir)

## UH ####
# filter for only upland-herb
uh_shift <- shift_comm %>% filter(occupancy_1950 %in% "UH")

# get rid of extrac columns
uh_shift <- subset(uh_shift, select = -c(Mean_elevation_1950,
                                         Mean_elevation_2014, 
                                         occupancy_2014, 
                                         occupancy_1950) )

# match order of species in trait data with order in phylogeny
uh_shift <- uh_shift[ order(match(uh_shift$Species, 
                                  SB_list$SB_list)), ]

# reformat
uh_shift_vec <- uh_shift %>% remove_rownames %>% column_to_rownames(var="Species")
uh_shift_vec <- df2vec(uh_shift_vec, colID=1)

# as vector for trimming phylo
herb_list <- as.vector(uh_shift$Species)

## prune tree
pruned.tree.uplandherb <- drop.tip(SBtree,SBtree$tip.label[-match(herb_list, SBtree$tip.label)])
plot(pruned.tree.uplandherb)

## A ####
# filter for only alpine
a_shift <- shift_comm %>% filter(occupancy_1950 %in% "A")

# get rid of extrac columns
a_shift <- subset(a_shift, select = -c(Mean_elevation_1950,
                                         Mean_elevation_2014, 
                                         occupancy_2014, 
                                         occupancy_1950) )

# match order of species in trait data with order in phylogeny
a_shift <- a_shift[ order(match(a_shift$Species, 
                                  SB_list$SB_list)), ]

# reformat
a_shift_vec <- a_shift %>% remove_rownames %>% column_to_rownames(var="Species")
a_shift_vec <- df2vec(a_shift_vec, colID=1)

# as vector for trimming phylo
alpine_list <- as.vector(a_shift$Species)

## prune tree
pruned.tree.alpine <- drop.tip(SBtree,SBtree$tip.label[-match(alpine_list, SBtree$tip.label)])
plot(pruned.tree.alpine)

#Abundance shifts with reduced dataset (lumped congeners--------
#and only >14% constancy to match Langenheim methods and data

#bring in data
ab_change <- read.csv("Data/abundance_change_reduced_dataset.csv") 

#add underscores to species names
ab_change$Species <- gsub(" ", "_", ab_change$Species)

#get rid of extra columns 
ab_change = subset(ab_change, select = -c(Rank_abundance_1950,
                                          Rank_abundance_2014,
                                          Constancy_1950, 
                                          Constancy_2014,
                                          Relative_abundance_1950, 
                                          Relative_abundance_2014,
                                          S.B_replacement, 
                                          Additional.species.in.Zorio.genus.groups, 
                                          Addition.species.2,
                                          Additional.species.3))

#show missing species
missing <- ab_change %>% filter(!Species %in% SB_list$SB_list)

#save as csv
write.csv(missing, "missing_reduced_dataset.csv")

#split into lists by community type
sagebrush_ab <- ab_change %>% filter(!Community_type %in% c("Spruce_fir", 
                                                            "Upland_herb",
                                                            "Alpine"))
spruce_ab <- ab_change %>% filter(Community_type %in% "Spruce_fir")
upland_ab <- ab_change %>% filter(Community_type %in% "Upland_herb")
alpine_ab <- ab_change %>% filter(Community_type %in% "Alpine")


## SB #####
sagebrush_ab$Species[sagebrush_ab$Species == 'Arenaria_congesta'] <- 'Eremogone_congesta'
sagebrush_ab$Species[sagebrush_ab$Species == 'Bromopsis_spp.'] <- 'Bromus_frondosus'
sagebrush_ab$Species[sagebrush_ab$Species == 'Chrysothamnus_spp.'] <- 'Chrysothamnus_viscidiflorus'
sagebrush_ab$Species[sagebrush_ab$Species == 'Elymus_spp.'] <- 'Elymus_trachycaulus'
sagebrush_ab$Species[sagebrush_ab$Species == 'Erigeron_spp.'] <- 'Erigeron_speciosus'
sagebrush_ab$Species[sagebrush_ab$Species == 'Lupinus_spp.'] <- 'Lupinus_bakeri'
sagebrush_ab$Species[sagebrush_ab$Species == 'Oligosporus_dracunculus'] <- 'Artemisia_dracunculus'
sagebrush_ab$Species[sagebrush_ab$Species == 'Potentilla_spp.'] <- 'Potentilla_pulcherrima'
sagebrush_ab$Species[sagebrush_ab$Species == 'Seriphidium_tridentatum'] <- 'Artemisia_tridentata'
 
### reformat for signal calculation --------
# get rid of extra columns
sagebrush_ab_sig <- subset(sagebrush_ab, select = -c(Community_type,
                                                         Constancy_change_1950_2014))
#match order of species in trait data with order in phylogeny
sagebrush_ab_sig <- sagebrush_ab_sig[ order(match(sagebrush_ab$Species, 
                                              SB_list$SB_list)), ]
# as vector for trimming phylo
sagebrush_ab_vec <- df2vec(sagebrush_ab_sig, colID=1)
sage_list <- sagebrush_ab_vec

# reformat
sagebrush_ab_sig <- sagebrush_ab_sig %>% remove_rownames %>% 
  column_to_rownames(var="Species")

### prune tree ------
tree.sb.ab <- drop.tip(SBtree,SBtree$tip.label[-match(sage_list, SBtree$tip.label)])

plot(tree.sb.ab)
Ntip(tree.sb.ab)

## SB lumped species removed ---------

#remove grouped 
sb_remove <- c("Bromus_frondosus",
               "Chrysothamnus_viscidiflorus",
               "Elymus_trachycaulus",
               "Erigeron_speciosus",
               "Lupinus_bakeri",
               "Potentilla_pulcherrima")

sagebrush_ab_removed <- sagebrush_ab_sig %>% filter(!Species %in% sb_remove)

# reformat
sagebrush_ab_vec_removed <- sagebrush_ab_removed %>% remove_rownames %>% 
  column_to_rownames(var="Species")
sagebrush_ab_vec_removed <- df2vec(sagebrush_ab_vec_removed, colID=1)

# as vector for trimming phylo
sage_list_rm <- as.vector(sagebrush_ab_removed$Species)

### prune tree ------
tree.sb.ab.rm <- drop.tip(SBtree,SBtree$tip.label[-match(sage_list_rm, SBtree$tip.label)])
                                             
## SF ------------------
spruce_ab$Species[spruce_ab$Species == 'Carex_spp.'] <- 'Carex_geyeri'
spruce_ab$Species[spruce_ab$Species == 'Erigeron_spp.'] <- 'Erigeron_glabellus'
spruce_ab$Species[spruce_ab$Species == 'Lupinus_spp.'] <- 'Lupinus_bakeri'
spruce_ab$Species[spruce_ab$Species == 'Mertensia_spp.'] <- 'Mertensia_ciliata'
spruce_ab$Species[spruce_ab$Species == 'Pedicularis_spp.'] <- 'Pedicularis_racemosa_subsp._alba'
spruce_ab$Species[spruce_ab$Species == 'Polemonium_pulcherrimum'] <- 'Polemonium_pulcherrimum'
spruce_ab$Species[spruce_ab$Species == 'Pseudocymopterus_montanus'] <- 'Cymopterus_planosus'
spruce_ab$Species[spruce_ab$Species == 'Senecio_spp.'] <- 'Senecio_integerrimus_var._exaltatus'
spruce_ab$Species[spruce_ab$Species == 'Vaccinium_spp.'] <- 'Vaccinium_myrtillus'

###reformat for signal calculation--------
#get rid of extra columns
spruce_ab_sig <- subset(spruce_ab, select = -c(Community_type,
                                                     Constancy_change_1950_2014))
#match order of species in trait data with order in phylogeny
spruce_ab_sig <- spruce_ab_sig[ order(match(spruce_ab_sig$Species, 
                                                  SB_list$SB_list)), ]


#reformat
spruce_ab_vec <- spruce_ab_sig %>% remove_rownames %>% 
  column_to_rownames(var="Species")
spruce_ab_vec <- df2vec(spruce_ab_vec, colID=1)

#as vector for trimming phylo
spruce_list <- as.vector(spruce_ab$Species)

### prune tree------
tree.spruce.ab <- drop.tip(SBtree,SBtree$tip.label[-match(spruce_list, SBtree$tip.label)])

plot(tree.spruce.ab)

## spruce fir with lumped genera removed ---------

sf_remove <- c("Carex_geyeri",
               "Erigeron_glabellus",
               "Lupinus_bakeri",
               "Mertensia_ciliata",
               "Pedicularis_racemosa_subsp._alba",
               "Senecio_integerrimus_var._exaltatus",
               "Vaccinium_myrtillus")

spruce_ab_removed <- spruce_ab_sig %>% filter(!Species %in% sf_remove)

#reformat
spruce_ab_vec_rm <- spruce_ab_removed %>% remove_rownames %>% 
  column_to_rownames(var="Species")
spruce_ab_vec_rm <- df2vec(spruce_ab_vec_rm, colID=1)

#as vector for trimming phylo
spruce_list <- as.vector(spruce_ab_removed$Species)

### prune tree ------
tree.spruce.ab.rm <- drop.tip(SBtree,SBtree$tip.label[-match(spruce_list, SBtree$tip.label)])

## UH ########
upland_ab$Species[upland_ab$Species == 'Acomastylis_rossii'] <- 'Geum_rossii'
upland_ab$Species[upland_ab$Species == 'Bromelica_spectabilis'] <- 'Melica_spectabilis'
upland_ab$Species[upland_ab$Species == 'Bromopsis_spp.'] <- 'Bromus_ciliatus'
upland_ab$Species[upland_ab$Species == 'Carex_spp.'] <- 'Carex_aquatilis'
upland_ab$Species[upland_ab$Species == 'Erigeron_spp.'] <- 'Erigeron_glabellus'
upland_ab$Species[upland_ab$Species == 'Helianthella_quinquenervis'] <- 'Helianthella_uniflora'
upland_ab$Species[upland_ab$Species == 'Lupinus_spp.'] <- 'Lupinus_bakeri'
upland_ab$Species[upland_ab$Species == 'Mertensia_spp.'] <- 'Mertensia_ciliata'
upland_ab$Species[upland_ab$Species == 'Poa_spp.'] <- 'Poa_fendleriana'
upland_ab$Species[upland_ab$Species == 'Potentilla_spp.'] <- 'Potentilla_pulcherrima'
upland_ab$Species[upland_ab$Species == 'Salix_spp.'] <- 'Salix_brachycarpa'
upland_ab$Species[upland_ab$Species == 'Senecio_spp.'] <- 'Senecio_integerrimus_var._exaltatus'
upland_ab$Species[upland_ab$Species == 'Vaccinium_spp.'] <- 'Vaccinium_caespitosum'

###reformat for signal calculation--------
#get rid of extra columns
uh_ab_sig <- subset(upland_ab, select = -c(Community_type,
                                               Constancy_change_1950_2014))
#match order of species in trait data with order in phylogeny
uh_ab_sig <- uh_ab_sig[ order(match(uh_ab_sig$Species, 
                                            SB_list$SB_list)), ]


#reformat
uh_ab_vec <- uh_ab_sig %>% remove_rownames %>% 
  column_to_rownames(var="Species")
uh_ab_vec <- df2vec(uh_ab_vec, colID=1)

#as vector for trimming phylo
uh_list <- as.vector(upland_ab$Species)

### prune tree------
tree.uh.ab <- drop.tip(SBtree,SBtree$tip.label[-match(uh_list, SBtree$tip.label)])

plot(tree.uh.ab)
Ntip(tree.uh.ab)

## UH with lumped genera removed -------

uh_remove <- c("Bromus_ciliatus",
               "Carex_aquatilis",
               "Erigeron_glabellus",
               "Lupinus_bakeri",
               "Mertensia_ciliata",
               "Poa_fendleriana",
               "Potentilla_pulcherrima",
               "Salix_brachycarpa",
               "Senecio_integerrimus_var._exaltatus",
               "Vaccinium_caespitosum")

uh_ab_removed <- uh_ab_sig %>% filter(!Species %in% uh_remove)

#reformat
uh_ab_vec_remove <- uh_ab_removed %>% remove_rownames %>% 
  column_to_rownames(var="Species")
uh_ab_vec_remove <- df2vec(uh_ab_vec_remove, colID=1)

#as vector for trimming phylo
uh_list <- as.vector(uh_ab_removed$Species)

### prune tree------
tree.uh.ab.rm <- drop.tip(SBtree,SBtree$tip.label[-match(uh_list, SBtree$tip.label)])

##A#######
alpine_ab$Species[alpine_ab$Species == 'Acomastylis_rossii'] <- 'Geum_rossii'
alpine_ab$Species[alpine_ab$Species == 'Astragalus_spp.'] <- 'Astragalus_robbinsii_var._minor'
alpine_ab$Species[alpine_ab$Species == 'Carex_spp.'] <- 'Kobresia_myosuroides'
alpine_ab$Species[alpine_ab$Species == 'Elymus_spp.'] <- 'Elymus_trachycaulus'
alpine_ab$Species[alpine_ab$Species == 'Erigeron_spp.'] <- 'Erigeron_pinnatisectus'
alpine_ab$Species[alpine_ab$Species == 'Festuca_spp.'] <- 'Festuca_brachyphylla_subsp._brachyphylla'
alpine_ab$Species[alpine_ab$Species == 'Ivesia_gordonii'] <- 'Potentilla_gordonii'
alpine_ab$Species[alpine_ab$Species == 'Oxytropis_deflexa_ssp._sericea'] <- 'Oxytropis_deflexa_var._sericea'
alpine_ab$Species[alpine_ab$Species == 'Poa_spp.'] <- 'Poa_alpina_var._alpina'
alpine_ab$Species[alpine_ab$Species == 'Potentilla_spp.'] <- 'Potentilla_nivea'
alpine_ab$Species[alpine_ab$Species == 'Rydbergia_grandiflora'] <- 'Hymenoxys_hoopesii'
alpine_ab$Species[alpine_ab$Species == 'Salix_spp.'] <- 'Salix_petrophila'
alpine_ab$Species[alpine_ab$Species == 'Senecio_spp.'] <- 'Packera_cana'

###reformat for signal calculation--------
#get rid of extra columns
a_ab_sig <- subset(alpine_ab, select = -c(Community_type,
                                           Constancy_change_1950_2014))
#match order of species in trait data with order in phylogeny
a_ab_sig <- a_ab_sig[ order(match(a_ab_sig$Species, 
                                    tree.a.ab$tip.label)), ]
a_ab_sig <- ReorderData(tree.a.ab, a_ab_sig, taxa.names = "Species")

#reformat
a_ab_vec <- a_ab_sig %>% remove_rownames %>% 
  column_to_rownames(var="Species")
a_ab_vec <- df2vec(a_ab_vec)

match.phylo.data(tree.a.ab, a_ab_vec)

#as vector for trimming phylo
a_list <- as.vector(alpine_ab$Species)

##prune tree------
tree.a.ab <- drop.tip(SBtree,SBtree$tip.label[-match(a_list, SBtree$tip.label)])

plot(tree.a.ab)
Ntip(tree.a.ab)

## alpine with lumped genera removed ---------

a_remove <- c("Astragalus_robbinsii_var._minor",
              "Kobresia_myosuroides",
              "Elymus_trachycaulus",
              "Erigeron_pinnatisectus",
              "Festuca_brachyphylla_subsp._brachyphylla",
              "Potentilla_gordonii",
              "Poa_alpina_var._alpina",
              "Potentilla_nivea",
              "Salix_petrophila",
              "Packera_cana")

a_ab_removed <- a_ab_sig %>% filter(!Species %in% a_remove)

# reformat
a_ab_vec_removed <- a_ab_removed %>% remove_rownames %>% 
  column_to_rownames(var="Species")
a_ab_vec_removed <- df2vec(a_ab_vec_removed, colID=1)

# as vector for trimming phylo
a_list <- as.vector(a_ab_removed$Species)

## prune tree------
tree.a.ab.rm <- drop.tip(SBtree,SBtree$tip.label[-match(a_list, SBtree$tip.label)])

# Constancy change ----------

## SB -----------------------------------------------

### reformat for signal calculation #####
# get rid of extra columns
sagebrush_co_sig <- subset(sagebrush_ab, select = -c(Community_type,
                                                     Abundance_change_1950_2014))
# match order of species in trait data with order in phylogeny
sagebrush_co_sig <- sagebrush_co_sig[ order(match(sagebrush_co_sig$Species, 
                                                  SB_list$SB_list)), ]

# reformat
sagebrush_co_vec <- sagebrush_co_sig %>% remove_rownames %>% 
  column_to_rownames(var="Species")
sagebrush_co_vec <- df2vec(sagebrush_co_vec, colID=1)

## SB with lumped genera removed -----------------------------------------------

sb_co_remove <- sagebrush_co_sig %>% filter(!Species %in% sb_remove)

# reformat
sagebrush_co_vec_rm <- sb_co_remove %>% remove_rownames %>% 
  column_to_rownames(var="Species")
sagebrush_co_vec_rm <- df2vec(sagebrush_co_vec_rm, colID=1)

## SF -------------------------------------------------------------------------
### reformat for signal calculation ######
# get rid of extra columns
spruce_co_sig <- subset(spruce_ab, select = -c(Community_type,
                                                     Abundance_change_1950_2014))
# match order of species in trait data with order in phylogeny
spruce_co_sig <- spruce_co_sig[ order(match(spruce_co_sig$Species, 
                                                  SB_list$SB_list)), ]

# reformat
spruce_co_vec <- spruce_co_sig %>% remove_rownames %>% 
  column_to_rownames(var="Species")
spruce_co_vec <- df2vec(spruce_co_vec, colID=1)

## SF lumped genera removed --------------------------------------------------
sf_co_removed <- spruce_co_sig %>% filter(!Species %in% sf_remove)

# reformat
spruce_co_vec_rm <- sf_co_removed %>% remove_rownames %>% 
  column_to_rownames(var="Species")
spruce_co_vec_rm <- df2vec(spruce_co_vec_rm, colID=1)

## UH --------------------------------------------------
### reformat for signal calculation ########
# get rid of extra columns
uh_co_sig <- subset(upland_ab, select = -c(Community_type,
                                               Abundance_change_1950_2014))
# match order of species in trait data with order in phylogeny
uh_co_sig <- uh_co_sig[ order(match(uh_co_sig$Species, 
                                            SB_list$SB_list)), ]

# reformat
uh_co_vec <- uh_co_sig %>% remove_rownames %>% 
  column_to_rownames(var="Species")
uh_co_vec <- df2vec(uh_co_vec, colID=1)

## UH lumped genera removed ---------------------------------------
uh_co_removed <- uh_co_sig %>% filter(!Species %in% uh_remove)

# reformat
uh_co_vec_rm <- uh_co_removed %>% remove_rownames %>% 
  column_to_rownames(var="Species")
uh_co_vec_rm <- df2vec(uh_co_vec_rm, colID=1)

## alpine ----------------------------------------------------
### reformat for signal calculation #########

# get rid of extra columns
a_co_sig <- subset(alpine_ab, select = -c(Community_type,
                                           Abundance_change_1950_2014))
# match order of species in trait data with order in phylogeny
a_co_sig <- a_co_sig[ order(match(a_co_sig$Species, 
                                    SB_list$SB_list)), ]

# reformat
a_co_vec <- a_co_sig %>% remove_rownames %>% 
  column_to_rownames(var="Species")
a_co_vec <- df2vec(a_co_vec, colID=1)

## alpine lumped genera removed --------------------------
a_co_removed <- a_co_sig %>% filter(!Species %in% a_remove)

# reformat
a_co_vec_rm <- a_co_removed %>% remove_rownames %>% 
  column_to_rownames(var="Species")
a_co_vec_rm <- df2vec(a_co_vec_rm, colID=1)



