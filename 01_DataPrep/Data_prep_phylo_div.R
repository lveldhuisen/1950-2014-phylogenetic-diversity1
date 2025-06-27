library(tidyverse)
library(dplyr)
library(picante)
library(geiger)
library(ape)
library(vegan)
library(forcats)
library(broom)
library(janitor)
library(patchwork)
library(car)

# bring in phylogeny ---------
SBtree <- read.tree("Data/ALLMB.tre")
SBlist <- as.data.frame(SBtree$tip.label)
SBlist <- SBlist %>% rename(taxalist = 'SBtree$tip.label')

# 1950 datasets ---------

## bring in data ######
sb_1950 <- read.csv("Data/Matrices/sb_plots_1950.csv")
sf_1950 <- read.csv("Data/Matrices/sf_plots_1950.csv")
uh_1950 <- read.csv("Data/Matrices/uh_plots_1950.csv")
a_1950 <- read.csv("Data/Matrices/a_plots_1950.csv")

# add underscore to all species names 
names(sb_1950) <- gsub(x = names(sb_1950), pattern = "\\.", replacement = "_") 
names(sf_1950) <- gsub(x = names(sf_1950), pattern = "\\.", replacement = "_")
names(uh_1950) <- gsub(x = names(uh_1950), pattern = "\\.", replacement = "_")
names(a_1950) <- gsub(x = names(a_1950), pattern = "\\.", replacement = "_")

# convert first column to row names
sb_1950 <- sb_1950 %>%  column_to_rownames(var="X")
sf_1950 <- sf_1950 %>%  column_to_rownames(var="X")
uh_1950 <- uh_1950 %>%  column_to_rownames(var="X")
a_1950 <- a_1950 %>%  column_to_rownames(var="X")

# add row at the bottom for all species in given community type
sb_1950[nrow(sb_1950) + 1, ] <- c(1:1)
row.names(sb_1950)[28] <- "ALL_SB" 

sf_1950[nrow(sf_1950) + 1, ] <- c(1:1)
row.names(sf_1950)[28] <- "ALL_SF" 

uh_1950[nrow(uh_1950) + 1, ] <- c(1:1)
row.names(uh_1950)[32] <- "ALL_UH" 

a_1950[nrow(a_1950) + 1, ] <- c(1:1)
row.names(a_1950)[36] <- "ALL_A" 

## update species names with replacements from appendix S1 -------------
### SB -------

sb1950list <- colnames(sb_1950)
sb1950list <- as.data.frame(sb1950list)

missing_1950sb <- sb1950list %>% filter(!sb1950list %in% SBlist$SBlist)
write.csv(missing_1950sb, "Data/Missing_species_lists_for_phylogeny/sagebrush_1950_plots.csv")

# update col names
sb_1950 <- sb_1950 %>% 
  rename(Achillea_millefolium = Achillea_lanulosa,
    Elymus_violaceus = Agropyron_trachycaulum,
    Amelanchier_alnifolia = Amelanchier_pumila,
    Antennaria_microphylla = Antennaria_spp_,
    Boechera_stricta = Arabis_drummondii,
    Eremogone_congesta = Arenaria_congesta,
    Artemisia_ludoviciana = Artemsia_ludoviciana,
    Artemisia_tridentata = Artemsia_tridentata,
    Bromus_frondosus = Bromus_fondosus,
    Castilleja_linariifolia = Castilleja_linariaefolia,
    Heterotheca_villosa = Chrysopsis_villosa,
    Chrysothamnus_viscidiflorus = Chrysothamnus_spp_, 
    Eriogonum_umbellatum = Eriogonum_neglectum,
    Geranium_viscosissimum = Geranium_spp_,
    Ipomopsis_aggregata = Gilia_aggregata,
    Helianthella_uniflora = Helianthella_quinquenervis,
    Heuchera_parviflora = Heuchara_parvifolia,
    Rostraria_cristata = Koeleria_crisata,
    Lupinus_bakeri = Lupinus_spp_,
    Paxistima_myrsinites = Pachystima_myrsiniteo,
    Potentilla_gracilis_var._flabelliformis = Potentilla_gracilis_pulcherimma,
    Rosa_woodsii = Rosa_spp_,
    Maianthemum_stellatum = Smilacina_stellata,
    Achnatherum_lettermanii = Stipa_lettermani,
    Symphoricarpos_rotundifolius= Symphoricarpos_spp_
  )

# save as csv
write.csv(sb_1950, "Data/Matrices/With_full_pool/sagebrush_1950_plots_clean.csv")

### SF --------

sf1950list <- colnames(sf_1950)
sf1950list <- as.data.frame(sf1950list)

missing_1950sf <- sf1950list %>% filter(!sf1950list %in% SBlist$SBlist)
write.csv(missing_1950sf, "Data/Missing_species_lists_for_phylogeny/sprucefir_1950_plots.csv")

# delete species 
sf_1950 <- select(sf_1950, -c("Senecio_simplectens",
                  "Erigeron_peregrinus"))

# update col names
sf_1950 <- sf_1950 %>% 
  rename(
    Achillea_millefolium = Achillea_lanulosa,
    Aquilegia_elegantula = Aquilegia_elegantuls, 
    Castilleja_septentrionalis = C__septentrionalis, 
    Castilleja_rhexiifolia = Castilleja_rhexifolia, 
    Deschampsia_cespitosa_subsp._cespitosa = Deschampsia_caespitosa,
    Draba_spectabilis = Draba_spectabilis_var__oxyloba,
    Chamerion_angustifolium = Epilobium_angustifolium, 
    Fragaria_virginiana = Fragaria_ovalis,
    Osmorhiza_depauperata = Osmorhiza_obtusa,
    Paxistima_myrsinites = Pachystima_myrsinites,
    Pedicularis_bracteosa_var._paysoniana = Pedicularis_paysoniana,
    Pedicularis_racemosa_subsp._alba = Pedicularis_racemosa,
    Cymopterus_planosus = Psuedocymoptreus_montanus,
    Orthilia_secunda = Pyrola_secunda,
    Moneses_uniflora = Pyrola_uniflora,
    Solidago_multiradiata = Solidago_ciliosa,
    Vaccinium_myrtillus = Vaccinium_spp_, 
    Mitella_stauropetala = Witella_stauropetala,
    Anticlea_elegans = Zygademus_elegans
    )

# save as csv
write.csv(sf_1950, "Data/Matrices/With_full_pool/sprucefir_1950_plots_clean.csv")

### UH -------

uh1950list <- colnames(uh_1950)
uh1950list <- as.data.frame(uh1950list)

missing_1950uh <- uh1950list %>% filter(!uh1950list %in% SBlist$SBlist)
write.csv(missing_1950uh, "Data/Missing_species_lists_for_phylogeny/uplandherb_1950_plots.csv")

# delete species 
uh_1950 <- select(uh_1950, -c("Senecio_crassulus",
"Erigeron_coulteri",
"Erigeron_elatior",
"Erigeron_peregrinus"))

# update col names
uh_1950 <- uh_1950 %>% 
  rename(
    Achillea_millefolium = Achillea_lanulosa_subsp__Alpicola,
    Agoseris_glauca_var._dasycephala = Agoseris_glauca,
    Elymus_violaceus = Agropyron_trachycaulum,
    Antennaria_microphylla = Antennaria_spp_,
    Eremogone_congesta = Arenaria_congesta,
    Calamagrostis_purpurascens_subsp._purpurascens = Calamagrostis_purpurascens, 
    Carex_aquatilis = Carex_spp_,
    Castilleja_rhexiifolia = Castilleja_rhexifolia,
    Castilleja_septentrionalis = Castilleja_septrionalis,
    Heterotheca_villosa = Chrysopsis_villosa,
    Cirsium_eatonii = Circium_scopulorum,
    Festuca_ovina_var._brevifolia = Festuca_ovina,
    Fragaria_virginiana = Fragaria_ovalis__F__virginiana_, 
    Helianthella_uniflora = Heliantella_quinquenervis,
    Lupinus_parvifolius = Lupinus_parvifolia,
    Oxytropis_deflexa_var._sericea = Oxytropis_deflexa,
    Phleum_alpinum = Phelum_alpinum,
    Poa_alpina_var._alpina = Poa_alpina,
    Poa_stenantha = Poa_macroclada,
    Poa_glauca_subsp._glauca = Poa_glauca_var___Rubicola,
    Bistorta_bistortoides = Polygonum_bistortoides,
    Potentilla_gracilis_var._flabelliformis = Potentilla_pulcherrima,
    Cymopterus_planosus = Pseudocymopterus_montanus,
    Salix_krylovii = Salix_pseudolapponum,
    Micranthes_rhomboidea = Saxifraga_rhombiodea,
    Rhodiola_integrifolia = Sedum_integrifolium,
    Frasera_speciosa = Swertia_speciosa__Frasera_speciosa_,
    Trisetum_spicatum = Tristetum_spicatum,
    Vaccinium_caespitosum = Vaccinium_spp_,
    Anticlea_elegans = Zygademus_elegans
  )

# save as csv
write.csv(uh_1950, "Data/Matrices/With_full_pool/uplandherb_1950_plots_clean.csv")

### A --------

a1950list <- colnames(a_1950)
a1950list <- as.data.frame(a1950list)

missing_1950a <- a1950list %>% filter(!a1950list %in% SBlist$SBlist)
write.csv(missing_1950a, "Data/Missing_species_lists_for_phylogeny/alpine_1950_plots.csv")

a_1950 <- select(a_1950, -c("Senecio_amplectens",
"Erigeron_peregrinus"))

# update col names
a_1950 <- a_1950 %>% 
  rename(
    Achillea_millefolium = Achillea_alpicola,
    Elymus_scribneri = Agropyron_scribneri,
    Elymus_violaceus = Agropyron_trachycaulum,
    Antennaria_rosea = Antennaria_spp_,
    Eremogone_congesta = Arenaria_congesta,
    Carex_rupestris = Carex_drummondiana,
    Castilleja_septentrionalis = Castilleja_septentrioccidentalis,
    Cerastium_beeringianum = Cerastium_boeringianum,
    Heterotheca_villosa = Chrysopsis_villosa,
    Draba_oligosperma = Draba_spp_,
    Dryas_octopetala = Dryans_octopetala,
    Festuca_ovina_var._brevifolia = Festuca_ovins_v__brachyphylla,
    Gentianopsis_barbellata = Gentiana_barbellata,
    Hymenoxys_hoopesii = Hymenoxys_grandifolia,
    Potentilla_gordonii = Ivesia_gordoni,
    Carex_borealipolaris = Kobresia_bellardil,
    Mertensia_viridis_var._viridis = Mertensia_viridis,
    Oxytropis_deflexa_var._sericea = Oxytopis_deflexa,
    Oxytropis_podocarpa = Oxytopis_podocarpa,
    Poa_alpina_var._alpina = Poa_alpina,
    Poa_arctica_subsp._arctica = Poa_arctica,
    Poa_glauca_subsp._glauca = Poa_rupicola,
    Polemonium_viscosum = Polemonium_viscocum,
    Bistorta_bistortoides = Polygonum_bistortoides,
    Bistorta_vivipara = Polygonum_viviparum,
    Potentilla_diversifolia_var._glaucophylla = Potentilla_diversifolia,
    Salix_arctica = Salix_anglorum_v__antiplasta,
    Micranthes_rhomboidea = Saxifraga_rhomboidea,
    Packera_heterophylla = Senecio_porteri,
    Senecio_werneriaefolius = Senecio_saxosus,
    Solidago_simplex = Solidago_glutinosa,
    Vaccinium_caespitosum = Vaccinium_spp_,
  )

# save as csv
write.csv(a_1950, "Data/Matrices/With_full_pool/alpine_1950_plots_clean.csv")

## generate lists for entire species pool for PD calculations

# pull species lists from community matrices, rename columns 
sb_1950_list <- as.data.frame(colnames(sb_1950))
sb_1950_list <- sb_1950_list %>% 
  rename(
    species = `colnames(sb_1950)`)

sf_1950_list <- as.data.frame(colnames(sf_1950))
sf_1950_list <- sf_1950_list %>% 
  rename(
    species = `colnames(sf_1950)`)

uh_1950_list <- as.data.frame(colnames(uh_1950))
uh_1950_list <- uh_1950_list %>% 
  rename(
    species = `colnames(uh_1950)`)

a_1950_list <- as.data.frame(colnames(a_1950))
a_1950_list <- a_1950_list %>% 
  rename(
    species = `colnames(a_1950)`)

# merge all
sb_sf <- rbind(sb_1950_list, sf_1950_list)
sb_sf_uh <- rbind(sb_sf, uh_1950_list)
all_1950 <- rbind(sb_sf_uh, a_1950_list)

# get rid of duplicated species
all_1950 <- all_1950[!duplicated(all_1950), ]
all_1950 <- as.data.frame(all_1950)

# need to delete S. crassulus, other species with no good replacement
all_1950 <- all_1950 %>% filter(!all_1950 %in% c("Senecio_crassulus",
                                                 "Erigeron_elatior",
                                                 "Erigeron_glacialis",
                                                 "Senecio_amplectens",
                                                 "Erigeron_coulteri",
                                                 "Senecio_simplectens"))

# rename column to merge
all_1950 <- all_1950 %>% 
  rename(species = all_1950)

# 2014 datasets ----------------

# bring in data
sb_2014 <- read.csv("Data/Matrices/sb_plots_2014.csv")
sf_2014 <- read.csv("Data/Matrices/sf_plots_2014.csv")
uh_2014 <- read.csv("Data/Matrices/uh_plots_2014.csv")
a_2014 <- read.csv("Data/Matrices/a_plots_2014.csv")

# add underscore to all species names 
names(sb_2014) <- gsub(x = names(sb_2014), pattern = "\\.", replacement = "_") 
names(sf_2014) <- gsub(x = names(sf_2014), pattern = "\\.", replacement = "_")
names(uh_2014) <- gsub(x = names(uh_2014), pattern = "\\.", replacement = "_")
names(a_2014) <- gsub(x = names(a_2014), pattern = "\\.", replacement = "_")

# convert first column to row names
sb_2014 <- sb_2014 %>%  column_to_rownames(var="X")
sf_2014 <- sf_2014 %>%  column_to_rownames(var="X")
uh_2014 <- uh_2014 %>%  column_to_rownames(var="X")
a_2014 <- a_2014 %>%  column_to_rownames(var="X")

## update species names with replacements from appendix S1 -------------
### SB -------

sb2014list <- colnames(sb_2014)
sb2014list <- as.data.frame(sb2014list)

missing_2014sb <- sb2014list %>% filter(!sb2014list %in% SBlist$taxalist)
write.csv(missing_2014sb, "Data/Missing_species_lists_for_phylogeny/missing_sagebrush_2014_plots.csv")

#delete
sb_2014 <- select(sb_2014, -c("Helianthella_parryi",
                              "Equisteum_hyemale",
                              "Erigeron_canus"))

# update col names
sb_2014 <- sb_2014 %>% 
  rename(Agoseris_glauca_var._dasycephala	=Agoseris_glauca,
         Allium_geyeri_var._tenerum	=Allium_geyeri,
         Antennaria_rosea=	Antennaria_spp_,
         Anticlea_elegans=	Zigademus_elegans,
         Arctostaphylos_uvaursi=	Arctostaphylos_uva_ursi,
         Berberis_repens=	Mahonia_repens,
         Bistorta_vivipara=	Polygonum_viviparum,
         Bromus_ciliatus=	Bromopsis_ciliata,
         Bromus_inermis=	Bromis_ineris,
         Bromus_richardsonii=	Bromopsis_richardsonii,
         Carex_siccata=	Carex_spp_,
         Castilleja_linariifolia=	Castilleja_linariaefolia,
         Castilleja_rhexiifolia=	Castilleja_rhexifolia,
         Chrysothamnus_viscidiflorus=	Chrysothamnus_spp_,
         Crepis_tectorum=	Crepsis_tectorum,
         Cymopterus_planosus=	Pseudocymopterus_montanus,
         Dasiphora_fruticosa=	Pentaphylloides_floribunda,
         Delphinium_nuttallianum=	Delphinium_nelsonii,
         Deschampsia_cespitosa_subsp._cespitosa=	Deschampsia_caespitosa,
         Draba_albertina=	Draba_spp_,
         Drymocallis_arguta=	Potentilla_arguta,
         Eremogone_congesta=	Arenaria_congesta,
         Gayophytum_diffusum_subsp._parviflorum	=Gayophytum_diffusum,
         Geranium_caespitosum	=Geranium_spp_,
         Geranium_viscosissimum	=Geranium_visicossium,
         Hackelia_floribunda=	Hacklelia_floribunda,
         Helianthella_uniflora=	Helianthella_quinquenervis,
         Heliomeris_multiflora=	Heliomaris_multiflora,
         Hymenoxys_hoopesii=	Dugaldia_hoopsii,
         Juncus_mertensianus=	Juncus_spp_,
         Lonicera_involucrata	=Distegia_involucrata,
         Lupinus_sericeus=	Lupinus_spp_,
         Melica_spectabilis	=Bromelica_spectabilis,
         Mertensia_brevistyla=	Mertensia_spp_,
         Packera_cana	=Senecio_canus,
         Penstemon_caespitosus_var._caespitosus=	Penstemon_caespitosus,
         Poa_alpina_var._alpina=	Poa_alpina,
         Poa_pratensis_subsp._angustifolia=	Poa_spp_,
         Polygonum_douglasii_subsp._douglasii=	Polygonum_douglasii,
         Potentilla_diversifolia_var._glaucophylla=	Potentilla_diversifolia,
         Potentilla_gracilis_var._flabelliformis=	Potentilla_gracilis,
         Rosa_woodsii=	Rosa_spp_,
         Schoenoplectus_tabernaemontani=	Scirpus_tabernaemontani,
         Sedum_lanceolatum=	Sedum_spp_,
         Senecio_eremophilus=	Senecio_spp_,
         Senecio_integerrimus_var._exaltatus=	Senecio_integerrimus,
         Solidago_canadensis=	Solidago_spp_,
         Stipa_hymenoides=	Achnatherum_hymenoides,
         Symphoricarpos_rotundifolius=	Symphoricarpos_spp_,
         Symphyotrichum_campestre=	Virgulus_campestris,
         Vaccinium_myrtillus=	Vaccinium_spp_
) #collapse B. stricta, Ericameria and P. pulcherrima in Excel

# save as csv
write.csv(sb_2014, "Data/Matrices/With_full_pool/sagebrush_2014_plots_clean.csv")


### SF --------

sf2014list <- colnames(sf_2014)
sf2014list <- as.data.frame(sf2014list)

missing_2014sf <- sf2014list %>% filter(!sf2014list %in% SBlist$taxalist)
write.csv(missing_2014sf, "Data/Missing_species_lists_for_phylogeny/missing_sprucefir_2014_plots.csv")

#delete
sf_2014 <- select(sf_2014, -c("Helianthella_parryi",
"Cirsium_hookerianum",
"Ligularia_amplectens_var__Holmii",
"Senecio_amplectens",
"Senecio_crassulus",
"Senecio_spp_",
"Equisetum_arvense",
"Equisetum_palustre",
"Erigeron_elatior",
"Erigeron_glacialis",
"Erigeron_peregrinus"))

# update col names
sf_2014 <- sf_2014 %>% 
  rename(Agoseris_glauca_var._dasycephala=	Agoseris_glauca,
         Anticlea_elegans=	Zigademus_elegans,
         Aquilegia_coerulea	=Aquilegia_caerulea,
         Arctostaphylos_uvaursi	=Arctostaphylos_uva_ursi,
         Berberis_repens	=Mahonia_repens,
         Bromus_ciliatus=	Bromopsis_ciliata,
         Bromus_inermis=	Bromis_ineris,
         Bromus_richardsonii=	Bromopsis_richardsonii,
         Castilleja_rhexiifolia	=Castilleja_rhexifolia,
         Chamerion_angustifolium=	Epilobium_angustifolium,
         Cymopterus_planosus=	Pseudocymopterus_montanus,
         Deschampsia_cespitosa_subsp._cespitosa=	Deschampsia_caespitosa,
         Dieteria_bigelovii=	Machaeranthera_bigelovii,
         Draba_spectabilis=	Draba_spp_,
         Eremogone_congesta	=Arenaria_congesta,
         Geranium_viscosissimum	=Geranium_visicossium,
         Helianthella_uniflora=	Helianthella_quinquenervis,
         Hydrophyllum_capitatum_var._capitatum=	Hydrophyllum_capitatum,
         Hymenoxys_hoopesii=	Dugaldia_hoopsii,
         Lonicera_involucrata=	Distegia_involucrata,
         Lupinus_argenteus=	Lupinus_spp_,
         Melica_spectabilis	=Bromelica_spectabilis,
         Pedicularis_bracteosa_var._paysoniana=	Pedicularis_bracteosa,
         Pedicularis_racemosa_subsp._alba=	Pedicularis_racemosa,
         Phleum_alpinum=	Phleum_commutatum,
         Poa_pratensis_subsp._angustifolia=	Poa_spp_,
         Poa_secunda_subsp._secunda=	Poa_secunda,
         Potentilla_diversifolia_var._glaucophylla=	Potentilla_diversifolia,
         Potentilla_gracilis_var._flabelliformis=	Potentilla_gracilis,
         Pyrola_rotundifolia=	Pyrola_rotundifolia_ssp__Asarifolia,
         Rosa_blanda=	Rosa_spp_,
         Senecio_integerrimus_var._exaltatus=	Senecio_integerrimus,
         Solidago_multiradiata=	Solidago_spp_,
         Symphoricarpos_rotundifolius=	Symphoricarpos_spp_,
         Thlaspi_montanum=	Noccaea_montana,
         Vaccinium_myrtillus=	Vaccinium_spp_,
         Veronica_wormskjoldii=	Veronica_nutans,
    ) #combine P. racemosa, Carex geyeri, C. miniata, G. richardsonii, M. unflora, V. rydbergii in Excel 

# save as csv
write.csv(sf_2014, "Data/Matrices/With_full_pool/sprucefir_2014_plots_clean.csv")

### UH -------

uh2014list <- colnames(uh_2014)
uh2014list <- as.data.frame(uh2014list)

missing_2014uh <- uh2014list %>% filter(!uh2014list %in% SBlist$taxalist)
write.csv(missing_2014uh, "Data/Missing_species_lists_for_phylogeny/missing_uplandherb_2014_plots.csv")

# delete species 
uh_2014 <- select(uh_2014, -c("Aster_spp_",
                              "Helianthella_parryi",
                              "Cirsium_hookerianum",
                              "Ligularia_soldanella",
                              "Senecio_amplectens",
                              "Senecio_crassulus",
                              "Erigeron_canus",
                              "Erigeron_coulteri",
                              "Erigeron_elatior",
                              "Erigeron_glacialis"))

# update col names
uh_2014 <- uh_2014 %>% 
  rename(Agoseris_glauca_var._dasycephala=	Agoseris_glauca,
         Anemone_multifida=	Aneome_multifida_ssp__Saxicola,
         Antennaria_media_subsp._compacta=	Antennaria_spp_,
         Anticlea_elegans=	Zigademus_elegans,
         Aquilegia_coerulea	=Aquilegia_caerulea,
         Bromus_ciliatus=	Bromopsis_ciliata,
         Bromus_inermis=	Bromis_ineris,
         Bromus_richardsonii=	Bromopsis_richardsonii,
         Calamagrostis_purpurascens_subsp._purpurascens=	Calamagrostis_purpurascens,
         Carex_aquatilis=	Carex_spp_,
         Castilleja_linariifolia=	Castilleja_linariaefolia,
         Castilleja_rhexiifolia=	Castilleja_rhexifolia,
         Cirsium_scariosum=	Cirsium_spp_,
         Crepis_tectorum=	Crepsis_tectorum,
         Cymopterus_planosus=	Pseudocymopterus_montanus,
         Dasiphora_fruticosa=	Pentaphylloides_floribunda,
         Delphinium_nuttallianum=	Delphinium_nelsonii,
         Draba_albertina=	Draba_spp_,
         Eremogone_congesta=	Arenaria_congesta,
         Erigeron_compositus=	Erigeron_compositsus,
         Festuca_ovina_var._brevifolia=	Festuca_ovina,
         Gagea_serotina=	Lloydia_serotina,
         Geranium_richardsonii=	Geranium_spp_,
         Geranium_viscosissimum	=Geranium_visicossium,
         Helianthella_uniflora=	Helianthella_quinquenervis,
         Heliomeris_multiflora=	Heliomaris_grandifolia,
         Hydrophyllum_capitatum_var._capitatum=	Hydrophyllum_capitatum,
         Hymenoxys_hoopesii=	Dugaldia_hoopsii,
         Juncus_mertensianus=	Juncus_spp_,
         Lupinus_argenteus=	Lupinus_spp_,
         Melica_spectabilis	=Bromelica_spectabilis,
         Moneses_uniflora	=Pyrola_uniflora,
         Oxytropis_campestris_var._gracilis=	Oxytropis_campestris,
         Oxytropis_deflexa_var._sericea	=Oxytropis_deflexa_var__sericea,
         Pedicularis_bracteosa_var._paysoniana=	Pedicularis_bracteosa,
         Pedicularis_parryi	=Pedicularis_spp_,
         Pedicularis_racemosa_subsp._alba=	Pedicularis_racemosa,
         Phleum_alpinum	=Phleum_commutatum,
         Poa_alpina_var._alpina=	Poa_alpina,
         Polygonum_douglasii_subsp._douglasii=	Polygonum_douglasii,
         Potentilla_gordonii=	Ivesia_gordonii,
         Potentilla_gracilis_var._flabelliformis=	Potentilla_gracilis,
         Ribes_montigenum	=Ribes_spp_,
         Rosa_blanda=	Rosa_spp_,
         Sedum_lanceolatum=	Sedum_spp_,
         Senecio_integerrimus_var._exaltatus=	Senecio_integerrimus,
         Solidago_multiradiata=	Solidago_spp_,
         Symphoricarpos_rotundifolius=	Symphoricarpos_spp_,
         Tetraneuris_acaulis=	Tetraneuris_grandiflora,
         Thlaspi_montanum	=Noccaea_montana,
         Townsendia_exscapa	=Townsendia_sp_,
         Vaccinium_caespitosum=	Vaccinium_spp_,
         Veronica_besseya=	Besseya_alpina,
         Veronica_wormskjoldii=	Veronica_nutans,
         Viola_epipsila_subsp._repens=	Viola_repens
  ) #combine S. integerrimus, C. sulphurea, M. ciliata, Packera cana, P. fendleriana, P. pulcherrima in Excel 

# save as csv
write.csv(uh_2014, "Data/Matrices/With_full_pool/uplandherb_2014_plots_clean.csv")

### A --------

a2014list <- colnames(a_2014)
a2014list <- as.data.frame(a2014list)

missing_2014a <- a2014list %>% filter(!a2014list %in% SBlist$taxalist)
write.csv(missing_2014a, "Data/Missing_species_lists_for_phylogeny/missing_alpine_2014_plots.csv")

# delete species 
a_2014 <- select(a_2014, -c("Ligularia_holmii",
"Senecio_amplectens",
"Erigeron_glacialis",
"Erigeron_peregrinus"))

# update col names
a_2014 <- a_2014 %>% 
  rename(Anemone_multifida=	Aneome_multifida,
         Anemone_patens=	Pulsatilla_patens,
         Antennaria_corymbosa=	Antennaria_spp_,
         Anticlea_elegans	=Zigademus_elegans,
         Artemisia_frigida	=Artemisia_fridgia,
         Bistorta_bistortoides=	Bistort_bistortoides,
         Boechera_oxylobula	=Bochera_sp_,
         Carex_atrosquama	=Carex_atrosquma,
         Carex_chalciolepis	=Carex_chalxiolepis,
         Carex_pyrenaica=	Carex_pyrenica,
         Castilleja_rhexiifolia	=Castilleja_rhexifolia,
         Chaenactis_douglasii=	Chaenactis_alpina,
         Cirsium_eatonii=	Circium_sp_,
         Cymopterus_planosus=	Pseudocymopterus_montanus,
         Dasiphora_fruticosa=	Pentaphylloides_floribunda,
         Eremogone_congesta=	Arenaria_congesta,
         Gagea_serotina=	Lloydia_serotina,
         Gayophytum_diffusum_subsp._parviflorum	=Gayophytum_diffusum_ssp__Parviflorum,
         Geranium_viscosissimum	=Geranium_visicossium,
         Helianthella_uniflora=	Helianthella_quinquenervis,
         Hesperostipa_comata=	Stipa_comata,
         Hymenoxys_hoopesii	=Dugaldia_hoopesii,
         Juncus_drummondii=	Juncus_spp_,
         Mertensia_lanceolata	=Mertensia_spp_,
         Micranthes_occidentalis=	Saxifraga_occidentalis,
         Micranthes_rhomboidea=	Saxifraga_rhomboidea,
         Oxytropis_deflexa_var._sericea	=Oxytropis_deflexa_var_sericea,
         Pedicularis_bracteosa_var._paysoniana=	Pedicularis_bracteosa,
         Physaria_reediana=	Lesquerella_alpina,
         Poa_leptocoma=	Poa_spp_,
         Polygonum_douglasii_subsp._douglasii=	Polygonum_douglasii,
         Potentilla_diversifolia_var._glaucophylla=	Potentilla_diversifolia,
         Sabulina_rubella	=Minuartia_rubella,
         Senecio_integerrimus_var._exaltatus=	Senecio_integerrimus,
         Senecio_werneriaefolius=	Packera_werneriifolia,
         Solidago_simplex=	Solidago_simples,
         Taraxacum_officinale	=Taraxcum_officinale,
         Vaccinium_caespitosum=	Vaccinium_spp_,
         Veronica_besseya	=Besseya_alpina
  ) #combine D. aurea, L. parviflorus, P. nivea in Excel

# save as csv
write.csv(a_2014, "Data/Matrices/With_full_pool/alpine_2014_plots_clean.csv")

## make all 2014 list ------------------------------------------
# pull species lists from community matrices, rename columns 
sb_2014_list <- as.data.frame(colnames(sb_2014))
sb_2014_list <- sb_2014_list %>% 
  rename(
    species = `colnames(sb_2014)`)

sf_2014_list <- as.data.frame(colnames(sf_2014))
sf_2014_list <- sf_2014_list %>% 
  rename(
    species = `colnames(sf_2014)`)

uh_2014_list <- as.data.frame(colnames(uh_2014))
uh_2014_list <- uh_2014_list %>% 
  rename(
    species = `colnames(uh_2014)`)

a_2014_list <- as.data.frame(colnames(a_2014))
a_2014_list <- a_2014_list %>% 
  rename(
    species = `colnames(a_2014)`)

# merge all
sb_sf14 <- rbind(sb_2014_list, sf_2014_list)
sb_sf_uh14 <- rbind(sb_sf14, uh_2014_list)
all_2014 <- rbind(sb_sf_uh14, a_2014_list)

# get rid of duplicated species
all_2014 <- all_2014[!duplicated(all_2014), ]
all_2014 <- as.data.frame(all_2014)

# rename column to merge
all_2014 <- all_2014 %>% 
  rename(species = all_2014)

# merge 1950 and 2014 species lists 
all_list <- rbind(all_1950, all_2014)

# get rid of duplicated species
all_list <- all_list[!duplicated(all_list), ]
all_list <- as.data.frame(all_list)

# make species pool for all species across both time points -------------------

# generate lists of species **not** in each community type
# to add to community matrices

# 1950 plots
sb_to_add_1950 <- all_list %>% filter(!all_list %in% sb_1950_list$species)
sf_to_add_1950 <- all_list %>% filter(!all_list %in% sf_1950_list$species)
uh_to_add_1950 <- all_list %>% filter(!all_list %in% uh_1950_list$species)
a_to_add_1950 <- all_list %>% filter(!all_list %in% a_1950_list$species)

# 2014 plots
sb_to_add_2014 <- all_list %>% filter(!all_list %in% sb_2014_list$species)
sf_to_add_2014 <- all_list %>% filter(!all_list %in% sf_2014_list$species)
uh_to_add_2014 <- all_list %>% filter(!all_list %in% uh_2014_list$species)
a_to_add_2014 <- all_list %>% filter(!all_list %in% a_2014_list$species)

#save as csv to add to community matrices in Excel
write.csv(sb_to_add_1950, "Data/Missing_species_lists_for_phylogeny/SB_1950_to_add_species_pool.csv")
write.csv(sf_to_add_1950, "Data/Missing_species_lists_for_phylogeny/SF_1950_to_add_species_pool.csv")
write.csv(uh_to_add_1950, "Data/Missing_species_lists_for_phylogeny/UH_1950_to_add_species_pool.csv")
write.csv(a_to_add_1950, "Data/Missing_species_lists_for_phylogeny/A_1950_to_add_species_pool.csv")

write.csv(sb_to_add_2014, "Data/Missing_species_lists_for_phylogeny/SB_2014_to_add_species_pool.csv")
write.csv(sf_to_add_2014, "Data/Missing_species_lists_for_phylogeny/SF_2014_to_add_species_pool.csv")
write.csv(uh_to_add_2014, "Data/Missing_species_lists_for_phylogeny/UH_2014_to_add_species_pool.csv")
write.csv(a_to_add_2014, "Data/Missing_species_lists_for_phylogeny/A_2014_to_add_species_pool.csv")


# bring back matrices with row for whole species pool
sb_1950_mat <- read.csv("Data/Matrices/With_full_pool/sagebrush_1950_plots_clean.csv")
sf_1950_mat <- read.csv("Data/Matrices/With_full_pool/sprucefir_1950_plots_clean.csv")
uh_1950_mat <- read.csv("Data/Matrices/With_full_pool/uplandherb_1950_plots_clean.csv")
a_1950_mat <- read.csv("Data/Matrices/With_full_pool/alpine_1950_plots_clean.csv")

sb_2014_mat <- read.csv("Data/Matrices/With_full_pool/sagebrush_2014_plots_clean.csv")
sf_2014_mat <- read.csv("Data/Matrices/With_full_pool/sprucefir_2014_plots_clean.csv")
uh_2014_mat <- read.csv("Data/Matrices/With_full_pool/uplandherb_2014_plots_clean.csv")
a_2014_mat <- read.csv("Data/Matrices/With_full_pool/alpine_2014_plots_clean.csv")

# convert first column to row names
sb_1950_mat <- sb_1950_mat %>%  column_to_rownames(var="X")
sf_1950_mat <- sf_1950_mat %>%  column_to_rownames(var="X")
uh_1950_mat <- uh_1950_mat %>%  column_to_rownames(var="X")
a_1950_mat <- a_1950_mat %>%  column_to_rownames(var="X")

sb_2014_mat <- sb_2014_mat %>%  column_to_rownames(var="X")
sf_2014_mat <- sf_2014_mat %>%  column_to_rownames(var="X")
uh_2014_mat <- uh_2014_mat %>%  column_to_rownames(var="X")
a_2014_mat <- a_2014_mat %>%  column_to_rownames(var="X")

# format to work in picante
# 1950 
sb_1950_mat <- sb_1950_mat %>% mutate_if(is.numeric, as.integer)
sb_1950_mat <- as.matrix(sb_1950_mat)
sb_1950_mat[is.na(sb_1950_mat)] <- 0

sf_1950_mat <- sf_1950_mat %>% mutate_if(is.numeric, as.integer)
sf_1950_mat <- as.matrix(sf_1950_mat)
sf_1950_mat[is.na(sf_1950_mat)] <- 0

uh_1950_mat <- uh_1950_mat %>% mutate_if(is.numeric, as.integer)
uh_1950_mat <- as.matrix(uh_1950_mat)
uh_1950_mat[is.na(uh_1950_mat)] <- 0

a_1950_mat <- a_1950_mat %>% mutate_if(is.numeric, as.integer)
a_1950_mat <- as.matrix(a_1950_mat)
a_1950_mat[is.na(a_1950_mat)] <- 0

# 2014
sb_2014_mat <- sb_2014_mat %>% mutate_if(is.numeric, as.integer)
sb_2014_mat <- as.matrix(sb_2014_mat)
sb_2014_mat[is.na(sb_2014_mat)] <- 0

sf_2014_mat <- sf_2014_mat %>% mutate_if(is.numeric, as.integer)
sf_2014_mat <- as.matrix(sf_2014_mat)
sf_2014_mat[is.na(sf_2014_mat)] <- 0

uh_2014_mat <- uh_2014_mat %>% mutate_if(is.numeric, as.integer)
uh_2014_mat <- as.matrix(uh_2014_mat)
uh_2014_mat[is.na(uh_2014_mat)] <- 0

a_2014_mat[is.na(a_2014_mat)] <- 0 #1


#a_2014_mat <- a_2014_mat %>% mutate_if(is.character, as.integer)
a_2014_mat <- sapply(a_2014_mat, as.integer)
a_2014_mat <- as.matrix(a_2014_mat) 
a_2014_mat[] <- sapply(a_2014_mat, as.numeric)

sapply(a_2014_mat,class)



## prune phylogeny --------- 

# make list to prune
phylo_list <- as.vector(all_list$all_list)

# prune tree
Zorio.tree <- drop.tip(SBtree,SBtree$tip.label[-match(phylo_list, SBtree$tip.label)]) 
class(Zorio.tree)
write.tree(Zorio.tree, "Data/Pruned_community_tree.tre")

# check 
plot(Zorio.tree)
is.rooted(Zorio.tree)
is.binary(Zorio.tree)
Ntip(Zorio.tree)

