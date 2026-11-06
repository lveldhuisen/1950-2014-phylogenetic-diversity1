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

# prune tree
Zorio.tree <- drop.tip(SBtree,SBtree$tip.label[-match(phylo_list, SBtree$tip.label)]) 

# remove species below 14% constancy -------
## SB -------------
# bring in data 
sb_plots_2014 <- read.csv("Data/Matrices/With_full_pool/sagebrush_2014_plots_clean.csv")
sb_plots_2014 <- sb_plots_2014 %>%  column_to_rownames(var="X")

# remove bottom two rows to not include in constancy calculation
sb_plots_2014 <- sb_plots_2014[-c(28, 29), ]

# keep columns where more than 0 appears in at least 4 rows
sb_14_reduced <- sb_plots_2014[, colSums(sb_plots_2014 > 0, na.rm = TRUE) >= 4]

# add row at the bottom for all species in community type
sb_14_reduced[nrow(sb_14_reduced) + 1, ] <- c(1:1)
row.names(sb_14_reduced)[28] <- "ALL_SB" 

# save as csv
write.csv(sb_14_reduced, "Data/Matrices/sagebrush_14_reduced.csv")

# make list of reduced community species
sb_list_reduced <- as.data.frame(colnames(sb_14_reduced))

# bring in list for whole species pool
pool <- read.csv("Data/Species_lists/full_species_pool.csv")

# generate list to add for full pool
sb_to_add_2014_reduced <- pool %>% filter(!pool_list %in% sb_list_reduced$`colnames(sb_14_reduced)`)

# save as csv to add in excel 
write.csv(sb_to_add_2014_reduced, "Data/Missing_species_lists_for_phylogeny/SB_addtopool_reduced2014.csv")

# bring back matrices with row for whole species pool
sb_14_reduced <- read.csv("Data/Matrices/With_full_pool/sagebrush_14_reduced.csv")

# convert first column to row names
sb_14_reduced_mat <- sb_14_reduced %>%  column_to_rownames(var="X")

# format to work in picante
sb_14_reduced_mat <- sb_14_reduced_mat %>% mutate_if(is.numeric, as.integer)
sb_14_reduced_mat <- as.matrix(sb_14_reduced_mat)

## SF ----------------------------------------

# bring in data 
sf_plots_2014 <- read.csv("Data/Matrices/With_full_pool/sprucefir_2014_plots_clean.csv")
sf_plots_2014 <- sf_plots_2014 %>%  column_to_rownames(var="X")

# remove bottom two rows to not include in constancy calculation
sf_plots_2014 <- sf_plots_2014[-c(32,33), ]

# keep columns where more than 0 appears in at least 4 rows
sf_14_reduced <- sf_plots_2014[, colSums(sf_plots_2014 > 0, na.rm = TRUE) >= 5]

# add row at the bottom for all species in community type
sf_14_reduced[nrow(sf_14_reduced) + 1, ] <- c(1:1)
row.names(sf_14_reduced)[32] <- "ALL_SF" 

# save as csv
write.csv(sf_14_reduced, "Data/Matrices/sprucefir_14_reduced.csv")

# make list of reduced community species
sf_list_reduced <- as.data.frame(colnames(sf_14_reduced))

# generate list to add for full pool
sf_to_add_2014_reduced <- pool %>% filter(!pool_list %in% sf_list_reduced$`colnames(sf_14_reduced)`)

# save as csv to add in excel 
write.csv(sf_to_add_2014_reduced, "Data/Missing_species_lists_for_phylogeny/SF_addtopool_reduced2014.csv")

# bring back matrices with row for whole species pool
sf_14_reduced <- read.csv("Data/Matrices/With_full_pool/sprucefir_14_reduced.csv")

# convert first column to row names
sf_14_reduced_mat <- sf_14_reduced %>%  column_to_rownames(var="X")

# format to work in picante
sf_14_reduced_mat <- sf_14_reduced_mat %>% mutate_if(is.numeric, as.integer)
sf_14_reduced_mat <- as.matrix(sf_14_reduced_mat)

## UH ----------------------------------------

# bring in data 
uh_plots_2014 <- read.csv("Data/Matrices/With_full_pool/uplandherb_2014_plots_clean.csv")
uh_plots_2014 <- uh_plots_2014 %>%  column_to_rownames(var="X")

# remove bottom two rows to not include in constancy calculation
uh_plots_2014 <- sf_plots_2014[-c(31,32), ]

# keep columns where more than 0 appears in at least 4 rows
uh_14_reduced <- uh_plots_2014[, colSums(uh_plots_2014 > 0, na.rm = TRUE) >= 5]

# add row at the bottom for all species in community type
uh_14_reduced[nrow(uh_14_reduced) + 1, ] <- c(1:1)
row.names(uh_14_reduced)[31] <- "ALL_UH" 

# save as csv
write.csv(uh_14_reduced, "Data/Matrices/uplandherb_14_reduced.csv")

# make list of reduced community species
uh_list_reduced <- as.data.frame(colnames(uh_14_reduced))

# generate list to add for full pool
uh_to_add_2014_reduced <- pool %>% filter(!pool_list %in% uh_list_reduced$`colnames(uh_14_reduced)`)

# save as csv to add in excel 
write.csv(uh_to_add_2014_reduced, "Data/Missing_species_lists_for_phylogeny/UH_addtopool_reduced2014.csv")

# bring back matrices with row for whole species pool
uh_14_reduced <- read.csv("Data/Matrices/With_full_pool/uplandherb_14_reduced.csv")

# convert first column to row names
uh_14_reduced_mat <- uh_14_reduced %>%  column_to_rownames(var="X")

# format to work in picante
uh_14_reduced_mat <- uh_14_reduced_mat %>% mutate_if(is.numeric, as.integer)
uh_14_reduced_mat <- as.matrix(uh_14_reduced_mat)

## A ---------------------------------------

# bring in data 
a_plots_2014 <- read.csv("Data/Matrices/With_full_pool/alpine_2014_plots_clean.csv")
a_plots_2014 <- a_plots_2014 %>%  column_to_rownames(var="X")

# remove bottom two rows to not include in constancy calculation
a_plots_2014 <- a_plots_2014[-c(34,35), ]

# keep columns where more than 0 appears in at least 4 rows
a_14_reduced <- a_plots_2014[, colSums(a_plots_2014 > 0, na.rm = TRUE) >= 5]

# add row at the bottom for all species in community type
a_14_reduced[nrow(a_14_reduced) + 1, ] <- c(1:1)
row.names(a_14_reduced)[34] <- "ALL_A" 

# save as csv
write.csv(a_14_reduced, "Data/Matrices/alpine_14_reduced.csv")

# make list of reduced community species
a_list_reduced <- as.data.frame(colnames(a_14_reduced))

# generate list to add for full pool
a_to_add_2014_reduced <- pool %>% filter(!pool_list %in% a_list_reduced$`colnames(a_14_reduced)`)

# save as csv to add in excel 
write.csv(a_to_add_2014_reduced, "Data/Missing_species_lists_for_phylogeny/A_addtopool_reduced2014.csv")

# bring back matrices with row for whole species pool
a_14_reduced <- read.csv("Data/Matrices/With_full_pool/alpine_14_reduced.csv")

# convert first column to row names
a_14_reduced_mat <- a_14_reduced %>%  column_to_rownames(var="X")

a_14_reduced_mat[is.na(a_14_reduced_mat)] <- 0

# format to work in picante
a_14_reduced_mat <- a_14_reduced_mat %>% mutate_if(is.character, as.integer)
a_14_reduced_mat <- a_14_reduced_mat %>% mutate_if(is.numeric, as.integer)
a_14_reduced_mat <- as.matrix(a_14_reduced_mat)

# matrices with lumped genera removed -------
## 1950 ------
### SB #####
# bring back matrices with row for whole species pool
sb_50_NL <- read.csv("Data/Matrices/With_full_pool/sagebrush_1950_NL.csv")

# convert first column to row names
sb_50_NL_mat <- sb_50_NL %>%  column_to_rownames(var="X")

sb_50_NL_mat[is.na(sb_50_NL_mat)] <- 0

# format to work in picante
sb_50_NL_mat <- sb_50_NL_mat %>% mutate_if(is.character, as.integer)
sb_50_NL_mat <- sb_50_NL_mat %>% mutate_if(is.numeric, as.integer)
sb_50_NL_mat <- as.matrix(sb_50_NL_mat)

### SF ####
# bring back matrices with row for whole species pool
sf_50_NL <- read.csv("Data/Matrices/With_full_pool/sprucefir_1950_NL.csv")

# convert first column to row names
sf_50_NL_mat <- sf_50_NL %>%  column_to_rownames(var="X")

sf_50_NL_mat[is.na(sf_50_NL_mat)] <- 0

# format to work in picante
sf_50_NL_mat <- sf_50_NL_mat %>% mutate_if(is.character, as.integer)
sf_50_NL_mat <- sf_50_NL_mat %>% mutate_if(is.numeric, as.integer)
sf_50_NL_mat <- as.matrix(sf_50_NL_mat)

### UH #####
# bring back matrices with row for whole species pool
uh_50_NL <- read.csv("Data/Matrices/With_full_pool/uplandherb_1950_NL.csv")

# convert first column to row names
uh_50_NL_mat <- uh_50_NL %>%  column_to_rownames(var="X")

uh_50_NL_mat[is.na(uh_50_NL_mat)] <- 0

# format to work in picante
uh_50_NL_mat <- uh_50_NL_mat %>% mutate_if(is.character, as.integer)
uh_50_NL_mat <- uh_50_NL_mat %>% mutate_if(is.numeric, as.integer)
uh_50_NL_mat <- as.matrix(uh_50_NL_mat)

### A #####
# bring back matrices with row for whole species pool
a_50_NL <- read.csv("Data/Matrices/With_full_pool/alpine_1950_NL.csv")

# convert first column to row names
a_50_NL_mat <- a_50_NL %>%  column_to_rownames(var="X")

a_50_NL_mat[is.na(a_50_NL_mat)] <- 0

# format to work in picante
a_50_NL_mat <- a_50_NL_mat %>% mutate_if(is.character, as.integer)
a_50_NL_mat <- a_50_NL_mat %>% mutate_if(is.numeric, as.integer)
a_50_NL_mat <- as.matrix(a_50_NL_mat)

## 2014 ------
### SB ####
sb_14_NL <- read.csv("Data/Matrices/With_full_pool/sagebrush_14_reduced_NL.csv")

# convert first column to row names
sb_14_NL_mat <- sb_50_NL %>%  column_to_rownames(var="X")

sb_14_NL_mat[is.na(sb_14_NL_mat)] <- 0

# format to work in picante
sb_14_NL_mat <- sb_14_NL_mat %>% mutate_if(is.character, as.integer)
sb_14_NL_mat <- sb_14_NL_mat %>% mutate_if(is.numeric, as.integer)
sb_14_NL_mat <- as.matrix(sb_14_NL_mat)

### SF ####
sf_14_NL <- read.csv("Data/Matrices/With_full_pool/sprucefir_14_reduced_NL.csv")

# convert first column to row names
sf_14_NL_mat <- sf_14_NL %>%  column_to_rownames(var="X")

sf_14_NL_mat[is.na(sf_14_NL_mat)] <- 0

# format to work in picante
sf_14_NL_mat <- sf_14_NL_mat %>% mutate_if(is.character, as.integer)
sf_14_NL_mat <- sf_14_NL_mat %>% mutate_if(is.numeric, as.integer)
sf_14_NL_mat <- as.matrix(sf_14_NL_mat)

### UH #####
uh_14_NL <- read.csv("Data/Matrices/With_full_pool/uplandherb_14_reduced_NL.csv")

# convert first column to row names
uh_14_NL_mat <- uh_14_NL %>%  column_to_rownames(var="X")

uh_14_NL_mat[is.na(uh_14_NL_mat)] <- 0

# format to work in picante
uh_14_NL_mat <- uh_14_NL_mat %>% mutate_if(is.character, as.integer)
uh_14_NL_mat <- uh_14_NL_mat %>% mutate_if(is.numeric, as.integer)
uh_14_NL_mat <- as.matrix(uh_14_NL_mat)

### A ####
a_14_NL <- read.csv("Data/Matrices/With_full_pool/alpine_14_reduced_NL.csv")

# convert first column to row names
a_14_NL_mat <- a_14_NL %>%  column_to_rownames(var="X")

a_14_NL_mat[is.na(a_14_NL_mat)] <- 0

# format to work in picante
a_14_NL_mat <- a_14_NL_mat %>% mutate_if(is.character, as.integer)
a_14_NL_mat <- a_14_NL_mat %>% mutate_if(is.numeric, as.integer)
a_14_NL_mat <- as.matrix(a_14_NL_mat)

# remove species below 14% constancy for 2014 raw data and no name substitutions --------
## SB -------------
# bring in data 
sb_plots_2014 <- read.csv("Data/Matrices/sb_plots_2014.csv")
sb_plots_2014 <- sb_plots_2014 %>%  column_to_rownames(var="X")

# remove bottom two rows to not include in constancy calculation
sb_plots_2014 <- sb_plots_2014[-c(28), ]

# keep columns where more than 0 appears in at least 4 rows
sb_14_reduced <- sb_plots_2014[, colSums(sb_plots_2014 > 0, na.rm = TRUE) >= 4]

# save as csv
write.csv(sb_14_reduced, "Data/Matrices/sb_plots_14_reduced_raw.csv")

## SF -----------------------

# bring in data 
sf_plots_2014 <- read.csv("Data/Matrices/sf_plots_2014.csv")
sf_plots_2014 <- sf_plots_2014 %>%  column_to_rownames(var="X")

# remove bottom two rows to not include in constancy calculation
sf_plots_2014 <- sf_plots_2014[-c(32), ]

# keep columns where more than 0 appears in at least 4 rows
sf_14_reduced <- sf_plots_2014[, colSums(sf_plots_2014 > 0, na.rm = TRUE) >= 5]

# save as csv
write.csv(sf_14_reduced, "Data/Matrices/sf_plots_14_reduced_raw.csv")

## UH ----------------------------------------

# bring in data 
uh_plots_2014 <- read.csv("Data/Matrices/uh_plots_2014.csv")
uh_plots_2014 <- uh_plots_2014 %>%  column_to_rownames(var="X")

# remove bottom two rows to not include in constancy calculation
uh_plots_2014 <- sf_plots_2014[-c(31), ]

# keep columns where more than 0 appears in at least 4 rows
uh_14_reduced <- uh_plots_2014[, colSums(uh_plots_2014 > 0, na.rm = TRUE) >= 5]

# save as csv
write.csv(uh_14_reduced, "Data/Matrices/uh_plots_2014_reduced_raw.csv")

## A ---------------------------------------

# bring in data 
a_plots_2014 <- read.csv("Data/Matrices/a_plots_2014.csv")
a_plots_2014 <- a_plots_2014 %>%  column_to_rownames(var="X")

# remove bottom two rows to not include in constancy calculation
a_plots_2014 <- a_plots_2014[-c(34), ]

# keep columns where more than 0 appears in at least 4 rows
a_14_reduced <- a_plots_2014[, colSums(a_plots_2014 > 0, na.rm = TRUE) >= 5]

# save as csv
write.csv(a_14_reduced, "Data/Matrices/a_plots_2014_reduced_raw.csv")
