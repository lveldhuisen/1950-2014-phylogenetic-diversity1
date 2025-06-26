library(dplyr)
library(WorldFlora)
library(TNRS)


#bring in Zorio list
sp_list <- read.csv("Data/species_list.csv")

t_sp_list <- transpose(sp_list)

#check names with worldflora
wfo <- read.csv("Data/classification.csv")

WFO.match(spec.data = t_sp_list, WFO.data = "Data/classification.csv",
          no.dates = TRUE)




#bring in S&B taxa list
SB_list <- read.csv("SmithBrown18_taxaalist.csv")

#show missing species
missing_species <- sp_list %>% filter(!Species %in% SB_list$x)
missing_species$rows <- rownames(missing_species)

#convert to vector
missing_vec <- as.vector(missing_species)

#save as csv
write.csv(missing_species, "Data/missing_species_fromR.csv")

