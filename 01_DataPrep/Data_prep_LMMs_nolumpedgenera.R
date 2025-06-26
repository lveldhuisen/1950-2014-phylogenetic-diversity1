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
library(ggpubr)

# bring in elev data ----------
gps_df <- read.csv("Data/Zorio_plot_locations.csv")

gps_df <- gps_df %>% 
  rename(replicates = Plot) # make sure plyr package is uninstalled to run this

gps_df$replicates <- gps_df$replicates <- as.factor(gps_df$replicates)

## make data frames for each community type -------
sb_gps <- gps_df %>% filter(community %in% "sagebrush")
sf_gps <- gps_df %>% filter(community %in% "spruce-fir")
uh_gps <- gps_df %>% filter(community %in% "upland-herb")
a_gps <- gps_df %>% filter(community %in% "alpine")


# bring in phylo diversity data ----------------------
# 1950 
pd_sb_1950 <- read.csv("Data/Phylo_div_results/PD_SB_1950_nolumped.csv")
mpd_sb_1950 <- read.csv("Data/Phylo_div_results/MPD_SB_1950_nolumped.csv")
mntd_sb_1950 <- read.csv("Data/Phylo_div_results/MNTD_SB_1950_nolumped.csv")

pd_sf_1950 <- read.csv("Data/Phylo_div_results/PD_SF_1950_nolumped.csv")
mpd_sf_1950 <- read.csv("Data/Phylo_div_results/MPD_SF_1950_nolumped.csv")
mntd_sf_1950 <- read.csv("Data/Phylo_div_results/MNTD_SF_1950_nolumped.csv")

pd_uh_1950 <- read.csv("Data/Phylo_div_results/PD_UH_1950_nolumped.csv")
mpd_uh_1950 <- read.csv("Data/Phylo_div_results/MPD_UH_1950_nolumped.csv")
mntd_uh_1950 <- read.csv("Data/Phylo_div_results/MNTD_UH_1950_nolumped.csv")

pd_a_1950 <- read.csv("Data/Phylo_div_results/PD_A_1950_nolumped.csv")
mpd_a_1950 <- read.csv("Data/Phylo_div_results/MPD_A_1950_nolumped.csv")
mntd_a_1950 <- read.csv("Data/Phylo_div_results/MNTD_A_1950_nolumped.csv")


# 2014 (using only reduced datasets)
pd_sb_2014 <- read.csv("Data/Phylo_div_results/PD_SB_2014_nolumped.csv")
mpd_sb_2014 <- read.csv("Data/Phylo_div_results/MPD_SB_2014_nolumped.csv")
mntd_sb_2014 <- read.csv("Data/Phylo_div_results/MNTD_SB_2014_nolumped.csv")

pd_sf_2014 <- read.csv("Data/Phylo_div_results/PD_SF_2014_nolumped.csv")
mpd_sf_2014 <- read.csv("Data/Phylo_div_results/MPD_SF_2014_nolumped.csv")
mntd_sf_2014 <- read.csv("Data/Phylo_div_results/MNTD_SF_2014_nolumped.csv")

pd_uh_2014 <- read.csv("Data/Phylo_div_results/PD_UH_2014_nolumped.csv")
mpd_uh_2014 <- read.csv("Data/Phylo_div_results/MPD_UH_2014_nolumped.csv")
mntd_uh_2014 <- read.csv("Data/Phylo_div_results/MNTD_UH_2014_nolumped.csv")

pd_a_2014 <- read.csv("Data/Phylo_div_results/PD_A_2014_nolumped.csv")
mpd_a_2014 <- read.csv("Data/Phylo_div_results/MPD_A_2014_nolumped.csv")
mntd_a_2014 <- read.csv("Data/Phylo_div_results/MNTD_A_2014_nolumped.csv")

# format tables to be combined -------------------------------------
## 1950 --------
### SB ####

# PD
# get rid of extra columns
pd_sb_1950 = subset(pd_sb_1950, select = -c(ntaxa,
                                            pd.obs,
                                            pd.rand.mean,
                                            pd.rand.sd,
                                            pd.obs.rank,
                                            runs))

# rename columns
pd_sb_1950 <- pd_sb_1950 %>% 
  rename(plot = X,
         SES = pd.obs.z, 
         pvalue = pd.obs.p)

# remove total pool row
pd_sb_1950 <- pd_sb_1950[-c(28,29), ]

# add columns 
pd_sb_1950$year <- "1950"
pd_sb_1950$community <- "sagebrush"
pd_sb_1950$metric <- "PD"
pd_sb_1950$replicates <- paste("SB","",pd_sb_1950$plot, sep = "")
pd_sb_1950 <- pd_sb_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
pd_sb_1950 <- left_join(pd_sb_1950, sb_gps, by = "replicates")

# get rid of extra columns 
pd_sb_1950 = subset(pd_sb_1950, select = -c(plot, 
                                            community.y,
                                            Lat,
                                            Long,
                                            Slope_.))


# MPD 

#get rid of extra columns
mpd_sb_1950 = subset(mpd_sb_1950, select = -c(ntaxa,
                                              mpd.obs,
                                              mpd.rand.mean,
                                              mpd.rand.sd,
                                              mpd.obs.rank,
                                              runs))

# rename columns
mpd_sb_1950 <- mpd_sb_1950 %>% 
  rename(plot = X,
         SES = mpd.obs.z, 
         pvalue = mpd.obs.p)

# remove total pool row
mpd_sb_1950 <- mpd_sb_1950[-c(28,29), ]

# add columns 
mpd_sb_1950$year <- "1950"
mpd_sb_1950$community <- "sagebrush"
mpd_sb_1950$metric <- "MPD"
mpd_sb_1950$replicates <- paste("SB","",mpd_sb_1950$plot, sep = "")
mpd_sb_1950 <- mpd_sb_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mpd_sb_1950 <- left_join(mpd_sb_1950, sb_gps, by = "replicates")

# get rid of extra columns 
mpd_sb_1950 = subset(mpd_sb_1950, select = -c(plot, 
                                              community.y,
                                              Lat,
                                              Long,
                                              Slope_.))

# MNTD
mntd_sb_1950 = subset(mntd_sb_1950, select = -c(ntaxa,
                                                mntd.obs,
                                                mntd.rand.mean,
                                                mntd.rand.sd,
                                                mntd.obs.rank,
                                                runs))

# rename columns
mntd_sb_1950 <- mntd_sb_1950 %>% 
  rename(plot = X,
         SES = mntd.obs.z, 
         pvalue = mntd.obs.p)

# remove total pool row
mntd_sb_1950 <- mntd_sb_1950[-c(28,29), ]

# add columns 
mntd_sb_1950$year <- "1950"
mntd_sb_1950$community <- "sagebrush"
mntd_sb_1950$metric <- "MNTD"
mntd_sb_1950$replicates <- paste("SB","",mntd_sb_1950$plot, sep = "")
mntd_sb_1950 <- mntd_sb_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mntd_sb_1950 <- left_join(mntd_sb_1950, sb_gps, by = "replicates")

# get rid of extra columns 
mntd_sb_1950 = subset(mntd_sb_1950, select = -c(plot, 
                                                community.y,
                                                Lat,
                                                Long,
                                                Slope_.))

#### combine all sagebrush -------
sb_1950_phylo <- rbind(pd_sb_1950, mpd_sb_1950, mntd_sb_1950)

### SF -----------------------
# PD
# get rid of extra columns
pd_sf_1950 = subset(pd_sf_1950, select = -c(ntaxa,
                                            pd.obs,
                                            pd.rand.mean,
                                            pd.rand.sd,
                                            pd.obs.rank,
                                            runs))

# rename columns
pd_sf_1950 <- pd_sf_1950 %>% 
  rename(plot = X,
         SES = pd.obs.z, 
         pvalue = pd.obs.p)

# remove total pool row
pd_sf_1950 <- pd_sf_1950[-c(28,29), ]

# add columns 
pd_sf_1950$year <- "1950"
pd_sf_1950$community <- "spruce-fir"
pd_sf_1950$metric <- "PD"
pd_sf_1950$replicates <- paste("SF","",pd_sf_1950$plot, sep = "")
pd_sf_1950 <- pd_sf_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
pd_sf_1950 <- left_join(pd_sf_1950, sf_gps, by = "replicates")

# get rid of extra columns 
pd_sf_1950 = subset(pd_sf_1950, select = -c(plot, 
                                            community.y,
                                            Lat,
                                            Long,
                                            Slope_.))

# MPD
# get rid of extra columns
mpd_sf_1950 = subset(mpd_sf_1950, select = -c(ntaxa,
                                              mpd.obs,
                                              mpd.rand.mean,
                                              mpd.rand.sd,
                                              mpd.obs.rank,
                                              runs))

# rename columns
mpd_sf_1950 <- mpd_sf_1950 %>% 
  rename(plot = X,
         SES = mpd.obs.z, 
         pvalue = mpd.obs.p)

# remove total pool row
mpd_sf_1950 <- mpd_sf_1950[-c(28,29), ]

# add columns 
mpd_sf_1950$year <- "1950"
mpd_sf_1950$community <- "spruce-fir"
mpd_sf_1950$metric <- "MPD"
mpd_sf_1950$replicates <- paste("SF","",mpd_sf_1950$plot, sep = "")
mpd_sf_1950 <- mpd_sf_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mpd_sf_1950 <- left_join(mpd_sf_1950, sf_gps, by = "replicates")

# get rid of extra columns 
mpd_sf_1950 = subset(mpd_sf_1950, select = -c(plot, 
                                              community.y,
                                              Lat,
                                              Long,
                                              Slope_.))

# MNTD
# get rid of extra columns
mntd_sf_1950 = subset(mntd_sf_1950, select = -c(ntaxa,
                                                mntd.obs,
                                                mntd.rand.mean,
                                                mntd.rand.sd,
                                                mntd.obs.rank,
                                                runs))

# rename columns
mntd_sf_1950 <- mntd_sf_1950 %>% 
  rename(plot = X,
         SES = mntd.obs.z, 
         pvalue = mntd.obs.p)

# remove total pool row
mntd_sf_1950 <- mntd_sf_1950[-c(28,29), ]

# add columns 
mntd_sf_1950$year <- "1950"
mntd_sf_1950$community <- "spruce-fir"
mntd_sf_1950$metric <- "MNTD"
mntd_sf_1950$replicates <- paste("SF","",mntd_sf_1950$plot, sep = "")
mntd_sf_1950 <- mntd_sf_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mntd_sf_1950 <- left_join(mntd_sf_1950, sf_gps, by = "replicates")

# get rid of extra columns 
mntd_sf_1950 = subset(mntd_sf_1950, select = -c(plot, 
                                                community.y,
                                                Lat,
                                                Long,
                                                Slope_.))

#### combine all sprucefir -------
sf_1950_phylo <- rbind(pd_sf_1950, mpd_sf_1950, mntd_sf_1950)

### UH ---------
# PD
# get rid of extra columns
pd_uh_1950 = subset(pd_uh_1950, select = -c(ntaxa,
                                            pd.obs,
                                            pd.rand.mean,
                                            pd.rand.sd,
                                            pd.obs.rank,
                                            runs))

# rename columns
pd_uh_1950 <- pd_uh_1950 %>% 
  rename(plot = X,
         SES = pd.obs.z, 
         pvalue = pd.obs.p)

# remove total pool row
pd_uh_1950 <- pd_uh_1950[-c(32,33), ]

# add columns 
pd_uh_1950$year <- "1950"
pd_uh_1950$community <- "upland-herb"
pd_uh_1950$metric <- "PD"
pd_uh_1950$replicates <- paste("U","",pd_uh_1950$plot, sep = "")
pd_uh_1950 <- pd_uh_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
pd_uh_1950 <- left_join(pd_uh_1950, uh_gps, by = "replicates")

# get rid of extra columns 
pd_uh_1950 = subset(pd_uh_1950, select = -c(plot, 
                                            community.y,
                                            Lat,
                                            Long,
                                            Slope_.))

# MPD
# get rid of extra columns
mpd_uh_1950 = subset(mpd_uh_1950, select = -c(ntaxa,
                                              mpd.obs,
                                              mpd.rand.mean,
                                              mpd.rand.sd,
                                              mpd.obs.rank,
                                              runs))

# rename columns
mpd_uh_1950 <- mpd_uh_1950 %>% 
  rename(plot = X,
         SES = mpd.obs.z, 
         pvalue = mpd.obs.p)

# remove total pool row
mpd_uh_1950 <- mpd_uh_1950[-c(32,33), ]

# add columns 
mpd_uh_1950$year <- "1950"
mpd_uh_1950$community <- "upland-herb"
mpd_uh_1950$metric <- "MPD"
mpd_uh_1950$replicates <- paste("U","",mpd_uh_1950$plot, sep = "")
mpd_uh_1950 <- mpd_uh_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mpd_uh_1950 <- left_join(mpd_uh_1950, uh_gps, by = "replicates")

# get rid of extra columns 
mpd_uh_1950 = subset(mpd_uh_1950, select = -c(plot, 
                                              community.y,
                                              Lat,
                                              Long,
                                              Slope_.))

# MNTD
# get rid of extra columns
mntd_uh_1950 = subset(mntd_uh_1950, select = -c(ntaxa,
                                                mntd.obs,
                                                mntd.rand.mean,
                                                mntd.rand.sd,
                                                mntd.obs.rank,
                                                runs))

# rename columns
mntd_uh_1950 <- mntd_uh_1950 %>% 
  rename(plot = X,
         SES = mntd.obs.z, 
         pvalue = mntd.obs.p)

# remove total pool row
mntd_uh_1950 <- mntd_uh_1950[-c(32,33), ]

# add columns 
mntd_uh_1950$year <- "1950"
mntd_uh_1950$community <- "upland-herb"
mntd_uh_1950$metric <- "MNTD"
mntd_uh_1950$replicates <- paste("U","",mntd_uh_1950$plot, sep = "")
mntd_uh_1950 <- mntd_uh_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mntd_uh_1950 <- left_join(mntd_uh_1950, uh_gps, by = "replicates")

# get rid of extra columns 
mntd_uh_1950 = subset(mntd_uh_1950, select = -c(plot, 
                                                community.y,
                                                Lat,
                                                Long,
                                                Slope_.))


#### combine all upland herb -------
uh_1950_phylo <- rbind(pd_uh_1950, mpd_uh_1950, mntd_uh_1950)

### A ----------
# PD
# get rid of extra columns
pd_a_1950 = subset(pd_a_1950, select = -c(ntaxa,
                                          pd.obs,
                                          pd.rand.mean,
                                          pd.rand.sd,
                                          pd.obs.rank,
                                          runs))

# rename columns
pd_a_1950 <- pd_a_1950 %>% 
  rename(plot = X,
         SES = pd.obs.z, 
         pvalue = pd.obs.p)

# remove total pool row
pd_a_1950 <- pd_a_1950[-c(36,37), ]

# add columns 
pd_a_1950$year <- "1950"
pd_a_1950$community <- "alpine"
pd_a_1950$metric <- "PD"
pd_a_1950$replicates <- paste("A","",pd_a_1950$plot, sep = "")
pd_a_1950 <- pd_a_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
pd_a_1950 <- left_join(pd_a_1950, a_gps, by = "replicates")

# get rid of extra columns 
pd_a_1950 = subset(pd_a_1950, select = -c(plot, 
                                          community.y,
                                          Lat,
                                          Long,
                                          Slope_.))


# MPD
# get rid of extra columns
mpd_a_1950 = subset(mpd_a_1950, select = -c(ntaxa,
                                            mpd.obs,
                                            mpd.rand.mean,
                                            mpd.rand.sd,
                                            mpd.obs.rank,
                                            runs))

# rename columns
mpd_a_1950 <- mpd_a_1950 %>% 
  rename(plot = X,
         SES = mpd.obs.z, 
         pvalue = mpd.obs.p)

# remove total pool row
mpd_a_1950 <- mpd_a_1950[-c(36,37), ]

# add columns 
mpd_a_1950$year <- "1950"
mpd_a_1950$community <- "alpine"
mpd_a_1950$metric <- "MPD"
mpd_a_1950$replicates <- paste("A","",mpd_a_1950$plot, sep = "")
mpd_a_1950 <- mpd_a_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mpd_a_1950 <- left_join(mpd_a_1950, a_gps, by = "replicates")

# get rid of extra columns 
mpd_a_1950 = subset(mpd_a_1950, select = -c(plot, 
                                            community.y,
                                            Lat,
                                            Long,
                                            Slope_.))

# MNTD
# get rid of extra columns
mntd_a_1950 = subset(mntd_a_1950, select = -c(ntaxa,
                                              mntd.obs,
                                              mntd.rand.mean,
                                              mntd.rand.sd,
                                              mntd.obs.rank,
                                              runs))

# rename columns
mntd_a_1950 <- mntd_a_1950 %>% 
  rename(plot = X,
         SES = mntd.obs.z, 
         pvalue = mntd.obs.p)

# remove total pool row
mntd_a_1950 <- mntd_a_1950[-c(36,37), ]

# add columns 
mntd_a_1950$year <- "1950"
mntd_a_1950$community <- "alpine"
mntd_a_1950$metric <- "MNTD"
mntd_a_1950$replicates <- paste("A","",mntd_a_1950$plot, sep = "")
mntd_a_1950 <- mntd_a_1950 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mntd_a_1950 <- left_join(mntd_a_1950, a_gps, by = "replicates")

# get rid of extra columns 
mntd_a_1950 = subset(mntd_a_1950, select = -c(plot, 
                                              community.y,
                                              Lat,
                                              Long,
                                              Slope_.))

#### combine all alpine -------
a_1950_phylo <- rbind(pd_a_1950, mpd_a_1950, mntd_a_1950)

# combine into one 1950 dataset ------
phylo_1950 <- rbind(sb_1950_phylo, sf_1950_phylo, uh_1950_phylo, a_1950_phylo)

## 2014 --------------

### SB ########

# PD
# get rid of extra columns
pd_sb_2014 = subset(pd_sb_2014, select = -c(ntaxa,
                                            pd.obs,
                                            pd.rand.mean,
                                            pd.rand.sd,
                                            pd.obs.rank,
                                            runs))

# rename columns
pd_sb_2014 <- pd_sb_2014 %>% 
  rename(plot = X,
         SES = pd.obs.z, 
         pvalue = pd.obs.p)

# remove total pool row
pd_sb_2014 <- pd_sb_2014[-c(28,29), ]

# add columns 
pd_sb_2014$year <- "2014"
pd_sb_2014$community <- "sagebrush"
pd_sb_2014$metric <- "PD"
pd_sb_2014$replicates <- paste("SB","",pd_sb_2014$plot, sep = "")
pd_sb_2014 <- pd_sb_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
pd_sb_2014 <- left_join(pd_sb_2014, sb_gps, by = "replicates")

# get rid of extra columns 
pd_sb_2014 = subset(pd_sb_2014, select = -c(plot, 
                                            community.y,
                                            Lat,
                                            Long,
                                            Slope_.))

# MPD 

#get rid of extra columns
mpd_sb_2014 = subset(mpd_sb_2014, select = -c(ntaxa,
                                              mpd.obs,
                                              mpd.rand.mean,
                                              mpd.rand.sd,
                                              mpd.obs.rank,
                                              runs))

# rename columns
mpd_sb_2014 <- mpd_sb_2014 %>% 
  rename(plot = X,
         SES = mpd.obs.z, 
         pvalue = mpd.obs.p)

# remove total pool row
mpd_sb_2014 <- mpd_sb_2014[-c(28,29), ]

# add columns 
mpd_sb_2014$year <- "2014"
mpd_sb_2014$community <- "sagebrush"
mpd_sb_2014$metric <- "MPD"
mpd_sb_2014$replicates <- paste("SB","",mpd_sb_2014$plot, sep = "")
mpd_sb_2014 <- mpd_sb_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mpd_sb_2014 <- left_join(mpd_sb_2014, sb_gps, by = "replicates")

# get rid of extra columns 
mpd_sb_2014 = subset(mpd_sb_2014, select = -c(plot, 
                                              community.y,
                                              Lat,
                                              Long,
                                              Slope_.))


# MNTD
mntd_sb_2014 = subset(mntd_sb_2014, select = -c(ntaxa,
                                                mntd.obs,
                                                mntd.rand.mean,
                                                mntd.rand.sd,
                                                mntd.obs.rank,
                                                runs))

# rename columns
mntd_sb_2014 <- mntd_sb_2014 %>% 
  rename(plot = X,
         SES = mntd.obs.z, 
         pvalue = mntd.obs.p)

# remove total pool row
mntd_sb_2014 <- mntd_sb_2014[-c(28,29), ]

# add columns 
mntd_sb_2014$year <- "2014"
mntd_sb_2014$community <- "sagebrush"
mntd_sb_2014$metric <- "MNTD"
mntd_sb_2014$replicates <- paste("SB","",mntd_sb_2014$plot, sep = "")
mntd_sb_2014 <- mntd_sb_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mntd_sb_2014 <- left_join(mntd_sb_2014, sb_gps, by = "replicates")

# get rid of extra columns 
mntd_sb_2014 = subset(mntd_sb_2014, select = -c(plot, 
                                                community.y,
                                                Lat,
                                                Long,
                                                Slope_.))


#### combine all sagebrush -------
sb_2014_phylo <- rbind(pd_sb_2014, mpd_sb_2014, mntd_sb_2014)


### SF -----------------------
# PD
# get rid of extra columns
pd_sf_2014 = subset(pd_sf_2014, select = -c(ntaxa,
                                            pd.obs,
                                            pd.rand.mean,
                                            pd.rand.sd,
                                            pd.obs.rank,
                                            runs))

# rename columns
pd_sf_2014 <- pd_sf_2014 %>% 
  rename(plot = X,
         SES = pd.obs.z, 
         pvalue = pd.obs.p)

# remove total pool row
pd_sf_2014 <- pd_sf_2014[-c(32,33), ]

# add columns 
pd_sf_2014$year <- "2014"
pd_sf_2014$community <- "spruce-fir"
pd_sf_2014$metric <- "PD"
pd_sf_2014$replicates <- paste("SF","",pd_sf_2014$plot, sep = "")
pd_sf_2014 <- pd_sf_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
pd_sf_2014 <- left_join(pd_sf_2014, sf_gps, by = "replicates")

# get rid of extra columns 
pd_sf_2014 = subset(pd_sf_2014, select = -c(plot, 
                                            community.y,
                                            Lat,
                                            Long,
                                            Slope_.))

# MPD
# get rid of extra columns
mpd_sf_2014 = subset(mpd_sf_2014, select = -c(ntaxa,
                                              mpd.obs,
                                              mpd.rand.mean,
                                              mpd.rand.sd,
                                              mpd.obs.rank,
                                              runs))

# rename columns
mpd_sf_2014 <- mpd_sf_2014 %>% 
  rename(plot = X,
         SES = mpd.obs.z, 
         pvalue = mpd.obs.p)

# remove total pool row
mpd_sf_2014 <- mpd_sf_2014[-c(32,33), ]

# add columns 
mpd_sf_2014$year <- "2014"
mpd_sf_2014$community <- "spruce-fir"
mpd_sf_2014$metric <- "MPD"
mpd_sf_2014$replicates <- paste("SF","",mpd_sf_2014$plot, sep = "")
mpd_sf_2014 <- mpd_sf_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mpd_sf_2014 <- left_join(mpd_sf_2014, sf_gps, by = "replicates")

# get rid of extra columns 
mpd_sf_2014 = subset(mpd_sf_2014, select = -c(plot, 
                                              community.y,
                                              Lat,
                                              Long,
                                              Slope_.))

# MNTD
# get rid of extra columns
mntd_sf_2014 = subset(mntd_sf_2014, select = -c(ntaxa,
                                                mntd.obs,
                                                mntd.rand.mean,
                                                mntd.rand.sd,
                                                mntd.obs.rank,
                                                runs))

# rename columns
mntd_sf_2014 <- mntd_sf_2014 %>% 
  rename(plot = X,
         SES = mntd.obs.z, 
         pvalue = mntd.obs.p)

# remove total pool row
mntd_sf_2014 <- mntd_sf_2014[-c(32,33), ]

# add columns 
mntd_sf_2014$year <- "2014"
mntd_sf_2014$community <- "spruce-fir"
mntd_sf_2014$metric <- "MNTD"
mntd_sf_2014$replicates <- paste("SF","",mntd_sf_2014$plot, sep = "")
mntd_sf_2014 <- mntd_sf_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mntd_sf_2014 <- left_join(mntd_sf_2014, sf_gps, by = "replicates")

# get rid of extra columns 
mntd_sf_2014 = subset(mntd_sf_2014, select = -c(plot, 
                                                community.y,
                                                Lat,
                                                Long,
                                                Slope_.))


#### combine all sprucefir -------
sf_2014_phylo <- rbind(pd_sf_2014, mpd_sf_2014, mntd_sf_2014)

### UH ---------
# PD
# get rid of extra columns
pd_uh_2014 = subset(pd_uh_2014, select = -c(ntaxa,
                                            pd.obs,
                                            pd.rand.mean,
                                            pd.rand.sd,
                                            pd.obs.rank,
                                            runs))

# rename columns
pd_uh_2014 <- pd_uh_2014 %>% 
  rename(plot = X,
         SES = pd.obs.z, 
         pvalue = pd.obs.p)

# remove total pool row
pd_uh_2014 <- pd_uh_2014[-c(31,32), ]

# add columns 
pd_uh_2014$year <- "2014"
pd_uh_2014$community <- "upland-herb"
pd_uh_2014$metric <- "PD"
pd_uh_2014$replicates <- paste("U","",pd_uh_2014$plot, sep = "")
pd_uh_2014 <- pd_uh_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
pd_uh_2014 <- left_join(pd_uh_2014, uh_gps, by = "replicates")

# get rid of extra columns 
pd_uh_2014 = subset(pd_uh_2014, select = -c(plot, 
                                            community.y,
                                            Lat,
                                            Long,
                                            Slope_.))

# MPD
# get rid of extra columns
mpd_uh_2014 = subset(mpd_uh_2014, select = -c(ntaxa,
                                              mpd.obs,
                                              mpd.rand.mean,
                                              mpd.rand.sd,
                                              mpd.obs.rank,
                                              runs))

# rename columns
mpd_uh_2014 <- mpd_uh_2014 %>% 
  rename(plot = X,
         SES = mpd.obs.z, 
         pvalue = mpd.obs.p)

# remove total pool row
mpd_uh_2014 <- mpd_uh_2014[-c(31,32), ]

# add columns 
mpd_uh_2014$year <- "2014"
mpd_uh_2014$community <- "upland-herb"
mpd_uh_2014$metric <- "MPD"
mpd_uh_2014$replicates <- paste("U","",mpd_uh_2014$plot, sep = "")
mpd_uh_2014 <- mpd_uh_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mpd_uh_2014 <- left_join(mpd_uh_2014, uh_gps, by = "replicates")

# get rid of extra columns 
mpd_uh_2014 = subset(mpd_uh_2014, select = -c(plot, 
                                              community.y,
                                              Lat,
                                              Long,
                                              Slope_.))

# MNTD
# get rid of extra columns
mntd_uh_2014 = subset(mntd_uh_2014, select = -c(ntaxa,
                                                mntd.obs,
                                                mntd.rand.mean,
                                                mntd.rand.sd,
                                                mntd.obs.rank,
                                                runs))

# rename columns
mntd_uh_2014 <- mntd_uh_2014 %>% 
  rename(plot = X,
         SES = mntd.obs.z, 
         pvalue = mntd.obs.p)

# remove total pool row
mntd_uh_2014 <- mntd_uh_2014[-c(31,32), ]

# add columns 
mntd_uh_2014$year <- "2014"
mntd_uh_2014$community <- "upland-herb"
mntd_uh_2014$metric <- "MNTD"
mntd_uh_2014$replicates <- paste("U","",mntd_uh_2014$plot, sep = "")
mntd_uh_2014 <- mntd_uh_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mntd_uh_2014 <- left_join(mntd_uh_2014, uh_gps, by = "replicates")

# get rid of extra columns 
mntd_uh_2014 = subset(mntd_uh_2014, select = -c(plot, 
                                                community.y,
                                                Lat,
                                                Long,
                                                Slope_.))

#### combine all upland herb -------
uh_2014_phylo <- rbind(pd_uh_2014, mpd_uh_2014, mntd_uh_2014) 

### A ----------
# PD
# get rid of extra columns
pd_a_2014 = subset(pd_a_2014, select = -c(ntaxa,
                                          pd.obs,
                                          pd.rand.mean,
                                          pd.rand.sd,
                                          pd.obs.rank,
                                          runs))

# rename columns
pd_a_2014 <- pd_a_2014 %>% 
  rename(plot = X,
         SES = pd.obs.z, 
         pvalue = pd.obs.p)

# remove total pool row
pd_a_2014 <- pd_a_2014[-c(34,35), ]

# add columns 
pd_a_2014$year <- "2014"
pd_a_2014$community <- "alpine"
pd_a_2014$metric <- "PD"
pd_a_2014$replicates <- paste("A","",pd_a_2014$plot, sep = "")
pd_a_2014 <- pd_a_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
pd_a_2014 <- left_join(pd_a_2014, a_gps, by = "replicates")

# get rid of extra columns 
pd_a_2014 = subset(pd_a_2014, select = -c(plot, 
                                          community.y,
                                          Lat,
                                          Long,
                                          Slope_.))

# MPD
# get rid of extra columns
mpd_a_2014 = subset(mpd_a_2014, select = -c(ntaxa,
                                            mpd.obs,
                                            mpd.rand.mean,
                                            mpd.rand.sd,
                                            mpd.obs.rank,
                                            runs))

# rename columns
mpd_a_2014 <- mpd_a_2014 %>% 
  rename(plot = X,
         SES = mpd.obs.z, 
         pvalue = mpd.obs.p)

# remove total pool row
mpd_a_2014 <- mpd_a_2014[-c(34,35), ]

# add columns 
mpd_a_2014$year <- "2014"
mpd_a_2014$community <- "alpine"
mpd_a_2014$metric <- "MPD"
mpd_a_2014$replicates <- paste("A","",mpd_a_2014$plot, sep = "")
mpd_a_2014 <- mpd_a_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mpd_a_2014 <- left_join(mpd_a_2014, a_gps, by = "replicates")

# get rid of extra columns 
mpd_a_2014 = subset(mpd_a_2014, select = -c(plot, 
                                            community.y,
                                            Lat,
                                            Long,
                                            Slope_.))

# MNTD
# get rid of extra columns
mntd_a_2014 = subset(mntd_a_2014, select = -c(ntaxa,
                                              mntd.obs,
                                              mntd.rand.mean,
                                              mntd.rand.sd,
                                              mntd.obs.rank,
                                              runs))

# rename columns
mntd_a_2014 <- mntd_a_2014 %>% 
  rename(plot = X,
         SES = mntd.obs.z, 
         pvalue = mntd.obs.p)

# remove total pool row
mntd_a_2014 <- mntd_a_2014[-c(34,35), ]

# add columns 
mntd_a_2014$year <- "2014"
mntd_a_2014$community <- "alpine"
mntd_a_2014$metric <- "MNTD"
mntd_a_2014$replicates <- paste("A","",mntd_a_2014$plot, sep = "")
mntd_a_2014 <- mntd_a_2014 %>% mutate(replicates = as.factor(gsub("plot_", "", replicates)))

# add gps data 
mntd_a_2014 <- left_join(mntd_a_2014, a_gps, by = "replicates")

# get rid of extra columns 
mntd_a_2014 = subset(mntd_a_2014, select = -c(plot, 
                                              community.y,
                                              Lat,
                                              Long,
                                              Slope_.))

#### combine all alpine -------
a_2014_phylo <- rbind(pd_a_2014, mpd_a_2014, mntd_a_2014)

# combine into one 2014 dataset ------
phylo_2014 <- rbind(sb_2014_phylo, sf_2014_phylo, uh_2014_phylo, a_2014_phylo)

# combine into one dataset for all --------
phylo_df_NL <- rbind(phylo_1950, phylo_2014)

phylo_df_NL <- phylo_df_NL %>% 
  rename(community = community.x)

write.csv(phylo_df_NL, "Data/Phylo_div_results/all_phylo_div_results_nolumped.csv")

# make data frames for each community type -------

sb_df <- phylo_df_NL %>% filter(community %in% "sagebrush")
sf_df <- phylo_df_NL %>% filter(community %in% "spruce-fir")
uh_df <- phylo_df_NL %>% filter(community %in% "upland-herb")
a_df <- phylo_df_NL %>% filter(community %in% "alpine")

# split dataframes to subtract 2014 SES from 1950 SES for PD change -----------

## sagebrush -------
# split by year and rename SES column
sb_1950_df <- sb_df %>% filter(year %in% "1950")
sb_1950_df <- sb_1950_df %>% 
  rename(SES_1950 = SES,
         pvalue_1950 = pvalue)

sb_2014_df <- sb_df %>% filter(year %in% "2014")
sb_2014_df <- sb_2014_df %>% 
  rename(SES_2014 = SES,
         pvalue_2014 = pvalue)

# bring dataframes back together
sb_df <- left_join(sb_1950_df, sb_2014_df, by = c("replicates", "metric"))
sb_df$SES_change <- sb_df$SES_2014-sb_df$SES_1950

# get rid of extra columns
sb_df = subset(sb_df, select = -c(year.x,
                                  year.y,
                                  community.x,
                                  Aspect.x,
                                  Elevation_m.x,
                                  Substrate.x))
# rename columns 
sb_df <- sb_df %>% 
  rename(community = community.y,
         Aspect = Aspect.y, 
         Elevation_m = Elevation_m.y, 
         Substrate = Substrate.y)

## spruce-fir --------

# split by year and rename SES column
sf_1950_df <- sf_df %>% filter(year %in% "1950")
sf_1950_df <- sf_1950_df %>% 
  rename(SES_1950 = SES,
         pvalue_1950 = pvalue)

sf_2014_df <- sf_df %>% filter(year %in% "2014")
sf_2014_df <- sf_2014_df %>% 
  rename(SES_2014 = SES,
         pvalue_2014 = pvalue)

# bring dataframes back together
sf_df <- left_join(sf_1950_df, sf_2014_df, by = c("replicates", "metric"))
sf_df$SES_change <- sf_df$SES_2014-sf_df$SES_1950

# get rid of extra columns
sf_df = subset(sf_df, select = -c(year.x,
                                  year.y,
                                  community.x,
                                  Aspect.x,
                                  Elevation_m.x,
                                  Substrate.x))
# rename columns 
sf_df <- sf_df %>% 
  rename(community = community.y,
         Aspect = Aspect.y, 
         Elevation_m = Elevation_m.y, 
         Substrate = Substrate.y)

## upland herb -----------

# split by year and rename SES column
uh_1950_df <- uh_df %>% filter(year %in% "1950")
uh_1950_df <- uh_1950_df %>% 
  rename(SES_1950 = SES,
         pvalue_1950 = pvalue)

uh_2014_df <- uh_df %>% filter(year %in% "2014")
uh_2014_df <- uh_2014_df %>% 
  rename(SES_2014 = SES,
         pvalue_2014 = pvalue)

# bring dataframes back together
uh_df <- left_join(uh_1950_df, uh_2014_df, by = c("replicates", "metric"))
uh_df$SES_change <- uh_df$SES_2014-uh_df$SES_1950

# get rid of extra columns
uh_df = subset(uh_df, select = -c(year.x,
                                  year.y,
                                  community.x,
                                  Aspect.x,
                                  Elevation_m.x,
                                  Substrate.x))
# rename columns 
uh_df <- uh_df %>% 
  rename(community = community.y,
         Aspect = Aspect.y, 
         Elevation_m = Elevation_m.y, 
         Substrate = Substrate.y)

## alpine ------------

# split by year and rename SES column
a_1950_df <- a_df %>% filter(year %in% "1950")
a_1950_df <- a_1950_df %>% 
  rename(SES_1950 = SES,
         pvalue_1950 = pvalue)

a_2014_df <- a_df %>% filter(year %in% "2014")
a_2014_df <- a_2014_df %>% 
  rename(SES_2014 = SES,
         pvalue_2014 = pvalue)

# bring dataframes back together
a_df <- left_join(a_1950_df, a_2014_df, by = c("replicates", "metric"))
a_df$SES_change <- a_df$SES_2014-a_df$SES_1950

# get rid of extra columns
a_df = subset(a_df, select = -c(year.x,
                                year.y,
                                community.x,
                                Aspect.x,
                                Elevation_m.x,
                                Substrate.x))
# rename columns 
a_df <- a_df %>% 
  rename(community = community.y,
         Aspect = Aspect.y, 
         Elevation_m = Elevation_m.y, 
         Substrate = Substrate.y)

# combine 
phylo_paired_df <- rbind(sb_df, sf_df, uh_df, a_df)

# remove NAs from plots wtih only one species 
phylo_paired_df_NL <- phylo_paired_df[!is.na(phylo_paired_df$SES_change),]

# save as
write.csv(phylo_paired_df_NL, "Data/Phylo_div_results/all_PD_paired_nolumped.csv")
