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

# 1950 with lumped genera removed --------------
## SB --------
### PD ------
pd_sb_1950_nolumped <- ses.pd(sb_50_NL_mat, Zorio.tree, 
                              null.model = c("sample.pool"),runs = 999, 
                              include.root=TRUE)

write.csv(pd_sb_1950_nolumped, "Data/Phylo_div_results/PD_SB_1950_nolumped.csv")

### MPD -------
mpd_sb_1950_nolumped <- ses.mpd(sb_50_NL_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                       runs = 999)

write.csv(mpd_sb_1950_nolumped, "Data/Phylo_div_results/MPD_SB_1950_nolumped.csv")

### MNTD ------
mntd_sb_1950_NL <- ses.mntd(sb_50_NL_mat, cophenetic(Zorio.tree), 
                         null.model = c("sample.pool"),
                         runs = 999)

write.csv(mntd_sb_1950_NL, "Data/Phylo_div_results/MNTD_SB_1950_nolumped.csv")

## SF -----------
### PD -------
pd_sf_1950_nolumped <- ses.pd(sf_50_NL_mat, Zorio.tree, 
                     null.model = c("sample.pool"),
                     runs = 999, include.root=TRUE)

write.csv(pd_sf_1950_nolumped, "Data/Phylo_div_results/PD_SF_1950_nolumped.csv")

### MPD -------
mpd_sf_1950_NL <- ses.mpd(sf_50_NL_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                       runs = 999)

write.csv(mpd_sf_1950_NL, "Data/Phylo_div_results/MPD_SF_1950_nolumped.csv")

### MNTD ------
mntd_sf_1950_NL <- ses.mntd(sf_50_NL_mat, cophenetic(Zorio.tree), 
                         null.model = c("sample.pool"),
                         runs = 999)

write.csv(mntd_sf_1950_NL, "Data/Phylo_div_results/MNTD_SF_1950_nolumped.csv")

## UH ----------
### PD -------
pd_uh_1950_NL <- ses.pd(uh_50_NL_mat, Zorio.tree, 
                     null.model = c("sample.pool"),
                     runs = 999, include.root=TRUE)

write.csv(pd_uh_1950_NL, "Data/Phylo_div_results/PD_UH_1950_nolumped.csv")

### MPD -------
mpd_uh_1950_NL <- ses.mpd(uh_50_NL_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                       runs = 999)

write.csv(mpd_uh_1950_NL, "Data/Phylo_div_results/MPD_UH_1950_nolumped.csv")

### MNTD ------
mntd_uh_1950_NL <- ses.mntd(uh_50_NL_mat, cophenetic(Zorio.tree),
                         null.model = c("sample.pool"),
                         runs = 999)

write.csv(mntd_uh_1950_NL, "Data/Phylo_div_results/MNTD_UH_1950_nolumped.csv")

## A ------------------
### PD -------
pd_a_1950_NL <- ses.pd(a_50_NL_mat, Zorio.tree, 
                    null.model = c("sample.pool"),
                    runs = 999, include.root=TRUE)

write.csv(pd_a_1950_NL, "Data/Phylo_div_results/PD_A_1950_nolumped.csv")

### MPD -------
mpd_a_1950_NL <- ses.mpd(a_50_NL_mat, cophenetic(Zorio.tree), 
                      null.model = c("sample.pool"),
                      runs = 999)
write.csv(mpd_a_1950_NL, "Data/Phylo_div_results/MPD_A_1950_nolumped.csv")

### MNTD ------
mntd_a_1950_NL <- ses.mntd(a_50_NL_mat, cophenetic(Zorio.tree), 
                        null.model = c("sample.pool"),
                        runs = 999)

write.csv(mntd_a_1950_NL, "Data/Phylo_div_results/MNTD_A_1950_nolumped.csv")

# 2014 with lumped genera removed ---------
## SB --------
### PD ------
pd_sb_2014_nolumped <- ses.pd(sb_14_NL_mat, Zorio.tree, 
                              null.model = c("sample.pool"),runs = 999, 
                              include.root=TRUE)

write.csv(pd_sb_2014_nolumped, "Data/Phylo_div_results/PD_SB_2014_nolumped.csv")

### MPD -------
mpd_sb_2014_nolumped <- ses.mpd(sb_14_NL_mat, cophenetic(Zorio.tree), 
                                null.model = c("sample.pool"),
                                runs = 999)

write.csv(mpd_sb_2014_nolumped, "Data/Phylo_div_results/MPD_SB_2014_nolumped.csv")

### MNTD ------
mntd_sb_2014_NL <- ses.mntd(sb_14_NL_mat, cophenetic(Zorio.tree), 
                            null.model = c("sample.pool"),
                            runs = 999)

write.csv(mntd_sb_2014_NL, "Data/Phylo_div_results/MNTD_SB_2014_nolumped.csv")

## SF -----------
### PD -------
pd_sf_2014_nolumped <- ses.pd(sf_14_NL_mat, Zorio.tree, 
                              null.model = c("sample.pool"),
                              runs = 999, include.root=TRUE)

write.csv(pd_sf_2014_nolumped, "Data/Phylo_div_results/PD_SF_2014_nolumped.csv")

### MPD -------
mpd_sf_2014_NL <- ses.mpd(sf_14_NL_mat, cophenetic(Zorio.tree), 
                          null.model = c("sample.pool"),
                          runs = 999)

write.csv(mpd_sf_2014_NL, "Data/Phylo_div_results/MPD_SF_2014_nolumped.csv")

### MNTD ------
mntd_sf_2014_NL <- ses.mntd(sf_14_NL_mat, cophenetic(Zorio.tree), 
                            null.model = c("sample.pool"),
                            runs = 999)

write.csv(mntd_sf_2014_NL, "Data/Phylo_div_results/MNTD_SF_2014_nolumped.csv")

## UH ----------
### PD -------
pd_uh_2014_NL <- ses.pd(uh_14_NL_mat, Zorio.tree, 
                        null.model = c("sample.pool"),
                        runs = 999, include.root=TRUE)

write.csv(pd_uh_2014_NL, "Data/Phylo_div_results/PD_UH_2014_nolumped.csv")

### MPD -------
mpd_uh_2014_NL <- ses.mpd(uh_14_NL_mat, cophenetic(Zorio.tree), 
                          null.model = c("sample.pool"),
                          runs = 999)

write.csv(mpd_uh_2014_NL, "Data/Phylo_div_results/MPD_UH_2014_nolumped.csv")

### MNTD ------
mntd_uh_2014_NL <- ses.mntd(uh_14_NL_mat, cophenetic(Zorio.tree),
                            null.model = c("sample.pool"),
                            runs = 999)

write.csv(mntd_uh_2014_NL, "Data/Phylo_div_results/MNTD_UH_2014_nolumped.csv")

## A ------------------
### PD -------
pd_a_2014_NL <- ses.pd(a_14_NL_mat, Zorio.tree, 
                       null.model = c("sample.pool"),
                       runs = 999, include.root=TRUE)

write.csv(pd_a_2014_NL, "Data/Phylo_div_results/PD_A_2014_nolumped.csv")

### MPD -------
mpd_a_2014_NL <- ses.mpd(a_14_NL_mat, cophenetic(Zorio.tree), 
                         null.model = c("sample.pool"),
                         runs = 999)
write.csv(mpd_a_2014_NL, "Data/Phylo_div_results/MPD_A_2014_nolumped.csv")

### MNTD ------
mntd_a_2014_NL <- ses.mntd(a_14_NL_mat, cophenetic(Zorio.tree), 
                           null.model = c("sample.pool"),
                           runs = 999)

write.csv(mntd_a_2014_NL, "Data/Phylo_div_results/MNTD_A_2014_nolumped.csv")










# 1950 plots (including lumped genera) ----------------------------
## SB --------
### PD ------
pd_sb_1950 <- ses.pd(sb_1950_mat, Zorio.tree, null.model = c("sample.pool"),
                      runs = 999, include.root=TRUE)

write.csv(pd_sb_1950, "Data/Phylo_div_results/PD_SB_1950.csv")

### MPD -------
mpd_sb_1950 <- ses.mpd(sb_1950_mat, cophenetic(Zorio.tree), 
                      null.model = c("sample.pool"),
                     runs = 999)

write.csv(mpd_sb_1950, "Data/Phylo_div_results/MPD_SB_1950.csv")

### MNTD ------
mntd_sb_1950 <- ses.mntd(sb_1950_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                     runs = 999)

write.csv(mntd_sb_1950, "Data/Phylo_div_results/MNTD_SB_1950.csv")

## SF -----------
### PD -------
pd_sf_1950 <- ses.pd(sf_1950_mat, Zorio.tree, 
                     null.model = c("sample.pool"),
                     runs = 999, include.root=TRUE)

write.csv(pd_sf_1950, "Data/Phylo_div_results/PD_SF_1950.csv")

### MPD -------
mpd_sf_1950 <- ses.mpd(sf_1950_mat, cophenetic(Zorio.tree), 
                      null.model = c("sample.pool"),
                      runs = 999)

write.csv(mpd_sf_1950, "Data/Phylo_div_results/MPD_SF_1950.csv")

### MNTD ------
mntd_sf_1950 <- ses.mntd(sf_1950_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                      runs = 999)

write.csv(mntd_sf_1950, "Data/Phylo_div_results/MNTD_SF_1950.csv")

## UH ----------
### PD -------
pd_uh_1950 <- ses.pd(uh_1950_mat, Zorio.tree, 
                     null.model = c("sample.pool"),
                     runs = 999, include.root=TRUE)

write.csv(pd_uh_1950, "Data/Phylo_div_results/PD_UH_1950.csv")

### MPD -------
mpd_uh_1950 <- ses.mpd(uh_1950_mat, cophenetic(Zorio.tree), 
                      null.model = c("sample.pool"),
                      runs = 999)

write.csv(mpd_uh_1950, "Data/Phylo_div_results/MPD_UH_1950.csv")

### MNTD ------
mntd_uh_1950 <- ses.mntd(uh_1950_mat, cophenetic(Zorio.tree),
                       null.model = c("sample.pool"),
                      runs = 999)

write.csv(mntd_uh_1950, "Data/Phylo_div_results/MNTD_UH_1950.csv")

## A ------------------
### PD -------
pd_a_1950 <- ses.pd(a_1950_mat, Zorio.tree, 
                    null.model = c("sample.pool"),
                     runs = 999, include.root=TRUE)

write.csv(pd_a_1950, "Data/Phylo_div_results/PD_A_1950.csv")

### MPD -------
mpd_a_1950 <- ses.mpd(a_1950_mat, cophenetic(Zorio.tree), 
                     null.model = c("sample.pool"),
                      runs = 999)
write.csv(mpd_a_1950, "Data/Phylo_div_results/MPD_A_1950.csv")

### MNTD ------
mntd_a_1950 <- ses.mntd(a_1950_mat, cophenetic(Zorio.tree), 
                      null.model = c("sample.pool"),
                       runs = 999)

write.csv(mntd_a_1950, "Data/Phylo_div_results/MNTD_A_1950.csv")

# 2014 reduced dataset with 14% constancy cutoff -----------------------------

## SB --------
### PD ------
pd_sb_2014_red <- ses.pd(sb_14_reduced_mat, Zorio.tree, null.model = c("sample.pool"),
                     runs = 999, include.root=TRUE)

write.csv(pd_sb_2014_red, "Data/Phylo_div_results/PD_SB_2014_reduced.csv")

### MPD -------
mpd_sb_2014_red <- ses.mpd(sb_14_reduced_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                       runs = 999)

write.csv(mpd_sb_2014_red, "Data/Phylo_div_results/MPD_SB_2014_reduced.csv")

### MNTD ------
mntd_sb_2014_red <- ses.mntd(sb_14_reduced_mat, cophenetic(Zorio.tree), 
                         null.model = c("sample.pool"),
                         runs = 999)

write.csv(mntd_sb_2014_red, "Data/Phylo_div_results/MNTD_SB_2014_reduced.csv")

## SF ------------
### PD ------
pd_sf_2014_red <- ses.pd(sf_14_reduced_mat, Zorio.tree, null.model = c("sample.pool"),
                         runs = 999, include.root=TRUE)

write.csv(pd_sf_2014_red, "Data/Phylo_div_results/PD_SF_2014_reduced.csv")

### MPD -------
mpd_sf_2014_red <- ses.mpd(sf_14_reduced_mat, cophenetic(Zorio.tree), 
                           null.model = c("sample.pool"),
                           runs = 999)

write.csv(mpd_sf_2014_red, "Data/Phylo_div_results/MPD_SF_2014_reduced.csv")

### MNTD ------
mntd_sf_2014_red <- ses.mntd(sf_14_reduced_mat, cophenetic(Zorio.tree), 
                             null.model = c("sample.pool"),
                             runs = 999)

write.csv(mntd_sf_2014_red, "Data/Phylo_div_results/MNTD_SF_2014_reduced.csv")

## UH -----------
### PD ------
pd_uh_2014_red <- ses.pd(uh_14_reduced_mat, Zorio.tree, null.model = c("sample.pool"),
                         runs = 999, include.root=TRUE)

write.csv(pd_uh_2014_red, "Data/Phylo_div_results/PD_UH_2014_reduced.csv")

### MPD -------
mpd_uh_2014_red <- ses.mpd(uh_14_reduced_mat, cophenetic(Zorio.tree), 
                           null.model = c("sample.pool"),
                           runs = 999)

write.csv(mpd_uh_2014_red, "Data/Phylo_div_results/MPD_UH_2014_reduced.csv")

### MNTD ------
mntd_uh_2014_red <- ses.mntd(uh_14_reduced_mat, cophenetic(Zorio.tree), 
                             null.model = c("sample.pool"),
                             runs = 999)

write.csv(mntd_uh_2014_red, "Data/Phylo_div_results/MNTD_UH_2014_reduced.csv")

## A -------------

### PD ------
pd_a_2014_red <- ses.pd(a_14_reduced_mat, Zorio.tree, null.model = c("sample.pool"),
                         runs = 999, include.root=TRUE)

write.csv(pd_a_2014_red, "Data/Phylo_div_results/PD_A_2014_reduced.csv")

### MPD -------
mpd_a_2014_red <- ses.mpd(a_14_reduced_mat, cophenetic(Zorio.tree), 
                           null.model = c("sample.pool"),
                           runs = 999)

write.csv(mpd_a_2014_red, "Data/Phylo_div_results/MPD_A_2014_reduced.csv")

### MNTD ------
mntd_a_2014_red <- ses.mntd(a_14_reduced_mat, cophenetic(Zorio.tree), 
                             null.model = c("sample.pool"),
                             runs = 999)

write.csv(mntd_a_2014_red, "Data/Phylo_div_results/MNTD_A_2014_reduced.csv")



# 2014 plots (not reduced) -----------------------------------------------------------------
## SB --------
### PD ------
pd_sb_2014 <- ses.pd(sb_2014_mat, Zorio.tree, null.model = c("sample.pool"),
                     runs = 999, include.root=TRUE)

write.csv(pd_sb_2014, "Data/Phylo_div_results/PD_SB_2014.csv")

### MPD -------
mpd_sb_2014 <- ses.mpd(sb_2014_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                       runs = 999)
write.csv(mpd_sb_2014, "Data/Phylo_div_results/MPD_SB_2014.csv")

### MNTD ------
mntd_sb_2014 <- ses.mntd(sb_2014_mat, cophenetic(Zorio.tree), 
                         null.model = c("sample.pool"),
                         runs = 999)
write.csv(mntd_sb_2014, "Data/Phylo_div_results/MNTD_SB_2014.csv")

## SF -----------
### PD -------
pd_sf_2014 <- ses.pd(sf_2014_mat, Zorio.tree, 
                     null.model = c("sample.pool"),
                     runs = 999, include.root=TRUE)

write.csv(pd_sf_2014, "Data/Phylo_div_results/PD_SF_2014.csv")

### MPD -------
mpd_sf_2014 <- ses.mpd(sf_2014_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                       runs = 999)

write.csv(mpd_sf_2014, "Data/Phylo_div_results/MPD_SF_2014.csv")

### MNTD ------
mntd_sf_2014 <- ses.mntd(sf_2014_mat, cophenetic(Zorio.tree), 
                         null.model = c("sample.pool"),
                         runs = 999)
write.csv(mntd_sf_2014, "Data/Phylo_div_results/MNTD_SF_2014.csv")

## UH ----------
### PD -------
pd_uh_2014 <- ses.pd(uh_2014_mat, Zorio.tree, 
                     null.model = c("sample.pool"),
                     runs = 999, include.root = TRUE)
write.csv(pd_uh_2014, "Data/Phylo_div_results/PD_UH_2014.csv")

### MPD -------
mpd_uh_2014 <- ses.mpd(uh_2014_mat, cophenetic(Zorio.tree), 
                       null.model = c("sample.pool"),
                       runs = 999)
write.csv(mpd_uh_2014, "Data/Phylo_div_results/MPD_UH_2014.csv")

### MNTD ------
mntd_uh_2014 <- ses.mntd(uh_2014_mat, cophenetic(Zorio.tree),
                         null.model = c("sample.pool"),
                         runs = 999)

write.csv(mntd_uh_2014, "Data/Phylo_div_results/MNTD_UH_2014.csv")

## A ------------------
### PD -------
pd_a_2014 <- ses.pd(a_2014_mat, Zorio.tree, 
                    null.model = c("sample.pool"),
                    runs = 999, include.root=TRUE)

write.csv(pd_a_2014, "Data/Phylo_div_results/PD_A_2014.csv")

### MPD -------
mpd_a_2014 <- ses.mpd(a_2014_mat, cophenetic(Zorio.tree), 
                      null.model = c("sample.pool"),
                      runs = 999)
write.csv(mpd_a_2014, "Data/Phylo_div_results/MPD_A_2014.csv")

### MNTD ------
mntd_a_2014 <- ses.mntd(a_2014_mat, cophenetic(Zorio.tree), 
                        null.model = c("sample.pool"),
                        runs = 999)
write.csv(mntd_a_2014, "Data/Phylo_div_results/MNTD_A_2014.csv")
