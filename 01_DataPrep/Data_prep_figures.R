library(tidyverse)



# pair plot data and use change in SES value for linear model ----------

# with lumped
pd_pred$Metric <- "PD"
mpd_pred$Metric <- "MPD"
mntd_pred$Metric <- "MNTD"

all_pred <- rbind(pd_pred, mpd_pred, mntd_pred)

# order metrics to match
all_pred$Metric <- factor(all_pred$Metric,
                              levels  = c("PD",
                                          "MPD",
                                          "MNTD"))

# no lumped
pd_pred_NL$metric <- "PD"
mpd_pred_NL$metric <- "MPD"
mntd_pred_NL$metric <- "MNTD"

all_pred_NL <- rbind(pd_pred_NL, mpd_pred_NL, mntd_pred_NL)

all_pred_NL$metric <- factor(all_pred_NL$metric,
                          levels  = c("PD",
                                      "MPD",
                                      "MNTD"))



# Community change over time ------------------------------------------------

## PD ####
# bring in data
pred_PD <- read.csv("ModelOutput/Prediction_PD.csv")

# combine columns
pred_PD$comparison <- paste(pred_PD$Level1,"_",pred_PD$Level2)

# keep only comparisons for each community across years
keep <- c("2014, alpine _ 1950, alpine", "2014, sagebrush _ 1950, sagebrush",
          "2014, spruce-fir _ 1950, spruce-fir","2014, upland-herb _ 1950, upland-herb")

pred_PD <- pred_PD %>% filter(comparison %in% keep)

# split to make community type column
pred_PD <- separate_wider_delim(pred_PD, cols = Level1, delim = ",", 
                               names = c("year1", "community"))

# reorder communities
pred_PD$community <- as.factor(pred_PD$community)
pred_PD$community <- factor(pred_PD$community,
                            levels  = c(" sagebrush",
                                        " spruce-fir",
                                        " upland-herb",
                                        " alpine"))

pred_PD <- pred_PD[order(levels(pred_PD$community)),]



## MPD #####

# bring in data
pred_MPD <- read.csv("ModelOutput/Prediction_MPD.csv")

# combine columns
pred_MPD$comparison <- paste(pred_MPD$Level1,"_",pred_MPD$Level2)

# keep only comparisons for each community across years
pred_MPD <- pred_MPD %>% filter(comparison %in% keep)

# split to make community type column
pred_MPD <- separate_wider_delim(pred_MPD, cols = Level1, delim = ",", 
                                names = c("year1", "community"))

# reorder communities
pred_MPD$community <- as.factor(pred_MPD$community)
pred_MPD$community <- factor(pred_MPD$community,
                            levels  = c(" sagebrush",
                                        " spruce-fir",
                                        " upland-herb",
                                        " alpine"))

pred_MPD <- pred_MPD[order(levels(pred_MPD$community)),]

## MNTD ####

# bring in data
pred_MNTD <- read.csv("ModelOutput/Prediction_MNTD.csv")

# combine columns
pred_MNTD$comparison <- paste(pred_MNTD$Level1,"_",pred_MNTD$Level2)

# keep only comparisons for each community across years
pred_MNTD <- pred_MNTD %>% filter(comparison %in% keep)

# split to make community type column
pred_MNTD <- separate_wider_delim(pred_MNTD, cols = Level1, delim = ",", 
                                 names = c("year1", "community"))

# reorder communities
pred_MNTD$community <- as.factor(pred_MNTD$community)
pred_MNTD$community <- factor(pred_MNTD$community,
                             levels  = c(" sagebrush",
                                         " spruce-fir",
                                         " upland-herb",
                                         " alpine"))

pred_MNTD <- pred_MNTD[order(levels(pred_MNTD$community)),]

