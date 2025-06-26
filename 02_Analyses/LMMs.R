library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(ggeffects)
library(sjPlot)
library(vegan)
library(stringr)
library(dplyr)
library(merTools)
library(see)
library(emmeans)
library(marginaleffects)
library(jtools)
library(multcomp)
library(lsr)
library(arm)

# linear models WITHOUT lumped genera ------

# bring in data
phylo_paired_df_NL <- read.csv("Data/Phylo_div_results/all_PD_paired_nolumped.csv")
phylo_paired_df_NL$metric <- as.factor(phylo_paired_df_NL$metric)
phylo_paired_df_NL$Aspect <- as.factor(phylo_paired_df_NL$Aspect)

### PD ######

# use only PD
pd_paired_NL <- phylo_paired_df_NL %>% filter(metric %in% "PD")

pd_paired_NL$community <- as.factor(pd_paired_NL$community)
pd_paired_NL$community <- factor(pd_paired_NL$community,
                              levels  = c("sagebrush",
                                          "spruce-fir",
                                          "upland-herb",
                                          "alpine"))
pd_paired_NL$metric <- as.factor(pd_paired_NL$metric)
pd_paired_NL$replicates <- as.factor(pd_paired_NL$replicates)
pd_paired_NL$Aspect <- as.factor(pd_paired_NL$Aspect)
pd_paired_NL$Aspect <- factor(pd_paired_NL$Aspect,
                           levels  = c(
                                       "N",
                                       "NE",
                                       "E",
                                       "SE",
                                       "S",
                                       "SW",
                                       "W",
                                       "NW"))


# set up sum to zero contrast
contrasts(pd_paired_NL$community) <- contr.sum(length(levels(pd_paired_NL$community)))
contrasts(pd_paired_NL$Aspect) <- contr.sum(length(levels(pd_paired_NL$Aspect)))

# model
model_pd_p_NL <- lm(SES_change ~ + community + scale(Elevation_m) + Aspect, data = pd_paired_NL)

# check model diagnostics before you look at summary
check_model(model_pd_p_NL)

### see model summary ####
summary(model_pd_p_NL)
summ(model_pd_p_NL)
anova(model_pd_p_NL)
AIC(model_pd_p_NL)

plot_summs(model_pd_p_NL, scale = TRUE)

# post hoc test 
pd_post_hoc_NL <- glht(model_pd_p_NL, linfct = mcp(community = "Tukey"))
summary(pd_post_hoc_NL)

# save model output 
saveRDS(model_pd_p_NL, file = "ModelOutput/PD_Paired_LM_nolumped.RDS")

# test predictions
prediction_pd_p_NL <- test_predictions(model_pd_p_NL, terms = c("community"))

pd_pred_NL <- predictions(model_pd_p_NL, by = "community")

# save as csv
write_csv(prediction_pd_p_NL, file = "ModelOutput/Prediction_PD_paired_nolumped.csv")

### MPD #####
# use only MPD
mpd_paired_NL <- phylo_paired_df_NL %>% filter(metric %in% "MPD")

mpd_paired_NL$community <- as.factor(mpd_paired_NL$community)
mpd_paired_NL$community <- factor(mpd_paired_NL$community,
                                 levels  = c("sagebrush",
                                             "spruce-fir",
                                             "upland-herb",
                                             "alpine"))
mpd_paired_NL$metric <- as.factor(mpd_paired_NL$metric)
mpd_paired_NL$replicates <- as.factor(mpd_paired_NL$replicates)
mpd_paired_NL$Aspect <- as.factor(mpd_paired_NL$Aspect)
mpd_paired_NL$Aspect <- factor(mpd_paired_NL$Aspect,
                              levels  = c("NW",
                                          "N",
                                          "NE",
                                          "E",
                                          "SE",
                                          "S",
                                          "SW",
                                          "W"))


# set up sum to zero contrast
contrasts(mpd_paired_NL$community) <- contr.sum(length(levels(mpd_paired_NL$community)))
contrasts(mpd_paired_NL$Aspect) <- contr.sum(length(levels(mpd_paired_NL$Aspect)))

# model
model_mpd_p_NL <- lm(SES_change ~ + community + scale(Elevation_m) + Aspect, data = mpd_paired_NL)

# check model diagnostics before you look at summary
check_model(model_mpd_p_NL)

### see model summary ####
summary(model_mpd_p_NL)
summ(model_mpd_p_NL)
anova(model_mpd_p_NL)
AIC(model_mpd_p_NL)

plot_summs(model_mpd_p_NL, scale = TRUE)

# post hoc test 
mpd_post_hoc_NL <- glht(model_mpd_p_NL, linfct = mcp(community = "Tukey"))
summary(mpd_post_hoc_NL)

# save model output 
saveRDS(model_mpd_p_NL, file = "ModelOutput/MPD_Paired_LM_nolumped.RDS")

# test predictions
prediction_mpd_p_NL <- test_predictions(model_mpd_p_NL, terms = c("community"))

mpd_pred_NL <- predictions(model_mpd_p_NL, by = "community")

# save as csv
write_csv(prediction_mpd_p_NL, file = "ModelOutput/Prediction_MPD_paired_nolumped.csv")

### MNTD ######
# use only MPD
mntd_paired_NL <- phylo_paired_df_NL %>% filter(metric %in% "MNTD")

mntd_paired_NL$community <- as.factor(mntd_paired_NL$community)
mntd_paired_NL$community <- factor(mntd_paired_NL$community,
                                  levels  = c("sagebrush",
                                              "spruce-fir",
                                              "upland-herb",
                                              "alpine"))
mntd_paired_NL$metric <- as.factor(mntd_paired_NL$metric)
mntd_paired_NL$replicates <- as.factor(mntd_paired_NL$replicates)
mntd_paired_NL$Aspect <- as.factor(mntd_paired_NL$Aspect)
mntd_paired_NL$Aspect <- factor(mntd_paired_NL$Aspect,
                               levels  = c("NW",
                                           "N",
                                           "NE",
                                           "E",
                                           "SE",
                                           "S",
                                           "SW",
                                           "W"))

# set up sum to zero contrast
contrasts(mntd_paired_NL$community) <- contr.sum(length(levels(mntd_paired_NL$community)))
contrasts(mntd_paired_NL$Aspect) <- contr.sum(length(levels(mntd_paired_NL$Aspect)))

# model
model_mntd_p_NL <- lm(SES_change ~ + community + Elevation_m + Aspect, data = mntd_paired_NL)

# check model diagnostics before you look at summary
check_model(model_mntd_p_NL)

### see model summary ####
summary(model_mntd_p_NL)
summ(model_mntd_p_NL)
anova(model_mntd_p_NL)
AIC(model_mntd_p_NL)

plot_summs(model_mntd_p_NL, scale = TRUE)

# post hoc test 
mntd_post_hoc_NL <- glht(model_mntd_p_NL, linfct = mcp(community = "Tukey"))
summary(mntd_post_hoc_NL)

emmeans(model_mntd_p_NL, pairwise ~ community)

# save model output 
saveRDS(model_mntd_p_NL, file = "ModelOutput/MNTD_Paired_LM_nolumped.RDS")

# test predictions
prediction_mntd_p_NL <- test_predictions(model_mntd_p_NL, terms = c("community"))

mntd_pred_NL <- predictions(model_mntd_p_NL, by = "community")

# save as csv
write_csv(prediction_mntd_p_NL, file = "ModelOutput/Prediction_MNTD_paired_nolumped.csv")

# linear models including lumped genera  ---------------

## paired 2014-1950 SES change as response #####

# bring in data
phylo_paired_df <- read.csv("Data/Phylo_div_results/all_PD_paired.csv")
phylo_paired_df$metric <- as.factor(phylo_paired_df$metric)
phylo_paired_df$Aspect <- as.factor(phylo_paired_df$Aspect)

### PD ######

# use only PD
pd_paired <- phylo_paired_df %>% filter(metric %in% "PD")


pd_paired$community <- factor(pd_paired$community,
                              levels  = c("sagebrush",
                                          "spruce-fir",
                                          "upland-herb",
                                          "alpine"))
pd_paired$community <- as.factor(pd_paired$community)
pd_paired$metric <- as.factor(pd_paired$metric)
pd_paired$replicates <- as.factor(pd_paired$replicates)
pd_paired$Aspect <- as.factor(pd_paired$Aspect)
pd_paired$Aspect <- factor(pd_paired$Aspect,
                              levels  = c("NW",
                                          "N",
                                          "NE",
                                          "E",
                                          "SE",
                                          "S",
                                          "SW",
                                          "W"))


# set up sum to zero contrast for community and aspect 
contrasts(pd_paired$community) <- contr.sum(length(levels(pd_paired$community)))
contrasts(pd_paired$Aspect) <- contr.sum(length(levels(pd_paired$Aspect)))

# model
model_pd_p <- lm(SES_change ~ + community + scale(Elevation_m) + Aspect, data = pd_paired)
model_pd_p2 <- lm(SES_change ~ + community + Elevation_m, data = pd_paired)

# check model diagnostics before you look at summary
check_model(model_pd_p)
compare_performance(model_pd_p, model_pd_p2, rank= TRUE)

### see model summary ####
summary(model_pd_p)
summ(model_pd_p)
anova(model_pd_p)
AIC(model_pd_p)

plot_summs(model_pd_p, scale = TRUE)

# post hoc test 
pd_post_hoc <- glht(model_pd_p, linfct = mcp(community = "Tukey"))
summary(pd_post_hoc)

emmeans(model_pd_p, pairwise ~ community)

# save model output 
saveRDS(model_pd_p, file = "ModelOutput/PD_Paired_LM.RDS")

# test predictions
prediction_pd_p <- test_predictions(model_pd_p, terms = c("community"))
summary(prediction_pd_p)

pd_pred <- predictions(model_pd_p, by = "community")
summary(pd_pred)

# save as csv
write_csv(prediction_pd_p, file = "ModelOutput/Prediction_PD_paired.csv")

## MPD ---------------

# use only MPD
mpd_paired <- phylo_paired_df %>% filter(metric %in% "MPD")
mpd_paired$community <- as.factor(mpd_paired$community)
mpd_paired$community <- factor(mpd_paired$community,
                              levels  = c("sagebrush",
                                          "spruce-fir",
                                          "upland-herb",
                                          "alpine"))
mpd_paired$replicates <- as.factor(mpd_paired$replicates)
mpd_paired$Aspect <- as.factor(mpd_paired$Aspect)
mpd_paired$Aspect <- factor(mpd_paired$Aspect,
                           levels  = c(
                                       "N",
                                       "NE",
                                       "E",
                                       "SE",
                                       "S",
                                       "SW",
                                       "W",
                                       "NW"))

# set up sum to zero contrast
contrasts(mpd_paired$community) <- contr.sum(length(levels(mpd_paired$community)))
contrasts(mpd_paired$Aspect) <- contr.sum(length(levels(mpd_paired$Aspect)))

# model
model_mpd_p <- lm(SES_change ~ community + Aspect + scale(Elevation_m), data = mpd_paired)

# check model diagnostics before you look at summary
check_model(model_mpd_p)

### see model summary ####
summary(model_mpd_p)
summ(model_mpd_p)
anova(model_mpd_p)
AIC(model_mpd_p)

plot_summs(model_mpd_p)

# post hoc test
mpd_post_hoc <- glht(model_mpd_p, linfct = mcp(community = "Tukey"))
summary(mpd_post_hoc)

# save model output 
saveRDS(model_mpd_p, file = "ModelOutput/MPD_Paired_LM.RDS")

# test predictions
prediction_mpd_p <- test_predictions(model_mpd_p, terms = c("community"))
summary(prediction_mpd_p)

mpd_pred <- predictions(model_mpd_p, by = "community")

# save as csv
write_csv(prediction_mpd_p, file = "ModelOutput/Prediction_MPD_paired.csv")

## MNTD ----------

# use only MNTD
mntd_paired <- phylo_paired_df %>% filter(metric %in% "MNTD")
mntd_paired$community <- as.factor(mntd_paired$community)
mntd_paired$community <- factor(mntd_paired$community,
                               levels  = c("sagebrush",
                                           "spruce-fir",
                                           "upland-herb",
                                           "alpine"))
mntd_paired$replicates <- as.factor(mntd_paired$replicates)
mntd_paired$Aspect <- as.factor(mntd_paired$Aspect)
mntd_paired$Aspect <- factor(mntd_paired$Aspect,
                            levels  = c("W",
                                        "NW",
                                        "N",
                                        "NE",
                                        "E",
                                        "SE",
                                        "S",
                                        "SW"
                                        ))



# set up sum to zero contrast
contrasts(mntd_paired$community) <- contr.sum(length(levels(mntd_paired$community)))
contrasts(mntd_paired$Aspect) <- contr.sum(length(levels(mntd_paired$Aspect)))

# model
model_mntd_p <- lm(SES_change ~ community + Aspect + scale(Elevation_m), 
                   data = mntd_paired)

# check model diagnostics before you look at summary
check_model(model_mntd_p)

### see model summary ####
summary(model_mntd_p)
summ(model_mntd_p)
ranova(model_mntd_p)
anova(model_mntd_p)
AIC(model_mntd_p)

plot_summs(model_mntd_p)

# save model output 
saveRDS(model_mntd_p, file = "ModelOutput/MNTD_Paired_LM.RDS")

# post hoc test
mntd_post_hoc <- glht(model_mntd_p, linfct = mcp(community = "Tukey"))
summary(mntd_post_hoc)

# test predictions
prediction_mntd_p <- test_predictions(model_mntd_p, terms = c("community"))

mntd_pred <- predictions(model_mntd_p, by = "community")

# save as csv
write_csv(prediction_mntd_p, file = "ModelOutput/Prediction_MNTD_paired.csv")

# linear mixed models: raw SES values with time as fixed effect -----------------

wilcox.test(SES ~ year, data = sb_df)
wilcox.test(SES ~ year, data = sf_df)
wilcox.test(SES ~ year, data = uh_df)
wilcox.test(SES ~ year, data = a_df) # significant differences for sagebrush and spruce fir for all metrics combined

# bring in data
phylo_df <- read.csv("Data/Phylo_div_results/all_phylo_div_results.csv")

### PD --------------------------
# use only PD
pd_df <- phylo_df %>% filter(metric %in% "PD")

pd_df$year <- as.factor(pd_df$year)
pd_df$community <- as.factor(pd_df$community)
pd_df$metric <- as.factor(pd_df$metric)
pd_df$replicates <- as.factor(pd_df$replicates)

# set up sum to zero contrast
contrasts(pd_df$community) <- contr.sum(length(levels(pd_df$community)))

# make 1950 baseline
pd_df$year <- relevel(factor(pd_df$year),
                      ref = "1950")

### models #####
model_pd1 <- lmer(SES ~ year + community + year*community + (1|replicates), data = pd_df)
model_pd2 <- lmer(SES ~ year + community + (1|replicates), data = pd_df)

# compare models
compare_performance(model_pd1,model_pd2, rank = T)

# check model diagnostics before you look at summary
check_model(model_pd1)
plot(resid(model_pd), pd_df$SES)
qqmath(model_pd1)

### see model summary ####
summary(model_pd1)
ranova(model_pd1)
Anova(model_pd1)
tab_model(model_pd1)
AIC(model_pd1)

#save model output 
saveRDS(model_pd1, file = "ModelOutput/PD_LMM.RDS")

#test predictions
prediction_pd <- test_predictions(model_pd1, terms = c("year","community"))

#save as csv
write_csv(prediction_pd, file = "ModelOutput/Prediction_PD.csv")

### MPD --------------------------
# use only MPD
mpd_df <- phylo_df %>% filter(metric %in% "MPD")

mpd_df$year <- as.factor(mpd_df$year)
mpd_df$community <- as.factor(mpd_df$community)
mpd_df$replicates <- as.factor(mpd_df$replicates)

# set up sum to zero contrast
contrasts(mpd_df$community) <- contr.sum(length(levels(mpd_df$community)))

### models #####
model_mpd1 <- lmer(SES ~ year + community + year*community + (1|replicates, data = mpd_df)
model_mpd2 <- lmer(SES ~ year + community + (1|replicates), data = mpd_df)

# check model diagnostics before you look at summary
check_model(model_mpd1)
qqmath(model_mpd1)

# compare models
compare_performance(model_mpd1,model_mpd2, rank = T)

### see model summary ####
summary(model_mpd1)
ranova(model_mpd1)
Anova(model_mpd1)
tab_model(model_mpd1)
AIC(model_mpd1)

# save model output 
saveRDS(model_mpd1, file = "ModelOutput/MPD_LMM.RDS")

# test predictions
prediction_mpd <- test_predictions(model_mpd1, terms = c("year","community"))

# save as csv
write_csv(prediction_mpd, file = "ModelOutput/Prediction_MPD.csv")

### MNTD --------

# use only MNTD
mntd_df <- phylo_df %>% filter(metric %in% "MNTD")

mntd_df$year <- as.factor(mntd_df$year)
mntd_df$community <- as.factor(mntd_df$community)
mntd_df$replicates <- as.factor(mntd_df$replicates)

# set up sum to zero contrast
contrasts(mntd_df$community) <- contr.sum(length(levels(mntd_df$community)))


### models #####
model_mntd1 <- lmer(SES ~ year + community + year*community + (1|replicates), data = mntd_df)
model_mntd2 <- lmer(SES ~ year + community + (1|replicates), data = mntd_df)

# check model diagnostics before you look at summary
check_model(model_mntd1)
qqmath(model_mntd2)

# compare models
compare_performance(model_mntd1,model_mntd2, rank = T)

### see model summary ####
summary(model_mntd1)
ranova(model_mntd1)
Anova(model_mntd1)
tab_model(model_mntd1)
AIC(model_mntd1)

# save model output 
saveRDS(model_mntd1, file = "ModelOutput/MNTD_LMM.RDS")

# test predictions
prediction_mntd <- test_predictions(model_mntd1, terms = c("year","community"))

# save as csv
write_csv(prediction_mntd, file = "ModelOutput/Prediction_MNTD.csv")

