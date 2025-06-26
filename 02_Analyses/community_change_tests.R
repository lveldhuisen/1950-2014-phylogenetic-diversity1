library(tidyverse)
library(lsr)

# kruskal wallis test - is change in SES different by community type? ------

## PD ------

# bring in data
pd_paired <- phylo_paired_df %>% filter(metric %in% "PD")

# format data
pd_paired$community <- as.factor(pd_paired$community)
pd_paired$community <- factor(pd_paired$community,
                              levels  = c("sagebrush",
                                          "spruce-fir",
                                          "upland-herb",
                                          "alpine"))
pd_paired$metric <- as.factor(pd_paired$metric)
pd_paired$replicates <- as.factor(pd_paired$replicates)

# test 
kruskal.test(SES_change ~ community, data = pd_paired)
pairwise_result <- pairwise.wilcox.test(pd_paired$SES_change, pd_paired$community, p.adjust.method = "bonferroni")
print(pairwise_result)

# calculate effect size -------

## PD -------

# calculate
cohensD(pd_sb_1950$SES, pd_sb_2014$SES)
cohensD(pd_sf_1950$SES, pd_sf_2014$SES)
cohensD(pd_uh_1950$SES, pd_uh_2014$SES)
cohensD(pd_a_1950$SES, pd_a_2014$SES)
