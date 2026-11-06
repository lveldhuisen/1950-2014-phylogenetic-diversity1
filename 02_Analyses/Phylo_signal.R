library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(plyr)
library(picante)
library(geiger)
library(ape)
library(phytools)

#Elevation shift signal-----

## all communties combined ####
### Blomberg's K (calculated with picante) ####

phylosignal(shift_vec, pruned.tree, reps = 5000, checkdata = TRUE) 

# plot phylogeny
phylo_shift <- contMap(pruned.tree, shift_vec, res=100, plot=FALSE)
contMap_shift <- setMap(phylo_shift, viridisLite::viridis(n=8))
plot(contMap_shift)

### Pagels lambda #####
phylosig(pruned.tree, shift_vec, method="lambda", test=TRUE, nsim=5000,
        se=NULL, start=NULL, control=list(), niter=10)

## all communties combined, removed lumped genera ####

### Blomberg's K (calculated with picante) ####

phylosignal(shift_vec_removed, pruned.tree.removed, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(pruned.tree.removed, shift_vec_removed, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## SB #####
### Blomberg's K (calculated with picante) ####
phylosignal(sb_shift_vec, pruned.tree.sagebrush, reps = 5000, checkdata = TRUE) 

# plot phylogeny
sb_phylo <- contMap(pruned.tree.sagebrush, sb_shift_vec, res=600, plot=FALSE)
sb_phylo <- setMap(sb_phylo, viridisLite::viridis(n=8))
plot(sb_phylo)

### Pagels lambda #####
phylosig(pruned.tree.sagebrush, sb_shift_vec, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## SB with lumped genera removed ------

### Blomberg's K (calculated with picante) ####
phylosignal(sb_shift_vec_removed, pruned.tree.sagebrush.rm, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(pruned.tree.sagebrush.rm, sb_shift_vec_removed, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## SF #######
### Blomberg's K (calculated with picante) ####
phylosignal(sf_shift_vec, pruned.tree.sprucefir, reps = 5000, checkdata = TRUE) 

# plot phylogeny
sf_phylo <- contMap(pruned.tree.sprucefir, sf_shift_vec, res=600, plot=FALSE)
sf_phylo <- setMap(sf_phylo, viridisLite::viridis(n=8))
plot(sf_phylo)

### Pagels lambda #####
phylosig(pruned.tree.sprucefir, sf_shift_vec, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## upland herb #######
### Blomberg's K (calculated with picante) ####
phylosignal(uh_shift_vec, pruned.tree.uplandherb, reps = 5000, checkdata = TRUE) 

# plot phylogeny
uh_phylo <- contMap(pruned.tree.uplandherb, uh_shift_vec, res=600, plot=FALSE)
uh_phylo <- setMap(uh_phylo, viridisLite::viridis(n=8))
plot(uh_phylo)

### Pagels lambda #####
phylosig(pruned.tree.uplandherb, uh_shift_vec, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## alpine #######
### Blomberg's K (calculated with picante) ####
phylosignal(a_shift_vec, pruned.tree.alpine, reps = 5000, checkdata = TRUE) 

# plot phylogeny
a_phylo <- contMap(pruned.tree.alpine, a_shift_vec, res=600, plot=FALSE)
a_phylo <- setMap(a_phylo, viridisLite::viridis(n=8))
plot(a_phylo)

### Pagels lambda #####
phylosig(pruned.tree.alpine, a_shift_vec, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

# Abundance shift signal --------
## sagebrush #####

### Blomberg's K (calculated with picante) ####
phylosignal(sagebrush_ab_vec, tree.sb.ab, reps = 5000, checkdata = TRUE) 

# plot phylogeny
sb_ab_phylo <- contMap(tree.sb.ab, sagebrush_ab_vec, res=600, plot=FALSE)
sb_ab_phylo <- setMap(sb_ab_phylo, viridisLite::viridis(n=8))
plot(sb_ab_phylo)

### Pagels lambda #####
phylosig(tree.sb.ab, sagebrush_ab_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## sagebrush with lumped genera removed ------------

#### Blomberg's K (calculated with picante) ####
phylosignal(sagebrush_ab_vec_removed, tree.sb.ab.rm, reps = 5000, checkdata = TRUE) 

#### Pagels lambda #####
phylosig(tree.sb.ab.rm, sagebrush_ab_vec_removed, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)


## spruce-fir #####
### Blomberg's K (calculated with picante) ####
phylosignal(spruce_ab_vec, tree.spruce.ab, reps = 5000, checkdata = TRUE) 

# plot phylogeny
spruce_ab_phylo <- contMap(tree.spruce.ab, spruce_ab_vec, res=600, plot=FALSE)
spruce_ab_phylo <- setMap(spruce_ab_phylo, viridisLite::viridis(n=8))
plot(spruce_ab_phylo)

### Pagels lambda #####
phylosig(tree.spruce.ab, spruce_ab_vec, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## spruce with lumped genera removed ####
### Blomberg's K (calculated with picante) ####
phylosignal(spruce_ab_vec_rm, tree.spruce.ab.rm, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(tree.spruce.ab.rm, spruce_ab_vec_rm, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## upland herb #####
### Blomberg's K (calculated with picante) ####
phylosignal(uh_ab_vec, tree.uh.ab, reps = 5000, checkdata = TRUE) 

#plot phylogeny
uh_ab_phylo <- contMap(tree.uh.ab, uh_ab_vec, res=600, plot=FALSE)
uh_ab_phylo <- setMap(uh_ab_phylo, viridisLite::viridis(n=8))
plot(uh_ab_phylo)

### Pagels lambda #####
phylosig(tree.uh.ab, uh_ab_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## upland herb with lumped genera removed #####

### Blomberg's K (calculated with picante) ####
phylosignal(uh_ab_vec_remove, tree.uh.ab.rm, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(tree.uh.ab.rm, uh_ab_vec_remove, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## alpine ####
### Blomberg's K (calculated with picante) ####
phylosignal(a_ab_vec, tree.a.ab,checkdata = TRUE) 

# plot phylogeny
a_ab_phylo <- contMap(tree.a.ab, a_ab_vec, res=600, plot=FALSE)
a_ab_phylo <- setMap(a_ab_phylo, viridisLite::viridis(n=8))
plot(a_ab_phylo)

### Pagels lambda #####
phylosig(tree.a.ab, a_ab_vec, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## alpine with lumped genera removed #####
### Blomberg's K (calculated with picante) ####
phylosignal(a_ab_vec_removed, tree.a.ab.rm, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(tree.a.ab.rm, a_ab_vec_removed, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

# Constancy shift signal ------------

## sagebrush #####
### Blomberg's K (calculated with picante) ####
phylosignal(sagebrush_co_sig, tree.sb.ab, reps = 5000, checkdata = TRUE) 

# plot phylogeny
sb_co_phylo <- contMap(tree.sb.ab, sagebrush_co_sig, res=600, plot=FALSE)
sb_co_phylo <- setMap(sb_co_phylo, viridisLite::viridis(n=8))
plot(sb_co_phylo)

### Pagels lambda #####
phylosig(tree.sb.ab, sagebrush_co_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## sagebrush with lumped genera removed ######
### Blomberg's K (calculated with picante) ####
phylosignal(sagebrush_co_vec_rm, tree.sb.ab.rm, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(tree.sb.ab.rm, sagebrush_co_vec_rm, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)


## spruce-fir #####
### Blomberg's K (calculated with picante) ####
phylosignal(spruce_co_sig, tree.spruce.ab, reps = 5000, checkdata = TRUE) 

# plot phylogeny
spruce_co_phylo <- contMap(tree.spruce.ab, spruce_co_sig, res=600, plot=FALSE)
spruce_co_phylo <- setMap(spruce_co_phylo, viridisLite::viridis(n=8))
plot(spruce_co_phylo)

### Pagels lambda #####
phylosig(tree.spruce.ab, spruce_co_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## spruce-fir lumped genera removed #######

### Blomberg's K (calculated with picante) ####
phylosignal(spruce_co_vec_rm, tree.spruce.ab.rm, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(tree.spruce.ab.rm, spruce_co_vec_rm, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)



## upland herb #####
### Blomberg's K (calculated with picante) ####
phylosignal(uh_co_sig, tree.uh.ab, reps = 5000, checkdata = TRUE) 

# plot phylogeny
uh_co_phylo <- contMap(tree.uh.ab, uh_co_sig, res=600, plot=FALSE)
uh_co_phylo <- setMap(uh_co_phylo, viridisLite::viridis(n=8))
plot(uh_co_phylo)

### Pagels lambda #####
phylosig(tree.uh.ab, uh_co_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## upland herb lumped genera removed ##########
### Blomberg's K (calculated with picante) ####
phylosignal(uh_co_vec_rm, tree.uh.ab.rm, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(tree.uh.ab.rm, uh_co_vec_rm, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)


## alpine ####
### Blomberg's K (calculated with picante) ####
phylosignal(a_co_vec, tree.a.ab, reps = 5000, checkdata = TRUE) 

# plot phylogeny
a_co_phylo <- contMap(tree.a.ab, a_co_vec, res=600, plot=FALSE)
a_co_phylo <- setMap(a_co_phylo, viridisLite::viridis(n=8))
plot(a_co_phylo)

### Pagels lambda #####
phylosig(tree.a.ab, a_co_sig, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

## alpine lumped genera removed ###########
### Blomberg's K (calculated with picante) ####
phylosignal(a_co_vec_rm, tree.a.ab.rm, reps = 5000, checkdata = TRUE) 

### Pagels lambda #####
phylosig(tree.a.ab.rm, a_co_vec_rm, method="lambda", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10)

