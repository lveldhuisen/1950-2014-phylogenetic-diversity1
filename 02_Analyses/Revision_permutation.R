## SES of the differences (change in PD / MPD / MNTD, 1950 -> 2014): SAGEBRUSH PLOTS, FULL SPECIES POOL
## Null: the SAME species-label shuffle is applied to both time points in each iteration.

library(picante)
library(ape)

# SAGEBRUSH ##########
## ============================ 1. SETTINGS ====================================
n_iter    <- 999
abundance <- FALSE   # set TRUE if your original MPD/MNTD were abundance-weighted
# (the consistency check in section 4 tells you which one matches)
set.seed(42)

## Your tree. Load it here if it is not already in the session, e.g.:
tree <- Zorio.tree
stopifnot(exists("tree"))

## Observed deltas (long format: one row per plot x metric), sagebrush only
raw <- read.csv("Data/Phylo_div_results/Raw_paired_forpermutation.csv", stringsAsFactors = FALSE)
raw <- raw[raw$community == "sagebrush", ]

## ============================ 2. LOAD COMMUNITIES ============================
ids <- raw$replicates[raw$metric == "PD"]       # SB1 ... SB27, in file order

read_comm <- function(file, new_ids) {
  x <- read.csv(file, row.names = 1, check.names = FALSE)
  x <- x[!grepl("^all", rownames(x), ignore.case = TRUE), , drop = FALSE]  # drop ALL_SB pooled row
  if (nrow(x) != length(new_ids))
    stop(file, ": has ", nrow(x), " plot rows but the delta CSV lists ",
         length(new_ids), " sagebrush plots.")
  rownames(x) <- new_ids
  colnames(x) <- gsub(" ", "_", colnames(x))
  x[is.na(x)] <- 0
  x
}

sb_A <- read_comm("Data/Matrices/Matrices_for_permutation/sagebrush_1950_plots_clean.csv", ids)
sb_B <- read_comm("Data/Matrices/Matrices_for_permutation/sagebrush_2014_plots_clean.csv", ids)

## ============================ 3. COMMON SPECIES POOL =========================
## Pool = full species list across all four sites and both years (full_species_pool.csv).
## The null shuffles species labels across this whole pool, even though only
## sagebrush plots are analyzed here.
pool_csv <- read.csv("Data/Species_lists/full_species_pool.csv", stringsAsFactors = FALSE)
full_pool <- unique(gsub(" ", "_", trimws(pool_csv$pool_list)))

## Every species in the sagebrush plots must be in the pool and in the tree.
sb_spp <- union(colnames(sb_A), colnames(sb_B))
not_in_pool <- setdiff(sb_spp, full_pool)
if (length(not_in_pool))
  stop("Sagebrush species not in full_species_pool.csv:\n  ",
       paste(not_in_pool, collapse = "\n  "))
sb_not_in_tree <- setdiff(sb_spp, tree$tip.label)
if (length(sb_not_in_tree))
  stop("Sagebrush species not in tree:\n  ",
       paste(sb_not_in_tree, collapse = "\n  "))

## Pool species missing from the tree can't be shuffled; report them and continue without them.
pool_not_in_tree <- setdiff(full_pool, tree$tip.label)
if (length(pool_not_in_tree))
  warning(length(pool_not_in_tree), " pool species are not in the tree and are excluded from the null:\n  ",
          paste(pool_not_in_tree, collapse = "\n  "))

tree <- drop.tip(tree, setdiff(tree$tip.label, full_pool))   # prune tree to the full pool
pool <- tree$tip.label
cat("Species pool used for the null:", length(pool), "species\n")
d    <- as.matrix(cophenetic(tree))

fill_to_pool <- function(x, pool) {
  miss <- setdiff(pool, colnames(x))
  if (length(miss)) x[miss] <- 0
  m <- as.matrix(x[, pool])
  rownames(m) <- rownames(x)
  m
}
comm_A <- fill_to_pool(sb_A, pool)
comm_B <- fill_to_pool(sb_B, pool)
stopifnot(identical(rownames(comm_A), rownames(comm_B)))

## ============================ 4. OBSERVED DELTAS + CONSISTENCY CHECK =========
obs <- lapply(split(raw, raw$metric), function(x) setNames(x$Raw_change, x$replicates))
stopifnot(all(unlist(lapply(obs, names)) %in% rownames(comm_A)))

## Recompute the observed values exactly the way you originally did
## (ses.* with null.model = "sample.pool", no abundance weighting) and confirm that
## the 1950 -> 2014 change reproduces Raw_change in your CSV.
## runs = 2 because only the observed-value column is used here, not the SES.
get_obs <- list(
  PD   = function(cm) ses.pd(cm, tree, null.model = "sample.pool", runs = 2)$pd.obs,
  MPD  = function(cm) ses.mpd(cm, d, null.model = "sample.pool",
                              abundance.weighted = FALSE, runs = 2)$mpd.obs,
  MNTD = function(cm) ses.mntd(cm, d, null.model = "sample.pool",
                               abundance.weighted = FALSE, runs = 2)$mntd.obs
)

checks <- sapply(names(get_obs), function(m) {
  p <- names(obs[[m]])
  recomputed <- get_obs[[m]](comm_B[p, , drop = FALSE]) - get_obs[[m]](comm_A[p, , drop = FALSE])
  max(abs(unname(recomputed) - unname(obs[[m]])))
})
cat("\nConsistency check: max |recomputed change - Raw_change| per metric\n")
print(signif(checks, 3))
cat("\n")

if (any(is.na(checks) | checks > 1e-6))
  stop("Recomputed changes do not match Raw_change for: ",
       paste(names(checks)[is.na(checks) | checks > 1e-6], collapse = ", "),
       ".\nCheck the tree version/name matching, the species columns in the community ",
       "matrices, or (for PD) the include.root setting before running the null.")

## ============================ 5. NULL DISTRIBUTION OF CHANGES ================
shuffle_dist <- function(d) {
  idx <- sample(nrow(d)); d2 <- d[idx, idx]; dimnames(d2) <- dimnames(d); d2
}

null <- list()

for (m in c("MPD", "MNTD")) {
  f <- if (m == "MPD") mpd else mntd
  plots <- names(obs[[m]])
  A <- comm_A[plots, , drop = FALSE]; B <- comm_B[plots, , drop = FALSE]
  out <- matrix(NA_real_, n_iter, length(plots), dimnames = list(NULL, plots))
  for (i in seq_len(n_iter)) {
    dn <- shuffle_dist(d)                                  # ONE shuffle, used for both times
    out[i, ] <- f(B, dn, abundance.weighted = abundance) -
      f(A, dn, abundance.weighted = abundance)
  }
  null[[m]] <- out
}

plots <- names(obs$PD)
A <- comm_A[plots, , drop = FALSE]; B <- comm_B[plots, , drop = FALSE]
null$PD <- t(replicate(n_iter, {
  tr <- tree; tr$tip.label <- sample(tr$tip.label)         # same shuffled tree for both times
  setNames(pd(B, tr)$PD - pd(A, tr)$PD, plots)
}))

## ============================ 6. SES OF THE DIFFERENCE =======================
summarize <- function(obs_delta, null_delta) {
  o <- mean(obs_delta, na.rm = TRUE)                       # mean change across plots
  n <- rowMeans(null_delta, na.rm = TRUE)                  # mean null change per iteration
  data.frame(n_plots = length(obs_delta),
             obs_delta = o, null_mean = mean(n), null_sd = sd(n),
             SES = (o - mean(n)) / sd(n),
             p_two_tailed = (sum(abs(n - mean(n)) >= abs(o - mean(n))) + 1) / (n_iter + 1))
}

results <- do.call(rbind, lapply(c("PD", "MPD", "MNTD"), function(m)
  cbind(metric = m, summarize(obs[[m]], null[[m]]))))
results$p_holm <- p.adjust(results$p_two_tailed, method = "holm")   # 3 tests

print(results, digits = 3)
write.csv(results, "Data/Revision_permutation/SES_of_differences_sagebrush.csv", row.names = FALSE)
