devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")

# bray-curtis distance matrix
woody.dist <- vegdist(mts.woody, method = "bray")
ferns.dist <- vegdist(mts.ferns, method = "bray")

# PERMANOVA via adonis2
perm <- how(nperm = 9999)
woody.perm <- adonis2(woody.dist ~ site, permutations = perm)
ferns.perm <- adonis2(ferns.dist ~ site, permutations = perm)

woody.df <- data.frame(site = site, mts.woody)
woody.perm.pair <- pairwiseAdonis::pairwise.adonis2(mts.woody ~ site, nperm = 9999, data = woody.df)

ferns.df <- data.frame(site = site, mts.ferns)
ferns.perm.pair <- pairwiseAdonis::pairwise.adonis2(mts.ferns ~ site, nperm = 9999, data = ferns.df)


# HOMOGENEITY OF MV VARIANCE
woody.mod <- betadisper(woody.dist, site)

## Perform test
woody.aov <- anova(woody.mod)
## Permutation test for F
woody.pairs <- permutest(woody.mod, pairwise = TRUE, permutations = perm)


# HOMOGENEITY OF MV VARIANCE
ferns.mod <- betadisper(ferns.dist, site)

## Perform test
ferns.aov <- anova(ferns.mod)
## Permutation test for F
ferns.pairs <- permutest(ferns.mod, pairwise = TRUE, permutations = perm)



