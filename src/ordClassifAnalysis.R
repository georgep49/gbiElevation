library(vegan)
library(tidyverse)
library(broom)
library(patchwork)
library(ggrepel)
library(ggdendro)
library(dendextend)

source("./src/helperPlots.r")


## Load the data
mts.woody <- read.csv("./data/woodyAltitude_final.csv", header = T, row.names = 1)
mts.ferns <- read.csv("./data/fernAltitude_final.csv", header = T, row.names = 1)

# Extract the site codes and elevations 
site <- str_sub(names(mts.woody), 1, 1)
elev <- as.numeric(str_sub(names(mts.woody), 3, 5))

# Transpose and convert NAs to zero for vegan
mts.woody <- t(mts.woody)
mts.woody[is.na(mts.woody)] <- 0

mts.ferns <- t(mts.ferns)
mts.ferns[is.na(mts.ferns)] <- 0


all.data <- data.frame(elev = elev, site = site, woody = rowSums(mts.woody), ferns = rowSums(mts.ferns))

# Regression of ruichness vs elevation
source("./src/regrBuilder.r")  # assumes MuMIn installed

# Into plotting of the data
woody.plot <- filter(all.data.long, taxa == "woody")

elev.s.woody <- ggplot(woody.plot) +
  geom_point(aes(x = elev, y = richness, col= site, shape = site), size = 2.5) +
  geom_smooth(aes(x = elev, y = richness), method="glm", method.args=list(family="poisson")) +
  scale_colour_brewer(type = "qual", name = "Site", labels = c("Hirikimata", "Ruahine", "Tataweka")) +
  scale_fill_brewer(type = "qual", name = "Site", labels = c("Hirikimata", "Ruahine", "Tataweka")) +
  scale_shape_manual(values = c(16,17, 15), name = "Site", labels = c("Hirikimata", "Ruahine", "Tataweka")) +  
  labs(x = "Altitude (m)", y = "Species richness") +
  ylim(0,70) +
  theme_bw()

ferns.plot <- filter(all.data.long, taxa == "ferns")
elev.s.ferns <- ggplot(ferns.plot) +
  geom_point(aes(x = elev, y = richness, col= site, shape = site), size = 2.5) +
  geom_smooth(aes(x = elev, y = richness), method="glm", method.args=list(family="poisson")) +
  scale_colour_brewer(type = "qual", name = "Site", labels = c("Hirikimata", "Ruahine", "Tataweka")) +
  scale_fill_brewer(type = "qual", name = "Site", labels = c("Hirikimata", "Ruahine", "Tataweka")) +
  scale_shape_manual(values = c(16,17, 15), name = "Site", labels = c("Hirikimata", "Ruahine", "Tataweka")) +  
  labs(x = "Altitude (m)") +
  ylim(0,70) +
  theme_bw() +
  theme(axis.title.y = element_blank())

elev.plots <- elev.s.woody + elev.s.ferns +
  plot_annotation(tag_levels = "a") + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# One of the refs asked about proportional change in richness with elevation for ferns vs woody
prop.change <- all.data %>% 
  group_by(site) %>%
  filter(elev == 150 | elev == 400) %>%
  mutate(delta_w = lead(woody) / woody, 
         delta_D = lead(ferns) / ferns) %>%
  ungroup()
    

## Into the ordination and classification analysis

# Bray-curtis distance matrix
woody.dist <- vegdist(mts.woody, method = "bray")
ferns.dist <- vegdist(mts.ferns, method = "bray")


# Beta diversity for each site (mean Sorensen index)
hira.woody <- mts.woody[substr(rownames(mts.woody), 1, 1) == "H",]
tata.woody <- mts.woody[substr(rownames(mts.woody), 1, 1) == "T",]
ruah.woody <- mts.woody[substr(rownames(mts.woody), 1, 1) == "R",]

beta_h <- mean(vegdist(hira.woody, binary = TRUE))
beta_t <- mean(vegdist(tata.woody, binary = TRUE))
beta_r <- mean(vegdist(ruah.woody, binary = TRUE))



# Beta diversity for each site (mean Sorensen index)
hira.fern <- mts.ferns[substr(rownames(mts.ferns), 1, 1) == "H",]
tata.fern <- mts.ferns[substr(rownames(mts.ferns), 1, 1) == "T",]
ruah.fern <- mts.ferns[substr(rownames(mts.ferns), 1, 1) == "R",]

beta_fh <- mean(vegdist(hira.fern, binary = TRUE))
beta_ft <- mean(vegdist(tata.fern, binary = TRUE))
beta_fr <- mean(vegdist(ruah.fern, binary = TRUE))

######

## Pairwise analysis of woody species
woody.mts.dist.df <- data.frame(as.matrix(woody.dist)) %>%
  rownames_to_column(var = "site_elev") %>%
  pivot_longer(cols = -1) %>%
  separate(site_elev, c("site1", "elev1")) %>%
  separate(name, c("site2", "elev2")) %>%
  distinct() %>%
  mutate(same_elev = ifelse(elev1 == elev2 & site1 != site2 , 1, 0)) %>%
  mutate(site_pair = paste0(site1, ".", site2))

woody.pairwise.bc <- woody.mts.dist.df %>%
  filter(same_elev == 1) %>%
  filter(site_pair == "R.H" | site_pair == "T.H" | site_pair == "T.R")

woody.pairs.gg <- ggplot(woody.pairwise.bc) +
  geom_point(aes(y = value, x = elev1, col = site_pair, shape = site_pair), size = 3) +
  labs(x = "Elevation (m)", y = "Dissimilarity (Bray-Curtis)") +
  scale_colour_brewer(type = "qual", name = "Site pair") +
  ylim(0,1) +
  guides(shape = "none") +
  theme_bw()


## Pairwise analysis of fern species

ferns.mts.dist.df <- data.frame(as.matrix(ferns.dist)) %>%
  rownames_to_column(var = "site_elev") %>%
  pivot_longer(cols = -1) %>%
  separate(site_elev, c("site1", "elev1")) %>%
  separate(name, c("site2", "elev2")) %>%
  distinct() %>%
  mutate(same_elev = ifelse(elev1 == elev2 & site1 != site2 , 1, 0)) %>%
  mutate(site_pair = paste0(site1, ".", site2))

ferns.pairwise.bc <- ferns.mts.dist.df %>%
  filter(same_elev == 1) %>%
  filter(site_pair == "R.H" | site_pair == "T.H" | site_pair == "T.R")

ferns.pairs.gg <- ggplot(ferns.pairwise.bc) +
  geom_point(aes(y = value, x = elev1, col = site_pair, shape = site_pair), size = 3) +
  labs(x = "Elevation (m)", y = "Dissimilarity (Bray-Curtis)") +
  scale_colour_brewer(type = "qual", name = "Site pair") +
  ylim(0,1) +
  guides(shape = "none") +
  theme_bw() +
  theme(axis.title.y = element_blank())

pair.dist.plot <- (woody.pairs.gg + ferns.pairs.gg) + 
  plot_annotation(tag_levels = "a") + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


## Into the hierarchical classification

pal <- RColorBrewer::brewer.pal(3, "Accent")
woody.hcl <- hclust(woody.dist, method = "average")
woody.hcl <- as.dendrogram(woody.hcl)

# Sort sites based on appearance in the dendrogram

woody.hcl.col <- as.numeric(factor(site[order.dendrogram(woody.hcl)]))

woody.hcl <- woody.hcl %>% 
  set("labels_colors", pal[woody.hcl.col]) %>%
  set("leaves_pch", 19) %>%
  set("leaves_col", pal[woody.hcl.col]) %>%
  set("leaves_cex", 2)

woody.hcl.gg <- as.ggdend(woody.hcl) %>% 
  ggplot() + 
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  ylim(0, 0.8)+
  labs(x = "", y = "Distance") + 
  theme_bw()

## Now the ferns
ferns.hcl <- hclust(ferns.dist, method = "average")

ferns.hcl <- as.dendrogram(ferns.hcl)
ferns.hcl.col <- as.numeric(factor(site[order.dendrogram(ferns.hcl)]))


ferns.hcl <- ferns.hcl %>% 
  set("labels_colors", pal[ferns.hcl.col]) %>%
  set("leaves_pch", 19) %>%
  set("leaves_col", pal[ferns.hcl.col]) %>%
  set("leaves_cex", 2)

ferns.hcl.gg <- as.ggdend(ferns.hcl) %>% 
  ggplot() + 
  scale_x_discrete(labels = NULL, breaks = NULL) +
  labs(x = "", y = "Distance") + 
  ylim(0, 0.8)+
  theme_bw()

hcl.plots <- (woody.hcl.gg + ferns.hcl.gg) + 
  plot_annotation(tag_levels = "a")


## And the nMDS

### Woody species
woody.mds <- metaMDS(mts.woody, distance = "bray", wascores = TRUE, autotransform = FALSE, k = 2, trace = 0)
woody.mds.sc <- data.frame(scores(woody.mds, display = "sites"), site.code = rownames(mts.woody), site = site, elev = elev)
woody.stress.lbl <- paste("Stress = ", round(woody.mds$stress, 4))

woody.mds.simp.gg <- plot.mds.gg(woody.mds, clusters = site, txt.x = 1, txt.y = 0.7) +
  geom_text_repel(data = woody.mds.sc, aes(x = NMDS1, y = NMDS2, label = elev, col = site)) +
  scale_colour_brewer(type = "qual", name = "Site", labels = c("Hirikimata", "Ruahine", "Tataweka")) +
  guides(shape = "none") +
  theme(legend.position = "bottom")


### Ferns
ferns.mds <- metaMDS(mts.ferns, distance = "bray", wascores = TRUE, autotransform = FALSE, k = 2, rty = 100, trace = 0)
ferns.mds.sc <- data.frame(scores(ferns.mds, display = "sites"), site.code = rownames(mts.ferns), site = site, elev = elev)
ferns.stress.lbl <- paste("Stress = ", round(ferns.mds$stress, 4))

ferns.mds.simp.gg <- plot.mds.gg(ferns.mds, clusters = site, txt.x = 0.7, txt.y = -1) +
  geom_text_repel(data = ferns.mds.sc, aes(x = NMDS1, y = NMDS2, label = elev, col = site)) +
  scale_colour_brewer(type = "qual", name = "Site", labels = c("Hirikimata", "Ruahine", "Tataweka")) +
  guides(shape = "none") +
  theme(legend.position = "bottom")

source("./src/dispLocAnalysis.r")

## ----
mds.plots <- woody.mds.simp.gg / ferns.mds.simp.gg +  
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
