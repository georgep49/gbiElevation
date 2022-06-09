## R code to setup data and draw the range plots

library(tidyverse)
library(patchwork)

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

##########################################################
# Woody species occurrences

# Get ranges across all woody species, all sites
spp.list <- colnames(mts.woody)
woody.ranges <- get.elevation.limits(mts.woody, spp.list)

spp_at_h <- woody.ranges %>%
  filter(site == "H") %>%
  pull(species)

spp_at_t <- woody.ranges %>%
  filter(site == "T") %>%
  pull(species)

spp_at_r <- woody.ranges %>%
  filter(site == "R") %>%
  pull(species)

# spp present at all three sites
spp_at_all <- Reduce(intersect, list(spp_at_h, spp_at_t, spp_at_r)) 

# spp only at Hira
spp_at_hira_only <- Reduce(setdiff, list(spp_at_h, spp_at_t, spp_at_r)) 

# species at both T and R (and poss H)
spp_at_tr <- union(spp_at_t, spp_at_r) # spp at T and R

# get list of lowland woody species: present at 150 and 200 not above 500
lowland.spp <- (colSums(mts.woody[elev == 150,]) == 3) + (colSums(mts.woody[elev == 200,]) == 3) > 0
spp.notabv.450 <- colSums(mts.woody[elev %in% seq(550,600,50),]) == 0

lowland.spp <- lowland.spp * spp.notabv.450
lowland.spp <- names(lowland.spp[lowland.spp == TRUE]) 


########################
# Fern spp

f.spp.list <- colnames(mts.ferns)
fern.ranges <- get.elevation.limits(mts.ferns, f.spp.list)

fspp_at_h <- fern.ranges %>%
  filter(site == "H") %>%
  pull(species)

fspp_at_t <- fern.ranges %>%
  filter(site == "T") %>%
  pull(species)

fspp_at_r <- fern.ranges %>%
  filter(site == "R") %>%
  pull(species)

# spp present at all three sites
fspp_at_all <- Reduce(intersect, list(fspp_at_h, fspp_at_t, fspp_at_r)) 

# spp only at Hira
fspp_at_hira_only <- Reduce(setdiff, list(fspp_at_h, fspp_at_t, fspp_at_r)) 

# species at both T and R (and poss H)
fspp_at_tr <- union(fspp_at_t, fspp_at_r) # spp at T and R

# get list of lowland species: present at 150 and 200 not above 450
lowland.f.spp <- (colSums(mts.ferns[elev == 150,]) == 3) + (colSums(mts.ferns[elev == 200,]) == 3) > 0
fspp.notabv.450 <- colSums(mts.ferns[elev %in% seq(550,600,50),]) == 0

lowland.f.spp <- lowland.f.spp * fspp.notabv.450
lowland.f.spp <- names(lowland.f.spp[lowland.f.spp == TRUE]) 

###
# Get the range data for woody spp and fern spp for Hirakimata

# ordering by min
hira_only.range <- woody.ranges %>%
  filter(species %in% spp_at_hira_only)
hira_only.range$species <- fct_reorder(hira_only.range$species, hira_only.range$min_elev, min)

hira_f_only.range <- fern.ranges %>%
  filter(species %in% fspp_at_hira_only)
hira_f_only.range$species <- fct_reorder(hira_f_only.range$species, hira_f_only.range$min_elev, min)


hira.wdy.rg <- ggplot(data = hira_only.range) +
  geom_segment(aes(x = min_elev, xend = max_elev, y = species, yend = species), size = 3) +
  geom_point(data = hira_only.range %>% filter(single == TRUE), aes(x = min_elev, y = species), shape = 3) +
  labs(x = "Elevation (m)", y = "Species") +
  theme_bw()

hira.fern.rg <- ggplot(data = hira_f_only.range) +
  geom_segment(aes(x = min_elev, xend = max_elev, y = species, yend = species), size = 3) +
  geom_point(data = hira_f_only.range %>% filter(single == TRUE), aes(x = min_elev, y = species), shape = 3) +
  labs(x = "Elevation (m)", y = "Species") +
  theme_bw()


####
# Extract the spp at Hira also present elsewhere (as may have different range) - have to be "lowland species"
# and occur at all three sites

woody.low.all <- woody.ranges %>%
  filter(n_sites == 3, species %in% upper_n(lowland.spp, 3))

hira.e <- woody.low.all %>% filter(hirak == TRUE)
hira.sgl.e <- filter(hira.e, single == 1)

all.e <- woody.low.all %>%
  filter(hirak == FALSE) %>%  # range not at Hirak
  group_by(species) %>%
  summarise(min_elev = min(min_elev), max_elev = max(max_elev)) %>%
  ungroup()

# Plots ordered by upper limit on Hirikimata
hira.e$species <- fct_reorder(hira.e$species, hira.e$max_elev, min)
all.e$species <- fct_reorder(all.e$species, hira.e$max_elev, min)


cols <- c("All" = "grey", "Hirakimata" = "#7FC97F")

woody.low.comb <- bind_rows(list(All = all.e, Hirakimata = hira.e), .id = "Site")
woody.low.comb$species <- fct_reorder(woody.comb$species, woody.comb$max_elev, min)


all.wdy.ls <- ggplot() +
  geom_linerange(data = woody.low.comb, aes(ymin = min_elev, ymax = max_elev, x = species, group = Site, col = Site), position = position_dodge(.5), size = 1) +
  geom_hline(yintercept = 400, lty = 2, size = 1.2) +
  geom_point(data = hira.e %>% filter(single == 1), aes(x = species, y = max_elev), col = "#7FC97F", size = 2) +
  scale_colour_manual(values = cols) +
  labs(x = "Species", y = "Elevation (m)") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

####

fern.low.all <- fern.ranges %>%
  filter(n_sites == 3, species %in% upper_n(lowland.f.spp, 3))

hira.f.e <- fern.low.all %>% filter(hirak == TRUE)
hira.f.sgl.e <- filter(hira.f.e, single == 1)

all.f.e <- fern.low.all %>%
  filter(hirak == FALSE) %>%    # get range not at Hirakimata
  group_by(species) %>%
  summarise(min_elev = min(min_elev), max_elev = max(max_elev)) %>%
  ungroup()


fern.comb <- bind_rows(list(All = all.f.e, Hirakimata = hira.f.e), .id = "Site")
fern.comb$species <- fct_reorder(fern.comb$species, fern.comb$max_elev, min)

all.fern.ls <- ggplot() +
  geom_linerange(data = fern.comb, aes(ymin = min_elev, ymax = max_elev, x = species, group = Site, col = Site), position = position_dodge(.5), size = 1) +
  geom_hline(yintercept = 400, lty = 2, size = 1.2) +
  geom_point(data = hira.f.e %>% filter(single == 1), aes(x = species, y = max_elev), col = "#7FC97F", size = 2) +
  scale_colour_manual(values = cols) +
  labs(x = "Species", y = "Elevation (m)") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

####
hira.range.plots <- (hira.wdy.rg | hira.fern.rg) +
  plot_annotation(tag_levels = "a")

all.ls.plots <- (all.wdy.ls | all.fern.ls) +
  plot_annotation(tag_levels = "a") + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
