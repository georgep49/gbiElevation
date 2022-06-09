# Multinomial analysis for the three peaks

# We assume that a species is distrbuted relative to the area ~ elevation of
# the mountain - this is a multinomial trial with p based on area and k = 3
# and categories as per the nulls table below

## nulls table
# h	0	1	2	3	0	1	2	0	1	0
# t	0	0	0	0	1	1	1	2	2	3
# r	3	2	1	0	2	1	0	1	0	0
##

mt.combo <- data.frame(
  stringsAsFactors = FALSE,
  row.names = c("h", "t", "r"),
  rrr = c(0L, 0L, 3L),
  hrr = c(1L, 0L, 2L),
  hhr = c(2L, 0L, 1L),
  hhh = c(3L, 0L, 0L),
  trr = c(0L, 1L, 2L),
  hrt = c(1L, 1L, 1L),
  hht = c(2L, 1L, 0L),
  ttr = c(0L, 2L, 1L),
  htt = c(1L, 2L, 0L),
  ttt = c(0L, 3L, 0L)
)

p <- c(6, 5, 4) / 15

# Null prop of species at combinations of sites
# (H, R, T,	R & T,	H & R,	H & T,	H & R & T )
null.dist <- apply(t(mt.combo), 1, FUN = dmultinom, size = 3, prob = p)

# Null dist with categories collapse (e.g. HHR and HRR to one)
null.dist.collapse <- c(0.064,	0.019,	0.037,	0.16,	0.213,	0.293,	0.213)

# Observed number of species across the site combinations
obs.woody <- c(23, 4, 6, 15,	6, 8, 46)
obs.fern <- c(12, 1, 0, 3, 8, 10,	19)

# Chisq tests
chi.woody <- chisq.test(obs.woody, p = null.dist.collapse, rescale.p = TRUE)
chi.fern <- chisq.test(obs.fern, p = null.dist.collapse, rescale.p = TRUE)

# Residuals are the pearson residuals (o - e / sqrt(e))
round(prop.table(chi.woody$residuals^2), 3) * 100
round(prop.table(chi.fern$residuals^2), 3) * 100


