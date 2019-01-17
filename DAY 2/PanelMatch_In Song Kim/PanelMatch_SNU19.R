rm(list=ls())

## install PanelMatch package via github
library(devtools)
install_github("insongkim/PanelMatch", ref = "development")
library(PanelMatch)

## setting working directory
setwd("~/Downloads")
FIG_DIR <- file.path(getwd())

## ----------------------------------------------------------------
## 1. DisplayTreatment
pdf(file= file.path(FIG_DIR, "treat_display.pdf"))
DisplayTreatment(unit.id = "wbcode2",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code", y.size=.5,
                 treatment = "dem", data = dem)
dev.off()
## ----------------------------------------------------------------

treateds <- findAllTreated(dmat = dem, treatedvar = "dem",
                           time.var = "year", unit.var = "wbcode2")
msets <- get.matchedsets(t = treateds$year, id = treateds$wbcode2,
                         data = dem, L = 4, t.column = "year",
                         id.column = "wbcode2", treatedvar = "dem")

## ----------------------------------------------------------------
## wbcode2 of 177 corresponds to Thailand 
## ----------------------------------------------------------------

thailand.1978.index <- which(names(msets) == "177.1978") 
msets[[thailand.1978.index]]

pdf(file= file.path(FIG_DIR, "thailand_treat_display.pdf"))
DisplayTreatment(unit.id = "wbcode2",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "Country Code",
                 treatment = "dem", data = dem,
                 matched.set = msets[thailand.1978.index])
dev.off()

## plot
pdf(file= file.path(FIG_DIR, "treat_hist.pdf"))
plot(msets)
dev.off()

## ----------------------------------------------------------------
## wbcode2 = 6 is Argentina
## ----------------------------------------------------------------

arg.1983.idx <- which(names(msets) == "6.1983")
msets[[arg.1983.idx]]

pdf(file= file.path(FIG_DIR, "argentina_treat_display.pdf"))
DisplayTreatment(unit.id = "wbcode2",
                 time.id = "year", legend.position = "none",
                 xlab = "year", ylab = "",
                 treatment = "dem", data = dem,
                 matched.set = msets[arg.1983.idx])
dev.off()

## ----------------------------------------------------------------
## Refinement of matched sets
## ----------------------------------------------------------------

## Mahalanobis distance
pm.obj <- PanelMatch2(lag = 4, time.id = "year", unit.id = "wbcode2",
                      treatment = "dem", outcome = "y",
                      refinement.method = "mahalanobis", data = dem,
                      match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4),
                      size.match = 10)#can also tweak size.match
pm.obj[[arg.1983.idx]]


## CBPS
pm.obj <- PanelMatch2(lag = 4, time.id = "year",
                      unit.id = "wbcode2", treatment = "dem",
                      outcome = "y", refinement.method = "CBPS.match",
                      data = dem,
                      match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4),
                      size.match = 10)
pm.obj[[arg.1983.idx]]

## Propensity Score Matching
pm.obj <- PanelMatch2(lag = 4, time.id = "year", unit.id = "wbcode2",
                      treatment = "dem", outcome = "y",
                      refinement.method = "ps.match", data = dem,
                      match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 10)
pm.obj[[arg.1983.idx]]


## CBPS weighting
pm.obj <- PanelMatch2(lag = 4, time.id = "year", unit.id = "wbcode2",
                      treatment = "dem", outcome = "y",
                      refinement.method = "CBPS.weight", data = dem,
                      match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4)) #no longer need size.match
pm.obj[[arg.1983.idx]]

## Propensity Score Weighting
pm.obj <- PanelMatch2(lag = 4, time.id = "year", unit.id = "wbcode2",
                      treatment = "dem", outcome = "y",
                      refinement.method = "ps.weight", data = dem,
                      match.missing = T, 
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4)) #no longer need size.match
pm.obj[[arg.1983.idx]]


## ----------------------------------------------------------------
## PanelEstimate
## ----------------------------------------------------------------

dem$auth <- ifelse(dem$dem == 0, 1, 0)
layout((matrix(c(1,2,3,4,5,6,7,8,9,10), byrow = TRUE, nrow = 2)))
layout.show(n = 10)

## democratization
pm.obj <- PanelMatch2(4, "year", "wbcode2", "dem", "y",
                      refinement.method = "mahalanobis",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 5)
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj,
                      data = dem, outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "maha_m5.pdf"))
plot(res,ylim = c(-20, 10),
     main = "mahalanobis matching, max size = 5",
     ylab = "democratization effect")
dev.off()


pm.obj <- PanelMatch2(4, "year", "wbcode2", "dem", "y",
                      refinement.method = "mahalanobis",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 10)
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj,
                      data = dem, outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "maha_m10.pdf"))
plot(res,ylim = c(-20, 10),
     main = "mahalanobis matching, max size = 10",
     ylab = "democratization effect")
dev.off()


pm.obj <- PanelMatch2(4, "year", "wbcode2", "dem", "y",
                      refinement.method = "CBPS.match",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 5)
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj, data = dem,
                      outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "cbps_m5.pdf"))
plot(res,ylim = c(-20, 10), main = "PS matching, max size = 5",
     ylab = "democratization effect")
dev.off()

pm.obj <- PanelMatch2(4, "year", "wbcode2", "dem", "y",
                      refinement.method = "CBPS.match", data = dem, match.missing = T, covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 10)
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj, data = dem,
                      outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "cbps_m10.pdf"))
plot(res,ylim = c(-20, 10), main = "PS matching, max size = 10",
     ylab = "democratization effect")
dev.off()

pm.obj <- PanelMatch2(4, "year", "wbcode2", "dem", "y",
                      refinement.method = "CBPS.weight",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4))
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj, data = dem,
                      outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "CBPS_weight.pdf"))
par(cex.lab=1.7, cex.main=2, mar=c(4,6,4,2))
plot(res,ylim = c(-10, 5), main = "CBPS weighting",
     ylab ="Effects of Democratization on Growth",
     xlab="")
dev.off()


## authoritarian reversal
pm.obj <- PanelMatch2(4, "year", "wbcode2", "auth", "y",
                      refinement.method = "mahalanobis",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 5)
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj, data = dem,
                      outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "auth_maha_m5.pdf"))
plot(res,ylim = c(-20, 10), main = "mahalanobis matching, max size = 5",
     ylab = "authoritarian reversal effect")
dev.off()

pm.obj <- PanelMatch2(4, "year", "wbcode2", "auth", "y",
                      refinement.method = "mahalanobis",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 10)
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj, data = dem,
                      outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "auth_maha_m10.pdf"))
plot(res,ylim = c(-20, 10), main = "mahalanobis matching, max size = 10",
     ylab = "authoritarian reversal effect")
dev.off()


pm.obj <- PanelMatch2(4, "year", "wbcode2", "auth", "y",
                      refinement.method = "CBPS.match",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 5)
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj, data = dem,
                      outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "auth_cbps_m5.pdf"))
plot(res,ylim = c(-20, 10), main = "PS matching, max size = 5",
     ylab = "authoritarian reversal effect")
dev.off()

pm.obj <- PanelMatch2(4, "year", "wbcode2", "auth", "y",
                      refinement.method = "CBPS.match",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4), size.match = 10)
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj, data = dem,
                      outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "auth_cbps_m10.pdf"))
plot(res,ylim = c(-20, 10), main = "PS matching, max size = 10",
     ylab = "authoritarian reversal effect")
dev.off()

    
pm.obj <- PanelMatch2(4, "year", "wbcode2", "auth", "y",
                      refinement.method = "CBPS.weight",
                      data = dem, match.missing = T,
                      covs.formula = ~ tradewb + lag("tradewb", 1:4) +
                      lag("y", 1:4))
res <- PanelEstimate2(lead = 0:4, inference = "bootstrap",
                      qoi = "att", sets = pm.obj, data = dem,
                      outcome.variable = "y")

pdf(file= file.path(FIG_DIR, "auth_cbps_weight.pdf"))
par(cex.lab=2, cex.main=2, mar=c(4,6,4,2))
plot(res,ylim = c(-20, 5), main = "CBPS weighting",
     ylab ="Authoritarian reversal effect")
dev.off()
