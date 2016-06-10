# -----------------------------------------
# select 2015 votes in 2006 map, alternative coalition spec: pri priGreen green as happened
# -----------------------------------------
dat <- elec0315$df2015d0
tmp.ptot <- dat$ptot # keep ptot for use later
rownames(dat) <- NULL
################################################################################################################
# DATA NEEDS [EFEC V1SH V2SH] FOR STD LINZER OR [PTOT V1SH V2SH ... ABS] (SHARES OVER PTOT) FOR my GKB VERSION #
head(dat)
colnames(dat)
################################################################################################################
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
dat <- dat[,c("pri","pan","prd","pt","pvem","mc","panal","morena","ph","ps","indep1","indep2","pric","prdc")]
# consider pri and pri-pvem the same (else pri "absent" from 1/3 district has lower elasticity)
#dat$pri <- dat$pri + dat$pric; dat$pric <- NULL
dat$prd <- dat$prd + dat$prdc; dat$prdc <- NULL
# who won the seats? useful if non-winners (or subset of them) wish to be dropped
table(apply(dat, 1, function(x) which.max(x)))
# drop pt, ph, ps didn't win seats, drop indep (even if won 1 seat, but clouthier only ran in 1 district) and panal (1 seat won too)
dat <- dat[,-which(colnames(dat) %in% c("pt","ph","ps","indep1","indep2","panal"))]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
## compute abstentions
tmp.abst <- tmp.ptot - tmp.efec # useful later
#
# CHOOSE ROUTE 1 OR ROUTE 2
## ## ROUTE 1: keep efec and votes only (as Linzer does) to use standard functions
## # vote shares
## dat <- dat/tmp.efec
## # add efec vote in 1st column (or, if included, ptot in 1st col and efec in 2nd) 
## dat <- cbind(tmp.efec, dat); colnames(dat)[which(colnames(dat)=="tmp.efec")] <- "efec"
## rm(tmp.ptot, tmp.efec, tmp.abst)
#
## ROUTE 2: add ptot in col1, abst in last col, and express vote shares relative to ptot --- simulates ptot for GKB
# vote shares
dat <- dat/tmp.ptot
# add ptot in 1st column
dat <- cbind(tmp.ptot, dat); colnames(dat)[which(colnames(dat)=="tmp.ptot")] <- "ptot"
# add abst in last column
dat <- cbind(dat, tmp.abst/tmp.ptot); colnames(dat)[grep("tmp.abst", colnames(dat))] <- "abs"
rm(tmp.ptot, tmp.efec, tmp.abst)
#
# clean: rename left coalition and green
colnames(dat)[which(colnames(dat)=="prd")] <- "left"
colnames(dat)[which(colnames(dat)=="pvem")] <- "green"
head(dat)

# find patterns of party contestation, change data to lists
dat.pat <- findpatterns(dat)
head(dat.pat[[2]]) # debug

# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually, increase if needed)
fit <- list()
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 2, nrep = 10, scatter = TRUE) 
fit[[2]] <- mvnmix( dat = dat.pat[[2]], components = 1, nrep = 10, scatter = TRUE)
# note: successive fits with same component number change a lot! This is true in Linzer (my tweaked functions have not yet been invoked)... Inheritance problem?
#
summary(fit[[1]])
head(fit[[1]]$y)

# tweak function show.marginals formerly plotting marginals to get plot input NOT USED FOR NOW
# use tweaked function to verify fit of model to data: plot vote share histograms and party marginal densities
# (Linzer has show.marginals() function, but this offers control over output)
#par(mar = c(4, 3, 0.5, 0.5))
#mtext(side = 1, text = "Turnout (x100k)", line = 2.5, cex = 1.1)
#mtext(side = 2, text = "density", line = 2, cex = 0.8)
#layout(matrix(c(0,0,1,0,2,3,4,5,6), nrow = 3, ncol = 3, byrow = FALSE))
## prep <- as.data.frame(my.marginals.prep(fit, numdraws=10000))
## colnames(prep) <- colnames(dat)
## head(prep)
## #
## hist(dat$efec/10000,         breaks = 30,              col = "gray90",                freq = FALSE, main = "", xlab = "Turnout (x10k)")
## lines(density(prep$efec[prep$efec > 0], na.rm = TRUE, adjust = 0.7), lwd = 2)
## #
## hist(dat$pan,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pan")
## lines(density(prep$pan, na.rm = TRUE, adjust = 0.6), lwd = 2)
## #
## hist(dat$pri,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pri")
## lines(density(prep$pri, na.rm = TRUE, adjust = 0.6), lwd = 2)
## #
## hist(dat$left,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
## lines(density(prep$left, na.rm = TRUE, adjust = 0.6), lwd = 2)
## #
## hist(dat$green[dat$green>0],   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
## lines(density(prep$green, na.rm = TRUE, adjust = 0.6), lwd = 2)
## #
## hist(dat$panal[dat$panal>0], breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, panal")
## lines(density(prep$panal, na.rm = TRUE, adjust = 0.6), lwd = 2)
# packaged plot
show.marginals(fit, numdraws=50000)

# Simulate elections, estimate swing ratios and plot results
#res <- swingratio((fit, sims=5000, graph = TRUE) # original call
elas <- my.swingratio.gkb(fit, sims=1000) # fit includes ptot and abstention for GKB -- version above does sim but does not estimate swings nor plot them

## add results to list with swing-ratios sims for different elections
#swRats <- list()
swRats$df2015d0priCoalAsItWas <- elas
summary(swRats)
save(swRats, file = paste(dd, "swingRatios9715.RData", sep = ""))

## loads pre-estimated swing-ratios for different elections
load(paste(dd, "swingRatios9715.RData", sep = ""))
summary(swRats)

## # rename object: will be useful when and if tweaked function also estimates swing ratios as Linzer does
## names(elas)[which(names(elas)=="swing")] <- "swing.mean"
## # add components to compute swing ratios (seats-votes elaticity)
## elas$vdiff <- as.data.frame(t(t(elas$votemat) - colMeans(elas$votemat)))
## elas$sdiff <- as.data.frame(t(t(elas$seatmat) - colMeans(elas$seatmat)))
## # re-compute elasticity
## elas$swing.sims <- elas$sdiff / elas$vdiff
## # add number parties
## elas$nParties <- ncol(elas$seatmat)
#
## # obtain estimates with 95% ci --- looks wrong: RE-MODEL WITH M-P AND R-M DONE INSIDE BUGS TO GET ERRORS
## elas.ci <- as.data.frame(matrix(NA, nrow = 3, ncol = 3)); colnames(elas.ci) <- c("lo","mu","hi"); rownames(elas.ci) <- c("pri","pan","left")
## # pri
## tmp <- as.data.frame(elas$vs[[1]])
## elas.ci$lo[1] <- (tmp$lower[which(tmp$vote==.39)] - tmp$lower[which(tmp$vote==.37)]) / .02
## elas.ci$mu[1] <- (tmp$mean [which(tmp$vote==.39)] - tmp$mean [which(tmp$vote==.37)]) / .02
## elas.ci$hi[1] <- (tmp$upper[which(tmp$vote==.39)] - tmp$upper[which(tmp$vote==.37)]) / .02
## # pan
## tmp <- as.data.frame(elas$vs[[2]])
## elas.ci$lo[2] <- (tmp$lower[which(tmp$vote==.28)] - tmp$lower[which(tmp$vote==.26)]) / .02
## elas.ci$mu[2] <- (tmp$mean [which(tmp$vote==.28)] - tmp$mean [which(tmp$vote==.26)]) / .02
## elas.ci$hi[2] <- (tmp$upper[which(tmp$vote==.28)] - tmp$upper[which(tmp$vote==.26)]) / .02
## # left
## tmp <- as.data.frame(elas$vs[[3]])
## elas.ci$lo[3] <- (tmp$lower[which(tmp$vote==.29)] - tmp$lower[which(tmp$vote==.27)]) / .02
## elas.ci$mu[3] <- (tmp$mean [which(tmp$vote==.29)] - tmp$mean [which(tmp$vote==.27)]) / .02
## elas.ci$hi[3] <- (tmp$upper[which(tmp$vote==.29)] - tmp$upper[which(tmp$vote==.27)]) / .02
## #
## elas$swing.ci <- elas.ci
#
rm(dat,dat.pat,elas,fit) # clean

# -----------------------------------------
# select 2015 votes in 2006 map, alternative coalition spec: pri=pri+priGreen+green
# -----------------------------------------
dat <- elec0315$df2015d0
tmp.ptot <- dat$ptot # keep ptot for use later
rownames(dat) <- NULL
################################################################################################################
# DATA NEEDS [EFEC V1SH V2SH] FOR STD LINZER OR [PTOT V1SH V2SH ... ABS] (SHARES OVER PTOT) FOR my GKB VERSION #
head(dat)
colnames(dat)
################################################################################################################
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
dat <- dat[,c("pri","pan","prd","pt","pvem","mc","panal","morena","ph","ps","indep1","indep2","pric","prdc")]
# consider pvem as a faction of the pri
dat$pri <- dat$pri + dat$pric + dat$pvem; dat$pric <- dat$pvem <- NULL
dat$prd <- dat$prd + dat$prdc; dat$prdc <- NULL
# who won the seats? useful if non-winners (or subset of them) wish to be dropped
table(apply(dat, 1, function(x) which.max(x)))
# drop pt, ph, ps didn't win seats, drop indep (even if won 1 seat, but clouthier only ran in 1 district) and panal (1 seat won too)
dat <- dat[,-which(colnames(dat) %in% c("pt","ph","ps","indep1","indep2","panal"))]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
## compute abstentions
tmp.abst <- tmp.ptot - tmp.efec # useful later
#
# CHOOSE ROUTE 1 OR ROUTE 2
## ## ROUTE 1: keep efec and votes only (as Linzer does) to use standard functions
## # vote shares
## dat <- dat/tmp.efec
## # add efec vote in 1st column (or, if included, ptot in 1st col and efec in 2nd) 
## dat <- cbind(tmp.efec, dat); colnames(dat)[which(colnames(dat)=="tmp.efec")] <- "efec"
## rm(tmp.ptot, tmp.efec, tmp.abst)
#
## ROUTE 2: add ptot in col1, abst in last col, and express vote shares relative to ptot --- simulates ptot for GKB
# vote shares
dat <- dat/tmp.ptot
# add ptot in 1st column
dat <- cbind(tmp.ptot, dat); colnames(dat)[which(colnames(dat)=="tmp.ptot")] <- "ptot"
# add abst in last column
dat <- cbind(dat, tmp.abst/tmp.ptot); colnames(dat)[grep("tmp.abst", colnames(dat))] <- "abs"
rm(tmp.ptot, tmp.efec, tmp.abst)
#
# clean: rename left coalition and green
colnames(dat)[which(colnames(dat)=="prd")] <- "left"
head(dat)



# find patterns of party contestation, change data to lists
dat.pat <- findpatterns(dat)

# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually, increase if needed)
fit <- list()
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 3, nrep = 10, scatter = TRUE) 
# note: successive fits with same component number change a lot! This is true in Linzer (my tweaked functions have not yet been invoked)... Inheritance problem?
#
summary(fit[[1]])
head(fit[[1]]$y)

# tweak function show.marginals formerly plotting marginals to get plot input NOT USED FOR NOW
# use tweaked function to verify fit of model to data: plot vote share histograms and party marginal densities
# (Linzer has show.marginals() function, but this offers control over output)
#par(mar = c(4, 3, 0.5, 0.5))
#mtext(side = 1, text = "Turnout (x100k)", line = 2.5, cex = 1.1)
#mtext(side = 2, text = "density", line = 2, cex = 0.8)
#layout(matrix(c(0,0,1,0,2,3,4,5,6), nrow = 3, ncol = 3, byrow = FALSE))
## prep <- as.data.frame(my.marginals.prep(fit, numdraws=10000))
## colnames(prep) <- colnames(dat)
## head(prep)
## #
## hist(dat$efec/10000,         breaks = 30,              col = "gray90",                freq = FALSE, main = "", xlab = "Turnout (x10k)")
## lines(density(prep$efec[prep$efec > 0], na.rm = TRUE, adjust = 0.7), lwd = 2)
## #
## hist(dat$pan,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pan")
## lines(density(prep$pan, na.rm = TRUE, adjust = 0.6), lwd = 2)
## #
## hist(dat$pri,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pri")
## lines(density(prep$pri, na.rm = TRUE, adjust = 0.6), lwd = 2)
## #
## hist(dat$left,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
## lines(density(prep$left, na.rm = TRUE, adjust = 0.6), lwd = 2)
## #
## hist(dat$green[dat$green>0],   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
## lines(density(prep$green, na.rm = TRUE, adjust = 0.6), lwd = 2)
## #
## hist(dat$panal[dat$panal>0], breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, panal")
## lines(density(prep$panal, na.rm = TRUE, adjust = 0.6), lwd = 2)
# packaged plot
show.marginals(fit, numdraws=50000)

# Simulate elections, estimate swing ratios and plot results
#res <- swingratio((fit, sims=5000, graph = TRUE) # original call
elas <- my.swingratio.gkb(fit, sims=1000) # fit includes ptot and abstention for GKB -- version above does sim but does not estimate swings nor plot them

## add results to list with swing-ratios sims for different elections
#swRats <- list()
swRats$df2015d0greenAsPriFaction <- elas
summary(swRats)
save(swRats, file = paste(dd, "swingRatios9715.RData", sep = ""))

## loads pre-estimated swing-ratios for different elections
load(paste(dd, "swingRatios9715.RData", sep = ""))
summary(swRats)

## # rename object: will be useful when and if tweaked function also estimates swing ratios as Linzer does
## names(elas)[which(names(elas)=="swing")] <- "swing.mean"
## # add components to compute swing ratios (seats-votes elaticity)
## elas$vdiff <- as.data.frame(t(t(elas$votemat) - colMeans(elas$votemat)))
## elas$sdiff <- as.data.frame(t(t(elas$seatmat) - colMeans(elas$seatmat)))
## # re-compute elasticity
## elas$swing.sims <- elas$sdiff / elas$vdiff
## # add number parties
## elas$nParties <- ncol(elas$seatmat)
#
## # obtain estimates with 95% ci --- looks wrong: RE-MODEL WITH M-P AND R-M DONE INSIDE BUGS TO GET ERRORS
## elas.ci <- as.data.frame(matrix(NA, nrow = 3, ncol = 3)); colnames(elas.ci) <- c("lo","mu","hi"); rownames(elas.ci) <- c("pri","pan","left")
## # pri
## tmp <- as.data.frame(elas$vs[[1]])
## elas.ci$lo[1] <- (tmp$lower[which(tmp$vote==.39)] - tmp$lower[which(tmp$vote==.37)]) / .02
## elas.ci$mu[1] <- (tmp$mean [which(tmp$vote==.39)] - tmp$mean [which(tmp$vote==.37)]) / .02
## elas.ci$hi[1] <- (tmp$upper[which(tmp$vote==.39)] - tmp$upper[which(tmp$vote==.37)]) / .02
## # pan
## tmp <- as.data.frame(elas$vs[[2]])
## elas.ci$lo[2] <- (tmp$lower[which(tmp$vote==.28)] - tmp$lower[which(tmp$vote==.26)]) / .02
## elas.ci$mu[2] <- (tmp$mean [which(tmp$vote==.28)] - tmp$mean [which(tmp$vote==.26)]) / .02
## elas.ci$hi[2] <- (tmp$upper[which(tmp$vote==.28)] - tmp$upper[which(tmp$vote==.26)]) / .02
## # left
## tmp <- as.data.frame(elas$vs[[3]])
## elas.ci$lo[3] <- (tmp$lower[which(tmp$vote==.29)] - tmp$lower[which(tmp$vote==.27)]) / .02
## elas.ci$mu[3] <- (tmp$mean [which(tmp$vote==.29)] - tmp$mean [which(tmp$vote==.27)]) / .02
## elas.ci$hi[3] <- (tmp$upper[which(tmp$vote==.29)] - tmp$upper[which(tmp$vote==.27)]) / .02
## #
## elas$swing.ci <- elas.ci
#
rm(dat,dat.pat,elas,fit) # clean

