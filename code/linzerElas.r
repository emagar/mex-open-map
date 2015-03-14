rm(list = ls())
options(width = 150)
#
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/")  # where to save and retrieve objects
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/") # raw data directory
gd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/graphs/") # graph data directory
setwd(wd)

# data preprared in analizaEscenarios.r is read here
load(file = paste(dd, "elec060912.RData"))
summary(elec060912)
#
## ## use to extract all objects from list elec060912 if so wished
## for(i in 1:length(elec060912)){
##     #i <- 1 # debug
##     ##first extract the object value
##     tempobj=elec060912[[i]]
##     ##now create a new variable with the original name of the list item
##     eval(parse(text=paste(names(elec060912)[[i]],"= tempobj")))
## }
## rm(elec060912)
## dim(df2012d0)

# -----------------------------------------
# select 2012 votes in 2006 map
# -----------------------------------------
dat <- elec060912$df2012d0
rownames(dat) <- NULL
head(dat)
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
dat <- dat[,c("pri","pan","pric","prdc","panal","pvem")]
# consider pri and pri-pvem the same (else pri "absent" from 1/3 district has lower elasticity)
dat$pri <- dat$pri + dat$pric; dat$pric <- NULL
# who won the seats?
table(apply(dat, 1, function(x) which.max(x)))
## # drop panal, didn't win seats
## dat <- dat[,-5]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
# vote shares
dat <- dat/tmp.efec
# add total vote in 1st column
dat <- cbind(tmp.efec, dat); colnames(dat)[1] <- "efec"; rm(tmp.efec)
# rename left coalition and green
colnames(dat)[which(colnames(dat)=="prdc")] <- "left"
colnames(dat)[which(colnames(dat)=="pvem")] <- "green"
head(dat)

# find patterns of party contestation, change data to lists
library(seatsvotes)
dat.pat <- findpatterns(dat)
# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually)
fit <- list()
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 1, nrep = 10, scatter = TRUE) 
fit[[2]] <- mvnmix( dat = dat.pat[[2]], components = 2, nrep = 10, scatter = TRUE)

# tweak function show.marginals formerly plotting marginals to get plot input
my.marginals.prep <- function (fit, numdraws = 10000){
    dat <- reconstruct(fit)
    if (numdraws > 0) {
        numdraws <- ceiling(numdraws/nrow(dat)) * nrow(dat)
        vsim <- NULL
        for (i in 1:(numdraws/nrow(dat))) {
            vsim <- rbind(vsim, sim.election(fit, dat))
        }
    }
    return(vsim)
}
environment(my.marginals.prep) <- asNamespace('seatsvotes')
# use tweaked function to verify fit of model to data: plot vote share histograms and party marginal densities
# (Linzer has show.marginals() function, but this offers control over output)
#par(mar = c(4, 3, 0.5, 0.5))
#mtext(side = 1, text = "Turnout (x100k)", line = 2.5, cex = 1.1)
#mtext(side = 2, text = "density", line = 2, cex = 0.8)
#layout(matrix(c(0,0,1,0,2,3,4,5,6), nrow = 3, ncol = 3, byrow = FALSE))
prep <- as.data.frame(my.marginals.prep(fit, numdraws=10000))
colnames(prep) <- colnames(dat)
head(prep)
#
hist(dat$efec/10000,         breaks = 30,              col = "gray90",                freq = FALSE, main = "", xlab = "Turnout (x10k)")
lines(density(prep$efec[prep$efec > 0], na.rm = TRUE, adjust = 0.7), lwd = 2)
#
hist(dat$pan,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pan")
lines(density(prep$pan, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$pri,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pri")
lines(density(prep$pri, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$left,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
lines(density(prep$left, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$green[dat$green>0],   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
lines(density(prep$green, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$panal[dat$panal>0], breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, panal")
lines(density(prep$panal, na.rm = TRUE, adjust = 0.6), lwd = 2)
# packaged plot
show.marginals(fit, numdraws=50000)

## # tweak/understand swinratio function
## elas.sims <- function (fit, sims = 1000, rule = plurality, graph = TRUE) 
## {
##     starttime <- Sys.time()
##     dat <- reconstruct(fit)
##     dat[is.na(dat)] <- 0
##     np <- ncol(dat)
##     votemat <- NULL
##     seatmat <- NULL
##     for (s in 1:sims) {
##         vsim <- sim.election(fit, dat)
##         vsim[is.na(vsim)] <- 0
##         partyvotes <- vsim[, 1] * vsim[, -1]
##         votemat <- rbind(votemat, colSums(partyvotes)/sum(partyvotes))
##         seatmat <- rbind(seatmat, rule(vsim[, -1]))
##     }
##     ret <- list()
##     truevotes <- dat[, 1] * dat[, -1]
##     ret$truevote <- colSums(truevotes)/sum(truevotes)
##     ret$trueseat <- rule(dat[, -1])
##     names(ret$trueseat) <- names(ret$truevote)
##     swing <- NULL
##     swing.lm <- NULL
##     seatlist <- list()
##     for (j in 1:(np - 1)) {
##         # hi and lo select sims +1% and -1% with some jitter
##         smean.hi <- mean(seatmat[(votemat[, j] < (ret$truevote[j] + 0.012))
##                                & (votemat[, j] > (ret$truevote[j] + 0.008)), j])
##         smean.lo <- mean(seatmat[(votemat[, j] > (ret$truevote[j] - 0.012))
##                                & (votemat[, j] < (ret$truevote[j] - 0.008)), j])
##         swing <- c(swing, round((smean.hi - smean.lo)/0.02, 2))
##         swing.lm <- c(swing.lm, round(coefficients(lm(seatmat[, j] ~ votemat[, j]))[2], 2))
##         vrange <- seq(min(floor(votemat[, j] * 100)), max(floor(votemat[, j] * 100)), 1)/100
##         seatlist[[j]] <- cbind(vrange, matrix(NA, nrow = length(vrange), 
##             ncol = 3))
##         colnames(seatlist[[j]]) <- c("vote", "mean", "lower", "upper")
##         for (i in 1:length(vrange)) {
##             sel <- seatmat[(votemat[, j] >= (vrange[i] - 0.002))
##                          & (votemat[, j] <  (vrange[i] + 0.002)), j]
##             # this is why parties with few seats are dropped... percentage points with fewer than 10 simulated obs dropped
##             if (length(sel) < 10) {
##                 seatlist[[j]][i, 2:4] <- NA
##             }
##             else {
##                 seatlist[[j]][i, 2] <- mean(sel)
##                 seatlist[[j]][i, 3:4] <- quantile(sel, probs = c(0.025, 
##                   0.975))
##             }
##         }
##     }
##     if (graph) {
##         par(mar = c(4.4, 4.1, 0.5, 0.5), mfrow = c(1, 1))
##         plot(ret$truevote[!is.na(swing)], ret$trueseat[!is.na(swing)], 
##             pch = 19, cex = 1.2, xlim = c(0, 0.7), ylim = c(0, 
##                 0.7), xlab = "Party vote share", ylab = "Party seat share", 
##             cex.lab = 1.5, xaxt = "n", yaxt = "n")
##         axis(1, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         for (j in 1:(np - 1)) {
##             if (!is.na(swing[j])) {
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 2], 
##                   lwd = 2.5)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 3], 
##                   lwd = 2, lty = 3)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 4], 
##                   lwd = 2, lty = 3)
##                 text(ret$truevote[j] + 0.01, ret$trueseat[j], 
##                   paste(colnames(dat)[j + 1], ": ", swing[j], 
##                     sep = ""), cex = 1.2, pos = 4)
##             }
##         }
##     }
##     vdiff <- as.data.frame(t(t(votemat) - colMeans(votemat)))
##     sdiff <- as.data.frame(t(t(seatmat) - colMeans(seatmat)))
##     swing.median <- round(apply(sdiff/vdiff, 2, median), 2)
##     names(seatlist) <- colnames(votemat) <- colnames(seatmat) <- names(ret$truevote)
##     names(swing) <- names(swing.lm) <- names(swing.median) <- names(ret$truevote)
##     swingtab <- cbind(round(100 * ret$truevote, 1), round(100 * ret$trueseat, 1), swing)
##     rownames(swingtab) <- colnames(dat[, -1])
##     colnames(swingtab) <- c("Votes", "Seats", "Swing ratio")
##     print(swingtab)
##     ret$votemat <- votemat
##     ret$seatmat <- seatmat
##     ret$swing <- swing
##     ret$swing.lm <- swing.lm
##     ret$swing.median <- swing.median
##     ret$vs <- seatlist
##     ret$time <- Sys.time() - starttime
##     return(ret)
## }
## <environment: namespace:seatsvotes>
#
# Simulate swing ratios and plot results
elas <- swingratio(fit, sims=5000, graph = TRUE)
# rename object
names(elas)[which(names(elas)=="swing")] <- "swing.mean"
# add components to compute swing ratios (seats-votes elaticity)
elas$vdiff <- as.data.frame(t(t(elas$votemat) - colMeans(elas$votemat)))
elas$sdiff <- as.data.frame(t(t(elas$seatmat) - colMeans(elas$seatmat)))
# re-compute elasticity
elas$swing.sims <- elas$sdiff / elas$vdiff
# add number parties
elas$nParties <- ncol(elas$seatmat)
#
# obtain estimates with 95% ci --- looks wrong
elas.ci <- as.data.frame(matrix(NA, nrow = 3, ncol = 3)); colnames(elas.ci) <- c("lo","mu","hi"); rownames(elas.ci) <- c("pri","pan","left")
# pri
tmp <- as.data.frame(elas$vs[[1]])
elas.ci$lo[1] <- (tmp$lower[which(tmp$vote==.39)] - tmp$lower[which(tmp$vote==.37)]) / .02
elas.ci$mu[1] <- (tmp$mean [which(tmp$vote==.39)] - tmp$mean [which(tmp$vote==.37)]) / .02
elas.ci$hi[1] <- (tmp$upper[which(tmp$vote==.39)] - tmp$upper[which(tmp$vote==.37)]) / .02
# pan
tmp <- as.data.frame(elas$vs[[2]])
elas.ci$lo[2] <- (tmp$lower[which(tmp$vote==.28)] - tmp$lower[which(tmp$vote==.26)]) / .02
elas.ci$mu[2] <- (tmp$mean [which(tmp$vote==.28)] - tmp$mean [which(tmp$vote==.26)]) / .02
elas.ci$hi[2] <- (tmp$upper[which(tmp$vote==.28)] - tmp$upper[which(tmp$vote==.26)]) / .02
# left
tmp <- as.data.frame(elas$vs[[3]])
elas.ci$lo[3] <- (tmp$lower[which(tmp$vote==.29)] - tmp$lower[which(tmp$vote==.27)]) / .02
elas.ci$mu[3] <- (tmp$mean [which(tmp$vote==.29)] - tmp$mean [which(tmp$vote==.27)]) / .02
elas.ci$hi[3] <- (tmp$upper[which(tmp$vote==.29)] - tmp$upper[which(tmp$vote==.27)]) / .02
#
elas$swing.ci <- elas.ci
#
elas2012d0 <- elas
#
rm(dat,dat.pat,elas,elas.ci,fit,prep,tmp) # clean




# -----------------------------------------
# select 2012 votes in 2015 map
# -----------------------------------------
dat <- elec060912$df2012d3
rownames(dat) <- NULL
head(dat)
# round pri and pvem votes that were split in districts combining secciones with/without coalition
dat$pri <- round(dat$pri, 0)
dat$pric <- round(dat$pric, 0)
dat$pvem <- round(dat$pvem, 0)
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
dat <- dat[,c("pri","pan","pric","prdc","panal","pvem")]
# consider pri and pri-pvem the same (else pri "absent" from 1/3 district has lower elasticity)
dat$pri <- dat$pri + dat$pric; dat$pric <- NULL
# who won the seats?
table(apply(dat, 1, function(x) which.max(x)))
## # drop panal, didn't win seats
## dat <- dat[,-5]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
# vote shares
dat <- dat/tmp.efec
# add total vote in 1st column
dat <- cbind(tmp.efec, dat); colnames(dat)[1] <- "efec"; rm(tmp.efec)
# rename left coalition and green
colnames(dat)[which(colnames(dat)=="prdc")] <- "left"
colnames(dat)[which(colnames(dat)=="pvem")] <- "green"
head(dat)

# find patterns of party contestation, change data to lists
library(seatsvotes)
dat.pat <- findpatterns(dat)
# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually)
fit <- list()
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 1, nrep = 10, scatter = TRUE) 
fit[[2]] <- mvnmix( dat = dat.pat[[2]], components = 1, nrep = 10, scatter = TRUE)

# tweak function show.marginals formerly plotting marginals to get plot input
my.marginals.prep <- function (fit, numdraws = 10000){
    dat <- reconstruct(fit)
    if (numdraws > 0) {
        numdraws <- ceiling(numdraws/nrow(dat)) * nrow(dat)
        vsim <- NULL
        for (i in 1:(numdraws/nrow(dat))) {
            vsim <- rbind(vsim, sim.election(fit, dat))
        }
    }
    return(vsim)
}
environment(my.marginals.prep) <- asNamespace('seatsvotes')
# use tweaked function to verify fit of model to data: plot vote share histograms and party marginal densities
# (Linzer has show.marginals() function, but this offers control over output)
#par(mar = c(4, 3, 0.5, 0.5))
#mtext(side = 1, text = "Turnout (x100k)", line = 2.5, cex = 1.1)
#mtext(side = 2, text = "density", line = 2, cex = 0.8)
#layout(matrix(c(0,0,1,0,2,3,4,5,6), nrow = 3, ncol = 3, byrow = FALSE))
prep <- as.data.frame(my.marginals.prep(fit, numdraws=10000))
colnames(prep) <- colnames(dat)
head(prep)
#
hist(dat$efec/10000,         breaks = 30,              col = "gray90",                freq = FALSE, main = "", xlab = "Turnout (x10k)")
lines(density(prep$efec[prep$efec > 0], na.rm = TRUE, adjust = 0.7), lwd = 2)
#
hist(dat$pan,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pan")
lines(density(prep$pan, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$pri,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pri")
lines(density(prep$pri, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$left,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
lines(density(prep$left, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$green[dat$green>0],   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
lines(density(prep$green, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$panal[dat$panal>0], breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, panal")
lines(density(prep$panal, na.rm = TRUE, adjust = 0.6), lwd = 2)
# packaged plot
show.marginals(fit, numdraws=50000)

## # tweak/understand swinratio function
## elas.sims <- function (fit, sims = 1000, rule = plurality, graph = TRUE) 
## {
##     starttime <- Sys.time()
##     dat <- reconstruct(fit)
##     dat[is.na(dat)] <- 0
##     np <- ncol(dat)
##     votemat <- NULL
##     seatmat <- NULL
##     for (s in 1:sims) {
##         vsim <- sim.election(fit, dat)
##         vsim[is.na(vsim)] <- 0
##         partyvotes <- vsim[, 1] * vsim[, -1]
##         votemat <- rbind(votemat, colSums(partyvotes)/sum(partyvotes))
##         seatmat <- rbind(seatmat, rule(vsim[, -1]))
##     }
##     ret <- list()
##     truevotes <- dat[, 1] * dat[, -1]
##     ret$truevote <- colSums(truevotes)/sum(truevotes)
##     ret$trueseat <- rule(dat[, -1])
##     names(ret$trueseat) <- names(ret$truevote)
##     swing <- NULL
##     swing.lm <- NULL
##     seatlist <- list()
##     for (j in 1:(np - 1)) {
##         # hi and lo select sims +1% and -1% with some jitter
##         smean.hi <- mean(seatmat[(votemat[, j] < (ret$truevote[j] + 0.012))
##                                & (votemat[, j] > (ret$truevote[j] + 0.008)), j])
##         smean.lo <- mean(seatmat[(votemat[, j] > (ret$truevote[j] - 0.012))
##                                & (votemat[, j] < (ret$truevote[j] - 0.008)), j])
##         swing <- c(swing, round((smean.hi - smean.lo)/0.02, 2))
##         swing.lm <- c(swing.lm, round(coefficients(lm(seatmat[, j] ~ votemat[, j]))[2], 2))
##         vrange <- seq(min(floor(votemat[, j] * 100)), max(floor(votemat[, j] * 100)), 1)/100
##         seatlist[[j]] <- cbind(vrange, matrix(NA, nrow = length(vrange), 
##             ncol = 3))
##         colnames(seatlist[[j]]) <- c("vote", "mean", "lower", "upper")
##         for (i in 1:length(vrange)) {
##             sel <- seatmat[(votemat[, j] >= (vrange[i] - 0.002))
##                          & (votemat[, j] <  (vrange[i] + 0.002)), j]
##             # this is why parties with few seats are dropped... percentage points with fewer than 10 simulated obs dropped
##             if (length(sel) < 10) {
##                 seatlist[[j]][i, 2:4] <- NA
##             }
##             else {
##                 seatlist[[j]][i, 2] <- mean(sel)
##                 seatlist[[j]][i, 3:4] <- quantile(sel, probs = c(0.025, 
##                   0.975))
##             }
##         }
##     }
##     if (graph) {
##         par(mar = c(4.4, 4.1, 0.5, 0.5), mfrow = c(1, 1))
##         plot(ret$truevote[!is.na(swing)], ret$trueseat[!is.na(swing)], 
##             pch = 19, cex = 1.2, xlim = c(0, 0.7), ylim = c(0, 
##                 0.7), xlab = "Party vote share", ylab = "Party seat share", 
##             cex.lab = 1.5, xaxt = "n", yaxt = "n")
##         axis(1, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         for (j in 1:(np - 1)) {
##             if (!is.na(swing[j])) {
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 2], 
##                   lwd = 2.5)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 3], 
##                   lwd = 2, lty = 3)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 4], 
##                   lwd = 2, lty = 3)
##                 text(ret$truevote[j] + 0.01, ret$trueseat[j], 
##                   paste(colnames(dat)[j + 1], ": ", swing[j], 
##                     sep = ""), cex = 1.2, pos = 4)
##             }
##         }
##     }
##     vdiff <- as.data.frame(t(t(votemat) - colMeans(votemat)))
##     sdiff <- as.data.frame(t(t(seatmat) - colMeans(seatmat)))
##     swing.median <- round(apply(sdiff/vdiff, 2, median), 2)
##     names(seatlist) <- colnames(votemat) <- colnames(seatmat) <- names(ret$truevote)
##     names(swing) <- names(swing.lm) <- names(swing.median) <- names(ret$truevote)
##     swingtab <- cbind(round(100 * ret$truevote, 1), round(100 * ret$trueseat, 1), swing)
##     rownames(swingtab) <- colnames(dat[, -1])
##     colnames(swingtab) <- c("Votes", "Seats", "Swing ratio")
##     print(swingtab)
##     ret$votemat <- votemat
##     ret$seatmat <- seatmat
##     ret$swing <- swing
##     ret$swing.lm <- swing.lm
##     ret$swing.median <- swing.median
##     ret$vs <- seatlist
##     ret$time <- Sys.time() - starttime
##     return(ret)
## }
## <environment: namespace:seatsvotes>
#
# Simulate swing ratios and plot results
elas <- swingratio(fit, sims=5000, graph = TRUE)
elas$vs
elas$truevote
# rename object
names(elas)[which(names(elas)=="swing")] <- "swing.mean"
# add components to compute swing ratios (seats-votes elaticity)
elas$vdiff <- as.data.frame(t(t(elas$votemat) - colMeans(elas$votemat)))
elas$sdiff <- as.data.frame(t(t(elas$seatmat) - colMeans(elas$seatmat)))
# re-compute elasticity
elas$swing.sims <- elas$sdiff / elas$vdiff
# add number parties
elas$nParties <- ncol(elas$seatmat)
#
# obtain estimates with 95% ci
elas.ci <- as.data.frame(matrix(NA, nrow = 3, ncol = 3)); colnames(elas.ci) <- c("lo","mu","hi"); rownames(elas.ci) <- c("pri","pan","left")
# pri
tmp <- as.data.frame(elas$vs[[1]])
elas.ci$lo[1] <- (tmp$lower[which(tmp$vote==.4)] - tmp$lower[which(tmp$vote==.38)]) / .02
elas.ci$mu[1] <- (tmp$mean [which(tmp$vote==.4)] - tmp$mean [which(tmp$vote==.38)]) / .02
elas.ci$hi[1] <- (tmp$upper[which(tmp$vote==.4)] - tmp$upper[which(tmp$vote==.38)]) / .02
# pan
tmp <- as.data.frame(elas$vs[[2]])
elas.ci$lo[2] <- (tmp$lower[which(tmp$vote==.28)] - tmp$lower[which(tmp$vote==.26)]) / .02
elas.ci$mu[2] <- (tmp$mean [which(tmp$vote==.28)] - tmp$mean [which(tmp$vote==.26)]) / .02
elas.ci$hi[2] <- (tmp$upper[which(tmp$vote==.28)] - tmp$upper[which(tmp$vote==.26)]) / .02
# left
tmp <- as.data.frame(elas$vs[[3]])
elas.ci$lo[3] <- (tmp$lower[which(tmp$vote==.29)] - tmp$lower[which(tmp$vote==.27)]) / .02
elas.ci$mu[3] <- (tmp$mean [which(tmp$vote==.29)] - tmp$mean [which(tmp$vote==.27)]) / .02
elas.ci$hi[3] <- (tmp$upper[which(tmp$vote==.29)] - tmp$upper[which(tmp$vote==.27)]) / .02
#
elas$swing.ci <- elas.ci
#
elas2012d3 <- elas
#
rm(dat,dat.pat,elas,elas.ci,fit,prep,tmp) # clean




# -----------------------------------------
# select 2009 votes in 2006 map
# -----------------------------------------
dat <- elec060912$df2009d0
rownames(dat) <- NULL
head(dat)
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
#dat <- dat[,c("pri","pan","pric","prd","panal","ptc","pvem")]
dat <- dat[,c("pri","pan","pric","prd","ptc","pvem")] #drop panal
# consider pri and pri-pvem the same (else pri "absent" from 1/3 district has lower elasticity)
dat$pri <- dat$pri + dat$pric; dat$pric <- NULL
# who won the seats?
table(apply(dat, 1, function(x) which.max(x)))
## # drop panal, didn't win seats
## dat <- dat[,-5]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
# vote shares
dat <- dat/tmp.efec
# add total vote in 1st column
dat <- cbind(tmp.efec, dat); colnames(dat)[1] <- "efec"; rm(tmp.efec)
# rename left coalition and green
colnames(dat)[which(colnames(dat)=="pvem")] <- "green"
head(dat)

# find patterns of party contestation, change data to lists
library(seatsvotes)
dat.pat <- findpatterns(dat)
# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually)
fit <- list()
#pdf(file = paste(gd, "linzerLogVot2009-1.pdf", sep = ""))
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 2, nrep = 10, scatter = TRUE) 
#dev.off()
#pdf(file = paste(gd, "linzerLogVot2009-2.pdf", sep = ""))
fit[[2]] <- mvnmix( dat = dat.pat[[2]], components = 1, nrep = 10, scatter = TRUE)
#dev.off()

# tweak function show.marginals formerly plotting marginals to get plot input
my.marginals.prep <- function (fit, numdraws = 10000){
    dat <- reconstruct(fit)
    if (numdraws > 0) {
        numdraws <- ceiling(numdraws/nrow(dat)) * nrow(dat)
        vsim <- NULL
        for (i in 1:(numdraws/nrow(dat))) {
            vsim <- rbind(vsim, sim.election(fit, dat))
        }
    }
    return(vsim)
}
environment(my.marginals.prep) <- asNamespace('seatsvotes')
# use tweaked function to verify fit of model to data: plot vote share histograms and party marginal densities
# (Linzer has show.marginals() function, but this offers control over output)
#par(mar = c(4, 3, 0.5, 0.5))
#mtext(side = 1, text = "Turnout (x100k)", line = 2.5, cex = 1.1)
#mtext(side = 2, text = "density", line = 2, cex = 0.8)
#layout(matrix(c(0,0,1,0,2,3,4,5,6), nrow = 3, ncol = 3, byrow = FALSE))
prep <- as.data.frame(my.marginals.prep(fit, numdraws=10000))
colnames(prep) <- colnames(dat)
head(prep)
#
hist(dat$efec/10000,         breaks = 30,              col = "gray90",                freq = FALSE, main = "", xlab = "Turnout (x10k)")
lines(density(prep$efec[prep$efec > 0], na.rm = TRUE, adjust = 0.7), lwd = 2)
#
hist(dat$pan,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pan")
lines(density(prep$pan, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$pri,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pri")
lines(density(prep$pri, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$prd,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
lines(density(prep$prd, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$green[dat$green>0],   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
lines(density(prep$green, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$panal[dat$panal>0], breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, panal")
lines(density(prep$panal, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$ptc,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
lines(density(prep$ptc, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
# packaged plot
#pdf(file = paste(gd, "linzerMarg2009.pdf", sep = ""))
show.marginals(fit, numdraws=50000)
#dev.off()

## # tweak/understand swinratio function
## elas.sims <- function (fit, sims = 1000, rule = plurality, graph = TRUE) 
## {
##     starttime <- Sys.time()
##     dat <- reconstruct(fit)
##     dat[is.na(dat)] <- 0
##     np <- ncol(dat)
##     votemat <- NULL
##     seatmat <- NULL
##     for (s in 1:sims) {
##         vsim <- sim.election(fit, dat)
##         vsim[is.na(vsim)] <- 0
##         partyvotes <- vsim[, 1] * vsim[, -1]
##         votemat <- rbind(votemat, colSums(partyvotes)/sum(partyvotes))
##         seatmat <- rbind(seatmat, rule(vsim[, -1]))
##     }
##     ret <- list()
##     truevotes <- dat[, 1] * dat[, -1]
##     ret$truevote <- colSums(truevotes)/sum(truevotes)
##     ret$trueseat <- rule(dat[, -1])
##     names(ret$trueseat) <- names(ret$truevote)
##     swing <- NULL
##     swing.lm <- NULL
##     seatlist <- list()
##     for (j in 1:(np - 1)) {
##         # hi and lo select sims +1% and -1% with some jitter
##         smean.hi <- mean(seatmat[(votemat[, j] < (ret$truevote[j] + 0.012))
##                                & (votemat[, j] > (ret$truevote[j] + 0.008)), j])
##         smean.lo <- mean(seatmat[(votemat[, j] > (ret$truevote[j] - 0.012))
##                                & (votemat[, j] < (ret$truevote[j] - 0.008)), j])
##         swing <- c(swing, round((smean.hi - smean.lo)/0.02, 2))
##         swing.lm <- c(swing.lm, round(coefficients(lm(seatmat[, j] ~ votemat[, j]))[2], 2))
##         vrange <- seq(min(floor(votemat[, j] * 100)), max(floor(votemat[, j] * 100)), 1)/100
##         seatlist[[j]] <- cbind(vrange, matrix(NA, nrow = length(vrange), 
##             ncol = 3))
##         colnames(seatlist[[j]]) <- c("vote", "mean", "lower", "upper")
##         for (i in 1:length(vrange)) {
##             sel <- seatmat[(votemat[, j] >= (vrange[i] - 0.002))
##                          & (votemat[, j] <  (vrange[i] + 0.002)), j]
##             # this is why parties with few seats are dropped... percentage points with fewer than 10 simulated obs dropped
##             if (length(sel) < 10) {
##                 seatlist[[j]][i, 2:4] <- NA
##             }
##             else {
##                 seatlist[[j]][i, 2] <- mean(sel)
##                 seatlist[[j]][i, 3:4] <- quantile(sel, probs = c(0.025, 
##                   0.975))
##             }
##         }
##     }
##     if (graph) {
##         par(mar = c(4.4, 4.1, 0.5, 0.5), mfrow = c(1, 1))
##         plot(ret$truevote[!is.na(swing)], ret$trueseat[!is.na(swing)], 
##             pch = 19, cex = 1.2, xlim = c(0, 0.7), ylim = c(0, 
##                 0.7), xlab = "Party vote share", ylab = "Party seat share", 
##             cex.lab = 1.5, xaxt = "n", yaxt = "n")
##         axis(1, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         for (j in 1:(np - 1)) {
##             if (!is.na(swing[j])) {
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 2], 
##                   lwd = 2.5)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 3], 
##                   lwd = 2, lty = 3)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 4], 
##                   lwd = 2, lty = 3)
##                 text(ret$truevote[j] + 0.01, ret$trueseat[j], 
##                   paste(colnames(dat)[j + 1], ": ", swing[j], 
##                     sep = ""), cex = 1.2, pos = 4)
##             }
##         }
##     }
##     vdiff <- as.data.frame(t(t(votemat) - colMeans(votemat)))
##     sdiff <- as.data.frame(t(t(seatmat) - colMeans(seatmat)))
##     swing.median <- round(apply(sdiff/vdiff, 2, median), 2)
##     names(seatlist) <- colnames(votemat) <- colnames(seatmat) <- names(ret$truevote)
##     names(swing) <- names(swing.lm) <- names(swing.median) <- names(ret$truevote)
##     swingtab <- cbind(round(100 * ret$truevote, 1), round(100 * ret$trueseat, 1), swing)
##     rownames(swingtab) <- colnames(dat[, -1])
##     colnames(swingtab) <- c("Votes", "Seats", "Swing ratio")
##     print(swingtab)
##     ret$votemat <- votemat
##     ret$seatmat <- seatmat
##     ret$swing <- swing
##     ret$swing.lm <- swing.lm
##     ret$swing.median <- swing.median
##     ret$vs <- seatlist
##     ret$time <- Sys.time() - starttime
##     return(ret)
## }
## <environment: namespace:seatsvotes>
#
# Simulate swing ratios and plot results
elas <- swingratio(fit, sims=5000, graph = TRUE)
# rename object
names(elas)[which(names(elas)=="swing")] <- "swing.mean"
# add components to compute swing ratios (seats-votes elaticity)
elas$vdiff <- as.data.frame(t(t(elas$votemat) - colMeans(elas$votemat)))
elas$sdiff <- as.data.frame(t(t(elas$seatmat) - colMeans(elas$seatmat)))
# re-compute elasticity
elas$swing.sims <- elas$sdiff / elas$vdiff
# add number parties
elas$nParties <- ncol(elas$seatmat)
#
# obtain estimates with 95% ci
elas.ci <- as.data.frame(matrix(NA, nrow = 4, ncol = 3)); colnames(elas.ci) <- c("lo","mu","hi"); rownames(elas.ci) <- c("pri","pan","prd","ptc")
#elas$truevote
# pri
tmp <- as.data.frame(elas$vs[[1]])
elas.ci$lo[1] <- (tmp$lower[which(tmp$vote==.41)] - tmp$lower[which(tmp$vote==.39)]) / .02
elas.ci$mu[1] <- (tmp$mean [which(tmp$vote==.41)] - tmp$mean [which(tmp$vote==.39)]) / .02
elas.ci$hi[1] <- (tmp$upper[which(tmp$vote==.41)] - tmp$upper[which(tmp$vote==.39)]) / .02
# pan
tmp <- as.data.frame(elas$vs[[2]])
elas.ci$lo[2] <- (tmp$lower[which(tmp$vote==.3)] - tmp$lower[which(tmp$vote==.28)]) / .02
elas.ci$mu[2] <- (tmp$mean [which(tmp$vote==.3)] - tmp$mean [which(tmp$vote==.28)]) / .02
elas.ci$hi[2] <- (tmp$upper[which(tmp$vote==.3)] - tmp$upper[which(tmp$vote==.28)]) / .02
# left
tmp <- as.data.frame(elas$vs[[3]])
elas.ci$lo[3] <- (tmp$lower[which(tmp$vote==.15)] - tmp$lower[which(tmp$vote==.13)]) / .02
elas.ci$mu[3] <- (tmp$mean [which(tmp$vote==.15)] - tmp$mean [which(tmp$vote==.13)]) / .02
elas.ci$hi[3] <- (tmp$upper[which(tmp$vote==.15)] - tmp$upper[which(tmp$vote==.13)]) / .02
# can add ptc (and even green here, but not panal)
tmp <- as.data.frame(elas$vs[[5]])
elas.ci$lo[4] <- (tmp$lower[which(tmp$vote==.08)] - tmp$lower[which(tmp$vote==.06)]) / .02
elas.ci$mu[4] <- (tmp$mean [which(tmp$vote==.08)] - tmp$mean [which(tmp$vote==.06)]) / .02
elas.ci$hi[4] <- (tmp$upper[which(tmp$vote==.08)] - tmp$upper[which(tmp$vote==.06)]) / .02
#
elas$swing.ci <- elas.ci
#
elas2009d0 <- elas
#
rm(dat,dat.pat,elas,elas.ci,fit,prep,tmp) # clean




# -----------------------------------------
# select 2009 votes in 2015 map
# -----------------------------------------
dat <- elec060912$df2009d3
rownames(dat) <- NULL
head(dat)
#dat[141:150,]
#dat$shSecCoalPri
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
# round pri and pvem votes that were split in districts combining secciones with/without coalition
dat$pri <- round(dat$pri, 0)
dat$pric <- round(dat$pric, 0)
dat$pvem <- round(dat$pvem, 0)
#dat <- dat[,c("pri","pan","pric","prd","panal","ptc","pvem")]
dat <- dat[,c("pri","pan","pric","prd","ptc","pvem")]
# consider pri and pri-pvem the same (else pri "absent" from 1/3 district has lower elasticity)
dat$pri <- dat$pri + dat$pric; dat$pric <- NULL
# who won the seats?
table(apply(dat, 1, function(x) which.max(x)))
## # drop panal, didn't win seats
## dat <- dat[,-5]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
# vote shares
dat <- dat/tmp.efec
# add total vote in 1st column
dat <- cbind(tmp.efec, dat); colnames(dat)[1] <- "efec"; rm(tmp.efec)
# rename left coalition and green
colnames(dat)[which(colnames(dat)=="pvem")] <- "green"
head(dat)

# find patterns of party contestation, change data to lists
library(seatsvotes)
dat.pat <- findpatterns(dat)
# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually)
fit <- list()
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 2, nrep = 10, scatter = TRUE) 
fit[[2]] <- mvnmix( dat = dat.pat[[2]], components = 2, nrep = 10, scatter = TRUE)

# tweak function show.marginals formerly plotting marginals to get plot input
my.marginals.prep <- function (fit, numdraws = 10000){
    dat <- reconstruct(fit)
    if (numdraws > 0) {
        numdraws <- ceiling(numdraws/nrow(dat)) * nrow(dat)
        vsim <- NULL
        for (i in 1:(numdraws/nrow(dat))) {
            vsim <- rbind(vsim, sim.election(fit, dat))
        }
    }
    return(vsim)
}
environment(my.marginals.prep) <- asNamespace('seatsvotes')
# use tweaked function to verify fit of model to data: plot vote share histograms and party marginal densities
# (Linzer has show.marginals() function, but this offers control over output)
#par(mar = c(4, 3, 0.5, 0.5))
#mtext(side = 1, text = "Turnout (x100k)", line = 2.5, cex = 1.1)
#mtext(side = 2, text = "density", line = 2, cex = 0.8)
#layout(matrix(c(0,0,1,0,2,3,4,5,6), nrow = 3, ncol = 3, byrow = FALSE))
prep <- as.data.frame(my.marginals.prep(fit, numdraws=10000))
colnames(prep) <- colnames(dat)
head(prep)
#
hist(dat$efec/10000,         breaks = 30,              col = "gray90",                freq = FALSE, main = "", xlab = "Turnout (x10k)")
lines(density(prep$efec[prep$efec > 0], na.rm = TRUE, adjust = 0.7), lwd = 2)
#
hist(dat$pan,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pan")
lines(density(prep$pan, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$pri,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pri")
lines(density(prep$pri, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$prd,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
lines(density(prep$prd, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$green[dat$green>0],   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
lines(density(prep$green, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$panal[dat$panal>0], breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, panal")
lines(density(prep$panal, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$ptc,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
lines(density(prep$ptc, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
# packaged plot
show.marginals(fit, numdraws=50000)

## # tweak/understand swinratio function
## elas.sims <- function (fit, sims = 1000, rule = plurality, graph = TRUE) 
## {
##     starttime <- Sys.time()
##     dat <- reconstruct(fit)
##     dat[is.na(dat)] <- 0
##     np <- ncol(dat)
##     votemat <- NULL
##     seatmat <- NULL
##     for (s in 1:sims) {
##         vsim <- sim.election(fit, dat)
##         vsim[is.na(vsim)] <- 0
##         partyvotes <- vsim[, 1] * vsim[, -1]
##         votemat <- rbind(votemat, colSums(partyvotes)/sum(partyvotes))
##         seatmat <- rbind(seatmat, rule(vsim[, -1]))
##     }
##     ret <- list()
##     truevotes <- dat[, 1] * dat[, -1]
##     ret$truevote <- colSums(truevotes)/sum(truevotes)
##     ret$trueseat <- rule(dat[, -1])
##     names(ret$trueseat) <- names(ret$truevote)
##     swing <- NULL
##     swing.lm <- NULL
##     seatlist <- list()
##     for (j in 1:(np - 1)) {
##         # hi and lo select sims +1% and -1% with some jitter
##         smean.hi <- mean(seatmat[(votemat[, j] < (ret$truevote[j] + 0.012))
##                                & (votemat[, j] > (ret$truevote[j] + 0.008)), j])
##         smean.lo <- mean(seatmat[(votemat[, j] > (ret$truevote[j] - 0.012))
##                                & (votemat[, j] < (ret$truevote[j] - 0.008)), j])
##         swing <- c(swing, round((smean.hi - smean.lo)/0.02, 2))
##         swing.lm <- c(swing.lm, round(coefficients(lm(seatmat[, j] ~ votemat[, j]))[2], 2))
##         vrange <- seq(min(floor(votemat[, j] * 100)), max(floor(votemat[, j] * 100)), 1)/100
##         seatlist[[j]] <- cbind(vrange, matrix(NA, nrow = length(vrange), 
##             ncol = 3))
##         colnames(seatlist[[j]]) <- c("vote", "mean", "lower", "upper")
##         for (i in 1:length(vrange)) {
##             sel <- seatmat[(votemat[, j] >= (vrange[i] - 0.002))
##                          & (votemat[, j] <  (vrange[i] + 0.002)), j]
##             # this is why parties with few seats are dropped... percentage points with fewer than 10 simulated obs dropped
##             if (length(sel) < 10) {
##                 seatlist[[j]][i, 2:4] <- NA
##             }
##             else {
##                 seatlist[[j]][i, 2] <- mean(sel)
##                 seatlist[[j]][i, 3:4] <- quantile(sel, probs = c(0.025, 
##                   0.975))
##             }
##         }
##     }
##     if (graph) {
##         par(mar = c(4.4, 4.1, 0.5, 0.5), mfrow = c(1, 1))
##         plot(ret$truevote[!is.na(swing)], ret$trueseat[!is.na(swing)], 
##             pch = 19, cex = 1.2, xlim = c(0, 0.7), ylim = c(0, 
##                 0.7), xlab = "Party vote share", ylab = "Party seat share", 
##             cex.lab = 1.5, xaxt = "n", yaxt = "n")
##         axis(1, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         for (j in 1:(np - 1)) {
##             if (!is.na(swing[j])) {
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 2], 
##                   lwd = 2.5)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 3], 
##                   lwd = 2, lty = 3)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 4], 
##                   lwd = 2, lty = 3)
##                 text(ret$truevote[j] + 0.01, ret$trueseat[j], 
##                   paste(colnames(dat)[j + 1], ": ", swing[j], 
##                     sep = ""), cex = 1.2, pos = 4)
##             }
##         }
##     }
##     vdiff <- as.data.frame(t(t(votemat) - colMeans(votemat)))
##     sdiff <- as.data.frame(t(t(seatmat) - colMeans(seatmat)))
##     swing.median <- round(apply(sdiff/vdiff, 2, median), 2)
##     names(seatlist) <- colnames(votemat) <- colnames(seatmat) <- names(ret$truevote)
##     names(swing) <- names(swing.lm) <- names(swing.median) <- names(ret$truevote)
##     swingtab <- cbind(round(100 * ret$truevote, 1), round(100 * ret$trueseat, 1), swing)
##     rownames(swingtab) <- colnames(dat[, -1])
##     colnames(swingtab) <- c("Votes", "Seats", "Swing ratio")
##     print(swingtab)
##     ret$votemat <- votemat
##     ret$seatmat <- seatmat
##     ret$swing <- swing
##     ret$swing.lm <- swing.lm
##     ret$swing.median <- swing.median
##     ret$vs <- seatlist
##     ret$time <- Sys.time() - starttime
##     return(ret)
## }
## <environment: namespace:seatsvotes>
#
# Simulate swing ratios and plot results
elas <- swingratio(fit, sims=5000, graph = TRUE)
# rename object
names(elas)[which(names(elas)=="swing")] <- "swing.mean"
# add components to compute swing ratios (seats-votes elaticity)
elas$vdiff <- as.data.frame(t(t(elas$votemat) - colMeans(elas$votemat)))
elas$sdiff <- as.data.frame(t(t(elas$seatmat) - colMeans(elas$seatmat)))
# re-compute elasticity
elas$swing.sims <- elas$sdiff / elas$vdiff
# add number parties
elas$nParties <- ncol(elas$seatmat)
#
# obtain estimates with 95% ci
elas.ci <- as.data.frame(matrix(NA, nrow = 3, ncol = 3)); colnames(elas.ci) <- c("lo","mu","hi"); rownames(elas.ci) <- c("pri","pan","prd")
elas$truevote
# pri
tmp <- as.data.frame(elas$vs[[1]])
elas.ci$lo[1] <- (tmp$lower[which(tmp$vote==.41)] - tmp$lower[which(tmp$vote==.39)]) / .02
elas.ci$mu[1] <- (tmp$mean [which(tmp$vote==.41)] - tmp$mean [which(tmp$vote==.39)]) / .02
elas.ci$hi[1] <- (tmp$upper[which(tmp$vote==.41)] - tmp$upper[which(tmp$vote==.39)]) / .02
# pan
tmp <- as.data.frame(elas$vs[[2]])
elas.ci$lo[2] <- (tmp$lower[which(tmp$vote==.3)] - tmp$lower[which(tmp$vote==.28)]) / .02
elas.ci$mu[2] <- (tmp$mean [which(tmp$vote==.3)] - tmp$mean [which(tmp$vote==.28)]) / .02
elas.ci$hi[2] <- (tmp$upper[which(tmp$vote==.3)] - tmp$upper[which(tmp$vote==.28)]) / .02
# left
tmp <- as.data.frame(elas$vs[[3]])
elas.ci$lo[3] <- (tmp$lower[which(tmp$vote==.15)] - tmp$lower[which(tmp$vote==.13)]) / .02
elas.ci$mu[3] <- (tmp$mean [which(tmp$vote==.15)] - tmp$mean [which(tmp$vote==.13)]) / .02
elas.ci$hi[3] <- (tmp$upper[which(tmp$vote==.15)] - tmp$upper[which(tmp$vote==.13)]) / .02
#
elas$swing.ci <- elas.ci
#
elas2009d3 <- elas
#
rm(dat,dat.pat,elas,elas.ci,fit,prep,tmp) # clean






# -----------------------------------------
# select 2006 votes in 2006 map
# -----------------------------------------
dat <- elec060912$df2006d0
rownames(dat) <- NULL
head(dat)
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
dat <- dat[,c("pric","pan","prdc","panal","asdc")]
# who won the seats?
table(apply(dat, 1, function(x) which.max(x)))
# drop panal, asdc, didn't win seats
dat <- dat[,-4:-5]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
# vote shares
dat <- dat/tmp.efec
# add total vote in 1st column
dat <- cbind(tmp.efec, dat); colnames(dat)[1] <- "efec"; rm(tmp.efec)
# rename pric to pri, left coalition
colnames(dat)[which(colnames(dat)=="pric")] <- "pri"
colnames(dat)[which(colnames(dat)=="prdc")] <- "left"
head(dat)

# find patterns of party contestation, change data to lists
library(seatsvotes)
dat.pat <- findpatterns(dat)
# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually)
fit <- list()
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 2, nrep = 10, scatter = TRUE) 

# tweak function show.marginals formerly plotting marginals to get plot input
my.marginals.prep <- function (fit, numdraws = 10000){
    dat <- reconstruct(fit)
    if (numdraws > 0) {
        numdraws <- ceiling(numdraws/nrow(dat)) * nrow(dat)
        vsim <- NULL
        for (i in 1:(numdraws/nrow(dat))) {
            vsim <- rbind(vsim, sim.election(fit, dat))
        }
    }
    return(vsim)
}
environment(my.marginals.prep) <- asNamespace('seatsvotes')
# use tweaked function to verify fit of model to data: plot vote share histograms and party marginal densities
# (Linzer has show.marginals() function, but this offers control over output)
#par(mar = c(4, 3, 0.5, 0.5))
#mtext(side = 1, text = "Turnout (x100k)", line = 2.5, cex = 1.1)
#mtext(side = 2, text = "density", line = 2, cex = 0.8)
#layout(matrix(c(0,0,1,0,2,3,4,5,6), nrow = 3, ncol = 3, byrow = FALSE))
prep <- as.data.frame(my.marginals.prep(fit, numdraws=10000))
colnames(prep) <- colnames(dat)
head(prep)
#
hist(dat$efec/10000,         breaks = 30,              col = "gray90",                freq = FALSE, main = "", xlab = "Turnout (x10k)")
lines(density(prep$efec[prep$efec > 0], na.rm = TRUE, adjust = 0.7), lwd = 2)
#
hist(dat$pan,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pan")
lines(density(prep$pan, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$pri,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pri")
lines(density(prep$pri, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$left,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
lines(density(prep$left, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$asdc,   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
lines(density(prep$asdc, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
# packaged plot
show.marginals(fit, numdraws=50000)

## # tweak/understand swinratio function
## elas.sims <- function (fit, sims = 1000, rule = plurality, graph = TRUE) 
## {
##     starttime <- Sys.time()
##     dat <- reconstruct(fit)
##     dat[is.na(dat)] <- 0
##     np <- ncol(dat)
##     votemat <- NULL
##     seatmat <- NULL
##     for (s in 1:sims) {
##         vsim <- sim.election(fit, dat)
##         vsim[is.na(vsim)] <- 0
##         partyvotes <- vsim[, 1] * vsim[, -1]
##         votemat <- rbind(votemat, colSums(partyvotes)/sum(partyvotes))
##         seatmat <- rbind(seatmat, rule(vsim[, -1]))
##     }
##     ret <- list()
##     truevotes <- dat[, 1] * dat[, -1]
##     ret$truevote <- colSums(truevotes)/sum(truevotes)
##     ret$trueseat <- rule(dat[, -1])
##     names(ret$trueseat) <- names(ret$truevote)
##     swing <- NULL
##     swing.lm <- NULL
##     seatlist <- list()
##     for (j in 1:(np - 1)) {
##         # hi and lo select sims +1% and -1% with some jitter
##         smean.hi <- mean(seatmat[(votemat[, j] < (ret$truevote[j] + 0.012))
##                                & (votemat[, j] > (ret$truevote[j] + 0.008)), j])
##         smean.lo <- mean(seatmat[(votemat[, j] > (ret$truevote[j] - 0.012))
##                                & (votemat[, j] < (ret$truevote[j] - 0.008)), j])
##         swing <- c(swing, round((smean.hi - smean.lo)/0.02, 2))
##         swing.lm <- c(swing.lm, round(coefficients(lm(seatmat[, j] ~ votemat[, j]))[2], 2))
##         vrange <- seq(min(floor(votemat[, j] * 100)), max(floor(votemat[, j] * 100)), 1)/100
##         seatlist[[j]] <- cbind(vrange, matrix(NA, nrow = length(vrange), 
##             ncol = 3))
##         colnames(seatlist[[j]]) <- c("vote", "mean", "lower", "upper")
##         for (i in 1:length(vrange)) {
##             sel <- seatmat[(votemat[, j] >= (vrange[i] - 0.002))
##                          & (votemat[, j] <  (vrange[i] + 0.002)), j]
##             # this is why parties with few seats are dropped... percentage points with fewer than 10 simulated obs dropped
##             if (length(sel) < 10) {
##                 seatlist[[j]][i, 2:4] <- NA
##             }
##             else {
##                 seatlist[[j]][i, 2] <- mean(sel)
##                 seatlist[[j]][i, 3:4] <- quantile(sel, probs = c(0.025, 
##                   0.975))
##             }
##         }
##     }
##     if (graph) {
##         par(mar = c(4.4, 4.1, 0.5, 0.5), mfrow = c(1, 1))
##         plot(ret$truevote[!is.na(swing)], ret$trueseat[!is.na(swing)], 
##             pch = 19, cex = 1.2, xlim = c(0, 0.7), ylim = c(0, 
##                 0.7), xlab = "Party vote share", ylab = "Party seat share", 
##             cex.lab = 1.5, xaxt = "n", yaxt = "n")
##         axis(1, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         for (j in 1:(np - 1)) {
##             if (!is.na(swing[j])) {
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 2], 
##                   lwd = 2.5)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 3], 
##                   lwd = 2, lty = 3)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 4], 
##                   lwd = 2, lty = 3)
##                 text(ret$truevote[j] + 0.01, ret$trueseat[j], 
##                   paste(colnames(dat)[j + 1], ": ", swing[j], 
##                     sep = ""), cex = 1.2, pos = 4)
##             }
##         }
##     }
##     vdiff <- as.data.frame(t(t(votemat) - colMeans(votemat)))
##     sdiff <- as.data.frame(t(t(seatmat) - colMeans(seatmat)))
##     swing.median <- round(apply(sdiff/vdiff, 2, median), 2)
##     names(seatlist) <- colnames(votemat) <- colnames(seatmat) <- names(ret$truevote)
##     names(swing) <- names(swing.lm) <- names(swing.median) <- names(ret$truevote)
##     swingtab <- cbind(round(100 * ret$truevote, 1), round(100 * ret$trueseat, 1), swing)
##     rownames(swingtab) <- colnames(dat[, -1])
##     colnames(swingtab) <- c("Votes", "Seats", "Swing ratio")
##     print(swingtab)
##     ret$votemat <- votemat
##     ret$seatmat <- seatmat
##     ret$swing <- swing
##     ret$swing.lm <- swing.lm
##     ret$swing.median <- swing.median
##     ret$vs <- seatlist
##     ret$time <- Sys.time() - starttime
##     return(ret)
## }
## <environment: namespace:seatsvotes>
#
# Simulate swing ratios and plot results
elas <- swingratio(fit, sims=5000, graph = TRUE)
# rename object
names(elas)[which(names(elas)=="swing")] <- "swing.mean"
# add components to compute swing ratios (seats-votes elaticity)
elas$vdiff <- as.data.frame(t(t(elas$votemat) - colMeans(elas$votemat)))
elas$sdiff <- as.data.frame(t(t(elas$seatmat) - colMeans(elas$seatmat)))
# re-compute elasticity
elas$swing.sims <- elas$sdiff / elas$vdiff
# add number parties
elas$nParties <- ncol(elas$seatmat)
#
# obtain estimates with 95% ci
elas.ci <- as.data.frame(matrix(NA, nrow = 3, ncol = 3)); colnames(elas.ci) <- c("lo","mu","hi"); rownames(elas.ci) <- c("pri","pan","left")
#elas$truevote
# pri
tmp <- as.data.frame(elas$vs[[1]])
elas.ci$lo[1] <- (tmp$lower[which(tmp$vote==.31)] - tmp$lower[which(tmp$vote==.29)]) / .02
elas.ci$mu[1] <- (tmp$mean [which(tmp$vote==.31)] - tmp$mean [which(tmp$vote==.29)]) / .02
elas.ci$hi[1] <- (tmp$upper[which(tmp$vote==.31)] - tmp$upper[which(tmp$vote==.29)]) / .02
# pan
tmp <- as.data.frame(elas$vs[[2]])
elas.ci$lo[2] <- (tmp$lower[which(tmp$vote==.38)] - tmp$lower[which(tmp$vote==.36)]) / .02
elas.ci$mu[2] <- (tmp$mean [which(tmp$vote==.38)] - tmp$mean [which(tmp$vote==.36)]) / .02
elas.ci$hi[2] <- (tmp$upper[which(tmp$vote==.38)] - tmp$upper[which(tmp$vote==.36)]) / .02
# left
tmp <- as.data.frame(elas$vs[[3]])
elas.ci$lo[3] <- (tmp$lower[which(tmp$vote==.34)] - tmp$lower[which(tmp$vote==.32)]) / .02
elas.ci$mu[3] <- (tmp$mean [which(tmp$vote==.34)] - tmp$mean [which(tmp$vote==.32)]) / .02
elas.ci$hi[3] <- (tmp$upper[which(tmp$vote==.34)] - tmp$upper[which(tmp$vote==.32)]) / .02
#
elas$swing.ci <- elas.ci
#
elas2006d0 <- elas
#
rm(dat,dat.pat,elas,elas.ci,fit,prep,tmp) # clean




# -----------------------------------------
# select 2006 votes in 2015 map
# -----------------------------------------
dat <- elec060912$df2006d3
rownames(dat) <- NULL
head(dat)
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
dat <- dat[,c("pric","pan","prdc","panal","asdc")]
# who won the seats?
table(apply(dat, 1, function(x) which.max(x)))
# drop panal, asdc, didn't win seats
dat <- dat[,-4:-5]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
# vote shares
dat <- dat/tmp.efec
# add total vote in 1st column
dat <- cbind(tmp.efec, dat); colnames(dat)[1] <- "efec"; rm(tmp.efec)
# rename pric to pri, left coalition
colnames(dat)[which(colnames(dat)=="pric")] <- "pri"
colnames(dat)[which(colnames(dat)=="prdc")] <- "left"
head(dat)

# find patterns of party contestation, change data to lists
library(seatsvotes)
dat.pat <- findpatterns(dat)
# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually)
fit <- list()
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 2, nrep = 10, scatter = TRUE) 

# tweak function show.marginals formerly plotting marginals to get plot input
my.marginals.prep <- function (fit, numdraws = 10000){
    dat <- reconstruct(fit)
    if (numdraws > 0) {
        numdraws <- ceiling(numdraws/nrow(dat)) * nrow(dat)
        vsim <- NULL
        for (i in 1:(numdraws/nrow(dat))) {
            vsim <- rbind(vsim, sim.election(fit, dat))
        }
    }
    return(vsim)
}
environment(my.marginals.prep) <- asNamespace('seatsvotes')
# use tweaked function to verify fit of model to data: plot vote share histograms and party marginal densities
# (Linzer has show.marginals() function, but this offers control over output)
#par(mar = c(4, 3, 0.5, 0.5))
#mtext(side = 1, text = "Turnout (x100k)", line = 2.5, cex = 1.1)
#mtext(side = 2, text = "density", line = 2, cex = 0.8)
#layout(matrix(c(0,0,1,0,2,3,4,5,6), nrow = 3, ncol = 3, byrow = FALSE))
prep <- as.data.frame(my.marginals.prep(fit, numdraws=10000))
colnames(prep) <- colnames(dat)
head(prep)
#
hist(dat$efec/10000,         breaks = 30,              col = "gray90",                freq = FALSE, main = "", xlab = "Turnout (x10k)")
lines(density(prep$efec[prep$efec > 0], na.rm = TRUE, adjust = 0.7), lwd = 2)
#
hist(dat$pan,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pan")
lines(density(prep$pan, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$pri,                breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, pri")
lines(density(prep$pri, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$left,               breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, left")
lines(density(prep$left, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$panal,   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
lines(density(prep$panal, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
hist(dat$asdc,   breaks = seq(0, 1, 0.02), col = "gray90", xlim = c(0,1), freq = FALSE, main = "", xlab = "Vote share, green")
lines(density(prep$asdc, na.rm = TRUE, adjust = 0.6), lwd = 2)
#
# packaged plot
show.marginals(fit, numdraws=50000)

## # tweak/understand swinratio function
## elas.sims <- function (fit, sims = 1000, rule = plurality, graph = TRUE) 
## {
##     starttime <- Sys.time()
##     dat <- reconstruct(fit)
##     dat[is.na(dat)] <- 0
##     np <- ncol(dat)
##     votemat <- NULL
##     seatmat <- NULL
##     for (s in 1:sims) {
##         vsim <- sim.election(fit, dat)
##         vsim[is.na(vsim)] <- 0
##         partyvotes <- vsim[, 1] * vsim[, -1]
##         votemat <- rbind(votemat, colSums(partyvotes)/sum(partyvotes))
##         seatmat <- rbind(seatmat, rule(vsim[, -1]))
##     }
##     ret <- list()
##     truevotes <- dat[, 1] * dat[, -1]
##     ret$truevote <- colSums(truevotes)/sum(truevotes)
##     ret$trueseat <- rule(dat[, -1])
##     names(ret$trueseat) <- names(ret$truevote)
##     swing <- NULL
##     swing.lm <- NULL
##     seatlist <- list()
##     for (j in 1:(np - 1)) {
##         # hi and lo select sims +1% and -1% with some jitter
##         smean.hi <- mean(seatmat[(votemat[, j] < (ret$truevote[j] + 0.012))
##                                & (votemat[, j] > (ret$truevote[j] + 0.008)), j])
##         smean.lo <- mean(seatmat[(votemat[, j] > (ret$truevote[j] - 0.012))
##                                & (votemat[, j] < (ret$truevote[j] - 0.008)), j])
##         swing <- c(swing, round((smean.hi - smean.lo)/0.02, 2))
##         swing.lm <- c(swing.lm, round(coefficients(lm(seatmat[, j] ~ votemat[, j]))[2], 2))
##         vrange <- seq(min(floor(votemat[, j] * 100)), max(floor(votemat[, j] * 100)), 1)/100
##         seatlist[[j]] <- cbind(vrange, matrix(NA, nrow = length(vrange), 
##             ncol = 3))
##         colnames(seatlist[[j]]) <- c("vote", "mean", "lower", "upper")
##         for (i in 1:length(vrange)) {
##             sel <- seatmat[(votemat[, j] >= (vrange[i] - 0.002))
##                          & (votemat[, j] <  (vrange[i] + 0.002)), j]
##             # this is why parties with few seats are dropped... percentage points with fewer than 10 simulated obs dropped
##             if (length(sel) < 10) {
##                 seatlist[[j]][i, 2:4] <- NA
##             }
##             else {
##                 seatlist[[j]][i, 2] <- mean(sel)
##                 seatlist[[j]][i, 3:4] <- quantile(sel, probs = c(0.025, 
##                   0.975))
##             }
##         }
##     }
##     if (graph) {
##         par(mar = c(4.4, 4.1, 0.5, 0.5), mfrow = c(1, 1))
##         plot(ret$truevote[!is.na(swing)], ret$trueseat[!is.na(swing)], 
##             pch = 19, cex = 1.2, xlim = c(0, 0.7), ylim = c(0, 
##                 0.7), xlab = "Party vote share", ylab = "Party seat share", 
##             cex.lab = 1.5, xaxt = "n", yaxt = "n")
##         axis(1, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
##         for (j in 1:(np - 1)) {
##             if (!is.na(swing[j])) {
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 2], 
##                   lwd = 2.5)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 3], 
##                   lwd = 2, lty = 3)
##                 lines(seatlist[[j]][, 1], seatlist[[j]][, 4], 
##                   lwd = 2, lty = 3)
##                 text(ret$truevote[j] + 0.01, ret$trueseat[j], 
##                   paste(colnames(dat)[j + 1], ": ", swing[j], 
##                     sep = ""), cex = 1.2, pos = 4)
##             }
##         }
##     }
##     vdiff <- as.data.frame(t(t(votemat) - colMeans(votemat)))
##     sdiff <- as.data.frame(t(t(seatmat) - colMeans(seatmat)))
##     swing.median <- round(apply(sdiff/vdiff, 2, median), 2)
##     names(seatlist) <- colnames(votemat) <- colnames(seatmat) <- names(ret$truevote)
##     names(swing) <- names(swing.lm) <- names(swing.median) <- names(ret$truevote)
##     swingtab <- cbind(round(100 * ret$truevote, 1), round(100 * ret$trueseat, 1), swing)
##     rownames(swingtab) <- colnames(dat[, -1])
##     colnames(swingtab) <- c("Votes", "Seats", "Swing ratio")
##     print(swingtab)
##     ret$votemat <- votemat
##     ret$seatmat <- seatmat
##     ret$swing <- swing
##     ret$swing.lm <- swing.lm
##     ret$swing.median <- swing.median
##     ret$vs <- seatlist
##     ret$time <- Sys.time() - starttime
##     return(ret)
## }
## <environment: namespace:seatsvotes>
#
# Simulate swing ratios and plot results
elas <- swingratio(fit, sims=5000, graph = TRUE)
# rename object
names(elas)[which(names(elas)=="swing")] <- "swing.mean"
# add components to compute swing ratios (seats-votes elaticity)
elas$vdiff <- as.data.frame(t(t(elas$votemat) - colMeans(elas$votemat)))
elas$sdiff <- as.data.frame(t(t(elas$seatmat) - colMeans(elas$seatmat)))
# re-compute elasticity
elas$swing.sims <- elas$sdiff / elas$vdiff
# add number parties
elas$nParties <- ncol(elas$seatmat)
#
# obtain estimates with 95% ci
elas.ci <- as.data.frame(matrix(NA, nrow = 3, ncol = 3)); colnames(elas.ci) <- c("lo","mu","hi"); rownames(elas.ci) <- c("pri","pan","left")
#elas$truevote
# pri
tmp <- as.data.frame(elas$vs[[1]])
elas.ci$lo[1] <- (tmp$lower[which(tmp$vote==.31)] - tmp$lower[which(tmp$vote==.29)]) / .02
elas.ci$mu[1] <- (tmp$mean [which(tmp$vote==.31)] - tmp$mean [which(tmp$vote==.29)]) / .02
elas.ci$hi[1] <- (tmp$upper[which(tmp$vote==.31)] - tmp$upper[which(tmp$vote==.29)]) / .02
# pan
tmp <- as.data.frame(elas$vs[[2]])
elas.ci$lo[2] <- (tmp$lower[which(tmp$vote==.38)] - tmp$lower[which(tmp$vote==.36)]) / .02
elas.ci$mu[2] <- (tmp$mean [which(tmp$vote==.38)] - tmp$mean [which(tmp$vote==.36)]) / .02
elas.ci$hi[2] <- (tmp$upper[which(tmp$vote==.38)] - tmp$upper[which(tmp$vote==.36)]) / .02
# left
tmp <- as.data.frame(elas$vs[[3]])
elas.ci$lo[3] <- (tmp$lower[which(tmp$vote==.34)] - tmp$lower[which(tmp$vote==.32)]) / .02
elas.ci$mu[3] <- (tmp$mean [which(tmp$vote==.34)] - tmp$mean [which(tmp$vote==.32)]) / .02
elas.ci$hi[3] <- (tmp$upper[which(tmp$vote==.34)] - tmp$upper[which(tmp$vote==.32)]) / .02
#
elas$swing.ci <- elas.ci
#
elas2006d3 <- elas
#
rm(dat,dat.pat,elas,elas.ci,fit,prep,tmp) # clean

ls()

## USE SEATS/VOTES REGRESSIONS TO SUMMARIZE ELASTICITIES (WITH STD ERRORS)
## POOLS d0 AND d3 WITH DUMMY AND INTERACTION TO TEST CHANGE IN ELASTICITY
# 2006
elas.pool.res.2006 <- sapply(c("pri","pan","left"),function(x) NULL) # initialize list to receive party regression results
tmp <- as.data.frame(elas2006d0$seatmat)
seat <- tmp
tmp <- as.data.frame(elas2006d3$seatmat)
seat <- rbind(seat, tmp)
tmp <- as.data.frame(elas2006d0$votemat); tmp$dd3 <- 0
vote <- tmp
tmp <- as.data.frame(elas2006d3$votemat); tmp$dd3 <- 1
vote <- rbind(vote, tmp)
dd3 <- vote[,which(colnames(vote)=="dd3")]; vote <- vote[,-which(colnames(vote)=="dd3")]
votexdd3 <- vote * dd3
#
i <- 1 # pri
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("pri 2006"); summary(tmp)$coefficients
elas.pool.res.2006[[i]] <- summary(tmp)
#
i <- 2 # pan
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("pan 2006"); summary(tmp)$coefficients
elas.pool.res.2006[[i]] <- summary(tmp)
#
i <- 3 # prd
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("left 2006"); summary(tmp)$coefficients
elas.pool.res.2006[[i]] <- summary(tmp)
#
# 2009
elas.pool.res.2009 <- sapply(c("pri","pan","left"),function(x) NULL) # initialize list to receive party regression results
tmp <- as.data.frame(elas2009d0$seatmat)
seat <- tmp
tmp <- as.data.frame(elas2009d3$seatmat)
seat <- rbind(seat, tmp)
tmp <- as.data.frame(elas2009d0$votemat); tmp$dd3 <- 0
vote <- tmp
tmp <- as.data.frame(elas2009d3$votemat); tmp$dd3 <- 1
vote <- rbind(vote, tmp)
dd3 <- vote[,which(colnames(vote)=="dd3")]; vote <- vote[,-which(colnames(vote)=="dd3")]
votexdd3 <- vote * dd3
#
i <- 1 # pri
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("pri 2009"); summary(tmp)$coefficients
elas.pool.res.2009[[i]] <- summary(tmp)
#
i <- 2 # pan
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("pan 2009"); summary(tmp)$coefficients
elas.pool.res.2009[[i]] <- summary(tmp)
#
i <- 3 # prd
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("left 2009"); summary(tmp)$coefficients
elas.pool.res.2009[[i]] <- summary(tmp)
#
# 2012
elas.pool.res.2012 <- sapply(c("pri","pan","left"),function(x) NULL) # initialize list to receive party regression results
tmp <- as.data.frame(elas2012d0$seatmat)
seat <- tmp
tmp <- as.data.frame(elas2012d3$seatmat)
seat <- rbind(seat, tmp)
tmp <- as.data.frame(elas2012d0$votemat); tmp$dd3 <- 0
vote <- tmp
tmp <- as.data.frame(elas2012d3$votemat); tmp$dd3 <- 1
vote <- rbind(vote, tmp)
dd3 <- vote[,which(colnames(vote)=="dd3")]; vote <- vote[,-which(colnames(vote)=="dd3")]
votexdd3 <- vote * dd3
#
i <- 1 # pri
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("pri 2012"); summary(tmp)$coefficients
elas.pool.res.2012[[i]] <- summary(tmp)
#
i <- 2 # pan
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("pan 2012"); summary(tmp)$coefficients
elas.pool.res.2012[[i]] <- summary(tmp)
#
i <- 3 # prd
tmp <- lm(seat[,i] ~ vote[,i] + dd3 + votexdd3[,i])
print("left 2012"); summary(tmp)$coefficients
elas.pool.res.2012[[i]] <- summary(tmp)


# DO THIS ONLY AFTER ALL ELAS OBJECTS CREATED --- SEE HOW 1% INCREASE IN PTY AFFECTS OTHER PTIES
# Scatterplot matrix of simulated national-level vote and seat shares.
#   Note, loading package "car" conflicts with package "ellipse" --
#   will need to re-start R to proceed with other countries.
library(car)
# 2012
sim.vm <- as.data.frame(elas2012d0$votemat)
scatterplotMatrix(sim.vm[1:1000,],pch=19,diagonal="none",cex=0.4,lty=2,
#                  var.labels=c("\n\n\nPRI","\n\n\nPAN",
#                               "\n\n\nLeft","\n\n\nPVEM","\n\n\nPANAL"),
                  smooth=FALSE,cex.axis=1.5,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)

sim.sm <- as.data.frame(elas2012d0$seatmat)
scatterplotMatrix(sim.sm[1:1000,],pch=19,diagonal="none",cex=0.4,lty=2,
#                  var.labels=c("\n\n\nPRI","\n\n\nPAN",
#                               "\n\n\nLeft","\n\n\nPVEM","\n\n\nPANAL"),
                  smooth=FALSE,cex.axis=1.5,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)

# 2009
sim.vm <- as.data.frame(elas2009d0$votemat)
#pdf(file = paste(gd, "linzerVoteSims2009.pdf", sep = ""))
scatterplotMatrix(sim.vm[1:1000,],pch=19,diagonal="none",cex=0.4,lty=2,
                  var.labels=c("\n\nPRI","\n\nPAN",
                               "\n\nPRD",
                               "\n\nPT-C","\n\nPVEM"),
                  smooth=FALSE,cex.axis=1.5,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)
#dev.off()    

sim.sm <- as.data.frame(elas2009d0$seatmat)
#pdf(file = paste(gd, "linzerSeatSims2009.pdf", sep = ""))
scatterplotMatrix(sim.sm[1:1000,],pch=19,diagonal="none",cex=0.4,lty=2,
                  var.labels=c("\n\nPRI","\n\nPAN",
                               "\n\nPRD",
                               "\n\nPT-C","\n\nPVEM"),
                  smooth=FALSE,cex.axis=1.5,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)
#dev.off()    


# 2006
sim.vm <- as.data.frame(elas2006d0$votemat)
scatterplotMatrix(sim.vm[1:1000,],pch=19,diagonal="none",cex=0.4,lty=2,
#                  var.labels=c("\n\n\n\nPRI","\n\n\n\nPAN",
#                               "\n\n\n\nLeft"),
                  smooth=FALSE,cex.axis=1.5,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)

sim.sm <- as.data.frame(elas2006d0$seatmat)
scatterplotMatrix(sim.sm[1:1000,],pch=19,diagonal="none",cex=0.4,lty=2,
#                  var.labels=c("\n\n\n\nPRI","\n\n\n\nPAN",
#                               "\n\n\n\nLeft"),
                  smooth=FALSE,cex.axis=1.5,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)


                                        #
# Plot effect of a 1% increase in the Liberal Party national-level vote share.
y <- as.data.frame(t(t(sim.vm)-colMeans(sim.vm))) # demeaned simulated votes
names(y) <- c("pri","pan","left","pvem", "panal")

par(mfrow=c(3,1),mar=c(4,3,1.5,1.5))
for (i in 2:4) {
    plot(density(y[round(y[,1],3)==0.00,i],adjust=1.3),lwd=2,lty=2,ylim=c(0,100),
            main="",ylab="",xlab="",cex.axis=1.1,cex.lab=1.5,xlim=c(-0.035,0.03),bty="n")
    lines(density(y[round(y[,1],3)==0.01,i],adjust=1.3),lwd=2)
    mtext(side=2,text="density",line=2,cex=0.8)
    mtext(side=1,text=names(y[i]),line=2.5,cex=1.1)
    abline(v=0,col="gray50",lwd=2,lty=2)
    abline(v=mean(y[round(y[,1],3)==0.01,i]),col="gray50",lwd=2)
    if (i==2) {
        text(0.017,57,"Liberals receive\n mean (observed)\n vote share",cex=1.4)
        text(-0.023,60,"Liberals increase\n vote share by 1%",cex=1.4)
    }
}




