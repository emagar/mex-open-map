rm(list = ls())
#
wd <- c("~/Desktop/tmp/")  # where to save and retrieve objects
dd <- c("~/Desktop/tmp/") # raw data directory
gd <- c("~/Desktop/tmp/") # graph data directory
setwd(wd)

# electoral data is read here
load(file = paste(dd, "elec0312.RData", sep = ""))
summary(elec0312)
colnames(elec0312$df2006d0)

## notations: df20..d0 in dataset are diputados federales returns for year 20.. aggregated into map "d0" (ie, 2006 map); use "d3" for 2015 map and "d97" for 1997 map
##            efec refers to effective vote (votes cast - voided votes - votes for parties/candidates dropped from analysis)
##            ptot is total population

#
## ## use to extract all objects from list elec060912 if so wished
## for(i in 1:length(elec060912)){
##     ##first extract the object value
##     tempobj=elec060912[[i]]
##     ##now create a new variable with the original name of the list item
##     eval(parse(text=paste(names(elec060912)[[i]],"= tempobj")))
## }
## rm(elec060912)
## dim(df2012d0)

## TWEAK LINZER FUNCTIONS TO ADAPT FOR grofman-etal (GKB) ESTIMATION
library(seatsvotes) # Linzer's original suite --- get it from dataverse
#seatsvotes:::reconstruct # how to see hidden function reconstruct() in package
#
## # tweak function findpatterns to include efec and ptot columns NOT USED ANYMORE---NEVER WORKED PROPERLY
## my.findpatterns <- function (dat){
##     dat <- as.data.frame(dat)
##     #gtz <- as.data.frame(dat[, 2:ncol(dat)] > 0)
##     gtz <- as.data.frame(dat[, 3:ncol(dat)] > 0)
##     o <- do.call(order, gtz[names(gtz)])
##     gtz <- gtz[o, ]
##     dat <- dat[o, ]
##     splitdat <- list()
##     pat <- 1
##     prev <- gtz[1, ]
##     splitdat[[pat]] <- dat[1, ]
##     for (i in 2:nrow(dat)) {
##         if (!all(gtz[i, ] == prev)) {
##             pat <- pat + 1
##             splitdat[[pat]] <- dat[i, ]
##         }
##         else {
##             splitdat[[pat]] <- rbind(splitdat[[pat]], dat[i,])
##         }
##         prev <- gtz[i, ]
##     }
##     cat(pat, "patterns of contestation: \n")
##     nm <- NULL
##     for (i in 1:length(splitdat)) {
##         #splitdat[[i]] <- splitdat[[i]][, c(TRUE, colSums(splitdat[[i]][, -1]) > 0)]
##         splitdat[[i]] <- splitdat[[i]][, c(TRUE, colSums(splitdat[[i]][, -1:-2]) > 0)]
##         #nm <- c(nm, paste(names(splitdat[[i]])[-1], collapse = "-"))
##         nm <- c(nm, paste(names(splitdat[[i]])[-1:-2], collapse = "-"))
##         cat("[", i, "] ", nm[i], ": ", nrow(splitdat[[i]]), "\n", sep = "")
##     }
##     names(splitdat) <- nm
##     invisible(splitdat)
## }
## environment(my.findpatterns) <- asNamespace('seatsvotes')
#
## # tweak mvnmix function to deal with efec and ptot columns NOT USED ANYMORE (opted to model abstention as the n+1th party instead)
## my.mvnmix <- function (dat, components = 2, maxiter = 1000, tol = 1e-05, inits = NULL, scatter = FALSE, plot.ell = TRUE, nrep = 1) {
##     ret <- list()
##     R <- components
##     nd <- nrow(dat)
##     #np <- ncol(dat) - 1
##     np <- ncol(dat) - 2
##     if (np > 1) {
##         #y <- dat[, 1]/10000
##         y <- dat[, 2]/10000
##         y.ptot <- dat[, 1]/10000; y <- cbind(y.ptot, y)
##         #dat[, -1] <- dat[, -1]/rowSums(dat[, -1])
##         dat[, -1:-2] <- dat[, -1:-2]/rowSums(dat[, -1:-2]) # turn into vote shares in case they were not
##         #for (i in 3:(np + 1)) {
##         for (i in 4:(np + 2)) {
##             #y <- cbind(y, log(dat[, i]/dat[, 2]))
##             y <- cbind(y, log(dat[, i]/dat[, 3]))
##         }
##     }
##     else { # didn't tweak uncontested districts (none in Mx)
##         dat[, 1] <- y <- rep(NA, nd)
##         dat[, 2] <- rep(1, nd)
##         trap <- T
##         ret$cols <- names(dat)
##         ret$N <- nd
##         nrep <- 0
##     }
##     bestllik <- -Inf
##     repl <- 1
##     while (repl <= nrep) {
##         #mu.init <- matrix(y[sample(c(1:nd), R), ], ncol = np)
##         mu.init <- matrix(y[sample(c(1:nd), R), ], ncol = (np+1))
##         sig.init <- array(0, dim = c(np, np, R))
##         for (r in 1:R) {
##             #sig.init[, , r] <- (c(runif(1, 0.5, 1.5) * sd(y[, 1]), runif(np - 1, 0.2, 1)) * diag(np))^2
##             sig.init[, , r] <- (c(runif(1, 0.5, 1.5) * sd(y[, 2]), runif(np - 1, 0.2, 1)) * diag(np))^2
##         }
##         P.init <- matrix(runif(R, min = 0.25, max = 0.75), nrow = R, 
##             ncol = 1)
##         P.init <- P.init/sum(P.init)
##         if (!is.null(inits)) {
##             mu.init <- inits$mu
##             sig.init <- inits$sig
##             P.init <- inits$P
##         }
##         mu.old <- mu.init
##         sig.old <- sig.init
##         P.old <- P.init
##         mu.new <- matrix(NA, nrow = R, ncol = ncol(y))
##         sig.new <- array(NA, dim = c(np, np, R))
##         P.new <- matrix(NA, nrow = R, ncol = 1)
##         llik <- -Inf
##         iter <- 1
##         dll <- Inf
##         while ((iter <= maxiter) & (dll > tol)) {
##             iter <- iter + 1
##             denom <- 0
##             trap <- F
##             for (r in 1:R) {
##                 trap.try <- try(dmvnorm(y, mean = mu.old[r, ], 
##                   sigma = sig.old[, , r]), silent = T)
##                 if (inherits(trap.try, "try-error")) {
##                   trap <- T
##                 }
##             }
##             if (trap) {
##                 llik <- c(llik, llik[iter - 1])
##             }
##             else {
##                 for (r in 1:R) {
##                   denom <- denom + (dmvnorm(y, mean = mu.old[r,], sigma = sig.old[, , r]) * P.old[r])
##                 }
##                 for (r in 1:R) {
##                   rgivy <- (dmvnorm(y, mean = mu.old[r, ], sigma = sig.old[, , r]) * P.old[r])/denom
##                   mu.new[r, ] <- (colSums(rgivy * y))/sum(rgivy)
##                   d <- y - matrix(mu.new[r, ], nrow = nd, ncol = ncol(mu.new), 
##                     byrow = T)
##                   sig.new[, , r] <- (t(d) %*% (rgivy * d))/sum(rgivy)
##                   P.new[r] <- sum(rgivy)/nd
##                 }
##                 mu.old <- mu.new
##                 sig.old <- sig.new
##                 P.old <- P.new
##                 llik <- c(llik, sum(log(denom)))
##             }
##             dll <- llik[iter] - llik[iter - 1]
##         }
##         if (!trap) {
##             if (llik[iter] > bestllik) {
##                 bestllik <- llik[iter]
##                 ret$cols <- names(dat)
##                 ret$N <- nd
##                 ret$inits <- list()
##                 ret$inits$mu <- mu.init
##                 ret$inits$sig <- sig.init
##                 ret$inits$P <- P.init
##                 ret$mu <- mu.new
##                 ret$sig <- sig.new
##                 ret$P <- c(P.new)
##                 ret$ml <- llik[iter]
##                 ret$npar <- (R - 1) + (R * np) + (R * np * (np + 1)/2) # no veo qué hace esto
##                 ret$aic <- (-2 * ret$ml) + (2 * ret$npar)
##                 ret$bic <- (-2 * ret$ml) + (log(nd) * ret$npar)
##             }
##             if (nrep > 1) {
##                 cat("Model ", repl, ": llik = ", llik[iter], 
##                   " ... best llik = ", ret$ml, "\n", sep = "")
##                 flush.console()
##             }
##             repl <- repl + 1
##         }
##     }
##     ret$y <- y
##     ret$dat <- dat
##     if (np > 1) {
##         cat("\n For", R, "component distributions:")
##         cat("\n   Number of districts:", ret$N)
##         cat("\n   Maximum log-likelihood:", ret$ml)
##         cat("\n   BIC:", ret$bic, "\n \n")
##         if (scatter) {
##             #splom.lr(y, names(dat)[-1], ret$mu, ret$sig, ret$P, plot.ell)
##             splom.lr(y, names(dat)[-1:-2], ret$mu, ret$sig, ret$P, plot.ell)
##         }
##     }
##     else { # did't tweak uncontested districts
##         cat(paste("\n Uncontested ", ret$cols[2], ": ", ret$N, 
##             " districts \n \n", sep = ""))
##     }
##     return(ret)
## }
## environment(my.mvnmix) <- asNamespace('seatsvotes')
#
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
#
## # tweak swingratio() function to work with ptot and abstention (to simulate Grofman et al.'s quantities) 
my.swingratio.gkb <- function (fit, sims = 1000, rule = plurality) {  # RETURNS SIMS ONLY (NO SWINGS OBJECT NOR GRAPH) BUT INCLUDES POPULATION AND TURNOUT DATA FOR GROFMAN ET AL
    starttime <- Sys.time()
    dat <- reconstruct(fit)  # needs no tweaking: internal function turns fit into matrix
    dat[is.na(dat)] <- 0
    np <- ncol(dat)
    nd <- nrow(dat) # added to deal with cases where some districts are missing (eg. voided, as 2 were in 2003)
    votemat <- NULL
    seatmat <- NULL
    w.barmat <- v.barmat <- vmat <- absmat <- ptotmat <- NULL # will receive three aggregates of national vote, national ptot and abstention (simulated)
    for (s in 1:sims) {
        #s <- 1 # debug
        #message(sprintf("loop %s of %s", s, sims))  # progress
        vsim <- sim.election(fit, dat)
        #vsim <- my.sim.election(fit, dat) # seems unnecessary
        vsim[is.na(vsim)] <- 0
        #partyvotes <- vsim[, 1] * vsim[, -1]
        #votemat <- rbind(votemat, colSums(partyvotes)/sum(partyvotes))
        #seatmat <- rbind(seatmat, rule(vsim[, -1]))
        vsim2 <- vsim; colnames(vsim2) <- colnames(dat)           # duplicate for manipulation
        vsim2[,1] <- vsim2[,1]*10000                              # remove thousands
        vsim2[,-1] <- vsim2[,1] * vsim2[,-1]                      # raw votes
        efec <- vsim2[,1] - vsim2[, grep("abs", colnames(vsim2))] # effective vote
        ptotmat <- rbind(ptotmat, sum(vsim2[, 1]))                # append total population simulation
        absmat <-  rbind( absmat, sum(vsim2[, grep("abs",
                          colnames(vsim2))]) / sum(vsim2[, 1]))   # append abstention
        vsim2 <- vsim2[, -grep("abs", colnames(vsim2))]           # drop abstention
        rawvotes <- vsim2[, -1]                                   # used to compute quantities of interest
        vsim2[,-1] <- vsim2[, -1] / efec                          # vote shares relative to effective vote
        hr <- vsim2[, 1] / sum(vsim2[, 1])                        # district:national population ratio
        vr <- efec / sum(efec)                                    # district:national raw vote ratio
        vmat <-     rbind(vmat,     colSums(vsim2[,-1] * vr))     # append natl vote shares
        v.barmat <- rbind(v.barmat, colSums(vsim2[,-1] *1/nd))    # append mean district vote shares
        w.barmat <- rbind(w.barmat, colSums(vsim2[,-1] * hr))     # append pop-weighted mean district vote shares
        votemat <-  rbind(votemat, colSums(vsim2[,-1] * vr))      # redundant, but avoids further tweaking this function below
        seatmat <-  rbind(seatmat, rule(vsim2[, -1]))             # seat shares won
    }
    ret <- list()
    truevotes <- dat[, 1] * dat[, -1]
    truevotes2 <- truevotes                                       # duplicate for manipulation
    trueptot <- dat[,1]                                           # save population to report
    trueabs <- truevotes2[,grep("abs", colnames(truevotes2))]     # save abstention rate to report
    truevotes2 <- truevotes2[,-grep("abs", colnames(truevotes2))] # drop abstention from vote object                                    
    #ret$truevote <- colSums(truevotes)/sum(truevotes)
    ret$truevote <- colSums(truevotes2)/sum(truevotes2)           # true vote shares won
    ret$trueptot <- sum(trueptot)                                 # true total population
    ret$trueabs <- sum(trueabs) / sum(trueptot)                   # true abstention
    dat2 <- dat[, -1]                                             # duplicate for manipulation
    dat2 <- dat2[, -grep("abs", colnames(dat2))]
    #ret$trueseat <- rule(dat[, -1])
    ret$trueseat <- rule(dat2)                                    # true seat shares won
    names(ret$trueseat) <- names(ret$truevote)
    #### block below commented because analysis uninterested in obtaining swing ratios; if wished, code will need tweaking
    ## swing <- NULL
    ## swing.lm <- NULL
    ## seatlist <- list()
    ## for (j in 1:(np - 1)) {
    ##     smean.hi <- mean(seatmat[(votemat[, j] < (ret$truevote[j] + 
    ##         0.012)) & (votemat[, j] > (ret$truevote[j] + 0.008)), 
    ##         j])
    ##     smean.lo <- mean(seatmat[(votemat[, j] > (ret$truevote[j] - 
    ##         0.012)) & (votemat[, j] < (ret$truevote[j] - 0.008)), 
    ##         j])
    ##     swing <- c(swing, round((smean.hi - smean.lo)/0.02, 2))
    ##     swing.lm <- c(swing.lm, round(coefficients(lm(seatmat[, 
    ##         j] ~ votemat[, j]))[2], 2))
    ##     vrange <- seq(min(floor(votemat[, j] * 100)), max(floor(votemat[, 
    ##         j] * 100)), 1)/100
    ##     seatlist[[j]] <- cbind(vrange, matrix(NA, nrow = length(vrange), 
    ##         ncol = 3))
    ##     colnames(seatlist[[j]]) <- c("vote", "mean", "lower", "upper")
    ##     for (i in 1:length(vrange)) {
    ##         sel <- seatmat[(votemat[, j] >= (vrange[i] - 0.002)) & 
    ##             (votemat[, j] < (vrange[i] + 0.002)), j]
    ##         if (length(sel) < 10) {
    ##             seatlist[[j]][i, 2:4] <- NA
    ##         }
    ##         else {
    ##             seatlist[[j]][i, 2] <- mean(sel)
    ##             seatlist[[j]][i, 3:4] <- quantile(sel, probs = c(0.025, 
    ##               0.975))
    ##         }
    ##     }
    ## }
    ## if (graph) {
    ##     par(mar = c(4.4, 4.1, 0.5, 0.5), mfrow = c(1, 1))
    ##     plot(ret$truevote[!is.na(swing)], ret$trueseat[!is.na(swing)], 
    ##         pch = 19, cex = 1.2, xlim = c(0, 0.7), ylim = c(0, 
    ##             0.7), xlab = "Party vote share", ylab = "Party seat share", 
    ##         cex.lab = 1.5, xaxt = "n", yaxt = "n")
    ##     axis(1, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
    ##     axis(2, seq(0, 1, 0.1), seq(0, 1, 0.1), cex.axis = 1.5)
    ##     for (j in 1:(np - 1)) {
    ##         if (!is.na(swing[j])) {
    ##             lines(seatlist[[j]][, 1], seatlist[[j]][, 2], 
    ##               lwd = 2.5)
    ##             lines(seatlist[[j]][, 1], seatlist[[j]][, 3], 
    ##               lwd = 2, lty = 3)
    ##             lines(seatlist[[j]][, 1], seatlist[[j]][, 4], 
    ##               lwd = 2, lty = 3)
    ##             text(ret$truevote[j] + 0.01, ret$trueseat[j], 
    ##               paste(colnames(dat)[j + 1], ": ", swing[j], 
    ##                 sep = ""), cex = 1.2, pos = 4)
    ##         }
    ##     }
    ## }
    ## vdiff <- as.data.frame(t(t(votemat) - colMeans(votemat)))
    ## sdiff <- as.data.frame(t(t(seatmat) - colMeans(seatmat)))
    ## swing.median <- round(apply(sdiff/vdiff, 2, median), 2)
    ## names(seatlist) <- colnames(votemat) <- colnames(seatmat) <- names(ret$truevote)
    ## names(swing) <- names(swing.lm) <- names(swing.median) <- names(ret$truevote)
    ## swingtab <- cbind(round(100 * ret$truevote, 1), round(100 * 
    ##     ret$trueseat, 1), swing)
    ## rownames(swingtab) <- colnames(dat[, -1])
    ## colnames(swingtab) <- c("Votes", "Seats", "Swing ratio")
    ## print(swingtab)
    ret$votemat <- votemat
    ret$seatmat <- seatmat
    ret$vmat <- vmat                  # add national vote shares
    ret$v.barmat <- v.barmat          # add mean district vote shares
    ret$w.barmat <- w.barmat          # add pop-weighted mean district vote shares
    names(ret$seatmat) <- names(vmat) # add colnames
    ret$ptotmat <- ptotmat            # add total population
    ret$absmat <- absmat              # add abstention share relative to total population
    ## ret$swing <- swing
    ## ret$swing.lm <- swing.lm
    ## ret$swing.median <- swing.median
    ## ret$vs <- seatlist
    ret$time <- Sys.time() - starttime
    return(ret)
}
environment(my.swingratio.gkb) <- asNamespace('seatsvotes')




# -----------------------------------------
# select 2012 votes in 2006 map
# -----------------------------------------
dat <- elec0312$df2012d0
tmp.ptot <- dat$ptot # keep ptot for use later
rownames(dat) <- NULL
################################################################################################################
# DATA NEEDS [EFEC V1SH V2SH] FOR STD LINZER OR [PTOT V1SH V2SH ... ABS] (SHARES OVER PTOT) FOR my GKB VERSION #
################################################################################################################
colnames(dat) # many redundant columns
# select votes only, move pri to 1st col to make reference pty, and green to last because often has no votes (and sim.votes pushes it there anyway)
dat <- dat[,c("pri","pan","pric","prdc","panal","pvem")]
# consider pri and pri-pvem the same (else pri "absent" from 1/3 district has lower elasticity)
dat$pri <- dat$pri + dat$pric; dat$pric <- NULL
# who won the seats? useful if non-winners (or subset of them) wish to be dropped
table(apply(dat, 1, function(x) which.max(x)))
## # drop panal, didn't win seats
## dat <- dat[,-5]
## # re-compute effective vote (in case parties dropped)
tmp.efec <- apply(dat, 1, sum)
## compute abstentions
tmp.abst <- tmp.ptot - tmp.efec # useful later
#
# CHOOSE ROUTE 1 OR ROUTE 2
## ## ROUTE 1: keep efec and votes only (as Linzer does) to use standard functions
## # vote shares
## dat <- dat/tmp.efec
## # add efec vote in 1st column (or, in included, ptot in 1st col and efec in 2nd) 
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
colnames(dat)[which(colnames(dat)=="prdc")] <- "left"
colnames(dat)[which(colnames(dat)=="pvem")] <- "green"
head(dat)

# find patterns of party contestation, change data to lists
dat.pat <- findpatterns(dat)
head(dat.pat[[2]]) # debug

# estimate mixture models for each pattern of contestation (start w components=1, inspect fit visually, increase if needed)
fit <- list()
fit[[1]] <- mvnmix( dat = dat.pat[[1]], components = 1, nrep = 10, scatter = TRUE) 
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
summary(elas)
# plot pri's simulations and actual obs
plot(elas$vmat[,1], elas$seatmat[,1], xlim = c(0,1), ylim = c(0,1), col = "gray"); points(elas$truevote[1], elas$trueseat[1]);
#
# point estimate of swing-ratios (error estimate would need tweaking Linzer's function) 
lm(elas$seatmat[,1] ~ elas$vmat[,1]) # pri
lm(elas$seatmat[,2] ~ elas$vmat[,2]) # pan

# rename
elas2012d0 <- elas


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


                                        #
## # Plot effect of a 1% increase in the Liberal Party national-level vote share.
## y <- as.data.frame(t(t(sim.vm)-colMeans(sim.vm))) # demeaned simulated votes
## names(y) <- c("pri","pan","left","pvem", "panal")

## par(mfrow=c(3,1),mar=c(4,3,1.5,1.5))
## for (i in 2:4) {
##     plot(density(y[round(y[,1],3)==0.00,i],adjust=1.3),lwd=2,lty=2,ylim=c(0,100),
##             main="",ylab="",xlab="",cex.axis=1.1,cex.lab=1.5,xlim=c(-0.035,0.03),bty="n")
##     lines(density(y[round(y[,1],3)==0.01,i],adjust=1.3),lwd=2)
##     mtext(side=2,text="density",line=2,cex=0.8)
##     mtext(side=1,text=names(y[i]),line=2.5,cex=1.1)
##     abline(v=0,col="gray50",lwd=2,lty=2)
##     abline(v=mean(y[round(y[,1],3)==0.01,i]),col="gray50",lwd=2)
##     if (i==2) {
##         text(0.017,57,"Liberals receive\n mean (observed)\n vote share",cex=1.4)
##         text(-0.023,60,"Liberals increase\n vote share by 1%",cex=1.4)
##     }
## }

