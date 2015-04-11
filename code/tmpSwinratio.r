# tweak function sim.election() NOT NEEDED THUS FAR
my.sim.election <- function (fit, dat) {
    vsim <- matrix(NA, nrow = nrow(dat), ncol = ncol(dat))
    pos <- 1
    for (f in 1:length(fit)) {
        pcol <- match(fit[[f]]$cols, names(dat))
        if (length(pcol) == 2) { # uncontested districts
            vsim[pos:(pos + fit[[f]]$N - 1), pcol] <- cbind(rep(0, fit[[f]]$N), rep(1, fit[[f]]$N))
        }
        else { # contested districts
            R <- length(fit[[f]]$P)
            draws <- matrix(NA, nrow = fit[[f]]$N, ncol = ncol(fit[[f]]$dat) - 1)
            grp <- rmultinom(1, fit[[f]]$N, fit[[f]]$P)
            ins <- 1
            for (r in 1:R) {
                if (grp[r] > 0) {
                  draws[ins:sum(grp[1:r]), ] <- rmvnorm(grp[r], 
                    mean = fit[[f]]$mu[r, ], sigma = fit[[f]]$sig[, , r], method = "chol")
                  ins <- ins + grp[r]
                }
            }
            voteshares <- exp(cbind(0, draws[, 2:ncol(draws)]))/rowSums(exp(cbind(0, draws[, 2:ncol(draws)])))
            vsim[pos:(pos + fit[[f]]$N - 1), pcol] <- cbind(draws[, 1], voteshares)
        }
        pos <- pos + fit[[f]]$N
    }
    vsim[(vsim[, 1] < 0), 1] <- 1e-04
    return(vsim)
}
environment(my.sim.election) <- asNamespace('seatsvotes')


# tweak swingratio function
my.swingratio <- function (fit, sims = 1000, rule = plurality, graph = TRUE) {
    starttime <- Sys.time()
    dat <- reconstruct(fit)  # needs no tweaking: internal function turns fit into matrix
    dat[is.na(dat)] <- 0
    np <- ncol(dat)
    votemat <- NULL
    seatmat <- NULL
    w.barmat <- v.barmat <- vmat <- absmat <- ptotmat <- NULL # will receive three aggregates of national vote, national ptot and abstention (simulated)
    for (s in 1:sims) {
        #s <- 1 # debug
        #message(sprintf("loop %s of %s", s, sims))  # progress
        vsim <- sim.election(fit, dat)
        #vsim <- my.sim.election(fit, dat) # may be unnecessary
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
        vsim2[,-1] <- vsim2[, -1] / efec                          # votes shares relative to effective vote
        hr <- vsim2[, 1] / sum(vsim2[, 1])                        # district:national population ratio
        vr <- efec / sum(efec)                                    # district:national raw vote ratio
        vmat <-     rbind(vmat,     colSums(vsim2[,-1] * vr))     # append natl vote shares
        v.barmat <- rbind(v.barmat, colSums(vsim2[,-1] *1/300))   # append mean district vote shares
        w.barmat <- rbind(w.barmat, colSums(vsim2[,-1] * hr))     # append pop-weighted mean district vote shares
        votemat <-  rbind(votemat, colSums(vsim2[,-1] * vr))      # redundant, but avoids further tweaking this function below
        seatmat <-  rbind(seatmat, rule(vsim2[, -1]))             # seat shares won
    }
    ##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@5@"]]))##:ess-bp-end:##
    ret <- list()
    truevotes <- dat[, 1] * dat[, -1]
    truevotes2 <- truevotes                                       # duplicate for manipulation
    trueptot <- dat[,1]                                           # save ptot to report
    trueabs <- truevotes2[,grep("abs", colnames(truevotes2))]     # save abstention rate to report
    truevotes2 <- truevotes2[,-grep("abs", colnames(truevotes2))] # drop abstention from vote object                                    
    #ret$truevote <- colSums(truevotes)/sum(truevotes)
    ret$truevote <- colSums(truevotes2)/sum(truevotes2)           # true vote shares won
    ret$trueptot <- trueptot                                      # true total population
    ret$trueabs <- trueabs                                        # true abstention
    dat2 <- dat[, -1]                                             # duplicate for manipulation
    dat2 <- dat2[, -grep("abs", colnames(dat2))]
    #ret$trueseat <- rule(dat[, -1])
    ret$trueseat <- rule(dat2[, -1])                              # true seat shares won
    names(ret$trueseat) <- names(ret$truevote)
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
    ret$vmat <- vmat          # add national vote shares
    ret$v.barmat <- v.barmat  # add mean district vote shares
    ret$w.barmat <- w.barmat  # add pop-weighted mean district vote shares
    ret$ptotmat <- ptotmat    # add total population
    ret$absmat <- absmat      # add abstention share relative to total population
    ## ret$swing <- swing
    ## ret$swing.lm <- swing.lm
    ## ret$swing.median <- swing.median
    ## ret$vs <- seatlist
    ret$time <- Sys.time() - starttime
    return(ret)
}
environment(my.swingratio) <- asNamespace('seatsvotes')


# create binominal function
binominal <- function(votes, listmap=listmap) {
    # listmap is dataframe pairing same-list candidates in district (same dimensionality as votes)
    nd <- nrow(votes) # number of districts
    np <- ncol(votes) # number of columns (for candidates) 
    ## wm <- NULL
    ## for (i in 1:nr) { wm <- c(wm,which.max(votes[i,])) }
    ## seats <- NULL
    ## for (i in 1:nc) { seats <- c(seats,sum(wm==i)) }
    seats <- votes; seats[,] <- 0 ## will receive seat winners
    finished <- t(apply(tmp*10, 1, function(x) rank(-x)))  # who came 1st, 2nd... in district
    ## create object aggregating list votes (using listmap)
    ## which list came 1st, 2nd, ...
    ## did 1st list double 2nd list's votes?
    ## if so, wins both seats
    ## else, winners of lists 1st and 2nd get seats
    #return(seats/nr)
    return(colSums(seats)/nr)
}
environment(binominal) <- asNamespace('seatsvotes')

rank(-c(3, 1, 4, 10), ties.method = "average")
    

