load(paste(dd, "swingRatios9715.RData", sep = ""))
#
# wrap data prep and jags estimation in one function
my.jags <- function(which.elec=2006,          # options are: 2003, 2006, 2009, 2012, 2015
                    which.map="d0",           #              "d0", "d1", "d3"
                    which.measure="v",        #              "v" for R, "v.bar" for P, "w.bar" for M
                    model.file=lambda.rho.4,
                    test.ride=TRUE,           # if TRUE, overrides n.chains, n.iter, n.thin 
                    n.chains=3,               # jags parameters
                    n.iter=50000,
                    n.thin=50
                    ){
    ####################################################################################################
    ### Data prep for national-agg King with Linzer-simulated data (data matrix with 6 party columns ###
    ####################################################################################################
    ## read swing ratio simulated data (Linzer) to estimate sigma and lamda on 1000 simulated elections each year
    if (which.measure=="v") which.measure <- "vmat";                            # v is called vmat in linzer sims
    data <- eval(parse(text=paste("swRats$df", which.elec, which.map, sep=""))) # <-- select year/map object to manipulate
                                        #data <- swRats$df2006d0                        # <-- select year/map object to manipulate
    colnames(data$seatmat) <- colnames(data$vmat)                             # linzer code does not add names here
    v <- as.data.frame(eval(parse(text=paste("data", "$", which.measure, sep=""))))          # extracts vote aggregation for estimation
    S <- as.data.frame(data$seatmat)                                          # extracts seat allocations
                                        #
                                        # orders party columns, adds zeroes for those not fielding candidates that year
    colnames(v)[which(colnames(v)=="left")] <- "prd"  # rename
    colnames(S)[which(colnames(S)=="left")] <- "prd"  # rename
    ordered <- c("pan","pri","prd","green","mc","morena")
    tmp <- setdiff(ordered, colnames(v))
    if (length(tmp)>0) {
        tmp1 <- data.frame(matrix(0, nrow=nrow(v), ncol=length(tmp)))
        colnames(tmp1) <- setdiff(ordered, colnames(v))
        v <- cbind(v, tmp1)
        S <- cbind(S, tmp1)
    }
    v <- v[, ordered]
    S <- S[, ordered]
                                        #
    D <- 300 # OJO: 298 for df2003d97!
    S <- S*D # turn share into number of seats won 
                                        #
    I <- nrow(S)
    J <- ncol(S)
    D <- rep(D, I)
    dummy <- v; dummy[,] <- 0; dummy[v>0] <- 1 # indicates parties with v>0
                                        #
                                        # labels to interpret parameters
    party.labels <- list(party.labels.model = colnames(v),
                         lambda.labels      = paste(colnames(v)[-2], colnames(v)[2], sep = "."))
                                        #
                                        # reduce from 1000 to 100 sims to accelerate jags estimation
    S <- S[1:100,]; v <- v[1:100,]; I <- 100; D <- D[1:100]; dummy <- dummy[1:100,]
    ### Data, initial values, and parameter vector for bugs
    l.r.data <- list("S", "v", "I", "J", "D", "dummy")
    l.r.inits <- function(){ list (lambda=rnorm(J-1), rho=rexp(1)) }
    l.r.parameters <- c("lambda", "rho")
    ########################################################################################
    ### End data prep for nat-agg king with Linzer sim data (matrix with 7 party columns ###
    ########################################################################################
    ## test ride
    if (test.ride==TRUE){
        n.chains=2;
        n.iter=100;
        n.thin=10;
    }
    message(sprintf("Will run jags with %s chains and %s iterations", n.chains, n.iter))
    ## estimate
    tmpRes <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                    model.file=lambda.rho.4,
                    n.chains=n.chains,
                    n.iter=n.iter,
                    n.thin=n.thin
                    )
    tmpRes$party.labels <- party.labels # add object to interpret bias parameters: relative to pri=2
    summary(tmpRes)
    return(tmpRes)
}

tmpRes <- my.jags(which.elec = 2012,
                  which.map  = "d0",
                  which.measure = "w.bar",
                  test.ride=FALSE
                  )

res2012d0w.bar <- tmpRes; #rm(tmpRes)

# inspect results
quantile(tmpRes$BUGSoutput$sims.list$lambda[,1])



# create a list with all results in, save it
biasRespOnLinzerSimsRPM <- lapply(ls(pattern = "res[0-9]"), get);
names(biasRespOnLinzerSimsRPM) <- ls(pattern = "res[0-9]")
summary(biasRespOnLinzerSimsRPM)
save(biasRespOnLinzerSimsRPM,
     file=paste(dd, "biasRespOnLinzerSims3components0315.RData", sep =""))

