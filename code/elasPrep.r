### THIS CODE RUNS AFTER SCRIPT red.r HAS RAN, UP TO WHERE THIS FILE IS MENTIONED ###
### IT ESTIMATES 32 REGRESSIONS WITH EFFECTS OF STATE CHANGE IN DIPUTADOS IN EVERY STATE'S SECCIONES ###
### PREPARED 21JUL2014ERIC MAGAR
#
# Limitations:
# 1 Uses two first differences only (2006-09 and 2009-12), other should be added to get more empirical leverage
# 2 Object eq has info on section changes (from which, to which) that should be exploited. The idea is to
#   transfer votes accordingly so that first differences of changed sections do no drop to zero.
# 3 If 2 is not carried, a dummy for changed secciones should be incorporated (votes drop to zero for all
#   parties after a seccion is discarded, increasing volatility artificially.)
#
# state first differences
v1 <- df2006d0; v2 <- df2009d0; v3 <- df2012d0;
v1$pan <- ave(x = v1$pan,   as.factor(v1$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v1$pric <- ave(x = v1$pric,   as.factor(v1$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v1$prdc <- ave(x = v1$prdc,   as.factor(v1$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v1$efec <- ave(x = v1$efec,   as.factor(v1$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v2$pan <- ave(x = v2$pan,   as.factor(v2$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v2$pri <- ave(x = v2$pri,   as.factor(v2$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v2$prd <- ave(x = v2$prd,   as.factor(v2$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v2$efec <- ave(x = v2$efec,   as.factor(v2$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v3$pan <- ave(x = v3$pan,   as.factor(v3$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v3$pri <- ave(x = v3$pri,   as.factor(v3$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v3$prdc <- ave(x = v3$prdc,   as.factor(v3$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v3$efec <- ave(x = v3$efec,   as.factor(v3$edon),   FUN = function(x) sum(x), na.rm=TRUE)
v1 <- v1[duplicated(v1$edon)==FALSE,]; # one obs per state
v2 <- v2[duplicated(v2$edon)==FALSE,];
v3 <- v3[duplicated(v3$edon)==FALSE,];
v1$pan <- v1$pan/v1$efec; v1$pric <- v1$pric/v1$efec; v1$prdc <- v1$prdc/v1$efec; # shares
v2$pan <- v2$pan/v2$efec; v2$pri <- v2$pri/v2$efec; v2$prd <- v2$prd/v2$efec;
v3$pan <- v3$pan/v3$efec; v3$pri <- v3$pri/v3$efec; v3$prdc <- v3$prdc/v3$efec;
#
v21 <- v32 <- v2[,c("edon","pan","pri","prd")] # prepare object to receive first differences
v21$pan <- v2$pan - v1$pan; v21$pri <- v2$pri - v1$pric; v21$prd <- v2$prd - v1$prdc;
v32$pan <- v3$pan - v2$pan; v32$pri <- v3$pri - v2$pri; v32$prd <- v3$prdc - v2$prd;
#
state.1stdifs <- list(v21=v21, v32=v32)
state.1stdifs$v21
# COMPUTE SECTION FIRST DIFFERENCES
#
#2006
v1 <- elecs060912.seccion$e06;
v1$efec <- v1$pan + v1$apm + v1$pbt + v1$panal + v1$asdc;
v1 <- v1[,c("edon","seccion","pan","apm","pbt","efec")]; colnames(v1) <- c("edon","seccion","pan","pri","prd","efec")# keep majors only
v1 <- v1[-which(v1$efec==0),] # sections with no votes dropped
v1$pan <- v1$pan / v1$efec; v1$pri <- v1$pri / v1$efec; v1$prd <- v1$prd / v1$efec # vote shares
head(v1) # debug
#
#2009
v2 <- elecs060912.seccion$e09;
v2$efec <- v2$pan + v2$pri + v2$prd + v2$pvem + v2$pt + v2$conve + v2$panal + v2$psd + v2$primero_mexico + v2$salvemos_mexico;
v2$pri <- v2$pri + (v2$pri/(v2$pri+v2$pvem)) * v2$primero_mexico # in case of coalition, common vote split proportionally (ie. not add pvem to pri's vote)
v2 <- v2[,c("edon","seccion","pan","pri","prd","efec")] # keep majors only
v2 <- v2[-which(v2$efec==0),] # sections with no votes dropped
v2$pri[which(is.na(v2$pri)==TRUE)] <- 0 # secciones with zero coal vote became indetermined (i think)
v2$pan <- v2$pan / v2$efec; v2$pri <- v2$pri / v2$efec; v2$prd <- v2$prd / v2$efec # vote shares
head(v2) # debug
#
#2012
v3 <- elecs060912.seccion$e12;
v3$efec <- v3$pan + v3$pri + v3$prd + v3$pvem + v3$pt + v3$mc + v3$panal + v3$pripvem + v3$prdptmc + v3$prdpt + v3$prdmc + v3$ptmc;
v3$pri <- v3$pri + (v3$pri/(v3$pri+v3$pvem)) * v3$pripvem # in case of coalition, common vote split proportionally (ie. not add pvem to pri's vote)
v3$prd <- v3$prd + (v3$prd/(v3$prd+v3$pt+v3$mc)) * (v3$prdptmc+v3$prdpt+v3$prdmc) # in case of coalition, common vote split proportionally
v3 <- v3[,c("edon","seccion","pan","pri","prd","efec")] # keep majors only
v3 <- v3[-which(v3$efec==0),] # sections with no votes dropped
v3$pri[which(is.na(v3$pri)==TRUE)] <- 0 # secciones with zero coal vote became indetermined (i think)
v3$prd[which(is.na(v3$prd)==TRUE)] <- 0 # secciones with zero coal vote became indetermined (i think)
v3$pan <- v3$pan / v3$efec; v3$pri <- v3$pri / v3$efec; v3$prd <- v3$prd / v3$efec # vote shares
head(v3) # debug
#
v21 <- merge( x = v1, y = v2, by = c("edon","seccion") )
v21$pan <- v21$pan.y - v21$pan.x
v21$pri <- v21$pri.y - v21$pri.x
v21$prd <- v21$prd.y - v21$prd.x
v21 <- v21[,c("edon","seccion","pan","pri","prd")]
#
v32 <- merge( x = v2, y = v3, by = c("edon","seccion") )
v32$pan <- v32$pan.y - v32$pan.x
v32$pri <- v32$pri.y - v32$pri.x
v32$prd <- v32$prd.y - v32$prd.x
v32 <- v32[,c("edon","seccion","pan","pri","prd")]
#
v21$panst <- v21$prist <- v21$prdst <- v32$panst <- v32$prist <- v32$prdst <- 0 # will receive state first differences
i <- 1
for (i in 1:32){
    v21[which(v21$edon==i), c("panst","prist","prdst")] <- state.1stdifs$v21[state.1stdifs$v21$edon==i, 2:4];
    v32[which(v32$edon==i), c("panst","prist","prdst")] <- state.1stdifs$v32[state.1stdifs$v32$edon==i, 2:4];
}
v <- rbind(v21, v32); v <- v[order(v$edon, v$seccion),]
head(v)

# keep only objects needed to estimate elasticity
rm(list=setdiff(ls(), c("wd","v","eq","edos")))
ls()

library(R2jags)
#
### switching regime model (each district one regime, seccion is unit)
cat("
model {
  for (n in 1:N){                ## loop over observations
      depvar[n] ~ dnorm(depvar.hat[n] , tau);
      depvar.hat[n] <- inprod(beta[],X[n,]);  ## FLEXIBLE SPECIFICATION FOR VARYING
                                              ## NUMBER OF REGRESSORS, PREPARE X IN R
                }
  ############################
  ## NON-INFORMATIVE PRIORS ##
  ############################
  for (k in 1:K){
    beta[k] ~ dnorm(0, .0001);
    }
  tau <- pow(sigma, -2);
  sigma ~ dunif(0,100);
}
", file="linearModel.txt")
#

##################
## d0 districts ##
##################
tmpv <- merge( x = v, y = eq[,c("edon","seccion","dis2012")], by = c("edon","seccion") )
colnames(tmpv)[grep(pattern = "dis.*", colnames(tmpv))] <- "disn"
tmpv <- tmpv[order(tmpv$edon, tmpv$disn),]
head(tmpv)
#
# prepare data
alldis <- paste("d", tmpv$edon/100 + tmpv$disn/10000 + .000099, sep = ""); alldis <- alldis[duplicated(alldis)==FALSE]
alldis <- gsub(pattern = "99", replacement = "", x = gsub(pattern = "0\\.", replacement = "", x = alldis) )
regs.pan <- regs.pri <- regs.prd <- list() # empty lists to receive posterior samples
elas.pan <- elas.pri <- elas.prd <- vector() # empty vector to receive point estimates
#
# PAN d0
#i <- 1 # debug
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$pan;                                   ## PAN
    X <- cbind ( dis.dummies, dis.dummies * tmp$panst ); ## PAN
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.pan[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.pan)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    colnames(regs.pan[[i]])[grep(pattern = "beta.*", colnames(regs.pan[[i]]))] <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    elas.pan <- c ( elas.pan, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PAN
    print(paste(edos[i], "pan d0 done"))
}
#
names(elas.pan) <- alldis
#
# PRI d0
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$pri;                                   ## PRI
    X <- cbind ( dis.dummies, dis.dummies * tmp$prist ); ## PRI
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    ## #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.pri[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.pri)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    elas.pri <- c ( elas.pri, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PRI
    print(paste(edos[i], "pri d0 done"))
}
#
## names(regs) <- paste("state", 1:32, sep = "")
names(elas.pri) <- alldis
#
# PRD d0
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$prd;                                   ## PRD
    X <- cbind ( dis.dummies, dis.dummies * tmp$prdst ); ## PRD
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.prd[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.prd)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    elas.prd <- c ( elas.prd, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PRD
    print(paste(edos[i], "prd d0 done"))
}
#
## names(regs) <- paste("state", 1:32, sep = "")
names(elas.prd) <- alldis
#
elas.d0 <- data.frame(district = alldis, pan = elas.pan, pri = elas.pri, prd = elas.prd)
regs.d0 <- list(pan=regs.pan, pri=regs.pri, prd=regs.prd);

dis.elasticity <- list(elas.d0=elas.d0, regs.d0=regs.d0)
save(dis.elasticity, file = "disElast.RData")

##################
## d1 districts ##
##################
tmpv <- merge( x = v, y = eq[,c("edon","seccion","dis2013.1")], by = c("edon","seccion") )
colnames(tmpv)[grep(pattern = "dis.*", colnames(tmpv))] <- "disn"
tmpv <- tmpv[order(tmpv$edon, tmpv$disn),]
head(tmpv)
tmpv <- tmpv[-which(tmpv$disn==0),]
#
# prepare data
alldis <- paste("d", tmpv$edon/100 + tmpv$disn/10000 + .000099, sep = ""); alldis <- alldis[duplicated(alldis)==FALSE]
alldis <- gsub(pattern = "99", replacement = "", x = gsub(pattern = "0\\.", replacement = "", x = alldis) )
regs <- list() # empty list to receive regressions
elas.pan <- elas.pri <- elas.prd <- vector()
#
# PAN d1
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$pan;                                   ## PAN
    X <- cbind ( dis.dummies, dis.dummies * tmp$panst ); ## PAN
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.pan[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.pan)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    elas.pan <- c ( elas.pan, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PAN
    print(paste(edos[i], "pan d1 done"))
}
#
## names(regs) <- paste("state", 1:32, sep = "")
names(elas.pan) <- alldis
#
# PRI d1
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$pri;                                   ## PAN
    X <- cbind ( dis.dummies, dis.dummies * tmp$prist ); ## PAN
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.pri[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.pri)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    elas.pri <- c ( elas.pri, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PRI
    print(paste(edos[i], "pri d1 done"))
}
#
## names(regs) <- paste("state", 1:32, sep = "")
names(elas.pri) <- alldis
#
# PRD d1
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$prd;                                   ## PAN
    X <- cbind ( dis.dummies, dis.dummies * tmp$prdst ); ## PAN
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.prd[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.prd)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    elas.prd <- c ( elas.prd, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PRD
    print(paste(edos[i], "prd d1 done"))
}
#
## names(regs) <- paste("state", 1:32, sep = "")
names(elas.prd) <- alldis
#
elas.d1 <- data.frame(district = alldis, pan = elas.pan, pri = elas.pri, prd = elas.prd)
regs.d1 <- list(pan=regs.pan, pri=regs.pri, prd=regs.prd);

dis.elasticity <- list(elas.d0=elas.d0, elas.d1=elas.d1, regs.d0=regs.d0, regs.d1=regs.d1)
save(dis.elasticity, file = "disElast.RData")

##################
## d3 districts ##
##################
tmpv <- merge( x = v, y = eq[,c("edon","seccion","dis2013.1")], by = c("edon","seccion") )
colnames(tmpv)[grep(pattern = "dis.*", colnames(tmpv))] <- "disn"
tmpv <- tmpv[order(tmpv$edon, tmpv$disn),]
head(tmpv)
tmpv <- tmpv[-which(tmpv$disn==0),]
#
# prepare data
alldis <- paste("d", tmpv$edon/100 + tmpv$disn/10000 + .000099, sep = ""); alldis <- alldis[duplicated(alldis)==FALSE]
alldis <- gsub(pattern = "99", replacement = "", x = gsub(pattern = "0\\.", replacement = "", x = alldis) )
regs <- list() # empty list to receive regressions
elas.pan <- elas.pri <- elas.prd <- vector()
#
# PAN d3
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$pan;                                   ## PAN
    X <- cbind ( dis.dummies, dis.dummies * tmp$panst ); ## PAN
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.pan[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.pan)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    elas.pan <- c ( elas.pan, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PAN
    print(paste(edos[i], "pan d3 done"))
}
#
## names(regs) <- paste("state", 1:32, sep = "")
names(elas.pan) <- alldis
#
# PRI d3
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$pri;                                   ## PAN
    X <- cbind ( dis.dummies, dis.dummies * tmp$prist ); ## PAN
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.pri[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.pri)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    elas.pri <- c ( elas.pri, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PRI
    print(paste(edos[i], "pri d3 done"))
}
#
## names(regs) <- paste("state", 1:32, sep = "")
names(elas.pri) <- alldis
#
# PRD d3
for (i in 1:32){  # loop over states
    tmp <- tmpv[tmpv$edon==i,]
    n.dis <- max(tmp$disn)
    N <- nrow(tmp)
    #
    # district dummies for switching regime model
    dis.dummies <- data.frame(matrix(0, nrow = N, ncol = n.dis))
    # d <- 2 # debug
    for (d in 1:n.dis){ # loop over  districts
        select <- which(tmp$disn==d)
        dis.dummies[select,d] <- 1;
    }
    depvar <- tmp$prd;                                   ## PAN
    X <- cbind ( dis.dummies, dis.dummies * tmp$prdst ); ## PAN
    colnames (X) <- c( paste("d", 1:n.dis, sep = ""), paste("stch", 1:n.dis, sep = "") );
    K <- ncol(X);
    constant <- rep(1, times = N) # constant
    #
    elas.data <- list ( "depvar","X","N","K" )
    elas.inits <- function (){
        list (
        beta=rnorm(K),
        sigma=runif(1)
        )
        }
    elas.parameters <- c("beta", "sigma")
    #
    #test ride
    ## thisreg <- jags (elas.data, elas.inits, elas.parameters,
    ##                 "linearModel.txt", n.chains=3,
    ##                 n.iter=50, n.thin=2)
    # estimate
    thisreg <- jags (elas.data, elas.inits, elas.parameters,
                    "linearModel.txt", n.chains=3,
                    n.iter=990, n.thin=15)
    regs.prd[[i]] <- thisreg$BUGSoutput$sims.matrix; names(regs.prd)[i] <- paste(edos[i]) ## SAVES POSTERIOR CHAINS FOR STATE REGRESSION
    elas.prd <- c ( elas.prd, thisreg$BUGSoutput$median$beta[(n.dis+1):(2*n.dis)] ) ## PRD
    print(paste(edos[i], "prd d3 done"))
}
#
## names(regs) <- paste("state", 1:32, sep = "")
names(elas.prd) <- alldis
#
elas.d3 <- data.frame(district = alldis, pan = elas.pan, pri = elas.pri, prd = elas.prd)
regs.d3 <- list(pan=regs.pan, pri=regs.pri, prd=regs.prd);

dis.elasticity <- list(elas.d0=elas.d0, elas.d1=elas.d1, elas.d3=elas.d3, regs.d0=regs.d0, regs.d1=regs.d1, regs.d3=regs.d3)
save(dis.elasticity, file = "disElast.RData")


######################
#### END elasPrep ####
######################
