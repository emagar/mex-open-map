#################################################################################
### Data preparations for national-agg King (data matrix with 7 party columns ###
#################################################################################
obj1 <- df2012d0
obj2 <- df2009d0
obj3 <- df2006d0
# national aggregates
S <-          data.frame(pan=obj1$panw, pri=obj1$priw+obj1$pricw, prd=obj1$prdcw, pvem=obj1$pvemw, panal=obj1$panalw, ptc=rep(0,300), asdc=rep(0,300))
S <- apply(S, 2, sum) / 300
tmp <-        data.frame(pan=obj2$panw, pri=obj2$priw+obj2$pricw, prd=obj2$prdw,  pvem=obj2$pvemw, panal=obj2$panalw, ptc=obj2$ptcw, asdc=rep(0,300))
tmp <- apply(tmp, 2, sum) / 300
S <- rbind(S, tmp)
tmp <-        data.frame(pan=obj3$panw, pri=obj3$pricw,           prd=obj3$prdcw, pvem=rep(0,300), panal=obj3$panalw, ptc=rep(0,300), asdc=obj3$asdcw)
tmp <- apply(tmp, 2, sum) / 300
S <- rbind(S, tmp)
# re-does data.frame to check that aggregation went well
S <- data.frame(S=as.vector(S), yr=rep(c(2012,2009,2006),7), pty=c(rep("pan",3),rep("pri",3),rep("prd",3),rep("pvem",3),rep("panal",3),rep("ptc",3),rep("asdc",3)))
S <- S$S
#
# v=R is national vote shares --- Grofman et al 1997 show that its use conflates 3 sources of party bias, so below are options to use other vote shares to separate them
R <-          data.frame(pan=obj1$pan, pri=obj1$pri+obj1$pric, prd=obj1$prdc, pvem=obj1$pvem, panal=obj1$panal, ptc=rep(0,300),  asdc=rep(0,300), efec=obj1$efec)
R <- apply(R, 2, sum)
R <- R[-length(R)] / R[length(R)]
tmp <-        data.frame(pan=obj2$pan, pri=obj2$pri+obj2$pric, prd=obj2$prd,  pvem=obj2$pvem,  panal=obj2$panal, ptc=obj2$ptc, asdc=rep(0,300), efec=obj2$efec)
tmp <- apply(tmp, 2, sum)
tmp <- tmp[-length(tmp)] / tmp[length(tmp)]
R <- rbind(R, tmp)
tmp <-        data.frame(pan=obj3$pan, pri=obj3$pric,          prd=obj3$prdc, pvem=rep(0,300), panal=obj3$panal, ptc=rep(0,300), asdc=obj3$asdc, efec=obj3$efec)
tmp <- apply(tmp, 2, sum)
tmp <- tmp[-length(tmp)] / tmp[length(tmp)]
R <- rbind(R, tmp)
# re-does data.frame to check that aggregation went well
R <- data.frame(R=as.vector(R), yr=rep(c(2012,2009,2006),7), pty=c(rep("pan",3),rep("pri",3),rep("prd",3),rep("pvem",3),rep("panal",3),rep("ptc",3),rep("asdc",3)))
R <- R$R
#
# v=M is malapportionment-corrected (statewide) vote share --- for party bias due to malapportioned districts
M <-   data.frame(pan=obj1$pan, pri=obj1$pri+obj1$pric, prd=obj1$prdc, pvem=obj1$pvem, panal=obj1$panal, ptc=rep(0,300), asdc=rep(0,300))/obj1$efec
totPop <- obj1$ptot / sum(obj1$ptot) # district:national population ratios vector
M <- M*totPop # weight district vote shares by district:state population ratio
M <- apply(M, 2, sum)
tmp <- data.frame(pan=obj2$pan, pri=obj2$pri+obj2$pric, prd=obj2$prd, pvem=obj2$pvem, panal=obj2$panal, ptc=obj2$ptc, asdc=rep(0,300))/obj2$efec
totPop <- obj2$ptot / sum(obj2$ptot) # district:national population ratios vector
tmp <- tmp*totPop # weight district vote shares by district:state population ratio
tmp <- apply(tmp, 2, sum)
M <- rbind(M, tmp)
tmp <- data.frame(pan=obj3$pan, pri=obj3$pric, prd=obj3$prdc, pvem=rep(0,300), panal=obj3$panal, ptc=rep(0,300), asdc=obj3$asdc)/obj3$efec
totPop <- obj3$ptot / sum(obj3$ptot) # district:national population ratios vector
tmp <- tmp*totPop # weight district vote shares by district:state population ratio
tmp <- apply(tmp, 2, sum)
M <- rbind(M, tmp)
# re-does data.frame to check that aggregation went well
M <- data.frame(M=as.vector(M), yr=rep(c(2012,2009,2006),7), pty=c(rep("pan",3),rep("pri",3),rep("prd",3),rep("pvem",3),rep("panal",3),rep("ptc",3),rep("asdc",3)))
M <- M$M

##-----------------------------------------------------------------
## SELECT HERE WHAT MEASURE OF VOTE SHARE TO USE: CLASSIC USES v=R
##-----------------------------------------------------------------
v <- R   # classic: conflates three sources of party bias (subtract bias(R) - bias(M) to separate turnout-based party bias)
#v <- P   # measures party bias due to *distributional* factors (vote geography)
#v <- M # measures party bias due to *malapportionment* (subtract bias(M) - bias(P) to separate malapp.-based party bias)
#
dummy <- R; dummy[] <- 0; dummy[R>0] <- 1 # indicates parties with v>0
#
D <- 300
I <- length(R);
J <- 7;
#
# changes S from share to number of seats
S <- S*D
### Data, initial values, and parameter vector for bugs
l.r.data <- list("S", "v", "I", "J", "D", "dummy")
l.r.inits <- function(){ list (lambda=rnorm(J-1), rho=rexp(1)) }
l.r.parameters <- c("lambda", "rho")
#############################################################################
### End data prep for national-agg king (data matrix with 7 party columns ###
#############################################################################


# v=P is mean district vote share (in state) --- for party bias due to distributive party strength
P <-   data.frame(pan=obj1$pan, pri=obj1$pri+obj1$pric, prd=obj1$prdc, pvem=obj1$pvem, panal=obj1$panal, ptc=rep(0,300), asdc=rep(0,300))/obj1$efec
P <- apply(P, 2, mean)
tmp <- data.frame(pan=obj2$pan, pri=obj2$pri+obj2$pric, prd=obj2$prd, pvem=obj2$pvem, panal=obj2$panal, ptc=obj2$ptc, asdc=rep(0,300))/obj2$efec
tmp <- apply(tmp, 2, mean)
P <- rbind(P, tmp)
tmp <- data.frame(pan=obj3$pan, pri=obj3$pric, prd=obj3$prdc, pvem=rep(0,300), panal=obj3$panal, ptc=rep(0,300), asdc=obj3$asdc)/obj3$efec
tmp <- apply(tmp, 2, mean)
P <- rbind(P, tmp)


res <- tmp$res0612n0R; shift.v <- -.3
length(sample(res$BUGSoutput$sims.list$rho, size=300))
