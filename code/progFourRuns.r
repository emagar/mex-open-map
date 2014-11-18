## requiere haber corrido ~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/code/analizaEscenarios2013.r hasta la definición de los cuatro modelos

# prepara frame para recibir estimaciones puntuales
tmp1 <- rep(NA, 4)
lambda.hat.s0 <- data.frame(pan=tmp1, pri=tmp1, prd=tmp1, pvem=tmp1, panal=tmp1, ptc=tmp1, asdc=tmp1)
rho.hat.s0 <- tmp1
rm(tmp1)


## calvo-micozzi with effective number of parties (data in vectors)
J <- 7; S <- tmp$seats; v <- tmp$votes; N <- tmp$N; d <- tmp[,paste("d", 1:J, sep="")]; pty <- tmp$pty; D <- tmp$ndis; I <- length(S); 

l.r.data <- list("S", "v", "I", "J", "pty", "N", "D")
l.r.inits <- function(){ list (lambda=rnorm(J), c=rnorm(1), rho=rexp(1)) }
l.r.parameters <- c("lambda", "c", "rho")

## test ride
tmpR <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.1, n.chains=3,
                 n.iter=100, n.thin=10
                 )
## estimate
cmns0 <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.1, n.chains=3,
                 n.iter=10000, n.thin=50
                 )


lambda.hat.s0[1,] <- cmns0$BUGSoutput$median$lambda
rho.hat.s0[1] <- cmns0$BUGSoutput$median$rho
c.hat.s0 <- cmns0$BUGSoutput$median$c

### FOR CALVO-MICOZZI VERSIONS GRAPH
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
mean.N <- mean(tmp$N[tmp$pty==1]) ## pan ran in all elections
equis <- seq(0,1,by=.01)
p <- 1; s.hat.pan <- antilogit(c.hat.s0 * log(mean.N) + lambda.hat.s0[1,p] + rho.hat.s0[1] * logit(equis))
p <- 2; s.hat.pri <- antilogit(c.hat.s0 * log(mean.N) + lambda.hat.s0[1,p] + rho.hat.s0[1] * logit(equis))
p <- 3; s.hat.prd <- antilogit(c.hat.s0 * log(mean.N) + lambda.hat.s0[1,p] + rho.hat.s0[1] * logit(equis))

## GRAPH COMMANDS HERE


## calvo-micozzi without effective number of parties (data in vectors)
J <- 7; S <- tmp$seats; v <- tmp$votes; N <- tmp$N; d <- tmp[,paste("d", 1:J, sep="")]; pty <- tmp$pty; D <- tmp$ndis; I <- length(S); 

l.r.data <- list("S", "v", "I", "J", "pty", "D")
l.r.inits <- function(){ list (lambda=rnorm(J), rho=rexp(1)) }
l.r.parameters <- c("lambda", "rho")

## test ride
tmpR <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.2, n.chains=3,
                 n.iter=100, n.thin=10
                 )
## estimate
cms0 <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.2, n.chains=3,
                 n.iter=10000, n.thin=50
                 )

lambda.hat.s0[2,] <- cms0$BUGSoutput$median$lambda
rho.hat.s0[2] <- cms0$BUGSoutput$median$rho

### FOR CALVO-MICOZZI VERSIONS GRAPH
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
equis <- seq(0,1,by=.01)
p <- 1; s.hat.pan <- antilogit(lambda.hat.s0[2,p] + rho.hat.s0[2] * logit(equis))
p <- 2; s.hat.pri <- antilogit(lambda.hat.s0[2,p] + rho.hat.s0[2] * logit(equis))
p <- 3; s.hat.prd <- antilogit(lambda.hat.s0[2,p] + rho.hat.s0[2] * logit(equis))

## GRAPH COMMANDS HERE





### king with all parties

####################################################################
### Data preparations for king (data matrix with 7 party columns ###
####################################################################
S <-          data.frame(pan=df2012s0$panw, pri=df2012s0$priw+df2012s0$pricw, prd=df2012s0$prdcw, pvem=df2012s0$pvemw, panal=df2012s0$panalw, ptc=rep(0,32),     asdc=rep(0,32))
S <- rbind(S, data.frame(pan=df2009s0$panw, pri=df2009s0$priw+df2009s0$pricw, prd=df2009s0$prdw,  pvem=df2009s0$pvemw, panal=df2009s0$panalw, ptc=df2009s0$ptcw, asdc=rep(0,32)))
S <- rbind(S, data.frame(pan=df2006s0$panw, pri=df2006s0$pricw,               prd=df2006s0$prdcw, pvem=rep(0,32),      panal=df2006s0$panalw, ptc=rep(0,32),     asdc=df2006s0$asdcw))
#
v <-          data.frame(pan=df2012s0$pansh, pri=df2012s0$prish+df2012s0$pricsh, prd=df2012s0$prdcsh, pvem=df2012s0$pvemsh, panal=df2012s0$panalsh, ptc=rep(0,32),     asdc=rep(0,32))
v <- rbind(v, data.frame(pan=df2009s0$pansh, pri=df2009s0$prish+df2009s0$pricsh, prd=df2009s0$prdsh,  pvem=df2009s0$pvemsh, panal=df2009s0$panalsh, ptc=df2009s0$ptcsh, asdc=rep(0,32)))
v <- rbind(v, data.frame(pan=df2006s0$pansh, pri=df2006s0$pricsh,               prd=df2006s0$prdcsh, pvem=rep(0,32),      panal=df2006s0$panalsh, ptc=rep(0,32),     asdc=df2006s0$asdcsh))
#
dummy <- v; dummy[,] <- 0; dummy[v>0] <- 1 # indicates parties with v>0
#
D <- c(df2012s0$ndis, df2009s0$ndis, df2006s0$ndis); 
N <- c(df2012s0$N, df2009s0$N, df2006s0$N);
I <- length(D);
J <- 7;
#
# changes S from share to number of seats
for (j in 1:J){
    S[,j] <- S[,j]*D
}

l.r.data <- list("S", "v", "I", "J", "D", "dummy")
l.r.inits <- function(){ list (lambda=rnorm(J), rho=rexp(1)) }
l.r.parameters <- c("lambda", "rho")


tmpR <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.3, n.chains=3,
                 n.iter=100, n.thin=10
                 )
## estimate
k7s0 <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.3, n.chains=3,
                 n.iter=10000, n.thin=50
                 )


lambda.hat.s0[3,] <- k7s0$BUGSoutput$median$lambda
rho.hat.s0[3] <- k7s0$BUGSoutput$median$rho

### king with pri excluded as referece party

####################################################################
### Data preparations for king (data matrix with 7 party columns ###
####################################################################
S <-          data.frame(pan=df2012s0$panw, pri=df2012s0$priw+df2012s0$pricw, prd=df2012s0$prdcw, pvem=df2012s0$pvemw, panal=df2012s0$panalw, ptc=rep(0,32),     asdc=rep(0,32))
S <- rbind(S, data.frame(pan=df2009s0$panw, pri=df2009s0$priw+df2009s0$pricw, prd=df2009s0$prdw,  pvem=df2009s0$pvemw, panal=df2009s0$panalw, ptc=df2009s0$ptcw, asdc=rep(0,32)))
S <- rbind(S, data.frame(pan=df2006s0$panw, pri=df2006s0$pricw,               prd=df2006s0$prdcw, pvem=rep(0,32),      panal=df2006s0$panalw, ptc=rep(0,32),     asdc=df2006s0$asdcw))
#
v <-          data.frame(pan=df2012s0$pansh, pri=df2012s0$prish+df2012s0$pricsh, prd=df2012s0$prdcsh, pvem=df2012s0$pvemsh, panal=df2012s0$panalsh, ptc=rep(0,32),     asdc=rep(0,32))
v <- rbind(v, data.frame(pan=df2009s0$pansh, pri=df2009s0$prish+df2009s0$pricsh, prd=df2009s0$prdsh,  pvem=df2009s0$pvemsh, panal=df2009s0$panalsh, ptc=df2009s0$ptcsh, asdc=rep(0,32)))
v <- rbind(v, data.frame(pan=df2006s0$pansh, pri=df2006s0$pricsh,               prd=df2006s0$prdcsh, pvem=rep(0,32),      panal=df2006s0$panalsh, ptc=rep(0,32),     asdc=df2006s0$asdcsh))
#
dummy <- v; dummy[,] <- 0; dummy[v>0] <- 1 # indicates parties with v>0
#
D <- c(df2012s0$ndis, df2009s0$ndis, df2006s0$ndis); 
N <- c(df2012s0$N, df2009s0$N, df2006s0$N);
I <- length(D);
J <- 7;
#
# changes S from share to number of seats
for (j in 1:J){
    S[,j] <- S[,j]*D
}

l.r.data <- list("S", "v", "I", "J", "D", "dummy")
l.r.inits <- function(){ list (lambda=rnorm(J-1), rho=rexp(1)) }
l.r.parameters <- c("lambda", "rho")


tmpR <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.4, n.chains=3,
                 n.iter=100, n.thin=10
                 )
## estimate
k6s0 <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.4, n.chains=3,
                 n.iter=10000, n.thin=50
                 )


lambda.hat.s0[4,] <- c(k6s0$BUGSoutput$median$lambda[1], 0, k6s0$BUGSoutput$median$lambda[2:6])
rho.hat.s0[4] <- k6s0$BUGSoutput$median$rho


lambda.hat.s0 <- round(lambda.hat.s0, digits=2)
rho.hat.s0 <- round(rho.hat.s0, digits=2)
c.hat.s0 <- round(c.hat.s0, digits=2)
rownames(lambda.hat.s0) <- c("cmn","cm","k7","k6")
names(rho.hat.s0) <- c("cmn","cm","k7","k6")


lambda.hat.s0[1,]
lambda.hat.s0[2,]

antilogit(lambda.hat.s0[1,] + c.hat.s0 * log(mean.N - 1))
lambda.hat.s0[3,] - lambda.hat.s0[3,2]
lambda.hat.s0[4,]



############################################
## king 1990 britain data and replication ##
############################################
uk <- data.frame(
yr=c(1950, 1951, 1955, 1959, 1964, 1966, 1970, 1974, 1974.8, 1979, 1983, 1987),
vtot=c(28771124, 28596594, 26759729, 27862652, 27657148, 27264747, 28344798, 31340162, 29189104, 31221362, 30671137, 32529600),
st=c(627, 629, 630, 630, 630, 630, 630, 635, 635, 635, 650, 650),
consv=c(12492404, 13718199, 13310891, 13750875, 12002642, 11418455, 13145123, 11872180, 10462565, 13697923, 13012316, 13760021),
conss=c(300, 325, 345, 365, 304, 253, 330, 297, 277, 339, 397, 376),
labv=c(13266176, 13948883, 12405254, 12216172, 12205808, 13096629, 12208758, 11645616, 11457079, 11532218, 8456934, 10019117),
labs=c(315, 295, 277, 258, 317, 364, 288, 301, 319, 269, 209, 229),
libv=c(2621487, 730546, 722402, 1640760, 3099283, 2327457, 2117035, 6059519, 5346704, 4313804, 7780949, 7351690),
libs=c(9, 6, 6, 6, 9, 12, 6, 14, 13, 11, 23, 22),
othv=c(391057, 198966, 321182, 254845, 349415, 422206, 873882, 1762847, 1922756, 1677417, 1420938, 1398772),
oths=c(3, 3, 2, 1, 0, 1, 6, 23, 26, 16, 22, 23),
age=c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1),
suspend=c(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
ulster=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
    )
#
colnames(uk)
uk$consv <- uk$consv / uk$vtot
uk$labv <- uk$labv / uk$vtot
uk$libv <- uk$libv / uk$vtot
uk$othv <- uk$othv / uk$vtot
## uk$conss <- uk$conss / uk$st
## uk$labs <- uk$labs / uk$st
## uk$libs <- uk$libs / uk$st
## uk$oths <- uk$oths / uk$st
#
lambda.rho.uk <- function() {
    ### likelihood King donde PRI es partido de referencia
    for (i in 1:I){ # loop over state-years
        for (j in 1:J){ # loop over parties (dummy will select those voted)
            S[i,j] ~ dbin(pi[i,j], D[i])  # D is the number of SMD seats in observation i's state
        }
        numerator[i,1] <- exp(             rho * log(v[i,1])  + ba * age[i] + bs * suspend[i] + bu * ulster[i] )
        for (j in 2:J){
            numerator[i,j] <- exp( lambda[j-1] + rho * log(v[i,j]) + ba * age[i] + bs * suspend[i] + bu * ulster[i] )
        }
        for (j in 1:J){
            d1[i,j] <- exp(             rho * log(v[i,1])  + ba * age[i] + bs * suspend[i] + bu * ulster[i] )
            d2[i,j] <- exp( lambda[1] + rho * log(v[i,2])  + ba * age[i] + bs * suspend[i] + bu * ulster[i] )
            d3[i,j] <- exp( lambda[2] + rho * log(v[i,3])  + ba * age[i] + bs * suspend[i] + bu * ulster[i] )
            d4[i,j] <- exp( lambda[3] + rho * log(v[i,4])  + ba * age[i] + bs * suspend[i] + bu * ulster[i] )
            denominator[i,j] <- d1[i,j]+d2[i,j]+d3[i,j]+d4[i,j]
            pi[i,j] <- numerator[i,j] / denominator[i,j]
        }
    }
    ### priors
    for (p in 1:3){ # there are 7 party labels in the 3-election data
        lambda[p] ~ dnorm( 0, tau.lambda )
    }
    tau.lambda <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
    ba ~ dnorm( 0 , tau.lambda )
    bs ~ dnorm( 0 , tau.lambda )
    bu ~ dnorm( 0 , tau.lambda )
}
#
### Data prep
S <- data.frame(cons=uk$conss, lab=uk$labs, lib=uk$libs, oth=uk$oths)
v <- data.frame(cons=uk$consv, lab=uk$labv, lib=uk$libv, oth=uk$othv)
D <- uk$st
age <- uk$age; suspend <- uk$suspend; ulster <- uk$ulster
I <- length(D);
J <- 4;
#
l.r.data <- list("S", "v", "I", "J", "D", "age", "suspend", "ulster")
l.r.inits <- function(){ list (lambda=rnorm(J-1), rho=rexp(1), ba=rnorm(1), bs=rnorm(1), bu=rnorm(1)) }
l.r.parameters <- c("lambda", "rho", "ba", "bs", "bu")
#
tmpR <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.uk, n.chains=3,
                 n.iter=100, n.thin=10
                 )

## estimate
kuk <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.uk, n.chains=3,
                 n.iter=100000, n.thin=500
                 )
rho.hat.uk <- kuk$BUGSoutput$median$rho
lambda.hat.uk <- c(1, kuk$BUGSoutput$median$lambda)
kuk$BUGSoutput$median$bs

save(kuk, k6s0, k7s0, cms0, cmns0, file="lambda.rho.estimates.RData")
