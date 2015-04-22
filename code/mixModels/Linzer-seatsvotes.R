##
## Drew Linzer
## dlinzer@emory.edu
## February 24, 2012
## 
## Linzer-seatsvotes.R
## 
## "The Relationship between Seats and Votes in Multiparty Systems"
## Political Analysis (2012).
## Archived at PA Dataverse, study ID hdl:1902.1/17691,
##   with the accompanying R package, "seatsvotes".
## 

library(seatsvotes)
library(vcd)

#####################################################################################

## Canada 1979.

data(Canada1979)
Canada1979$Other <- NULL
names(Canada1979) <- c("Votes Cast","Liberal","Prog. Conservative","New Democratic Party","Social Credit")
#names(Canada1979) <- c("Votes Cast","Liberal","PC","NDP","SC")

# Find patterns of party contestation.
ca79.split <- findpatterns(Canada1979)

# Estimate mixture models for each pattern of contestation.
fit79 <- list()
fit79[[1]] <- mvnmix(dat=ca79.split[[1]],components=2,nrep=5,scatter=T)
fit79[[2]] <- mvnmix(dat=ca79.split[[2]],components=2,nrep=5,scatter=T)

# Verify fit of model to data.
windows(4,8)
show.marginals(fit79,numdraws=50000)

# Simulate swing ratios and plot results.
windows(6,6)
sr79 <- swingratio(fit79,sims=50000)

# Scatterplot matrix of simulated national-level vote and seat shares.
#   Note, loading package "car" conflicts with package "ellipse" --
#   will need to re-start R to proceed with other countries.
library(car)
windows()
sim.vm <- as.data.frame(sr79$votemat)
scatterplotMatrix(sim.vm[1:1000,],pch=19,diagonal="none",cex=0.4,lty=2,
                  var.labels=c("\n\n\n\nLiberal","\n\n\nProgressive\n Conservative",
                               "\n\n\n\nNDP","\n\n\n\nSocial Credit"),
                  smooth=F,cex.axis=1.5,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)

windows()
sim.sm <- as.data.frame(sr79$seatmat)
scatterplotMatrix(sim.sm[1:1000,],pch=19,diagonal="none",cex=0.4,lty=2,
                  var.labels=c("\n\n\n\nLiberal","\n\n\nProgressive\n Conservative",
                               "\n\n\n\nNDP","\n\n\n\nSocial Credit"),
                  smooth=F,cex.axis=1.5,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)



# Plot effect of a 1% increase in the Liberal Party national-level vote share.
y <- as.data.frame(t(t(sim.vm)-colMeans(sim.vm))) # demeaned simulated votes
names(y) <- c("Liberal","Progressive Conservative","New Democratic Party","Social Credit")

windows(7.5,4.5)
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




#####################################################################################

## U.S. 1900-2006.

library(JudgeIt)

# Replication of Gelman and King AJPS 1994, Figure 2:
# "Responsiveness in the U.S. House, Non-South," 1900-1992

data(US.elections)

unc <- function(inp) { -1*(inp<0.05)+1*(inp>0.95) }
same.dists <- rep(1,length(US.elections))
same.dists[seq(2,length(US.elections),by=5)] <- 0

jres <- judgeit(model.formula=Democrat~unc(Democrat),
                vote.formula=Total~1,
                data=US.elections,
                use.last.votes=T,
                same.districts=same.dists,
                subset=SOUTH==0)

jit.sr <- NULL
for (yr in seq(1900,2006,2)) {
    br <- bias.resp(jres,year=yr)
    jit.sr <- c(jit.sr,br$svsums[4,1])
}

windows(9,4)
par(mar=c(4.5,4,0.5,0.5),las=1)
plot(seq(1900,2006,2),jit.sr,xlim=c(1900,2008.5),ylim=c(0,3.5),type="l",lwd=2,bty="n",
     cex.lab=1.3,cex.axis=1.3,col="darkgray",xlab="Year",ylab="Swing Ratio",xaxt="n")
axis(1,seq(1900,2010,10),seq(1900,2010,10),cex.axis=1.3)


# Compare results from Gelman & King (1994).
# Differences (especially after 1966) are due to [1] a more complete and
#   accurate data file; and [2] not using incumbency in the model formula.

#GK94resp <- c(3.25,3.2,2.3,3.1,3.25,4.1,2.9,3.3,3.3,2.7,1.7,2.9,1.75,1.4,
#              1.9,2.25,3.35,3.4,3.05,3.15,3.1,2.65,2.9,2.6,3.25,2.5,2.35,
#              2.55,2.4,2.8,2.6,2.35,2.55,1.8,1.25,1.3,1.35,2.1,1.35,1.3,
#              1.4,1.6,0.85,0.75,0.5,1.15,1.65)  
#lines(seq(1900,1992,2),GK94resp,lty=3)


# Multiparty seats-votes model, applied to same data.
swing.ns <- NULL
for (i in c(1:length(US.elections))) {
    cat("\n \n \n ***",seq(1900,2006,2)[i],"*** \n \n")
    us.dat <- US.elections[[i]]
    # remove NA districts
    us.dat <- us.dat[rowSums(is.na(us.dat))==0,]
    # remove at-large election districts
    us.dat <- us.dat[substr(rownames(us.dat),nchar(rownames(us.dat)),nchar(rownames(us.dat))) %in% c(0:9),]
    # remove southern districts
    us.dat <- us.dat[us.dat$SOUTH==0,]
    us.dat$SOUTH <- NULL
    us.dat$Republican <- 1-us.dat$Democrat
    # recode districts where a party receives less than 5% of two-party vote as uncontested
    us.dat[us.dat<0.05] <- 0
    us.split <- findpatterns(us.dat)
    fit <- list()
    if (length(us.split)==2) {
        fit[[1]] <- mvnmix(dat=us.split[[1]])
        fit[[2]] <- mvnmix(dat=us.split[[2]],components=3,nrep=10,scatter=F)
    } else {
        fit[[1]] <- mvnmix(dat=us.split[[1]])
        fit[[2]] <- mvnmix(dat=us.split[[2]])
        fit[[3]] <- mvnmix(dat=us.split[[3]],components=3,nrep=10,scatter=F)
    }
    sr <- swingratio(fit,sims=10000,graph=F)
    swing.ns <- c(swing.ns,sr$swing[1])
    lines(seq(1900,1898+2*length(swing.ns),2),swing.ns,lwd=2)
}





#####################################################################################

# If previously loaded "car" package, need to exit R and re-start here.
library(seatsvotes)

## U.K. 1955-1997.

data(UK.elections)

## 1955 ##

uk55 <- UK.elections$'1955'
uk55 <- data.frame(Total=uk55$Total,
                   Labour=uk55$Labour.Party+uk55$Northern.Ireland.Labour.Party,
                   Conservative=uk55$Conservatives+uk55$Unionists,
                   Liberal=uk55$Liberals,
                   Regional=uk55$Sinn.F+uk55$Plaid.Cymru+
                            uk55$Irish.Labour+uk55$Scottish.National.Party)

uk55.split <- findpatterns(uk55)

fit55 <- list()
fit55[[1]] <- mvnmix(dat=uk55.split[[1]])
fit55[[2]] <- mvnmix(dat=uk55.split[[2]],components=1,nrep=1,scatter=T)
fit55[[3]] <- mvnmix(dat=uk55.split[[3]])
fit55[[4]] <- mvnmix(dat=uk55.split[[4]],components=1,nrep=1,scatter=T)
fit55[[5]] <- mvnmix(dat=uk55.split[[6]],components=2,nrep=10,scatter=T)
fit55[[6]] <- mvnmix(dat=uk55.split[[7]],components=1,nrep=1,scatter=T)
fit55[[7]] <- mvnmix(dat=uk55.split[[8]],components=2,nrep=10,scatter=T)

#show.marginals(fit55,numdraws=5000)

windows(6,6)
sr55 <- swingratio(fit55,sims=10000)



## 1959 ##

uk59 <- UK.elections$'1959'
uk59 <- data.frame(Total=uk59$Total,
                   Labour=uk59$Labour.Party+uk59$Northern.Ireland.Labour.Party,
                   Conservative=uk59$Conservatives+uk59$Unionists,
                   Liberal=uk59$Liberals,
                   Regional=uk59$Plaid.Cymru+uk59$Sinn.F+uk59$Scottish.National.Party)

uk59.split <- findpatterns(uk59)

fit59 <- list()
fit59[[1]] <- mvnmix(dat=uk59.split[[1]],components=1,nrep=1,scatter=T)
fit59[[2]] <- mvnmix(dat=uk59.split[[2]])
fit59[[3]] <- mvnmix(dat=uk59.split[[5]],components=2,nrep=10,scatter=T)
fit59[[4]] <- mvnmix(dat=uk59.split[[6]],components=1,nrep=1,scatter=T)
fit59[[5]] <- mvnmix(dat=uk59.split[[7]],components=2,nrep=10,scatter=T)
fit59[[6]] <- mvnmix(dat=uk59.split[[8]],components=1,nrep=1,scatter=T)

#show.marginals(fit59,numdraws=5000)

windows(6,6)
sr59 <- swingratio(fit59,sims=10000)



## 1964 ##

uk64 <- UK.elections$'1964'
uk64 <- data.frame(Total=uk64$Total,
                   Labour=uk64$Labour.Party+uk64$Northern.Ireland.Labour.Party,
                   Conservative=uk64$Conservatives+uk64$Unionists,
                   Liberal=uk64$Liberals,
                   Regional=uk64$Independent.Republicans+uk64$Plaid.Cymru+uk64$Scottish.National.Party)

uk64.split <- findpatterns(uk64)

fit64 <- list()
fit64[[1]] <- mvnmix(dat=uk64.split[[2]],components=2,nrep=10,scatter=T)
fit64[[2]] <- mvnmix(dat=uk64.split[[3]],components=1,nrep=1,scatter=T)
fit64[[3]] <- mvnmix(dat=uk64.split[[4]],components=2,nrep=10,scatter=T)
fit64[[4]] <- mvnmix(dat=uk64.split[[5]],components=1,nrep=1,scatter=T)

#show.marginals(fit64,numdraws=5000)

windows(6,6)
sr64 <- swingratio(fit64,sims=10000)



## 1966 ##

uk66 <- UK.elections$'1966'
uk66 <- data.frame(Total=uk66$Total,
                   Labour=uk66$Labour.Party+uk66$Northern.Ireland.Labour.Party,
                   Conservative=uk66$Conservatives+uk66$Unionists,
                   Liberal=uk66$Liberals,
                   Regional=uk66$Scottish.National.Party+uk66$Independent.Republicans+uk66$Plaid.Cymru)

uk66.split <- findpatterns(uk66)

fit66 <- list()
fit66[[1]] <- mvnmix(dat=uk66.split[[1]])
fit66[[2]] <- mvnmix(dat=uk66.split[[2]],components=1,nrep=1,scatter=T)
fit66[[3]] <- mvnmix(dat=uk66.split[[5]])
fit66[[4]] <- mvnmix(dat=uk66.split[[6]],components=2,nrep=10,scatter=T)
fit66[[5]] <- mvnmix(dat=uk66.split[[7]],components=1,nrep=1,scatter=T)
fit66[[6]] <- mvnmix(dat=uk66.split[[8]],components=2,nrep=10,scatter=T)
fit66[[7]] <- mvnmix(dat=uk66.split[[9]],components=1,nrep=1,scatter=T)

#show.marginals(fit66,numdraws=5000)

windows(6,6)
sr66 <- swingratio(fit66,sims=10000)



## 1970 ##

uk70 <- UK.elections$'1970'
uk70 <- data.frame(Total=uk70$Total,
                   Labour=uk70$Labour.Party+uk70$Northern.Ireland.Labour.Party,
                   Conservative=uk70$Conservatives+uk70$Unionists,
                   Liberal=uk70$Liberals,
                   Regional=uk70$Scottish.National.Party+uk70$Plaid.Cymru+uk70$Unity)

uk70.split <- findpatterns(uk70)

fit70 <- list()
fit70[[1]] <- mvnmix(dat=uk70.split[[1]])
fit70[[2]] <- mvnmix(dat=uk70.split[[2]],components=1,nrep=1,scatter=T)
fit70[[3]] <- mvnmix(dat=uk70.split[[4]])
fit70[[4]] <- mvnmix(dat=uk70.split[[6]],components=1,nrep=1,scatter=T)
fit70[[5]] <- mvnmix(dat=uk70.split[[7]],components=1,nrep=1,scatter=T)
fit70[[6]] <- mvnmix(dat=uk70.split[[8]],components=3,nrep=10,scatter=T)
fit70[[7]] <- mvnmix(dat=uk70.split[[9]],components=1,nrep=1,scatter=T)

#show.marginals(fit70,numdraws=5000)

windows(6,6)
sr70 <- swingratio(fit70,sims=10000)



## 1974 (Februrary) ##

uk74 <- UK.elections$'1974a'
uk74 <- data.frame(Total=uk74$Total,
                   Labour=uk74$Labour.Party+uk74$Northern.Ireland.Labour.Party,
                   Conservative=uk74$Conservatives,
                   Liberal=uk74$Liberals,
                   Regional=uk74$Scottish.National.Party+uk74$Unionists+uk74$Plaid.Cymru+
                            uk74$Social.Democratic.and.Labour.Party+
                            uk74$Vanguard.Unionist.Progressive.Party+
                            uk74$Unionists.Pro.Assembly+uk74$Democratic.Unionist.Party)

uk74.split <- findpatterns(uk74)

fit74 <- list()
fit74[[1]] <- mvnmix(dat=uk74.split[[1]])
fit74[[2]] <- mvnmix(dat=uk74.split[[2]],components=1,nrep=1,scatter=T)
fit74[[3]] <- mvnmix(dat=uk74.split[[3]],components=1,nrep=1,scatter=T)
fit74[[4]] <- mvnmix(dat=uk74.split[[4]],components=2,nrep=10,scatter=T)
fit74[[5]] <- mvnmix(dat=uk74.split[[5]],components=3,nrep=10,scatter=T)
fit74[[6]] <- mvnmix(dat=uk74.split[[6]],components=1,nrep=1,scatter=T)

#show.marginals(fit74,numdraws=5000)

windows(6,6)
sr74 <- swingratio(fit74,sims=10000)



## 1974 (November) ##

uk75 <- UK.elections$'1974b'
uk75 <- data.frame(Total=uk75$Total,
                   Labour=uk75$Labour.Party+uk75$Northern.Ireland.Labour.Party,
                   Conservative=uk75$Conservatives,
                   Liberal=uk75$Liberals,
                   Regional=uk75$Scottish.National.Party+uk75$Ulster.Unionist.Party+
                            uk75$Plaid.Cymru+uk75$Social.Democratic.and.Labour.Party+
                            uk75$Vanguard.Unionist.Progressive.Party+
                            uk75$Democratic.Unionist.Party+uk75$Alliance.Party+
                            uk75$Unity+uk75$Unionist.Party.of.Northern.Ireland)

uk75.split <- findpatterns(uk75)

fit75 <- list()
fit75[[1]] <- mvnmix(dat=uk75.split[[1]])
fit75[[2]] <- mvnmix(dat=uk75.split[[2]],components=1,nrep=1,scatter=T)
fit75[[3]] <- mvnmix(dat=uk75.split[[6]],components=2,nrep=10,scatter=T)
fit75[[4]] <- mvnmix(dat=uk75.split[[7]],components=2,nrep=10,scatter=T)

#show.marginals(fit75,numdraws=5000)

windows(6,6)
sr75 <- swingratio(fit75,sims=10000)



## 1979 ##

uk79 <- UK.elections$'1979'
uk79 <- data.frame(Total=uk79$Total,
                   Labour=uk79$Labour.Party+uk79$Northern.Ireland.Labour.Party,
                   Conservative=uk79$Conservatives,
                   Liberal=uk79$Liberals,
                   Regional=uk79$Scottish.National.Party+uk79$Ulster.Unionist.Party+
                            uk79$Plaid.Cymru+uk79$Social.Democratic.and.Labour.Party+
                            uk79$Democratic.Unionist.Party+uk79$Alliance.Party+
                            uk79$United.Ulster.Unionist.Council+uk79$Independent.Unionists)

uk79.split <- findpatterns(uk79)

fit79 <- list()
fit79[[1]] <- mvnmix(dat=uk79.split[[1]])
fit79[[2]] <- mvnmix(dat=uk79.split[[2]],components=1,nrep=1,scatter=T)
fit79[[3]] <- mvnmix(dat=uk79.split[[3]],components=1,nrep=1,scatter=T)
fit79[[4]] <- mvnmix(dat=uk79.split[[4]],components=2,nrep=10,scatter=T)
fit79[[5]] <- mvnmix(dat=uk79.split[[5]],components=3,nrep=10,scatter=T)
fit79[[6]] <- mvnmix(dat=uk79.split[[6]],components=2,nrep=10,scatter=T)

#show.marginals(fit79,numdraws=5000)

windows(6,6)
sr79 <- swingratio(fit79,sims=10000)



## 1983 ##

uk83 <- UK.elections$'1983'
uk83 <- data.frame(Total=uk83$Total,
                   Labour=uk83$Labour.Party,
                   Conservative=uk83$Conservatives,
                   Alliance=uk83$Liberals+uk83$Social.Democratic.Party,
                   Regional=uk83$Scottish.National.Party+uk83$Ulster.Unionist.Party+
                            uk83$Democratic.Unionist.Party+uk83$Social.Democratic.and.Labour.Party+
                            uk83$Plaid.Cymru+uk83$Sinn.F+uk83$Alliance.Party+
                            uk83$Ulster.Popular.Unionist.Party)

uk83.split <- findpatterns(uk83)

fit83 <- list()
fit83[[1]] <- mvnmix(dat=uk83.split[[1]],components=1,nrep=1,scatter=T)
fit83[[2]] <- mvnmix(dat=uk83.split[[2]],components=3,nrep=10,scatter=T)
fit83[[3]] <- mvnmix(dat=uk83.split[[3]],components=2,nrep=10,scatter=T)

#show.marginals(fit83,numdraws=5000)

windows(6,6)
sr83 <- swingratio(fit83,sims=10000)



## 1987 ##

uk87 <- UK.elections$'1987'
uk87 <- data.frame(Total=uk87$Total,
                   Labour=uk87$Labour.Party,
                   Conservative=uk87$Conservatives,
                   Alliance=uk87$Liberals+uk87$Social.Democratic.Party,
                   Regional=uk87$Scottish.National.Party+uk87$Ulster.Unionist.Party+
                            uk87$Social.Democratic.and.Labour.Party+uk87$Plaid.Cymru+
                            uk87$Democratic.Unionist.Party+uk87$Sinn.F+uk87$Alliance.Party)

uk87.split <- findpatterns(uk87)

fit87 <- list()
fit87[[1]] <- mvnmix(dat=uk87.split[[1]],components=1,nrep=1,scatter=T)
fit87[[2]] <- mvnmix(dat=uk87.split[[2]],components=3,nrep=10,scatter=T)
fit87[[3]] <- mvnmix(dat=uk87.split[[3]],components=2,nrep=10,scatter=T)

#show.marginals(fit87,numdraws=5000)

windows(6,6)
sr87 <- swingratio(fit87,sims=10000)



## 1992 ##

uk92 <- UK.elections$'1992'
uk92 <- data.frame(Total=uk92$Total,
                   Labour=uk92$Labour.Party,
                   Conservative=uk92$Conservatives,
                   LibDem=uk92$Liberal.Democrat.Party,
                   Regional=uk92$Scottish.National.Party+uk92$Ulster.Unionist.Party+
                            uk92$Social.Democratic.and.Labour.Party+uk92$Plaid.Cymru+
                            uk92$Democratic.Unionist.Party+uk92$Sinn.F+uk92$Alliance.Party)

uk92.split <- findpatterns(uk92)

fit92 <- list()
fit92[[1]] <- mvnmix(dat=uk92.split[[1]],components=1,nrep=1,scatter=T)
fit92[[2]] <- mvnmix(dat=uk92.split[[2]],components=1,nrep=1,scatter=T)
fit92[[3]] <- mvnmix(dat=uk92.split[[3]],components=1,nrep=1,scatter=T)
fit92[[4]] <- mvnmix(dat=uk92.split[[4]],components=5,nrep=20,scatter=T)
fit92[[5]] <- mvnmix(dat=uk92.split[[5]],components=3,nrep=10,scatter=T)

#show.marginals(fit92,numdraws=5000)

windows(6,6)
sr92 <- swingratio(fit92,sims=10000)



## 1997 ##

uk97 <- UK.elections$'1997'
uk97 <- data.frame(Total=uk97$Total,
                   Labour=uk97$Labour.Party,
                   Conservative=uk97$Conservatives,
                   LibDem=uk97$Liberal.Democrat.Party,
                   Regional=uk97$Scottish.National.Party+uk97$Ulster.Unionist.Party+
                            uk97$Social.Democratic.and.Labour.Party+uk97$Plaid.Cymru+
                            uk97$Democratic.Unionist.Party+uk97$Sinn.F)
uk97.split <- findpatterns(uk97)

fit97 <- list()
fit97[[1]] <- mvnmix(dat=uk97.split[[1]])
fit97[[2]] <- mvnmix(dat=uk97.split[[2]])
fit97[[3]] <- mvnmix(dat=uk97.split[[3]],components=1,scatter=T)
fit97[[4]] <- mvnmix(dat=uk97.split[[4]])
fit97[[5]] <- mvnmix(dat=uk97.split[[7]],components=3,nrep=10,scatter=T)
fit97[[6]] <- mvnmix(dat=uk97.split[[8]],components=3,nrep=20,scatter=T)

#show.marginals(fit97,numdraws=10000)

windows(6,6)
sr97 <- swingratio(fit97,sims=10000)



# Assemble results.
UKresults <- list(sr55,sr59,sr64,sr66,sr70,sr74,sr75,sr79,sr83,sr87,sr92,sr97)

swing <- NULL
swing.lm <- NULL
votes <- NULL
seats <- NULL
for (i in 1:length(UKresults)) {
    swing <- rbind.fill(swing,as.data.frame(t(UKresults[[i]]$swing)))
    swing.lm <- rbind.fill(swing.lm,as.data.frame(t(UKresults[[i]]$swing.lm)))
    votes <- rbind.fill(votes,as.data.frame(t(UKresults[[i]]$truevote)))
    seats <- rbind.fill(seats,as.data.frame(t(UKresults[[i]]$trueseat)))
}

swing[is.na(swing)] <- swing.lm[is.na(swing)]

proc <- function(dat) {
    dat[is.na(dat)] <- 0
    dat$LibDem <- dat$LibDem+dat$Liberal+dat$Alliance
    dat$Liberal <- NULL
    dat$Alliance <- NULL
    return(dat)
}

swing <- proc(swing)
votes <- proc(votes)
seats <- proc(seats)



# Compare vote shares to swing ratios for elections in and after 1974.
# (Discussed but not shown in paper)
windows(6,6)
par(mar=c(5,4,0.5,0.5))
year <- c(1955,1959,1964,1966,1970,1973.5,1974.5,1979,1983,1987,1992,1997)
plot(votes$Conservative[year>1970],swing$Conservative[year>1970],
     xlim=c(0,0.55),ylim=c(0,4),xlab="Vote share",ylab="Swing ratio",cex=1.5,las=1,lwd=2,cex.lab=1.3,cex.axis=1.3)
points(votes$Labour[year>1970],swing$Labour[year>1970],pch=2,cex=1.5,lwd=2)
points(votes$LibDem[year>1970],swing$LibDem[year>1970],pch=3,cex=1.5,lwd=2)
points(votes$Regional[year>1970],swing$Regional[year>1970],pch=4,cex=1.5,lwd=2)
legend(0,4,pch=c(1,2,3,4),cex=1.2,pt.cex=1.5,pt.lwd=2,ncol=1,bty="n",
       legend=c("Conservative","Labour","Liberal Democrats","Regional Parties"))



# Liberals became SDP-Liberal Alliance in 1983 & 1987, then renamed Liberal Democrats in 1992.
year <- c(1955,1959,1964,1966,1970,1973.5,1974.5,1979,1983,1987,1992,1997)
windows(9,5)
par(mar=c(5,4,0.5,0.5))
plot(year,swing$Conservative,type="b",cex=1.5,lwd=2,ylim=c(0,4.5),las=1,cex.axis=1.3,cex.lab=1.3,
       col="black",xlab="Year",ylab="Swing Ratio",xaxt="n",bty="n")
axis(1,at=c(1955,1959,1964,1966,1970,1974,1974,1979,1983,1987,1992,1997),
       labels=c(1955,1959,"1964   ",1966,1970,1974,1974,1979,1983,1987,1992,1997),cex.axis=1.3)
axis(1,at=c(1966.5),labels=1966,cex.axis=1.3,tick=F)
points(year,swing$Labour,type="b",cex=1.5,lwd=2,pch=2,lty=2)
points(year,swing$LibDem,type="b",cex=1.5,lwd=2,pch=3,lty=3)
points(year,swing$Regional,type="b",cex=1.5,lwd=2,pch=4,lty=4)
legend(1964,4.6,pch=c(1,2,3,4),cex=1.2,pt.cex=1.5,pt.lwd=2,ncol=2,bty="n",
       legend=c("Conservative","Labour","Liberal Democrats","Regional Parties"))





#####################################################################################

## Botswana 2004.

data(Botswana2004)
bots04 <- Botswana2004

bots04$BNF <- bots04$BNF + bots04$BPP + bots04$BAM  # electoral alliance
bots04$BPP <- NULL
bots04$BAM <- NULL
bots04$Indep <- NULL
bots04$Mels <- NULL
bots04$NDF <- NULL

bots04.split <- findpatterns(bots04)

fit04 <- list()
fit04[[1]] <- mvnmix(dat=bots04.split[[1]])
fit04[[2]] <- mvnmix(dat=bots04.split[[2]],components=1,scatter=T)
fit04[[3]] <- mvnmix(dat=bots04.split[[4]],components=1,scatter=T)

show.marginals(fit04,numdraws=10000)

windows(6,6)
sr04 <- swingratio(fit04,sims=10000)


# What if the BNF and BCP joined forces?

bots04b <- bots04
bots04b$BNF <- bots04b$BNF + bots04b$BCP
bots04b$BCP <- NULL

bots04b.split <- findpatterns(bots04b)

fit04b <- list()
fit04b[[1]] <- mvnmix(dat=bots04b.split[[1]])
fit04b[[2]] <- mvnmix(dat=bots04b.split[[2]],components=2,nrep=10,scatter=T)

show.marginals(fit04b,numdraws=10000)

windows(6,6)
sr04b <- swingratio(fit04b,sims=10000)




# end of file.
