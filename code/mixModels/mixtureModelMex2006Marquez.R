rm(list = ls(all = TRUE)) #clear workspace
setwd("~/Dropbox/REDISTRICTING/MIXTURE_MODEL_2006")

## ------------------------------------
## COMPUTOS DISTRITALES 2006 - DIPUTADO
## ------------------------------------
data <- read.table("DIPUTADOS_2006.txt", head=TRUE, sep="\t")
total.votos <- sum(data$TOTAL)
data <- data[,c("PAN","ALIANZA.POR.MÉXICO","POR.EL.BIEN.DE.TODOS",
                "NUEVA.ALIANZA","ALTERNATIVA")]

# Verificar que la info es correcta
table(apply(data, 1, function(x) names(data)[which.max(x)]))
# ...ok!

partidos <- c("PAN","APM","PBT","PANAL","ALT")
names(data) <- partidos
data$Total <- rowSums(data)

#Porcentajes
data[, partidos] <- data[, partidos] / data$Total
data <- data[, c("Total", partidos)]

## -----------------------
## Bias, 2006
## -----------------------
library(seatsvotes)
library(vcd)

# Mixtures
fit <- vector("list",1)
pdf("mvnmix.pdf")
fit[[1]] <- mvnmix(data,components=3,nrep=5,scatter=T,plot.ell=T)
dev.off()

# Verify fit of model to data.
pdf("marginals.pdf")
par(mfrow=c(2,3))
show.marginals(fit,numdraws=50000)
dev.off()

# Simulate swing ratios and plot results.
set.seed(12345)
pdf("swing.pdf")
sr <- swingratio(fit,sims=50000)
dev.off()
save("sr", file="simulations")

# Scatterplot matrix of simulated national-level vote and seat shares.
library(car)
pdf("sim.vm.pdf")
sim.vm <- as.data.frame(sr$votemat)
scatterplotMatrix(sim.vm[1:1000,],pch=1,diagonal="none",cex=1,lty=2,
                  smooth=F,cex.axis=1,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)
dev.off()

pdf("sim.sm.pdf")
sim.sm <- as.data.frame(sr$seatmat)
scatterplotMatrix(sim.sm[1:1000,c("PAN","APM","PBT")],pch=1,diagonal="none",cex=1,lty=2,
                  smooth=F,cex.axis=1,col=c("black",rep("gray50",ncol(sim.vm))),lwd=2)
dev.off()

# Plot effect of a 1% increase in the PRI national-level vote share.
sim.vm <- sim.vm[,c("APM","PAN","PBT","PANAL","ALT")]
y <- as.data.frame(t(t(sim.vm)-colMeans(sim.vm))) # demeaned simulated votes

pdf("increase.pri.pdf")
par(mfrow=c(4,1), mar=c(4,3,1.5,1.5))
for (i in 2:5) {
  plot(density(y[round(y[,1],3)==0.00,i],adjust=1.3),lwd=2,lty=2,
       main="",ylab="",xlab="",cex.axis=1.1,cex.lab=1.5,bty="n")
  lines(density(y[round(y[,1],3)==0.01,i],adjust=1.3),lwd=2)
  mtext(side=2,text="density",line=2,cex=0.8)
  mtext(side=1,text=names(y[i]),line=2.5,cex=1.1)
  abline(v=0,col="gray50",lwd=2,lty=2)
  abline(v=mean(y[round(y[,1],3)==0.01,i]),col="gray50",lwd=2)
}
dev.off()



