rm(list = ls(all = TRUE)) #clear workspace
setwd("~/Dropbox/REDISTRICTING/MIXTURE_MODEL_2006")

## -----------------------
## Generate data
## -----------------------
data <- read.table("DIPUTADOS_2006.txt", head=TRUE, sep="\t")
data <- with(data, data.frame(
  PAN=PAN,
  APM=ALIANZA.POR.MÉXICO,
  PBT=POR.EL.BIEN.DE.TODOS,
  PANAL=NUEVA.ALIANZA,
  ALT=ALTERNATIVA,
  ABS=(LISTA.NOMINAL-TOTAL)+NULOS+NO.REGISTRADOS))

#Resultados observados (para grafica)
tmp <- c("PAN","APM","PBT","PANAL","ALT")
vote.obs <- apply(data[,tmp], 2, function(x) sum(x)/sum(data[,tmp]))
seats.obs <- apply(data[,tmp], 1, function(x) names(which.max(x)))
seats.obs <- table(seats.obs)

#District Size
Size <- rowSums(data)
#Porcentajes
data <- data/Size
#Log ratios
log.ratios <- log(data[,-ncol(data)]/data[,ncol(data)])

## -----------------------
## Density Estimation
## -----------------------
library(mclust)
mvnmix <- Mclust(cbind(Size, log.ratios), G=3)
print(mvnmix)

##Plots
pdf("density.pdf")
plot(mvnmix, "density", type="image", col=topo.colors(20))
dev.off()

pdf("classification.pdf", 10, 10)
plot(mvnmix, "classification", color=c("blue","red","yellow"), symbols=c(16,16,16))
dev.off()

## -----------------------
## Simulate
## -----------------------
sim.elec <- function(clust.obj, names, exclude.vote=NULL, exclude.seats=NULL, keep) {
  sims <- sim(modelName = clust.obj$modelName, 
              parameters = clust.obj$parameters, 
              n = 300)
  colnames(sims) <- c("group", names)
  ## Numero de votos
  vote <- exp(sims[,-(1:2)]) #Transformar a % (sin considera cluster y Size)
  vote <- apply(vote, 2, function(x) x/(1+rowSums(vote)))
  vote <- cbind(vote, REF=1-rowSums(vote)) # Partido de referencia
  vote <- vote*sims[,2] # Numero de votos
  ## Porcentaje total de votos
  foo <- vote
  if (!is.null(exclude.vote)) {
    exclude.vote <- which(colnames(foo) %in% exclude.vote) #Partidos que se excluyen del calculo
    foo <- foo[,-exclude.vote]
  }
  out.vote <- apply(foo, 2, function(x) sum(x)/sum(foo)) # % Votacion Nacional
  ## Numero de curules
  foo <- vote
  if (!is.null(exclude.seats)) {
    exclude.seats <- which(colnames(foo) %in% exclude.seats) #Partidos que se excluyen del calculo
    foo <- foo[,-exclude.seats]
  }
  seats <- apply(foo, 1, function(x) x==max(x)) # Numero de Curules
  out.seats <- apply(seats, 1, sum) 
  ## Return
  return(cbind(vote=out.vote[keep], seats=out.seats[keep]))
}

set.seed(12345)
names <- c("Size", names(log.ratios))
out.sims <- replicate(n=100000, 
                      sim.elec(mvnmix, 
                               names, 
                               exclude.vote="REF",
                               exclude.seats="REF",
                               keep=c("PAN","APM","PBT")), 
                      simplify=FALSE)

## -----------------------
## Summary
## -----------------------
summarize <- function(data) {
  rango <- range(data[,"vote"])
  breaks <- seq(round(rango[1], 2)-.005, round(rango[2], 2)+.005, .01)
  cuts <- cut(data[,"vote"], breaks=breaks)
  labels <- seq(breaks[1]+.005, breaks[length(breaks)]-.005, .01)
  foo <- by(data[,"seats"], cuts, function(x) 
    c(seats=quantile(x, .5), 
      mean=mean(x/300), 
      ci=quantile(x, c(.025, .975)),
      quantile(x/300, c(.025, .975))))
  foo <- do.call(rbind, foo)
  return(cbind(vote=labels, foo))
}

tmp <- do.call(rbind, out.sims)
tmp <- split(as.data.frame(tmp), rownames(tmp))
results <- lapply(tmp, summarize)
print(results)

## -----------------------
## Plot curve
## -----------------------
plot.curve <- function(data, col) {
  # Mean
  lines(data[,c("vote","mean")], lwd=2, col=col)
  # Confidence Interval
  polygon(c(data[,"vote"],rev(data[,"vote"])),
          c(data[,"2.5%"],rev(data[,"97.5%"])), 
          col=rgb(t(col2rgb(col)), alpha=70, max=255),border=NA)
}

pdf("local_curve.pdf", 5, 5)
par(bty="n")
  plot(1, type="n", xlim=c(.1,.6), ylim=c(.1,.6),
       xlab="% Votos", ylab="% Curules", main="Curva local de curules-votos")
  abline(0,1)
  text(0.45,0.45, expression("" %<-% "Repr. Prop."), pos=4, cex=.7)
  plot.curve(results[["PAN"]][-c(1,9),], col="royalblue3")
  plot.curve(results[["APM"]][-c(1,7),], col="firebrick")
  plot.curve(results[["PBT"]], col="gold3")
  points(vote.obs[c("PAN","APM","PBT")], seats.obs[c("PAN","APM","PBT")]/300, pch=16)
dev.off()


