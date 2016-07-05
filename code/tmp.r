#################################################################
# plot 95% CIs of Linzer sims with swing ratio regression lines #
#################################################################
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/")  # where to save and retrieve objects
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/") # raw data directory
pd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/graphs/appendixPlots") # where to save plots
load(paste(dd, "swingRatios9715.RData", sep = ""))
#
# 2003
tmp <- swRats$df2003d97 # object to manipulate/analyze
tmp$truevote
#
# pri
tmpPty <- 1
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2003, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2003 PRI", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# pan
tmpPty <- 2
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2003, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2003 PAN", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# left
tmpPty <- 3
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2003, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2003 PRD", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# 2006
tmp <- swRats$df2006d0 # object to manipulate/analyze
tmp$truevote
#
# pri
tmpPty <- 1
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2006, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2006 PRI", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# pan
tmpPty <- 2
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2006, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2006 PAN", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# left
tmpPty <- 3
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2006, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2006 PRD", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# 2009
tmp <- swRats$df2009d0 # object to manipulate/analyze
tmp$truevote
#
# pri
tmpPty <- 1
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2009, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2009 PRI", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# pan
tmpPty <- 2
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2009, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2009 PAN", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# left
tmpPty <- 3
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2009, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2009 PRD", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# 2012
tmp <- swRats$df2012d0 # object to manipulate/analyze
tmp$truevote
#
# pri
tmpPty <- 1
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2012, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2012 PRI", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# pan
tmpPty <- 2
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2012, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2012 PAN", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# left
tmpPty <- 3
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2012, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2012 PRD", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# 2015
tmp <- swRats$df2015d0 # object to manipulate/analyze
tmp$truevote
#
# pri
tmpPty <- 1
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2015, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2015 PRI", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# pan
tmpPty <- 2
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2015, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2015 PAN", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()
#
# left
tmpPty <- 3
tmpTrue <- round(tmp$truevote[tmpPty],2)
tmpDummy <- as.numeric(tmp$votemat[,tmpPty] < (tmpTrue + .001) & tmp$votemat[,tmpPty] > (tmpTrue - .001))
tmpRange <- quantile(tmp$seatmat[,tmpPty][tmpDummy==1], probs=c(.025, .975)) - mean(tmp$seatmat[,tmpPty])
#
tmpMod <- lm(tmp$seatmat[,tmpPty] ~ tmp$votemat[,tmpPty])
pdf(file = paste (pd, "/95ciSwR-", 2015, tmpPty, ".pdf", sep = ""))
plot(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], main = "2015 PRD", xlab = "vote share", ylab = "seat share", type = "n")
points(tmp$votemat[,tmpPty], tmp$seatmat[,tmpPty], pch = 19, col = "gray")
#points(tmp$truevote[tmpPty], tmp$trueseat[tmpPty], pch = 19, col = "black")
abline(tmpMod)
abline(a = tmpMod$coefficients[1] + tmpRange[1], b = tmpMod$coefficients[2], lty = 2)
abline(a = tmpMod$coefficients[1] + tmpRange[2], b = tmpMod$coefficients[2], lty = 2)
dev.off()


