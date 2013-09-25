### R code from vignette source 'decision-vegan.Rnw'

###################################################
### code chunk number 1: decision-vegan.Rnw:21-26
###################################################
figset <- function() par(mar=c(4,4,1,1)+.1)
options(SweaveHooks = list(fig = figset))
options("prompt" = "> ", "continue" = "  ")
options(width = 55) 
require(vegan)


###################################################
### code chunk number 2: decision-vegan.Rnw:50-61
###################################################
getOption("SweaveHooks")[["fig"]]()
data(sipoo)
mod <- nestedtemp(sipoo)
plot(mod, "i")
x <- mod$c["Falcsubb"]
y <- 1 - mod$r["Svartholm"]
points(x,y, pch=16, cex=1.5)
abline(x+y, -1, lty=2)
f <- function(x, p) (1-(1-x)^p)^(1/p)
cross <- function(x, a, p) f(x,p) - a + x
r <- uniroot(cross, c(0,1), a = x+y, p = mod$p)$root
arrows(x,y, r, f(r, mod$p), lwd=4)


###################################################
### code chunk number 3: decision-vegan.Rnw:418-422
###################################################
library(vegan)
data(varespec)
data(varechem)
orig <- cca(varespec ~ Al + K, varechem)


###################################################
### code chunk number 4: a
###################################################
plot(orig, dis=c("lc","bp"))


###################################################
### code chunk number 5: decision-vegan.Rnw:431-432
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(orig, dis=c("lc","bp"))


###################################################
### code chunk number 6: decision-vegan.Rnw:441-443
###################################################
i <- sample(nrow(varespec))
shuff <- cca(varespec[i,] ~ Al + K, varechem)


###################################################
### code chunk number 7: decision-vegan.Rnw:446-447
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(shuff, dis=c("lc","bp"))


###################################################
### code chunk number 8: a
###################################################
plot(procrustes(scores(orig, dis="lc"), 
                scores(shuff, dis="lc")))


###################################################
### code chunk number 9: decision-vegan.Rnw:460-461
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(procrustes(scores(orig, dis="lc"), 
                scores(shuff, dis="lc")))


###################################################
### code chunk number 10: decision-vegan.Rnw:469-472
###################################################
tmp1 <- rda(varespec ~ Al + K, varechem)
i <- sample(nrow(varespec)) # Different shuffling
tmp2 <- rda(varespec[i,] ~ Al + K, varechem)


###################################################
### code chunk number 11: decision-vegan.Rnw:475-477
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(procrustes(scores(tmp1, dis="lc"), 
                scores(tmp2, dis="lc")))


###################################################
### code chunk number 12: decision-vegan.Rnw:494-496
###################################################
orig
shuff


###################################################
### code chunk number 13: decision-vegan.Rnw:501-502
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(procrustes(orig, shuff))


###################################################
### code chunk number 14: decision-vegan.Rnw:515-520
###################################################
tmp1 <- rda(varespec ~ ., varechem)
tmp2 <- rda(varespec[i,] ~ ., varechem)
proc <- procrustes(scores(tmp1, dis="lc", choi=1:14), 
                   scores(tmp2, dis="lc", choi=1:14))
max(residuals(proc))


###################################################
### code chunk number 15: decision-vegan.Rnw:532-535
###################################################
data(dune)
data(dune.env)
orig <- cca(dune ~ Moisture, dune.env)


###################################################
### code chunk number 16: decision-vegan.Rnw:540-541
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(orig, dis="lc")


###################################################
### code chunk number 17: a
###################################################
plot(orig, display="wa", type="points")
ordispider(orig, col="red")
text(orig, dis="cn", col="blue")


###################################################
### code chunk number 18: decision-vegan.Rnw:565-566
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(orig, display="wa", type="points")
ordispider(orig, col="red")
text(orig, dis="cn", col="blue")


