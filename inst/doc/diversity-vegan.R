### R code from vignette source 'diversity-vegan.Rnw'

###################################################
### code chunk number 1: diversity-vegan.Rnw:21-26
###################################################
par(mfrow=c(1,1))
options(width=55) 
figset <- function() par(mar=c(4,4,1,1)+.1)
options(SweaveHooks = list(fig = figset))
options("prompt" = "> ", "continue" = "  ")


###################################################
### code chunk number 2: diversity-vegan.Rnw:58-60
###################################################
library(vegan)
data(BCI)


###################################################
### code chunk number 3: diversity-vegan.Rnw:78-79
###################################################
H <- diversity(BCI)


###################################################
### code chunk number 4: diversity-vegan.Rnw:86-87
###################################################
J <- H/log(specnumber(BCI))


###################################################
### code chunk number 5: diversity-vegan.Rnw:113-115
###################################################
k <- sample(nrow(BCI), 6)
R <- renyi(BCI[k,])


###################################################
### code chunk number 6: diversity-vegan.Rnw:122-123
###################################################
getOption("SweaveHooks")[["fig"]]()
print(plot(R))


###################################################
### code chunk number 7: diversity-vegan.Rnw:134-135
###################################################
alpha <- fisher.alpha(BCI)


###################################################
### code chunk number 8: diversity-vegan.Rnw:171-172
###################################################
quantile(rowSums(BCI))


###################################################
### code chunk number 9: diversity-vegan.Rnw:175-176
###################################################
Srar <- rarefy(BCI, min(rowSums(BCI)))


###################################################
### code chunk number 10: diversity-vegan.Rnw:184-185
###################################################
S2 <- rarefy(BCI, 2)


###################################################
### code chunk number 11: diversity-vegan.Rnw:189-190
###################################################
all(rank(Srar) == rank(S2))


###################################################
### code chunk number 12: diversity-vegan.Rnw:196-197
###################################################
range(diversity(BCI, "simp") - (S2 -1))


###################################################
### code chunk number 13: diversity-vegan.Rnw:258-262
###################################################
data(dune)
data(dune.taxon)
taxdis <- taxa2dist(dune.taxon, varstep=TRUE)
mod <- taxondive(dune, taxdis)


###################################################
### code chunk number 14: diversity-vegan.Rnw:265-266
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(mod)


###################################################
### code chunk number 15: diversity-vegan.Rnw:292-294
###################################################
tr <- hclust(taxdis, "aver")
mod <- treedive(dune, tr)


###################################################
### code chunk number 16: diversity-vegan.Rnw:316-319
###################################################
k <- sample(nrow(BCI), 1)
fish <- fisherfit(BCI[k,])
fish


###################################################
### code chunk number 17: diversity-vegan.Rnw:322-323
###################################################
getOption("SweaveHooks")[["fig"]]()
plot(fish)


###################################################
### code chunk number 18: diversity-vegan.Rnw:351-352
###################################################
prestondistr(BCI[k,])


###################################################
### code chunk number 19: diversity-vegan.Rnw:383-385
###################################################
rad <- radfit(BCI[k,])
rad


###################################################
### code chunk number 20: diversity-vegan.Rnw:388-389
###################################################
getOption("SweaveHooks")[["fig"]]()
print(radlattice(rad))


###################################################
### code chunk number 21: a
###################################################
sac <- specaccum(BCI)
plot(sac, ci.type="polygon", ci.col="yellow")


###################################################
### code chunk number 22: diversity-vegan.Rnw:458-459
###################################################
getOption("SweaveHooks")[["fig"]]()
sac <- specaccum(BCI)
plot(sac, ci.type="polygon", ci.col="yellow")


###################################################
### code chunk number 23: diversity-vegan.Rnw:487-488
###################################################
ncol(BCI)/mean(specnumber(BCI)) - 1


###################################################
### code chunk number 24: diversity-vegan.Rnw:505-507
###################################################
beta <- vegdist(BCI, binary=TRUE)
mean(beta)


###################################################
### code chunk number 25: diversity-vegan.Rnw:514-515
###################################################
betadiver(help=TRUE)


###################################################
### code chunk number 26: diversity-vegan.Rnw:533-535
###################################################
z <- betadiver(BCI, "z")
quantile(z)


###################################################
### code chunk number 27: diversity-vegan.Rnw:545-550
###################################################
data(dune)
data(dune.env)
z <- betadiver(dune, "z")
mod <- with(dune.env, betadisper(z, Management))
mod


###################################################
### code chunk number 28: diversity-vegan.Rnw:553-554
###################################################
getOption("SweaveHooks")[["fig"]]()
boxplot(mod)


###################################################
### code chunk number 29: diversity-vegan.Rnw:611-612
###################################################
specpool(BCI)


###################################################
### code chunk number 30: diversity-vegan.Rnw:617-619
###################################################
s <- sample(nrow(BCI), 25)
specpool(BCI[s,])


###################################################
### code chunk number 31: diversity-vegan.Rnw:630-631
###################################################
estimateR(BCI[k,])


###################################################
### code chunk number 32: diversity-vegan.Rnw:667-669
###################################################
veiledspec(prestondistr(BCI[k,]))
veiledspec(BCI[k,])


###################################################
### code chunk number 33: diversity-vegan.Rnw:683-684
###################################################
smo <- beals(BCI)


###################################################
### code chunk number 34: a
###################################################
j <- which(colnames(BCI) == "Ceiba.pentandra")
plot(beals(BCI, species=j, include=FALSE), BCI[,j], 
     main="Ceiba pentandra", xlab="Probability of occurrence",
     ylab="Occurrence")


###################################################
### code chunk number 35: diversity-vegan.Rnw:697-698
###################################################
getOption("SweaveHooks")[["fig"]]()
j <- which(colnames(BCI) == "Ceiba.pentandra")
plot(beals(BCI, species=j, include=FALSE), BCI[,j], 
     main="Ceiba pentandra", xlab="Probability of occurrence",
     ylab="Occurrence")


