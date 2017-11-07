#   
#  Approval Data

# Set Directory
setwd(githubdir)

# Loading Functions and libraries
source("in-n-out/scripts/robustse.R")

# Load libs.
library(car)
library(goji)
library(lme4)
library(splines)

# Get Data
alldat <- read.csv(file = "in-n-out/data/polldwcf.csv")
alldat <- alldat[is.na(alldat$party) | alldat$party != '328', ]

# Add state ideology data
# Via Warshaw/Tausanovitch
wt     <- read.csv("in-n-out/data/states_estimates.csv")
wt     <- subset(wt, select = c("mrp_estimate", "mrp_sd", "abb", "raw_mean", "raw_sd", "dem_2008", "dem_2004"))
alldat <- merge(alldat, wt, by.x = "statecode", by.y = "abb", all.x = T, all.y = F)

wt2     <- read.csv("in-n-out/data/states_parties_estimates.csv")
wt2     <- subset(wt2, select = c("state_num", "dems.mean", "dems.sd", "reps.mean", "reps.sd", "ind.mean", "ind.sd", "abb"))
alldat  <- merge(alldat, wt2, by.x = "statecode", by.y = "abb", all.x = T, all.y = F)
 
# Recode
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fix the party situation
alldat$party <- car::recode(alldat$party, "100 = 'Republican';200 = 'Democrat'")
alldat$dem   <- alldat$party == 'Democrat'
alldat$rep   <- alldat$party == 'Republican'

# Year as Factor
alldat$yearf <- as.factor(alldat$year)

# Recode % Approving to range from 0 (=0) to 1 (=100)
alldat$rep.approver <- zero1(alldat$rep.approve, 0, 100)
alldat$dem.approver <- zero1(alldat$dem.approve, 0, 100)
alldat$ind.approver <- zero1(alldat$ind.approve, 0, 100)
alldat$in.approver  <- zero1(alldat$in.approve,  0, 100)
alldat$out.approver <- zero1(alldat$out.approve, 0, 100)

# Recode ideology to lie from most liberal (=0) to most conservative (=1) observed in the dataset
# For dwnom: most lib. sen to most con. sen
alldat$z1dw1<- zero1(alldat$dwnom1, -0.512, 0.900)
alldat$z1dw2<- zero1(alldat$dwnom2, -1.124, 0.977)

# Range for cfscore very close to range for senators so lets keep it
# range(alldat[!is.na(alldat$name) & alldat$type ==  "Senator","cfscore"], na.rm=T)
#  -1.426654  1.322548
alldat$z1cfscore  <- zero1(alldat$cfscore,  -1.426654, 1.334880)

# 0 to 1
alldat$z1absdw1    <- zero1(abs(alldat$dwnom1))
alldat$abscfscore  <- zero1(abs(alldat$cfscore))

# Position
alldat$type  <- relevel(factor(alldat$type, levels = c("President", "Governor", "Senator")), ref = "Senator")
alldat$senpg <- car::recode(alldat$type, "c('President', 'Governor') = 1; else = 0")

# No. of observations per state
alldat$nobsst <- unsplit(lapply(split(alldat$X, alldat$statename), function(a) length(a)), alldat$statename)

# Some Convenient Subsets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# No NA on senator
sena <- alldat[!is.na(alldat$name) & alldat$type ==  "Senator",]
# Democratic Senators only
send <- alldat[!is.na(alldat$name) & alldat$type ==  "Senator" & alldat$party ==  'Democrat',]
# Republican Senators only
senr <- alldat[!is.na(alldat$name) & alldat$type ==  "Senator" & alldat$party ==  'Republican',]

# GovPres
# Dems.
gpd <- alldat[!is.na(alldat$name) & alldat$type != "President" & alldat$party ==  'Democrat',]
# Republican GPs
gpr <- alldat[!is.na(alldat$name) & alldat$type != "President" & alldat$party ==  'Republican',]

# Democratic Reps only
alld <- alldat[!is.na(alldat$name) & alldat$party ==  'Democrat',]

# Republican Reps only
allr <- alldat[!is.na(alldat$name) & alldat$party ==  'Republican',]

length(unique(allr$pollid)) # [1] 1938
length(unique(alld$pollid)) # [1] 1538

# Analyses
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Just raw relationship between support and dw-nom
with(send, plot(dwnom1,dem.approver, col = "white", ylim = c(.2,1)))
with(send, lines(lowess(dwnom1, dem.approver)))
with(send, lines(lowess(dwnom1, rep.approver)))

with(senr[!is.na(senr$dwnom1),], plot(dwnom1,dem.approver, col = "white", ylim = c(.2,1)))
with(senr[!is.na(senr$dwnom1),], lines(lowess(dwnom1, dem.approver)))
with(senr[!is.na(senr$dwnom1),], lines(lowess(dwnom1, rep.approver)))

with(sena, plot(abs(dwnom1), seq(.2,.8,length.out=nrow(sena)), col = "white"))
with(sena[!is.na(sena$in.approver),], lines(lowess(abs(dwnom1), in.approver, f = 1)))
with(sena[!is.na(sena$in.approver),], lines(lowess(abs(dwnom1), out.approver, f = 1)))

with(send, plot(z1dw1, seq(0,1,length.out = nrow(send)), col = "white"))
with(send[!is.na(send$dem.approver),], lines(lowess(z1dw1, dem.approver, f = .9)))
with(send[!is.na(send$dem.approver),], lines(lowess(z1dw1, rep.approver, f = .9)))

with(senr, plot(z1dw1, seq(0,1,length.out = nrow(senr)), col = "white"))
with(senr[!is.na(senr$dem.approver) & !is.na(senr$dwnom1),], lines(lowess(z1dw1, dem.approver, f = .9)))
with(senr[!is.na(senr$dem.approver) & !is.na(senr$dwnom1),], lines(lowess(z1dw1, rep.approver, f = .9)))

# Linear Regression
with(sena[!is.na(sena$in.approver),], summary(lm(out.approver ~ zero1(abs(dwnom1)))))
with(sena[!is.na(sena$in.approver),], summary(lm(in.approver ~ zero1(abs(dwnom1)))))
# with splines

with(sena[!is.na(sena$in.approver),], summary(lm(in.approver ~ ns(zero1(abs(dwnom1)),2))))

# For Paper
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#pdf("in-n-out/figs/ApproveInOutDW.pdf")
tiff("in-n-out/figs/ApproveInOutDW.tiff")
par(mar = c(5,5,2,2)+0.1, bg = "white")
plot(abs(sena$dwnom1), 
seq(.2,.8,length.out = nrow(sena)), 
xlab = "Ideological Extremity", 
ylab = "Approval", 
type = "p",  
pch = 20, 
col = "#ffffff",
axes = F,
col.lab = "#222222",
yaxt = "n",
xaxt = "n")
axis(2, seq(.2, .8, .1), las = 1, tck = -.015,  col.axis = "#333333", col = '#333333', labels = nolead0s(round(seq(.2, .8, .1),2)))
axis(1, seq(0,1,.1), tck = -.015, col = '#333333', labels = nolead0s(round(seq(0, 1, .1),2)), col.axis = "#333333")
text(.8,.3,  "Out-Party",cex = .8, col = "#aacc00")
text(.8,.62, "In-Party", cex = .8,)
with(sena[!is.na(sena$in.approver),], lines(lowess(abs(dwnom1), in.approver, f = 1),  lwd = 2, lty = 1))
with(sena[!is.na(sena$in.approver),], lines(lowess(abs(dwnom1), out.approver, f = 1), lwd = 2, lty = 2, col = "#aacc00"))
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~````

# Support enjoyed by most extreme decile
mean(senr$rep.approver[senr$dwnom1 > .8], na.rm = T)
mean(send$dem.approver[send$dwnom1 < -.4], na.rm = T)


# Thinking about the model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# IdeologyApproval RApproval DState
# .770%40%DAlabama
# .730%20%California

# Support senator enjoys ~ 
# some base level + 
#state level differences in partisans + 
#senator level differences (ideology)*
#in or out party

# Three ways to deal with state level variation
# Ignoring state level differences
# State level differences are correlated with senator level differences
# They ought to inflate correlations between in-party Senator ideology and approval
# They are going to deflate correlations 

# One idea is to not model state level differences
# Fixed effects regression

# Another idea is to model state level differences
# One idea: use presidential approval as a covariate
# Presidential ideology mostly uncorrelated with state level 

# Ignoring partisan differences across states
# Using DW-Nominate
summary(with(senr, lmer(rep.approver ~ z1dw1 + yearf + (1|name) + (1|pollid))))
summary(with(senr, lmer(dem.approver ~ z1dw1 + yearf + (1|name) + (1|pollid))))
summary(with(senr, lmer(ind.approver ~ z1dw1 + yearf + (1|name) + (1|pollid))))
summary(with(send, lmer(rep.approver ~ z1dw1 + yearf + (1|name) + (1|pollid))))
summary(with(send, lmer(dem.approver ~ z1dw1 + yearf + (1|name) + (1|pollid))))
summary(with(send, lmer(ind.approver ~ z1dw1 + yearf + (1|name) + (1|pollid))))

# Interaction
summary(with(sena, lmer(rep.approver ~ z1dw1*rep + z1dw2 + yearf + (1|name) + (1|pollid))))
summary(with(sena, lmer(dem.approver ~ z1dw1*dem + z1dw2 + yearf + (1|name) + (1|pollid))))
ols(rep.approver  ~ z1dw1*rep + z1dw2  + yearf,sena, cluster = "name")
ols(dem.approver  ~ z1dw1*dem + z1dw2  + yearf,sena, cluster = "name")

# In and out
summary(with(sena, lmer(in.approver  ~ z1absdw1 + z1dw2 + yearf + (1|name) + (1|pollid))))
summary(with(sena, lmer(out.approver ~ z1absdw1 + z1dw2 + yearf + (1|name) + (1|pollid))))
summary(with(sena, lmer(in.approver  ~ abscfscore + yearf + (1|name) + (1|pollid))))
summary(with(sena, lmer(out.approver ~ abscfscore + z1dw2 + yearf + (1|name) + (1|pollid))))

# Not modeling state level differences
# Appendix C: Table C1 
summary(with(senr, lmer(rep.approver ~ z1dw1 + reps.mean + (1|statename) + (1|name) + (1|pollid))))
summary(with(senr, lmer(dem.approver ~ z1dw1 + dems.mean + (1|statename) + (1|name) + (1|pollid))))
summary(with(send, lmer(rep.approver ~ z1dw1 + reps.mean + (1|statename) + (1|name) + (1|pollid))))
summary(with(send, lmer(dem.approver ~ z1dw1 + dems.mean + (1|statename) + (1|name) + (1|pollid))))

# Random effects
summary(with(senr, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))
summary(with(senr, lmer(dem.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))
summary(with(senr, lmer(ind.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))
summary(with(send, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))
summary(with(send, lmer(dem.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))
summary(with(send, lmer(ind.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))

# Modeling across state differences
summary(with(senr, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + mrp_estimate + (1|statename) + (1|name) + (1|pollid))))
summary(with(senr, lmer(dem.approver ~ z1dw1 + z1dw2 + yearf + mrp_estimate + (1|statename) + (1|name) + (1|pollid))))
summary(with(senr, lmer(ind.approver ~ z1dw1 + z1dw2 + yearf + mrp_estimate + (1|statename) + (1|name) + (1|pollid))))
summary(with(send, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + mrp_estimate + (1|statename) + (1|name) + (1|pollid))))
summary(with(send, lmer(dem.approver ~ z1dw1 + z1dw2 + yearf + mrp_estimate + (1|statename) + (1|name) + (1|pollid))))
summary(with(send, lmer(ind.approver ~ z1dw1 + z1dw2 + yearf + mrp_estimate + (1|statename) + (1|name) + (1|pollid))))


# Fixed effects
# Table 1
# Cluster by name (95% there; se larger here)
ols(rep.approver ~ z1dw1 + statename, senr, cluster = "name")
ols(dem.approver ~ z1dw1 + statename, senr, cluster = "name")
ols(rep.approver ~ z1dw1 + statename, send, cluster = "name")
ols(dem.approver ~ z1dw1 + statename, send, cluster = "name")

print(with(senr, lmer(rep.approver ~ z1dw1 + statename + (1|name) + (1|pollid))))
print(with(senr, lmer(dem.approver ~ z1dw1 + statename + (1|name) + (1|pollid))))
print(with(send, lmer(rep.approver ~ z1dw1 + statename + (1|name) + (1|pollid))))
print(with(send, lmer(dem.approver ~ z1dw1 + statename + (1|name) + (1|pollid))))

# Table A1
ols(rep.approver ~ z1cfscore + statename,senr,cluster = "name")
ols(dem.approver ~ z1cfscore + statename,senr,cluster = "name")
ols(rep.approver ~ z1cfscore + statename,send,cluster = "name")
ols(dem.approver ~ z1cfscore + statename,send,cluster = "name")

print(with(senr, lmer(rep.approver ~ z1cfscore + statename + (1|name) + (1|pollid))))
print(with(senr, lmer(dem.approver ~ z1cfscore + statename + (1|name) + (1|pollid))))
print(with(send, lmer(rep.approver ~ z1cfscore + statename + (1|name) + (1|pollid))))
print(with(send, lmer(dem.approver ~ z1cfscore + statename + (1|name) + (1|pollid))))

# Table A2: Absolute Ideology/In and Out
ols(in.approver  ~ z1absdw1 + statename,sena,cluster = "name")
ols(out.approver ~ z1absdw1 + statename,sena,cluster = "name")

print(with(sena, lmer(in.approver  ~ z1absdw1 + statename + (1|name) + (1|pollid))))
print(with(sena, lmer(out.approver ~ z1absdw1 + statename + (1|name) + (1|pollid))))


# Additional robustness check for FE (also see how it compares to RE results below)
# Subsetting on states which contribute more than 2 Republican/Democratic Senators
senr$nsen <-unsplit(lapply(split(senr$name, senr$statename), function(a) length(unique(a))), senr$statename)
senr2 <- subset(senr, senr$nsen > 2)

send$nsen <-unsplit(lapply(split(send$name, send$statename), function(a) length(unique(a))), send$statename)
send2 <- subset(send, send$nsen > 2)

summary(with(senr2, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + as.factor(statename) + (1|name) + (1|pollid))))
summary(with(senr2, lmer(dem.approver ~ z1dw1 + z1dw2 + yearf + as.factor(statename) + (1|name) + (1|pollid))))
summary(with(send2, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + as.factor(statename) + (1|name) + (1|pollid))))
summary(with(send2, lmer(dem.approver ~ z1dw1 + z1dw2 + yearf + as.factor(statename) + (1|name) + (1|pollid))))

# Robustness Check: Among these states, re effects for diff. always more modest
summary(with(senr2, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))
summary(with(senr2, lmer(dem.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))
summary(with(send2, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))
summary(with(send2, lmer(dem.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid))))


# Using all the data
# Move to cfscore
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table 2
# Cluster by name (95% there; se larger here)
ols(rep.approver ~ z1cfscore + type + statename,allr,cluster = "name")
ols(dem.approver ~ z1cfscore + type + statename,allr,cluster = "name")
ols(rep.approver ~ z1cfscore + type + statename,alld,cluster = "name")
ols(dem.approver ~ z1cfscore + type + statename,alld,cluster = "name")

ols(rep.approver ~ z1cfscore*senpg + statename,allr,cluster = "name")
ols(dem.approver ~ z1cfscore*senpg + statename,allr,cluster = "name")
ols(rep.approver ~ z1cfscore*senpg + statename,alld,cluster = "name")
ols(dem.approver ~ z1cfscore*senpg + statename,alld,cluster = "name")

ols(in.approver  ~ abscfscore*senpg + statename,alldat,cluster = "name")
ols(out.approver ~ abscfscore*senpg + statename,alldat,cluster = "name")

summary(with(allr, lmer(rep.approver ~ z1cfscore + type  + (1|statename) + (1|name) + (1|pollid))))
summary(with(allr, lmer(dem.approver ~ z1cfscore + type  + (1|statename) + (1|name) + (1|pollid))))
summary(with(alld, lmer(rep.approver ~ z1cfscore + type  + (1|statename) + (1|name) + (1|pollid))))
summary(with(alld, lmer(dem.approver ~ z1cfscore + type  + (1|statename) + (1|name) + (1|pollid))))

# Appendix C: Table C2
summary(with(allr, lmer(rep.approver ~ z1cfscore*senpg  +  reps.mean + (1|statename) + (1|name) + (1|pollid))))
summary(with(allr, lmer(dem.approver ~ z1cfscore*senpg  +  dems.mean + (1|statename) + (1|name) + (1|pollid))))
summary(with(alld, lmer(rep.approver ~ z1cfscore*senpg  +  reps.mean + (1|statename) + (1|name) + (1|pollid))))
summary(with(alld, lmer(dem.approver ~ z1cfscore*senpg  +  dems.mean + (1|statename) + (1|name) + (1|pollid))))


summary(with(allr, lmer(rep.approver ~ zero1(cfscore)*senpg + as.factor(statename) + (1|name) + (1|pollid))))
summary(with(allr, lmer(dem.approver ~ zero1(cfscore)*senpg + as.factor(statename) + (1|name) + (1|pollid))))
summary(with(alld, lmer(dem.approver ~ zero1(cfscore)*senpg + as.factor(statename) + (1|name) + (1|pollid))))
summary(with(alld, lmer(rep.approver ~ zero1(cfscore)*senpg + as.factor(statename) + (1|name) + (1|pollid))))

# Senators Only
with(senr, lmer(rep.approver ~ zero1(cfscore) + as.factor(statename) + (1|name) + (1|pollid)))
with(senr, lmer(dem.approver ~ zero1(cfscore) + as.factor(statename) + (1|name) + (1|pollid)))
with(send, lmer(rep.approver ~ zero1(cfscore) + as.factor(statename) + (1|name) + (1|pollid)))
with(send, lmer(dem.approver ~ zero1(cfscore) + as.factor(statename) + (1|name) + (1|pollid)))

# Governors and Presidents only
with(gpr, lmer(rep.approver ~ zero1(cfscore) + as.factor(statename) + (1|name) + (1|pollid)))
with(gpr, lmer(dem.approver ~ zero1(cfscore) + as.factor(statename) + (1|name) + (1|pollid)))
with(gpd, lmer(rep.approver ~ zero1(cfscore) + as.factor(statename) + (1|name)))
with(gpd, lmer(dem.approver ~ zero1(cfscore) + as.factor(statename) + (1|name)))

ols(rep.approver ~ z1cfscore + statename,gpr,cluster = "name")


summary(with(allr, lmer(rep.approver ~ zero1(cfscore) + type + (1|statename) + (1|name) + (1|pollid))))
summary(with(allr, lmer(dem.approver ~ zero1(cfscore) + type + (1|statename) + (1|name) + (1|pollid))))
summary(with(alld, lmer(rep.approver ~ zero1(cfscore) + type + (1|statename) + (1|name) + (1|pollid))))
summary(with(alld, lmer(dem.approver ~ zero1(cfscore) + type + (1|statename) + (1|name) + (1|pollid))))

# Hierarchical model
ctherm <- c("dem.approver", "rep.approver")
# Subsetting on states which contribute more than 2 Republican/Democratic Senators
alldat$nsen <-unsplit(lapply(split(alldat$name, alldat$statename), function(a) length(unique(a))), alldat$statename)
alldatr <- subset(alldat, nsen > 3)

test.long<-reshape(alldatr, varying = list(ctherm), 
idvar = "X", 
v.names = c("ctherm"), 
timevar = "candno", 
times = 0:1, 
direction = "long")

summary(with(test.long, lmer(ctherm ~ z1dw1*rep*candno + yearf + (1|name) + (1|pollid))))
summary(with(test.long[test.long$rep ==  'TRUE',], lmer(ctherm ~ z1dw1*candno + yearf + (1|name) + (1|pollid))))


# Hausman's test for RE/FE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hausman <- function(fixed,random) {
rNames <- names(fixef(random))
fNames <- names(fixef(fixed))
timevarNames <- intersect(rNames,fNames)
k <- length(timevarNames)
rV <- vcov(random)
rownames(rV)=rNames
colnames(rV)=rNames
bDiff <- (fixef(random))[timevarNames] - fixef(fixed)[timevarNames]
vDiff <- vcov(fixed)[timevarNames,timevarNames] - rV[timevarNames,timevarNames]
(H <- t(bDiff) %*% solve(vDiff) %*% bDiff)
c(H=H,p.value=pchisq(as.numeric(H),k,lower.tail=FALSE))
}
# Table 1
a <- with(senr, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + statename + (1|name) + (1|pollid)))
b <- with(senr, lmer(rep.approver ~ z1dw1 + z1dw2 + yearf + (1|statename) + (1|name) + (1|pollid)))
hausman(a,b)