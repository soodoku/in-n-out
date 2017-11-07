#
#  CCES 2006
#

# What is in the script that is also in the paper/presentation
# Table 2 + Robustness with covariates
# Table 3
# Figure 3 (Paper + Presentation)
# Errors in perceptions

# Notes
# Lower values of true vote mean more conservative
# aggregate(ccesp$sen1.meanvote, list(ccesp$sen1.party), mean, na.rm = T)
# pid7n: lower values = dem.

# Set Working dir.
setwd(githubdir)

# Load library
library(lme4)
library(goji)
library(plyr)
library(texreg)

# CCES data
source("in-n-out/scripts/cces_recode.R")

#load("in-n-out/data/cces06r_s.Rdata")

# Initialize New Variables
# Strong partisan
cces06$pidstr <- cces06$pid7n %in% c(0, 1) 

# Convenient subsets
# Only partisans
ccesp <- subset(cces06, pid3 == 'Republican' | pid3 == 'Democrat')
# Partisans + Only where senator data are available (cuts out 18 cases)
ccesp <- subset(ccesp, (!is.na(sen1.party) & !is.na(sen2.party)))

sociodem <- c("age", "female", "educ", "marital", "race")

# Kludgy way to wide to long
# ------------------------

a <- cces06[!is.na(cces06$pid3), c("sen1.approve", "cvote.sen1", "cself.vote", "vote.sen1",
	                               "self.vote", "pvote.sen1", "sen1.party", "sen1.inparty",
	                               "sen1.inpparty", "pid3", "pidstr", "sen1.dw1", sociodem)]
a <- cbind(a, a1 = rep(0, nrow(a)))
b <- cces06[!is.na(cces06$pid3), c("sen2.approve", "cvote.sen2", "cself.vote", "vote.sen2",
	                               "self.vote", "pvote.sen2", "sen2.party", "sen2.inparty",
	                               "sen2.inpparty","pid3", "pidstr", "sen2.dw1", sociodem)]
b <- cbind(b, a1 = rep(1, nrow(b)))
names(a) <- names(b)
comb <- rbind(a, b)

# Initialize new variables
# Two absolute diff. (rescaled 0 to 1)
comb$absdiff  <- with(comb, zero1(abs(cvote.sen2 - cself.vote)))
comb$absdiff2 <- with(comb, zero1(abs(vote.sen2 - self.vote)))
comb$absdiff3 <- with(comb, zero1(abs(pvote.sen2 - self.vote)))
comb$absdiff4 <- with(comb, zero1(abs(vote.sen2 - pvote.sen2)))

# Extremity
comb$extreme <- ifelse(comb$sen2.party == "Republican", zero1(-comb$vote.sen2), zero1(comb$vote.sen2))

# Sidedness
comb$sameside <- ((comb$pid3 == 'Republican' & comb$cvote.sen2 > comb$cself.vote) |
	              (comb$pid3 == 'Democrat' & comb$cvote.sen2 < comb$cself.vote))

comb$oppside  <- ((comb$pid3 == 'Republican' & comb$cvote.sen2 < comb$cself.vote) |
	              (comb$pid3 == 'Democrat' & comb$cvote.sen2 > comb$cself.vote))

comb$samesidep <- ((comb$pid3 == 'Republican' & comb$pvote.sen2 > comb$self.vote) |
	               (comb$pid3 == 'Democrat' & comb$pvote.sen2 < comb$cself.vote))

comb$oppsidep  <- ((comb$pid3 == 'Republican' & comb$pvote.sen2 < comb$self.vote) |
	               (comb$pid3 == 'Democrat' & comb$pvote.sen2 > comb$cself.vote))

# Partisans only
comb$partisans <- comb$pid3 != 'Independent'

# Long form subsets
combp <- subset(comb, comb$pid3 == 'Republican' | comb$pid3 == 'Democrat')
combi <- subset(comb, comb$pid3 == 'Independent')

# Appendix C1
# -----------------
# Replicates Table 3
l1 <- lmer(sen2.approve ~ sen2.inparty + partisans + absdiff + I(absdiff*sen2.inparty) + I(absdiff*partisans) + (1|a1), data = comb)
l2 <- lmer(sen2.approve ~ sen2.inparty + partisans + absdiff + I(absdiff*sen2.inparty) + I(absdiff*partisans) + age + as.factor(educ) + female + as.factor(race) + as.factor(marital) + (1|a1), data = comb)

out <- texreg(list(l1, l2), custom.coef.names = 
                            c("Intercept", "In-Party", "Out-Party", "Abs. Diff.", "Abs. Diff.*In-Party", "Abs. Diff.*Out-Party", "Age", 
                              "Educ.: HS or Less", "Educ.: Missing", "Educ.: Post-grad", "Educ.: Some College", "Female",
                              "Race: Black", "Race: Hispanic", "Race: Other", "Race: White", 
                               "Marital: Missing", "Marital: Other", "Marital: Single"), 
                             fontsize = "tiny", caption.above=T, leading.zero  =  F,
                             caption = "Relationship between approval and ideology", label = "tab:robustness")

# fileConn <- file("in-n-out/Appendtable.tex")
# writeLines(out, fileConn)
# close(fileConn)

# To output to Word (press)
htmlreg(list(l1, l2), file = "in-n-out/texreg.doc",
                      inline.css = FALSE, doctype = TRUE, html.tag = TRUE,
                      head.tag = TRUE, body.tag = TRUE, 
                      custom.coef.names = c("Intercept", "In-Party", "Out-Party", "Abs. Diff.", "Abs. Diff.*In-Party",
                       	"Abs. Diff.*Out-Party", "Age", 
                        "Educ.: HS or Less", "Educ.: Missing", "Educ.: Post-grad", "Educ.: Some College", "Female",
                        "Race: Black", "Race: Hispanic", "Race: Other", "Race: White", 
                        "Marital: Missing", "Marital: Other", "Marital: Single"), 
                      fontsize = "tiny", caption.above=T, leading.zero  =  F,
                      caption = "Relationship between approval and ideology", label = "tab:robustness")

l1 <- lmer(sen2.approve ~ partisans + sen2.inparty + I(absdiff3*sen2.inparty) + (1|a1), data = comb)
l2 <- lmer(sen2.approve ~ partisans + absdiff*sen2.inparty + age + as.factor(educ) + female + as.factor(race) + as.factor(marital) + (1|a1), data = comb)

# How to do this:
# aggregate pvote by unique dwnom, and PID of respondent
combpd <- subset(comb, comb$pid3 == 'Democrat')
combpr <- subset(comb, comb$pid3 == 'Republican')
a <- ddply(combpd, c("sen2.dw1"), summarise, mean = mean(pvote.sen2))[-95, ]
b <- ddply(combpr, c("sen2.dw1"), summarise, mean = mean(pvote.sen2))[-95, ]

# DW-Nom ranges for Ds
range(combp$sen2.dw1[combp$sen2.party == 'Democrat'], na.rm = T)
# -0.497 -0.216

# DW-Nom ranges for Rs
range(combp$sen2.dw1[combp$sen2.party == 'Republican'], na.rm = T)
#  -0.052  0.900

# Higher values mean more liberal
# How on average Dems. perceive Dems. - How on average Republicans perceive Dems.
a$mean[a$sen2.dw1 < -.052] - b$mean[a$sen2.dw1 < -.052]
# More Negative - Less Negative -.4 - (-.2) = Negative
a$mean[a$sen2.dw1 > -.051] - b$mean[a$sen2.dw1 > -.051]

cor(a$sen2.dw1[a$sen2.dw1 > 0], a$mean[a$sen2.dw1 > 0] - b$mean[a$sen2.dw1 > 0])


# Plot of Democratic Officials
# As they become more liberal, Democrats perceptions ought to be 
# As difference becomes greater, 
plot(a$sen2.dw1[a$sen2.dw1 < -.052], a$mean[a$sen2.dw1 < -.052] - b$mean[a$sen2.dw1 < -.052])
abline(lm(I(a$mean[a$sen2.dw1 < -.052] - b$mean[a$sen2.dw1 < -.052]) ~ a$sen2.dw1[a$sen2.dw1 < -.052]))

abline(lm(a$mean[a$sen2.dw1 < -.052] - b$mean[a$sen2.dw1 < -.052] ~ a$sen2.dw1[a$sen2.dw1 < -.052]))

# Plot of Democratic Officials
plot(a$sen2.dw1[a$sen2.dw1 < -.272], a$mean[a$sen2.dw1 < -.272], ylim=c(0,.75), col="blue")
points(a$sen2.dw1[a$sen2.dw1 < -.272], b$mean[a$sen2.dw1 < -.272], ylim=c(0,.75), col="red")
lines(abline(lm(a$mean[a$sen2.dw1 < -.272] ~ a$sen2.dw1[a$sen2.dw1 < -.272])), col="blue")
lines(abline(lm(b$mean[a$sen2.dw1 < -.272] ~ a$sen2.dw1[a$sen2.dw1 < -.272])), col="red")

plot(a$sen2.dw1[a$sen2.dw1 > -.052], a$mean[a$sen2.dw1 > -.052], ylim=c(-.75,0), col="blue")
points(a$sen2.dw1[a$sen2.dw1 > -.052], b$mean[a$sen2.dw1 > -.052], col="red")
lines(abline(lm(a$mean[a$sen2.dw1 > -.052] ~ a$sen2.dw1[a$sen2.dw1 > -.052])), col="blue")
lines(abline(lm(b$mean[a$sen2.dw1 > -.052] ~ a$sen2.dw1[a$sen2.dw1 > -.052])), col="red")


lines(lowess(a$mean[a$sen2.dw1 < -.252] ~a$sen2.dw1[a$sen2.dw1 < -.252], f=.5), col="blue")
lines(lowess(b$mean[a$sen2.dw1 < -.252] ~a$sen2.dw1[a$sen2.dw1 < -.252], f =.5), col="red")

tiff("in-n-out/figs/perceptions.tiff", width = 580)
par(mfrow = c(1, 2))
boxplot(-a$mean[a$sen2.dw1 > -.052], -b$mean[a$sen2.dw1 > -.052], names = c("Democrats", "Republicans"), main = "Republican Senators")
boxplot(-a$mean[a$sen2.dw1 < -.252], -b$mean[a$sen2.dw1 < -.252], names = c("Democrats", "Republicans"), main = "Democratic Senators")
dev.off()

abline(lm(I(a$mean[a$sen2.dw1 < -.052]) ~ a$sen2.dw1[a$sen2.dw1 < -.052]))
abline(lm(I(b$mean[a$sen2.dw1 < -.052]) ~ a$sen2.dw1[a$sen2.dw1 < -.052]))

# Plot
plot(a$sen2.dw1[a$sen2.dw1 > -.051], a$mean[a$sen2.dw1 > -.051])
abline(lm(I(a$mean[a$sen2.dw1 > -.051]) ~ a$sen2.dw1[a$sen2.dw1 > -.051]))
abline(lm(I(b$mean[a$sen2.dw1 > -.051]) ~ a$sen2.dw1[a$sen2.dw1 > -.051]))


# Table 2
# How do respondents account for actual distances
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Independents
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(lmer(sen2.approve ~ absdiff + (1|a1), data = combi))
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combi))

# Simulating counter-factuals
i <- lm(sen2.approve ~ absdiff, data = combi)
ip <- predict(i, data.frame(sen2.approver = combp$sen2.approve, absdiff = combp$absdiff))
mean(ip[combp$sen2.inparty == 1], na.rm = T)
mean(combp[combp$sen2.inparty == 1, "sen2.approve"], na.rm = T)
mean(ip[combp$sen2.inparty == 0], na.rm = T)

# Simulating counter-factuals
i <- lm(sen2.approve ~ absdiff3, data = combi)
ip <- predict(i, data.frame(sen2.approver=combp$sen2.approve, absdiff3 = combp$absdiff3))
mean(ip[combp$sen2.inparty == 1], na.rm = T)
mean(combp[combp$sen2.inparty == 1,"sen2.approve"], na.rm = T)
mean(ip[combp$sen2.inparty == 0], na.rm = T)
mean(combp[combp$sen2.inparty == 0,"sen2.approve"], na.rm = T)

# Joint
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(lmer(sen2.approve ~ absdiff*sen2.inparty + (1|a1), data = combp))
summary(lmer(sen2.approve ~ absdiff3*sen2.inparty + (1|a1), data = combp))

# In-Party
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(lmer(sen2.approve ~ absdiff + (1|a1), data = combp[combp$sen2.inparty == 1, ]))

# Directional Voting
# Sameside: Senator more conservative for Republicans; Senator more liberal for Democrats
summary(lmer(sen2.approve ~ absdiff + (1|a1), data = combp[combp$sen2.inparty == 1 & combp$sameside == 1, ]))

# Senator more liberal for Republicans; Senator more conservative for Democrats
summary(lmer(sen2.approve ~ absdiff + (1|a1), data = combp[combp$sen2.inparty == 1 & combp$oppside == 1, ]))

# Perceived Distances
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combp[combp$sen2.inpparty == 1, ]))
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combp[combp$sen2.inpparty == 1 & combp$samesidep == 1, ]))
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combp[combp$sen2.inpparty == 1 & combp$oppsidep == 1, ]))

# Out-Party
# ~~~~~~~~~~~~~~~~
summary(lmer(sen2.approve ~ absdiff + (1|a1), data = combp[combp$sen2.inparty == 0, ]))

# Directional Voting
# Sameside: Senator more conservative for Republicans; Senator more liberal for Democrats
summary(lmer(sen2.approve ~ absdiff + (1|a1), data = combp[combp$sen2.inparty == 0 & combp$sameside == 1, ]))

# Senator more liberal for Republicans; Senator more conservative for Democrats
summary(lmer(sen2.approve ~ absdiff + (1|a1), data = combp[combp$sen2.inparty == 0 & combp$oppside == 1, ]))

# Perceived Distances
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combp[combp$sen2.inpparty == 0, ]))
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combp[combp$sen2.inpparty == 0 & combp$samesidep == 1, ]))
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combp[combp$sen2.inpparty == 0 & combp$oppsidep == 1, ]))

# Plot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 0 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff), ]
plot(b$absdiff, b$sen2.approve, col="white", 
xlab = "Absolute Distance", 
ylab = "Feeling Thermometer")
lines(lowess(b$absdiff, b$sen2.approve))
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 1 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff), ]
lines(lowess(b$absdiff, b$sen2.approve))

# Perceived Differences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combi))
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combp[combp$sen2.inparty == 0, ]))
summary(lmer(sen2.approve ~ absdiff3 + (1|a1), data = combp[combp$sen2.inparty == 1, ]))

# Plotting Real and Perceived, Lowess and Linear
# Figure 2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# REAL DIFF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lowess plot
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Present: pdf("in-n-out/figs/realDiffApproveLowessPresent.pdf")
tiff("in-n-out/figs/realDiffApproveLowess.tiff")
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 0 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff), ]
plot(b$absdiff, b$sen2.approve, col = "white",
     xlab = "Actual Ideological Distance", 
     ylab = "Approval")
lines(lowess(b$absdiff, b$sen2.approve, f = 1), lty = 2, col = "#aacc00")
text(.07, .44, "Out-Party", col = "#aacc00", cex = .8)
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 1 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff), ]
lines(lowess(b$absdiff, b$sen2.approve, f = 1))
text(.1, .86, "In-Party", cex = .8)
b <- combi[!is.na(combi$sen2.approve) & !is.na(combi$absdiff), ]
lines(lowess(b$absdiff, b$sen2.approve, f = 1), lty = 3, col = "#00aacc")
text(.11, .66, "Independents", col = "#00aacc", cex = .8)
title("Lowess", cex = .9)
dev.off()

# Linear Reg. Plot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tiff("in-n-out/figs/realDiffApproveLinear.tiff")
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 0 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff), ]
outpart <- lm(b$sen2.approve ~ b$absdiff) 
plot(b$absdiff, b$sen2.approve, col = "white", 
     xlab = "Actual Ideological Distance",
     ylab = "Approval")
abline(outpart, col="#aacc00", lty = 2) 
text(.05, .49, "Out-Party", col = "#aacc00", cex = .8)
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 1 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff), ]
inpart<- lm(b$sen2.approve ~ b$absdiff) 
abline(inpart, lty = 1) 
text(.1, .81, "In-Party", cex = .8)
b <- combi[!is.na(combi$sen2.approve) & !is.na(combi$absdiff), ]
indpart <- lm(b$sen2.approve ~ b$absdiff) 
abline(indpart, col="#00aacc", lty = 3) 
text(.1, .68, "Independents", col = "#00aacc", cex = .8)
title("Linear Approximation", cex = .9)
dev.off()

# PERCEIVED DIFF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lowess plot
# ~~~~~~~~~~~~~~~~~~~~~~~~
pdf("in-n-out/figs/perceivedDiffApproveLowess.pdf")
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 0 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff3), ]
plot(b$absdiff3, b$sen2.approve, col="white", 
xlab = "Perceived Ideological Distance", 
ylab = "Approval")
lines(lowess(b$absdiff3, b$sen2.approve, f=1), lty=2, col="#aacc00")
text(.07, .44, "Out-Party", col = "#aacc00", cex = .8)
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 1 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff3), ]
lines(lowess(b$absdiff3, b$sen2.approve, f=1))
text(.1, .86, "In-Party", cex = .8)
b <- combi[!is.na(combi$sen2.approve) & !is.na(combi$absdiff3), ]
lines(lowess(b$absdiff3, b$sen2.approve, f=1), lty=3, col="#00aacc")
text(.11, .66, "Independents", col="#00aacc", cex=.8)
title("Lowess", cex=.9)
dev.off()

# Linear Reg. Plot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf("in-n-out/figs/perceivedDiffApproveLinear.pdf")
b<- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 0 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff3), ]
outpart <- lm(b$sen2.approve ~ b$absdiff3) 
plot(b$absdiff3, b$sen2.approve, col="white", 
xlab = "Perceived Ideological Distance", ylab = "Approval")
abline(outpart, col="#aacc00", lty=1) 
text(.05,.43, "Out-Party", col="#aacc00", cex=.8)
b <- combp[!is.na(combp$sen2.inparty) & combp$sen2.inparty == 1 & !is.na(combp$sen2.approve) & !is.na(combp$absdiff3), ]
inpart<- lm(b$sen2.approve ~ b$absdiff3) 
abline(inpart, lty=2) 
text(.1,.84, "In-Party", cex=.8)
b <- combi[!is.na(combi$sen2.approve) & !is.na(combi$absdiff3), ]
indpart<- lm(b$sen2.approve ~ b$absdiff3) 
abline(indpart, col="#00aacc", lty=3) 
text(.1,.65, "Independents", col="#00aacc", cex=.8)
title("Linear Approximation", cex=.9)
dev.off()


# The structure of errors in perception and their consequences
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hypotheses
# Greater random error for in-party
# More strongly negatively correlated amount of error with ideology for in-party
# More strongly negatively correlated directional error with ideology for in-party

# Findings
# Random error about same for in-party/out-party
# Amount of absolute error more strongly negatively correlated with ideology for in-party
# Greater directional error for in-party than out-party
# Amount of directional error ....

# Mean absolute error
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with(comb[comb$sen2.inparty == 1, ], mean(absdiff4, na.rm = T))
with(comb[comb$sen2.inparty == 0, ], mean(absdiff4, na.rm = T))

# Extent of directional error
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with(comb[comb$sen2.inparty == 1 & comb$pid3 == "Republican", ], mean(pvote.sen2 - vote.sen2, na.rm = T))
with(comb[comb$sen2.inparty == 0 & comb$pid3 == "Republican", ], mean(pvote.sen2 - vote.sen2, na.rm = T))
with(comb[comb$sen2.inparty == 1 & comb$pid3 == "Democrat", ],   mean(pvote.sen2 - vote.sen2, na.rm = T))
with(comb[comb$sen2.inparty == 0 & comb$pid3 == "Democrat", ],   mean(pvote.sen2 - vote.sen2, na.rm = T))

# Extremity and gross errors
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Expectation: Stronger correlation b/w extremity and errors for in-party than opposing party
# You see that in the data
# Similar/Stronger errors by 'perceived party) - also in the data

# By In/Out Party
with(comb[comb$sen2.inparty == 1, ], cor(absdiff4, extreme, use="na.or.complete"))
with(comb[comb$sen2.inparty == 0, ], cor(absdiff4, extreme, use="na.or.complete"))

# With perceived party the relationship reverses for out-party though rel. is weak (cool result)
with(comb[comb$sen2.inpparty == 1, ], cor(absdiff4, extreme, use="na.or.complete"))
with(comb[comb$sen2.inpparty == 0, ], cor(absdiff4, extreme, use="na.or.complete"))

# Also with DW-Nom
with(comb[comb$sen2.inpparty == 1, ], summary(lm(absdiff4 ~ abs(sen2.dw1))))
with(comb[comb$sen2.inpparty == 0, ], summary(lm(absdiff4 ~ abs(sen2.dw1))))
with(comb[comb$sen2.inpparty == 1, ], cor(absdiff4, abs(sen2.dw1), use="na.or.complete"))
with(comb[comb$sen2.inpparty == 0, ], cor(absdiff4, abs(sen2.dw1), use="na.or.complete"))

# By Party
with(comb[comb$pid3 == "Republican" & comb$sen2.inparty == 1, ], cor(absdiff4, extreme, use="na.or.complete"))
with(comb[comb$pid3 == "Republican" & comb$sen2.inparty == 0, ], cor(absdiff4, extreme, use="na.or.complete"))
with(comb[comb$pid3 == "Democrat"   & comb$sen2.inparty == 1, ], cor(absdiff4, extreme, use="na.or.complete"))
with(comb[comb$pid3 == "Democrat"   & comb$sen2.inparty == 0, ], cor(absdiff4, extreme, use="na.or.complete"))

# Extremity and directional errors
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pvote.sen2 - vote.sen2: as extremity increases, the extent to which people seen as more moderate shld increase

# more extreme the R senator, more pvote-cvote diff. (considered more moderate)
with(comb[comb$pid3 == "Republican" & comb$sen2.inparty == 1, ], cor(pvote.sen2 - cvote.sen2, abs(sen2.dw1), use="na.or.complete"))
with(comb[comb$pid3 == "Republican" & comb$sen2.inparty == 1, ], summary(lm(I(pvote.sen2 - cvote.sen2) ~ abs(sen2.dw1))))

# more extreme the D senator, less pvote-cvote diff.
# more moderate the D senator, more pvote-cvote diff.
# the more moderate the dem, the more extreme she is thought
with(comb[comb$pid3 == "Republican" & comb$sen2.inparty == 0, ], cor(pvote.sen2 - cvote.sen2, abs(sen2.dw1), use="na.or.complete"))

# more extreme the dem. senator, less pvote-cvote 
# negative
with(comb[comb$pid3 == "Democrat" & comb$sen2.inparty == 1, ], cor(pvote.sen2 - cvote.sen2, abs(sen2.dw1), use="na.or.complete"))

# more moderate the r senator, 
# negative
with(comb[comb$pid3 == "Democrat" & comb$sen2.inparty == 0, ], cor(pvote.sen2 - cvote.sen2, abs(sen2.dw1), use="na.or.complete"))

# Errors by sidedness
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# People make more errors when in-party elite is more extreme than them than when she is more moderate than them
with(combp[combp$samesidep == 1 & combp$sen2.inparty == 1, ], mean(absdiff4, na.rm = T))
with(combp[combp$oppsidep == 1 & combp$sen2.inparty == 1, ], mean(absdiff4, na.rm = T))

with(combp[combp$samesidep == 1 & combp$sen2.inparty == 0, ], mean(absdiff4, na.rm = T))
with(combp[combp$oppsidep == 1 & combp$sen2.inparty == 0, ], mean(absdiff4, na.rm = T))

# How do errors matter
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(lmer(sen2.approve ~ absdiff4 + (1|a1), data = combp[combp$sen2.inparty == 1, ]))
summary(lmer(sen2.approve ~ absdiff4 + (1|a1), data = combp[combp$sen2.inparty == 0, ]))

# How accurate are perceptions of party?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# People know more about Gov and Senators than Reps. 

# Representatives
# At least 33.6% get the party of representatives wrong
with(cces06, table(rep.pparty, rep.party, useNA = "always"))
# Extent to errors by in/out party
# Respondents are slightly less likely to know the party of the opposing party's representative
with(ccesp[ccesp$pid3 == 'Democrat', ],  table(rep.pparty, rep.party, useNA="always"))
# 23.6 (in-party, incorrect/total), vs. 24.8 (out-party, incorrect/total)
with(ccesp[ccesp$pid3 == 'Republican', ], table(rep.pparty, rep.party, useNA="always"))
# 20.01 vs. 23.03 (out-party, incorrect/total)

# Senators
# At least 16% don't know the party of Senators
# People getting incorrect/NA for Senator 1 = 5049
# People getting incorrect/NA for Senator 2 = 5237
# (5049 + 5237)/(31775*2) = 16.2%
t(with(ccesp, t(table(sen1.pparty, sen1.party, useNA="always"))/colSums(table(sen1.pparty, sen1.party, useNA="always"))))
t(with(ccesp, t(table(sen2.pparty, sen2.party, useNA="always"))/colSums(table(sen2.pparty, sen2.party, useNA="always"))))

# In and Out-party
# Respondents are slightly less likely to know the party of the opposing party's representative
a <- with(ccesp[ccesp$sen1.inparty == 1, ], table(sen1.pparty, sen1.party, useNA="always")) + with(ccesp[ccesp$sen2.inparty == 1, ], table(sen2.pparty, sen2.party, useNA="always"))
sum(a[2, ] + a[4, ] + 512+424)/(14495 + 13988 + sum(a[2, ] + a[4, ] + 512+424))
# 20.37%

b <- with(ccesp[ccesp$sen1.inparty == 0, ], table(sen1.pparty, sen1.party, useNA="always")) + with(ccesp[ccesp$sen2.inparty == 0, ], table(sen2.pparty, sen2.party, useNA="always"))
sum(b[2, ] + b[4, ] + 359+480)/(11901 + 12839 + sum(b[2, ] + b[4, ] + 359+480))
# ~ 21.12%

# Governors
# 13% screw up on gov. party
with(cces06, table(gov.pparty, gov.party, useNA="always"))


# Plotting Diff. b/w/ Voters and b/w Senators or voters and Senators
# Divergence across parties
pdf()
plot(density(ccesp$cself.vote[ccesp$pid3 == 'Republican'], bw=.25), col="red", ylim=c(0,1.1))
lines(density(ccesp$cself.vote[ccesp$pid3 == 'Democrat'], bw=.25), col="blue")

plot(density(comb$vote.sen2[!is.na(comb$sen2.party) & comb$sen2.party == 'Republican' & !is.na(comb$vote.sen2)], bw=.22), col="red", xlim=c(-2,1.8), ylim=c(0,1.4))
lines(density(comb$vote.sen2[!is.na(comb$sen2.party) &  comb$sen2.party == 'Democrat'], bw=.22), col="blue")
dev.off()
