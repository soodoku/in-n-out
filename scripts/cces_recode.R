#
#   CCES 2006 Recode
#

# Set Working dir.
setwd(githubdir)

# Load libs
library(ltm)
library(goji)
library(car)
library(foreign)

# CCES data
load("in-n-out/data/cces_2006_common.rdata")
cces06 <- x

# Sociodem
# ------------
# CC06_V2018 Education
# CC06_V2019 Marital Status
# CC06_V2020 Birth Year
# CC06_V2004 Gender
# CC06_V2005 Race

cces06$female  <- cces06$v2004 == 'Female'
cces06$race    <- car::recode(as.character(cces06$v2005), "c('Middle Eastern','Native American', 'Mixed')='Other'; NA='Missing'")
cces06$age     <- 2006 - as.numeric(as.character(cces06$v2020))
cces06$marital <- car::recode(as.character(cces06$v2019), "NA='Missing'; c('Divorced', 'Domestic partnership', 'Separated', 'Widowed')='Other'")
cces06$educ    <- car::recode(as.character(cces06$v2018), "NA='Missing'; c('No HS', 'High school graduate')='HS or Below'; c('Some college', '2-year')='Some college'")

# Party ID
# v3005: pid3 3 Point Party ID
# v3006: 3 Point Party ID - other

# pid7 7 Point Party ID
cces06$pid7  <- car::recode(cces06$v3007, "'Not sure'=NA")
cces06$pid7n <- zero1(as.numeric(car::recode(cces06$v3007, "'Strong Democrat'=1; 'Weak Democrat'=2; 'Lean Democrat'=3;'Independent'=4; 'Lean Republican'=5;'Weak Republican'=6;'Strong Republican'=7;'Not sure'=NA")))
cces06$pid3  <- car::recode(cces06$pid7n, "0:.49='Democrat';.5='Independent';.5:1='Republican';8=NA")

# Group membership
# We can scale people based on these also
# nrl - National Right to Life
# cca - Christian Coalition of America
# vfw - Veterans of Foreign Wars
# lwv - League of Women Voters
to.names   <- c("aaa", "aarp", "nra", "lwv", "pta", "vfw", "legion", "sierra", "naral", "nrl", "cca")
from.names <- paste0("v40", 50:60)
cces06[ , to.names] <- sapply(cces06[ , from.names], function(x) as.numeric(car::recode(x, "'Yes' = 1; 'No' = 0; c('Skipped', 'Not Asked', 99) = NA")) - 1)

# Candidates and Actual Reps. Both covered
# Candidate Info.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CC06_V5001 House Candidate 1 - Name
# CC06_V5003 House Candidate 2 - Name
# CC06_V5005 Senate Candidate 1 - Name
# CC06_V5007 Senate Candidate 2 - Name
# CC06_V5009 Governor Candidate 1 - Name
# CC06_V5011 Governor Candidate 2 - Name
# CC06_V5004 House Candidate 2 - Party ID
# CC06_V5006 Senate Candidate 1 - Party ID
# CC06_V5008 Senate Candidate 2 - Party ID
# CC06_V5010 Governor Candidate 1 - Party ID
# CC06_V5012 Governor Candidate 2 - Party ID

# Rep. Names
# CC06_V5013 Representative - Name
# CC06_V5015 Senator 1 - Name
# CC06_V5017 Senator 2 - Name
# CC06_V5019 Governor - Name

# Actual Party of Rep.
# CC06_V5014 Representative - Party ID
# CC06_V5016 Senator 1 - Party ID
# CC06_V5018 Senator 2 - Party ID
# CC06_V5020 Governor - Party ID
from.names <- paste0("v50", seq(14,20,2))
to.names   <- paste0(c("rep", "sen1", "sen2", "gov"), ".party")
cces06[ , to.names] <- sapply(cces06[, from.names], function(x) car::recode(x, '0 = NA'))

# Party of Rep. Recalled
# CC06_V3012 - Governor Party Recall
# CC06_V3014 - Senator 1 Party Recall
# CC06_V3016 - Senator 2 Party Recall
# CC06_V3018 - Rep. Party Recall
from.names <- paste0("v30", seq(12,18,2))
to.names   <- paste0(c("gov", "sen1", "sen2", "rep"), ".pparty")
cces06[ , to.names] <- sapply(cces06[ , from.names], function(x) car::recode(x, 'c("Don\'t know") = NA'))

# Actual Senators
# Get DW-Nominate
# Load DW-Nominate data (senators only)
dwnom <- foreign::read.dta("in-n-out/data/dwnominate.senate.DTA")
dwnom<- subset(dwnom, cong > 107 & cong < 110)

dwnom$sencces <- tolower(dwnom$name)
dwnom$sencces[grep("kennedy", tolower(dwnom$name))] <- "kennedy"
dwnom$sencces[grep("byrd",   tolower(dwnom$name))]  <- "byrd"
dwnom$sencces[grep("levin",  tolower(dwnom$name))]  <- "levin"
dwnom$sencces[grep("kerry",  tolower(dwnom$name))]  <- "kerry"
dwnom$sencces[grep("smith",  tolower(dwnom$name))]  <- "smith"

# Standardize CCES names
# Senator 1 and 2
cces06$v5015l <- tolower(cces06$v5015)
cces06$v5017l <- tolower(cces06$v5017)

# Senator 1
cces06$sen1.last <- sapply(strsplit(cces06$v5015l, split = " "), function(x) tail(x, 1))
# Fix Biden 
cces06$sen1.last[grep("jr.", cces06$sen1.last)] <- "biden"

# Senator 2
cces06$sen2.last <- sapply(strsplit(cces06$v5017l, split = " "), function(x) tail(x, 1))
# Fix Rockefeller
cces06$sen2.last[grep("iv", cces06$sen2.last)] <- "rockefeller"

# get DW-Nom. Data
cces06$sen1.dw1  <- dwnom$dwnom1[match(cces06$sen1.last, dwnom$sencces)]
cces06$sen2.dw1  <- dwnom$dwnom1[match(cces06$sen2.last, dwnom$sencces)]

cces06$sencces.1 <- dwnom$sencces[match(cces06$sen1.last, dwnom$sencces)]
cces06$sencces.2 <- dwnom$sencces[match(cces06$sen2.last, dwnom$sencces)]

cces06$sen1.dwp  <- dwnom$party[match(cces06$sen1.last, dwnom$sencces)]
cces06$sen2.dwp  <- dwnom$party[match(cces06$sen2.last, dwnom$sencces)]

# Validation
# all senators
table(cces06$sen1.last[is.na(cces06$sen1.dw1)])
table(cces06$sen2.last[is.na(cces06$sen2.dw1)])
table(cces06$sen1.party, cces06$sen1.dwp)
aggregate(cces06$sen1.dw1, list(cces06$sen1.party), mean, na.rm = T)

# In and out party
# Based on Actual
cces06$sen1.inparty <- with(cces06, ifelse((!is.na(sen1.party) & sen1.party ==  'Democrat' & pid3 == 'Democrat') | 
	                                       (!is.na(sen1.party) & sen1.party ==  'Republican' & pid3 == 'Republican'), 1, 0))
cces06$sen2.inparty <- with(cces06, ifelse((!is.na(sen2.party) & sen2.party ==  'Democrat' & pid3 == 'Democrat') | 
	                                       (!is.na(sen2.party) & sen2.party ==  'Republican' & pid3 == 'Republican'), 1, 0))
cces06$gov.inparty  <- with(cces06, ifelse((!is.na(gov.party) & gov.party ==  'Democrat' & pid3 == 'Democrat')  | 
	                                       (!is.na(gov.party) & gov.party ==  'Republican' & pid3 == 'Republican'), 1, 0))
cces06$rep.inparty  <- with(cces06, ifelse((!is.na(rep.party) & rep.party ==  'Democrat' & pid3 == 'Democrat')  | 
	                                       (!is.na(rep.party) & rep.party ==  'Republican' & pid3 == 'Republican'), 1, 0))

cces06$sen1.inparty[is.na(cces06$sen1.party) | is.na(cces06$pid3)] <- NA
cces06$sen2.inparty[is.na(cces06$sen2.party) | is.na(cces06$pid3) | cces06$sen2.party == 'Independent'] <- NA
cces06$gov.inparty[is.na(cces06$gov.party) | is.na(cces06$pid3)] <- NA
cces06$rep.inparty[is.na(cces06$rep.party) | is.na(cces06$pid3)] <- NA

# Based on Perceived
cces06$sen1.inpparty <- with(cces06, ifelse((sen1.pparty == 'Democrat' & pid3 == 'Democrat') |
	                                        (sen1.pparty ==  'Republican' & pid3 == 'Republican'), 1, 0))
cces06$sen2.inpparty<- with(cces06, ifelse((sen2.pparty ==  'Democrat' & pid3 == 'Democrat') |
	                                       (sen2.pparty ==  'Republican' & pid3=='Republican'), 1, 0))
cces06$gov.inpparty<- with(cces06, ifelse((gov.pparty ==  'Democrat' & pid3 == 'Democrat')  |
	                                      (gov.pparty ==  'Republican' & pid3 == 'Republican'), 1, 0))
cces06$rep.inpparty<- with(cces06, ifelse((rep.pparty ==  'Democrat' & pid3 == 'Democrat')  |
	                                      (rep.pparty ==  'Republican' & pid3 == 'Republican'), 1, 0))

cces06$sen1.inpparty[is.na(cces06$sen1.pparty) | is.na(cces06$pid3)] <- NA
cces06$sen2.inpparty[is.na(cces06$sen2.pparty) | is.na(cces06$pid3) | cces06$sen2.pparty == 'Independent'] <- NA
cces06$gov.inpparty[is.na(cces06$gov.pparty) | is.na(cces06$pid3)] <- NA
cces06$rep.inpparty[is.na(cces06$rep.pparty) | is.na(cces06$pid3)] <- NA

# Ideology
# CC06_V5021 Representative - Party Unity Score
cces06$punity <- zero1(as.numeric(as.character(car::recode(cces06$v5021, "'Not asked' = NA"))))

# Senatorial Voting
# Partial Birth Abortion Vote, Stem Cell Research Vote
# Levin,  Immg
# Min. Wage, Cap. Gains, CAFTA, Estate, Alito

from.names <- paste0("v50", 22:43) 
to.names   <- paste0("sen", c(1, 2), ".", rep(c("abortion", "stemcell", "levin", "imm", "minwage", "gains",
	                                             "cafta", "tax", "tax.cloture", "ato.cloture", "alito"), each = 2))
# 9 = Did Not Vote
cces06[, to.names] <- sapply(cces06[,from.names], function(x) as.numeric(as.character(car::recode(x, "'Support' = 1; 'Oppose' = 0; 'Did not vote' = NA; 'Not in congress' = NA"))))

# cor(cces06[,to.names], use="na.or.complete")
# abortion, gains, cafta, tax, alito
toto.names <- paste0("sen", c(1, 2), ".", rep(c("abortion", "gains", "cafta", "tax", "alito"), each = 2))
cces06[,toto.names] <- sapply(cces06[,toto.names], function(x) car::recode(x, "1 = 0; 0 = 1"))

# Get ideology estimates via ltm
vote.sen1 <- ltm(cces06[ , to.names[seq(1, 21, 2)]] ~ z1)
vote.sen2 <- ltm(cces06[ , to.names[seq(2, 22, 2)]] ~ z1)

# Takes long time: cces06$vote.sen1 <-  factor.scores(vote.sen1, resp.patterns=cces06[,to.names[seq(1,21,2)]])$score.dat$z1
# cces06$vote.sen2 <-  factor.scores(vote.sen2, resp.patterns=cces06[, to.names[seq(2,22,2)]])$score.dat$z1

scoredf      <- factor.scores(vote.sen1)$score.dat
scoredf$join <- do.call(paste, c(scoredf[, 1:11], sep = ""))
cces06$join  <- do.call(paste, c(cces06[,to.names[seq(1, 21, 2)]], sep = ""))
cces06$vote.sen1 <- scoredf$z1[match(cces06$join,scoredf$join)]
# Validation: aggregate(cces06$vote.sen1, list(cces06$sen1.party), FUN=median)
# cor(cces06$vote.sen1, cces06$sen1.dw1, use="na.or.complete")

scoredf      <- factor.scores(vote.sen2)$score.dat
scoredf$join <- do.call(paste, c(scoredf[, 1:11], sep = ""))
cces06$join  <- do.call(paste, c(cces06[, to.names[seq(2, 22, 2)]], sep = ""))
cces06$vote.sen2 <- scoredf$z1[match(cces06$join,scoredf$join)]
# Validation: aggregate(cces06$vote.sen2, list(cces06$sen2.party), FUN=median)
# cor(cces06$vote.sen2, cces06$sen2.dw1, use="na.or.complete")

# Perceived Vote
from.names <- paste0("v30", c(61, 62, 64, 65, 67, 68, 70, 71, 73, 74, 76, 77, 79, 80)) 
to.names   <- paste0("psen", c(1, 2), ".", rep(c("abortion", "stemcell", "levin", "imm", "minwage", "gains", "cafta"), each = 2))
# 9 = Don't Know
cces06[,to.names] <- sapply(cces06[,from.names], function(x) car::recode(as.numeric(x), "1 = 1; 2 = 0; 3 = NA"))

# cor(cces06[,to.names], use="na.or.complete")
# abortion, gains, cafta
toto.names <- paste0("psen", c(1, 2), ".", rep(c("abortion", "gains", "cafta"), each = 2))
cces06[,toto.names] <- sapply(cces06[,toto.names], function(x) car::recode(x, "1 = 0; 0 = 1"))

# Get ideology estimates
pvote.sen1 <- ltm(cces06[,to.names[seq(1, 13, 2)]] ~ z1)
pvote.sen2 <- ltm(cces06[,to.names[seq(2, 14, 2)]] ~ z1)

# Get perceived ideology scores 
scoredf      <- factor.scores(pvote.sen1)$score.dat
scoredf$join <- do.call(paste, c(scoredf[ , 1:7], sep = ""))
cces06$join  <- do.call(paste, c(cces06[, to.names[seq(1, 13, 2)]], sep = ""))
cces06$pvote.sen1 <- scoredf$z1[match(cces06$join,scoredf$join)]
# Validation: aggregate(cces06$pvote.sen1, list(cces06$sen1.party), FUN=median)
# aggregate(cces06$pvote.sen1, list(cces06$sen1.pparty), mean, na.rm=T)

scoredf      <- factor.scores(pvote.sen2)$score.dat
scoredf$join <- do.call(paste, c(scoredf[ , 1:7], sep = ""))
cces06$join  <- do.call(paste, c(cces06[,to.names[seq(2, 14, 2)]], sep = ""))
cces06$pvote.sen2 <- scoredf$z1[match(cces06$join,scoredf$join)]
# Validation: aggregate(cces06$pvote.sen2, list(cces06$sen2.party), FUN=median)
# aggregate(cces06$pvote.sen2, list(cces06$sen2.pparty), mean, na.rm=T)

# Own Vote
from.names<- paste0("v30", seq(60, 78, 3)) 
to.names<- paste0("self.", c("abortion", "stemcell", "levin", "imm", "minwage", "gains", "cafta"))
# 9 = DK
cces06[,to.names] <- sapply(cces06[,from.names], function(x) car::recode(as.numeric(x), "1 = 1; 2 = 0; 3 = NA"))

# cor(cces06[,to.names], use="na.or.complete")
# cor(cces06[,to.names], use="na.or.complete")
# abortion, gains, cafta
toto.names <- paste0("self", ".", c("abortion", "gains", "cafta"))
cces06[,toto.names] <- sapply(cces06[,toto.names], function(x) car::recode(x, "1 = 0; 0 = 1"))

# ltm
self.vote    <- ltm(cces06[ , to.names] ~ z1)
scoredf      <- factor.scores(self.vote)$score.dat
scoredf$join <- do.call(paste, c(scoredf[ , 1:7], sep = ""))
cces06$join  <- do.call(paste, c(cces06[ ,to.names], sep = ""))
cces06$self.vote <- scoredf$z1[match(cces06$join, scoredf$join)]
# Validation: cor(cces06$self.vote, as.numeric(cces06$v2021), use="na.or.complete")
# aggregate(cces06$self.vote, list(cces06$pid3), mean, na.rm=T)
# cor(cces06$self.vote, cces06$pid7n, use="na.or.complete")

# Scaling together
# Lets get a measure based on just the 7 items people also vote on
sen1.names  <- paste0("sen1.", c("abortion", "stemcell", "levin", "imm", "minwage", "gains", "cafta"))
sen2.names  <- paste0("sen2.", c("abortion", "stemcell", "levin", "imm", "minwage", "gains", "cafta"))
self.names  <- paste0("self.", c("abortion", "stemcell", "levin", "imm", "minwage", "gains", "cafta"))

a <- cces06[!duplicated(cces06$sen1.last), sen1.names[1:6]]
b <- cces06[!duplicated(cces06$sen1.last), sen2.names[1:6]]
c <- cces06[ , self.names[1:6]]

names(a) <- names(b) <- names(c)
d <- rbind(a, b, c)

# cor(c, use="pairwise")
# cor(d) looks a bit shady - see how imm. works

  # Get ideology estimates via ltm
vote.all <- ltm(d ~ z1)

# Takes long time: cces06$vote.sen1 <-  factor.scores(vote.sen1, resp.patterns=cces06[,to.names[seq(1,21,2)]])$score.dat$z1
# cces06$vote.sen2 <-  factor.scores(vote.sen2, resp.patterns=cces06[,to.names[seq(2,22,2)]])$score.dat$z1
scoredf            <- factor.scores(vote.all)$score.dat
scoredf$join       <- do.call(paste, c(scoredf[ , 1:6], sep = ""))
cces06$join        <- do.call(paste, c(cces06[ , sen1.names[1:6]], sep = ""))
cces06$cvote.sen1  <- scoredf$z1[match(cces06$join,scoredf$join)]
# Validation: aggregate(cces06$cvote.sen1, list(cces06$sen1.party), FUN=median)
# cor(cces06$cvote.sen1, cces06$sen1.dw1, use="na.or.complete")

scoredf      <- factor.scores(vote.all)$score.dat
scoredf$join <- do.call(paste, c(scoredf[ , 1:6], sep = ""))
cces06$join  <- do.call(paste, c(cces06[ , sen2.names[1:6]], sep = ""))
cces06$cvote.sen2 <- scoredf$z1[match(cces06$join,scoredf$join)]
# Validation: aggregate(cces06$cvote.sen2, list(cces06$sen2.party), FUN=median)
# cor(cces06$cvote.sen2, cces06$sen2.dw1, use="na.or.complete")

scoredf      <- factor.scores(vote.all)$score.dat
scoredf$join <- do.call(paste, c(scoredf[ , 1:6], sep = ""))
cces06$join  <- do.call(paste, c(cces06[ , self.names[1:6]], sep = ""))
cces06$cself.vote <- scoredf$z1[match(cces06$join, scoredf$join)]
# Validation: aggregate(cces06$cself.vote, list(cces06$pid3), FUN=median, na.rm=T)
# cor(cces06$cself.vote, cces06$pid7n, use="na.or.complete")

# Approval
# CC06_V3003 - Bush Approval
# CC06_V3011 - Governor Approval
# CC06_V3013 - Senator 1 Approval
# CC06_V3015 - Senator 2 Approval
# CC06_V3017 - Rep. Approval
from.names <- paste0("v30", c("03", seq(11, 17, 2)))
to.names   <- paste0(c("bush", "gov", "sen1", "sen2", "rep"), ".approve")
cces06[ , to.names] <- sapply(cces06[ , from.names], function(x) car::recode(as.numeric(x), "1 = 1; 2 = .66; 3 = .33; 4 = 0; 5 = NA"))
# Expected pattern: more people neither approve nor disapprove of their representatives

# # Some Fun things
# Who is the fairest of them all?
# Which network do you think provides the fairest coverage of national news?
# table(cces06$v2112)
# table(cces06$v2112, cces06$v3005)
# dotplot(t(t(table(cces06$v2112, cces06$pid3))/colSums(table(cces06$v2112, cces06$pid3)))

library(plyr)
cces06$fairmedia <- revalue(cces06$v2112, c("abc" = "ABC", "cbs" = "CBS", "pbs" = "PBS", "nbc" = "NBC", "cnn" = "CNN", "msnbc" = "MSNBC"))

# Save
# save(cces06, file = "in-n-out/data/cces06r.rdata")

# Simple Analyses
# Recode our pid7n to more sensible coding
cces06$pid7nr <- round(cces06$pid7n,1)
with(cces06[!is.na(cces06$pid3) & cces06$pid7nr < .5,], summary(lm(sen1.approve ~ I(abs(cvote.sen1 - cself.vote))*sen1.inparty)))

with(cces06[!is.na(cces06$pid3) & cces06$pid7nr %in% c(0, .8, .2, 1), ], summary(lm(sen1.approve ~ I(abs(cvote.sen1 - cself.vote))*sen1.inparty)))
with(cces06[!is.na(cces06$pid3) & cces06$pid7nr %in% c(.3, .7),], summary(lm(sen1.approve ~ I(abs(cvote.sen1 - cself.vote))*sen1.inparty)))
with(cces06[!is.na(cces06$pid3) & cces06$pid7nr %in% c(0, .2, .8, 1), ], summary(lm(sen1.approve ~ I(abs(cvote.sen1 - cself.vote))*sen1.inpparty)))
