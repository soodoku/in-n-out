#
# Experiment on YG
# 

# Notes
# No data on Independents. 

# Set Directory
setwd(githubdir)

# Get data
data <- read.csv("in-n-out/data/yg/yg_exp.csv")

# Party ID
data$pid2 <- car::recode(data$pid7, "c('Strong Democrat', 'Lean Democrat', 'Not very strong Democrat') = 'Democrat'; c('Strong Republican', 'Lean Republican', 'Not very strong Republican') = 'Republican'")

# Experiment
data$govtsvc_rand2r <- car::recode(data$govtsvc_rand2, "'Republican - 1' = 1; 'Republican - 3' = 3; 'Democratic - 5' = 5; 'Democratic - 7' = 7", as.factor.result = F)
data$abort_rand2r <- car::recode(data$abort_rand2, "'Republican - 1' = 1; 'Republican - 3' = 3; 'Democratic - 5' = 5; 'Democratic - 7' = 7", as.factor.result = F)

# Government Services & Spending scale
data$govtsvcr <- car::recode(data$govtsvc, "997 = NA")

# Approve/Disapprove - Government Services & Spending Scale
data$govtsvc_candr <- car::recode(data$govtsvc_cand, "'Strongly Approve' = 1; 'Approve' = .75; 'Neither Approve nor Disapprove' = .5; 'Disapprove' = .25; 'Strongly Disapprove' = 0", as.factor.result = F)

# Abortion Scale
data$abortr <- car::recode(data$abort, "997=NA")

# Approve/Disapprove - Abortion Scale
data$abort_candr <- car::recode(data$abort_cand, "'Strongly Approve' = 1; 'Approve' = .75; 'Neither Approve nor Disapprove' = .5; 'Disapprove' = .25; 'Strongly Disapprove' = 0", as.factor.result = F)

# In/Out Party
data$in.party1 <- NA
data$in.party1[data$pid2 == 'Republican'& !is.na(data$govtsvc_rand2r) & data$govtsvc_rand2r < 5] <- 1
data$in.party1[data$pid2 == 'Democrat'& !is.na(data$govtsvc_rand2r) & data$govtsvc_rand2r > 5] <- 1
data$in.party1[data$pid2 == 'Republican'& !is.na(data$govtsvc_rand2r) & data$govtsvc_rand2r > 5] <- 0
data$in.party1[data$pid2 == 'Democrat'& !is.na(data$govtsvc_rand2r) & data$govtsvc_rand2r < 5] <- 0

data$in.party2 <- NA
data$in.party2[data$pid2 == 'Republican'& !is.na(data$abort_rand2r) & data$abort_rand2r < 5] <- 1
data$in.party2[data$pid2 == 'Democrat'& !is.na(data$abort_rand2r) & data$abort_rand2r > 5] <- 1
data$in.party2[data$pid2 == 'Republican'& !is.na(data$abort_rand2r) & data$abort_rand2r > 5] <- 0
data$in.party2[data$pid2 == 'Democrat'& !is.na(data$abort_rand2r) & data$abort_rand2r < 5] <- 0

# Analyses

# First piece of information
# ----------------------------

# Govt. Services

# Dems judging Dems
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 7], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 5], na.rm = T)
t.test(data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 7], data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 5])

# Democrats judging Republicans
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 3], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 1], na.rm = T)

# Republicans judging Republicans 
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 1], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 3], na.rm = T)
t.test(data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 1], data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 3])

# Republicans judging Democrats
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 5], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 7], na.rm = T)
 
# Abortion
# Dems judging Dems
mean(data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 7], na.rm = T)
mean(data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 5], na.rm = T)
t.test(data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 7], data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 5])

# Democrats judging Republicans
mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 3], na.rm = T)
mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 1], na.rm = T)

# Republicans judging Republicans 
mean( data$abort_candr[data$pid2  == "Republican" & data$abort_rand2r == 1], na.rm = T)
mean( data$abort_candr[data$pid2  == "Republican" & data$abort_rand2r == 3], na.rm = T)

# Republicans judging Democrats
mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 5], na.rm = T)
mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 7], na.rm = T)

# Dems. rating Dems.
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 7 & data$govtsvcr == 7], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 5 & data$govtsvcr == 7], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 5 & data$govtsvcr == 5], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 7 & data$govtsvcr == 5], na.rm = T)

mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 7 & data$abortr == 7], na.rm = T)
mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 5 & data$abortr == 7], na.rm = T)
mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 5 & data$abortr == 5], na.rm = T)
mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 7 & data$abortr == 5], na.rm = T)

# Dems. rating Reps.
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 1 & data$govtsvcr == 7], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 3 & data$govtsvcr == 7], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 1 & data$govtsvcr == 5], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Democrat" & data$govtsvc_rand2r == 3 & data$govtsvcr == 5], na.rm = T)

mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 1 & data$abortr == 7], na.rm = T)
mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 3 & data$abortr == 7], na.rm = T)
mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 1 & data$abortr == 5], na.rm = T)
mean( data$abort_candr[data$pid2 == "Democrat" & data$abort_rand2r == 3 & data$abortr == 5], na.rm = T)

# Reps. rating Reps.
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 1 & data$govtsvcr == 1], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 3 & data$govtsvcr == 1], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 1 & data$govtsvcr == 3], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 3 & data$govtsvcr == 3], na.rm = T)

mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 1 & data$abortr == 1], na.rm = T)
mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 3 & data$abortr == 1], na.rm = T)
mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 3 & data$abortr == 3], na.rm = T)
mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 1 & data$abortr == 3], na.rm = T)

# Reps. rating Dems.
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 7 & data$govtsvcr == 1], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 5 & data$govtsvcr == 1], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 5 & data$govtsvcr == 3], na.rm = T)
mean( data$govtsvc_candr[data$pid2 == "Republican" & data$govtsvc_rand2r == 7 & data$govtsvcr == 3], na.rm = T)

mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 7 & data$abortr == 1], na.rm = T)
mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 5 & data$abortr == 1], na.rm = T)
mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 5 & data$abortr == 3], na.rm = T)
mean( data$abort_candr[data$pid2 == "Republican" & data$abort_rand2r == 7 & data$abortr == 3], na.rm = T)

# In/Out by Party
with(data[data$pid2 == 'Republican' & data$in.party1 == 1, ], summary(lm(govtsvc_candr ~ I(abs(govtsvc_rand2r - govtsvcr)))))
with(data[data$pid2 == 'Republican' & data$in.party1 == 0, ], summary(lm(govtsvc_candr ~ I(abs(govtsvc_rand2r - govtsvcr)))))
with(data[data$pid2 == 'Democrat'   & data$in.party1 == 1, ], summary(lm(govtsvc_candr ~ I(abs(govtsvc_rand2r - govtsvcr)))))
with(data[data$pid2 == 'Democrat'   & data$in.party1 == 0, ], summary(lm(govtsvc_candr ~ I(abs(govtsvc_rand2r - govtsvcr)))))

# In Abortion/among Republicans, diff. going on. 
with(data[data$pid2 == 'Republican' & data$in.party2 == 1, ], summary(lm(abort_candr ~ I(abs(abort_rand2r - abortr)))))
with(data[data$pid2 == 'Republican' & data$in.party2 == 0, ], summary(lm(abort_candr ~ I(abs(abort_rand2r - abortr)))))
with(data[data$pid2 == 'Democrat' & data$in.party2 == 1, ],  summary(lm(abort_candr ~ I(abs(abort_rand2r - abortr))))) 
with(data[data$pid2 == 'Democrat' & data$in.party2 == 0, ], summary(lm(abort_candr ~ I(abs(abort_rand2r - abortr)))))

# In/Out
with(data[data$in.party1 == 1,], summary(lm(govtsvc_candr ~ I(zero1(abs(govtsvc_rand2r - govtsvcr))))))
with(data[data$in.party1 == 0,], summary(lm(govtsvc_candr ~ I(zero1(abs(govtsvc_rand2r - govtsvcr))))))

with(data[data$in.party2 == 1,], summary(lm(abort_candr ~ I(zero1(abs(abort_rand2r - abortr))))))
with(data[data$in.party2 == 0,], summary(lm(abort_candr ~ I(zero1(abs(abort_rand2r - abortr))))))

# Interaction
with(data[!is.na(data$pid2),], summary(lm(govtsvc_candr ~ I(zero1(abs(govtsvc_rand2r - govtsvcr)))*in.party1)))
with(data[!is.na(data$pid2),], summary(lm(abort_candr  ~ I(zero1(abs(abort_rand2r - abortr)))*in.party2)))

# Plot
b <- data[!is.na(data$pid2) & !is.na(data$in.party1) & data$in.party1 == 0 & !is.na(data$govtsvc_candr) & !is.na(data$govtsvcr) & !is.na(data$govtsvc_rand2r),]
plot(I(zero1(abs(b$govtsvc_rand2r - b$govtsvcr))), b$govtsvc_candr, col="white")
lines(lowess(I(zero1(abs(b$govtsvc_rand2r - b$govtsvcr))), b$govtsvc_candr))
b <- data[!is.na(data$pid2) & !is.na(data$in.party1) & data$in.party1 == 1 & !is.na(data$govtsvc_candr) & !is.na(data$govtsvcr) & !is.na(data$govtsvc_rand2r),]
lines(lowess(I(zero1(abs(b$govtsvc_rand2r - b$govtsvcr))), b$govtsvc_candr))

b <- data[!is.na(data$pid2) & !is.na(data$in.party2) & data$in.party2 == 0 & !is.na(data$abort_rand2r) & !is.na(data$abortr) & !is.na(data$abort_candr),]
plot(I(zero1(abs(b$abort_rand2r - b$abortr))), b$abort_candr)
lines(lowess(I(zero1(abs(b$abort_rand2r - b$abortr))), b$abort_candr, f = .6))
b <- data[!is.na(data$pid2) & !is.na(data$in.party2) & data$in.party2 == 1 & !is.na(data$abort_rand2r) & !is.na(data$abortr) & !is.na(data$abort_candr),]
lines(lowess(I(zero1(abs(b$abort_rand2r - b$abortr))), b$abort_candr, f=.6))

# Social Spending
pdf("in-n-out/figs/YGgovsvc.pdf")
inpart <- with(data[data$in.party1 == 1,], lm(govtsvc_candr ~ I(zero1(abs(govtsvc_rand2r - govtsvcr)))))
datas  <- data[data$in.party1 == 1,] 
plot(I(zero1(abs(datas$govtsvc_rand2r - datas$govtsvcr))), datas$govtsvc_candr, col = "white",
xlab="Absolute Distance", 
ylab="Approval",
ylim=c(0,1),
main="Social Spending")
abline(inpart)
outpart <- with(data[data$in.party1 == 0,], lm(govtsvc_candr ~ I(zero1(abs(govtsvc_rand2r - govtsvcr)))))
abline(outpart, lty=2, col="#aacc00")
text(.47,.33,"Out-Party", col="#aacc00")
text(.7,.43,"In-Party")
dev.off()

# Abortion
pdf("in.n.out/figs/YGabort.pdf")
inpart <- with(data[data$in.party2 == 1,], lm(abort_candr ~ I(zero1(abs(abort_rand2r - abortr)))))
datas  <- data[data$in.party1 == 1,] 
plot(I(zero1(abs(datas$abort_rand2r  - datas$abortr))), datas$abort_candr, col = "white",
xlab="Absolute Distance", 
ylab="Approval",
ylim=c(0,1),
main="Abortion")
abline(inpart)
outpart <- with(data[data$in.party2 == 0,], lm(abort_candr ~ I(zero1(abs(abort_rand2r - abortr)))))
abline(outpart, lty=2, col = "#aacc00")
text(.5,.35,"Out-Party", col = "#aacc00")
text(.7,.48,"In-Party")
dev.off()
