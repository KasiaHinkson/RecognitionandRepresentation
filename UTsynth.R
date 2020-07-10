library(foreign)
library(Synth)

synth<-as.data.frame(synth)

treated <- c(42, 6)
controls <- unique(synth$idnum[synth$law == 0])
controls <- controls[-which(controls %in% treated)]
controls<-c(1,3,4,9,10,12,14,16,17,18,22,24,25,26,27,35,34,37,39,41,43,46)

predictors <- c("tot_pop", "lgbt_pop", "const","church_att", "dem_vote", "urban_pop", "edu_hs", "edu_scol", "edu_col", "inc25", "inc45", "inc100", "inc200", "perAA", "court", "prot")


lrg.dp <- dataprep(foo = synth, 
                   predictors = predictors,
                   predictors.op = "mean",
                   time.predictors.prior = 1:14,
                   dependent = "lgbt_rep",
                   unit.variable = "idnum",
                   unit.names.variable = "State",
                   time.variable = "timenum",
                   treatment.identifier = 42,
                   controls.identifier = c(1,3,4,9,10,12,14,16,17,18,22,24,25,26,27,35,34,37,39,41,43,46,48),  
                   time.optimize.ssr = 1:14,
                   time.plot = 1:19)

lrg.synth <- synth(lrg.dp, method = "BFGS")
lrg.diag <- synth.tab(dataprep.res = lrg.dp, synth.res = lrg.synth)

lrg.pred <- lrg.dp$Y0plot %*% lrg.synth$solution.w 

par(mar = c(5, 5, .5, .5))
plot(1:19, lrg.dp$Y1plot, type = "n", ylim = c(0, 3), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 15, lwd = 1)
lines(1:19, lrg.dp$Y1plot, lwd = 2)
lines(1:19, lrg.pred, lwd = 2, lty = 2)
axis(1, at = 1:19, labels = seq(2000, 2018, 1), cex.axis = 1.25)
axis(2, at = seq(0, 3, 1), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Number of LGBT Reps", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Utah", "Synthetic Utah"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)


gaps.plot(synth.res    = lrg.synth,
          dataprep.res = lrg.dp,
          Ylab         = c("Gap"),
          Xlab         = c("Year"),
          Ylim         = c(-5, 10),
          Main         = ""
)

abline(v   = 9,
       lty = 2)


############
lrg.dp <- dataprep(foo = synth, 
                   predictors = predictors,
                   predictors.op = "mean",
                   time.predictors.prior = 1:8,
                   dependent = "lgbt_rep",
                   unit.variable = "idnum",
                   unit.names.variable = "State",
                   time.variable = "timenum",
                   treatment.identifier = 6,
                   controls.identifier = c(1,3,4,9,10,12,14,16,17,18,22,24,25,26,27,35,34,37,39,41,43,46),  
                   time.optimize.ssr = 1:8,
                   time.plot = 1:19)

lrg.synth <- synth(lrg.dp, method = "BFGS")
lrg.diag <- synth.tab(dataprep.res = lrg.dp, synth.res = lrg.synth)

lrg.pred <- lrg.dp$Y0plot %*% lrg.synth$solution.w 

par(mar = c(5, 5, .5, .5))
plot(1:19, lrg.dp$Y1plot, type = "n", ylim = c(0, 10), lwd = 2, xlab = "", ylab = "", axes = FALSE)
box(); grid()
abline(v = 9, lwd = 1)
lines(1:19, lrg.dp$Y1plot, lwd = 2)
lines(1:19, lrg.pred, lwd = 2, lty = 2)
axis(1, at = 1:19, labels = seq(2000, 2018, 1), cex.axis = 1.25)
axis(2, at = seq(0, 10, 1), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Number of LGBT Reps", cex.lab = 1.5, line = 3.5)
legend("topleft", inset = .025, bty = "n", c("Colorado", "Synthetic Colorado"), lwd = 2, col = "black", lty = c(1, 2, -1), cex = 1.25)

print(synth.tables   <- synth.tab(
  dataprep.res = lrg.dp,
  synth.res    = lrg.synth)
)



########
gaps <- matrix(nrow = length(controls), ncol = 19)
mspe.ratios <- numeric(length(controls))
bad.fit <- numeric(23)
for(i in 1:23){
  placebo.dp <- dataprep(synth, 
                         predictors = predictors,
                         predictors.op = "mean",
                         time.predictors.prior = 1:14,
                         dependent = "lgbt_rep",
                         unit.variable = "idnum",
                         unit.names.variable = "State",
                         time.variable = "timenum",
                         treatment.identifier = controls[i],
                         controls.identifier = controls[-i],  
                         time.optimize.ssr = 1:14,
                         time.plot = 1:19)  
  placebo.synth <- synth(placebo.dp, method = "BFGS")
  placebo.pred <- placebo.dp$Y0plot %*% placebo.synth$solution.w
  gaps[i, ] <- placebo.dp$Y1plot - (placebo.dp$Y0plot %*% placebo.synth$solution.w)
  mspe.ratios[i] <- mean((placebo.dp$Y1plot[15:19] - placebo.pred[15:19])^2)/mean((placebo.dp$Y1plot[1:14] - placebo.pred[1:14])^2) 
  bad.fit[i] <- as.numeric(ifelse(placebo.synth$loss.v > lrg.synth$loss.v*5, 1, 0))
  print(i)
}

lrg.gaps <- lrg.dp$Y1plot - (lrg.dp$Y0plot %*% lrg.synth$solution.w) 
lrg.ratio <- mean((lrg.dp$Y1plot[15:19] - lrg.pred[15:19])^2)/mean((lrg.dp$Y1plot[1:14] - lrg.pred[1:14])^2)
lrg.p <- length(which(mspe.ratios >= lrg.ratio))/(length(controls) + 5)

par(mar = c(5, 5, .5, .5))
plot(1:19, lrg.gaps, type = "n", ylim = c(-1, 7), lwd = 2, xlab = "", ylab = "", axes = FALSE) 
box(); grid()
for(i in 1:23){
  if(bad.fit[i] == 0){
    lines(1:19, gaps[i, ], type = "l", col = "gray80", lwd = 2)
  } else cat("bad fit", "\n")
} 
abline(v = 9, lwd = 1)
abline(h=0,lwd=1)
lines(1:19, lrg.gaps, lwd = 2, lty = 1)
axis(1, at = 1:19, labels = seq(2000, 2018, 1), cex.axis = 1.25)
axis(2, at = seq(-1, 7, 1), cex.axis = 1.25, las = 2)
title(xlab = "Year", cex.lab = 1.5, line = 3.75)
title(ylab = "Difference in Representation from Synthetic ", cex.lab = 1.5, line = 3.5)
