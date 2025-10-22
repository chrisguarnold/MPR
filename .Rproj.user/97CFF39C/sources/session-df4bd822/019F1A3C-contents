#===============================================================================
# Causal Effect of MPR: Updates from R&R Process in JCR
# Arnold & Schulz: "Reaching For The Threshold"
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================




# -- I MAIN PAPER --------------------------------------------------------------

# 1 Main Effect ----------------------------------------------------------------

# --- 1.1 Calculating ----------------------------------------------------------

ce.all.treaties.6y.l <- ce.se.all.treaties.6y.l <- vector(
  mode = 'list', length = length(treaty_codes))
na.counter <- 0
# mask for selecting those with a CE only
for (i in seq(1,length(treaty_codes))) {
  if (i%%10 == 0) cat('...', i, sep = '')
  one.ce <- estimate.ce.misc(dat.df.rat.misc.all.treaties, treaty_codes[i],
                             self.calc.entry.force.date = FALSE,
                             timespan.years = 10)
  ce.all.treaties.6y.l[[i]] <- one.ce[,1]
  ce.se.all.treaties.6y.l[[i]] <- one.ce[,2]
  # count the cases where it was not possible to calculate a CE because 
  # * matched data was not possible to construct since the treaty was not in force and the MPR was not reached.
  # * there were not enough control groups
  # both of the exceptions are defined in make_data_for_matching()
  if(unique(is.na(ce.all.treaties.6y.l[[i]]))) {
    na.counter <- na.counter + 1
  }
}


# Data Management
# Reshuffles data from results
dat.ce.all <- harmonise.experiments(
  ce.all.treaties.6y.l, ce.se.all.treaties.6y.l, 
  public_good, common_good
)
# Further data refinements
dat.ce.all$weight <- 1 / (dat.ce.all$se.delta^2)
#exclude all eights = inf and se.delta = inf
dat.ce.all <- subset(dat.ce.all, is.finite(dat.ce.all$weight))
dat.ce.all <- subset(dat.ce.all, is.finite(dat.ce.all$se.delta))



# Meta Analysis
# Run the fixed effects model
dat.ce.results.FE <- aggregate_survival_FE(dat.ce.all)
# Run the random effects model
# NB: The NAs happen at very late points where there are not enough obs for SEs
dat.ce.results.RE <- aggregate_survival_meta_RE(dat.ce.all)



# --- 1.2 Plotting -------------------------------------------------------------
# Plotting
# Main Manuscript Figure 5: One plot with just RE and uncertainty
pdf('ce_mpr_all_RE.pdf', width = 9)
cardiffblue.t <- adjustcolor( cardiffblue, alpha.f = 0.3) 
plot(ce.all.treaties.6y.l[[1]], type = 'n', col = cardiffblue.t, bty = 'n',
     ylab = 'CE of MPR (Kaplan Meier Difference)', xlab = 'Time after MPR (Days)',
     ylim = c(-1, 1), xlim = c(0,2500), 
     las =1)
# All individual CE
for (i in seq(1, length(ce.all.treaties.6y.l))){
  lines(ce.all.treaties.6y.l[[i]], col = cardiffblue.t, lwd = 0.5)
}
abline(h = 0, lwd = 2, lty = 2, col = 'grey20')
# plot RE weighted mean over treatment effects per time 
lines(dat.ce.results.RE$time, dat.ce.results.RE$weighted.effect, col = cardiffblue, lwd = 3)
lines(dat.ce.results.RE$time, dat.ce.results.RE$ci.lower, col = cardiffblue, lwd = 2, lty = 2)
lines(dat.ce.results.RE$time, dat.ce.results.RE$ci.upper, col = cardiffblue, lwd = 2, lty = 2)
dev.off()


# Plotting
# Appendix Figure 12: One plot with all three effects
pdf('appendix_meta_all_together.pdf', width = 9)
cardiffblue.t <- adjustcolor( cardiffblue, alpha.f = 0.3) 
plot(ce.all.treaties.6y.l[[1]], type = 'n', col = cardiffblue.t, bty = 'n',
     ylab = 'CE of MPR (Kaplan Meier Difference)', xlab = 'Time after MPR (Days)',
     ylim = c(-1, 1), xlim = c(0,2500), 
     las =1)
# All individual CE
for (i in seq(1, length(ce.all.treaties.6y.l))){
  lines(ce.all.treaties.6y.l[[i]], col = cardiffblue.t, lwd = 0.5)
}
abline(h = 0, lwd = 2, lty = 2, col = 'grey20')
# plot FE weighted mean over treatment effects per time 
lines(dat.ce.results.FE$time, dat.ce.results.FE$weighted.effect, col = cardiffgold, lwd = 3)
# calculate plain mean over treatment effects per time 
mask <- !is.na(ce.all.treaties.6y.l)
ce.all.treaties.6y.l.wona <- ce.all.treaties.6y.l[mask]
dat.ce.all.treaties.6y.wona <- do.call(rbind.data.frame, ce.all.treaties.6y.l.wona)
average.ce <- apply(dat.ce.all.treaties.6y.wona, 2, mean)
lines(average.ce, col = cardiffred, lwd = 3)
# plot RE weighted mean over treatment effects per time 
lines(dat.ce.results.RE$time, dat.ce.results.RE$weighted.effect, col = cardiffblue, lwd = 3)
legend('bottomright', legend = c('RE', 'FE', 'Mean'),
       lty = 1, lwd = 3, col = c(cardiffblue, cardiffgold, cardiffred))
dev.off()




# --- 1.3 Table for Appendix ---------------------------------------------------
# Appendix Table 3
dat.temp <- data.frame(
    time = dat.ce.results.FE$time[1:2500],
    ate.re = round(dat.ce.results.RE$weighted.effect[1:2500], 3),
    ate.fe = round(dat.ce.results.FE$weighted.effect[1:2500], 3),
    ate.mean = round(average.ce[1:2500], 3),
    row.names = NULL
)
dat.tab.all <- subset(dat.temp, dat.temp$time %% 100 == 0)
# Rename columns directly in the data frame
colnames(dat.tab.all) <- c("Days after MPR", "RE Model", "FE Model", "Mean")
# Generate LaTeX table
stargazer(dat.tab.all, type = "latex", summary = FALSE, rownames = FALSE,
          digits = 3, title = "Average Treatment Effect Estimates Over Time",
          label = "tab:ate_summary", align = TRUE)



# 2 Distinguish public good and non pg -----------------------------------------
# --- 2.1 Calculating ----------------------------------------------------------
# split goods from non-goods
mask.pg <- (dat.ce.all$public_good == 1 | dat.ce.all$common_good == 1)
mask.npg <- (dat.ce.all$public_good == 0 & dat.ce.all$common_good == 0)
dat.ce.pg <- subset(dat.ce.all, mask.pg)
dat.ce.npg <- subset(dat.ce.all, mask.npg)

# Fixed Effects 
dat.ce.results.FE.pg <- aggregate_survival_FE(dat.ce.pg)
dat.ce.results.FE.npg <- aggregate_survival_FE(dat.ce.npg)

# Random Effects models
dat.ce.results.RE.pg <- aggregate_survival_meta_RE(dat.ce.pg)
dat.ce.results.RE.npg <- aggregate_survival_meta_RE(dat.ce.npg)

# Random effects
dat.pg.npg.dif.RE <- compare_survival_curves(
  dat.ce.results.RE.pg$weighted.effect, dat.ce.results.RE.pg$ci.upper, dat.ce.results.RE.pg$ci.lower,
  dat.ce.results.RE.npg$weighted.effect, dat.ce.results.RE.npg$ci.upper, dat.ce.results.RE.npg$ci.lower,
  dat.ce.results.RE.npg$time
)

# Fixed Effects
dat.pg.npg.dif.FE <- compare_survival_curves(
  dat.ce.results.FE.pg$weighted.effect, dat.ce.results.FE.pg$ci.upper, dat.ce.results.FE.pg$ci.lower,
  dat.ce.results.FE.npg$weighted.effect, dat.ce.results.FE.npg$ci.upper, dat.ce.results.FE.npg$ci.lower,
  dat.ce.results.FE.npg$time
)



# --- 2.2 Plotting -------------------------------------------------------------
# Plotting 
# Main Manuscript Figure 7
pdf('ce_mpr_pg_npg_RE.pdf', width = 9)
cardiffblue.t <- adjustcolor( cardiffblue, alpha.f = 0.3) 
cardiffred.t <- adjustcolor( cardiffred, alpha.f = 0.3) 
plot(dat.ce.results.RE.pg$time, dat.ce.results.RE.pg$weighted.effect,
     col = cardiffred, type = "n", lwd = 1, bty = 'n',
     ylim = c(-1, 1), xlim = c(0, 2500), las = 1, 
     ylab = 'CE of MPR (Kaplan Meier Difference)', xlab = 'Time after MPR (Days)')
abline(h = 0, lwd = 2, lty = 2, col = 'grey20')
lines(dat.ce.results.RE.pg$time, dat.ce.results.RE.pg$weighted.effect, 
      col = cardiffred, type = "l", lwd = 1)
lines(dat.ce.results.RE.npg$time, dat.ce.results.RE.npg$weighted.effect, 
      col = cardiffblue, type = "l", lwd = 1)
# if stat. significant, plot
for (i in seq(1,length(dat.ce.results.RE.npg$time))){
  if (dat.pg.npg.dif.RE$p_value[i] < 0.05){
    points(dat.ce.results.RE.npg$time[i], dat.ce.results.RE.npg$weighted.effect[i], 
           col = cardiffblue, cex = 0.4, pch = 15)  
  }
}
# if stat. significant, plot
for (i in seq(1,length(dat.ce.results.RE.pg$time))){
  if (dat.pg.npg.dif.RE$p_value[i] < 0.05){
    points(dat.ce.results.RE.pg$time[i], dat.ce.results.RE.pg$weighted.effect[i], 
           col = cardiffred, cex = 0.4, pch = 15)  
  }
}
legend('bottomright', col = c(cardiffred, cardiffblue), pch = 16,
       legend = c('Public or Common Good', 'No Public or Common Good'), bty = 'n')
dev.off()


# Random Effects
# Figure 13b
pdf('appendix_difference_pgoods_RE_pvals.pdf', width = 9)
par(fig=c(0,1,0,1))
x.lim <- c(0,2500)
# p-values
plot(dat.pg.npg.dif.RE$time, dat.pg.npg.dif.RE$p_value, type = 'l', 
     xlab = "Time after MPR (Days)",bty = 'n',
     xlim = x.lim, ylim = c(0,.4), las = 1,
     ylab = 'p-value of difference')
abline(h = 0.05, lwd = 2, lty = 2, col = 'grey20')
dev.off()



# plot all three effects in one go
# Appendix Figure 13a
pdf('appendix_difference_pgoods_all.pdf', width = 9)
par(fig=c(0,1,0,1))
plot(ce.all.treaties.6y.l[[1]], type = 'n', col = cardiffblue.t, bty = 'n',
     ylab = 'CE of MPR (Kaplan Meier Difference)', xlab = 'Time after MPR (Days)',
     ylim = c(-1, 1), xlim = c(0,2500), las =1)
abline(h = 0, lwd = 2, lty = 2, col = 'grey20')
# REs
lines(dat.ce.results.RE.pg$time, dat.ce.results.RE.pg$weighted.effect,
      col = cardiffblue, type = "l", lwd = 2)
lines(dat.ce.results.RE.pg$time, dat.ce.results.RE.npg$weighted.effect, 
      col = cardiffblue, type = "l", lwd = 2, lty =2)
# FEs
lines(dat.ce.results.FE.pg$time, dat.ce.results.FE.pg$weighted.effect,
      col = cardiffgold, type = "l", lwd = 2)
lines(dat.ce.results.FE.pg$time, dat.ce.results.FE.npg$weighted.effect, 
      col = cardiffgold, type = "l", lwd = 2, lty =2)
# Plain means
mask <- !is.na(ce.all.treaties.6y.l) & (public_good == 1 | common_good == 1)
ce.all.treaties.6y.l.wona <- ce.all.treaties.6y.l[mask]
dat.ce.all.treaties.6y.wona <- suppressWarnings(do.call(rbind.data.frame, ce.all.treaties.6y.l.wona))
average.ce.pg <- apply(dat.ce.all.treaties.6y.wona, 2, mean)
lines(average.ce.pg, col = cardiffred, lwd = 2)
mask <-!is.na(ce.all.treaties.6y.l) & (public_good == 0 & common_good == 0)
ce.all.treaties.6y.l.wona <- ce.all.treaties.6y.l[mask]
dat.ce.all.treaties.6y.wona <- suppressWarnings(do.call(rbind.data.frame, ce.all.treaties.6y.l.wona))
average.ce.others <- apply(dat.ce.all.treaties.6y.wona, 2, mean)
lines(average.ce.others, col = cardiffred, lwd = 2, lty = 2)

legend('bottomright', lty = c(1,3), lwd = 2,
       legend = c('Public or Common Good', 'No Public or Common Good'), 
       bty = 'n')
legend('bottomleft', lty = 1, lwd = 2,
       col = c(cardiffblue, cardiffgold, cardiffred),
       legend = c('RE', 'FE', "Mean"), 
       bty = 'n')
dev.off()






# --- 2.3 Table ----------------------------------------------------------------
# Appendix Table 4
dat.temp.goods <- data.frame(
  time = dat.ce.results.FE.pg$time[1:2500],
  ate.re.pg = round(dat.ce.results.FE.pg$weighted.effect, 3)[1:2500],
  ate.re.npg = round(dat.ce.results.FE.npg$weighted.effect, 3)[1:2500],
  diff.p.r = round(dat.pg.npg.dif.RE$p_value, 3)[1:2500],
  ate.fe.pg = round(dat.ce.results.FE.pg$weighted.effect, 3)[1:2500],
  ate.fe.npg = round(dat.ce.results.FE.npg$weighted.effect, 3)[1:2500],
  diff.p.fe = round(dat.pg.npg.dif.FE$p_value, 3)[1:2500],
  ate.mean.pg = round(average.ce.pg, 3)[1:2500],
  ate.mean.npg = round(average.ce.others, 3)[1:2500],
  row.names = NULL
)

# difference between npg and pg over first 2500 days
mean(dat.temp.goods$ate.re.pg - dat.temp.goods$ate.re.npg)
dat.tab.goods <- subset(dat.temp.goods, dat.temp$time %% 100 == 0)
# Rename columns directly in the data frame
colnames(dat.tab.goods) <- c("Days after MPR", 
                             "RE Model Pub/Com Goods", 
                             "RE Model No Pub/Com Goods", 'p-value of difference (RE)',
                             "FE Model Pub/Com Goods", 
                             "FE Model No Pub/Com Goods", 'p-value of difference (FE)',
                             "Mean Pub/Com Goods", "Mean No Pub/Com Goods"
)

# Generate LaTeX table
stargazer(dat.tab.goods, type = "latex", summary = FALSE, rownames = FALSE,
          digits = 3, title = "Aggregate Average Treatment Effect Estimates Over Time",
          label = "tab:ate_summary_goods", align = TRUE)


# -- II ROBUSTNESS -------------------------------------------------------------
# -- 3.1 Robustness H1 Including Controls --------------------------------------
# Based on explicit reformulation of estimate.ce.misc() for chapters and depth
# -- 3.1.1 Robustness Including Chapters -----------------------------------------

# Controlling for Chapter
ce.all.treaties.6y.l.chapter <- ce.se.all.treaties.6y.l <- vector(
  mode = 'list', length = length(treaty_codes))
na.counter <- 0
for (i in seq(1,length(treaty_codes))) {
  if (i%%10 == 0) cat('...', i, sep = '')
  one.ce <- estimate.ce.misc.chapter(dat.df.rat.misc.all.treaties, treaty_codes[i],
                             self.calc.entry.force.date = FALSE,
                             timespan.years = 10)
  ce.all.treaties.6y.l.chapter[[i]] <- one.ce[,1]
  ce.se.all.treaties.6y.l[[i]] <- one.ce[,2]
  if(unique(is.na(ce.all.treaties.6y.l.chapter[[i]]))) {
    na.counter <- na.counter + 1
  }
}


# Data Management
# Reshuffles data from results
dat.ce.all.chapter <- harmonise.experiments(
  ce.all.treaties.6y.l.chapter, ce.se.all.treaties.6y.l, 
  public_good, common_good
)
# Further data refinements
dat.ce.all.chapter$weight <- 1 / (dat.ce.all.chapter$se.delta^2)
#exclude all eights = inf and se.delta = inf
dat.ce.all.chapter <- subset(dat.ce.all.chapter, is.finite(dat.ce.all.chapter$weight))
dat.ce.all.chapter <- subset(dat.ce.all.chapter, is.finite(dat.ce.all.chapter$se.delta))
# Run the random effects model
# NB: The NAs happen at very late points where there are not enough obs for SEs
dat.ce.results.RE.chapter <- aggregate_survival_meta_RE(dat.ce.all.chapter)


# -- 3.2 Robustness Including Cooperation Depth --------------------------------
ce.all.treaties.6y.l.depth <- ce.se.all.treaties.6y.l <- vector(
  mode = 'list', length = length(treaty_codes))
na.counter <- 0
# mask for selecting those with a CE only
for (i in seq(1,length(treaty_codes))) {
  if (i%%10 == 0) cat('...', i, sep = '')
  one.ce <- estimate.ce.misc.depth(dat.df.rat.misc.all.treaties, treaty_codes[i],
                                     self.calc.entry.force.date = FALSE,
                                     timespan.years = 10)
  ce.all.treaties.6y.l.depth[[i]] <- one.ce[,1]
  ce.se.all.treaties.6y.l[[i]] <- one.ce[,2]
  if(unique(is.na(ce.all.treaties.6y.l.depth[[i]]))) {
    na.counter <- na.counter + 1
  }
}

# Data Management
# Reshuffles data from results
dat.ce.all.depth <- harmonise.experiments(
  ce.all.treaties.6y.l.depth, ce.se.all.treaties.6y.l, 
  public_good, common_good
)
# Further data refinements
dat.ce.all.depth$weight <- 1 / (dat.ce.all.depth$se.delta^2)
#exclude all eights = inf and se.delta = inf
dat.ce.all.depth <- subset(dat.ce.all.depth, is.finite(dat.ce.all.depth$weight))
dat.ce.all.depth <- subset(dat.ce.all.depth, is.finite(dat.ce.all.depth$se.delta))
# Run the random effects model
# NB: The NAs happen at very late points where there are not enough obs for SEs
dat.ce.results.RE.depth <- aggregate_survival_meta_RE(dat.ce.all.depth)

# Plotting 
# Appendix Figure 7
pdf('appendix_ce_mpr_all_RE_robustness.pdf', width = 9)
plot(1,1, type = 'n', col = cardiffblue.t, bty = 'n',
     ylab = 'CE of MPR (Kaplan Meier Difference)', xlab = 'Time after MPR (Days)',
     ylim = c(-1, 1), xlim = c(0,2500), 
     las =1)
abline(h = 0, lwd = 2, lty = 2, col = 'grey20')
# Basic RE 
lines(dat.ce.results.RE$time, 
      dat.ce.results.RE$weighted.effect, col = cardiffblue, lwd = 3)
lines(dat.ce.results.RE$time, 
      dat.ce.results.RE$ci.lower, col = cardiffblue, lwd = 2, lty = 2)
lines(dat.ce.results.RE$time, 
      dat.ce.results.RE$ci.upper, col = cardiffblue, lwd = 2, lty = 2)

# Chapter RE
lines(dat.ce.results.RE.chapter$time, 
      dat.ce.results.RE.chapter$weighted.effect, col = cardiffred, lwd = 3)
lines(dat.ce.results.RE.chapter$time, 
      dat.ce.results.RE.chapter$ci.lower, col = cardiffred, lwd = 2, lty = 2)
lines(dat.ce.results.RE.chapter$time, 
      dat.ce.results.RE.chapter$ci.upper, col = cardiffred, lwd = 2, lty = 2)

# Depth RE
lines(dat.ce.results.RE.depth$time, 
      dat.ce.results.RE.depth$weighted.effect, col = cardiffgold, lwd = 3)
lines(dat.ce.results.RE.depth$time, 
      dat.ce.results.RE.depth$ci.lower, col = cardiffgold, lwd = 2, lty = 2)
lines(dat.ce.results.RE.depth$time, 
      dat.ce.results.RE.depth$ci.upper, col = cardiffgold, lwd = 2, lty = 2)


legend('bottomright', border = 'white', col = c(cardiffblue, cardiffred, cardiffgold),
       lwd = 3, legend = c("Main Effect", "Including Chapter", "Including Cooperation Depth") )

dev.off()























