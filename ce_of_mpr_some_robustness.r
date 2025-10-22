#===============================================================================
# Some Robustness Checks for Causal Effect of MPR
# Arnold & Schulz: "Reaching For The Threshold"
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================


# 1 Robustness: Placebo Timing ------------------------------------------------

# calculate interesting placebo time points to check
dat.treaty$rat_failure_time_at_threshold <- as.integer(
  dat.treaty$entry_force_date - dat.treaty$adoption_date)

rat_failure_time_at_threshold.cleaned <- na.omit(
  dat.treaty$rat_failure_time_at_threshold[dat.treaty$rat_failure_time_at_threshold != 0])

core.MPR.activation.timepoints <- quantile(rat_failure_time_at_threshold.cleaned, 
                                    probs = c(.35, .4, .45, .5, .55, .6, .65))

placebo.MPR.timepoints.to.check <- round(
  median(rat_failure_time_at_threshold.cleaned) - core.MPR.activation.timepoints)


# This is the lop over all placebo time cases
# calculate the average treatment effect across all treaties
placebo.ce.all.treaties <- list()
placebo.MPR.timepoints.to.check <- c(180, 90, 0, -90, -180, -270, -365)
for (j in seq(1, length(placebo.MPR.timepoints.to.check))){
  cat('Working on placebo timepoint',placebo.MPR.timepoints.to.check[j],'\n')
  placebo.time.manipulator.days <- placebo.MPR.timepoints.to.check[j]
  ce.all.treaties.6y.l <- vector(mode = 'list', length = length(treaty_codes))
  na.counter <- 0
  # mask for selecting those with a CE only
  for (i in seq(1,length(treaty_codes))) {
    if (i%%50 == 0) cat('Working on treaty #',i,'\n')
    one.ce <- estimate.ce.misc(dat.df.rat.misc.all.treaties, treaty_codes[i],
                                         self.calc.entry.force.date = FALSE,
                                         timespan.years = 10,
                                         placebo.time.manipulator.days = placebo.time.manipulator.days)
    ce.all.treaties.6y.l[[i]] <- one.ce
    try (if (is.na(ce.all.treaties.6y.l[[i]][1])) {
      na.counter <- na.counter + 1
    })
  }
  mask <- !is.na(ce.all.treaties.6y.l)
  ce.all.treaties.6y.l.wona <- ce.all.treaties.6y.l[mask]
  dat.ce.all.treaties.6y.wona <- do.call(rbind.data.frame, ce.all.treaties.6y.l.wona)
  placebo.ce.all.treaties[[j]] <- dat.ce.all.treaties.6y.wona
}

# calculate the average treatment effect across all treaties
average.ces.over.time <- function(dat){
  average.ce <- apply(dat, 2, mean)  
  return(average.ce)
}

placebo.ce.all.treaties.means <- lapply(placebo.ce.all.treaties, average.ces.over.time)

# Use Viridis Colours
placebo.MPR.timepoints.to.check.weeks <- placebo.MPR.timepoints.to.check/7
library(viridisLite)
v.colors <- viridis_pal(option = 'viridis')(6)

# Plotting 
# Appendix Figure 11
pdf("ce_mpr_time_placebos.pdf", width = 9)
plot(1,1, type = 'n', col = 'white', bty = 'n',
     ylab = 'CE of MPR (Kaplan Meier Difference)', xlab = 'Time after (Placebo-) MPR (Days)',
     ylim = c(-.5, .6), las =1, xlim = c(0, 1000))

abline(v = -1*placebo.MPR.timepoints.to.check[4:7], lwd = 2, lty = 2, 
       col = v.colors[3:6])
abline(v = 0, lwd = 2, lty = 2, 
       col = cardiffred)
abline(h = 0, lwd = 1, lty = 2, col = 'grey20')

lines(placebo.ce.all.treaties.means[[1]], col = v.colors[1], lwd = 2)
lines(placebo.ce.all.treaties.means[[2]], col = v.colors[2], lwd = 2)
lines(placebo.ce.all.treaties.means[[3]], col = cardiffred , lwd = 2)
lines(placebo.ce.all.treaties.means[[4]], col = v.colors[3], lwd = 2)
lines(placebo.ce.all.treaties.means[[5]], col = v.colors[4], lwd = 2)
lines(placebo.ce.all.treaties.means[[6]], col = v.colors[5], lwd = 2)
lines(placebo.ce.all.treaties.means[[7]], col = v.colors[6], lwd = 2)

legend('bottomright', col = c(v.colors[1], v.colors[2], cardiffred , 
                        v.colors[3], v.colors[4], v.colors[5], v.colors[6]), 
       bty = 'n',
       lty = 1, lwd = 2, legend = c(
         paste("Placebo Treatment at", placebo.MPR.timepoints.to.check[1], 'days'),
         paste("Placebo Treatment at", placebo.MPR.timepoints.to.check[2], 'days'),
         paste("Real Treatment (at", placebo.MPR.timepoints.to.check[3], 'days)'),
         paste("Placebo Treatment at", placebo.MPR.timepoints.to.check[4], 'days'),
         paste("Placebo Treatment at", placebo.MPR.timepoints.to.check[5], 'days'),
         paste("Placebo Treatment at", placebo.MPR.timepoints.to.check[6], 'days'),
         paste("Placebo Treatment at", placebo.MPR.timepoints.to.check[7], 'days')
       ))

dev.off()




# 2 Robustness: Placebo Treatments --------------------------------------------


# Calculate CE with loop over all treaties -------
ce.all.treaties.6y.l <- vector(mode = 'list', length = length(treaty_codes))
na.counter <- 0
for (i in seq(1,length(treaty_codes))) {
  if (i%%50 == 0) cat('Working on treaty #',i,'\n')
  one.ce <- estimate.ce.misc(dat.df.rat.misc.all.treaties, treaty_codes[i],
                                  self.calc.entry.force.date = FALSE,
                                  timespan.years = 10,
                                  placebo.case.manipulator = TRUE)
  ce.all.treaties.6y.l[[i]] <- one.ce
  try (if (is.na(ce.all.treaties.6y.l[[i]][1])) {
    na.counter <- na.counter + 1
  })
}

# plot results: All treaties -----------
cardiffblue.t <- adjustcolor( cardiffblue, alpha.f = 0.35) 
# Plotting
# Appendix Figure 9 
pdf("ce_mpr_all_placebo_treatment.pdf", width = 9)
plot(ce.all.treaties.6y.l[[1]], type = 'n', col = cardiffblue.t, bty = 'n',
     ylab = 'CE of MPR (Kaplan Meier Difference)', xlab = 'Time after MPR (Days)',
     ylim = c(-1, 1), xllim = c(0, 3000), las =1)
for (i in seq(1, length(ce.all.treaties.6y.l))){
    lines(ce.all.treaties.6y.l[[i]], col = cardiffblue.t, lwd = 0.5)
}
abline(h = 0, lwd = 1, lty = 1)
mask <- !is.na(ce.all.treaties.6y.l)
ce.all.treaties.6y.l.wona <- ce.all.treaties.6y.l[mask]
dat.ce.all.treaties.6y.wona <- do.call(rbind.data.frame, ce.all.treaties.6y.l.wona)
average.ce <- apply(dat.ce.all.treaties.6y.wona, 2, mean)
lines(average.ce, col = cardiffblue, lwd = 3)
dev.off()





# -- 3 Robustness Low and High MPR ---------------------------------------------

# Core idea: make a subset for small and another one for large treaties
# cutoff at median
cutoff.num.mpr <- median(dat.treaty$threshold_1_number[dat.treaty$threshold_1_number!=0])
# Low ------------
dat.df.rat.misc.all.treaties.lowmpr <- subset(
  dat.df.rat.misc.all.treaties, 
  dat.df.rat.misc.all.treaties$threshold_1_number < cutoff.num.mpr)
treaty_codes <- unique(dat.df.rat.misc.all.treaties.lowmpr$treaty_code)

# Calculate CE with loop over all treaties
ce.all.treaties.6y.l <- vector(mode = 'list', length = length(treaty_codes))
na.counter <- 0
# mask for selecting those with a CE only
for (i in seq(1,length(treaty_codes))) {
  if (i%%50 == 0) cat('Working on treaty #',i,'\n')
  one.ce <- estimate.ce.misc(dat.df.rat.misc.all.treaties.lowmpr, treaty_codes[i],
                             self.calc.entry.force.date = FALSE,
                             timespan.years = 10)
  ce.all.treaties.6y.l[[i]] <- one.ce
  try (if (is.na(ce.all.treaties.6y.l[[i]][1])) {
    na.counter <- na.counter + 1
  })
}

mask <- !is.na(ce.all.treaties.6y.l)
ce.all.treaties.6y.l.wona <- ce.all.treaties.6y.l[mask]
dat.ce.all.treaties.6y.wona <- do.call(rbind.data.frame, ce.all.treaties.6y.l.wona)
average.ce <- apply(dat.ce.all.treaties.6y.wona, 2, mean)
# saving vor later
average.ce.lowmpr <- average.ce



# High ------------
dat.df.rat.misc.all.treaties.highmpr <- subset(
  dat.df.rat.misc.all.treaties, 
  dat.df.rat.misc.all.treaties$threshold_1_number >= cutoff.num.mpr)
treaty_codes <- unique(dat.df.rat.misc.all.treaties.highmpr$treaty_code)

# Calculate CE with loop over all treaties -------
ce.all.treaties.6y.l <- vector(mode = 'list', length = length(treaty_codes))
na.counter <- 0
# mask for selecting those with a CE only
for (i in seq(1,length(treaty_codes))) {
  if (i%%50 == 0) cat('Working on treaty #',i,'\n')
  one.ce <- estimate.ce.misc(dat.df.rat.misc.all.treaties.highmpr, treaty_codes[i],
                             self.calc.entry.force.date = FALSE,
                             timespan.years = 10)
  ce.all.treaties.6y.l[[i]] <- one.ce
  try (if (is.na(ce.all.treaties.6y.l[[i]][1])) {
    na.counter <- na.counter + 1
  })
}

# calculate the average treatment effect across all treaties
mask <- !is.na(ce.all.treaties.6y.l)
ce.all.treaties.6y.l.wona <- ce.all.treaties.6y.l[mask]
dat.ce.all.treaties.6y.wona <- do.call(rbind.data.frame, ce.all.treaties.6y.l.wona)
average.ce <- apply(dat.ce.all.treaties.6y.wona, 2, mean)
# saving for later
average.ce.highmpr <- average.ce


# Plot the low and the high MPR in one figure -------
# Appendix Figure 8 
pdf("ce_mpr_lw_vs_high_mpr.pdf", width = 9)
plot(ce.all.treaties.6y.l[[1]], type = 'n', col = cardiffblue.t, bty = 'n',
     ylab = 'CE of MPR (Kaplan Meier Difference)', xlab = 'Time after MPR (Days)',
     ylim = c(-1, 1), xlim = c(0,3000), las =1)
abline(h = 0, lwd = 2, lty = 2, col = 'grey20')

lines(average.ce.lowmpr, col = cardiffred, lwd = 3)
lines(average.ce.highmpr, col = cardiffblue, lwd = 3)

legend('bottomright', col = c(cardiffblue, cardiffred), 
       bty = 'n',
       lty = 1, lwd = 2, legend = c('High Threshold', 'Low Threshold'))
dev.off()








