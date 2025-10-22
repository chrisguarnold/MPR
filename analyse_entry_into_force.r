#===============================================================================
# Hypothesis 2a: What drives when states satisfy the MPR? 
# Arnold & Schulz: "Reaching For The Threshold"
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================




# -- 1 Run the models ----------------------------------------------------------
# This is just the subset of the data with those that have a nr threshold
dat.treaty.w.nr.threshold <- subset(dat.treaty, subset = dat.treaty$threshold_1_dummy == TRUE)

# Main model for manuscript 
# Contains idealpoints for preferences and also the DSM
m.cox.manuscript <- coxph(Surv((threshold_reached_date - adoption_date), entry_force_binary) ~ 
                        # Substantive Effects
                        public_good +
                        common_good +
                        # Controls
                        # how many states in the number threshold?
                        threshold_1_number +
                        # How fancy the threshold?
                        # Reference here is those with just numbers threshold
                        threshold_3_qualified + #complex
                        threshold_2_dummy + #time
                        # Boilerplates
                        threshold_1_boilerplate_dummy +
                        # Other Controls
                        any_dsm_gpt4o +
                        countries.p.treaty +
                        post_cold_war +
                        any.p5.in.treaty +
                        US.in.treaty +
                        idealpoint.sd +
                        c_by_t_sample +
                        factor(chapter_title_consolidated),
                      control = coxph.control(iter.max = 150),
                      data = dat.treaty.w.nr.threshold)
# summary(m.cox.manuscript)


# Model for figure in appendix
m.cox.appendix <- coxph(Surv((threshold_reached_date - adoption_date), entry_force_binary) ~ 
                          # Substantive Effects
                          public_good +
                          common_good +
                          # Controls
                          # how many states in the number threshold?
                          threshold_1_number +
                          # How fancy the threshold?
                          # Reference here is those with just numbers threshold
                          threshold_3_qualified + #complex
                          threshold_2_dummy + #time
                          # Boilerplates
                          threshold_1_boilerplate_dummy + 
                          # Other Controls
                          countries.p.treaty +
                          post_cold_war + 
                          US.in.treaty +
                          any.p5.in.treaty + 
                          c_by_t_sample +
                          cooperation.depth +
                          factor(chapter_title_consolidated),
                        data = dat.treaty.w.nr.threshold)
# summary(m.cox.appendix)


# -- 2 Plot Figure from Changes in Effects -------------------------------------

# 2.1 Functions for plotting ---------------
# Calculates the change in effects. 
# How much is the risk of ratification different from a higher covariate value than from a lower?
# If dummy, how much is the risk of ratification different from a covar value = 1 than from a covar value = 0?
calc.effect.change <- function(summary.m, low.val, high.val){
  coefs <- summary.m$coefficients[,1]
  ses <- summary.m$coefficients[,3]
  # from Box-Steffensmeier/Jones (2004) p.60
  hr.change <- (exp(coefs*high.val) - exp(coefs*low.val))/exp(coefs*low.val)
  ci.upper <- (exp((coefs+1.96*ses)*high.val) - exp((coefs+1.96*ses)*low.val))/exp((coefs+1.96*ses)*low.val)
  ci.lower <- (exp((coefs-1.96*ses)*high.val) - exp((coefs-1.96*ses)*low.val))/exp((coefs-1.96*ses)*low.val)
  return(data.frame(hr.change, ci.upper, ci.lower)*100)  
}

# To plot effects that are significant in different colours
effect.plotter <- function(dat, i, col = col){
  # if the 0 is in the confidence interval, make colour transparent
  if (dat$ci.lower[i] * dat$ci.upper[i] < 0){
    col <- adjustcolor(col, alpha.f = 0.4)
  }
  points(dat$hr.change[i],i, col = col, pch = 16)
  lines(c(dat$ci.lower[i], dat$ci.upper[i]), 
        c(i,i), col = col)
}


# 2.2 Figure main manuscript ------------------
low.vals <- c(0,0,quantile(dat.treaty.w.nr.threshold$threshold_1_number, 0.25), 
              0, FALSE, FALSE,
              0, 
              quantile(dat.treaty.w.nr.threshold$countries.p.treaty, 0.25), 
              rep(0, 3),
              # conceptually this is low alignment
              quantile(dat.treaty.w.nr.threshold$idealpoint.sd, 0.75, na.rm=TRUE), 
              0, 
              rep(0, length(m.cox.manuscript$coefficients)-13))


high.vals <- c(1,1,quantile(dat.treaty.w.nr.threshold$threshold_1_number, 0.75), 
               1, TRUE, TRUE,
               1,
               quantile(dat.treaty.w.nr.threshold$countries.p.treaty, 0.75), 
               rep(1, 3),
               # conceptually this is high alignment
               quantile(dat.treaty.w.nr.threshold$idealpoint.sd, 0.25, na.rm=TRUE), 
               1, 
               rep(1, length(m.cox.manuscript$coefficients)-13))



dat.effects <- calc.effect.change(summary(m.cox.manuscript), low.vals, high.vals)
dat.effects.wo.c <- dat.effects[1:13,]
# reverse the order for plotting
dat.effects.wo.c<- dat.effects.wo.c[seq(dim(dat.effects.wo.c)[1],1),]

# Plotting
# Figure 6 Manuscript
pdf('reaching_MPR_idealpoints.pdf')
par(mfrow = c(1,1), mar = c(5,10,1,1))
# Plot Results
plot(1,1,type='n', bty = 'n', ylim =c(0.5,dim(dat.effects.wo.c)[1]+.5), 
     xlim = c(-100, max(dat.effects.wo.c$ci.upper)),
     # xlim = c(-100, 1500), 
     xlab = 'Change in Risk of Satisfying the MPR (in %)', ylab = '',
     yaxt='n')
axis(2, las = 1,
     labels = c("M&S 2016", "Sign. Alignment",
                "US in Treaty", "Any P5 in Treaty", 'Post Cold War', 
                'Nr. of Sign. Countries',
                "Any DSM",
                'Boilerplate MPR',
                "Time MPR", 
                "Qualified MPR",
                "MPR Threshold (Nr.)",
                'Common Good',
                'Public Good'), 
     at = c(seq(1, 11), 12.5, 13.5))
abline(v=0, lty = 2, col = "grey60")
for (i in c(seq(1, 11), 12.5, 13.5)){
  effect.plotter(dat.effects.wo.c, i, col = cardiffblue)
}
dev.off()


# 2.2 Figure Appendix ------------------
# Calc Vals
low.vals <- c(0,0,quantile(dat.treaty.w.nr.threshold$threshold_1_number, 0.25), 
              0, FALSE, FALSE,
              quantile(dat.treaty.w.nr.threshold$countries.p.treaty, 0.25), 
              rep(0, 5),
              rep(0, length(m.cox.appendix$coefficients)-12))

high.vals <- c(1,1,quantile(dat.treaty.w.nr.threshold$threshold_1_number, 0.75), 
               1, TRUE, TRUE,
               quantile(dat.treaty.w.nr.threshold$countries.p.treaty, 0.75), 
               rep(1, 5), 
               rep(1, length(m.cox.appendix$coefficients)-12))

dat.effects <- calc.effect.change(summary(m.cox.appendix), low.vals, high.vals)
# bye bye controls
dat.effects.wo.c <- dat.effects[1:12,]
# reverse the order for plotting
dat.effects.wo.c<- dat.effects.wo.c[seq(dim(dat.effects.wo.c)[1],1),]

# Plotting
# Figure 6 Appendix
pdf('reaching_MPR_new_15_reorder.pdf')
par(mfrow = c(1,1), mar = c(5,10,1,1))
# Plot Results
plot(1,1,type='n', bty = 'n', ylim =c(0.5,dim(dat.effects.wo.c)[1]+.5), 
     xlim = c(-100, max(dat.effects.wo.c$ci.upper)), 
     xlab = 'Change in Risk of Satisfying the MPR (in %)', ylab = '',
     yaxt='n')
axis(2, at = seq(1,dim(dat.effects.wo.c)[1]), las = 1,
     labels = c("Cooperation Depth","M&S 2016",
                "Any P5 in Treaty", "US in Treaty", 'Post Cold War', 
                'Nr. of Sign. Countries',
                'Boilerplate MPR',
                "Time MPR", 
                "Qualified MPR",
                "MPR Threshold (Nr.)",
                'Common Good',
                'Public Good'))
abline(v=0, lty = 2, col = "grey60")
for (i in seq(1, dim(dat.effects.wo.c)[1])){
  effect.plotter(dat.effects.wo.c, i, col = cardiffblue)
}
dev.off()




# -- 3 Write Regression Tables -------------------------------------------------

# Models with idealpoints
cat("\n\n-------- Regression Results for Appendix Table 1 Model 1 -----\n\n")
m.cox.id <- coxph(Surv((threshold_reached_date - adoption_date), entry_force_binary) ~ 
                 # Substantive Effects
                 public_good +
                 common_good +
                 # Controls
                 # how many states in the number threshold?
                 threshold_1_number +
                 # How fancy the threshold?
                 # Reference here is those with just numbers threshold
                 threshold_3_qualified + #complex
                 threshold_2_dummy + #time
                 # Boilerplates
                 threshold_1_boilerplate_dummy + 
                 # Other Controls
                 countries.p.treaty +
                 post_cold_war + 
                 US.in.treaty +
                 any.p5.in.treaty + 
                 idealpoint.sd +
                 c_by_t_sample + 
                 any_dsm_gpt4o,
               data = dat.treaty.w.nr.threshold)

print(summary(m.cox.id))

cat("\n\n-------- Regression Results for Appendix Table 1 Model 2 -----\n\n")
# (same as m.cox.manuscript above)
m.cox.id.c <- coxph(Surv((threshold_reached_date - adoption_date), entry_force_binary) ~ 
                   # Substantive Effects
                   public_good +
                   common_good +
                   # Controls
                   # how many states in the number threshold?
                   threshold_1_number +
                   # How fancy the threshold?
                   # Reference here is those with just numbers threshold
                   threshold_3_qualified + #complex
                   threshold_2_dummy + #time
                   # Boilerplates
                   threshold_1_boilerplate_dummy + 
                   # Other Controls
                   countries.p.treaty +
                   post_cold_war + 
                   US.in.treaty +
                   any.p5.in.treaty + 
                   idealpoint.sd +
                   c_by_t_sample +
                   any_dsm_gpt4o +
                   factor(chapter_title_consolidated),
                 control = coxph.control(iter.max = 150),
                 data = dat.treaty.w.nr.threshold)
print(summary(m.cox.id.c))




# Alternative specification with other substantive vars
cat("\n\n-------- Regression Results for Appendix Table 2 Model 1 -----\n\n")
m.cox <- coxph(Surv((threshold_reached_date - adoption_date), entry_force_binary) ~ 
                             # Substantive Effects
                             public_good +
                             common_good +
                             # Controls
                             # how many states in the number threshold?
                             threshold_1_number +
                             # How fancy the threshold?
                             # Reference here is those with just numbers threshold
                             threshold_3_qualified + #complex
                             threshold_2_dummy + #time
                             # Boilerplates
                             threshold_1_boilerplate_dummy + 
                             # Other Controls
                             countries.p.treaty +
                             post_cold_war + 
                             US.in.treaty +
                             any.p5.in.treaty + 
                             c_by_t_sample +
                             cooperation.depth,
                           data = dat.treaty.w.nr.threshold)

print(summary(m.cox))

# TODO add any_dsm_gpt4o to the appendix alternative specification models

cat("\n\n-------- Regression Results for Appendix Table 2 Model 2 -----\n\n")
# (same as m.cox.appendix above)
m.cox.c <- coxph(Surv((threshold_reached_date - adoption_date), entry_force_binary) ~ 
                   # Substantive Effects
                   public_good +
                   common_good +
                   # Controls
                   # how many states in the number threshold?
                   threshold_1_number +
                   # How fancy the threshold?
                   # Reference here is those with just numbers threshold
                   threshold_3_qualified + #complex
                   threshold_2_dummy + #time
                   # Boilerplates
                   threshold_1_boilerplate_dummy + 
                   # Other Controls
                   countries.p.treaty +
                   post_cold_war + 
                   US.in.treaty +
                   any.p5.in.treaty + 
                   c_by_t_sample +
                   cooperation.depth +
                 factor(chapter_title_consolidated),
                 data = dat.treaty.w.nr.threshold)
print(summary(m.cox.c))

