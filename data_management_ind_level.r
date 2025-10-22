#===============================================================================
# Some further data management at the individual country level
# Arnold & Schulz: "Reaching For The Threshold"
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================

load(file = "dat_wo_doubles.RData")


# Subsetting
# Get rid of some instances with weird data annotations due to countries 
# taking over treaties
dat.df.rat.misc.all.treaties <- subset(dat.df.rat.misc.all.treaties, dat.df.rat.misc.all.treaties$rat_failure_time > 0 )
# Select actions
dat.df.rat.misc.all.treaties <- subset(dat.df.rat.misc.all.treaties, subset = (
  dat.df.rat.misc.all.treaties$treaty_action_category == 1 |
    dat.df.rat.misc.all.treaties$treaty_action_category == 2 | 
    dat.df.rat.misc.all.treaties$treaty_action_category ==4))

# Creating further variables
dat.df.rat.misc.all.treaties$public_or_common_good <- 0 
dat.df.rat.misc.all.treaties$public_or_common_good[dat.df.rat.misc.all.treaties$public_good == 1] <- 1
dat.df.rat.misc.all.treaties$public_or_common_good[dat.df.rat.misc.all.treaties$common_good == 1] <- 1

# Treaty codes
treaty_codes <- unique(dat.df.rat.misc.all.treaties$treaty_code)

# Public Good
dat.temp <- aggregate(dat.df.rat.misc.all.treaties$public_good, 
                      by = list(dat.df.rat.misc.all.treaties$treaty_code), unique)
public_good <- rep(NA, length(treaty_codes))
for (i in seq(1,length(treaty_codes))) {
  public_good[i] <- dat.temp$x[dat.temp$Group.1 == treaty_codes[i]]
}

# Common Good
dat.temp <- aggregate(dat.df.rat.misc.all.treaties$common_good, 
                      by = list(dat.df.rat.misc.all.treaties$treaty_code), unique)

common_good <- rep(NA, length(treaty_codes))
for (i in seq(1,length(treaty_codes))) {
  common_good[i] <- dat.temp$x[dat.temp$Group.1 == treaty_codes[i]]
}

