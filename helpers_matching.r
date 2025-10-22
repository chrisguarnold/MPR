#===============================================================================
# Helpers for Matching the Data
# Reaching For The Threshold
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================


# -- Function that -----
# - selects the correct treatment group (ATE or ATT)
# - Allows to add those that are under the MPC treatment to the control group,
#    but of course then right censor them right before the treatment
# - bring both data sets together
# - Finally only select variables that you want under consideration for matching



# Function that 
make_data_for_matching <- function(dat.all, treaty.code){
  # Function that 
  #  - selects treaties with active MPRs 
  #  - takes countries with ratification after MPR threshold
  #  - add control cases
  dat <- subset(dat.all, dat.all$treaty_code == treaty.code)
  # only execute if the MPR really has been activated in at least one of the 
  # countries in a treaty. Matters for looping.
  # also, hand annotated entry_force_date must not be NA
  if (!is.na(table(dat$mpr_treatment)['TRUE']) & 
      !is.na(unique(dat$entry_force_date))){
    # time s^1 for treatment: take the value if not NA
    # -- Treatment Group -----
    # subset
    dat.treat <- subset(dat, dat$mpr_treatment == TRUE)
    # Define treatment group
    dat.treat$treat <- 1
    # explicitly write the tg rat failure time. You need the var again below.
    rat_failure_time_at_threshold_tg <- unique(dat$rat_failure_time_at_threshold)
    dat.treat$rat_failure_time_at_threshold_tg <- rat_failure_time_at_threshold_tg
    # cat(paste('treatment dims', dim(dat.treat), '\n'))
    #
    # -- Control Group -----
    # Filter criteria: Keep those that...
    # - are from another treaty 
    cond.1 <- dat.all$treaty_code != treaty.code
    # - have an MPR (should already be there, just a precaution to be complete)
    cond.2 <- dat.all$threshold_any_1_or_3
    # - do not receive a treatment 
    cond.3 <- dat.all$mpr_treatment == FALSE 
    # ratification in control group take place after treatment time 
    cond.4 <- dat.all$rat_failure_time > rat_failure_time_at_threshold_tg
    # join the conditions 
    control.group.selector <- cond.1 & cond.2 & cond.3 & cond.4
    # only execute if there are actually also TRUE conditions for the control group
    if (!is.na(table(control.group.selector)['TRUE'])){
      # now subset
      dat.contr <- subset(dat.all, subset = control.group.selector)
      # Define control group
      dat.contr$treat <- 0
      # Add the treatment time from the treatment group
      dat.contr$rat_failure_time_at_threshold_tg <- rat_failure_time_at_threshold_tg
      # join the two 
      dat.pre.m <-rbind(dat.treat, dat.contr)
      # -- Assemble Data -----
      dat.temp <- data.frame(dat.pre.m$treat, dat.pre.m$chapter, 
                             dat.pre.m$treaty_name,
                             dat.pre.m$treaty_year, dat.pre.m$post_cold_war,
                             dat.pre.m$country_iso3c, dat.pre.m$public_good,
                             dat.pre.m$nr_countries_in_treaty,
                             dat.pre.m$rat_dummy, dat.pre.m$rat_misc_dummy,
                             dat.pre.m$rat_failure_time,
                             dat.pre.m$rat_failure_time_at_threshold_tg,
                             dat.pre.m$threshold_2_time, dat.pre.m$entry_force_duration,
                             dat.pre.m$region_wdi_adapted, 
                             dat.pre.m$treaty_action_category, 
                             dat.pre.m$public_or_common_good,
                             dat.pre.m$common_good,
                             dat.pre.m$cooperation.depth)
      names(dat.temp) <- c("treat", "chapter", 
                           "treaty_name",
                           "treaty_year", "post_cold_war",
                           "country_iso3c", "public_good",
                           "nr_countries_in_treaty",
                           "rat_dummy", "rat_misc_dummy", "rat_failure_time",
                           "rat_failure_time_at_threshold_tg",
                           "threshold_2_time", "entry_force_duration",
                           "region_wdi_adapted", "treaty_action_category",
                           "public_or_common_good", "common_good",
                           "cooperation.depth")
      # na can come from public_good coding or entry_force_duration  
      dat.temp <- na.omit(dat.temp)
      # check whether there are any treatment observations left after the 
      # na.omit(). If not, return NA
      if (length(table(dat.temp$treat)) == 1){dat.temp <- NA}  
    } else {
      dat.temp <- NA
    }
  } else {
    dat.temp <- NA
  }
  return(dat.temp) 
}



# Same placebo function, but selecting 'scenario 2' cases for treatment and
# control group, too.
make_data_for_matching_time_placebo <- function(dat.all, treaty.code, placebo.time.manipulator.days){
  # Function that
  #  - selects treaties with active MPRs
  #  - takes countries with ratification after MPR threshold
  #  - add control cases
  # add it here so that you have it in the control group, too
  dat.all$mpr_treatment_placebo <- rep(NA, dim(dat.all)[1])
  # Calculate MPR treatment placebo for all (for efficiency this could be out of this loop...)
  dat.all$rat_failure_time_at_threshold_tg_placebo <- dat.all$rat_failure_time_at_threshold + placebo.time.manipulator.days
  dat.all$mpr_treatment_placebo[dat.all$rat_failure_time_at_threshold_tg_placebo <= dat.all$rat_failure_time] <- TRUE
  dat.all$mpr_treatment_placebo[dat.all$rat_failure_time_at_threshold_tg_placebo > dat.all$rat_failure_time] <- FALSE
  # select treaty to consider
  dat <- subset(dat.all, dat.all$treaty_code == treaty.code)
  # Recalculate the MPR treatment time for the treated on the basis of placebo time
  rat_failure_time_at_threshold_tg_placebo <- unique(dat$rat_failure_time_at_threshold) + placebo.time.manipulator.days
  # only execute if the MPR really has been activated in at least one of the
  # countries in a treaty. Matters for looping.
  # also, hand annotated entry_force_date must not be NA
  # also, there must not be an empty set of placebo treatment cases
  if (!is.na(table(dat$mpr_treatment_placebo)['TRUE']) &
      !is.na(unique(dat$entry_force_date))){
    # time s^1 for treatment: take the value if not NA
    # -- Treatment Group -----
    # subset
    # The 'new' false positive case
    dat.treat <- subset(dat, (dat$mpr_treatment_placebo == TRUE))
    # Define treatment group
    dat.treat$treat <- 1
    # explicitly write the tg rat failure time. You need the var again below.
    dat.treat$rat_failure_time_at_threshold_tg <- rat_failure_time_at_threshold_tg_placebo
    #
    # -- Control Group -----
    # Filter criteria: Keep those that...
    # - are from another treaty
    cond.1 <- dat.all$treaty_code != treaty.code
    # - have an MPR (should already be there, just a precaution to be complete)
    cond.2 <- dat.all$threshold_any_1_or_3
    # - do not receive a placebo treatment (which comes from a general treaty wise calculation)
    cond.3 <- dat.all$mpr_treatment== FALSE
    # ratification in control group take place after treatment time
    cond.4 <- dat.all$rat_failure_time > rat_failure_time_at_threshold_tg_placebo
    # join the conditions
    control.group.selector <- cond.1 & cond.2 & cond.3 & cond.4
    # only execute if there are actually also TRUE conditions for the control group
    if (!is.na(table(control.group.selector)['TRUE'])){
      # now subset
      dat.contr <- subset(dat.all, subset = control.group.selector)
      # Define control group
      dat.contr$treat <- 0
      # Add the treatment time from the treatment group
      dat.contr$rat_failure_time_at_threshold_tg <- rat_failure_time_at_threshold_tg_placebo
      # join the two
      dat.pre.m <-rbind(dat.treat, dat.contr)

      # -- Assemble Data -----
      dat.temp <- data.frame(dat.pre.m$treat, dat.pre.m$chapter,
                             dat.pre.m$treaty_name,
                             dat.pre.m$treaty_year, dat.pre.m$post_cold_war,
                             dat.pre.m$country_iso3c, dat.pre.m$public_good,
                             dat.pre.m$nr_countries_in_treaty,
                             dat.pre.m$rat_dummy, dat.pre.m$rat_misc_dummy,
                             dat.pre.m$rat_failure_time,
                             dat.pre.m$rat_failure_time_at_threshold_tg,
                             dat.pre.m$threshold_2_time, dat.pre.m$entry_force_duration,
                             dat.pre.m$region_wdi_adapted,
                             dat.pre.m$treaty_action_category,
                             dat.pre.m$public_or_common_good,
                             dat.pre.m$common_good)
      names(dat.temp) <- c("treat", "chapter",
                           "treaty_name",
                           "treaty_year", "post_cold_war",
                           "country_iso3c", "public_good",
                           "nr_countries_in_treaty",
                           "rat_dummy", "rat_misc_dummy", "rat_failure_time",
                           "rat_failure_time_at_threshold_tg",
                           "threshold_2_time", "entry_force_duration",
                           "region_wdi_adapted", "treaty_action_category",
                           "public_or_common_good", "common_good")
      # na can come from public_good coding or entry_force_duration
      dat.temp <- na.omit(dat.temp)
      # check whether there are any treatment observations left after the
      if (length(table(dat.temp$treat)) == 1){dat.temp <- NA}
    } else {
      dat.temp <- NA
    }
  } else {
    dat.temp <- NA
  }
  return(dat.temp)
}






# INCLUDE THIS


# This is the one that includes ONLY the new placebo group
# * reduce the treated 
# * control group: should be the same 
# * mabye plot only for the placebo time 
make_data_for_matching_time_placebo_select <- function(dat.all, treaty.code, placebo.time.manipulator.days){
  # Function that
  #  - selects treaties with active MPRs
  #  - takes countries with ratification after MPR threshold
  #  - add control cases
  # add it here so that you have it in the control group, too
  dat.all$mpr_treatment_placebo <- rep(NA, dim(dat.all)[1])
  # Calculate MPR treatment placebo for all (for efficiency this could be out of this loop...)
  dat.all$rat_failure_time_at_threshold_tg_placebo <- dat.all$rat_failure_time_at_threshold + placebo.time.manipulator.days
  dat.all$mpr_treatment_placebo[dat.all$rat_failure_time_at_threshold_tg_placebo <= dat.all$rat_failure_time] <- TRUE
  dat.all$mpr_treatment_placebo[dat.all$rat_failure_time_at_threshold_tg_placebo > dat.all$rat_failure_time] <- FALSE
  # select treaty to consider
  dat <- subset(dat.all, dat.all$treaty_code == treaty.code)
  # Recalculate the MPR treatment time for the treated on the basis of placebo time
  rat_failure_time_at_threshold_tg_placebo <- unique(dat$rat_failure_time_at_threshold) + placebo.time.manipulator.days
  # only execute if the MPR really has been activated in at least one of the
  # countries in a treaty. Matters for looping.
  # also, hand annotated entry_force_date must not be NA
  # also, there must not be an empty set of placebo treatment cases and real treatment cases
  if (!is.na(table(dat$mpr_treatment_placebo == TRUE & 
                   dat$mpr_treatment == FALSE)['TRUE']) &
      !is.na(unique(dat$entry_force_date))){
    # time s^1 for treatment: take the value if not NA
    # -- Treatment Group -----
    # subset
    # The 'new' false positive case
    dat.treat <- subset(dat, (dat$mpr_treatment_placebo == TRUE & 
                                dat$mpr_treatment == FALSE))
    # Define treatment group
    dat.treat$treat <- 1
    # explicitly write the tg rat failure time. You need the var again below.
    # rat_failure_time_at_threshold_tg <- unique(dat$rat_failure_time_at_threshold)
    dat.treat$rat_failure_time_at_threshold_tg <- rat_failure_time_at_threshold_tg_placebo
    #
    # -- Control Group -----
    # Filter criteria: Keep those that...
    # - are from another treaty
    cond.1 <- dat.all$treaty_code != treaty.code
    # - have an MPR (should already be there, just a precaution to be complete)
    cond.2 <- dat.all$threshold_any_1_or_3
    # - do not receive a placebo treatment (which comes from a general treaty wise calculation)
    cond.3 <- dat.all$mpr_treatment_placebo == FALSE
    # ratification in control group take place after treatment time, but before the treatment in their treaties
    cond.4 <- (dat.all$rat_failure_time > rat_failure_time_at_threshold_tg_placebo & 
                 dat.all$rat_failure_time < dat.all$rat_failure_time_at_threshold_tg_placebo)
    # join the conditions
    control.group.selector <- cond.1 & cond.2 & cond.3 & cond.4
    # only execute if there are actually also TRUE conditions for the control group
    if (!is.na(table(control.group.selector)['TRUE'])){
      # now subset
      dat.contr <- subset(dat.all, subset = control.group.selector)
      # Define control group
      dat.contr$treat <- 0
      # Add the treatment time from the treatment group
      dat.contr$rat_failure_time_at_threshold_tg <- rat_failure_time_at_threshold_tg_placebo
      # join the two
      dat.pre.m <-rbind(dat.treat, dat.contr)
      
      # -- Assemble Data -----
      dat.temp <- data.frame(dat.pre.m$treat, dat.pre.m$chapter,
                             dat.pre.m$treaty_name,
                             dat.pre.m$treaty_year, dat.pre.m$post_cold_war,
                             dat.pre.m$country_iso3c, dat.pre.m$public_good,
                             dat.pre.m$nr_countries_in_treaty,
                             dat.pre.m$rat_dummy, dat.pre.m$rat_misc_dummy,
                             dat.pre.m$rat_failure_time,
                             dat.pre.m$rat_failure_time_at_threshold_tg,
                             dat.pre.m$threshold_2_time, dat.pre.m$entry_force_duration,
                             dat.pre.m$region_wdi_adapted,
                             dat.pre.m$treaty_action_category,
                             dat.pre.m$public_or_common_good,
                             dat.pre.m$common_good)
      names(dat.temp) <- c("treat", "chapter",
                           "treaty_name",
                           "treaty_year", "post_cold_war",
                           "country_iso3c", "public_good",
                           "nr_countries_in_treaty",
                           "rat_dummy", "rat_misc_dummy", "rat_failure_time",
                           "rat_failure_time_at_threshold_tg",
                           "threshold_2_time", "entry_force_duration",
                           "region_wdi_adapted", "treaty_action_category",
                           "public_or_common_good", "common_good")
      # na can come from public_good coding or entry_force_duration
      dat.temp <- na.omit(dat.temp)
      # check whether there are any treatment observations left after the
      # na.omit(). If not, return NA
      if (length(table(dat.temp$treat)) == 1){dat.temp <- NA}
    } else {
      dat.temp <- NA
    }
  } else {
    dat.temp <- NA
  }
  return(dat.temp)
}




