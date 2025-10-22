#===============================================================================
# Functions useful for the Estimation of the CE
# Reaching For The Threshold
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================

# Functions for the Estimation of the Ce on the basis of the 
# Kaplan Meier estimate (Fredriksson and Johansson 2008) 



# ------------------
# calculate the CE at one point in time
# In production it gets the first day of each week.
calculate.ce <- function(surv_model, ce_t){
    # Save time
    # All until time is reached
    surv_model$until_t <- surv_model$time_after_mpr < ce_t
    # Identify CEs
    # select the treatment group and then select the share of those who have
    # ratified by choosing the min of ratification shares in the TS of ratifications
    treatment.selector <- (surv_model$until_t & surv_model$treatment == 1)
    # makes sure there is at least one element in the condition
    if (length(unique(treatment.selector)) > 1){
        # Selects how many have ratified until this point in time
        surv_t_treat <- min(surv_model$surv[treatment.selector])
    } else {
        surv_t_treat <- 1
    }
    # same for the control group
    # Note: The contributions are different per case here, since they are 
    #       weighed in the control group: This is because there may be several 
    #       exact matches from the control group.
    control.selector <- (surv_model$until_t & surv_model$treatment == 0)
    if (length(unique(control.selector)) > 1){
        surv_t_contr <- min(surv_model$surv[control.selector])
    } else {
        surv_t_contr <- 1
    }
    # CE is Treatment - Control
    # basically: how many have ratified? and how many still have to? (double check)
    surv_t_ce <- surv_t_treat - surv_t_contr
    # out
    return(surv_t_ce)
}




# ------------------
# Function that take general data for misc events similar to ratification
# prepares data for matching, matches, and 
# estimates the CE al la Fredricksson and Johansson 2008
estimate.ce.misc <- function(dat.for.matching, treaty.code, 
                                  self.calc.entry.force.date = TRUE,
                                  timespan.years = 10,
                                  placebo.time.manipulator.days = 0,
                                  placebo.time.sample.selector = FALSE,
                                  placebo.case.manipulator = FALSE){
    # Check whether you want to run the placebo
    if (placebo.time.manipulator.days == 0){
      dat.temp <- make_data_for_matching(dat.for.matching, treaty.code)  
    }
    if (placebo.time.manipulator.days != 0){
      if (placebo.time.sample.selector == FALSE){
        dat.temp <- make_data_for_matching_time_placebo(
          dat.for.matching, treaty.code, placebo.time.manipulator.days
        )    
      }
      if (placebo.time.sample.selector == TRUE){
        dat.temp <- make_data_for_matching_time_placebo_select(
          dat.for.matching, treaty.code, placebo.time.manipulator.days
        )    
      }
    }
    # check whether the make_data_for_matching() returns NA. If so do not exe
    # if dimensions of data are not NULL, do:
    if (!is.null(dim(dat.temp))){
        # catch if matching works
        # MatchIt package
        exact.match <- NULL
        try(exact.match <- matchit(formula = treat ~ country_iso3c + 
                                     public_or_common_good + post_cold_war +
                                       treaty_action_category,
                                   data = dat.temp, method = 'exact'), TRUE)
        if(!is.null(exact.match)){
            exact.match.data <- match.data(exact.match)
            # this is the adaption for the placebo treatment shuffle
            if (placebo.case.manipulator == TRUE){
              exact.match.data$treat <- sample(exact.match.data$treat, replace = FALSE)  
            }
            # Treatment object
            surv_model <- survfit(Surv(rat_failure_time, 
                                       rat_misc_dummy) ~ treat,
                                  data = exact.match.data,
                                  conf.type = "log",
                                  weights = exact.match.data$weights,
                                  type = "kaplan-meier")
            # Calculate the time after the MPR:
            # Define the MPR time
            mpr.time <- unique(exact.match.data$rat_failure_time_at_threshold_tg)
            # Calculate time after the MPR (for comparisons later on)
            surv_model$time_after_mpr <- surv_model$time - mpr.time
            # identify the control and treatment strata
            surv_model$treatment <- c(rep(0, surv_model$strata[1]),
                                      rep(1, surv_model$strata[2]))
            # Estimate the CE 
            # max right-censored observation time of the treatment group in days
            max.time <- max(
                exact.match.data$rat_failure_time[exact.match.data$treat == 1])
            # take min in days
            possible.obs.span <- min(round(timespan.years*365.25), max.time)
            # general time ruler in weeks
            # time.ruler <- c(0,seq(1,possible.obs.span, 7)) 
            # better: general time ruler in days
            time.ruler <- seq(0,possible.obs.span, 1) # this is in days
            ce.over.time <- rep(NA, length(time.ruler))
            for (i in seq(1, length(time.ruler))){
                ce.over.time[i] <- calculate.ce(surv_model, time.ruler[i])  
            }
        } else {ce.over.time <- NA}
    } else {ce.over.time <- NA}
    return(ce.over.time)
}











# The same as above, but has a flexible input for the matching formula
estimate.ce.misc.robustness <- function(dat.for.matching, treaty.code, 
                                                  formula,
                                       self.calc.entry.force.date = TRUE,
                                       timespan.years = 6){
  dat.temp <- make_data_for_matching(dat.for.matching, treaty.code)
  # check whether the make_data_for_matching() returns NA. If so do not execute
  # if dimensions of data are not NULL, do:
  if (!is.null(dim(dat.temp))){
    # catch if matching works
    # MatchIt package
    exact.match <- NULL
    # THIS IS WHERE IT IS DIFFERENT
    # Reads in the formula from 
    try(exact.match <- eval(parse(text = formula)), TRUE)
    if(!is.null(exact.match)){
      exact.match.data <- match.data(exact.match)
      
      # Treatment object
      surv_model <- survfit(Surv(rat_failure_time, 
                                 rat_misc_dummy) ~ treat,
                            data = exact.match.data,
                            conf.type = "log",
                            weights = exact.match.data$weights,
                            type = "kaplan-meier")
      # Calculate the time after the MPR:
      # Define the MPR time
      mpr.time <- unique(exact.match.data$rat_failure_time_at_threshold_tg)
      # Calculate time after the MPR (for comparisons later on)
      surv_model$time_after_mpr <- surv_model$time - mpr.time
      # identify the control and treatment strata
      surv_model$treatment <- c(rep(0, surv_model$strata[1]),
                                rep(1, surv_model$strata[2]))
      # Estimate the CE 
      # max right-censored observation time of the treatment group in days
      max.time <- max(
        exact.match.data$rat_failure_time[exact.match.data$treat == 1])
      # take min in days
      possible.obs.span <- min(round(timespan.years*365.25), max.time)
      # general time ruler in weeks
      # time.ruler <- c(0,seq(1,possible.obs.span, 7)) 
      # better: general time ruler in days
      time.ruler <- seq(0,possible.obs.span, 1) # this is in days
      ce.over.time <- rep(NA, length(time.ruler))
      for (i in seq(1, length(time.ruler))){
        ce.over.time[i] <- calculate.ce(surv_model, time.ruler[i])  
      }
    } else {ce.over.time <- NA}
  } else {ce.over.time <- NA}
  return(ce.over.time)
}



# useful..?
estimate.ce.difference.misc <- function(dat.for.matching, treaty.code, 
                                       self.calc.entry.force.date = TRUE,
                                       timespan.years = 6){
  # check whether the make_data_for_matching() returns NA. If so do not execute
  dat.temp <- make_data_for_matching(dat.for.matching, treaty.code)  
  # if dimensions of data are not NULL, do:
  if (!is.null(dim(dat.temp))){
    # catch if matching works
    # MatchIt package
    exact.match <- NULL
    try(exact.match <- matchit(formula = treat ~ country_iso3c + 
                                 # public_good + common_good + post_cold_war +
                                 public_or_common_good + post_cold_war +
                                 treaty_action_category,
                               data = dat.temp, method = 'exact'), TRUE)
    if(!is.null(exact.match)){
      exact.match.data <- match.data(exact.match)
      # Add uncertainty regarding the difference of the curves 
      surv_diff <- survdiff(Surv(rat_failure_time, 
                                 rat_misc_dummy) ~ treat,
                            data = exact.match.data)
    } else {surv_diff <- NA}
  } else {surv_diff <- NA}
  return(surv_diff)
}





# TODO do we use this in the final replication file?

# Function that take general data for only ratification cases
# prepares data for matching, matches, and
# estimates the CE al la Fredricksson and Johansson 2008
estimate.ce.each.week <- function(dat.for.matching, treaty.code,
                                  self.calc.entry.force.date = TRUE,
                                  timespan.years = 6){
  dat.temp <- make_data_for_matching(dat.for.matching, treaty.code)
  # check whether the make_data_for_matching() returns NA. If so do not exe
  # if dimensions of data are not NULL, do:
  if (!is.null(dim(dat.temp))){
    # catch if matching works
    # MatchIt package
    exact.match <- NULL
    try(exact.match <- matchit(formula = treat ~ country_iso3c +
                                 public_good + post_cold_war,
                               data = dat.temp, method = 'exact'), TRUE)
    if(!is.null(exact.match)){
      exact.match.data <- match.data(exact.match)
      
      # Treatment object
      surv_model <- survfit(Surv(rat_failure_time,
                                 rat_dummy) ~ treat,
                            data = exact.match.data,
                            conf.type = "log",
                            weights = exact.match.data$weights,
                            # conf.type="plain",
                            type = "kaplan-meier")
      # Calculate the time after the MPR:
      # this approach with simple min() works because we pre-select only those
      # ratification spells that are  active after the MPR
      # in make_data_for_matching()
      surv_model$time_after_mpr <- surv_model$time - min(surv_model$time)
      # identify the control and treatment strata
      surv_model$treatment <- c(rep(0, surv_model$strata[1]),
                                rep(1, surv_model$strata[2]))
      # Estimate the CE
      # max right-censored observation time of the treatment group in days
      max.time <- max(
        exact.match.data$rat_failure_time[exact.match.data$treat == 0])
      # take min in days
      possible.obs.span <- min(round(timespan.years*365.25), max.time)
      # general time ruler in weeks
      time.ruler <- seq(1,possible.obs.span, 7)
      ce.over.time <- rep(NA, length(time.ruler))
      for (i in seq(1, length(time.ruler))){
        ce.over.time[i] <- calculate.ce(surv_model, time.ruler[i])
      }
    } else {ce.over.time <- NA}
  } else {ce.over.time <- NA}
  return(ce.over.time)
}






