#===============================================================================
# Functions useful for the Estimation of the CE: Updates from R&R Process in JCR
# Arnold & Schulz: "Reaching For The Threshold"
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================

# -- Functions for the Estimation of the CE ----------------------

# This now contains SE 
calculate.ce <- function(surv_model, ce_t){
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
        # Selects the standard error based on the ID of that position from the treated data
        surv_t_treat.se <- surv_model$std.err[treatment.selector][
          surv_model$surv[treatment.selector] == surv_t_treat
        ]
    } else {
        surv_t_treat <- 1
        surv_t_treat.se <- 0
    }
    # same for the control group
    # Note: The contributions are different per case here, since they are 
    #       weighed in the control group: This is because there may be several 
    #       exact matches from the control group.
    control.selector <- (surv_model$until_t & surv_model$treatment == 0)
    if (length(unique(control.selector)) > 1){
        surv_t_contr <- min(surv_model$surv[control.selector])
        # Selects the standard error based on the ID of that position 
        surv_t_contr.se <- surv_model$std.err[control.selector][
          surv_model$surv[control.selector]== surv_t_contr]
    } else {
        surv_t_contr <- 1
        surv_t_contr.se <- 0
    }
    # CE is Treatment - Control
    surv_t_ce <- surv_t_treat - surv_t_contr
    # SE is var_delta = var_treat + var_contr
    surv_t_ce.se <- sqrt(surv_t_treat.se^2 + surv_t_contr.se^2) 
    # out
    return(c(surv_t_ce, surv_t_ce.se))
}





# Function that take general data for misc events similar to ratification
# estimates the CE al la Fredricksson and Johansson 2008
# includes SE now
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
        # MatchIt package
        exact.match <- NULL
        try(exact.match <- matchit(formula = treat ~ country_iso3c + 
                                     # public_good + common_good + post_cold_war +
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
                                  # conf.type="plain",
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
            # general time ruler 
            time.ruler <- seq(0, possible.obs.span, 1) # this is in days
            ce.over.time <- ce.se.over.time <- rep(NA, length(time.ruler))
            # Now: with SE 
            dat.ce <- data.frame(ce.over.time, ce.se.over.time)
            for (i in seq(1, length(time.ruler))){
              dat.ce$ce.over.time[i] <- calculate.ce(surv_model, time.ruler[i])[1]
              dat.ce$ce.se.over.time[i] <- calculate.ce(surv_model, time.ruler[i])[2]
            }
        } else {
          ce.over.time <- ce.se.over.time <- NA
          dat.ce <- data.frame(ce.se.over.time, ce.over.time)}
    } else {
      ce.over.time <- ce.se.over.time <- NA
      dat.ce <- data.frame(ce.se.over.time, ce.over.time)
    }
    return(dat.ce)
}




# -- Meta Analysis Models ------------------------------------------------------

# Reshuffle Data in prep for meta analysis
harmonise.experiments <- function(delta.list, se.list, public_good, common_good) {
  combined.df <- data.frame()
  for (i in seq_along(delta.list)) {
    # Skip experiments that are NA
    if (is.na(delta.list[[i]])[1]) next
    # Extract delta and SE values
    delta.values <- delta.list[[i]]
    se.values <- se.list[[i]]
    # Ensure lengths match
    len <- length(delta.values)
    time <- seq_len(len)
    # Data frame for this experiment, including indicators
    exp.df <- data.frame(
      experiment = i,
      time = time,
      delta = delta.values,
      se.delta = se.values,
      public_good = public_good[i],
      common_good = common_good[i]
    )
    # Append to combined data frame
    combined.df <- rbind(combined.df, exp.df)
  }
  row.names(combined.df) <- NULL
  return(combined.df)
}


# This is the fixed effects model for aggreting results
aggregate_survival_FE <- function(dat.ce.all){
  unique.times <- sort(unique(dat.ce.all$time))
  # Dat
  dat.ce.results <- data.frame(
    time = numeric(0),
    weighted.effect = numeric(0),
    aggregated.se = numeric(0),
    lower.CI = numeric(0),
    upper.CI = numeric(0)
  )
  # Iterate
  for (t in unique.times){
    if (t%%100 == 0) cat('...', t, sep = '')
    subset.df <- dat.ce.all[dat.ce.all$time == t,]
    weighted.effect <- sum(subset.df$delta * subset.df$weight) / sum(subset.df$weight)
    aggregated.se <- sqrt(1 / sum(subset.df$weight))
    # CIs
    lower.CI <- weighted.effect - 1.96 * aggregated.se
    upper.CI <- weighted.effect + 1.96 * aggregated.se
    # Results
    dat.ce.results <- rbind(dat.ce.results, data.frame(
      time = t,
      weighted.effect = weighted.effect,
      aggregated.se = aggregated.se,
      ci.lower = lower.CI,
      ci.upper = upper.CI
    ))
  }
  row.names(dat.ce.results) <- NULL
  return(dat.ce.results)
}


# This is the Random Effects Model
aggregate_survival_meta_RE <- function(dat.ce.all) {
  time.points <- sort(unique(dat.ce.all$time))
  # Instantiate
  dat.ce.results.RE <- data.frame(
    time = numeric(),
    weighted.effect = numeric(),
    se = numeric(),
    ci.lower = numeric(),
    ci.upper = numeric(),
    tau2 = numeric(),
    I2 = numeric()
  )
  # Iterate
  for (t in time.points) {
    if (t %% 100 == 0) cat('...', t, sep = '')
    subset.dat <- dat.ce.all[dat.ce.all$time == t, ]
    if (nrow(subset.dat) < 2) next
    
    res.df <- tryCatch({
      res <- metagen(
        TE = subset.dat$delta,
        seTE = subset.dat$se.delta,
        sm = "MD", 
        method.tau = "REML"
      )
      data.frame(
        time = t,
        weighted.effect = res$TE.random,
        se = res$seTE.random,
        ci.lower = res$lower.random,
        ci.upper = res$upper.random,
        tau2 = res$tau2,
        I2 = res$I2
      )
    }, error = function(e) {
      data.frame(
        time = t,
        weighted.effect = NA,
        se = NA,
        ci.lower = NA,
        ci.upper = NA,
        tau2 = NA,
        I2 = NA
      )
    })
    
    dat.ce.results.RE <- rbind(dat.ce.results.RE, res.df)
  }
  row.names(dat.ce.results.RE) <- NULL
  return(dat.ce.results.RE)
}


# For comparing the survival curves between different good types
compare_survival_curves <- function(surv_treat, ci_upper_treat, ci_lower_treat,
                                    surv_control, ci_upper_control, ci_lower_control,
                                    time_points) {
  # Compute standard errors from confidence intervals
  se_treat <- (ci_upper_treat - ci_lower_treat) / (2 * 1.96)
  se_control <- (ci_upper_control - ci_lower_control) / (2 * 1.96)
  # Initialize results data frame
  results <- data.frame(
    time = time_points,
    delta = surv_treat - surv_control,
    se_treat = se_treat,
    se_control = se_control,
    se_diff = sqrt(se_treat^2 + se_control^2),
    z_score = NA,
    p_value = NA
  )
  # Compute z-scores and p-values
  results$z_score <- results$delta / results$se_diff
  results$p_value <- 2 * (1 - pnorm(abs(results$z_score)))
  return(results)
} 



# -- Further Robustness for Appendix ---------------------------------------
# Not very elegant, but does the trick...
# Same function from above but also including depth in the matching formula
# Also contains SE
estimate.ce.misc.depth <- function(dat.for.matching, treaty.code, 
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
    # MatchIt package
    exact.match <- NULL
    try(exact.match <- matchit(formula = treat ~ country_iso3c + 
                                 # public_good + common_good + post_cold_war +
                                 public_or_common_good + post_cold_war +
                                 treaty_action_category + cooperation.depth,
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
                            # conf.type="plain",
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
      # time ruler
      time.ruler <- seq(0, possible.obs.span, 1) # this is in days
      ce.over.time <- ce.se.over.time <- rep(NA, length(time.ruler))
      # New 
      dat.ce <- data.frame(ce.over.time, ce.se.over.time)
      for (i in seq(1, length(time.ruler))){
        dat.ce$ce.over.time[i] <- calculate.ce(surv_model, time.ruler[i])[1]
        dat.ce$ce.se.over.time[i] <- calculate.ce(surv_model, time.ruler[i])[2]
      }
    } else {
      ce.over.time <- ce.se.over.time <- NA
      dat.ce <- data.frame(ce.se.over.time, ce.over.time)}
  } else {
    ce.over.time <- ce.se.over.time <- NA
    dat.ce <- data.frame(ce.se.over.time, ce.over.time)
  }
  return(dat.ce)
}


# Same function from above but also including chapter in the matching formula
# SE
estimate.ce.misc.chapter <- function(dat.for.matching, treaty.code, 
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
    # MatchIt package
    exact.match <- NULL
    try(exact.match <- matchit(formula = treat ~ country_iso3c + 
                                 # public_good + common_good + post_cold_war +
                                 public_or_common_good + post_cold_war +
                                 treaty_action_category + chapter,
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
                            # conf.type="plain",
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
      # time ruler
      time.ruler <- seq(0, possible.obs.span, 1) # this is in days
      ce.over.time <- ce.se.over.time <- rep(NA, length(time.ruler))
      # New 
      dat.ce <- data.frame(ce.over.time, ce.se.over.time)
      for (i in seq(1, length(time.ruler))){
        dat.ce$ce.over.time[i] <- calculate.ce(surv_model, time.ruler[i])[1]
        dat.ce$ce.se.over.time[i] <- calculate.ce(surv_model, time.ruler[i])[2]
      }
    } else {
      ce.over.time <- ce.se.over.time <- NA
      dat.ce <- data.frame(ce.se.over.time, ce.over.time)}
  } else {
    ce.over.time <- ce.se.over.time <- NA
    dat.ce <- data.frame(ce.se.over.time, ce.over.time)
  }
  return(dat.ce)
}


















