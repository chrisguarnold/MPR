#===============================================================================
# Data Helpers 
# Reaching For The Threshold
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================



data.enricher.p.treaty <- function(data, select.treaty, 
                                   self.calc.entry.force = FALSE){
  # This is a function that devises country level data at the treaty level.
  dat.treaty <- subset(data, data$treaty_code == select.treaty)
  
  # 1 Calculate ratification failure time 
  dat.treaty$rat_failure_time <- as.integer(
    dat.treaty$action_date - dat.treaty$adoption_date)
  # Correct for those occasions where ratification on the very first day
  dat.treaty$rat_failure_time[dat.treaty$rat_failure_time == 0] <- 1
  # Right censoring: failure time for non ratifiers
  non_ratifiers_fail_time <- as.Date("2018-08-01") - dat.treaty$adoption_date[1]
  dat.treaty$rat_failure_time[is.na(dat.treaty$action_date)] <- non_ratifiers_fail_time
  
  # 2 Calculate MPR failure time
  dat.treaty$rat_failure_time_at_threshold <- as.integer(
    dat.treaty$entry_force_date - dat.treaty$adoption_date)
  
  # 3 Calculate MPR treatment
  dat.treaty$mpr_treatment <- rep(NA, dim(dat.treaty)[1])
  dat.treaty$mpr_treatment[dat.treaty$rat_failure_time_at_threshold <= dat.treaty$rat_failure_time] <- TRUE  
  dat.treaty$mpr_treatment[dat.treaty$rat_failure_time_at_threshold > dat.treaty$rat_failure_time] <- FALSE  
  
  # 4 How many countries signed?
  dat.treaty$nr_countries_in_treaty <- dim(dat.treaty)[1]
  # How many countries from which region are in the treaty?
  dat.treaty$cntrs_in_treaty_from_Caribbean <- sum(dat.treaty$region_wdi_adapted_Caribbean)
  dat.treaty$cntrs_in_treaty_from_Central_America <- sum(dat.treaty$region_wdi_adapted_Central_America)
  dat.treaty$cntrs_in_treaty_from_Central_Asia <- sum(dat.treaty$region_wdi_adapted_Central_Asia)
  dat.treaty$cntrs_in_treaty_from_Eastern_Africa <- sum(dat.treaty$region_wdi_adapted_Eastern_Africa)
  dat.treaty$cntrs_in_treaty_from_Eastern_Asia <- sum(dat.treaty$region_wdi_adapted_Eastern_Asia)
  dat.treaty$cntrs_in_treaty_from_Eastern_Europe <- sum(dat.treaty$region_wdi_adapted_Eastern_Europe)
  dat.treaty$cntrs_in_treaty_from_Middle_Africa <- sum(dat.treaty$region_wdi_adapted_Middle_Africa)
  dat.treaty$cntrs_in_treaty_from_Northern_Africa <- sum(dat.treaty$region_wdi_adapted_Northern_Africa)
  dat.treaty$cntrs_in_treaty_from_NA <- sum(dat.treaty$region_wdi_adapted_NA)
  dat.treaty$cntrs_in_treaty_from_Northern_America <- sum(dat.treaty$region_wdi_adapted_Northern_America)
  dat.treaty$cntrs_in_treaty_from_Northern_Europe <- sum(dat.treaty$region_wdi_adapted_Northern_Europe)
  dat.treaty$cntrs_in_treaty_from_Oceania <- sum(dat.treaty$region_wdi_adapted_Oceania)
  dat.treaty$cntrs_in_treaty_from_South_America <- sum(dat.treaty$region_wdi_adapted_South_America)
  dat.treaty$cntrs_in_treaty_from_South_Eastern_Asia <- sum(dat.treaty$region_wdi_adapted_South_Eastern_Asia)
  dat.treaty$cntrs_in_treaty_from_Southern_Africa <- sum(dat.treaty$region_wdi_adapted_Southern_Africa)
  dat.treaty$cntrs_in_treaty_from_Southern_Asia <- sum(dat.treaty$region_wdi_adapted_Southern_Asia)
  dat.treaty$cntrs_in_treaty_from_Southern_Europe <- sum(dat.treaty$region_wdi_adapted_Southern_Europe)
  dat.treaty$cntrs_in_treaty_from_Western_Africa <- sum(dat.treaty$region_wdi_adapted_Western_Africa)
  dat.treaty$cntrs_in_treaty_from_Western_Asia <- sum(dat.treaty$region_wdi_adapted_Western_Asia)
  dat.treaty$cntrs_in_treaty_from_Western_Europe <- sum(dat.treaty$region_wdi_adapted_Western_Europe)
  
  # return
  return(dat.treaty)
}


