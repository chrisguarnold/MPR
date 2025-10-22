#===============================================================================
# Data Management for the UN Treaty Series
# Arnold & Schulz: "Reaching For The Threshold"
# Chris Arnold, University of Birmingham, October 2025
#===============================================================================

# Output: 
#   dat.treaty aggregates all information up to the treaty level.
#   dat.df.rat.misc.all.treaties uses all actions similar to ratification


# -- Add the reviewed annotation of common and public goods  -------------------
dat.pc.ann <- read.csv("dat_public_common_annotation.csv")

# assert no miscodings
if (dim(unique(dat.pc.ann))[1] != length(unique(dat.pc.ann$treaty_code))){
  cat('ERROR with the codings of the public and common good annotations')
}

treaties <- unique(dat$treaty_code)
dat$public_good_old <- dat$public_good
dat$common_good <- dat$public_good <- NA

for (i in seq(1, length(treaties))){
  treaty.name <- treaties[i]
  dat$common_good[dat$treaty_code == treaty.name] <- unique(dat.pc.ann$common_good[dat.pc.ann$treaty_code == treaty.name])
  dat$public_good[dat$treaty_code == treaty.name] <- unique(dat.pc.ann$public_good[dat.pc.ann$treaty_code == treaty.name])
}



# 1 General Data Housekeeping --------------------------------------------------
dat$adoption_date <- as.Date(dat$adoption_date, optional = TRUE)
dat$entry_force_date <- as.Date(dat$entry_force_date, optional = TRUE)
dat$registration_date <- as.Date(dat$registration_date, optional = TRUE)
dat$action_date <- as.Date(dat$action_date, optional = TRUE)

# Dummies for Selecting Thresholds
dat$threshold_combo_all <- rep(FALSE, nrow(dat))
dat$threshold_combo_all[dat$threshold_1_number > 0 &
                          dat$threshold_2_time > 0 & dat$threshold_3_qualified == 1] <- TRUE

dat$threshold_combo_1_and_3 <- rep(FALSE, nrow(dat))
dat$threshold_combo_1_and_3[
  dat$threshold_1_number > 0 & dat$threshold_3_qualified == 1] <- TRUE

dat$threshold_combo_1_and_2 <- rep(FALSE, nrow(dat))
dat$threshold_combo_1_and_2[
  dat$threshold_1_number > 0 & dat$threshold_2_time > 0] <- TRUE

dat$threshold_combo_2_and_3 <- rep(FALSE, nrow(dat))
dat$threshold_combo_2_and_3[
  dat$threshold_2_time > 0 & dat$threshold_3_qualified == 1] <- TRUE

dat$threshold_only_combo_1_and_3 <- rep(FALSE, nrow(dat))
dat$threshold_only_combo_1_and_3[
  dat$threshold_1_number > 0 & dat$threshold_3_qualified == 1 & 
  dat$threshold_combo_all == FALSE] <- TRUE

dat$threshold_only_combo_1_and_2 <- rep(FALSE, nrow(dat))
dat$threshold_only_combo_1_and_2[
  dat$threshold_1_number > 0 & dat$threshold_2_time > 0 & 
  dat$threshold_combo_all == FALSE] <- TRUE

dat$threshold_only_combo_2_and_3 <- rep(FALSE, nrow(dat))
dat$threshold_only_combo_2_and_3[
  dat$threshold_2_time > 0 & dat$threshold_3_qualified == 1 & 
  dat$threshold_combo_all == FALSE] <- TRUE

dat$threshold_any_1_or_3 <- rep(FALSE, nrow(dat))
dat$threshold_any_1_or_3[
  dat$threshold_1_number > 0 | dat$threshold_3_qualified == 1] <- TRUE

dat$threshold_1_dummy <- FALSE
dat$threshold_1_dummy[dat$threshold_1_number > 0] <- TRUE

dat$threshold_2_dummy <- FALSE
dat$threshold_2_dummy[dat$threshold_2_time > 0] <- TRUE

# for consistency
dat$threshold_3_dummy <- dat$threshold_3_qualified

dat$threshold_only_1_dummy  <- rep(FALSE, nrow(dat))
dat$threshold_only_1_dummy[
  dat$threshold_1_dummy == TRUE & dat$threshold_combo_1_and_3 == FALSE & 
  dat$threshold_combo_1_and_2 == FALSE & dat$threshold_combo_all == FALSE] <- TRUE

dat$threshold_only_2_dummy  <- rep(FALSE, nrow(dat))
dat$threshold_only_2_dummy[
  dat$threshold_2_dummy == TRUE & dat$threshold_combo_1_and_2 == FALSE & 
  dat$threshold_combo_2_and_3 == FALSE & dat$threshold_combo_all == FALSE] <- TRUE

dat$threshold_only_3_dummy  <- rep(FALSE, nrow(dat))
dat$threshold_only_3_dummy[
  dat$threshold_3_qualified == TRUE & dat$threshold_combo_1_and_3 == FALSE & 
  dat$threshold_combo_2_and_3 == FALSE & dat$threshold_combo_all == FALSE] <- TRUE

# Calculate duration of entry into force
dat$entry_force_duration <- dat$entry_force_date - dat$adoption_date

# Correct some glitches in the location
dat$location <- car::recode(dat$location, "'Aarhus Denmark'='Aarhus';
                            'Lake Success New York'='Lake Success'")

# Was the treaty signed during the cold war?
dat$post_cold_war <- NA
dat$post_cold_war[dat$treaty_year >= 1990] <- 1
dat$post_cold_war[dat$treaty_year < 1990] <- 0

# Dummies for the respective regions
# Collapsing the oceania vars and checking missings
dat$region_wdi_adapted <- car::recode(dat$region_wdi,
                                      "'Melanesia'='Oceania';
                                      'Micronesia'='Oceania';
                                      'Polynesia'='Oceania';
                                      'Australia and New Zealand'='Oceania';
                                      ''='NA'")
# replaces space with underscore for names
space.fixer <- function(stringvar){
  string <- gsub(" ", "_", stringvar, fixed = TRUE)
  string <- gsub("-", "_", string, fixed = TRUE)
  return(string)
}
dat$region_wdi_adapted <- sapply(dat$region_wdi_adapted, space.fixer)

# make dummies and get names right
# Remove the first column, since this contains the old data
temp.df <- dummy_cols(dat$region_wdi_adapted)[,-1]
names(temp.df) <- c("region_wdi_adapted_Caribbean", "region_wdi_adapted_Central_America", 
                                   "region_wdi_adapted_Central_Asia", "region_wdi_adapted_Eastern_Africa", 
                                   "region_wdi_adapted_Eastern_Asia", "region_wdi_adapted_Eastern_Europe", 
                                   "region_wdi_adapted_Middle_Africa", "region_wdi_adapted_NA", 
                                   "region_wdi_adapted_Northern_Africa", "region_wdi_adapted_Northern_America", 
                                   "region_wdi_adapted_Northern_Europe", "region_wdi_adapted_Oceania", 
                                   "region_wdi_adapted_South_America", "region_wdi_adapted_South_Eastern_Asia", 
                                   "region_wdi_adapted_Southern_Africa", "region_wdi_adapted_Southern_Asia", 
                                   "region_wdi_adapted_Southern_Europe", "region_wdi_adapted_Western_Africa", 
                                   "region_wdi_adapted_Western_Asia", "region_wdi_adapted_Western_Europe" 
)
# add
dat <- cbind(dat, temp.df)


# Make a more coarse distinction between regions
dat$region_coarsened <- car::recode(dat$region_wdi_adapted,
                                      "'South_America'='Latin_America';
                                      'Central_America'='Latin_America';
                                      'Western_Europe'='Europe';
                                      'Northern_Europe'='Europe';
                                      'Southern_Europe'='Europe';
                                      'Eastern_Europe'='Europe';
                                      'Northern_Africa'='Africa';
                                      'Southern_Africa'='Africa';
                                      'Western_Africa'='Africa';
                                      'Eastern_Africa'='Africa';
                                      'Middle_Africa'='Africa';
                                      'South_Eastern_Asia'='Asia';
                                      'Southern_Asia'='Asia';
                                      'Central_Asia'='Asia';
                                      'Western_Asia'='Asia';
                                      'Eastern_Asia'='Asia'")




# Generate the variable on the depth of cooperation
dat$cooperation.depth <- dat$treaty_name
# Specific 
# Order: from small to large to catch the names in good order
dat$cooperation.depth[grepl('Amendment', dat$cooperation.depth)] <- 'specific'
dat$cooperation.depth[grepl('Annex', dat$cooperation.depth)] <- 'specific'
dat$cooperation.depth[grepl('United Nations Regulation', dat$cooperation.depth)] <- 'specific'
dat$cooperation.depth[grepl('Articles', dat$cooperation.depth)] <- 'specific'
dat$cooperation.depth[grepl('Rule', dat$cooperation.depth)] <- 'specific'
# General 
dat$cooperation.depth[grepl('Terms of Reference', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Protocol', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Agreement', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Charter', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Constitution', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Convention', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Covenant', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('General Act', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Statutes', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Statute', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Treaty', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Declaration', dat$cooperation.depth)] <- 'general'
dat$cooperation.depth[grepl('Memorandum', dat$cooperation.depth)] <- 'general'



# Redefining succession to signature events 
# We define an own action category for this (treaty_action_category = 7), 
# which allows us to not consider these actions in the analysis. 
# In our approach we thus follow "Principles of Public International Law"
# ``Within the existing possibilities of inheritance of treaties there is 
# considerable practice to the effect that a new state can inherit the legal 
# consequences of a ratification by a predecessor of a treaty which is not yet 
# in force. But it is doubtful if a new state can inherit the consequences of 
# signature of a treaty which is subject to ratification.''
dat$treaty_action_category[dat$treaty_action_string_code == 48] <- 7


# Defines the ratification dummy in the action category 4
# The real 'used' ratification dummy is defined below as dat$rat_misc_dummy
# In here for legacy reasons
dat$rat_dummy <- (!is.na(dat$action_date) & dat$treaty_action_category ==4)





# ==============================================================================
# 2 Generate Data at Treaty Level ---------------------------------------------
cat('Working on data at the treaty level \n')

# N.B.: The ratification dummies here 'only' capture whether there is an 
# active ratification happening, not any 'ratification like' event.

# Calculating a number of variables at the treaty level
# defining
p5 <- c('United States of America', 'France', 'China', 
  'United Kingdom of Great Britain and Northern Ireland', 'Russian Federation')
treaty_code <- unique(dat$treaty_code)
countries.p.treaty <- rep(0, length(treaty_code))
Russia.in.treaty <- China.in.treaty <- rep(NA, length(treaty_code))
France.in.treaty <- UK.in.treaty <- US.in.treaty <-  rep(NA, length(treaty_code))
Russia.rat.treaty <- China.rat.treaty <- rep(NA, length(treaty_code))
France.rat.treaty <- UK.rat.treaty <- US.rat.treaty <-  rep(NA, length(treaty_code))
any.p5.in.treaty <- rep(NA, length(treaty_code))
any.rat <- nr.signatory.states <- rep(NA, length(treaty_code))





for (i in 1:length(treaty_code)){
  dat.temp <- dat[dat$treaty_code == treaty_code[i],]
  # Having an entry for treaty action category is not enough, 
  # there also needs to be a name associated with it.
  # action_date must not be NA
  countries.p.treaty[i] <- length(unique(dat.temp$country_name))
  # who signed?
  nr.signatory.states[i] <- suppressWarnings(
    table((dat.temp$treaty_action_category==5 & !is.na(dat.temp$action_date)))['TRUE']
    # table((dat.temp$treaty_action_category==5))['TRUE'] # <- old version
  )
  # Any P5 country in the treaty?
  if (sum(p5 %in% dat.temp$country_name) > 0) {
    any.p5.in.treaty[i] <- 1
  } else if (sum(p5 %in% dat.temp$country_name) == 0) {
    any.p5.in.treaty[i] <- 0  
  }
  # US in the treaty?
  if ('United States of America' %in% dat.temp$country_name) {
    US.in.treaty[i] <- 1
    # Check for successful ratification
    if (4 %in% dat.temp$treaty_action_category){
      dat.temp2 <- subset(dat.temp, dat.temp$treaty_action_category == 4)  
      if('United States of America' %in% dat.temp2$country_name){
        US.rat.treaty[i] <- unique(dat.temp2$rat_dummy[dat.temp2$country_name == 'United States of America'])
      }
    }
  } else if (!('United States of America' %in% dat.temp$country_name)) {
    US.in.treaty[i] <- 0  
  }
  # Russia in the treaty?
  if ('Russian Federation' %in% dat.temp$country_name) {
    Russia.in.treaty[i] <- 1
    # Check for successful ratification
    if (4 %in% dat.temp$treaty_action_category){
      dat.temp2 <- subset(dat.temp, dat.temp$treaty_action_category == 4)  
      if('Russian Federation' %in% dat.temp2$country_name){
        Russia.rat.treaty[i] <- unique(dat.temp2$rat_dummy[dat.temp2$country_name == 'Russian Federation'])
      } 
    }
  } else if (!('Russian Federation' %in% dat.temp$country_name)) {
    Russia.in.treaty[i] <- 0  
  }
  # China in the treaty?
  if ('China' %in% dat.temp$country_name) {
    China.in.treaty[i] <- 1
    # Check for successful ratification
    if (4 %in% dat.temp$treaty_action_category){
      dat.temp2 <- subset(dat.temp, dat.temp$treaty_action_category == 4)  
      if('China' %in% dat.temp2$country_name){
        China.rat.treaty[i] <- unique(dat.temp2$rat_dummy[dat.temp2$country_name == 'China'])
      } 
    }
  } else if (!('China' %in% dat.temp$country_name)) {
    China.in.treaty[i] <- 0  
  }
  # France in the treaty?
  if ('France' %in% dat.temp$country_name) {
    France.in.treaty[i] <- 1
    # Check for successful ratification
    if (4 %in% dat.temp$treaty_action_category){
      dat.temp2 <- subset(dat.temp, dat.temp$treaty_action_category == 4)  
      if('France' %in% dat.temp2$country_name){
        France.rat.treaty[i] <- unique(dat.temp2$rat_dummy[dat.temp2$country_name == 'France'])
      }
    }
  } else if (!('France' %in% dat.temp$country_name)) {
    France.in.treaty[i] <- 0  
  }
  # UK in the treaty?
  if ('United Kingdom of Great Britain and Northern Ireland' %in% dat.temp$country_name) {
    UK.in.treaty[i] <- 1
    # Check for successful ratification
    if (4 %in% dat.temp$treaty_action_category){
      dat.temp2 <- subset(dat.temp, dat.temp$treaty_action_category == 4)  
      if('United Kingdom of Great Britain and Northern Ireland' %in% dat.temp2$country_name){
        UK.rat.treaty[i] <- unique(dat.temp2$rat_dummy[dat.temp2$country_name == 'United Kingdom of Great Britain and Northern Ireland'])
      }
    }
  } else if (!('United Kingdom of Great Britain and Northern Ireland' %in% dat.temp$country_name)) {
    UK.in.treaty[i] <- 0  
  }
  # did ratification happen?
  if (4 %in% dat.temp$treaty_action_category) {
    any.rat[i] <- 1
  } else if (!(4 %in% dat.temp$treaty_action_category)) {
    any.rat[i] <- 0
  }
}



dat.treaty.1 <- data.frame(treaty_code, countries.p.treaty, US.in.treaty, 
                           Russia.in.treaty, China.in.treaty, France.in.treaty,
                           UK.in.treaty, any.p5.in.treaty, US.rat.treaty, 
                           Russia.rat.treaty, China.rat.treaty, France.rat.treaty,
                           UK.rat.treaty, nr.signatory.states)

dat.treaty.temp <- data.frame(dat['treaty_code'],
                              dat['mtdsg'],
                              dat['chapter'],
                              dat['chapter_title'],
                              dat['section'],
                              dat['treaty_name'],
                              dat['treaty_year'],
                              dat['location'],
                              dat['adoption_date'],
                              dat['iatp'],
                              dat['c_by_t_sample'],
                              dat['threshold_1_number'],
                              dat['threshold_1_dummy'],
                              dat['threshold_only_1_dummy'],
                              dat['threshold_2_time'],
                              dat['threshold_2_dummy'],
                              dat['threshold_only_2_dummy'],
                              dat['threshold_3_qualified'],
                              dat['threshold_3_qualified_expl'],
                              dat['threshold_only_3_dummy'],
                              dat['threshold_combo_1_and_3'],
                              dat['threshold_combo_1_and_2'],
                              dat['threshold_combo_2_and_3'],
                              dat['threshold_only_combo_1_and_3'],
                              dat['threshold_only_combo_1_and_2'],
                              dat['threshold_only_combo_2_and_3'],
                              dat['threshold_combo_all'],
                              dat['threshold_any_1_or_3'],
                              dat['entry_force_threshold_binary'],
                              dat['treaty_year'],
                              dat['entry_force_binary'],
                              dat['entry_force_date'],
                              dat['public_good'],
                              dat['common_good'],
                              dat['post_cold_war'],
                              dat['cooperation.depth'])

# reduce the data set 
dat.treaty.2 <- unique(dat.treaty.temp)
# assert that the dims are OK
if (dim(dat.treaty.2)[1] != dim(dat.treaty.1)[1]) stop(
  'Dimensions in the treaty level data match are not OK')

# merging the data sets
dat.treaty <- merge(dat.treaty.1, dat.treaty.2, by = 'treaty_code')

# calculate successful ratification and nr of expressions to be bound
dat.treaty$nr.accept.to.be.bound <- dat.treaty$nr.countries.rat.success <- rep(NA, nrow(dat.treaty))
for (i in 1:length(dat.treaty$treaty_code)){
  # i<-1
  dat.temp <- subset(dat, dat$treaty_code == dat.treaty$treaty_code[i])
  # ratifications
  dat.treaty$nr.countries.rat.success[i] <- sum(dat.temp$rat_dummy)
  # expressions to be bound
  coun.ac.1 <- dat.temp$country_name[dat.temp$treaty_action_category==1 & !is.na(dat.temp$action_date)]
  coun.ac.2 <- dat.temp$country_name[dat.temp$treaty_action_category==2 & !is.na(dat.temp$action_date)]
  coun.ac.3 <- dat.temp$country_name[dat.temp$treaty_action_category==3 & !is.na(dat.temp$action_date)]
  # coun.ac.4.nd <- dat.temp$country_name[dat.temp$treaty_action_category==4]
  coun.ac.4 <- dat.temp$country_name[dat.temp$treaty_action_category==4 & !is.na(dat.temp$action_date)]
  # if (length(coun.ac.1to3) == 0){
  #   coun.ac <- coun.ac.4
  # }
  # if (length(coun.ac.1to3) > 0){
  #   coun.ac.1to3 <- unique(c(coun.ac.1, coun.ac.2, coun.ac.3)[!is.na(c(coun.ac.1, coun.ac.2, coun.ac.3))])
  #   # coun.ac <- unique(coun.ac[!is.na(coun.ac)])
  #   coun.ac <- intersect(coun.ac.4.nd, coun.ac.1to3)  
  # }
  coun.ac <- unique(c(coun.ac.1, coun.ac.2, coun.ac.3, coun.ac.4))
  dat.treaty$nr.accept.to.be.bound[i] <- length(coun.ac)
}


# calculate whether threshold is active for numerical thresholds
# (later substituted by simply taking a look at the entry_into_force variable)
dat.treaty$passed_threshold_1_number <- (dat.treaty$nr.countries.rat.success >= dat.treaty$threshold_1_number & 
                                           dat.treaty$threshold_1_number > 0)

# Any threshold
dat.treaty$anythreshold <- FALSE
dat.treaty$anythreshold[
  dat.treaty$threshold_only_1_dummy == TRUE |
    dat.treaty$threshold_2_dummy == TRUE |
    dat.treaty$threshold_only_3_dummy == TRUE |
    dat.treaty$threshold_combo_1_and_3 == TRUE
] <- TRUE


boilerplate.thresholds <- as.numeric(names(
  table(dat.treaty$threshold_1_number)[table(dat.treaty$threshold_1_number)>15]))

boilerplate.thresholds <- boilerplate.thresholds[-1]

# Boilerplate Dummmy New
dat.treaty$threshold_1_boilerplate_dummy <- NA
dat.treaty$threshold_1_boilerplate_dummy[dat.treaty$threshold_1_number %in% boilerplate.thresholds] <- TRUE
dat.treaty$threshold_1_boilerplate_dummy[!(dat.treaty$threshold_1_number %in% boilerplate.thresholds)] <- FALSE


# correct missing dates where you can be certain things are right censored
dat.treaty$entry_force_date_corrected <- dat.treaty$entry_force_date
dat.treaty$entry_force_date_corrected[
  is.na(dat.treaty$entry_force_date) & dat.treaty$entry_force_binary == 0] <- as.Date("2019-06-01")

dat.treaty$threshold_reached_date <- dat.treaty$entry_force_date_corrected - dat.treaty$threshold_2_time
dat.treaty$threshold_1_share.old <- dat.treaty$threshold_1_number/dat.treaty$nr.signatory.states
dat.treaty$threshold_1_share <- dat.treaty$threshold_1_number/dat.treaty$countries.p.treaty

# Generate a different variable to avoid string error below
dat.treaty$cooperation.deep <- rep(NA, length(dat.treaty$cooperation.depth))
dat.treaty$cooperation.deep[dat.treaty$cooperation.depth == "framework"] <- 1
dat.treaty$cooperation.deep[dat.treaty$cooperation.depth == "protocol_and_amendment"] <- 0


# recode chapters with only one treaty
dat.treaty$chapter_title_consolidated <- car::recode(
  dat.treaty$chapter_title, "
  'Freedom of Information' = 'Miscellaneous';
  'Maintenance Obligations' = 'Miscellaneous';
  'Pacific Settlement of International Disputes' = 'Miscellaneous';
  'Economic Statistics' = 'Miscellaneous';
  'Fiscal Matters' = 'Miscellaneous';
  'Outer Space' = 'Miscellaneous';
  'Charter of the United Nations and Statute of the International Court of Justice' = 'UN_charter';
  'Educational and Cultural Matters' = 'edu_and_culture';
  'Commercial Arbitration' = 'commercial_arbitration';
  'Human Rights' = 'human_rights';
  'International Trade and Development' = 'int_trade_and_development';
  'Law of the Sea' = 'law_of_the_sea';
  'Law of Treaties' = 'law_of_treaties';
  'Narcotic Drugs and Psychotropic Substances' = 'narcotics';
  'Obscene Publications' = 'obscene_publications';
  'Penal Matters' = 'penal_matters';
  'Privileges and Immunities, Diplomatic and Consular Relations, etc' = 'diplomatic_priviliges';
  'Refugees and Stateless Persons' = 'refugees';
  'Status of Women' = 'women';
  'Traffic in Persons' = 'traffic_in_persons';
  'Transport and Communications' = 'transport_and_communications'
  "
)

# A consolidated variable for the thresholds
dat.treaty$which_threshold <- NA
dat.treaty$which_threshold[dat.treaty$threshold_only_1_dummy == TRUE] <- 'only_1'
dat.treaty$which_threshold[dat.treaty$threshold_only_2_dummy == TRUE] <- 'only_2'
dat.treaty$which_threshold[dat.treaty$threshold_only_3_dummy == TRUE] <- 'only_3'
dat.treaty$which_threshold[dat.treaty$threshold_only_combo_1_and_3 == TRUE] <- 'only_1_and_3'
dat.treaty$which_threshold[dat.treaty$threshold_only_combo_1_and_2 == TRUE] <- 'only_1_and_2'
dat.treaty$which_threshold[dat.treaty$threshold_only_combo_2_and_3 == TRUE] <- 'only_2_and_3'
dat.treaty$which_threshold[dat.treaty$threshold_combo_all == TRUE] <- 'threshold_combo_all'

# Make dummies
chapter_dummies <- dummy_cols(dat.treaty$chapter_title_consolidated)
# adjust names
names(chapter_dummies) <- gsub('.data', 'chapter_d', names(chapter_dummies))
# merge data 
dat.treaty <- cbind(dat.treaty, chapter_dummies)





# Add idealpoints and calculate signatories' alignment
dat.ip <- read.csv('dat_IdealpointestimatesAll_Jun2024.csv')
dat.ip$X <- NULL
# Year according to codebook 
dat.ip$year <- dat.ip$session + 1945

# Match with the original data 
# Based on a lose definition of signing states
mask.signing <- dat$treaty_action_category == 5
dat.first <- subset(dat, mask.signing)

# for the subset: add the idealpoint from that year for that country  
dat.ip.reduced <- dat.ip[, c("year", "iso3c", "IdealPointAll")]
dat.first <- merge(dat.first, dat.ip.reduced,
                   by.x = c("treaty_year", "country_iso3c"),
                   by.y = c("year", "iso3c"),
                   all.x = TRUE)

# for each group of observations in the variable dat.first$treaty_code: 
# calculate the variance and span of the idealpoints
# Define a custom function to compute variance and span
group_stats <- function(x) {
  x <- x[!is.na(x)]  # Remove NAs
  if (length(x) == 0) {
    return(c(variance = NA, span = NA))
  } else {
    return(c(variance = var(x), span = max(x) - min(x)))
  }
}

# Apply the function grouped by treaty_code
result <- aggregate(dat.first$IdealPointAll,
                    by = list(treaty_code = dat.first$treaty_code),
                    FUN = group_stats)

# Turn the matrix column into separate columns
result.dat.ip <- data.frame(treaty_code = result$treaty_code,
                        idealpoint.variance = result$x[, "variance"],
                        idealpoint.span = result$x[, "span"])

# Add the vars to the treaty level data 
dat.treaty <- merge(dat.treaty, result.dat.ip,
                    by = "treaty_code",
                    all.x = TRUE)
dat.treaty$idealpoint.sd <- sqrt(dat.treaty$idealpoint.variance)



# Add variable on conf reg mechanisms
# load the data 
# merge the data 
load(file = 'dat_mprdsm.rdata')
# recode
mpr.dsm.dat$any_dsm_gpt4o <- mpr.dsm.dat$keep
mpr.dsm.dat$keep <- NULL
# merge
dat.treaty <- merge(dat.treaty, mpr.dsm.dat,
    by.x = "treaty_code",
    by.y = "treaty_name",
    all.x = TRUE)



# define the M/S Sample
dat.treaty.c_by_t <- subset(dat.treaty, dat.treaty$c_by_t_sample == 1)
# define Sample with numerical threshold shares lower 100%
dat.treaty.share <- subset(dat.treaty, 
                           (dat.treaty$threshold_1_dummy==TRUE & 
                              dat.treaty$threshold_1_share <= 1))



dat.treaty$public_or_common_good <- pmax(dat.treaty$common_good, dat.treaty$public_good)





# -- 3 Country level data but with all kind of thresholds and all kinds ----
if(runall){
  cat('Running the loop to calculate rat_misc_dummy. The runtime can be 10 mins.')
  cat('\nGet a coffee while waiting:\n')
  cat(" ( (\n  ) )\nc[_]\n")
  # this is the ratification dummy for the expanded ratification understanding. 
  # See Von Stein "Exploring the Universe of UN Human Rights Agreements", JCR 2019
  # Here: as ratification (1,2,3,4,6)
  dat.rat.misc <- subset(dat, subset = (
    dat$treaty_action_category == 1 | dat$treaty_action_category == 2 |
      dat$treaty_action_category ==3 | dat$treaty_action_category ==4 |
      dat$treaty_action_category ==6))
  # identify multiple entries. 
  dat.rat.misc$countryXtreaty <- paste(dat.rat.misc$country_name, 
                                       dat.rat.misc$treaty_code, sep = '_')
  # Names of all double entries
  names.mult.entries <- names(table(dat.rat.misc$countryXtreaty)[table(dat.rat.misc$countryXtreaty)!=1])
  # Algorithm
  # for each treaty X country
  # select all the action categories
  # check which date is not NA. 
  #   Check whether double entry 
  #   Select that line as the relevant one 
  # if all are NA, then chose the ratification 4 as relevant one (check whether it is there, else error)
  # loop over all elements in names.mult.entries
  # initiate the data frame
  dat.rat.misc.wo.doubles <- dat.rat.misc[1,]
  dat.rat.misc.wo.doubles$rat_misc_dummy <- NA
  max.iter <- length(names.mult.entries)
  # run the loop
  for (i in seq(1,max.iter)){
    if (i%%500 == 0) cat(paste('at case',i,'out of', max.iter,'at the moment \n'))
    # select all the cases in the countryXtreaty
    dat.mult.temp <- subset(dat.rat.misc, dat.rat.misc$countryXtreaty ==names.mult.entries[i])
    # only one entry or no entry in the action date?
    if (sum(!is.na(dat.mult.temp$action_date)) == 0){
      # this is the ongoing ratification
      if(sum(dat.mult.temp$treaty_action_category == 4) > 0){
        dat.vec.out <- dat.mult.temp[dat.mult.temp$treaty_action_category == 4,]
        # this means that we only have right censored cases of ratification, but 
        # no other action category
        dat.vec.out$rat_misc_dummy <- 0
      } 
    } 
    if (sum(!is.na(dat.mult.temp$action_date)) == 1){
      # if there is only one non-missing, use that action date
      dat.vec.out <- dat.mult.temp[!is.na(dat.mult.temp$action_date),]
      # create the ratification dummy
      dat.vec.out$rat_misc_dummy <- 1
    } 
    dat.rat.misc.wo.doubles <- rbind(dat.rat.misc.wo.doubles, dat.vec.out)
  }
  # after the loop: subtract the line for data frame initiation
  dat.rat.misc.wo.doubles <- dat.rat.misc.wo.doubles[-1,]
  
  # Those doubles are all completely the same. Filter
  dat.rat.misc.wo.doubles.unique <- unique(dat.rat.misc.wo.doubles)
  
  # Now: add the values with only one entry
  # identify
  names.one.entry <- names(table(dat.rat.misc$countryXtreaty)[table(dat.rat.misc$countryXtreaty)==1])
  # make mask
  mask.one.entry <- dat.rat.misc$countryXtreaty %in% names.one.entry
  # subset
  dat.rat.misc.ones <- subset(dat.rat.misc, mask.one.entry)
  # Generate ratification dummy
  dat.rat.misc.ones$rat_misc_dummy <- 0
  dat.rat.misc.ones$rat_misc_dummy[!is.na(dat.rat.misc.ones$action_date)] <- 1
  # Join the two data strands: The ones
  dat.rat.misc.wo.doubles.unique.all <- rbind(dat.rat.misc.ones, dat.rat.misc.wo.doubles.unique)
  # Operations for which we need treaties separately: 
  # find the respective functions in helpers_data.r
  # all treaty codes
  treaty.codes <- unique(dat.rat.misc.wo.doubles.unique.all$treaty_code)
  # Normal Enrichment -----
  # This adds variables to the rawer data 
  dat.rat.misc.all.treaties <- list()
  for (i in 1:length(treaty.codes)){
    dat.rat.one.treaty <- data.enricher.p.treaty(dat.rat.misc.wo.doubles.unique.all, treaty.codes[i])
    dat.rat.misc.all.treaties[[i]] <- dat.rat.one.treaty
  }
  # Collapse all treaties into data frame: dat.df.rat.misc.all.treaties ---
  dat.df.rat.misc.all.treaties <- data.frame(
    matrix(ncol = ncol(dat.rat.one.treaty), nrow = 0))
  names(dat.rat.misc.all.treaties) <- names(dat.rat.one.treaty)
  for (i in 1:length(treaty.codes)){
    # merge empty one with list from above
    dat.df.rat.misc.all.treaties <- rbind(dat.df.rat.misc.all.treaties,
                                          dat.rat.misc.all.treaties[[i]])
  }
  # save that data!
  save.data.from.run <- TRUE
  if (save.data.from.run){
    save(dat.df.rat.misc.all.treaties, file = "dat_wo_doubles.RData")
  }
}



cat('\n --- data management out --- \n')
























