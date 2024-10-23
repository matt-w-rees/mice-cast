# clean and fix errors in monitoring project trap data

clean_monitoring_data_traps <- function(data){

  
  # FIX ERRORS --------------------------------------------------------------
  # specify date format
  data$date <- ymd(data$capturedate)
  
  ## DATE ERRORS
  # these 3 rows must just be an error - remove from dataset
  data <- filter(data, !(is.na(date) & is.na(session)))
  # the remaining rows have a session number - remove for now but come back to - might be salvageable?
  data <- filter(data, !(is.na(date)))
  # this is obvs a typo - fix it:
  data$date <- if_else(data$datasitenameold == "JWAF2Scrub" & data$date == "2014-03-14",  ymd("2014-03-24"), data$date)
  # delete this row - appears to be just a repeat with the wrong session ID
  data <- filter(data, !(datasitenameold == "29 & 30/30" & date == "2013-05-06" & session == "6"))
  # looks like an incorrect session ID - fix to match dates
  data$session <- if_else(data$sitename == "46 & 47" & data$date %within% interval(ymd("2013-05-05"), ymd("2013-05-07")), "2", as.character(data$session))
  
  
  ## SITE SAME ERRORS
  data$site <- dplyr::if_else(data$site == "Trifel1", "Trifel", data$site)
  # make same as ecology database
  data$areaname <- ifelse(data$areaname == "Central West", "Central West NSW", data$areaname)
  
  
  ## COORDINATE ERRORS
  data$easting <- ifelse(data$datasitenameold == "TriEGrid", 151.4594, data$easting)
  data$northing <- ifelse(data$datasitenameold == "TriEGrid", -27.73499, data$northing)
  
  
  # SUBSET AND RENAME COLUMNS --------------------------------------------------
  data_clean <- data %>% 
    transmute(      
      # site data
      north_south = region,
      region = areaname,
      site = sitename,
      subsite = datasitenameold,
      longitude = easting, 
      latitude = northing,
      # session data
      session, # NOT ACCURATE?
      date_start_session = 99,       # placeholder (see below)
      date_end_session = 99,         # placeholder (see below)
      session_length_days = 99,      # placeholder (see below)
      position = "99", #             # placeholder (see below)
      crop_type = ifelse(north_south == "north", tolower(croptypeold), tolower(cropname)),  
      crop_stage = tolower(cropstageold), 
      trap_type = traptype, 
      # night data
      date, 
      survey_night = 99, # placeholder
      traps_set = trapsset, 
      phantoms = ifelse(is.na(phantoms), 0, phantoms), # false triggers - NA's must mean 0
      traps_night = traps_set - phantoms, 
      mice_night = totalcaptures, 
      # individual data
      trap_location_x = traplocationx, 
      trap_location_y = traplocationy, 
      individual_id = pittag,
      class, 
      ear_mark = earmark,
      fate, 
      sex,
      age,
      weight_g = weight,
      length_mm = length,
      vagina,
      teats = teat,
      pregnant,
      uterus, 
      uterus_scars = uterus.scars,
      embryos,
      embryo_length = embryo.length,
      testis,
      breeding_status = if_else(pregnant == "yes" | vagina == "perforate_small" | vagina == "perforate_large" | teats == 'present_large_fur_not_at_base', "breeding", "no_sign_breeding"))
  
  
  # SPELL OUT NUMBERED CATEGORICAL VARIABLES --------------------------------
  # trap type
  data_clean$trap_type <- if_else(data_clean$trap_type == 1, "longworth", as.character(data_clean$trap_type))
  data_clean$trap_type <- if_else(data_clean$trap_type == 2, "elliott", data_clean$trap_type)
  data_clean$trap_type <- if_else(data_clean$trap_type == 3, "snapback", data_clean$trap_type)
  
  # class
  # 1 - first capture; 2 -recapture within trip; 3 - recapture between trips (treat them the same as first capture)
  data_clean$class <- if_else(data_clean$class == 1, "first_capture", as.character(data_clean$class))
  data_clean$class <- if_else(data_clean$class == "2", "recapture_within_trip", data_clean$class)
  data_clean$class <- if_else(data_clean$class == "3", "recapture_bw_trips", data_clean$class)
  data_clean$class <- if_else(data_clean$class == "4", "recapture_tag_lost", data_clean$class)
  
  # fate
  # 1;released; 2;died; 3;escaped no mark; 4;to lab
  data_clean$fate <- if_else(data_clean$fate == 1, "released", as.character(data_clean$fate))
  data_clean$fate <- if_else(data_clean$fate == "2", "dead", data_clean$fate)
  data_clean$fate <- if_else(data_clean$fate == "3", "no_mark", data_clean$fate)
  data_clean$fate <- if_else(data_clean$fate == "4", "dead_to_lab", data_clean$fate)
  
  # sex: 1, male; 2, female (make everything else NA)
  data_clean$sex <- ifelse(data_clean$sex %in% c("3", "0", "12"), NA, as.character(data_clean$sex))
  data_clean$sex <- if_else(data_clean$sex == "1", "male", data_clean$sex)
  data_clean$sex <- if_else(data_clean$sex == "2", "female", data_clean$sex)
  
  # vagina status
  data_clean$vagina <- ifelse(data_clean$vagina == 0,   NA,   as.character(data_clean$vagina))
  data_clean$vagina <- ifelse(data_clean$vagina == 1,   "closed_membrane", data_clean$vagina)
  data_clean$vagina <- ifelse(data_clean$vagina == "2", "closed",          data_clean$vagina)
  data_clean$vagina <- ifelse(data_clean$vagina == "3", "perforate_small", data_clean$vagina)
  data_clean$vagina <- ifelse(data_clean$vagina == "4", "perforate_large", data_clean$vagina)
  
  # teats
  data_clean$teats <- ifelse(data_clean$teats == 0,   NA,                 as.character(data_clean$teats))
  data_clean$teats <- ifelse(data_clean$teats == 1,   "not_visible",                   data_clean$teats)
  data_clean$teats <- ifelse(data_clean$teats == "2", "present_fur_at_base",           data_clean$teats)
  data_clean$teats <- ifelse(data_clean$teats == "3", "present_large_fur_not_at_base", data_clean$teats)
  
  # pregnant - P pregnant by palpation: 1 = no   2 = yes
  data_clean$pregnant <- ifelse(data_clean$pregnant == 0,   NA, as.character(data_clean$pregnant))
  data_clean$pregnant <- ifelse(data_clean$pregnant == 1,   "no",            data_clean$pregnant)
  data_clean$pregnant <- ifelse(data_clean$pregnant == "2", "yes",           data_clean$pregnant)
  
  

  # ADD SUBSITE DETAILS --------------------------------------------------------
  # position = crop (in the actual crop), fence (fenceline between two crops), scrub (in or on a fenceline adjacent to scrub)
  
  
  # region == "Northern Mallee" 
  data_clean$position <- if_else(data_clean$subsite %in% c("JWC Crop", "JW1StubPad", "JWA TGCrop", "JWB TGCrop", "JW2Crop"), "crop", data_clean$position)
  data_clean$position <- if_else(data_clean$subsite %in% c("JWAF1Crop"), "fence", data_clean$position) 
  data_clean$position <- if_else(data_clean$subsite %in% c("JWAF2Scrub", "JW2Edge"), "scrub", data_clean$position)
  
  # region == "Adelaide Plains" 
  data_clean$position <- if_else(data_clean$subsite %in% c("JLA TG", "JLB TG", "TuckEast", "PLHB", "BTHB TG", "RK Murphy TG"), "crop", data_clean$position)
  data_clean$position <- if_else(data_clean$subsite %in% c("RK Murphy FL", "JLBF2Crop", "TuckEastFL", "BTHB FL"), "fence", data_clean$position) 
  data_clean$position <- if_else(data_clean$subsite %in% c("JLAF1Scrub"), "scrub", data_clean$position)  

  # region == "Central West NSW"
  data_clean$position <- if_else(data_clean$subsite %in% c("GR2 TG1 AB", "GR2 TG2 AB"), "crop", data_clean$position)
  data_clean$position <- if_else(data_clean$subsite %in% c("GR2 FL 1 E-W"), "fence", data_clean$position) 
  data_clean$position <- if_else(data_clean$subsite %in% c("GR2 FL 2 N-S"), "scrub", data_clean$position)  
  
  # region == "Central Downs"
  
  x <- select(data_clean, region, site, subsite, position) %>% unique
  write.csv(x, "monitoring_sites.csv")
  
  
   tmp <- dplyr::filter(data_clean, region == "Downs Central")
   unique(tmp$subsite)
   
  
 
  tmp <- dplyr::filter(data_mon, datasitenameold ==  "JW2Edge")
    
    # mallala
    
    
    # central downs
    
    
    
    data_clean$subsite
    
  # ADD SESSION DETAILS -----------------------------------------------------
  data_clean <- data_clean %>% 
    group_by(site, subsite, session, trap_type) %>%
    mutate(date_start_session = min(date),
           date_end_session = max(date),
           session_length_days = as.integer(date_end_session - date_start_session + 1),
           survey_night = as.integer(date - min(date_start_session) + 1)) %>%
    ungroup() 
  
  # remove 'session' variable as it is rather meaningless - can now denote from date start / end 
  data_clean$session <- NULL
  
  # REDO CAPTURE SUMMARIES -------------------------------------------------------
  # recalculate total captures -- by subsite - for each night -- and add columns for available traps and proportion trap rate
  data_clean <- data_clean %>% 
    group_by(site, subsite, date_start_session, survey_night) %>% 
    mutate(mice_night = ifelse(is.na(class) & is.na(fate) & is.na(weight_g) & is.na(sex) & is.na(uterus) & is.na(uterus_scars) & is.na(embryos) & is.na(embryo_length) &is.na(testis), 0, n()))  %>% 
    ungroup()  
  

  # ARRANGE BY SITE / DATE / SUBSITE ----------------------------------------
  data_clean <- arrange(data_clean, site, date, subsite)
  
  
  
  # SAVE --------------------------------------------------------------------
  write_csv(data_clean, "derived_data/data_clean_monitoring_project_traps.csv")
  
  # FOR TARGETS PACKAGE -----------------------------------------------------
  # return only trapping data
  return(data_clean)

}