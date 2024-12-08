clean_list_datatypes <- function(data_monitoring_traps_clean, data_monitoring_rapid){

  # load datasets
  #tar_load(data_monitoring_traps_clean)
  #tar_load(data_monitoring_rapid)
  
  # REMOVE DATA WITH MISSING KEY VARIABLES ----------------------------------
  data_monitoring_traps_clean <- filter(data_monitoring_traps_clean, !(is.na(longitude) | is.na(latitude) | is.na(date_start_session) | is.na(date_end_session)))
  data_monitoring_rapid <- filter(data_monitoring_rapid, !(is.na(longitude) | is.na(latitude) | is.na(date_set) | is.na(date_recovered)))
  
  
  # REMOVE DATA NOT CONDUCTED IN CROPS --------------------------------------
  # checked these with steve (southern sites, he wasn't sure about northern)
  data_monitoring_traps_clean_filtered <- dplyr::filter(data_monitoring_traps_clean, !(subsite %in% c("JW1StubFence", "JW2Edge", "JWAF1Crop", "JWAF2Crop", "JLAF1Scrub", "JWAF2Scrub", "JLBF2Crop", "GR2 FL 1 E-W", "GR2 FL 2 N-S", "TuckEastFL", "BTHB FL", "RK Murphy FL")))
  data_monitoring_rapid_filtered <- dplyr::filter(data_monitoring_rapid, !(subsite %in% c("JW1StubFence", "JW2Edge", "JWAF1Crop", "JWAF2Crop", "JLAF1Scrub", "JWAF2Scrub", "JLBF2Crop", "GR2 FL 1 E-W", "GR2 FL 2 N-S", "TuckEastFL", "BTHB FL", "RK Murphy FL")))
  
  # for the rest, base it absence of, 
  data_monitoring_traps_clean_filtered <- dplyr::filter(data_monitoring_traps_clean_filtered, !(is.na(crop_type)))
  data_monitoring_rapid_filtered <- dplyr::filter(data_monitoring_rapid_filtered, !(is.na(crop_type)))         
  # or actual crop_type value
  data_monitoring_traps_clean_filtered <- dplyr::filter(data_monitoring_traps_clean_filtered, !(crop_type %in% c("fence_line", "pasture", "mown road verge (recent)", "unburned/unmown road verge", "unknown")))
  data_monitoring_rapid_filtered <- dplyr::filter(data_monitoring_rapid_filtered, !(crop_type %in% c("fence_line", "fence line", "pasture", "mown road verge (recent)", "unburned/unmown road verge", "unknown")))         
  
  # check that worked
  #unique(data_monitoring_traps_clean_filtered$crop_type)
  #unique(data_monitoring_rapid_filtered$crop_type)
  
  
  # CONDENSE CROP VARIABLES -------------------------------------------------
  
  # function copied from condense_crops and removed habitat_type references
  condense_crops <- function(data){
    
    ## crop type
    data$crop_type <- ifelse(data$crop_type %in% c("cereal unknown", "canary", "millet", "oats", "rye", "ryecorn", "sunflower", "triticale", "barley", "sorghum"), "cereal", 
                             ifelse(grepl("bean", data$crop_type) | grepl("pea", data$crop_type) | data$crop_type %in% c("lentils", "lupins", "peas", "vetch", "corn"), "legume",
                                    ifelse(data$crop_type %in% c("wheat", "grain_crop"), "grain", 
                                           ifelse(grepl("fallow", data$crop_type) | grepl("stubble", data$crop_type) | grepl("plough", data$crop_type), "fallow", 
                                                 ifelse(grepl("pasture", data$crop_type) | grepl("improved", data$crop_type) | data$crop_type == "grassed_paddock" | grepl("verge", data$crop_type) | grepl("fence", data$crop_type), "verge_or_pasture",
                        data$crop_type)))))
    
    # if there are crop types with fewer than x rows, remove
    data <- data %>%
      group_by(crop_type) %>%
      dplyr::filter(n() > 15) %>%
      ungroup()
    
    ## crop stage - also copied but cut out stubble column references
    data$crop_stage <- ifelse(data$crop_stage %in% c("stubble  (heads harvested)", "failed", "mulch (stubble cut/unploughed)", "mulch"), "stubble",
                              ifelse(data$crop_stage %in% c("old (older than harvest maturity)", "old", "ripening/ripe", "in head"), "seeding", 
                                     ifelse(data$crop_stage %in% c("mature (flowers/heads)", "mature"), "flowering",
                                            ifelse(data$crop_stage %in% c("seedling", "young (no flowers/head)", "young"), "tillering", 
                                                   ifelse(data$crop_stage %in% c("n/a", "fallow") | grepl("graze", data$crop_stage) | grepl("road", data$crop_stage) | grepl("farm", data$crop_stage), NA, data$crop_stage)))))
    
    # if there are crop stages with fewer than x rows, remove
    data <- data %>%
      group_by(crop_stage) %>%
      dplyr::filter(n() > 15) %>%
      ungroup()
    
    ## deal with fallow
    # lets put fallow as grain (crop_type), and make fallow a crop stage 
    data$crop_stage <- if_else(data$crop_type == "fallow", "fallow", data$crop_stage)
    data$crop_type <- if_else(data$crop_stage == "fallow", "grain", data$crop_type)
    # actually, lets merge fallow with stubble (still in grain)
    data$crop_stage <- if_else(data$crop_stage == "fallow", "stubble", data$crop_stage)
    
    return(data)
  }
  
  # use the above function to condense crop type variable and filter out rows 
  data_monitoring_traps_clean_filtered <- condense_crops(data_monitoring_traps_clean_filtered)
  data_monitoring_rapid_filtered <- condense_crops(data_monitoring_rapid_filtered)
  # check out the results
  #xtabs(~data_monitoring_traps_clean_filtered$crop_type + data_monitoring_traps_clean_filtered$crop_stage)
  #xtabs(~data_monitoring_rapid_filtered$crop_type + data_monitoring_rapid_filtered$crop_stage)
  
  
  
  # LIVE-TRAP DATA: FUTHER CLEAN -----------------------------------------------------------
  
  # cut out individual mice data -- CURRENTLY LAST 19 COLUMNS (and remove duplicate rows due to the individual mice columns)
  data_monitoring_traps_clean_filtered_night <- dplyr::select(data_monitoring_traps_clean_filtered, 1:(length(data_monitoring_traps_clean_filtered)-20)) %>% 
    unique()
  
  # group elliots and longworths together 
  data_monitoring_traps_clean_filtered_night$trap_type <- if_else(!(data_monitoring_traps_clean_filtered_night$trap_type == "snapback"), "box_trap", data_monitoring_traps_clean_filtered_night$trap_type)

  # remove snapback traps for now
  data_monitoring_traps_clean_filtered_night <- dplyr::filter(data_monitoring_traps_clean_filtered_night, trap_type != "snapback") 
  
  # reformat into wide dataframe with repeat surveys in a session (one row) as columns 
  data_monitoring_traps_clean_filtered_wide <- data_monitoring_traps_clean_filtered_night %>% 
    dplyr::select(!(c(date, north_south, traps_set, phantoms))) %>%  # remove unneccesary columns 
    tidyr::pivot_wider(
      names_sep = "",
      names_from = survey_night, # measured variables
      values_from = c('mice_night', 'traps_night'))
      
  
  # RAPID ASSESSMENT DATA: FUTHER CLEAN -----------------------------------------------------------
  
  # remove WA data 
  data_monitoring_rapid_filtered <- dplyr::filter(data_monitoring_rapid_filtered, !(region %in% c("Albany", "Esperance", "Kwinana West", "Geraldton")))
  
  # for consistency with trapping data
  data_monitoring_rapid_filtered <- rename(data_monitoring_rapid_filtered, date_start_session = date_set, date_end_session = date_recovered)

  # remove comment columns 
  data_monitoring_rapid_filtered <- select(data_monitoring_rapid_filtered, !(matches("comments")))
  
  ## Split into two datasets for each data type
  data_monitoring_rapid_filtered_burrows <- dplyr::select(data_monitoring_rapid_filtered, !(matches("chewcard")))
  data_monitoring_rapid_filtered_chewcards <- dplyr::select(data_monitoring_rapid_filtered, !(matches("burrow")))
  
  # now remove empty rows due to no effort (instances where burrows where searched but no chewcards, vice versa) - THESE ARE NOW REMOVED IN EXTRACTION SCRIPT
  #data_monitoring_rapid_filtered_burrows <- dplyr::filter(data_monitoring_rapid_filtered_burrows, !(is.na(burrow_effort)))
  #data_monitoring_rapid_filtered_chewcards <- filter(data_monitoring_rapid_filtered_chewcards, !(if_all(starts_with("chewcard."), ~ is.na(.))))
  
  # convert chewcards to presence / absence (1 / 0)
  data_monitoring_rapid_filtered_chewcards <- data_monitoring_rapid_filtered_chewcards %>%
    mutate(across(starts_with('chewcard.'), ~ifelse( .x > 0, 1, .x)))
  
  # CLEAN SUBSITE NAMES - mostly because I did this for the traps 
  # MALLEE: rename subsites for consistency - only the first few sessions had JW1StubPad / JW2Crop / JW2Edge - lets just assume naming convention changed / sites weren't moved far -- all the same coordinates and also, only happened for a few sessions so not worth accounting for 
  data_monitoring_rapid_filtered_chewcards$subsite <- if_else(data_monitoring_rapid_filtered_chewcards$subsite == "JW1StubPad", "JWA TGCrop", data_monitoring_rapid_filtered_chewcards$subsite)
  data_monitoring_rapid_filtered_chewcards$subsite <- if_else(data_monitoring_rapid_filtered_chewcards$subsite %in% c("JW2Crop"), "JWB TGCrop", data_monitoring_rapid_filtered_chewcards$subsite)
  data_monitoring_rapid_filtered_chewcards$subsite <- if_else(data_monitoring_rapid_filtered_chewcards$subsite == "JWC Crop" & data_monitoring_rapid_filtered_chewcards$date_start_session < ymd("2015-01-01"), "JWA TGCrop", data_monitoring_rapid_filtered_chewcards$subsite)
  data_monitoring_rapid_filtered_chewcards$subsite <- if_else(data_monitoring_rapid_filtered_chewcards$subsite == "JWC Crop" & data_monitoring_rapid_filtered_chewcards$date_start_session > ymd("2015-01-01"), "JWB TGCrop", data_monitoring_rapid_filtered_chewcards$subsite)
  # and same for burrows
  data_monitoring_rapid_filtered_burrows$subsite <- if_else(data_monitoring_rapid_filtered_burrows$subsite == "JW1StubPad", "JWA TGCrop", data_monitoring_rapid_filtered_burrows$subsite)
  data_monitoring_rapid_filtered_burrows$subsite <- if_else(data_monitoring_rapid_filtered_burrows$subsite %in% c("JW2Crop"), "JWB TGCrop", data_monitoring_rapid_filtered_burrows$subsite)
  data_monitoring_rapid_filtered_burrows$subsite <- if_else(data_monitoring_rapid_filtered_burrows$subsite == "JWC Crop" & data_monitoring_rapid_filtered_burrows$date_start_session < ymd("2015-01-01"), "JWA TGCrop", data_monitoring_rapid_filtered_burrows$subsite)
  data_monitoring_rapid_filtered_burrows$subsite <- if_else(data_monitoring_rapid_filtered_burrows$subsite == "JWC Crop" & data_monitoring_rapid_filtered_burrows$date_start_session > ymd("2015-01-01"), "JWB TGCrop", data_monitoring_rapid_filtered_burrows$subsite)
  
  
  # SAVE DATA TYPES AS LIST AND RETURN -------------------------------------------------
  
  # first arrange by date and site 
  data_monitoring_traps_clean_filtered_wide <- dplyr::arrange(data_monitoring_traps_clean_filtered_wide, date_start_session, region, site, subsite)
  data_monitoring_rapid_filtered_burrows <- dplyr::arrange(data_monitoring_rapid_filtered_burrows, date_start_session, region, site, subsite)
  data_monitoring_rapid_filtered_chewcards <- dplyr::arrange(data_monitoring_rapid_filtered_chewcards, date_start_session, region, site, subsite)
  
  # make sure each row is unique and combine in a list
  data_list <- list(unique(data_monitoring_traps_clean_filtered_wide), unique(data_monitoring_rapid_filtered_burrows), unique(data_monitoring_rapid_filtered_chewcards))
  names(data_list) <- c("traps", "burrows", "chewcards")
  
  # save visualisation plots of missing data 
  ggsave(vis_miss(data_list$traps), filename = "derived_data/data_vis/dataframes/data_traps_descending_date.png")
  ggsave(vis_miss(data_list$burrows), filename = "derived_data/data_vis/dataframes/data_burrows_descending_date.png")
  ggsave(vis_miss(data_list$chewcards), filename = "derived_data/data_vis/dataframes/data_chewcards_descending_date.png")
  
  return(data_list)

}