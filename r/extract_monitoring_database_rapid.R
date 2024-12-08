# this script:
# (1) extracts tables from access database (two different approaches based on operating system)
# (2) recombines tables to how they should fit together based on access relationships
# (3) stitches together tables from southern (1 table) and northern (2 tables) regions (including so minor cleaning and error fixing)
# (4) writes two csvs seperating active burrow counts and chewcards (but returns single dataframe for targets pipeline)

extract_monitoring_database_rapid <- function(access_monitoring){
  
  
  # EXTRACT DATA FROM MICROSOFT ACCESS --------------------------------------
  
  # for testing
  # specify path to access database
  #access_monitoring <- "raw_data/ms_access/MouseMonitoring.accdb"
  
  
  # check if mac or windows
  x <- Sys.info()
  
  # if windows, use RODBC package (no further installation required)
  if (x[1] == "Windows") {
    
    # define path
    PATH <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", access_monitoring)
    
    # establish connection to access database
    channel <- odbcDriverConnect(PATH)
    
    ## Download tables from database
    # south
    tblDataSiteNameID    <- sqlFetch(channel, "tblDataSiteNameID")
    tblCropTypeID        <- sqlFetch(channel, "tblCropTypeID")
    tblCropStageID       <- sqlFetch(channel, "tblCropStageID")
    tbl1SiteDataSouth    <- sqlFetch(channel, "tbl1SiteDataSouth")
    tbl2SessionSouth     <- sqlFetch(channel, "tbl2SessionSouth")
    tblRapidAssessSouth  <- sqlFetch(channel, "tblRapidAssessSouth")
    # north
    tbl1SiteDataNorth       <- sqlFetch(channel, "tbl1SiteDataNorth")
    tblSessionIDNorth       <- sqlFetch(channel, "tblSessionIDNorth")
    tblCropTypeNorthID      <- sqlFetch(channel, "tblCropTypeNorthID")
    tblCropStageNorthID     <- sqlFetch(channel, "tblCropStageNorthID")
    tblDataSiteNameNorthID  <- sqlFetch(channel, "tblDataSiteNameNorthID")
    tbl2SessionNorth        <- sqlFetch(channel, "tbl2SessionNorth")
    tblRapidAssessNorth     <- sqlFetch(channel, "tblRapidAssessNorth")
    
    # close channel
    close(channel) 
    
    
    # or for mac, use Hmisc (need to install mdbtools on computer first)
  } else {
    
  ## Download tables from database
  # south
  tblDataSiteNameID    <- mdb.get(access_monitoring, "tblDataSiteNameID") %>% remove_all_labels()
  tblCropTypeID        <- mdb.get(access_monitoring, "tblCropTypeID") %>% remove_all_labels()
  tblCropStageID       <- mdb.get(access_monitoring, "tblCropStageID") %>% remove_all_labels()
  tbl1SiteDataSouth    <- mdb.get(access_monitoring, "tbl1SiteDataSouth") %>% remove_all_labels()
  tbl2SessionSouth     <- mdb.get(access_monitoring, "tbl2SessionSouth") %>% remove_all_labels()
  tblRapidAssessSouth  <- mdb.get(access_monitoring, "tblRapidAssessSouth") %>% transform(DateSet = ymd(DateSet), DateRecovered = ymd(DateRecovered)) %>% remove_all_labels()
  # north
  tbl1SiteDataNorth       <- mdb.get(access_monitoring, "tbl1SiteDataNorth") %>% remove_all_labels()
  tblSessionIDNorth       <- mdb.get(access_monitoring, "tblSessionIDNorth")  %>% transform(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>% remove_all_labels()
  tblCropTypeNorthID      <- mdb.get(access_monitoring, "tblCropTypeNorthID") %>% remove_all_labels()
  tblCropStageNorthID     <- mdb.get(access_monitoring, "tblCropStageNorthID") %>% remove_all_labels()
  tblDataSiteNameNorthID  <- mdb.get(access_monitoring, "tblDataSiteNameNorthID") %>% remove_all_labels()
  tbl2SessionNorth        <- mdb.get(access_monitoring, "tbl2SessionNorth") %>% remove_all_labels()
  tblRapidAssessNorth     <- mdb.get(access_monitoring, "tblRapidAssessNorth") %>% transform(DateSet = ymd(DateSet), DateRecovered = ymd(DateRecovered)) %>% remove_all_labels()
  }
  
  
  
  # STITCH TOGETHER TABLES --------------------------------------------------
  
  ## SOUTH
  # renames first lot of tables - so match name of corresponding column in tbl2SessionSouth
  tblDataSiteNameID <- rename(tblDataSiteNameID, DataSiteNameOld = DataSiteName, DataSiteName = DataSiteNameID)
  tblCropTypeID <- rename(tblCropTypeID, CropType = CroptypeID)
  tblCropStageID <- rename(tblCropStageID, CropStageOld = CropStage, CropStage = CropStageID)
  
  # combine live trap base information about site / session
  base_south <- left_join(tbl1SiteDataSouth[,1:7], tbl2SessionSouth[,1:6], by = "SiteDataID") %>%
    left_join(., tblDataSiteNameID[,1:2], by = "DataSiteName") %>%
    left_join(., tblCropTypeID, by = "CropType") %>%
    left_join(., tblCropStageID, by = "CropStage") 
  
  # remove unnecessary ID cols now as they're now joined
  base_south <- select(base_south, !(c(SiteDataID, DataSiteName, CropType, CropStage))) 
  
  # add rapid assessment monitoring data
  DataRASouth <- left_join(base_south, tblRapidAssessSouth[,1:45], by = "SessionID") %>%
    filter(!(is.na(RapidAssessmentID))) # remove rows without rapid assessment data - this must be because these sites were only live trapped
  
  
  ## NORTH
  # renames columns so they match name of corresponding column in tbl2SessionNorth
  tblDataSiteNameNorthID <- rename(tblDataSiteNameNorthID, DataSiteNameNorthOld = DataSiteNameNorth, DataSiteNameNorth = DataSiteNameNorthID)
  tblCropTypeNorthID <- rename(tblCropTypeNorthID,  CropTypeOld = CropType, CropType = CropTypeID)
  tblCropStageNorthID <- rename(tblCropStageNorthID, CropStageOld = CropStage, CropStage = CropStageID)
  
  # combine live trap base information about site / session
  base_north <- 
    left_join(tbl1SiteDataNorth[,1:7], tbl2SessionNorth[1:6], by = "SiteDataID") %>%
    left_join(., tblDataSiteNameNorthID[,1:2], by = "DataSiteNameNorth") %>%
    left_join(., tblCropTypeNorthID, by = "CropType") %>%
    left_join(., tblCropStageNorthID, by = "CropStage") 
  
  # clean base table - remove unnecessary ID cols now as they're now joined
  base_north <- select(base_north, !(c(SiteDataID, DataSiteNameNorth, CropType, CropStage)))
  
  # add rapid assessment monitoring data
  DataRANorth <- left_join(base_north, tblRapidAssessNorth, by = "SessionID", relationship = "many-to-many") %>%
    filter(!(is.na(RapidAssessmentNorthID))) # remove rows without rapid assessment data - this must be because these sites were only live trapped
  
  

  # FIX ERRORS IN THE DATA --------------------------------------------------
  
  # coordinates - # specify correct col based on number (the incorrect easting / northing columns has been used) and also some latitude are missing negative sign (hence why 40 is the cutoff) 
  DataRANorth$longitude <- ifelse(DataRANorth$Easting > 40, DataRANorth$Easting, DataRANorth$Northing)
  DataRANorth$latitude <- ifelse(DataRANorth$Easting < 40, DataRANorth$Easting,  DataRANorth$Northing)
  # make sure all latitudes are negative 
  DataRANorth$latitude <- as.numeric(if_else(!(grepl("^-", DataRANorth$latitude)) & !(is.na(DataRANorth$latitude)), paste0("-", DataRANorth$latitude),  as.character(DataRANorth$latitude)))
  # note this code only works if assigned to new column names (lat, long, overwise rewriting them messes up the 2nd ifelse statement)
  # and add these same new cols for southern data for consistent merge
  DataRASouth$longitude <- DataRASouth$Easting
  DataRASouth$latitude <- DataRASouth$Northing

  # fix date mistakes
  DataRASouth$DateRecovered <- if_else(DataRASouth$RapidAssessmentID == 1269 | DataRASouth$RapidAssessmentID == 1271, ymd("2021-03-10"), ymd(DataRASouth$DateRecovered))
  DataRANorth$DateRecovered <- if_else(DataRANorth$DateRecovered == "2002-06-22", ymd("2022-06-22"), DataRANorth$DateRecovered)
  #DataRA$daterecovered <- if_else(DataRA$RapidAssessmentID == 39136684, ymd("2013-06-22"), ymd(DataRA$daterecovered)) # CHECK don't need?
  
  
  # fix inconsistent column names in south data for active burrow
  DataRASouth <- rename(DataRASouth, ActiveBurrow.225...250 = ActiveBurrow.225..250, ActiveBurrow.325...350 = ActiveBurrow325...350, ActiveBurrow.350...375 = ActiveBurrow.350..375)
  # remove blank column 
  DataRASouth$Total <- NULL 
  
  # remove this suspected duplicate row - same site / date with two rows
  DataRASouth <- filter(DataRASouth, !(DataSiteNameOld == "BWunYP" & DateSet == "2020-02-22" & RapidAssessmentID == 1156))
  

  # COMBINE NORTHERN AND SOUTHERN DATA ---------------------------------------------

  ## MAKE COLUMNS THE SAME FOR NORTH AND SOUTH
  # remove "North" in col names
  names(DataRANorth) <- gsub("North", "", names(DataRANorth))
  # fix northing due to ^
  DataRANorth <- rename(DataRANorth, Northing = ing)
  
  # remove spaces / make lowercase for both datasets
  names(DataRANorth) <- gsub(' ', '', tolower(names(DataRANorth)))
  names(DataRASouth) <- gsub(' ', '', tolower(names(DataRASouth)))
  
  # in north, croptypeold is same as cropname in south 
  DataRANorth <- rename(DataRANorth, cropname = croptypeold) 
  
  # bind together
  DataRA <- bind_rows(DataRANorth, DataRASouth)
  
  # and lets just get of easting and northing now to avoid confusion
  DataRA$easting <- NULL
  DataRA$northing <- NULL
  

  # CLEAN ACTUAL BURROW / CHEWCARD DATA ----------------------------------
  
  # ignore poo plates for now - we just care about active burrow counts and chewcards
  DataRA_clean <- dplyr::select(DataRA, !(contains("pooplate")))

  # remove these rows where effort is unknown
  DataRA_clean <- filter(DataRA_clean, !(grepl("Values 0-25 are the total number of active burrows lumped. Location of active burrows unknown, total distance walked unknown", DataRA_clean$burrowcomments)))
  
  # summarise active burrow counts and effort
  DataRA_clean <- DataRA_clean %>%
    mutate(burrow_effort = (ncol(select(DataRA_clean, contains("activeburrow"))) - rowSums(is.na(select(DataRA_clean, contains("activeburrow"))))) * 25, # metres searched for active burrows (25 m per column, 16 columns = possible 400 m searched, but subtract the possible by the number of NA's):
           burrow_total = rowSums(select(DataRA_clean, contains("activeburrow")), na.rm = T)) %>% # total number of active burrows counted
    select(!(contains(c("activeburrow")))) # remove old columns 
  
  # if effort = 0, make both effort and total are NA
  DataRA_clean$burrow_effort <- ifelse(DataRA_clean$burrow_effort == 0, NA, DataRA_clean$burrow_effort)
  DataRA_clean$burrow_total <- ifelse(is.na(DataRA_clean$burrow_effort), NA, DataRA_clean$burrow_total)
  
  # clean up chewcard column names
  names(DataRA_clean) <- gsub("...eaten", "", names(DataRA_clean))

  

  # REMOVE SURVEYS WITH NO-LITTLE EFFORT ---------------------------------------
  # remove rows with very little effort - not worth modelling
  
  ## Burrows
  # remove rows with less than 100 metres searched for burrow counts
  DataRA_clean <- dplyr::filter(DataRA_clean, !(burrow_effort < 100))
  
  ## Chewcards
  # first count chewcards deployed
  DataRA_clean <- mutate(DataRA_clean, chewcards_deployed = ncol(select(DataRA_clean, contains("chewcard."))) - rowSums(is.na(select(DataRA_clean, contains("chewcard.")))))
  # remove rows with less than 5 chewcards deployed
  DataRA_clean <- filter(DataRA_clean, !(chewcards_deployed < 5))

  
  # RENAME AND SUBSET COLUMNS -----------------------------------------------
  DataRA_clean2 <- transmute(DataRA_clean, 
                              # state,
                              region = areaname,
                              site = sitename,
                              subsite = if_else(is.na(datasitenameold), sitename,  datasitenameold),
                              longitude,
                              latitude, 
                              date_set = dateset, 
                              date_recovered = daterecovered,
                              crop_type = tolower(cropname),
                              crop_stage = tolower(cropstageold),
                              biomass_comments = biomass.comments,
                              burrow_effort, 
                              burrow_total,
                              burrow_comments = burrowcomments) %>%
    bind_cols(., dplyr::select(DataRA_clean, c(matches("chewcard."), chewcard_comments = cardcomments))) %>%
    arrange(date_set, region, site, subsite)  %>%
    unique()

    
  
  # IN-CROP, FENCELINE OR SCRUB? --------------------------------------------
  #unique(DataRA_clean2$crop_type)
  #DataRA_clean2$position == "fence line", "fence", 
  #DataRA_clean2$position == "pasture", ?
    

  
  # SPLIT INTO SEPERATE DATAFRAMES FOR EACH DATA TYPES  ---------------------
  DataRA_clean2_burrows <- dplyr::select(DataRA_clean2, !(matches("chewcard")))
  DataRA_clean2_chewcards <- dplyr::select(DataRA_clean2, !(matches("burrow")))
  
  # save
  write_csv(DataRA_clean2_burrows, "derived_data/data_clean_monitoring_project_rapid_burrows.csv")
  write_csv(DataRA_clean2_chewcards, "derived_data/data_clean_monitoring_project_rapid_chewcards.csv")
  
  
  # RETURN FOR TARGETS --------------------------------------------------------------------
  return(DataRA_clean2)
  
}
