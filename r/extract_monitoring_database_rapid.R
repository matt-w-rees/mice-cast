extract_monitoring_database_rapid <- function(access_mon){
  
  
  # EXTRACT DATA FROM MICROSOFT ACCESS --------------------------------------
  
  # specify path to access database
  #access_mon <- "raw_data/ms_access/MouseMonitoring.accdb"
  
  
  # check if mac or windows
  x <- Sys.info()
  
  # if windows, use RODBC package (no further installation required)
  if (x[1] == "Windows") {
    
    # define path
    PATH <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", access_mon)
    
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
  tblDataSiteNameID    <- mdb.get(access_mon, "tblDataSiteNameID") %>% remove_all_labels()
  tblCropTypeID        <- mdb.get(access_mon, "tblCropTypeID") %>% remove_all_labels()
  tblCropStageID       <- mdb.get(access_mon, "tblCropStageID") %>% remove_all_labels()
  tbl1SiteDataSouth    <- mdb.get(access_mon, "tbl1SiteDataSouth") %>% remove_all_labels()
  tbl2SessionSouth     <- mdb.get(access_mon, "tbl2SessionSouth") %>% remove_all_labels()
  tblRapidAssessSouth  <- mdb.get(access_mon, "tblRapidAssessSouth") %>% transform(DateSet = ymd(DateSet), DateRecovered = ymd(DateRecovered)) %>% remove_all_labels()
  # north
  tbl1SiteDataNorth       <- mdb.get(access_mon, "tbl1SiteDataNorth") %>% remove_all_labels()
  tblSessionIDNorth       <- mdb.get(access_mon, "tblSessionIDNorth")  %>% transform(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>% remove_all_labels()
  tblCropTypeNorthID      <- mdb.get(access_mon, "tblCropTypeNorthID") %>% remove_all_labels()
  tblCropStageNorthID     <- mdb.get(access_mon, "tblCropStageNorthID") %>% remove_all_labels()
  tblDataSiteNameNorthID  <- mdb.get(access_mon, "tblDataSiteNameNorthID") %>% remove_all_labels()
  tbl2SessionNorth        <- mdb.get(access_mon, "tbl2SessionNorth") %>% remove_all_labels()
  tblRapidAssessNorth     <- mdb.get(access_mon, "tblRapidAssessNorth") %>% transform(DateSet = ymd(DateSet), DateRecovered = ymd(DateRecovered)) %>% remove_all_labels()
  }
  
  
  
  # STITCH TOGETHER TABLES --------------------------------------------------
  
  ## STITCH TABLES TOGETHER - SOUTH
  
  # renames first lot of tables - so match name of corresponding column in tbl2SessionSouth
  tblDataSiteNameID <- rename(tblDataSiteNameID, DataSiteNameOld = DataSiteName, DataSiteName = DataSiteNameID)
  tblCropTypeID <- rename(tblCropTypeID, CropType = CroptypeID)
  tblCropStageID <- rename(tblCropStageID, CropStageOld = CropStage, CropStage = CropStageID)
  
  # join live trap base information about site / session
  base_south <- left_join(tbl1SiteDataSouth[,1:7], tbl2SessionSouth[,1:6], by = "SiteDataID") %>%
    left_join(., tblDataSiteNameID[,1:2], by = "DataSiteName") %>%
    left_join(., tblCropTypeID, by = "CropType") %>%
    left_join(., tblCropStageID, by = "CropStage") 
  
  # remove unnecessary ID cols now as they're now joined
  base_south <- select(base_south, !(c(SiteDataID, DataSiteName, CropType, CropStage))) 
  
  # rapid assessment monitoring data
  DataRASouth <- left_join(base_south, tblRapidAssessSouth[,1:45], by = "SessionID") %>%
    filter(!(is.na(RapidAssessmentID))) # remove rows without rapid assessment data - this must be because these sites were only live trapped
  
  
  ## STITCH TABLES TOGETHER - SOUTH
  
  # renames first lot of tables - so match name of corresponding column in tbl2SessionNorth
  tblDataSiteNameNorthID <- rename(tblDataSiteNameNorthID, DataSiteNameNorthOld = DataSiteNameNorth, DataSiteNameNorth = DataSiteNameNorthID)
  tblCropTypeNorthID <- rename(tblCropTypeNorthID,  CropTypeOld = CropType, CropType = CropTypeID)
  tblCropStageNorthID <- rename(tblCropStageNorthID, CropStageOld = CropStage, CropStage = CropStageID)
  
  # base information about site / session
  base_north <- 
    left_join(tbl1SiteDataNorth[,1:7], tbl2SessionNorth[1:6], by = "SiteDataID") %>%
    left_join(., tblDataSiteNameNorthID[,1:2], by = "DataSiteNameNorth") %>%
    left_join(., tblCropTypeNorthID, by = "CropType") %>%
    left_join(., tblCropStageNorthID, by = "CropStage") 
  
  # clean base table - remove unnecessary ID cols now as they're now joined
  base_north <- select(base_north, !(c(SiteDataID, DataSiteNameNorth, CropType, CropStage)))
  
  # rapid assessment monitoring data
  DataRANorth <- left_join(base_north, tblRapidAssessNorth, by = "SessionID", relationship = "many-to-many") %>%
    filter(!(is.na(RapidAssessmentNorthID))) # remove rows without rapid assessment data - this must be because these sites were only live trapped
  
  
  # COMBINE AND CLEAN RAPID ASSESSMENT DATA ---------------------------------------------
  
  ## CLEAN COLUMNS (MAKE SIMILAR NORTH AND SOUTH)
  # remove "North" in col names
  names(DataRANorth) <- gsub("North", "", names(DataRANorth))
  # fix northing due to ^ and rename CropName to sit with south
  DataRANorth <- rename(DataRANorth, Northing = ing)
  # don't need these
 # DataRASouth <- dplyr::select(DataRASouth, !(c("Total", "Comments")))
  # remove spaces / make lowercase
  names(DataRANorth) <- gsub(' ', '', tolower(names(DataRANorth)))
  names(DataRASouth) <- gsub(' ', '', tolower(names(DataRASouth)))
  
  # distinguish which region (when combined)
  DataRASouth$region <- "south"
  DataRANorth$region <- "north"
  
  # FIX LAT LONG MISTAKES IN THE NORTHERN DATA  # specify correct col based on number
  DataRANorth$longitude <- ifelse(DataRANorth$easting > 40, DataRANorth$easting, DataRANorth$northing)
  DataRANorth$latitude <- ifelse(DataRANorth$easting < 40, DataRANorth$easting,  DataRANorth$northing)
  # make sure all latitudes are negative 
  DataRANorth$latitude <- as.numeric(if_else(!(grepl("^-", DataRANorth$latitude)) & !(is.na(DataRANorth$latitude)), paste0("-", DataRANorth$latitude),  as.character(DataRANorth$latitude)))
  
  # add new cols for southern data for merge
  DataRASouth$longitude <- DataRASouth$easting
  DataRASouth$latitude <- DataRASouth$northing
  
  # make crop type col consistent
  DataRANorth <- rename(DataRANorth, cropname = croptypeold)
  
  # bind together
  DataRA <- bind_rows(DataRANorth, DataRASouth)
  
  # fix date mistakes
  DataRA$daterecovered <- if_else(DataRA$rapidassessmentid == 1269, ymd("2021-03-10"), ymd(DataRA$daterecovered))
  DataRA$daterecovered <- if_else(DataRA$rapidassessmentid == 1271, ymd("2021-03-10"), ymd(DataRA$daterecovered))
  DataRA$daterecovered <- if_else(DataRA$rapidassessmentid == 39136684, ymd("2013-06-22"),ymd(DataRA$daterecovered))
  
  
  ## Summarise efforts and data
  # ignore poo plates for now - just active burrow counts and chewcards
  DataRA <- DataRA %>%
    mutate(burrow_effort = (ncol(select(DataRA, contains("activeburrow"))) - rowSums(is.na(select(DataRA, contains("activeburrow"))))) * 25, # metres searched for active burrows (25 m per column, 16 columns = possible 400 m searched, but subtract the possible by the number of NA's):
           burrow_total = rowSums(select(DataRA, contains("activeburrow")), na.rm = T),                                                      # total number of active burrows counted:
           chewcards_deployed = ncol(select(DataRA, contains("chewcard"))) - rowSums(is.na(select(DataRA, contains("chewcard")))),           # number of chewcards deployed:
           chewcard_present = apply(select(DataRA, contains("chewcard")), 1, function(i) sum(i > 0, na.rm = TRUE)),                          # NOTE returns 0's instead of NA's - fix is below; total number of chewcards with any sign of mice chew: 0 - 10:
           chewcard_sum = apply(select(DataRA, contains("chewcard")), FUN = sum, MARGIN = 1, na.rm = TRUE), 
           chewcard_mean = apply(select(DataRA, contains("chewcard")), FUN = mean, MARGIN = 1, na.rm = TRUE))
  
  # if effort = 0, make it NA
  DataRA$burrow_effort <- ifelse(DataRA$burrow_effort == 0, NA, DataRA$burrow_effort)
  DataRA$chewcards_deployed <- ifelse(DataRA$chewcards_deployed == 0, NA, DataRA$chewcards_deployed)
  
  # if effort = NA, make sure resulting value is also NA (not just 0)
  DataRA$burrow_total <- ifelse(is.na(DataRA$burrow_effort), NA, DataRA$burrow_total)
  DataRA$chewcard_present <- ifelse(is.na(DataRA$chewcards_deployed), NA, DataRA$chewcard_present)
  DataRA$chewcard_sum <- ifelse(is.na(DataRA$chewcards_deployed), NA, DataRA$chewcard_sum)
  DataRA$chewcard_mean <- ifelse(is.na(DataRA$chewcards_deployed), NA, DataRA$chewcard_mean)
  
  
  ## final clean
  dataRA_cleaned <- transmute(DataRA, 
                              region = areaname,
                              site = sitename,
                              subsite = datasitenameold, 
                              longitude,
                              latitude, 
                              date_set = dateset, 
                              date_recovered = daterecovered,
                              crop_type = tolower(cropname),
                              crop_stage = tolower(cropstageold),
                              burrow_effort, 
                              burrow_total, 
                              chewcards_deployed,
                              chewcard_present,
                              chewcard_sum, 
                              chewcard_mean) %>%
    arrange(date_set, region, site, subsite)
  
  
  # SAVE --------------------------------------------------------------------
  write_csv(dataRA_cleaned, "derived_data/data_clean_monitoring_project_rapid.csv")
  
}
