expand_by_site_year_season <- function(data_list){
  
  # load list of data for testing
  #tar_load(data_list_ae)
  #data_list <- data_list_ae
  
# ADD SEASON COLUMN -------------------------------------------------------

  # first, add season information columns
  add_year_season <- function(data){
    
    # specify seasons - used later in mutate function
    seasons <- c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA", "JJA", "JJA", "SON", "SON", "SON", "DJF")
    
    data <- data %>%
      arrange(region, site, subsite, date_start_session) %>%
      mutate(year = year(date_end_session),
             season = factor(seasons[month(date_end_session)], levels = unique(seasons)))

    return(data)
  }
  
  # use the function
  data_list_season <- lapply(data_list, add_year_season)
  
  return(data_list_season)

  # SPLIT LIST --------------------------------------------------------------
  # split into seperate dataframes
  data_traps <- data_list_season$traps
  data_burrows <- data_list_season$burrows
  data_chewcards <- data_list_season$chewcards
  

  # TRAPS: HANDLE REPEAT SURVEYS IN A SEASON --------------------------------------------
  # having same crop type and stage!
  
  # save new df
  data_traps_adj <- data_traps
  
  # fix duplicates
  # this survey was conducted right on edge of season, assume next season as that is missing
  data_traps_adj$season <- if_else(data_traps_adj$subsite == "JWA TGCrop" & data_traps_adj$date_end_session == "2018-08-30", "SON", data_traps_adj$season)
  data_traps_adj$season <- if_else(data_traps_adj$subsite == "JWB TGCrop" & data_traps_adj$date_end_session == "2018-08-30", "SON", data_traps_adj$season)
  # same site was basically done a month early, lets again assume next season as that is missing
  data_traps_adj$season <- if_else(data_traps_adj$subsite == "JWA TGCrop" & data_traps_adj$date_end_session == "2022-08-10", "SON", data_traps_adj$season)
  data_traps_adj$season <- if_else(data_traps_adj$subsite == "JWB TGCrop" & data_traps_adj$date_end_session == "2022-08-10", "SON", data_traps_adj$season)
  
  # flag surveys where multiple conducted within a years season / at a site 
  data_traps_adj |>
    dplyr::group_by(ae_zone, site, subsite, season, year, trap_type) |> 
    mutate(duplicates_season = n()) |> 
    ungroup() |> 
    filter(duplicates_season > 1)
  # none after the Jim Wakefield sites are fixed above. 
  
  # for consistency with rapid assessment data, remove columns
  data_traps_adj <- dplyr::select(data_traps_adj, !(c(date_start_session, date_end_session)))
  
  
  # RAPID ASSESMENT DATA: HANDLE REPEAT SURVEYS IN A SEASON ------------------------------------------------
  # having same crop type and stage!
  
  # many more than traps, so need to automate with a function:
  adjust_seasons <- function(data) {
    
    ## add column identifying duplicate season
    data <- data |>
      dplyr::group_by(ae_zone, site, subsite, crop_type, crop_stage, season, year) |> 
      mutate(duplicate_season = if_else(n() > 1, "yes", "no")) |> 
      ungroup()
    # check out how many 
    print(paste0("before:", table(data$duplicate_season)))
  
    # change surveys conducted on-the-cusp of seasons that have duplicates
    # summer / autumn
    data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-02-15")), ymd(paste0(year(data$date_end_session),"-02-28"))), as.factor("MAM"), data$season)
    data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-03-01")), ymd(paste0(year(data$date_end_session),"-03-15"))), as.factor("DJF"), data$season)
    # autumn / winter
    data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-05-15")), ymd(paste0(year(data$date_end_session),"-05-31"))), as.factor("JJA"), data$season)
    data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-06-01")), ymd(paste0(year(data$date_end_session),"-06-15"))), as.factor("MAM"), data$season)
    # winter / spring
    data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-08-15")), ymd(paste0(year(data$date_end_session),"-08-31"))), as.factor("SON"), data$season)
    data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-09-01")), ymd(paste0(year(data$date_end_session),"-09-15"))), as.factor("JJA"), data$season)
    # spring / summer
    data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-11-15")), ymd(paste0(year(data$date_end_session),"-11-30"))), as.factor("DJF"), data$season)
    data$season <- if_else(data$duplicate_season == "yes" & data$date_end_session %within% interval(ymd(paste0(year(data$date_end_session),"-12-01")), ymd(paste0(year(data$date_end_session),"-12-15"))), as.factor("SON"), data$season)
    
    # calculate dupes again 
    data <- data |>
      dplyr::group_by(ae_zone, site, subsite, crop_type, crop_stage, season, year) |> 
      mutate(duplicate_season = if_else(n() > 1, "yes", "no")) |> 
      ungroup()
    # check out what changed 
    print(paste0("after:", table(data$duplicate_season)))
    
    # remove data and duplicate columns 
    data <- dplyr::select(data, !(c(duplicate_season, date_start_session, date_end_session)))
  
    return(data)
  }
  
  # use the function
  data_burrows_adj <- adjust_seasons(data_burrows)
  data_chewcards_adj <- adjust_seasons(data_chewcards)
  
  
  ## BURROW COUNTS - if there are duplicates, simply add counts and effort together (redo same columns), remove remove date columns as they are no longer relevant and repeat rows
  data_burrows_adj <- data_burrows_adj |>
    group_by(site, subsite, season, year, crop_type, crop_stage) |> 
    mutate(burrow_total = sum(burrow_total),
           burrow_effort = sum(burrow_effort)) |> 
    ungroup() |> 
    unique()
  
  
  ## CHEWCARDS 
  # currently max of two duplicates, and these all have max 10 chewcards deployed. 
  data_chewcards_adj <- data_chewcards_adj |>
    group_by(site, subsite, season, year, crop_type, crop_stage) |> 
    mutate(duplicates_season = n(),
           ID = row_number()) |> 
    ungroup() 

  # for the first duplicate, these will stay chewcard 1:10, just remove columns, ready for merge with dupe 2
  dupe1 <- bind_cols(dplyr::filter(data_chewcards_adj, duplicates_season > 1 & ID == 1)) |>
    select(!(c(duplicates_season, ID, chewcards_deployed, chewcard.11, chewcard.12, chewcard.13, chewcard.14, chewcard.15, chewcard.16, chewcard.17, chewcard.18, chewcard.19, chewcard.20)))

 # for the 2nd duplicate, add put chewcards 1:10 as 11:20
  dupe2 <- filter(data_chewcards_adj, ID == 2) |>
    # first remove actual 11-20 columns
    select(!(c(duplicates_season, ID, chewcards_deployed, chewcard.11, chewcard.12, chewcard.13, chewcard.14, chewcard.15, chewcard.16, chewcard.17, chewcard.18, chewcard.19, chewcard.20))) |> 
    # then rename 1-10 as 11-20 
    rename(chewcard.11 = chewcard.1, chewcard.12 = chewcard.2, chewcard.13 = chewcard.3, chewcard.14 = chewcard.4, chewcard.15 = chewcard.5, chewcard.16 = chewcard.6, chewcard.17 = chewcard.7, chewcard.18 = chewcard.8, chewcard.19 = chewcard.9, chewcard.20 = chewcard.10)
  
  # now combine
  unduped <- left_join(dupe1, dupe2)

  # now add fixed duplicate data into data that didn't have duplicates
  data_chewcards_adj2 <- filter(data_chewcards_adj, !(duplicates_season > 1)) |>
    select(!(c(chewcards_deployed, duplicates_season, ID))) |>
    bind_rows(unduped)
  

  # RECOMBINE LIST ----------------------------------------------------------
  data_list_adj <- list(data_traps_adj, data_burrows_adj, data_chewcards_adj2)
  names(data_list_adj) <- c("traps", "burrows", "chewcards")
  

  # EXPAND DATAFRAMES FOR MISSING SURVEYS PER SEASON ------------------------

  add_missing_season <- function(data){
  
    # make a new dataframe for every season x year from the start
    x <- expand.grid(ae_zone = unique(data$ae_zone), ## MIGHT NEED TO THINK ABOUT THIS AGAIN?
                     season = c("DJF", "MAM", "JJA", "SON"),
                     year = seq(min(data$year), max(data$year), 1))
    
    # now add to real data so there is a row for every missed season / year
    data_missing <- full_join(data, x) #%>%
      #arrange(year, season, ae_zone)
    
    # give back new dataframe 
    return(data_missing)
  
}

  # use the function on the list
  data_list_adj_exp <- lapply(data_list_adj, add_missing_season)

  # return list
  return(data_list_adj_exp)
  
}

# HELPFUL CODE GRAVEYARD --------------------------------------------------

# calculate how many days since last survey 

# filter to repeat surveys per site / season
# data |>
#   dplyr::group_by(ae_zone, subsite, season, year, crop_type, crop_stage, date_start_session) |> # trap_type
#   mutate(count = n(),
#          fini = date_start_session, 
#          fend = lead(date_end_session), 
#          deltaD = as.numeric(fend - fini)) %>%
#   #na.omit() %>%
#   filter(count > 1) %>%
#   ungroup() %>%
#   arrange(ae_zone, subsite, season, year)