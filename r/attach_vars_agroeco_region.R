attach_vars_agroeco_region <- function(data){
  
  # note this function relies on the extractOz package - download here:
  # remotes::install_github("DPIRD-FSI/extractOz", build_vignettes = TRUE)
  
  # condense dataframe into one row for each unique region and coords
  sites <- data %>%
    select(subsite, x = longitude, y = latitude) %>%
    unique() 
  
  # list of coordinates
  locs <- purrr::transpose(sites[,2:3])
  
  # name list using region
  names(locs) <- sites$subsite
  
  # make unique - extract_ae_zone seems to fall over without?
  locs <- unique(locs)
  
  # extract grdc agro-ecological zone, add to site data 
  data_ae <- extract_ae_zone(x = locs) %>%
   # rename(subsite = location) %>%
    transmute(longitude = x, latitude = y, ae_zone) 
   # bind_cols(sites, .) %>%
   # select(!(region)) %>% # remove region because there are multiple grdc ae zones wihin a 'region' - messes up the join
   # rename(x = longitude, y = latitude)
    
  
  ##### QUICK FIX 
  # some sites are missing AE zone, must be just outside the border, but these are all "NSW NE/Qld SE"
  data_ae <- mutate(data_ae, ae_zone = if_else(ae_zone == "", "NSW NE/Qld SE", ae_zone))
  ######## CHECK THIS IF NEW SITES ARE ADDED
  
  # add ae into original dataframe 
  data <- left_join(data, data_ae, by = c("longitude", "latitude"))
                   
  # save 
  return(data)

}
