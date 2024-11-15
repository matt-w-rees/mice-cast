attach_vars_precipitation <- function(data){
  
  terraOptions(tempdir = "tmp")
  
  # testing
  #library(targets)
  #library(terra)
  #library(tidyverse)
  #tar_load(data_list_ae_exp)
  #data <- data_list_ae_exp$traps
  

  # MAKE DATASET A TERRA VECT -----------------------------------------------
  
  #testing 
  # also add year_season column to dataset for merge 
  data$year_season <- paste0(data$year_adj, "-", data$season)
  
  # remove this survey as rainfall doesn't exist -- NEED TO DO THIS BY HAND
  data <- dplyr::filter(data, year_season != "2024-SON")
  
   # used at end of script
  data_vect <- terra::vect(sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = "epsg:4326"))
  
  
  
  # DOWNLOAD INTERPOLATED MONTHLY RAINFALL SUMMARIES ----------------------------------------------------------------
  dl_rain <- function(year_x){
    
    # check if file already exists    
    if(!(file.exists(paste0("raw_data/predictor_variables/rainfall/", year_x, ".monthly_rain.nc")))){
      
      # if not, download it
      download.file(paste0("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/monthly_rain/", year_x, ".monthly_rain.nc"), 
                    paste0("raw_data/predictor_variables/rainfall/", year_x, ".monthly_rain.nc"), 
                    method = "auto", quiet = TRUE, mode="wb", cacheOK = TRUE)
    }
  }
  
  # make a list of all years wanted 
  x <- as.list(seq(min(data$year_adj) - 1, # wind back 1 yearsbecause December the previous actual year is now the start of the adjusted year
                   max(data$year_adj), 
                   1))
  
  # apply function over the list
  sapply(x, FUN = dl_rain)
  
  

  # PROCESS RAINFALL AS RASTER STACK ----------------------------------------------------------

  # read all files in folder
  files <- list.files(path = "raw_data/predictor_variables/rainfall/", pattern = ".nc", full.names = TRUE)
  
  # read as raster stack
  rain_rasters <- terra::rast(files)
  
  ## rename raster to match year_month in data
  # remove day from ymd
  names_tmp <- substr(as.character(time(rain_rasters)), 0, nchar(as.character(time(rain_rasters))) - 3)
  # remove 0 from months
  names_tmp <- gsub("-0", "-", names_tmp)
  # now rename raster 
  names(rain_rasters) <- names_tmp
  

  # SUM RAINFALL BY YEAR AND SEASON  ----------------------------------------
  ## for each year, sum monthly rainfall into seasonal layers - remember December is start of the year!
  
  # first, lets remove 11 layers (months) so we start with december of the year previous to the one we want to start modelling with (note minus 1 year in the above code)
  rain_rasters <- rain_rasters[[12:nlyr(rain_rasters)]]

  # then remove last 1 - 2 layers when the last available month isn't at the end of the season (given we are summing and not averaging)
  if (!(is.integer(nlyr(rain_rasters) / 3))){                    # if the number of raster layers is not divisible by 3 (for 3 months in a season - this hinges on starting in december using the code above!)
    rain_rasters <- rain_rasters[[1:(nlyr(rain_rasters)-1)]]     # remove one layer
      if (!(is.integer(nlyr(rain_rasters) / 3))){                # check again 
        rain_rasters <- rain_rasters[[1:(nlyr(rain_rasters)-1)]] # remove one layer again if we need to 
      }
    }
  
  # now we can specify the index denoting which layers relate to the same season (for terra tapp function below)
  index = rep(1:(nlyr(rain_rasters) / 3), each = 3) # 3 times for 3 months in a season
  
  # sum rainfall amounts in each season, based on order (aka index) of the raster layer (which reflects seasons)
  rain_rasters_season <- tapp(rain_rasters, index=index, fun=sum)
  # this strips time info and layer names - add back in 
  
  # redo time value of each layer - take every 3rd time from previous raster - now reflects starting of season
  time(rain_rasters_season) <- time(rain_rasters)[seq(1, length(time(rain_rasters)), 3)]
  
  # adjust year in time for December months - push forward 1 year
  time(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) == 12, time(rain_rasters_season) + years(1), time(rain_rasters_season))
  
  # now specify name to year_season based on time value
  names(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) %in% c(12,1,2), paste0(year(time(rain_rasters_season)), "-DJF"), names(rain_rasters_season))
  names(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) %in% c(3,4,5), paste0(year(time(rain_rasters_season)), "-MAM"), names(rain_rasters_season))
  names(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) %in% c(6,7,8), paste0(year(time(rain_rasters_season)), "-JJA"), names(rain_rasters_season))
  names(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) %in% c(9,10,11), paste0(year(time(rain_rasters_season)), "-SON"), names(rain_rasters_season))
  
  
  # ADD SUMMED RAINFALL FROM RASTER TO MOUSE DATASET ------------------------
  
  # now extract values from soil moisture layer -- choose layer based on order of year_season 
  x <- terra::extract(rain_rasters_season, data_vect, layer = data_vect$year_season) 
  
  # add value to existing nonspatial dataset 
  data$precip <- x$value
  
  return(data)
}