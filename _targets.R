# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 

# Set target options:
tar_option_set(
  packages = c("tidyverse", "Hmisc", "sjlabelled", "sf", "cropgrowdays", "RcppRoll", "readxl", "qs", "terra", "extractOz"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)


# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source("r/")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  ## Extract and clean current monitoring databases
  tar_target(name = access_mon, command = "raw_data/ms_access/MouseMonitoring.accdb", format = "file"),
  tar_target(name = data_mon, command = extract_monitoring_database_traps(access_mon, habitat_lookup))
 # tar_target(name = data_mon_rap, command = extract_monitoring_database_rapid(access_mon)),
  
)
  

# to rerun microsoft access database extraction
#tar_invalidate(starts_with(c("data", "raw", "farm", "habitat", "access", "ALL", "i", "s", "t")))
#tar_make()


#   format = "feather" # efficient storage of large data frames # nolint
