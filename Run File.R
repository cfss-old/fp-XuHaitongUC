# one script to rule them all

# clean out any previous work
paths <- c("data", "graphics", "output")

for(path in paths){
  unlink(path, recursive = TRUE)    # delete folder and contents
  dir.create(path)                  # create empty folder
}

# run my scripts
source("00_download-data.R")
source("01_merge-filter.R")
source("02_statistical_learning.R")
