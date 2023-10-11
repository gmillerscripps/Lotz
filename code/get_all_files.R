# Author: Garrett Miller
# Version: 0.0.1
# Last Edited : 2023-10-11
# Purpose: This script is used to rename Project Detail CSV's from LIMS with the
# run information
# https://genomics-ccbb.scripps.edu/illumina/project_details.php
# Prerequisites: Download project detail csv's from LIMS and save in a "raw" directory 
# in the current working directory. 
# The files always download with the name: "project_details.csv". 
# I just keep them in the Downloads folder until I have downloaded all of them, 
# so that each each file will have a number since they will be repeat downloads
# 
# Inputs: Downloaded project detail csv's

# Current working directory
# Make sure that you are currently in code/
# Make sure you have saved the project detail csv's in a ./raw directory
dir <- "../raw/"

# Lists csv files in the directory (MAKE SURE THAT THIS MATCHES THE NUMBER OF PROJECTS
# THAT YOU SEE ON THE LIMS SITE)
file_names <- list.files(path = dir, pattern = 'csv')

# Loops through the files
ls <- lapply(file_names, function(file) {
  
  # Load current csv in original format  
  load <- read.csv(file = file.path(dir, file), as.is = TRUE)
  
  # Get the project name from the header pulled from the csv
  proj_name_raw <- names(load)
  
  # Clean up the name
  proj_name <- gsub("X..Project.Name.", "", x = proj_name_raw)
  
  # Rename current dataframe just so it is easier to reference and pull the run ID
  # Will not need this dataframe anymore
  names(load) <- 'name'
  
  # Subset the dataframe to get the run list
  get_run_ids <- subset(load, 
                        grepl("##Run List", name))
  
  # Reformat the run id and separate with underscore to use in filename
  run_id <- gsub(", ", "_", gsub("##Run List=", "", get_run_ids))
  
  # If there is no run id, then set the character string to "NORUN"
  if(run_id=="") run_id <- "NORUN"
  
  # Need to use readLines to get the sample dataframe from the csv
  full_read <- readLines(file.path(dir, file))
  
  # Index the row at which the sample dataframe begins
  samp_index <- max(which(grepl("Sample", full_read)))
  
  # Get the row with the column names for the samples dataframe
  a <- full_read[samp_index]

  # Split this string to get the individual column names
  b <- unlist(strsplit(a, ","))
  
  # Determine the max row, if the samples dataframe is empty then this will just be 
  # The row for the column headers
  max_row <- ifelse(samp_index==length(full_read), samp_index + 1, length(full_read))
  
  # Get the samples dataframe from the full read
  dat <- full_read[(max(which(grepl("Sample", full_read)))+1):(max_row)]
  
  # This comes in a comma delimited format, split on the format to get the full string
  dat_unsplit <- gsub('\\"', '', unlist(strsplit(dat, '","')))
  
  # The full character vector will have all the rows of data, split this according to 
  # the number of columns in the samples dataframe (which should just be 15)
  full <- split(dat_unsplit, ceiling(seq_along(dat_unsplit)/length(b)))
  
  # Combine this list into a dataframe
  combine <- data.frame(do.call('rbind', full))
  
  # Paste the sample names together if they exist, just to show in the summary table
  sample_names <- paste0(collapse = ', ', combine$X1)
  
  # Here we determine whether a project has "no info" on the samples used or not. 
  # Sometimes the sample id/name has the words "fake",
  # Sometimes the id comes across as "01id" or "02id"
  # sometimes there is only one row and we know we should have more sample info
  # sometimes the "exp" is an indicator that we need more sample info
  # sometimes the sample name says "dummy" or "training"
  if(any(is.na(combine$X1)) | 
     # These are all the conditions for which I might name a file 
     # with a "noinfo" naming convention at the beginning
     any(grepl("fake", combine$X1)) |
     nrow(combine) == 1 & all(grepl('01id|02id', combine$X1))|
     nrow(combine) == 1 |
     any(grepl("exp3X3", combine$X1)) | 
     any(grepl("exp", combine$X1)) | 
     any(grepl("Training", combine$X2))) {
    
    # Add a "noinfo" tag at the start of the filename if we think we 
    # don't have the sufficient sample info for this project
    # additionally, add the run information at the end of the filename
    file_name <- paste0(paste("noinfo", proj_name, run_id, sep = '_'), ".csv")
  } else {
    
    # Otherwise, don't add the "noinfo" tag but still add the run information
    file_name <- paste0(paste(proj_name, run_id, sep = '_'), ".csv")
  
  }
  
  # Copying the files (basically renaming them according to their project name,
  # but in a new folder)
  file.copy(file.path(dir, file), 
            to = file.path('../renamed', file_name), 
            overwrite = TRUE)
  
  # For each file, create this data frame that contains
  # Project name
  # run id(s)
  # no_info - whether or not we added the "noinfo" tag to the start of the filename
  # type - The unique "types" shown in the samples dataframe
  # sample_names - the sample names from the samples dataframe
  # file_name - What we renamed the file
  # orig_name - The original name for the file in the ./raw directory
  summary_tab <- data.frame(
    project = proj_name, 
    run_id = run_id, 
    no_info = ifelse(grepl('noinfo', file_name), TRUE, FALSE), 
    type = ifelse(is.null(unique(combine$X9)), "NULL", unique(combine$X9)), 
    sample_names = sample_names, 
    file_name = file_name,
    orig_name = file
  )

    return(summary_tab)
})

# After looping over the file names, we can combine each "summary dataframe"
# Check this table to ensure that the files being named "noinfo" are identified correctly
# and vice-versa
overall_summary_tab <- do.call('rbind',ls )

# Save this summary tab
write.csv(overall_summary_tab, 
          file = '../summary_info/summary_table.csv', 
          row.names =  FALSE)

# Check to ensure that there were no duplicates (as in, the same project's downloaded csv)
# in the ./raw folder
# Since the 'overwrite' argument in the above call to file.copy is set to TRUE
# Duplicates will automatically be removed
# However, one could click "Download Project Details" for the same project in LIMS
# and the downloaded files could for example appear as: 
# "project_details(1).csv" and "project_details(2).csv" 
# however, these are in fact the same project, therefore only one renamed file will
# be created. 
assertthat::assert_that(length(ls) == length(unique(summary_tab$file_name)), 
                        msg = "The number of saved files does not match the 
                        number of files in the ./raw folder. This could indicate that duplicates
                        were placed in the ./raw folder. Check that the number of projects saved 
                        (in the ./renamed folder) matches the expected number of projects based
                        on what you see in LIMS")

