### Downloaded files from Lotz lab on LIMS, now going to save them and rename them

dir <- "C:/Users/gmillerScripps/bot/Lotz/raw/"

ls <- list.files(path = dir, pattern = 'project_details')

lapply(ls, function(file) {
  print(file)
  load <- read.csv(file = file.path(dir, file), as.is = TRUE)
  name <- names(load)
  name <- gsub("X..Project.Name.", "", x = name)
  names(load) <- 'name'
  get_run_ids <- subset(load, 
                        grepl("##Run List", name))
  run_id <- gsub(", ", "_", gsub("##Run List=", "", get_run_ids))
  full_read <- readLines(file.path(dir, file))
  
  samp_index <- max(which(grepl("Sample", full_read)))
  
  a <- full_read[max(which(grepl("Sample", full_read)))]

  b <- unlist(strsplit(a, ","))
  dat <- full_read[(max(which(grepl("Sample", full_read)))+1):length(full_read)]
  if(is.na(dat[1])) {
    dat <- data.frame()
    file_name <- paste0(paste("noinfo", name, run_id, sep = '_'), ".csv")
  } else {
    
    dat_unsplit <- gsub('\\"', '', unlist(strsplit(dat, '","')))
    full <- split(dat_unsplit, ceiling(seq_along(dat_unsplit)/15))
    dat <- data.frame(do.call('rbind', full))
    names(dat) <- b
    file_name <- paste0(paste(name, run_id, sep = '_'), ".csv")
  
    }
  
  write.csv(x = full, file = file.path('Lotz/renamed', file_name), row.names = FALSE)
  
})
