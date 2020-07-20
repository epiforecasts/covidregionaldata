get_austria_regional_cases <- function() {
  
  # Path to data
  path <- "https://github.com/statistikat/coronaDAT/archive/master.zip"
  
  # Download repo
  temp <- tempfile()
  download.file(url = path,
                destfile = temp)
  
  # Unzip the repo
  p <- unzip(zipfile = temp)
  
  # Split by filetype
  rd <- p[grep(pattern = ".rds", x = p)]
  zp <- p[grep(pattern = ".zip", x = p)]
  
  # Extract data
  rd_dat <- lapply(rd, function(x){
    if(file.exists(x))
      tryCatch(tmp <- readRDS(x),
               error = function(e){})
    return(tmp)
  })
  
  zp_dat <- lapply(zp, function(x){
    if(file.exists(x))
      tryCatch(tmp <- read.csv(unz(x)),
               error = function(e){})
    return(tmp)
  })
  
  # TODO provide info on which files experience errors
  
  # Extract info on region and date/time from rds
  dat <- sapply(1 : length(rd_dat),
                function(x){
                  grab <- rd_dat[x]
                  grab <- grab[[1]]
                  cbind(grab$bezirke, grab$timestamp)
                })
  # Combine this
  dat <- do.call(rbind, dat)
  names(dat)[3] <- "time"
  
  # TODO Extract info on region and date/time from zip
  # and remove potential duplicates between the two formats
  
  #dat2 <- sapply(1 : length(zp_dat),
  #              function(x){
  #                grab <- zp_dat[x]
  #                grab <- grab[[1]]
  #                cbind(grab$bezirke, grab$timestamp)
  #              })
  # Combine this
  #dat2 <- do.call(rbind, dat2)
  #names(dat2)[3] <- "time"
  
  return(as.data.frame(dat))
}