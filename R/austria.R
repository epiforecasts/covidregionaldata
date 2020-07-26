get_austria_regional_cases <- function() {
  
  # Path to data
  path <- "https://github.com/statistikat/coronaDAT/archive/master.zip"
  
  # Download repo
  temp <- tempfile()
  download.file(url = path,
                destfile = temp)
  
  # Unzip the repo
  unzip(zipfile = temp)
  p <- unzip(zipfile = temp)
  
  # Split by filetype
  rd <- p[grep(pattern = "[0-9].rds", x = p)] # remove files containing non-case data
                                              # whose file names do not end with a number followed by .rds
  zp <- p[grep(pattern = "csv.zip", x = p)] # changed ".zip" to "csv.zip" because of 
                                            # .js files which cause error later on
  
  # Extract data
  rd_dat <- lapply(rd, function(x){
    if(file.exists(x))
      tryCatch(tmp <- readRDS(x),
               error = function(e){})
    return(tmp)
  })
  
  zp_dat <- lapply(zp, function(x){
    if(file.exists(x)){
      tmp <- read.csv(unzip(x, files = "Bezirke.csv"), header = TRUE, sep = ";")
      
      # add timestamp for those files that don't have one
      if(!("timestamp" %in% tolower(names(tmp)))){
        # for those that have time and date in filename
        if(nchar(x) == 69){ 
          datetime <- substr(x, 42, 56)
          timestamp <- strptime(datetime, format = "%Y%m%d_%H%M%S", tz = "Europe/Vienna")
          timestamp <- format(timestamp, "%Y-%m-%d %H:%M:%S")
        } else if(nchar(x) == 62){
          # for those that don't have time in filename
          # set time to 00:00:00 accordingly with datasets published later
          datetime <- paste0(substr(x, 42, 49), "000000")
          timestamp <- strptime(datetime, format = "%Y%m%d%H%M%S", tz = "Europe/Vienna")
          timestamp <- format(timestamp, "%Y-%m-%d %H:%M:%S")
        } else {
          stop("error is in adding timestamp to unzipped csv files in zp_dat") # for debugging
        }
        tmp <- cbind(tmp, "time" = timestamp)
      }
    }
    return(tmp)
  })
  
  # for debugging: check whether all files were imported
  #length(rd_dat) + length(zp_dat) == length(p[!grepl("js.zip", p)])
  
  # Extract info on region, cases and date/time from rds
  dat <- lapply(1 : length(rd_dat),
                function(x){
                  grab <- rd_dat[[x]]
                  return(cbind(grab$bezirke, 
                               "time" = format(grab$timestamp, "%Y-%m-%d %H:%M:%S")))
                })
  
  # Combine this
  
  dat <- do.call(rbind, dat)
  
  # Extract info on region, cases and date/time from zip
  dat2 <- lapply(1 : length(zp_dat), function(x){
    if(is.data.frame(zp_dat[[x]])){
      grab <- zp_dat[[x]]
      names(grab) <- tolower(names(grab))
      if("timestamp" %in% names(grab)){
        grab$time <- gsub("T", " ", grab$timestamp) # for consistency
      }
      save <- as.data.frame(cbind(as.character(grab$bezirk), grab$anzahl, 
                                  as.character(grab$time)))
      names(save) <- c("bezirk", "freq", "time")
      return(save)
    } else {
      stop("error in creation of dat2")
    }
  })
  
  # Combine this
  #dat2 <- sapply(dat2, na.omit) # I don't think this is needed
  dat2 <- do.call(rbind, dat2)
  
  # Combine old and new data
  dat <- rbind(dat, dat2)
  
  # Fix inconsistent naming of districts
  names_fix <- c("Braunau", "Kirchdorf", "Ried", "Stadt Linz", "Stadt Steyr", "Stadt Wels")
  dat$bezirk <- sapply(dat$bezirk, function(x){
    if (x == "Braunau") out <- "Braunau am Inn"
    else if (x == "Kirchdorf") out <- "Kirchdorf an der Krems"
    else if (x == "Ried") out <- "Ried im Innkreis"
    else if (x == "Stadt Linz") out <- "Linz(Stadt)"
    else if (x == "Stadt Steyr") out <- "Steyr(Stadt)"
    else if (x == "Stadt Wels") out <- "Wels(Stadt)"
    else out <- x
    return(out)
  })
  
  # TODO remove duplicates --> apparently there are none
  #vec <- sapply(unique(dat$bezirk), function(x){
  #  idx <- which(dat$bezirk == x)
  #  times <- dat[idx, "time"]
  #  ind <- length(times) == length(unique(times))
  #})
  #any(isFALSE(vec))
  
  # Add NUTS info  
  nuts <- data.frame(bezirk = c("Amstetten", "Baden", "Bludenz", "Braunau am Inn",
                             "Bregenz", "Bruck an der Leitha", "Bruck-Mürzzuschlag",
                             "Deutschlandsberg", "Dornbirn", "Eferding",
                             "Eisenstadt(Stadt)", "Eisenstadt-Umgebung", "Feldkirch",
                             "Feldkirchen", "Freistadt", "Gänserndorf", "Gmünd",
                             "Gmunden", "Graz(Stadt)", "Graz-Umgebung",
                             "Grieskirchen", "Gröbming", "Hallein", "Hartberg-Fürstenfeld",
                             "Hermagor", "Hollabrunn", "Imst", "Innsbruck-Land",
                             "Innsbruck-Stadt", "Jennersdorf", "Kirchdorf an der Krems",
                             "Kitzbühel", "Klagenfurt Land", "Klagenfurt Stadt",
                             "Korneuburg", "Krems an der Donau(Stadt)", "Krems(Land)",
                             "Kufstein", "Landeck", "Leibnitz", "Leoben", "Lienz",
                             "Liezen", "Lilienfeld", "Linz(Stadt)", "Linz-Land",
                             "Mattersburg", "Melk", "Mistelbach", "Mödling", "Murau",
                             "Murtal", "Neunkirchen", "Neusiedl am See", "Oberpullendorf",
                             "Oberwart", "Perg", "Reutte", "Ried im Innkreis",
                             "Rohrbach", "Salzburg(Stadt)", "Salzburg-Umgebung",
                             "Sankt Johann im Pongau", "Sankt Pölten(Land)",
                             "Sankt Pölten(Stadt)", "Sankt Veit an der Glan",
                             "Schärding", "Scheibbs", "Schwaz", "Spittal an der Drau",
                             "Steyr(Stadt)", "Steyr-Land", "Südoststeiermark",
                             "Tamsweg", "Tulln", "Urfahr-Umgebung", "Villach Land",
                             "Villach Stadt", "Vöcklabruck", "Voitsberg", "Völkermarkt",
                             "Waidhofen an der Thaya", "Waidhofen an der Ybbs(Stadt)",
                             "Weiz", "Wels(Stadt)", "Wels-Land", "Wien  1.",
                             "Wien  2.", "Wien  3.", "Wien  4.", "Wien  5.",
                             "Wien  6.", "Wien  7.", "Wien  8.", "Wien  9.",
                             "Wien 10.", "Wien 11.", "Wien 12.", "Wien 13.",
                             "Wien 14.", "Wien 15.", "Wien 16.", "Wien 17.",
                             "Wien 18.", "Wien 19.", "Wien 20.", "Wien 21.",
                             "Wien 22.", "Wien 23.", "Wiener Neustadt(Land)",
                             "Wiener Neustadt(Stadt)", "Wolfsberg", "Zell am See",
                             "Zwettl", "Horn", "Güssing", "Wien(Stadt)",
                             "Liezen (inkl. Gröbming)"),
                     lvl3 = c("AT121", "AT127", "AT341", "AT311", "AT341",
                              "AT127", "AT223", "AT225", "AT342", "AT312",
                              "AT112", "AT112", "AT342", "AT212", "AT313",
                              "AT125", "AT124", "AT315", "AT221", "AT221",
                              "AT311", "AT222", "AT323", "AT224", "AT212",
                              "AT125", "AT334", "AT332", "AT332", "AT113",
                              "AT314", "AT335", "AT211", "AT211", "AT126",
                              "AT124", "AT124", "AT335", "AT334", "AT225",
                              "AT223", "AT333", "AT222", "AT122", "AT312",
                              "AT312", "AT112", "AT121", "AT125", "AT127",
                              "AT226", "AT226", "AT122", "AT112", "AT111",
                              "AT113", "AT313", "AT331", "AT311", "AT313",
                              "AT323", "AT323", "AT322", "AT123", "AT123",
                              "AT213", "AT311", "AT121", "AT335", "AT212",
                              "AT314", "AT314", "AT224", "AT321", "AT126",
                              "AT312", "AT211", "AT211", "AT315", "AT225",
                              "AT213", "AT124", "AT121", "AT224", "AT312",
                              "AT312", rep("AT130", 23), "AT122", "AT122",
                              "AT213", "AT322", "AT124", "AT124", "AT113",
                              "AT130", "AT222"),
                     stringsAsFactors = FALSE)
  nuts$lvl2 <- substr(nuts$lvl3, 1, nchar(nuts$lvl3) - 1)
  nuts$lvl1 <- substr(nuts$lvl3, 1, nchar(nuts$lvl3) - 2)
  
  dat <- plyr::join(dat, nuts, by = "bezirk")
    
  return(as.data.frame(dat))
}

get_austria_regional_cases_only_level_1 <- function() {
  dat <- get_austria_regional_cases()
  dat <- dat[, c("freq", "time", "lvl1")]
  names(dat) <- c("cases", "date", "region_level_1")
  return(dat)
}
