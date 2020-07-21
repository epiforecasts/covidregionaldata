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
