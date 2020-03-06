build_df_rl_calls2<- function (response_content, fileName,uenb_type,rl_type) 
{
  library(logging)
  write.csv(response_content, file = fileName, eol = "\n")
  if (getOption("logging")) 
    loginfo("build_df_rl_calls - Called for Reserve Calls. Read in file")
  df <- read.csv2(file = fileName, header = TRUE, sep = ";", 
                  na.strings = c("", "-"), quote = "", skip = 1)
  colnames(df)[1] <- "DATUM"
  if(uenb_type == 1 & rl_type == "RZ_SALDO"){
    
  }else if(uenb_type == 1 & rl_type == "DEU"){
    
  }else{
    df <- df[, c("DATUM", "UHRZEIT.VON", "BETR..NEG", "BETR..POS")]
    df$DATUM <- as.Date(df$DATUM, "%d.%m.%Y")
    df[, c("BETR..NEG", "BETR..POS")] <- apply(df[, c("BETR..NEG", 
                                                      "BETR..POS")], MARGIN = 1:2, FUN = function(x2) formatGermanNumber(x2))
    df$BETR..NEG <- -as.numeric(df$BETR..NEG)
    df$BETR..POS <- as.numeric(df$BETR..POS)
  }
  invisible(if (file.exists(fileName)) file.remove(fileName))
  return(df)
}


getOperatingReserveCalls2<-function (date_from, date_to, uenb_type, rl_type) 
{
  library(logging)
  dates <- getDatesArrayOfMonths(date_from, date_to)
  df <- data.frame()
  if (getOption("logging")) 
    pb <- txtProgressBar(min = 0, max = nrow(dates), style = 3)
  for (e in 1:nrow(dates)) {
    if (getOption("logging")) 
      loginfo(paste("getOperatinReserveCalls - POST request for timeframe: ", 
                    dates[e, 1], " - ", dates[e, 2], sep = ""))
    r <- scrape_rl_calls(dates[e, 1], dates[e, 2], uenb_type, 
                         rl_type)
    p <- preprocess_rl_calls(r)
    filename <- paste("temp_calls_", e, sep = "")
    d <- build_df_rl_calls(p, filename)
    df <- rbind(df, d)
    if (getOption("logging")) 
      setTxtProgressBar(pb, e)
  }
  if (getOption("logging")) 
    close(pb)
  if (getOption("logging")) 
    loginfo("getOperatingReserveCalls - DONE")
  return(df)
}


preProcessOperatingReserveCalls2<-function (df.calls) 
{
  library(logging)
  library(data.table)
  library(lubridate)
  library(dplyr)
  library(magrittr)
  if (getOption("logging")) 
    loginfo("preProcessOperatingReserveCalls - Formatting POSIXct DateTime object")
  df.calls$DateTime <- as.POSIXct(paste(ymd(df.calls$DATUM), 
                                        paste(df.calls$UHRZEIT.VON, ":00", sep = ""), sep = " "), 
                                  tz = "Europe/Berlin")
  setnames(df.calls, "BETR..POS", "pos_MW")
  setnames(df.calls, "BETR..NEG", "neg_MW")
  df.calls <- addTimezone(df.calls)
  keeps <- c("DateTime", "neg_MW", "pos_MW", "TZ")
  if (getOption("logging")) 
    loginfo("preProcessOperatingReserveCalls - DONE")
  return(df.calls[, keeps])
}
getReserveCalls2<-function (startDate, endDate, uenb, rl) 
{
  library(logging)
  basicConfig(level = "DEBUG")
  nameLogFile <- paste("getReserveCalls_", gsub(":", "", as.character(Sys.time())), 
                       ".txt", sep = "")
  addHandler(writeToFile, file = nameLogFile, level = "DEBUG")
  df <- preProcessOperatingReserveCalls(getOperatingReserveCalls(startDate, 
                                                                 endDate, uenb, rl))
  return(df)
}
