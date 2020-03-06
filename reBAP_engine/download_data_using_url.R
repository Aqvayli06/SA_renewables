# download.file(url="https://www.regelleistung.net/download/SAMMELDOWNLOAD_BETRIEBLICHE_ABRUFWERTE.CSV",
#               destfile= "C:/Users/user/Documents/Assirem/Taftilt Analytics/MOL_complement/SAMMELDOWNLOAD_BETRIEBLICHE_ABRUFWERTE.CSV", quiet = FALSE, mode = "w",
#               cacheOK = TRUE,
#               extra = getOption("download.file.extra"))
# #..............................................................
akred_tukc_si_regelleistung_url <-function(unb = "NRV"){
 if(unb=="NRV"){
   #library(data.table)
   tukciwin <- fread("https://www.regelleistung.net/download/SAMMELDOWNLOAD_BETRIEBLICHE_ABRUFWERTE.CSV",sep2 = ";",skip=3,na.strings=c("","-"),dec=",",header=TRUE)
   tukciwin <- data.frame(tukciwin,check.names = FALSE)
   datum<- as.POSIXct(paste(as.matrix(tukciwin$DATUM),as.matrix(tukciwin$`UHRZEIT VON`)),format="%d.%m.%Y %H:%M")
   return(data.frame(DateTime=datum, BETR = tukciwin$RZ_SALDO ))
 } 
  if(nrw=="IGCC"){
    #library(data.table)
    
    tukciwin <- fread("https://www.regelleistung.net/download/SAMMELDOWNLOAD_IGCC.CSV",sep2 = ";",skip=3,na.strings=c("","-"),dec=",",header=TRUE)
    tukciwin <- tukciwin[tukciwin$DATENTYP=="Austausch_Deutschland",c("DATUM","UHRZEIT VON","BETR. IMPORT","BETR. EXPORT")]
    datum<- as.POSIXct(paste(as.matrix(tukciwin$DATUM),as.matrix(tukciwin$`UHRZEIT VON`)),format="%d.%m.%Y %H:%M")
    return(data.frame(DateTime=datum, BETR = tukciwin[,c(-1,2),drop=F] ))
  } 
  
  download.file(url = "https://www.regelleistung.net/apps/cpp-publisher/api/v1/download/tenders/anonymousresults?from=2018-12-09&to=2018-12-17&productTypes=mFRR&exportFormat=xlsx", destfile = "./farid.xlsx", mode="wb")
  tukciwin <- fread("https://www.regelleistung.net/download/RESULT_LIST_ANONYM_aFRR_2018-11-25_2018-12-03.xlsx")
  tukciwin <- tukciwin[tukciwin$DATENTYP=="Austausch_Deutschland",c("DATUM","UHRZEIT VON","BETR. IMPORT","BETR. EXPORT")]
  datum<- as.POSIXct(paste(as.matrix(tukciwin$DATUM),as.matrix(tukciwin$`UHRZEIT VON`)),format="%d.%m.%Y %H:%M")
  
}


