
akred_tukc_si_regelleistung_url <-function(unb = "NRV",ass=NULL){
  if(unb=="NRV"){
    #library(data.table)
    tukciwin <- fread("https://www.regelleistung.net/download/SAMMELDOWNLOAD_BETRIEBLICHE_ABRUFWERTE.CSV",sep2 = ";",skip=3,na.strings=c("","-"),dec=",",header=TRUE)
    datum<- as.POSIXct(paste(as.matrix(tukciwin$DATUM),as.matrix(tukciwin$`UHRZEIT VON`)),format="%d.%m.%Y %H:%M",tz="CET")
    tukciwin<-data.frame(DateTime=datum, BETR = tukciwin$RZ_SALDO )
  } 
  if(unb=="IGCC"){
    #library(data.table)
    tukciwin <- fread("https://www.regelleistung.net/download/SAMMELDOWNLOAD_IGCC.CSV",sep2 = ";",skip=3,na.strings=c("","-"),dec=",",header=TRUE)
    tukciwin <- tukciwin[tukciwin$DATENTYP=="Austausch_Deutschland",c("DATUM","UHRZEIT VON","BETR. IMPORT","BETR. EXPORT")]
    datum<- as.POSIXct(paste(as.matrix(tukciwin$DATUM),as.matrix(tukciwin$`UHRZEIT VON`)),format="%d.%m.%Y %H:%M",tz="CET")
    tukciwin<-data.frame(DateTime=datum,tukciwin[,c(-1,-2),drop=F],check.names = FALSE)
    if(is.na(tail(tukciwin,1)[,2]))tukciwin[dim(tukciwin)[1],2]<-0
    if(is.na(tail(tukciwin,1)[,3]))tukciwin[dim(tukciwin)[1],3]<-0
  } 
  tukciwin <- apply(tukciwin,2,function(x)gsub(",","\\.",x))
  if(!is.null(ass)){
    return(tukciwin[as.Date(datum,tz="CET")==ass,])
  }else{
    return(tukciwin)
  }
}



#Instal the package from GitHUB
# library(devtools)
# install_github("wagnertimo/rmarketcrawlR")

# Activate the package in the workspace
#library(rmarketcrawlR)

# You have to set logging to TRUE or FALSE if you want logs printed out and written in a file (Good for Debugging)
# No default yet. Will break if not set.
setLogging(FALSE)

# # Get sample data for operating needs, calls and auctions of secondary reserve power from the Netzregelverbund
 #needs = getReserveNeeds('01.01.2018', '10.01.2018')
# calls = getReserveCalls('21.10.2018', '21.10.2018', '4', 'SRL') # 6 == Netzregelverbund, SRL == secodary reserve power

  #auctions = getReserveAuctions('01.10.2018', '10.10.2018', '2') # 2 == secondary reserve power


amecah_n_tukciwin_regelleistung<-function(ukud_tazwara=NULL , ukud_tagara=NULL){
  #.....................
  tukciwin_tura <- list()
  tukciwin_tura[["50Hertz_SRL"]] = getReserveCalls(startDate =ukud_tazwara , endDate =ukud_tagara , '4', 'SRL') #
  tukciwin_tura[["TenneT_SRL"]] = getReserveCalls(startDate =ukud_tazwara , endDate =ukud_tagara , '2', 'SRL') 
  tukciwin_tura[["Amprion_SRL"]] = getReserveCalls(startDate = ukud_tazwara, endDate =ukud_tagara , '3', 'SRL') 
  tukciwin_tura[["TransnetBW_SRL"]] = getReserveCalls(startDate = ukud_tazwara, endDate = ukud_tagara, '1', 'SRL') 
  tukciwin_tura[["Netzregelverbund_SRL"]] = getReserveCalls(startDate =ukud_tazwara , endDate = ukud_tagara, '6', 'SRL') 
  #.................... MRL.........................................................
  tukciwin_tura[["50Hertz_MRL"]] = getReserveCalls(startDate =ukud_tazwara , endDate =ukud_tagara , '4', 'MRL') #
  tukciwin_tura[["TenneT_MRL"]] = getReserveCalls(startDate =ukud_tazwara , endDate =ukud_tagara , '2', 'MRL') 
  tukciwin_tura[["Amprion_MRL"]] = getReserveCalls(startDate = ukud_tazwara, endDate =ukud_tagara , '3', 'MRL') 
  tukciwin_tura[["TransnetBW_MRL"]] = getReserveCalls(startDate = ukud_tazwara, endDate = ukud_tagara, '1', 'MRL') 
  tukciwin_tura[["Netzregelverbund_MRL"]] = getReserveCalls(startDate =ukud_tazwara , endDate = ukud_tagara, '6', 'MRL') 
  #................... AUSTAUSCH....................................................
    #tukciwin_tura[["Netzregelverbund_Austausch"]]=getReserveCalls(startDate =ukud_tazwara , endDate = ukud_tagara, '11', 'Austausch') 
 
   #................... SALDO ....................................................
  #tukciwin_tura[["Netzregelverbund_SALDO"]]=getReserveCalls(startDate =ukud_tazwara , endDate = ukud_tagara, '6', 'RZ_SALDO') 
   #tukciwin_tura[["Netzregelverbund_SALDO"]] =data.frame(DateTime =tukciwin_tura[["Netzregelverbund_MRL"]]$DateTime ,BETR=tukciwin_tura[["Netzregelverbund_MRL"]]$neg_MW+tukciwin_tura[["Netzregelverbund_SRL"]]$neg_MW+tukciwin_tura[["Netzregelverbund_MRL"]]$pos_MW+tukciwin_tura[["Netzregelverbund_SRL"]]$pos_MW)
   tukciwin_tura[["Netzregelverbund_SALDO"]]   =akred_tukc_si_regelleistung_url(unb = "NRV",ass=as.Date(ukud_tazwara,"%d.%m.%Y"))
     tukciwin_tura[["Netzregelverbund_Austausch"]]= akred_tukc_si_regelleistung_url(unb = "IGCC",ass=as.Date(ukud_tazwara,"%d.%m.%Y"))

  #.................................................................................
  tukciwin_tura<-do.call(cbind,tukciwin_tura)
  #.................................................................................
  timeZone_indx <- grepl(".TZ",colnames(tukciwin_tura))
  tukciwin_tura<-tukciwin_tura[,!timeZone_indx,drop=F]
  #.................................................................................
  time_indx <-grepl(".DateTime",colnames(tukciwin_tura))
  datetime <- tukciwin_tura[,time_indx]
  tukciwin_tura<-tukciwin_tura[,!time_indx,drop=F]
  row.names(tukciwin_tura)<-datetime[,1]
  #.................................................................................
  #.... NEGATIVE Reserve ...........................................................
  tukciwin_tura[,grepl("neg_MW",colnames(tukciwin_tura))]<-  - tukciwin_tura[,grepl("neg_MW",colnames(tukciwin_tura))]
  #.................................................................................
  colnames(tukciwin_tura)<-gsub("MRL.","MRL_BETR.",colnames(tukciwin_tura))
  colnames(tukciwin_tura)<-gsub("SRL.","SRL_BETR.",colnames(tukciwin_tura))
  
  colnames(tukciwin_tura)<-gsub("Netzregelverbund_SALDO.BETR","Netzregelverbund_SALDO_BETR.",colnames(tukciwin_tura))
  colnames(tukciwin_tura)<-gsub("Netzregelverbund_Austausch.BETR","Netzregelverbund_Austausch_BETR",colnames(tukciwin_tura))
  
  #.................................................................................
  colnames(tukciwin_tura)<-gsub("neg_MW"," NEG",colnames(tukciwin_tura))
  colnames(tukciwin_tura)<-gsub("pos_MW"," POS",colnames(tukciwin_tura))
  #.................................................................................
  return(tukciwin_tura)
}


# a<-amecah_n_tukciwin_regelleistung(ukud_tazwara = "21.10.2018",ukud_tagara ="21.10.2018" )
