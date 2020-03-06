#...............................................
convert_MOL_into_vect_srl_amaynut <- function(woche=NULL,srl_wochen=NULL,MOL = NULL,MOL_levels= NULL){
  if(is.null(MOL_levels))MOL_levels <- c(0,15,25,50,75,100,150,200,270,350,480,600,750,1000,1200,1500)
  #...........................
  MOL_woche <- MOL[srl_wochen == woche,]
  #........ negative price when the Grid has to pay the provider for setting the control 
  temp_vect <- as.numeric(MOL_woche[MOL_woche[,"ENERGY_PRICE_PAYMENT_DIRECTION"]=="PROVIDER_TO_GRID","ENERGY_PRICE_[EUR/MWh]"])
  MOL_woche[MOL_woche[,"ENERGY_PRICE_PAYMENT_DIRECTION"]=="PROVIDER_TO_GRID","ENERGY_PRICE_[EUR/MWh]"]<- paste(- temp_vect)
  print("ghure- at tsu-d ma GRID_TO_PROVIDER negh winna nnidhen")
  
  #..............................
  MOL_POS_00_04 <-MOL_woche[MOL_woche[,"PRODUCT"]=="POS_00_04",]
  
  MOL_POS_00_04 <-MOL_POS_00_04[order(as.numeric(MOL_POS_00_04[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_POS_00_04[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_POS_00_04 <- MOL_POS_00_04[asurif,]
  
  MOL_POS_04_08 <-MOL_woche[MOL_woche[,"PRODUCT"]=="POS_04_08",]
  
  MOL_POS_04_08 <-MOL_POS_04_08[order(as.numeric(MOL_POS_04_08[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_POS_04_08[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_POS_04_08 <- MOL_POS_04_08[asurif,]
  
  
  MOL_POS_08_12 <-MOL_woche[MOL_woche[,"PRODUCT"]=="POS_08_12",]
  
  MOL_POS_08_12 <-MOL_POS_08_12[order(as.numeric(MOL_POS_08_12[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_POS_08_12[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_POS_08_12 <- MOL_POS_08_12[asurif,]
  
  
  MOL_POS_12_16 <-MOL_woche[MOL_woche[,"PRODUCT"]=="POS_12_16",]
  
  MOL_POS_12_16 <-MOL_POS_12_16[order(as.numeric(MOL_POS_12_16[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_POS_12_16[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_POS_12_16 <- MOL_POS_12_16[asurif,]
  
  
  MOL_POS_16_20 <-MOL_woche[MOL_woche[,"PRODUCT"]=="POS_16_20",]
  
  MOL_POS_16_20 <-MOL_POS_16_20[order(as.numeric(MOL_POS_16_20[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_POS_16_20[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_POS_16_20 <- MOL_POS_16_20[asurif,]
  
  
  MOL_POS_20_24 <-MOL_woche[MOL_woche[,"PRODUCT"]=="POS_20_24",]
  
  MOL_POS_20_24 <-MOL_POS_20_24[order(as.numeric(MOL_POS_20_24[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_POS_20_24[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_POS_20_24 <- MOL_POS_20_24[asurif,]
  #................................................
  MOL_NEG_00_04 <-MOL_woche[MOL_woche[,"PRODUCT"]=="NEG_00_04",]
  
  MOL_NEG_00_04 <-MOL_NEG_00_04[order(as.numeric(MOL_NEG_00_04[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_NEG_00_04[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_NEG_00_04 <- MOL_NEG_00_04[asurif,]
  
  MOL_NEG_04_08 <-MOL_woche[MOL_woche[,"PRODUCT"]=="NEG_04_08",]
  
  MOL_NEG_04_08 <-MOL_NEG_04_08[order(as.numeric(MOL_NEG_04_08[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_NEG_04_08[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_NEG_04_08 <- MOL_NEG_04_08[asurif,]
  
  
  MOL_NEG_08_12 <-MOL_woche[MOL_woche[,"PRODUCT"]=="NEG_08_12",]
  
  MOL_NEG_08_12 <-MOL_NEG_08_12[order(as.numeric(MOL_NEG_08_12[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_NEG_08_12[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_NEG_08_12 <- MOL_NEG_08_12[asurif,]
  
  
  MOL_NEG_12_16 <-MOL_woche[MOL_woche[,"PRODUCT"]=="NEG_12_16",]
  
  MOL_NEG_12_16 <-MOL_NEG_12_16[order(as.numeric(MOL_NEG_12_16[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_NEG_12_16[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_NEG_12_16 <- MOL_NEG_12_16[asurif,]
  
  
  MOL_NEG_16_20 <-MOL_woche[MOL_woche[,"PRODUCT"]=="NEG_16_20",]
  
  MOL_NEG_16_20 <-MOL_NEG_16_20[order(as.numeric(MOL_NEG_16_20[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_NEG_16_20[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_NEG_16_20 <- MOL_NEG_16_20[asurif,]
  
  
  MOL_NEG_20_24 <-MOL_woche[MOL_woche[,"PRODUCT"]=="NEG_20_24",]
  
  MOL_NEG_20_24 <-MOL_NEG_20_24[order(as.numeric(MOL_NEG_20_24[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_NEG_20_24[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_NEG_20_24 <- MOL_NEG_20_24[asurif,]
  
  
  
  #.............put all together...................
  
  
  #................................................
  MOL_vect<-rbind(c(MOL_POS_00_04[,1],MOL_NEG_00_04[,1]),
                  c(MOL_POS_04_08[,1],MOL_NEG_04_08[,1]),
                  c(MOL_POS_08_12[,1],MOL_NEG_08_12[,1]),
                  c(MOL_POS_12_16[,1],MOL_NEG_12_16[,1]),
                  c(MOL_POS_16_20[,1],MOL_NEG_16_20[,1]),
                  c(MOL_POS_20_24[,1],MOL_NEG_20_24[,1]))
  #................................................
  #................................................
  new_names <- c(paste0("P_POS_",1:nrow(MOL_NEG_20_24)),paste0("P_NEG_",1:nrow(MOL_NEG_20_24)))
  colnames(MOL_vect)<-new_names
  rownames(MOL_vect)<-paste0(base::as.Date(woche,tz="CET"),c("_00_04","_04_08","_08_12","_12_16","_16_20","_20_24"))
  #................................................
  return(MOL_vect)
}
#...
#.................................................

reBAP_for_Shiny<-function(tasga_n_tukc=NULL,reBAP_model=NULL,tala= "csv",ukud = NULL){

  
  print("ghre-d tukciwin timaynutin .....")
  if(tala == "csv"){
    ssuq_tukc_tuzrigin_tura<-ghred_and_heggid_tikciwin_tuzrigin( tasga_n_tukc = tasga_n_tukc )
  }else{
    if(is.null(ukud))ukud<-Sys.Date()
    ssuq_tukc_tuzrigin_tura<-amecah_n_tukciwin_regelleistung(ukud_tazwara = format(ukud,"%d.%m.%Y"),
                                                             ukud_tagara  = format(ukud,"%d.%m.%Y"))
  }
 
  print("ifuk.")
  print("sehnunfe-d ticekkarin n MOL")
  MOL_file_name<-paste0(tasga_n_tukc,"MOL/MOL_tukciwin_ass_",ukud,".RDS")
  if(file.exists(MOL_file_name)==FALSE){
    print("MOL: ulac iw assa , ad nessehluli....")
    MOL_tukciwin_tura<- tukc_tuzrigin_MOL_f(tasga_n_tukc =tasga_n_tukc )
    for(ass in paste(unique(MOL_tukciwin_tura$SRL$DATE_FROM))){
      MOL_file_name<-paste0(tasga_n_tukc,"MOL/MOL_tukciwin_ass_",base::as.Date(ass,tz="CET"),".RDS")
      if(file.exists(MOL_file_name)==FALSE){
        a<-MOL_tukciwin_tura
       # a$MRL<-a$MRL[a$MRL$DATE_FROM==ass,]
        a$SRL<-a$SRL[base::as.Date(a$SRL$DATE_FROM,tz="CET")==ass,]
        saveRDS(object =a,file =  MOL_file_name)
      }
    }
   
  }else{
    print("MOL: llant ya kan i w assa ")
    MOL_tukciwin_tura<- readRDS(file= MOL_file_name)
  }
  print("ifuk.")
  print("Cebhe-d tukciwin , sersitent , almad n tmacint(ML)")

  ML_reBAP_tukciwin_tura<- matching_product(tukc_tuzrigin= ssuq_tukc_tuzrigin_tura , MOL_tukciwin = MOL_tukciwin_tura,reserve = "SRL",tura = TRUE)
  print("ifuk.")
  #..................Cleaned up.......
  transfer_indx<- grep("Transfer",colnames(ML_reBAP_tukciwin_tura))
  if(length(transfer_indx)>0){
    colnames(ML_reBAP_tukciwin_tura)<-gsub("Transfer","Austausch",colnames(ML_reBAP_tukciwin_tura)) 
  }
  bad_indx <- colnames(ML_reBAP_tukciwin_tura)!="Netzregelverbund_SALDO_LETZTE AENDERUNG"
  ML_reBAP_tukciwin_tura<-ML_reBAP_tukciwin_tura[,bad_indx]
  #...................................
  print("seglilze-d reBAP ")
  reBAP<-round(predict(object = reBAP_model,ML_reBAP_tukciwin_tura ),2)
  shiny_reBAP <-   data.frame(reBAP = reBAP,"NRV SALDO" = ML_reBAP_tukciwin_tura[,"Netzregelverbund_SALDO_BETR."],check.names = FALSE)
  
  #...................................
  EPEX_id_file<- paste0("./data_template/EPEX/ID_price_",format(Sys.Date(),"%Y_%m_%d"),".RDS")
  if(file.exists(EPEX_id_file)){
    ID_price <- readRDS(EPEX_id_file)
     }else{
    do.call(file.remove, list(list.files("./data_template/EPEX/", pattern = "ID_price_",full.names = TRUE)))
    # ID_price<-get_epex_spot_tukciwin(ukud_tazwara = base::as.Date(min(rownames(shiny_reBAP)),tz="CET")-1,ukud_tagara=base::as.Date(min(rownames(shiny_reBAP)),tz="CET"),tamurt = "DE",EPEX_id_file=EPEX_id_file)
       ID_price <- EPEX_ID_price_crawler(trading_date = Sys.Date()-1,delivery_date = Sys.Date(),EPEX_id_file=EPEX_id_file)
    }
  shiny_reBAP<-cbind(shiny_reBAP,IDh=ID_price[rownames(shiny_reBAP),"hourly_ID_prices"])
  shiny_reBAP[,"reBAP"]<-apply(shiny_reBAP,1,function(x)reBAP_AEP3_f(reBAP_aep2 = x[1], SALDO = x[2] , ID_h_PRICE = x[3]))
  
  #...................................
  
  #...................................
  return(shiny_reBAP)
  #................................... 
}