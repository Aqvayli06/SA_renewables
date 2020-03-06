############################################
#
#  reBAP model 
#
############################################
#. dacu-t: 

############################################
#
#  download MOL data 
#
############################################
#. dacu-t: 

#........................................................
akred_MOL_tukciwin<-function(tasga_n_tukc = NULL){
  ukud_tazwara<-Sys.Date()-3
  ukud_tagara<-ukud_tazwara + 6
  url_MOL_SRL <-paste0("https://www.regelleistung.net/apps/cpp-publisher/api/v1/download/tenders/anonymousresults?from=",ukud_tazwara,"&to=",ukud_tagara,"&productTypes=aFRR&exportFormat=xlsx")
  #url_MOL_MRL <-paste0("https://www.regelleistung.net/apps/cpp-publisher/api/v1/download/tenders/anonymousresults?from=",ukud_tazwara,"&to=",ukud_tagara,"&productTypes=mFRR&exportFormat=xlsx")
  download.file(url = url_MOL_SRL, destfile = paste0(tasga_n_tukc,"MOL/SRL.xlsx"), mode="wb",quiet = TRUE)
 # download.file(url = url_MOL_MRL, destfile = paste0(tasga_n_tukc,"MOL/MRL.xlsx"), mode="wb",quiet = TRUE)
  print("dayen n ukre-d tukciwin n MOL ..... agh yeslem REBBI ")
}

tuck_restructure<-function(tukciwin=NULL,struct_mode="DE",tuck_uzen =NULL){
  #..............................
  if(struct_mode=="DE"){
    tukciwin <- unique(data.frame(tukciwin,check.names = FALSE))
    datum<- as.Date(as.matrix(tukciwin$DATUM),format="%d.%m.%Y")
    yiwen_product <- paste(datum,as.matrix(tukciwin$`UHRZEIT VON`))
    if(length(unique(yiwen_product))<length(yiwen_product)){
      temp_indx<-1:length(yiwen_product)
      yiwen_product<-sapply(unique(yiwen_product),function(x)temp_indx[yiwen_product==x] )
      yiwen_product <- unlist(lapply(yiwen_product, function(x)x[[1]]))
      datum<-datum[yiwen_product]
      tukciwin<-tukciwin[yiwen_product,]
    }
    rownames(tukciwin)<-paste(datum,as.matrix(tukciwin$`UHRZEIT VON`))
    tukciwin <- apply(tukciwin[,1:5],2,function(x)gsub(",",".",x))
    colnames(tukciwin)[-(1:3)] <-paste(tuck_uzen,colnames(tukciwin)[-(1:3)],sep="_") 
  }
  
  #..............................
  return(tukciwin)
}
zdi_tukciwin_f<-function(tacekkart_tukciwin=NULL){
  #...................
  k<-0
for(l in names(tacekkart_tukciwin)){
    #....................................
    not_needed<-c("DATUM","UHRZEIT VON" ,"UHRZEIT BIS" )
    k<-k+1
    if(k>1){
      tukciwin_yezdin <- cbind(tukciwin_yezdin,tacekkart_tukciwin[[l]][rownames(tukciwin_yezdin),4:5])
    }else{
      tukciwin_yezdin<-tacekkart_tukciwin[[l]][,4:5]
    }
 
    #....................................
}
  tukciwin_yezdin<-tukciwin_yezdin[,!colnames(tukciwin_yezdin)=="Netzregelverbund_REBAP_LETZTE AENDERUNG"]
  #...................
  return(tukciwin_yezdin)
}
############################################
#
#  tukciwin heggi-d sizdeg tessali-d 
#
############################################
#. dacu-t: 

ghred_and_heggid_tikciwin_tuzrigin<-function(tasga_n_tukc = NULL,
                                             timnadin=c("50Hertz","TenneT","Amprion","TransnetBW","Netzregelverbund"),
                                             reserves=c("SRL","MRL","Austausch","Transfer","REBAP","SALDO"),
                                             tamurt="DE"){
  #..... acu i yella-n 
  ayen_yellan <- list.files(path=tasga_n_tukc,pattern ="*.csv|*.CSV")
  #.......
  tukc_tuzrigin<- list()
  for(tim in timnadin){
    tim_files <- ayen_yellan[grepl(pattern = tim,ayen_yellan)]
    if(length(tim_files)>0){
      for(res in reserves){
        tuck_tu3dilin<-vector()
        if(TRUE%in%grepl(res,tim_files)){
          res_files <-paste0(tasga_n_tukc,tim_files[grepl(res,tim_files)])
          tuck_tu3dilin <-vector()
          for(fi in res_files){
            temp_tukc      <- tail(read.csv2(fi,header = FALSE,check.names = FALSE),-3)
            colnames(temp_tukc)      <-as.matrix(temp_tukc[1,])
            tuck_tu3dilin  <- rbind(tuck_tu3dilin,temp_tukc[-1,])  
          }
          tukc_tuzrigin[[paste(tim,res,sep="_")]]<- tuck_tu3dilin
        }
        
      }
      
    }
    
  }
  #............
  temp_names<-names(tukc_tuzrigin)
  tukc_tuzrigin <- lapply(names(tukc_tuzrigin),function(x)tuck_restructure(tukciwin=tukc_tuzrigin[[x]],tuck_uzen=x,struct_mode=tamurt))
  names(tukc_tuzrigin)<-temp_names
  #............
  
  tukc_tuzrigin <- zdi_tukciwin_f(tacekkart_tukciwin=tukc_tuzrigin)
  #............
  return(tukc_tuzrigin)
  #............
}
##########################################################
tukc_tuzrigin_MOL_f <-function(tasga_n_tukc = NULL,
                                   timnadin=c("50Hertz","TenneT","Amprion","TransnetBW","Netzregelverbund"),
                                   reserves=c("SRL","MRL","Austausch"),
                                   tamurt="DE",tura = TRUE){
  #........................................................
  #..... acu i yella-n ....................................
  ############ download MOL data from a given url (regelleistung)
  if(tura == TRUE){
    akred_MOL_tukciwin(tasga_n_tukc)
  }
  #........................................................
  ayen_yellan <- list.files(path=paste0(tasga_n_tukc,"MOL"),pattern = "*.CSV|*.csv|*.xlsx")
  #........................................................
  MOL_tukc_tuzrigin<- list()
  mol_files <- paste0(tasga_n_tukc,"MOL/",ayen_yellan)
  #........................................................
  for(f_mol in mol_files){
    warning("only 2017 Data are used ... please extend")
    #......................................................
    tasetta<-tail(unlist(strsplit(f_mol,split="\\.")),1)
    if(tasetta=="csv"){
      mol_asseggas <- read.csv(file = f_mol,sep=";",header = TRUE,check.names = FALSE)
    }
    if(tasetta =="xlsx"){
      # stop("raju dagi")
      mol_asseggas <- readxl::read_excel(path = f_mol,sheet = 1)
      
      # mol_asseggas <- read.xlsx(file = f_mol,sheetIndex = 1,header = TRUE)
    }
    #......................................................
    reserve_isem<-as.matrix(unique(mol_asseggas$TYPE_OF_RESERVES))
    #......................................................
    if(reserve_isem=="aFRR")reserve_isem<-"SRL"
    if(reserve_isem=="mFRR")reserve_isem<-"MRL"
    MOL_tukc_tuzrigin[[reserve_isem]] <- mol_asseggas
  }
  #........................................................
  return(MOL_tukc_tuzrigin)
} 



quart_MOL<-function(quart=NULL,woche=NULL,MOL_vect=NULL,tura=FALSE){
  #...........
if(tura==FALSE){
  if(as.numeric(format(quart,"%H")) >= 08 & as.numeric(format(quart,"%H")) < 20){
    PRODUCT<-paste(woche,"HT",sep="_")
  }else{
    PRODUCT<-paste(woche,"NT",sep="_")
  }
}else{
  if(as.numeric(format(quart,"%H")) >= 00 & as.numeric(format(quart,"%H")) < 04){
    PRODUCT<-paste(woche,"00_04",sep="_")
  }
  if(as.numeric(format(quart,"%H")) >= 04 & as.numeric(format(quart,"%H")) < 08){
    PRODUCT<-paste(woche,"04_08",sep="_")
  }
  if(as.numeric(format(quart,"%H")) >= 08 & as.numeric(format(quart,"%H")) < 12){
    PRODUCT<-paste(woche,"08_12",sep="_")
  }
  if(as.numeric(format(quart,"%H")) >= 12 & as.numeric(format(quart,"%H")) < 16){
    PRODUCT<-paste(woche,"12_16",sep="_")
  }
  if(as.numeric(format(quart,"%H")) >= 16 & as.numeric(format(quart,"%H")) < 20){
    PRODUCT<-paste(woche,"16_20",sep="_")
  }
  if(as.numeric(format(quart,"%H")) >= 20 & as.numeric(format(quart,"%H")) < 24){
    PRODUCT<-paste(woche,"20_24",sep="_")
  }
}
  #...........
  return(MOL_vect[PRODUCT,])
  #...........
}

matching_product <- function(tukc_tuzrigin= NULL , MOL_tukciwin = NULL,reserve = "SRL",tura=FALSE,ukud_format=NULL){
  #...............
  if(tura==FALSE){
    arvu3i <- parsedate::parse_date(rownames(tukc_tuzrigin))
  }else{
    arvu3i<-as.POSIXct(rownames(tukc_tuzrigin),tz="CET")
  }
  #...............
  if(reserve =="SRL"){
    MOL_vect <- convert_MOL_into_vect_main(MOL=MOL_tukciwin , reserve ="SRL",tura = tura,ukud_format = ukud_format)
    
    if(tura==FALSE){
      srl_woche<-unique(as.Date(MOL_tukciwin$SRL$DATE_FROM,"%d.%m.%Y",tz="CET"))
      arvu3i_woche<- base::as.Date(srl_woche[findInterval(arvu3i,srl_woche)],tz="CET")
    }else{
      #...... Daily SRL not weekly anymore.....
      srl_woche<-unique(as.Date(MOL_tukciwin$SRL$DATE_FROM))
      arvu3i_woche<-base::as.Date(substr(arvu3i,1,10),tz="CET")
      #........................................
    }
    srl_woche<-  sapply(1:length(arvu3i), function(x)quart_MOL(quart=arvu3i[x],woche=paste(arvu3i_woche[x]),MOL_vect=MOL_vect,tura=tura))
    srl_woche<-t(srl_woche)
    rownames(srl_woche)<-paste(arvu3i)
  }
  #................................................
  rownames(tukc_tuzrigin)<-paste(arvu3i)
  srl_woche<-cbind(tukc_tuzrigin,srl_woche)
  #................................................
   srl_woche<-apply(srl_woche,2,ekkes_yiwen_u3eqquc)
   srl_woche <- srl_woche[,apply(srl_woche,2,function(x)length(na.omit(x))!=0)]
   rownames(srl_woche)<-paste(arvu3i)
   # 
   bad_indx <- colnames(srl_woche)!="Netzregelverbund_SALDO_LETZTE AENDERUNG"
   srl_woche<-srl_woche[,bad_indx]
   srl_woche<-na.omit(srl_woche)
  #................................................
  return(srl_woche)
}
#..............................................
convert_MOL_into_vect_srl <- function(woche=NULL,srl_wochen=NULL,MOL = NULL,MOL_levels= NULL){
  if(is.null(MOL_levels))MOL_levels <- c(0,15,25,50,75,100,150,200,270,350,480,600,750,1000,1200,1500)
  #...........................
  MOL_woche <- MOL[srl_wochen == woche,]
  #........ negative price when the Grid has to pay the provider for setting the control 
  temp_vect <- as.numeric(MOL_woche[MOL_woche[,"ENERGY_PRICE_PAYMENT_DIRECTION"]=="PROVIDER_TO_GRID","ENERGY_PRICE_[EUR/MWh]"])
  MOL_woche[MOL_woche[,"ENERGY_PRICE_PAYMENT_DIRECTION"]=="PROVIDER_TO_GRID","ENERGY_PRICE_[EUR/MWh]"]<- paste(- temp_vect)
  print("ghure- at tsu-d ma GRID_TO_PROVIDER negh winna nnidhen")
  
  #..............................
  
 
  #...............................................
  MOL_NEG_HT <-MOL_woche[MOL_woche[,"PRODUCT"]=="NEG_HT",]

  MOL_NEG_HT <-MOL_NEG_HT[order(as.numeric(MOL_NEG_HT[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  
  asurif <- cumsum(as.numeric(MOL_NEG_HT[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_NEG_HT <- MOL_NEG_HT[asurif,]

  #...............................................;
  
  MOL_POS_HT <-MOL_woche[MOL_woche[,"PRODUCT"]=="POS_HT",]
  MOL_POS_HT <-MOL_POS_HT[order(as.numeric(MOL_POS_HT[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  asurif <- cumsum(as.numeric(MOL_POS_HT[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  
  MOL_POS_HT <- MOL_POS_HT[asurif,]

  #................................................
  MOL_NEG_NT <-MOL_woche[MOL_woche[,"PRODUCT"]=="NEG_NT",]
  
  MOL_NEG_NT <-MOL_NEG_NT[order(as.numeric(MOL_NEG_NT[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]
  asurif <- cumsum(as.numeric(MOL_NEG_NT[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_NEG_NT <- MOL_NEG_NT[asurif,]
  

  #................................................
  MOL_POS_NT <-MOL_woche[MOL_woche[,"PRODUCT"]=="POS_NT",]

  MOL_POS_NT <-MOL_POS_NT[order(as.numeric(MOL_POS_NT[,"ENERGY_PRICE_[EUR/MWh]"])),c("ENERGY_PRICE_[EUR/MWh]","OFFERED_CAPACITY_[MW]")]

  asurif <- cumsum(as.numeric(MOL_POS_NT[,"OFFERED_CAPACITY_[MW]"]))
  asurif <- 1 + sapply(MOL_levels,function(x)findInterval(x,asurif))
  MOL_POS_NT <- MOL_POS_NT[asurif,]
  
  #.............put all together...................
  
  
  #................................................
  MOL_vect<-rbind(c(MOL_POS_NT[,1],MOL_NEG_NT[,1]),c(MOL_POS_HT[,1],MOL_NEG_HT[,1]))
  #................................................
  #................................................
  new_names <- c(paste0("P_POS_",1:nrow(MOL_POS_NT)),paste0("P_NEG_",1:nrow(MOL_POS_NT)))
  colnames(MOL_vect)<-new_names
  rownames(MOL_vect)<-paste0(as.Date(woche),c("_NT","_HT"))
  
  return(MOL_vect)
}
#...............................................
convert_MOL_into_vect_main <- function(MOL=NULL , reserve ="SRL",tura=FALSE,ukud_format = NULL){
  if(reserve =="SRL"){
   # srl_wochen<-parsedate::parse_date(as.matrix(MOL$SRL$DATE_FROM))
    if(is.null(ukud_format)){
      srl_wochen<- base::as.Date(MOL$SRL$DATE_FROM,tz="CET")
    }else{
      srl_wochen<- base::as.Date(MOL$SRL$DATE_FROM,ukud_format,tz="CET") 
    }
    
    #....
    MOL$SRL <-as.matrix(MOL$SRL)
    MOL$SRL<-gsub(",","\\.",MOL$SRL)
    if(tura==FALSE){
      MOL_vect<-  lapply(unique(srl_wochen), function(x)convert_MOL_into_vect_srl(woche=x,srl_wochen=srl_wochen,MOL=MOL$SRL))
    }else{
      MOL_vect<-  lapply(unique(srl_wochen), function(x)convert_MOL_into_vect_srl_amaynut(woche=x,srl_wochen=srl_wochen,MOL=MOL$SRL))
    }
    
    MOL_vect<-do.call(rbind,MOL_vect)
    #.... how to align different MOLs
    return(MOL_vect)
    #....
    
    
  }
}

##############################################
#
# FUNCTION: Match quartely values with corresponding product
#
##############################################
#.......................................................... 
MOL_matching_f <-function(MOL_tukc_tuzrigin = NULL , ssuq_tukc_tuzrigin = NULL ){
  #.................................
  aa<- sapply(rownames(ssuq_tukc_tuzrigin),function(x)matching_product(arvu3i= x , MOL_tukciwin = MOL_tukc_tuzrigin,reserve = "SRL"))
  #.................................
}

ekkes_a3eqquc<-function(y=NULL){
  y1<-strsplit(y,split="\\.")[[1]]
  if(length(y1) == 3){
    return(paste(y1[1:2],collapse = ""))
  }
  return(y)
  
}
#.............................;
ekkes_yiwen_u3eqquc <- function(x = NULL){
 return(as.numeric(sapply(x,function(y)ekkes_a3eqquc(y=y))))

}
# 
# 
# # # # #..............................
# #
# 
#   tasga_n_tukc <- "C:/Users/user/Documents/Saldae_Analytics/aqbur/Shiny App/data template/reBAP/"
# # #
#   ssuq_tukc_tuzrigin<-ghred_and_heggid_tikciwin_tuzrigin( tasga_n_tukc = tasga_n_tukc )
# # #
#   MOL_tukciwin <- tukc_tuzrigin_MOL_f(tasga_n_tukc =tasga_n_tukc ,tura = FALSE)
#   ML_reBAP_tukciwin_ilindi<- matching_product(tukc_tuzrigin= ssuq_tukc_tuzrigin , MOL_tukciwin = MOL_tukciwin,reserve = "SRL",tura = FALSE,ukud_format = "%d.%m.%Y")
# # #..................Cleaned up.......
# #
#  bad_indx <- colnames(ML_reBAP_tukciwin_ilindi)!="Netzregelverbund_SALDO_LETZTE AENDERUNG"
#  ML_reBAP_tukciwin_ilindi<-ML_reBAP_tukciwin_ilindi[,bad_indx]
# # #...................................
#  library("randomForest")
# 
# train_indx <- 1:16000
# 
# output_indx <- grepl("Netzregelverbund_REBAP_QUAL.",colnames(ML_reBAP_tukciwin))
# reBAP_rf_model <-randomForest::randomForest( ML_reBAP_tukciwin[train_indx,!output_indx], ML_reBAP_tukciwin[train_indx,output_indx],ntree = 500)
# 
# test_perf <- predict(object = reBAP_rf_model,newdata= ML_reBAP_tukciwin[-train_indx,!output_indx])
# test_perf <- cbind(tes_label = ML_reBAP_tukciwin[-train_indx,output_indx] ,  fcast = test_perf )
# matplot(test_perf,type="l")
# # #.....................................................
# # #.....................................................
# View(test_perf)
# # #.....................................................
# saveRDS(reBAP_rf_model,"C:/Users/user/Documents/Saldae_Analytics/Saldae-Analytics-Platform/data_template/reBAP_model_full.RDS")
