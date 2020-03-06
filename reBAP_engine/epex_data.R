
get_epex_spot_tukciwin <- function(ukud_tazwara = NULL, ukud_tagara = NULL,tamurt ="DE",EPEX_id_file=NULL){

  # devtools::install_github("wagnertimo/emarketcrawlR")
  if(is.null(ukud_tazwara))ukud_tazwara<-Sys.Date()
  if(is.null(ukud_tagara))ukud_tagara<-Sys.Date()
  setLogging(FALSE)
  
  # Get the 15min (default: hour data) trading price data in the given time period of the german cont. intra. at EPEX SPOT
  # prices <- getIntradayContinuousEPEXSPOT("2018-12-09", "2018-12-09", "15", "DE")
  # #.............................;;
  # head(prices)
  # 
  #.............................
  prices <- emarketcrawlR::getIntradayAuctionEPEXSPOT(ukud_tazwara,ukud_tagara)
  prices <- prices%>%dplyr::distinct(DateTime,.keep_all=TRUE)
  rownames(prices)<-prices$DateTime
  # # Get the 15min auction price data in the given time period of the german intra. auction market at EPEX SPOT. 
  # auctionPrices <- getIntradayAuctionEPEXSPOT("2018-12-09", "2018-12-09")
  prices<- prices%>%dplyr::mutate(hours = format(DateTime,"%Y-%m-%d %H"))
  prices_net<- prices%>%dplyr::group_by(hours)%>%dplyr::summarise(hourly_ID_prices= round(mean(Prices),2))
  prices<-prices%>%dplyr::left_join(prices_net,by="hours")
  rownames(prices)<-prices$DateTime
  saveRDS(prices,file = EPEX_id_file)
  return(prices)
}

reBAP_AEP3_f <-function(reBAP_aep2 = NULL, SALDO = NULL , ID_h_PRICE = NULL){
  #...........
  # if(abs(SALDO) <= 125  & !is.null(ID_h_PRICE)){
  #   if(SALDO>= 0){
  #     reBAP_aep2 <- min(abs(ID_h_PRICE) , abs(ID_h_PRICE + 100  + 150*(SALDO/125)))
  #   }else{
  #     reBAP_aep2 <- - min(abs(ID_h_PRICE) , abs(ID_h_PRICE - 100  - 150*(SALDO/125)))
  #   }
  # }
  # 
  if(SALDO>=0 & !is.na(ID_h_PRICE))return(max(reBAP_aep2,ID_h_PRICE))
  
  if(SALDO<0 & !is.na(ID_h_PRICE))return(min(reBAP_aep2,ID_h_PRICE))
  #...........
}

plot_ID_data_farid <- function (df) 
{

  df$Buy_Vol = df$Buy_Vol/1000
  df$Sell_Vol = df$Sell_Vol/1000
  g <- ggplot(df, aes(DateTime)) + geom_line(aes(y = Last, 
                                                 colour = "Last")) + geom_line(aes(y = High, colour = "High")) + 
    geom_line(aes(y = Low, colour = "Low")) + geom_vline(xintercept = as.numeric(as.POSIXct(paste(unique(format(df$DateTime,"%Y-%m-%d"))[1:length(unique(format(df$DateTime, "%Y-%m-%d")))],"00:00:00", sep = ""))), linetype = 4, color = "#9B9B9B") + 
    geom_hline(yintercept = 0, color = "#9B9B9B") + scale_color_manual(values = c("#FF9C52","#FFCCA7", "#9B9B9B")) + coord_cartesian(ylim = c(min(df$Low,na.rm = TRUE) - 5, max(df$High,na.rm = TRUE) + 5), expand = FALSE) + scale_x_datetime(date_breaks = "1 hour",labels = function(x) paste(format(x, "%H"))) + 
    xlab("") + ylab("Eur/MWh") + ggtitle("EPEX SPOT Continuous Intraday Prices") + 
    theme(plot.title = element_text(size = 16, hjust = 0.5,face = "bold", colour = "#FF9C52", vjust = -1)) + 
    guides(colour = FALSE) + theme(panel.background = element_blank(), 
                                   panel.grid.minor.y = element_line(colour = "#9B9B9B")) + 
    annotate("text", color = "#9B9B9B", x = as.POSIXct(paste(unique(format(df$DateTime,"%Y-%m-%d"))[1:length(unique(format(df$DateTime,"%Y-%m-%d")))], "12:00:00", sep = ""),tz ="CET"), y = min(df$Low,na.rm = TRUE), 
             label = unique(format(df$DateTime, "%a %d/%m/%y")))
  b <- ggplot(df, aes(x = DateTime, y = Buy_Vol)) + geom_bar(aes(fill = "Buy Volume"), 
                                                             stat = "identity", fill = "#9B9B9B", alpha = 0.5) + 
    geom_vline(xintercept = as.numeric(as.POSIXct(paste(unique(format(df$DateTime,"%Y-%m-%d"))[1:length(unique(format(df$DateTime,"%Y-%m-%d")))], "00:00:00", sep = ""),tz="CET")), linetype = 4, 
               color = "#9B9B9B") + geom_hline(yintercept = 0, 
                                               color = "#9B9B9B") + coord_cartesian(ylim = c(0, max(df$Buy_Vol,na.rm = TRUE) + 
                                                                                               0.1), expand = FALSE) + scale_x_datetime(date_breaks = "1 hour", 
                                                                                                                                        labels = function(x) paste(format(x, "%H", tz = "Europe/Berlin"))) + 
    xlab("") + ylab("GWh") + ggtitle("Volume Buy") + theme(plot.title = element_text(size = 16, 
                                                                                     hjust = 0.5, face = "bold", colour = "#FF9C52", vjust = -1)) + 
    theme(panel.background = element_blank(), panel.grid.minor.y = element_line(colour = "#9B9B9B")) + 
    annotate(geom = "text", label = paste("Total:", round(sum(df$Buy_Vol,na.rm = TRUE), 
                                                          3), "GWh"), color = "black", x = as.POSIXct(paste(unique(format(df$DateTime, 
                                                                                                                          "%Y-%m-%d"))[length(unique(format(df$DateTime, "%Y-%m-%d")))], 
                                                                                                            "20:00:00", sep = "")), y = max(df$Buy_Vol,na.rm = TRUE)) + annotate("text", 
                                                                                                                                                                                 color = "black", x = as.POSIXct(paste(unique(format(df$DateTime, 
                                                                                                                                                                                                                                     "%Y-%m-%d"))[1:length(unique(format(df$DateTime, 
                                                                                                                                                                                                                                                                         "%Y-%m-%d")))], "12:00:00", sep = "")), y = 0.1, 
                                                                                                                                                                                 label = unique(format(df$DateTime, "%a %d/%m/%y")))
  s <- ggplot(df, aes(x = DateTime, y = Sell_Vol)) + geom_bar(aes(fill = "Buy Volume"), 
                                                              stat = "identity", fill = "#9B9B9B", alpha = 0.5) + 
    geom_vline(xintercept = as.numeric(as.POSIXct(paste(unique(format(df$DateTime, 
                                                                      "%Y-%m-%d"))[1:length(unique(format(df$DateTime, 
                                                                                                          "%Y-%m-%d")))], "00:00:00", sep = ""))), linetype = 4, 
               color = "#9B9B9B") + geom_hline(yintercept = 0, 
                                               color = "#9B9B9B") + coord_cartesian(ylim = c(0, max(df$Sell_Vol,na.rm = TRUE) + 
                                                                                               0.1), expand = FALSE) + scale_x_datetime(date_breaks = "1 hour", 
                                                                                                                                        labels = function(x) paste(format(x, "%H", tz = "Europe/Berlin"))) + 
    xlab("") + ylab("GWh") + ggtitle("Volume Sell") + theme(plot.title = element_text(size = 16, 
                                                                                      hjust = 0.5, face = "bold", colour = "#FF9C52", vjust = -1)) + 
    theme(panel.background = element_blank(), panel.grid.minor.y = element_line(colour = "#9B9B9B")) + 
    annotate(geom = "text", label = paste("Total:", round(sum(df$Sell_Vol,na.rm = TRUE), 
                                                          3), "GWh"), color = "black", x = as.POSIXct(paste(unique(format(df$DateTime,"%Y-%m-%d"))[length(unique(format(df$DateTime, "%Y-%m-%d")))], 
                                                                                                            "20:00:00", sep = "")), y = max(df$Sell_Vol,na.rm = TRUE)) + 
    annotate("text", color = "black", x = as.POSIXct(paste(unique(format(df$DateTime, 
                                                                         "%Y-%m-%d"))[1:length(unique(format(df$DateTime, 
                                                                                                             "%Y-%m-%d")))], "12:00:00", sep = "")), y = 0.1, 
             label = unique(format(df$DateTime, "%a %d/%m/%y")))
  return(list(g,s,b))
}


# #.................................................
# yiwen_product <- prices$DateTime
# temp_indx<-1:length(yiwen_product)
# yiwen_product<-sapply(unique(yiwen_product),function(x)temp_indx[yiwen_product==x] )
# yiwen_product <- unlist(lapply(yiwen_product, function(x)x[[1]]))
# prices_net <- prices[yiwen_product,]
# #.....................................
# saveRDS(prices_net,"C:/Users/user/Documents/Assirem/Taftilt Analytics/performances assessement/ID_price_epex.RDS")
# #.....................................
# hours <- format(prices_net$DateTime,"%Y-%m-%d %H")
# prices_net<-cbind(prices_net,hourly_ID_prices = rep(NA,times = nrow(prices_net)))
# #.....................................
# for(h in unique(hours)){
#   #.............
#   prices_net[hours==h,"hourly_ID_prices"]<-round(mean( prices_net[hours==h,"Prices"]),2)
#   #.............
# }
# #.....................................
# #.....................................
# saveRDS(prices_net,"C:/Users/user/Documents/Assirem/Taftilt Analytics/performances assessement/ID_price_epex.RDS")
