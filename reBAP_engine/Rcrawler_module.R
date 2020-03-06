

# a <- "https://www.epexspot.com/en/market-data?market_area=DE&trading_date=2019-12-07&delivery_date=2019-12-07&underlying_year=&modality=Auction&sub_modality=Intraday&product=15&data_mode=table&period="


EPEX_ID_price_crawler <- function(trading_date = NULL,delivery_date=NULL,EPEX_id_file=NULL){
  ID_Price_url <- "https://www.epexspot.com/en/market-data?market_area=DE&trading_date="
  ID_Price_url <- paste0(ID_Price_url,trading_date)
  ID_Price_url <- paste0(ID_Price_url,"&delivery_date=")
  ID_Price_url <- paste0(ID_Price_url,delivery_date)
  tasetta <- "&underlying_year=&modality=Auction&sub_modality=Intraday&product=15&data_mode=table&period="
  ID_Price_url <- paste0(ID_Price_url,tasetta)
  ID_Price <-  xml2::read_html(ID_Price_url)
  epex_html_page <- rvest::html_nodes(ID_Price, 'table')
  prices <- as.data.frame(epex_html_page %>%  rvest::html_table(fill=TRUE))
  prices <- prices[,1:4]
  colnames(prices)<-prices[1,]
  prices <- prices[-1,]
  prices<- apply(prices,2,function(x)as.numeric(gsub(",","",x)))
  
  
  prices <- dplyr::tbl_df(prices)
  prices <- prices%>%dplyr::distinct(DateTime,.keep_all=TRUE)
  
  if(TRUE%in%grepl("Price",colnames(prices))){
    colnames(prices)[grepl("Price",colnames(prices))]<- "Price"
  }
  DateTime <- tail(seq(as.POSIXct(delivery_date-1,tz="CET"),length.out = 188,by="15 min"),96)
  prices <- prices%>%dplyr::mutate(hours = format(DateTime,"%Y-%m-%d %H"))
  prices_net <- prices%>%dplyr::mutate(hours = format(DateTime,"%Y-%m-%d %H"))%>%
                dplyr::group_by(hours)%>%
                dplyr::summarise(hourly_ID_prices = round(mean(`Price`),2))
  
  prices<-prices%>%dplyr::left_join(prices_net,by="hours")
  rownames(prices)<-DateTime
  prices <- data.frame(prices,check.names = FALSE)
  # saveRDS(prices,file = EPEX_id_file)
  
  return(prices)
}
# a <- EPEX_ID_price_crawler(trading_date = Sys.Date()-1,delivery_date = Sys.Date(),EPEX_id_file="kan akka.rds")
akred_epex_spot_tisefka <- function(trading_date = Sys.Date()-1,delivery_date = Sys.Date()){
  ID_Price_url <- "https://www.epexspot.com/en/market-data?market_area=DE&trading_date="
  ID_Price_url <- paste0(ID_Price_url,trading_date)
  ID_Price_url <- paste0(ID_Price_url,"&delivery_date=")
  ID_Price_url <- paste0(ID_Price_url,delivery_date)
  tasetta <- "&underlying_year=&modality=Auction&sub_modality=Intraday&product=15&data_mode=table&period="
  ID_Price_url <- paste0(ID_Price_url,tasetta)
  ID_Price <-  xml2::read_html(ID_Price_url)
  epex_html_page <- rvest::html_nodes(ID_Price, 'table')
  prices <- as.data.frame(epex_html_page %>%  rvest::html_table(fill=TRUE))
  prices <- prices[,1:4]
  colnames(prices)<-prices[1,]
  prices <- prices[-1,]
  prices<- apply(prices,2,function(x)as.numeric(gsub(",","",x)))
  
  if(TRUE%in%grepl("Price",colnames(prices))){
    colnames(prices)[grepl("Price",colnames(prices))]<- "Price"
  }
  prices <- dplyr::tbl_df(prices)
  prices <- prices%>%dplyr::distinct(DateTime,.keep_all=TRUE)
  DateTime <- tail(seq(as.POSIXct(delivery_date-1,tz="CET"),length.out = 188,by="15 min"),96)
  rownames(prices)<-DateTime
  prices <- data.frame(prices,check.names = FALSE)
  return(prices)
}