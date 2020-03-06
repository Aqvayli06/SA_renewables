##This should detect and install missing packages before loading them - hopefully!
library("shinydashboardPlus")
library("shinythemes")
library("dashboardthemes")
library("shinydashboard")
library("shinyWidgets")
library("lubridate")
library("scales")
library("forecast")
library("ggplot2")
library("plotly")
library("tseries")
library("dplyr")
library("devtools")
library("DT")
library("zoo")
library("shinycssloaders")
library("randomForest")
library("parsedate")
library("reshape2")
library("ggthemes")

library("dygraphs")
library("rmarketcrawlR")
library("emarketcrawlR")
library("entsoecrawlR")

#------------reportin ----------------------

library("sodium")
library("shinyauthr")
#............ reBAP modules............................
source("./reBAP_engine/tukciwin_module.R")
source("./reBAP_engine/reBAP_Shiny_tuqna.R")
source("./reBAP_engine/epex_data.R")
source("./reBAP_engine/regelleistung_data_crawlerR.R")
source("./reBAP_engine/Rcrawler_module.R")
source("./reBAP_engine/tuffirin_module.R")
source("./ayen_nnidhen/anekcum_udem.R")


reBAP_ml_model<-readRDS("./data_template/reBAP_model_full.RDS")

#.some design parameters
spinner.type <- 6
spinner_color<-"black"
background_colour <- "#E0E0E0"
Sys.setenv(TZ='CET')