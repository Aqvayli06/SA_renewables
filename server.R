server <- function(input, output, session){
  credentials <- callModule(shinyauthr::login, "login", 
                            data = readRDS(tasga_n_tufra),
                            user_col = user,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  # setup any tab pages you want after login here with uiOutputs
  output$sidebar <- renderMenu({
    req(credentials()$user_auth)
      sidebarMenu(
        # Custom CSS to hide the default logout panel
        tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
        # The dynamically-generated user panel
        uiOutput("userpanel"),
        menuItem("Renewables", icon = icon("solar-panel"), tabName = "Renewables",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("Visit-us", icon = icon("send"), 
                 href = "https://www.saldaeanalytics.com/#contact")
      )
  })
#--------------------------------------------------------------
  #. Market Data
  autoInvalidate  <- reactiveTimer(120000)
  autoInvalidate2 <- reactiveTimer(900000)
  autoInvalidate_long <- reactiveTimer(900000)
  reBAP_tukciwin <- reactive({
    req(credentials()$user_auth)
    autoInvalidate()
    tasga_n_tukc_tura <- "./data_template/"
    reBAP_tura <- reBAP_for_Shiny(tasga_n_tukc = tasga_n_tukc_tura,reBAP_model=reBAP_ml_model,tala="datacrawlR",ukud=input$date_reBap)
    return(reBAP_tura)
  })
 updated_time <-reactive({
   req(credentials()$user_auth)
   autoInvalidate2()
   tura<- lubridate::ceiling_date(Sys.time(), "15 minutes")
   if(base::as.Date(tura,tz="CET")>input$date_reBap){
     tura<- as.POSIXct(paste(input$date_reBap,"23:45"),tz="CET")
     
   }else{
     tura<-lubridate::ceiling_date(Sys.time(), "15 minutes") 
   }
   return(tura)
  })  
  #.....................
  epex_spot_plot <- reactive({
    req(credentials()$user_auth)
    autoInvalidate2()
    cont_ID_price <- getIntradayContinuousEPEXSPOT(base::as.Date(input$date_reBap,tz="CET"), base::as.Date(input$date_reBap,tz="CET"), "15", "DE")
    plot_ID_data_farid(cont_ID_price)
  })
  output$expex_ID_price_plot<-renderPlotly({
    req(credentials()$user_auth)
    ggplotly(epex_spot_plot()[[1]])
  })
  
  output$expex_ID_volume_plot<-renderPlot({
    req(credentials()$user_auth)
    grid.arrange(epex_spot_plot()[[2]], epex_spot_plot()[[3]], nrow = 2, heights = c(0.5,0.5))
  })
  #.....................
  output$epexspot_data <-renderDataTable({
    req(credentials()$user_auth)
    autoInvalidate2()
    EPEX_id_file<- paste0("./data_template/EPEX/ID_price_",format(Sys.Date(),"%Y_%m_%d"),".RDS")
    # epex_prices<-get_epex_spot_tukciwin(ukud_tazwara = base::as.Date(input$date_reBap,tz="CET"),ukud_tagara=base::as.Date(input$date_reBap,tz="CET"),tamurt = "DE",EPEX_id_file=EPEX_id_file)
    # epex_prices<-get_epex_spot_tukciwin(ukud_tazwara = base::as.Date(input$date_reBap,tz="CET"),ukud_tagara=base::as.Date(input$date_reBap,tz="CET"),tamurt = "DE",EPEX_id_file=EPEX_id_file)
    epex_prices <- akred_epex_spot_tisefka()
    DT::datatable(epex_prices,options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))%>%
      formatStyle('Price',  color = 'black', backgroundColor = 'grey', fontWeight = 'bold')
    
  })
#  time_slider_values<- reactive( {output$asbatri()})
  DA_price_forecast <- reactive({
    req(credentials()$user_auth)
    EP_DA_pred<-DA_ahead_prediction(ukud_tagara = Sys.Date()+1,tamurt="FR")
    return(EP_DA_pred)
  })
  output$expex_DA_price_forecast_data <- DT::renderDataTable({
    req(credentials()$user_auth)
    DA_price <- DA_price_forecast()
    DA_price<-DA_price[nrow(DA_price):1,]
    rownames(DA_price)<-DA_price$DateTime
    DA_price$DateTime<-NULL
    DT::datatable(DA_price, filter="top", selection="multiple", escape=FALSE, 
                  options = list(sDom  = '<"top">lrt<"bottom">ip',pageLength = 8, info = FALSE))
    
    return(DA_price)
  })
  output$expex_DA_price_forecast_plot <- renderPlotly({
    req(credentials()$user_auth)
    sekned_EPEX(ass_ar_zdat_forecast =DA_price_forecast())%>% plotly::layout(plot_bgcolor=background_colour,paper_bgcolor = background_colour)
  })
  

  output$reBAP_tukc<-DT::renderDataTable({
    req(credentials()$user_auth)
    autoInvalidate()
    
    reBAP_uttun <- reBAP_tukciwin()
    rownames(reBAP_uttun)<-substr(rownames(reBAP_uttun),start = 12,16)
    reBAP_uttun$reBAP_to_ID = sign(reBAP_uttun$reBAP-reBAP_uttun$IDh)
    DT::datatable(tail(reBAP_uttun,min(10,nrow(reBAP_uttun))), options = list(dom = 't',
      columnDefs = list(list(targets = 4, visible = FALSE))
    )) %>% formatStyle(
      'reBAP', 'reBAP_to_ID',
      backgroundColor = styleEqual(c(-1,0, 1), c('darkseagreen', 'grey','lightblue'))
    )
  })
 time_slider_values<-reactive({
   req(credentials()$user_auth)
   autoInvalidate()
   input$time_slider
   })
 
  output$plot_reBAP<-renderDygraph({
    req(credentials()$user_auth)
    autoInvalidate()
    taciwin_ukud<- time_slider_values()
    reBAP_tura<-reBAP_tukciwin()[,c("reBAP","IDh"),drop=F]
      dygraph(reBAP_tura,main = "reBAP price estimation DE/AT/LUX")%>% 
      dyRangeSelector(height = 20)%>% dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
     })
  #------------------------------Authetification------------------
  user_info <- reactive({credentials()$info})
  output$rnudmamaynut <- renderUI({
    print("aqli daha")
    if(user_info()$permissions=="admin"){
      observe({ 
        if (!is.null(input$adduser)) {
          if (input$adduser > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            rnu_amdan <- rnu_negh_ekkes_amdan(tasga_n_tufra = tasga_n_tufra,
                                              isem = Username,awal_uffir = Password)
            if(rnu_amdan) {
              shinyjs::toggle(id = "yerna", anim = TRUE, time = 1, animType = "fade")
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
            
          } 
        }    
      })
      loginpage
    }
  })
  # rnuamaynut_fun <- reactive({
  #  
  # })
  
  
  
  #......................TAGARA....................................
  # Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(stopApp)
}