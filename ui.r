

dbHeader <- dashboardHeader(title = "Saldae-Analytics",
                            tags$li(a(href = 'http://www.saldaeanalytics.com/',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(class = "dropdown", style = "padding: 8px;",
                                    shinyauthr::logoutUI("logout")),
                            tags$li(a(href = 'http://www.saldaeanalytics.com/',
                                      img(src = 'saldae_logo2.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown")
)
dashboardPage(

  dbHeader,
  dashboardSidebar(
    collapsed = TRUE, sidebarMenuOutput("sidebar")
  ),
  dashboardBody(
    # includeCSS("https://bootswatch.com/4/superhero/bootstrap.css"),
    
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory")),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                          type="text/javascript"),
              includeScript("./ayen_nnidhen/returnClick.js")
    ),
    shinyauthr::loginUI("login"),
    tags$head(tags$style(HTML('.content-wrapper {
                                 background-color: #E0E0E0;
                                 }
                                 .well{background-color:#2E8B57
                            
                              '))),
    tabItems(
      #######################################RENEWABLES########################
      tabItem(tabName = "Renewables",
              tabsetPanel(type = "pills",id="renewables_id",
                          #.     Panel: 
                          tabPanel("Imbalance Price",icon = icon("balance-scale"),
                                   # Copy the line below to make a date selector 
                                   dateInput("date_reBap", label = NULL, value = Sys.Date()),
                                   mainPanel(width = 12,
                                             fluidPage(
                                               fluidRow( # Dsiplay summary
                                                 column(width = 8,
                                                        dygraphOutput("plot_reBAP")%>%withSpinner(color=spinner_color)
                                                 ),
                                                 #. plot correation matrix 
                                                 column(width=4,
                                                        DT::dataTableOutput("reBAP_tukc"))
                                                 
                                                 
                                                 #..........tfuk fluid row(market data)
                                                 ,  
                                                 DT::dataTableOutput("epexspot_data")
                                               )
                                               
                                             )
                                             
                                             
                                   )
                                   
                          )
              )
                   #        tabPanel("Market Data",icon = icon("chart-line"),
                   #                 fluidRow(
                   #                      plotlyOutput("expex_ID_price_plot")%>%withSpinner(color=spinner_color),
                   #                      plotOutput("expex_ID_volume_plot")%>%withSpinner(color=spinner_color)
                   #                 )
                   # ### the rest of your code
                   #          ),
                 #   tabPanel("Day Ahead Forecast",icon = icon("chart-line"),
                 #            fluidRow(
                 #              column(width = 8,
                 #              plotlyOutput("expex_DA_price_forecast_plot")%>%withSpinner(color = spinner_color)
                 #              ),
                 #              column(width = 4,
                 #              dataTableOutput("expex_DA_price_forecast_data")
                 #              )
                 #            )
                 # ) )
                 
      )
    )
  )
)