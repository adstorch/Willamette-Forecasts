# Call required packages --------------------------------------------------
library(ggplot2)
library(shiny)
library(plotly)
library(shinyjs)
library(mailtoR)

# define variables --------------------------------------------------------
curr_year <- 2022

# read data ---------------------------------------------------------------
dat <- readRDS(
  paste0(
    "./www/",
    curr_year,
    "willClackComb.shiny.dat.rds"
  )
)

# Define UI for application -----------------------------------------------
ui = fluidPage(
  
  titlePanel(
    div(
      column(
        width = 1.0,
        tags$a(
          href="https://www.dfw.state.or.us/",
          target="_blank",
          tags$img(src="odfwLogo.v.2.png",
                   height="100")
        )
      ),
      column(
        align = "left",
        width = 11.0,
        h2(
          'Willamette River spring-summer Chinook Salmon Returns/Forecasts',
          style="margin-bottom: 15 px;margin-top: 15 px;"
        ),
        h5(
          em(
            paste0(
              'Last updated: ',
              format(Sys.time(),
                     format = "%B %d, %Y %H:%M:%S"
              )
            )
          ),
          style="margin: 20px;margin-bottom: 40px;"
        )
      )
      
    ),
    windowTitle = "Willamette Runs"
  ),
  
  tags$head(
    tags$style(
      HTML(
        "hr {border-top: 1px solid #000000;}"
      )
    )
  ),
  
  tabsetPanel(
    tabPanel(
      "Data",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          style = "position:fixed;width:32%;",
          sliderInput(
            inputId = "year",
            label = "Return Year Range",
            min = min(dat$ReturnYr),
            max = max(dat$ReturnYr),
            value = c(min(dat$ReturnYr),
                      max(dat$ReturnYr)
            ),
            sep = "",
            step = 1
          ),
          downloadLink(
            "download", "Download selected data"
          )
        ),
        #         br(),
        #         br(),
        #         br(),
        #         hr(),
        #         h5("Tips:"),
        #         tags$head(
        #           tags$style(HTML("
        # li {
        # font-size: 14px;
        # 
        # }
        # li span {
        # font-size: 14px;
        # }
        # ul span{
        # font-size: 14px;
        # }
        # ul {
        # list-style-type: square;
        # }
        # 
        # "))
        #         ),
        #         tags$div(tags$ul(
        #           tags$li(tags$span("Use slider to select the range of data to display")),
        #           br(),
        #           tags$li(tags$span("Click 'Download selected data' to render a .csv file \n of the selected data")),
        #           br(),
        #           tags$li(tags$span("Scroll over points or smooth line in the figures to see X/Y coordinates")),
        #           br(),
        #           tags$li(tags$span("When the cursor is hovering over a figure, two icons should appear in \n the top right-hand corner.  Click on the camera icon to download an image of the figure"))
        #           ))),
        mainPanel(
          br(),
          plotlyOutput("TotalPlot"),
          br(),
          br(),
          plotlyOutput("AdultPlot"),
          br(),
          br(),
          plotlyOutput("WillRetAge3.plot"),
          br(),
          br(),
          plotlyOutput("WillRetAge4.plot"),
          br(),
          br(),
          plotlyOutput("WillRetAge5.plot"),
          br(),
          br(),
          plotlyOutput("WillRetAge6.plot")
        )
      )
    ),
    tabPanel("Documents", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 style = "position:fixed;width:32%;",
                 selectInput(inputId = "pdf_selection",
                             label ="Select Run/Forecast Year:",
                             list("2022/2023",
                                  "2021/2022",
                                  "2020/2021",
                                  "2019/2020",
                                  "2018/2019",
                                  "2017/2018",
                                  "2016/2017",
                                  "2015/2016",
                                  "2014/2015",
                                  "2013/2014",
                                  "2012/2013",
                                  "2011/2012",
                                  "2010/2011",
                                  "2009/2010",
                                  "2008/2009",
                                  "2007/2008")
                 )),
               mainPanel(
                 uiOutput("pdf_viewer")
               )
             )
    ),
    tabPanel("Tutorial", fluid = TRUE,
             br(),
             tags$iframe(src = "Tutorial.mp4", type = "video/mp4", autoplay = FALSE, controls = TRUE,style="border: 3px solid #EEE; height:600px; width:100%",frameborder="0")
    ),
    tabPanel("Metadata", fluid = TRUE,
             br(),
             
             paste0(
               'Last updated: ',
               format(Sys.time(),
                      format = "%B %d, %Y %H:%M:%S"
               )
             ),
             br(),
             br(),
             "Direct questions to:",
             br(),
             "Adam Storch",
             br(),
             "17330 SE Evelyn St,",
             br(),
             "Clackamas, OR 97015",
             br(),
             mailtoR(email = "adam.j.storch@odfw.oregon.gov",
                     text = "adam.j.storch@odfw.oregon.gov",
                     subject = "Willamette spring-summer Chinook"),
             
             use_mailtoR()
             
    )
  )
)

# Define server logic -----------------------------------------------------
server <- shinyServer(function(input, output) {
  
  thedata <- reactive(dat[dat[,1] >= input$year[1] & dat[,1] <= input$year[2] ,])
  # thedata2 <- reactive(dat2[dat2[,1] >= input$year[1] & dat2[,1] <= input$year[2] ,])
  
  ## Total return
  output$TotalPlot <- renderPlotly({
    ### Filter by date
    dat <- dat[dat$ReturnYr >= input$year[1] & dat$ReturnYr <= input$year[2] ,]
    
    cols <- c("Return Estimates"="black",
              "Fit (Ret. Ests.)"="#00CCFF",
              "Prior Forecasts"="#E69F00",
              "Current Forecast"="#D55E00")
    
    ggplotly(
      ggplot() +
        theme_bw()+
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              axis.title.y = element_text(face = "bold", size = 17,vjust = 1),
              axis.title.x = element_text(face = "bold", size = 17,vjust = -1),
              axis.text.x = element_text(face = "bold",size = 15,color="black", vjust=0.1,angle = 45),
              axis.text.y = element_text(face = "bold",size = 15,color="black"),
              legend.title = element_blank(),
              legend.text=element_text(size=12),
              legend.position = "right",
              axis.ticks.length = unit(0.15, "cm"))+
        labs(title = "Total", y = "Number of Individuals", x = "Return Year") +
        theme(plot.title = element_text(hjust = 0.0,size = 17,face = "bold", color = "black")) +
        geom_point(data = dat,aes(ReturnYr,Total, color = "Return Estimates"), size = 3.5)+
        geom_point(data = dat,aes(ReturnYr,predTotal, color = "Prior Forecasts"), size = 3.5,shape=17)+
        geom_line(data = dat,aes(ReturnYr,Total,color ="Fit (Ret. Ests.)"),stat ="smooth",method = "loess",formula = y ~ x, se = FALSE, linewidth = 1.0,alpha = 0.5)+
        geom_point(data = dat,aes(ReturnYr,predTotalCurr, color = "Current Forecast"), size = 3.5,shape=23)+
        # geom_point(data = dat,aes(ReturnYr,Total, color = "Return Ests."), size = 3.5)+
        scale_colour_manual(name="",values=cols)+
        scale_y_continuous(limits=c(0,max(max(dat$Total,na.rm = TRUE),
                                          max(dat$predTotal,na.rm = TRUE),
                                          max(dat$predTotalCurr,na.rm = TRUE))),
                           breaks = seq(0,max(max(dat$Total,na.rm = TRUE),
                                              max(dat$predTotal,na.rm = TRUE),
                                              max(dat$predTotalCurr,na.rm = TRUE)),10000), labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
        scale_x_continuous(limits=c(min(dat$ReturnYr),max(dat$ReturnYr)),breaks = seq(min(dat$ReturnYr),max(dat$ReturnYr),3))
    ) %>% 
      config(modeBarButtonsToRemove = c("zoomIn2d",
                                        "zoomOut2d",
                                        "pan2d",
                                        "zoom2d",
                                        "select2d",
                                        "lasso2d",
                                        "autoScale2d"),
             # "resetScale2d",
             # "hoverClosestCartesian",
             # "hoverCompareCartesian"),
             displaylogo = FALSE,
             toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                         filename= 'will_tot_ret',
                                         height= 700,
                                         width= 1100,
                                         scale= 1 ))
    
  })
  ## Adult return
  output$AdultPlot <- renderPlotly({
    ### Filter by date
    dat <- dat[dat$ReturnYr >= input$year[1] & dat$ReturnYr <= input$year[2] ,]
    
    cols <- c("Return Estimates"="black",
              "Fit (Ret. Ests.)"="#00CCFF",
              "Prior Forecasts"="#E69F00",
              "Current Forecast"="#D55E00")
    
    ggplotly(
      ggplot() +
        theme_bw()+
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              axis.title.y = element_text(face = "bold", size = 17,vjust = 1),
              axis.title.x = element_text(face = "bold", size = 17,vjust = -1),
              axis.text.x = element_text(face = "bold",size = 15,color="black", vjust=0.1,angle = 45),
              axis.text.y = element_text(face = "bold",size = 15,color="black"),
              legend.title = element_blank(),
              legend.text=element_text(size=12),
              legend.position = "right",
              axis.ticks.length = unit(0.15, "cm"))+
        labs(title = "Adults", y = "Number of Individuals", x = "Return Year") +
        theme(plot.title = element_text(hjust = 0.0,size = 17,face = "bold", color = "black")) +
        geom_point(data = dat,aes(ReturnYr,Adults,color="Return Estimates"), size = 3.5)+
        geom_point(data = dat,aes(ReturnYr,predAdults, color = "Prior Forecasts"), size = 3.5,shape=17)+
        geom_line(data = dat,aes(ReturnYr,Adults,color = "Fit (Ret. Ests.)"),stat ="smooth",method = "loess",formula = y ~ x, se = FALSE, linewidth = 1.0,alpha = 0.5)+
        geom_point(data = dat,aes(ReturnYr,predAdultsCurr, color = "Current Forecast"), size = 3.5,shape=23)+
        # geom_point(data = dat,aes(ReturnYr,Total,color="Return Ests."), size = 3.5)+
        scale_colour_manual(name="",values=cols)+
        scale_y_continuous(limits=c(0,max(max(dat$Adults,na.rm = TRUE),
                                          max(dat$predAdults,na.rm = TRUE),
                                          max(dat$predAdultsCurr,na.rm = TRUE))),
                           breaks = seq(0,max(max(dat$Adults,na.rm = TRUE),
                                              max(dat$predAdults,na.rm = TRUE),
                                              max(dat$predAdultsCurr,na.rm = TRUE)),10000), labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
        scale_x_continuous(limits=c(min(dat$ReturnYr),max(dat$ReturnYr)),breaks = seq(min(dat$ReturnYr),max(dat$ReturnYr),3))
    ) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d",
                                        "zoomOut2d",
                                        "pan2d",
                                        "zoom2d",
                                        "select2d",
                                        "lasso2d",
                                        "autoScale2d"),
             # "resetScale2d",
             # "hoverClosestCartesian",
             # "hoverCompareCartesian"),
             displaylogo = FALSE,
             toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                         filename= 'will_adult_ret',
                                         height= 700,
                                         width= 1100,
                                         scale= 1 ))
    
  })
  
  ## Age-3 return
  output$WillRetAge3.plot <- renderPlotly({
    ### Filter by date
    dat <- dat[dat$ReturnYr >= input$year[1] & dat$ReturnYr <= input$year[2] ,]
    
    cols <- c("Return Estimates"="black",
              "Fit (Ret. Ests.)"="#00CCFF",
              "Prior Forecasts"="#E69F00",
              "Current Forecast"="#D55E00")
    
    ggplotly(
      ggplot() +
        theme_bw()+
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              axis.title.y = element_text(face = "bold", size = 17,vjust = 1),
              axis.title.x = element_text(face = "bold", size = 17,vjust = -1),
              axis.text.x = element_text(face = "bold",size = 15,color="black", vjust=0.1,angle = 45),
              axis.text.y = element_text(face = "bold",size = 15,color="black"),
              legend.title = element_blank(),
              legend.text=element_text(size=12),
              legend.position = "right",
              axis.ticks.length = unit(0.15, "cm"))+
        labs(title = "Age-3",y = "Number of Individuals", x = "Return Year") +
        theme(plot.title = element_text(hjust = 0.0,size = 17,face = "bold", color = "black")) +
        geom_point(data = dat,aes(ReturnYr,Age3,color="Return Estimates"), size = 3.5)+
        geom_point(data = dat,aes(ReturnYr,predAge3, color = "Prior Forecasts"), size = 3.5,shape=17)+
        geom_line(data = dat,aes(ReturnYr,Age3,color = "Fit (Ret. Ests.)"),stat ="smooth",method = "loess",formula = y ~ x, se = FALSE, linewidth = 1.0,alpha = 0.5)+
        geom_point(data = dat,aes(ReturnYr,predAge3Curr, color = "Current Forecast"), size = 3.5,shape=23)+
        # geom_point(data = dat,aes(ReturnYr,Age3,color="Return Ests."), size = 3.5)+
        scale_colour_manual(name="",values=cols)+
        scale_colour_manual(name="",values=cols)+
        scale_y_continuous(limits=c(0,max(max(dat$Age3,na.rm = TRUE),
                                          max(dat$predAge3,na.rm = TRUE),
                                          max(dat$predAge3Curr,na.rm = TRUE))),
                           breaks = seq(0,max(max(dat$Age3,na.rm = TRUE),
                                              max(dat$predAge3,na.rm = TRUE),
                                              max(dat$predAge3Curr,na.rm = TRUE)),500), labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
        scale_x_continuous(limits=c(min(dat$ReturnYr),max(dat$ReturnYr)),breaks = seq(min(dat$ReturnYr),max(dat$ReturnYr),3))
    ) %>% config(modeBarButtonsToRemove = c("zoomIn2d",
                                            "zoomOut2d",
                                            "pan2d",
                                            "zoom2d",
                                            "select2d",
                                            "lasso2d",
                                            "autoScale2d"),
                 # "resetScale2d",
                 # "hoverClosestCartesian",
                 # "hoverCompareCartesian"),
                 displaylogo = FALSE,
                 # displayModeBar = TRUE,
                 toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                             filename= 'will_age3_ret',
                                             height= 700,
                                             width= 1100,
                                             scale= 1 ))
  })
  
  ## Age-4 return
  output$WillRetAge4.plot <- renderPlotly({
    ### Filter by date
    dat <- dat[dat$ReturnYr >= input$year[1] & dat$ReturnYr <= input$year[2] ,]
    
    cols <- c("Return Estimates"="black",
              "Fit (Ret. Ests.)"="#00CCFF",
              "Prior Forecasts"="#E69F00",
              "Current Forecast"="#D55E00")
    
    ggplotly(
      ggplot() +
        theme_bw()+
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              axis.title.y = element_text(face = "bold", size = 17,vjust = 1),
              axis.title.x = element_text(face = "bold", size = 17,vjust = -1),
              axis.text.x = element_text(face = "bold",size = 15,color="black", vjust=0.1,angle = 45),
              axis.text.y = element_text(face = "bold",size = 15,color="black"),
              legend.title = element_blank(),
              legend.text=element_text(size=12),
              legend.position = "right",
              axis.ticks.length = unit(0.15, "cm"))+
        labs(title = "Age-4",y = "Number of Individuals", x = "Return Year") +
        theme(plot.title = element_text(hjust = 0.0,size = 17,face = "bold", color = "black")) +
        geom_point(data = dat,aes(ReturnYr,Age4, color = "Return Estimates"), size = 3.5)+
        geom_point(data = dat,aes(ReturnYr,predAge4, color = "Prior Forecasts"), size = 3.5,shape=17)+
        geom_line(data = dat,aes(ReturnYr,Age4,color = "Fit (Ret. Ests.)"),stat ="smooth",method = "loess",formula = y ~ x, se = FALSE, linewidth = 1.0,alpha = 0.5)+
        geom_point(data = dat,aes(ReturnYr,predAge4Curr, color = "Current Forecast"), size = 3.5,shape=23)+
        # geom_point(data = dat,aes(ReturnYr,Age4, color = "Return Ests."), size = 3.5)+
        scale_colour_manual(name="",values=cols)+
        scale_y_continuous(limits=c(0,max(max(dat$Age4,na.rm = TRUE),
                                          max(dat$predAge4,na.rm = TRUE),
                                          max(dat$predAge4Curr,na.rm = TRUE))),
                           breaks = seq(0,max(max(dat$Age4,na.rm = TRUE),
                                              max(dat$predAge4,na.rm = TRUE),
                                              max(dat$predAge4Curr,na.rm = TRUE)),10000), labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
        scale_x_continuous(limits=c(min(dat$ReturnYr),max(dat$ReturnYr)),breaks = seq(min(dat$ReturnYr),max(dat$ReturnYr),3))
    ) %>% config(modeBarButtonsToRemove = c("zoomIn2d",
                                            "zoomOut2d",
                                            "pan2d",
                                            "zoom2d",
                                            "select2d",
                                            "lasso2d",
                                            "autoScale2d"),
                 # "resetScale2d",
                 # "hoverClosestCartesian",
                 # "hoverCompareCartesian"),
                 displaylogo = FALSE,
                 # displayModeBar = TRUE,
                 toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                             filename= 'will_age4_ret',
                                             height= 700,
                                             width= 1100,
                                             scale= 1 ))
  })
  
  ## Age-5 return
  output$WillRetAge5.plot <- renderPlotly({
    ### Filter by date
    dat <- dat[dat$ReturnYr >= input$year[1] & dat$ReturnYr <= input$year[2] ,]
    
    cols <- c("Return Estimates"="black",
              "Fit (Ret. Ests.)"="#00CCFF",
              "Prior Forecasts"="#E69F00",
              "Current Forecast"="#D55E00")
    
    ggplotly(
      ggplot() +
        theme_bw()+
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              axis.title.y = element_text(face = "bold", size = 17,vjust = 1),
              axis.title.x = element_text(face = "bold", size = 17,vjust = -1),
              axis.text.x = element_text(face = "bold",size = 15,color="black", vjust=0.1,angle = 45),
              axis.text.y = element_text(face = "bold",size = 15,color="black"),
              legend.title = element_blank(),
              legend.text=element_text(size=12),
              legend.position = "right",
              axis.ticks.length = unit(0.15, "cm"))+
        labs(title = "Age-5",y = "Number of Individuals", x = "Return Year") +
        theme(plot.title = element_text(hjust = 0.0,size = 17,face = "bold", color = "black")) +
        geom_point(data = dat,aes(ReturnYr,Age5, color = "Return Estimates"), size = 3.5)+
        geom_point(data = dat,aes(ReturnYr,predAge5, color = "Prior Forecasts"), size = 3.5,shape=17)+
        geom_line(data = dat,aes(ReturnYr,Age5,color = "Fit (Ret. Ests.)"),stat ="smooth",method = "loess",formula = y ~ x, se = FALSE, linewidth = 1.0,alpha = 0.5)+
        geom_point(data = dat,aes(ReturnYr,predAge5Curr, color = "Current Forecast"), size = 3.5,shape=23)+
        # geom_point(data = dat,aes(ReturnYr,Age5, color = "Return Ests."), size = 3.5)+
        scale_colour_manual(name="",values=cols)+
        scale_y_continuous(limits=c(0,max(max(dat$Age5,na.rm = TRUE),
                                          max(dat$predAge5,na.rm = TRUE),
                                          max(dat$predAge5Curr,na.rm = TRUE))),
                           breaks = seq(0,max(max(dat$Age5,na.rm = TRUE),
                                              max(dat$predAge5,na.rm = TRUE),
                                              max(dat$predAge5Curr,na.rm = TRUE)),10000), labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
        scale_x_continuous(limits=c(min(dat$ReturnYr),max(dat$ReturnYr)),breaks = seq(min(dat$ReturnYr),max(dat$ReturnYr),3))
    ) %>% config(modeBarButtonsToRemove = c("zoomIn2d",
                                            "zoomOut2d",
                                            "pan2d",
                                            "zoom2d",
                                            "select2d",
                                            "lasso2d",
                                            "autoScale2d"),
                 # "resetScale2d",
                 # "hoverClosestCartesian",
                 # "hoverCompareCartesian"),
                 displaylogo = FALSE,
                 # displayModeBar = TRUE,this is a test
                 toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                             height= 700,
                                             width= 1100,
                                             scale= 1 ))
  })
  
  ## Age-6 return
  output$WillRetAge6.plot <- renderPlotly({
    ### Filter by date
    dat <- dat[dat$ReturnYr >= input$year[1] & dat$ReturnYr <= input$year[2] ,]
    
    cols <- c("Return Estimates"="black",
              "Fit (Ret. Ests.)"="#00CCFF",
              "Prior Forecasts"="#E69F00",
              "Current Forecast"="#D55E00")
    
    ggplotly(
      ggplot() +
        theme_bw()+
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              axis.title.y = element_text(face = "bold", size = 17,vjust = 1),
              axis.title.x = element_text(face = "bold", size = 17,vjust = -1),
              axis.text.x = element_text(face = "bold",size = 15,color="black", vjust=0.1,angle = 45),
              axis.text.y = element_text(face = "bold",size = 15,color="black"),
              legend.title = element_blank(),
              legend.text=element_text(size=12),
              legend.position = "right",
              axis.ticks.length = unit(0.15, "cm"))+
        labs(title = "Age-6",y = "Number of Individuals", x = "Return Year") +
        theme(plot.title = element_text(hjust = 0.0,size = 17,face = "bold", color = "black")) +
        geom_point(data = dat,aes(ReturnYr,Age6, color = "Return Estimates"), size = 3.5)+
        geom_point(data = dat,aes(ReturnYr,predAge6, color = "Prior Forecasts"), size = 3.5,shape=17)+
        geom_line(data = dat,aes(ReturnYr,Age6,color = "Fit (Ret. Ests.)"),stat ="smooth",method = "loess",formula = y ~ x, se = FALSE, linewidth = 1.0,alpha = 0.5)+
        geom_point(data = dat,aes(ReturnYr,predAge6Curr, color = "Current Forecast"), size = 3.5,shape=23)+
        # geom_point(data = dat,aes(ReturnYr,Age6, color = "Return Ests."), size = 3.5)+
        scale_colour_manual(name="",values=cols)+
        scale_y_continuous(limits=c(0,max(max(dat$Age6,na.rm = TRUE),
                                          max(dat$predAge6,na.rm = TRUE),
                                          max(dat$predAge6Curr,na.rm = TRUE))),
                           breaks = seq(0,max(max(dat$Age6,na.rm = TRUE),
                                              max(dat$predAge6,na.rm = TRUE),
                                              max(dat$predAge6Curr,na.rm = TRUE)),500), labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
        scale_x_continuous(limits=c(min(dat$ReturnYr),max(dat$ReturnYr)),breaks = seq(min(dat$ReturnYr),max(dat$ReturnYr),3))
    ) %>% config(modeBarButtonsToRemove = c("zoomIn2d",
                                            "zoomOut2d",
                                            "pan2d",
                                            "zoom2d",
                                            "select2d",
                                            "lasso2d",
                                            "autoScale2d"),
                 # "resetScale2d",
                 # "hoverClosestCartesian",
                 # "hoverCompareCartesian"),
                 displaylogo = FALSE,
                 # displayModeBar = TRUE,
                 toImageButtonOptions = list(format= 'png', # one of png, svg, jpeg, webp
                                             filename= 'will_age6_ret',
                                             height= 700,
                                             width= 1100,
                                             scale= 1 ))
  })
  
  ## Raw data table
  # output$table <- renderTable(digits = 0,
  #                             align = "c",
  #                             ### Filter by date
  #                             dat2 <- dat2[dat2[,1] >= input$year[1] & dat2[,1] <= input$year[2] ,],
  #                             colnames(dat2) <- c("Return Year",
  #                                                 "Age-3",
  #                                                 "Age-4",
  #                                                 "Age-5",
  #                                                 "Age-6",
  #                                                 "Adults","Total")                       
  # )
  
  ## Download raw data to .csv (Plots tab)
  output$download <- downloadHandler(
    filename = function(){paste("Willsp_suChnRet",format(Sys.time(),format = "%Y%m%d"),".csv", sep = "")}, 
    content = function(fname){
      write.csv(thedata(), fname, row.names = FALSE)
    }
  )
  ## Download raw data to .csv (Table tab)
  # output$download2 <- downloadHandler(
  #   filename = function(){paste("Willsp_suChnRet",format(Sys.time(),format = "%Y%m%d"),".csv", sep = "")}, 
  #   content = function(fname){
  #     write.csv(thedata2(), fname, row.names = FALSE)
  #   }
  # )
  output$pdf_viewer <- renderUI(tags$iframe(style="height:850px; width:100%",src=paste(substring(input$pdf_selection,6,10),".pdf",sep="")))
})

# Run the application 
shinyApp(ui = ui, server = server)
