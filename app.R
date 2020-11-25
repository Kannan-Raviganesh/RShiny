library(shiny)
library(shinydashboard)
library(DT)
library(tseries)
library(shiny)
library(shinyWidgets)
library(jpeg)
library(plotly)
library(forecast)
library(shinycssloaders)
library(dplyr)
library(ggplot2)


data=read.csv(file.choose())
example=read.csv(file.choose())
ui <- dashboardPage( skin = "red",
                     dashboardHeader(title = "GDP Dashboard"),
                     dashboardSidebar(sidebarMenu(
                         menuItem("Homepage", tabName = "Homepage", icon = icon("home")),
                         menuItem("Data", icon = icon("th"), tabName = "Data"),
                         menuItem("GDP", tabName = "Prediction", icon = icon("bar-chart-o")),
                         menuItem("Top 10 GDP", icon = icon("money"), tabName = "Top10GDP"),
                         menuItem("Prediction-Search", icon = icon("search"), tabName = "Search")
                         
                     )),
                     dashboardBody(
                         tabItems(
                             tabItem(
                                 tabName = "Homepage", 
                                 class = "active", 
                                 
                                 column(
                                     width = 12,
                                     
                                     fluidRow(
                                         
                                         # Welcome Box
                                         box(title = "Welcome to Our GDP Dashboard", status = "success", width = NULL, solidHeader = TRUE, collapsible = FALSE,
                                             
                                             
                                             # Description
                                             column(width = 12, 
                                                    h2("GDP"),
                                                    h4("Gross domestic product is a monetary measure of the market value of all the final goods and services produced in a specific time period. As a broad measure of overall domestic production, it functions as a comprehensive scorecard of a given country's economic health.  The calculation of a country's GDP encompasses all private and public consumption, government outlays, investments, additions to private inventories, paid-in construction costs, and the foreign balance of trade. GDP estimates are used to determine the economic performance of a whole country, and to make international comparisons Businesses can also use GDP as a guide to decide how best to expand or contract their production and other business activities. And investors even watch GDP since it provides a framework for investment decision-making. There are 4 types of GDP , however the GDP which is used over here is Real GDP.  the real GDP accounts for inflation, so it's the total value of all final goods and services when valued at constant prices. This way, you're able to compare GDP more accurately between years, without the conflicting variable of inflation rates. To do this, you choose the prices of a particular year and calculate how much goods and services are worth for the other years you're comparing it to so that inflation is no longer a factor. ", align="justify"),
                                                    h2("GDP for Economists and Investors" , align="justify"),
                                                    h4("GDP is an important measurement for economists and investors because it is a representation of economic production and growth. Both economic production and growth have a large impact on nearly everyone within a given economy. When the economy is healthy, there is usually a lower level of unemployment, and wages tend to increase as businesses hire more labor to meet the growing demand of the economy. Economists look at positive GDP growth between different time periods (usually year-to-year) to make an assessment of how much an economy is flourishing. Conversely, if there is negative GDP growth, it may be an indicator that an economy is in a recession, or approaching a recession or an economic downturn." , align="justify"),
                                                    h4("Investors pay attention to the GDP because a significant percentage change in the GDP-either up or down-can have a significant impact on the stock market. In general, a bad economy usually means lower earnings for companies and this can translate into lower stock prices. Investors may pay attention to positive and negative GDP growth when they are devising an investment strategy. However, it's important to note that because GDP is a measurement of the economy in the previous quarter or year, it is better used to help explain how economic growth and production have impacted your stocks and your investments in the past. It is not considered a helpful predictor of how the market will move in the future." , align="justify"),
                                                    h4("Our Dashboard depicts the GDP of various countries between the years 1994-2017. Our Dashboard also provides a feature through which you would be able to predict the GDP of a country for the year you desire." , align="justify")
                                                    
                                                    
                                             )
                                         )
                                     )
                                 )
                             ),
                             
                             
                             
                             
                             
                             
                             tabItem("Data",
                                     fluidPage(
                                         h1("View Info"),
                                         dataTableOutput("View_Data")
                                         
                                     )
                                     
                             ),
                             tabItem("Prediction",
                                     
                                     box( #background = "black",
                                       fluidPage(   titlePanel("GDP of a Country"),   
                                                      sidebarLayout(sidebarPanel(selectInput("C1", "Select a Country",choices = colnames(data[2:length(data)]), selected="select")),      
                                                                    mainPanel(plotlyOutput("distPlot1")))  
                                     ), width = 10
                                     )
                                     
                                  
                                     
                             ),
                             
                             tabItem("Top10GDP",
                                      
                                              br(),
                                     selectInput("C3", "Select Country",choices = colnames(data[2:length(data)])),
                                              mainPanel(
                                                dataTableOutput("Top_10_GDP")
                                              )
                                     
                             ),
                             
                             tabItem("Search",
                                     box( background = "black",
                                     mainPanel(
                                       h2("Arima Forcast"),
                                       selectInput("C2", "Select the Country",choices = colnames(data[2:length(data)])),
                                       fluidRow(
                                         sliderInput("obs", "Number of Years to be Predicted:",
                                                     min = 1, max = 30, value = 10,
                                         ),
                                         plotOutput("contents1")
                                       )
                                     ),
                                     mainPanel( position = "right",
                                         h2("Summary"),
                                        
                                         verbatimTextOutput("Summary")
                                         #plotOutput("box")
                                         
                                     ),
                                    
                                      
                                     mainPanel( style = "border-style: solid; border-color: black", position="right",
                                       #column=6,
                                       
                                       h2("Forecasts"),
                                       verbatimTextOutput("Forecasts"),
                                       width = 9
                                     )
                                     )
                                     
                             )       
                         )
                     )  
)   



server <- function(input, output) {
    
    output$distPlot1 <- renderPlotly({
      
       
      plot_ly(type = 'scatter',  mode = 'lines+markers',  x=data$Year, y=data[[input$C1]] ) 
      
    })
    
  
    
    
    output$Summary <- renderPrint({
        summary(data[[input$C2]])
    })
    
    output$Top_10_GDP <- renderDataTable({
     
      top_n(data[input$C3],n=10)     
    })
    
    
    output$contents1 <- renderPlot({
        arima1<-ts(data[input$C2])
        M2=auto.arima(arima1)
        M2F=forecast(M2,input$obs)
        plot(M2F,main="ARIMA Forecast")
        
    
        
    })
    
    
    output$Forecasts <- renderPrint({
      arima1<-ts(data[input$C2])
      M2=auto.arima(arima1)
      M2F=forecast(M2,input$obs)
      forecast(M2F)
    })
    
    
    
    
    
    output$View_Data=renderDataTable(example)
}


shinyApp(ui, server)
   





