library(shiny)
library(shinydashboard)
library(DT)
library(tseries)
library(shiny)
library(shinyWidgets)

data=read.csv("C:\\Users\\kanna\\Downloads\\Finaldataset.csv")

ui <- dashboardPage( skin = "red",
                     dashboardHeader(title = "GDP Dashboard"),
                     dashboardSidebar(sidebarMenu(
                       menuItem("Homepage", tabName = "Homepage", icon = icon("home")),
                       menuItem("Data", icon = icon("th"), tabName = "Data"),
                       menuItem("GDP Prediction", tabName = "Prediction", icon = icon("bar-chart-o")),
                       menuItem("Top 10 GDP", icon = icon("money"), tabName = "Top10GDP"),
                       menuItem("Search", icon = icon("search"), tabName = "Search")
                       
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
                               box(title = "Welcome to The FIFA 19 Dashboard", status = "success", width = NULL, solidHeader = TRUE, collapsible = FALSE,
                                   
                                   # Ronaldo FIFA 19 Image
                                   column(width = 8, tags$img(src="fifa.jpg", style="width: 100%; height: 100%;")),
                                   
                                   # Description
                                   column(width = 4, 
                                          tags$img(src="pl.png", style = "height: 70%; width: 35%"),
                                          tags$img(src="laliga.png", style = "height: 80%; width: 40%"),
                                          tags$img(src="bundesliga.png", style = "height: 10%; width: 20%"),
                                          tags$br(), tags$br(),
                                          tags$hr(),
                                          tags$img(src = "seriea.png", style = "height: 10%; width: 14%; margin-left: 5px;"),
                                          tags$img(src = "ligue1.png", style = "height: 20%; width: 14%; margin-left: 5px;"),
                                          tags$img(src = "superlig.png", style = "height: 20%; width: 14%; margin-left: 5px"),
                                          tags$img(src = "liganos.png", style = "height: 20%; width: 14%; margin-left: 5px;"),
                                          tags$img(src = "eredivisie.png", style = "height: 20%; width: 23%; margin-left: 5px;"),
                                          tags$br(), tags$br(),
                                          tags$hr(),
                                          fluidRow(column(width = 4), 
                                                   column(width = 4, tags$img(src="respectuefa.png", style = "height: 5%; width: 100%")),
                                                   column(width = 4)),
                                          tags$br(),
                                          tags$hr(),
                                          #tags$p("This Shiny Dashboard designed for comparison of the teams and players stats of Bundesliga, La Liga and Premier League in FIFA 19."),
                                          tags$p("The dashboard that I designed serves the purpose to enhance the data science experience in sports analytics with FIFA 19 dataset. After the updates, the dashboard now contains lots of analysis, EDAs  and visuals to compare leagues, teams and players. It also helps to discover new talents."),
                                          
                                          br(), 
                                          fluidRow(
                                            # Start Dashboard
                                            tags$p("Please click to start!", style = "margin-left: 40%;"),
                                            div(style = "margin-left: 33%;",actionButton("start", label = "Become A Legend!", color = "success"))
                                          )
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
                                 box(fluidPage(   titlePanel("GDP of a Country"),   
                                              sidebarLayout(sidebarPanel(selectInput("C1", "Select a Country",choices = colnames(data[2:length(data)]))),      
                                              mainPanel(plotOutput("distPlot")))  
                         )
                         )
                         ),
                         
                         tabItem("Top10GDP",
                                 fluidPage(
                                   h1("Top10GDP Info")
                                 ),
                                 box(
                                   selectInput("Year", "Year",choices = data$Year)),
                                 box(selectInput("Country", "Country",choices = colnames(data))
                                 )
                                 
                         ),
                         
                         tabItem("Search",
                                 mainPanel(
                                   h2("Summary"),
                                   selectInput("C2", "Select a Feature",choices = colnames(data)),
                                   verbatimTextOutput("Summary"),
                                   #plotOutput("box")
                                 )
                                  
                         )       
                       )
                     )  
)   



server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    plot(data$Year, data[[input$C1]], xlab="Year", ylab= "Country")
    
  })
  
  output$Summary <- renderPrint({
    #dataset <- datasetInput()
    summary(data[[input$C2]])
  })
  
  output$View_Data=renderDataTable(data)
}

shinyApp(ui, server)






