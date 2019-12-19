
## app.R ##
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(sqldf)
library(RCurl)
packageVersion('plotly')


x <- getURL("https://raw.githubusercontent.com/yannisbelcaid/Projet_dashboards/master/full_trains.csv")
data_full_trains <- read.csv(text = x)

ui <- dashboardPage(
  dashboardHeader(title = "Shiny dashboard"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("SNCF Dashboard", tabName = "sncf_dashboard", icon = icon("dashboard")),
    menuItem("Flights Dashboard", tabName = "flights_dashboard", icon = icon("dashboard")), 
    menuSubItem("Airport traffic map", tabName = "map", icon = icon("fas fa-plane-departure"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sncf_dashboard",
        fluidRow(
          sidebarPanel(
            selectInput("year", "Year:", 
                        choices=unique(data_full_trains$year)),
            hr(),
            helpText("Data from SNCF.")
          ),
          valueBoxOutput("vbox_1"),
          valueBoxOutput("vbox_2"), 
          valueBoxOutput("vbox_3"),
          valueBoxOutput("vbox_4"),
          valueBoxOutput("vbox_5"),
          valueBoxOutput("vbox_6"), 
          valueBoxOutput("vbox_7"),
          valueBoxOutput("vbox_8"),
          valueBoxOutput("vbox_9")
          )
        ),   
        
      tabItem(tabName = "flights_dashboard", 
              fluidRow(
            
              )
      
      ),
      tabItem(tabName = "map", 
              fluidRow(
                
              
              )
              )
    )
  )
)

server <- function(input, output) { 
  
     output$vbox_1 <- renderValueBox({
       query1 <- data_full_trains %>%
         group_by(year) %>%
         summarise(trains_carried = sum(total_num_trips)-sum(num_of_canceled_trains))
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query1[x,"trains_carried"],"Numbers of trains carried out", icon = icon("train"))
      
     })
     output$vbox_2 <- renderValueBox({
       query2 <- sqldf("select year, sum(num_late_at_departure) as late_at_departure from data_full_trains group by year")
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query2[x,"late_at_departure"]," Number of trains late at departure", icon = icon("train"))
       
     })
     
     output$vbox_3 <- renderValueBox({
       query3 <- sqldf("select year, sum(num_arriving_late) as arrived_late from data_full_trains group by year")
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query3[x,"arrived_late"],"Number of trains wchich arrived late", icon = icon("train"))
       
     })
  
     output$vbox_4 <- renderValueBox({
       query4 <- sqldf("select year , round(avg(num_late_at_departure),3)as avg_num_late_at_departure from data_full_trains group by year")
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query4[x,"avg_num_late_at_departure"],"Average number of delayed trains at departure ", icon = icon("train"))
       
     })
     
     output$vbox_5 <- renderValueBox({
       query5 <- sqldf("select year , round(avg(num_arriving_late),3)as avg_num_arriving_late from data_full_trains group by year")
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query5[x,"avg_num_arriving_late"],"Average number of delayed trains at arrival ", icon = icon("train"))
       
     })
     
     output$vbox_6 <- renderValueBox({
       query6 <- sqldf("select year , round(avg(avg_delay_all_departing),3)as avg_delay_all_departing from data_full_trains group by year")
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query6[x,"avg_delay_all_departing"],"average departure delay time of all trains  (min) ", icon = icon("train"))
       
     })
     
     output$vbox_7 <- renderValueBox({
       query7 <- sqldf("select year , round(avg(avg_delay_all_arriving),3) as avg_delay_all_arriving from data_full_trains group by year")
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query7[x,"avg_delay_all_arriving"],"average arrival delay time of all trains  (min) ", icon = icon("train"))
       
     })
     
     output$vbox_8 <- renderValueBox({
       query8 <- sqldf("select year , round(avg(avg_delay_late_at_departure),3) as avg_delay_late_at_departure from data_full_trains group by year")
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query8[x,"avg_delay_late_at_departure"],"average departure delay time of delayed trains  (min) ", icon = icon("train"))
       
     })
     
     output$vbox_9 <- renderValueBox({
       query9 <- sqldf("select year , round(avg(avg_delay_late_on_arrival),3) as avg_delay_late_on_arrival from data_full_trains group by year")
       if (input$year == 2015){
         x <- 1
       }
       if (input$year == 2016){
         x <- 2
       }
       if (input$year == 2017){
         x <- 3
       }
       if (input$year == 2018){
         x <- 4
       }
       valueBox(query9[x,"avg_delay_late_on_arrival"],"average arrival delay time of delayed trains  (min) ", icon = icon("train"))
       
     })
     
  }

shinyApp(ui, server)