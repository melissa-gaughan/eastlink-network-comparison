library(shiny)
library(shinydashboard)

ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = "August 2019"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard",tabName = "dashboard"),
        menuSubItem("By Team", tabName = "byteam"),#remove empty space
        menuItem("item2", tabName = "item2"), #give tab name
        menuItem("Raw Data",tabName = "rawdata") #give tab name
      )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  box(valueBoxOutput("totalrevenue"))
                )),
        tabItem(tabName = "byteam", #remove empty space
                h1("Numbers by Team")
        ),
        tabItem(tabName = "item2",
                h2("Numbers by Customer Tab")
        ),
        tabItem(tabName = "rawdata",
                h2("Raw Data Tab")
        )))))

server <- shinyServer(function(input,output){
  total.revenue <- sum(August$Revenue)
  output$totalrevenue <- renderValueBox({
    valueBox(
      formatC(total.revenue, format = "d", big.mark = ',')
      ,paste('Revenue:',total.revenue)
      ,icon = icon("stats", lib = 'glyphicon')
      ,color = "blue")
  })
})
shinyApp(ui, server)