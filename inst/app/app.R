library(shiny)
library(DT)
library(shinydashboard)
library(DBI)
library(RMySQL)
library(spsurvey)
library(foreign)
library(rhandsontable)
library(shinyBS)
library(tools)
library(shinyjs)
library(stringr)
library(dplyr)
library(data.table)

##test (crear listas usando pedregree book)
##test (crear listas usando passport book)
#test combinar listas pedgree book (n>1)
#test combinar listas passport book (n>1)
#test combinar listas de pedegree y passport book


tabNameS <- "generateList"
tabNameS2 <- "generateList"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  #fbmlist::server_generate(input, output, session, values = values)
  #fbmlist::server_managerlist(input, output, session, values = values)
  fbmlist::server_create(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Germoplasm List"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Management", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        #fbmlist::generate_ui(name = tabNameS),
                        fbmlist::create_ui(name = tabNameS)
                        #fbmlist::managerlist_ui(name = tabNameS2)
                        
                      )
                    )
)

shinyApp(ui = ui, server = server)

