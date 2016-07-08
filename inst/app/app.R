library(shiny)
library(DT)
library(shinydashboard)
library(DBI)
library(RMySQL)
library(spsurvey)
library(foreign)

tabNameS <- "open_fieldbooks"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbmlist::fbmlist_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Open Fieldbook"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Germoplasm Managment", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        fbmlist::fbmlist_ui (name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)