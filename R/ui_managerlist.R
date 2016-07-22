#' UI for open books in HiDAP
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

managerlist_ui <- function(type = "tab", title = "Manage List", name = "manageList"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = "Manager List", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = 12,
                            #tabsetPanel(
                            tabBox(width = 10,
                                   tabPanel("Check", #begin tabset "CHECK"
                                            
                                            fluidRow(
                                              
                                              column(width = 12, 
                                                     
                                                     #HTML('<div style="float: right; margin: 0 5px 5px 10px;">'),
                                                     shiny::actionButton("fbmlist_syncronize", "Syncronize Material Lists", icon("refresh"), 
                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                     #HTML('</div>')
                                                     
                                                     )

                                            ),
  
                                            fluidRow( 
                                              #
                                              column(width = 12, DT::dataTableOutput('x2'))#,
                                             
                                            ), #end fluidow
                                            
                                            fluidRow(
                                              HTML('<div style="float: right; margin: 0 15px 18px 0px;">'),
                                              shiny::actionButton("fbmlist_refresh", "Refresh Table", icon("refresh"), 
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                              ),
                                              shiny::actionButton(inputId = "fbmlist_file" ,label = "Open Book", icon("file"),
                                                                  style="color: #fff; background-color: #51a351; border-color: #51a351"
                                              ),
                                              HTML('</div>')
                                            )#,

                                   )#,#end tab Panel "CHECK"
                                   
                            )
                          ),
                          br(),
                          br(),
                          br()
                          
                          
  )#End data_processing tabItem
  
}