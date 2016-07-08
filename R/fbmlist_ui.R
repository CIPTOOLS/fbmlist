#' UI germoplasm and material list managment
#' 
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title diaply title name
#' @param name UI TabName
#' @export
#' 

fbmlist_ui <- function(type = "tab", title = "Germoplasm Managment", name = "fbmlist_ui"){
  
  shinydashboard::tabItem(tabName = name,
                          h2(title),   
                          
                          box(
                            title = "Material List", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE, width = 12,
                            #tabsetPanel(
                            tabBox(width = 10,
                                   tabPanel(title = "TAB1", #begin tabset "CHECK"

                                            shiny::actionButton("fbMlist_syncronize", "Syncronize Material Lists", icon("refresh"), 
                                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                            ),
                                            fluidRow( 
                                              #
                                              column(width = 12, DT::dataTableOutput('fbmlist_table'))#,
                                              #column(8, shiny::textOutput("row_print"))
                                              #column(8, shiny::actionButton("refresh", "Refresh Sheet")),
                                              #column(8, shiny::actionButton(inputId = "fbmlist_file" ,label = "mlist Book"))
                                              
                                            ), #end fluidow
                                            
                                            fluidRow(
                                              HTML('<div style="float: right; margin: 0 15px 18px 0px;">'),
                                              shiny::actionButton("fbMlist_connect", "Refresh Table", icon("refresh"), 
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                              ),
#                                               shiny::actionButton(inputId = "fbMlist_connect" ,label = "mlist Book", icon("file"),
#                                                                   style="color: #fff; background-color: #51a351; border-color: #51a351"
#                                               ),
                                              HTML('</div>')
                                            ),
                                        br(),
                                        br(),
                                        br()


                                            
                                   )#,#end tab Panel "CHECK"
                                   
                            )
                          ),
                          br(),
                          br(),
                          br()
                          
                          
  )#End data_processing tabItem
  
}
