#' Server openbooks
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

server_managerlist <- function(input,output,session, values){
  
  #fb_file_list <- shiny::reactive({
#   tablita <- function(){#
#     
#     #fb_file_list <- list.files(getwd(),full.names = TRUE)
#     fb_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".xlsx")
#     ignore_temps <- grepl(pattern = "~\\$",x = fb_file_list)
#     
#     fb_file_list <- fb_file_list[!ignore_temps]
#     #fb_file_list <- basename(fb_file_list)
#     
#     files <- data.frame(fb_file_list)
#     names(files) <- "Files_Direction"
#     files
#   }#new
  # })
  
  output$x2  <-  DT::renderDataTable({
    # output$x1  <-  DT::renderDataTable({
    
    input$fbmlist_refresh
    #isolate( 
    
    #fb_file_list(), options = list(), selection = c("single"),server=FALSE
    #  DT::datatable(fb_file_list(),selection = c("single"),options = list()) 
    #)  
    
    tablita <- mtl_files()
    #DT::datatable(fb_file_list(),selection = c("single"))     
    DT::datatable(tablita,selection = c("single"))
    # }) 
  })
  
  shiny::observeEvent(input$fbmlist_syncronize , {
    shiny::withProgress(message = "Syncronizing material list from DB...",value= 0,
                        {
                          try({ #begin of Try
                            fbmlist_data()
                          })# end of Try
                        })
  })
 
  
} 