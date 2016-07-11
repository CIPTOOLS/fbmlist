#' Server germoplasm and material list managment
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

fbmlist_server <- function(input,output,session, values){
  
  
  gmtl_data <- reactive({
    
    btn_conn <- fbmlist_connect
    if(is.null(btn_conn)){ return(NULL) }
    
    germlist_db <- foreign::read.dbf(file ="mlist_biomart.dbf")
    
    
    
  }) 
  
  
  # 
  output$fbmlist_table  <-  DT::renderDataTable({

    DT::datatable(gmtl_data(), selection = c("single"))


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
