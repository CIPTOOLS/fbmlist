#' Server germoplasm and material list managment
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

fbmlist_server <- function(input,output,session, values){
  
  germlist_data <- shiny::reactive({
    
    #tablita <- function(){ 
    gmldata_button <- input$fbMlist_syncronize
    m <- dbDriver("MySQL");
    
    res <- dbSendQuery(con, "select * from materiallist")
    genes <- fetch(res, n = -1)
    
    write.dbf(germlist_data(),"mlist.dbf")
    germlist_db <- foreign::read.dbf(file ="mlist.dbf")
    
    #germlist_db
    germlist_db
   })
  
  output$fbmlist_table  <-  DT::renderDataTable({
    
    gmlconnect_button<- input$fbMlist_connect
    if(is.null(gmlconnect_button)){ return() }
    
    if(!is.null(gmlconnect_button)){
    #germlist_db <- foreign::read.dbf(file ="mlist.dbf")
    DT::datatable(germlist_data(), selection = c("single"))
    }
    
  })
  
  shiny::observeEvent(input$fbMlist_syncronize , {
    
    shiny::withProgress(message = "Opening Fieldbook...",value= 0,
                        {
                          try({ #begin of Try
                            index  <- input$fbmlist_table_rows_selected
                            n_pos <- length(index) #You select the last row selected. By default it will storage all positions
                            index <- index[n_pos] 
                            
                            index <- as.numeric(index)
                            print(index)
                            
                            table_files <- tablita()
                            
                            table_files <- table_files[index,"Files_Direction"]
                            selected_file <- as.character(table_files)
                            print(selected_file)
                            shell.exec(selected_file)
                            
                          })# end of Try
                        })
    
  })
  
  
  
  
} 
