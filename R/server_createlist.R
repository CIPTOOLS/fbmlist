#' Server Germoplasm and Material List Managment
#' 
#' @param input shinyserver input 
#' @param output shinyserver output
#' @param session shinyserver session
#' @param values reactive values
#' @author Omar Benites
#' @export

server_generate <- function(input,output,session, values){
  
  
  gmtl_data_new <- eventReactive(input$fbmlist_connect_new, {
    
    dbf_file <- input$fbmlist_sel_list_new
    n <- length(input$fbmlist_sel_list_new)
  
    if(n==1){
      germlist_db <- readRDS(dbf_file)
    }

    if(n > 1){
      combine <- list() 
      for(i in 1:n){  
        combine[[i]] <- readRDS(file = dbf_file[i]) 
      } 
      join_books <- data.table::rbindlist(combine,fill = TRUE)
      join_books <- as.data.frame(join_books)
      germlist_db <- join_books
    }
    
    
    germlist_db
    
  }) 
  
  output$show_mtable_new <- reactive({
    return(!is.null(gmtl_data_new()))
  })
  
  output$show_save <- reactive({
    return(length(input$fbmlist_select_new[1]))
  })
  
  outputOptions(output, 'show_mtable_new', suspendWhenHidden=FALSE)
  #outputOptions(output, 'show_save', suspendWhenHidden=FALSE)
  
  
  output$sel_list_new_btn <- renderUI({
  
    crop <- input$fbmlist_sel_crop_new
    type_db <- input$fbmlist_sel_type_new
    mtl_db_sel <- mtl_files()$short_name
    
    if(crop == "") {
      
      db_files_choices  <-  "" 
      sel_multiple <- FALSE 
      
    }   
    
    if(crop == "potato") { 
      
      if(type_db=="Institutional"){ 
        
        #db_files_choices <- list("dspotatotrials_dpassport.dbf", "potato_pedigree.dbf")
        db_files_choices <- list("dspotatotrials_dpassport.rds", "potato_pedigree.rds")
        sel_multiple <- FALSE                         
      }
      if(type_db=="Local"){ 
        
        db_files_choices <- mtl_db_sel[str_detect(mtl_db_sel , "PT")] 
        sel_multiple <- TRUE
      }
    }
    
    if(crop == "sweetpotato") {
      
      if(type_db=="Institutional") { 
        db_files_choices <- list("dssweettrials_dpassport.rds" , "sweetpotato_pedigree.rds")
        sel_multiple <- FALSE   
      }
      if(type_db=="Local"){ 
        db_files_choices <- mtl_db_sel[str_detect(mtl_db_sel , "SP")]
        sel_multiple <- TRUE 
      }
    }
    
    shiny::selectizeInput(inputId ="fbmlist_sel_list_new", label = "Select DataBase", 
                          multiple =  sel_multiple, width="100%", choices = db_files_choices,
                          options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
  })
  
  output$create_new_name <- renderUI({
    
    req(input$fbmlist_select_new)
    textInput("fbmlist_create_new_name", label = h3("New List Name"), value = "", placeholder = "Write a List Name")
  })
  
  output$savelist_new_btn <- renderUI({
    
    req(input$fbmlist_select_new)
    shiny::actionButton("fbmlist_save_new", label = "Save List", icon = icon("save"))
    
  })
  
  
  # Selection on generataed material list button ----------------------------------------------------
  output$fbmlist_table_new  <-  DT::renderDataTable({
    
    
    shiny::req(input$fbmlist_connect_new)
    #shiny::req(input$fbmlist_selectgenlist)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          mtl_table <- gmtl_data_new()

                          
                          if(input$fbmlist_txtarea_new!=""){
                            
                            search_filter <- str_split(input$fbmlist_txtarea_new,"\\n")[[1]]
                            search_filter <- stringr::str_trim(search_filter,side = "both")
                            
                            mtl_table_f <- filter(mtl_table, Accesion_Number %in% search_filter)
                            print("---primer")
                            print(nrow(mtl_table))
                            print("---primer-end")
                            
                            if(nrow(mtl_table_f)==0 &&  is.element("Accesion_Name",names(mtl_table_f))) {
                              print("---sedungo")
                              mtl_table_f <- dplyr::filter(mtl_table, Accesion_Name %in% search_filter)
                              print("---seundo-end")
                            }
                            
                            if(nrow(mtl_table_f)==0  &&  is.element("Population",names(mtl_table_f))) {
                              print("---tercer")
                              mtl_table_f <- dplyr::filter(mtl_table, Population %in% search_filter)
                              print("---tercer-end")
                            }
                            
                            if(nrow(mtl_table_f)>0){ 
                              mtl_table <- mtl_table_f
                            }

                            
                            DT::datatable(mtl_table, rownames = FALSE, 
                                          #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                          selection = list( mode = "multiple"), 
                                          filter = 'bottom',
                                          extensions = 'Buttons', options = list(
                                            dom = 'Bfrtip',
                                            buttons = 
                                              list(list(
                                                extend = 'collection',
                                                buttons = c('csv', 'excel'),
                                                text = 'Download'
                                              ))
                                            
                                          )
                            )
                            
                            
                            
                            
                            
                          } else {
                            
                            
                            
                            DT::datatable(mtl_table, rownames = FALSE, 
                                          #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                          selection = list( mode = "multiple"), 
                                          filter = 'bottom',
                                          extensions = 'Buttons', options = list(
                                            dom = 'Bfrtip',
                                            buttons = 
                                              list(list(
                                                extend = 'collection',
                                                buttons = c('csv', 'excel'),
                                                text = 'Download'
                                              ))
                                            
                                          )
                            )
                            
                          }
 
                          
                        }) #end of Progress
    
  })
  
  gmtl_row_index_new <- eventReactive(input$fbmlist_select_new,{
    
    row_select <- input$fbmlist_table_new_rows_selected #comand to get selected values
    row_filter <- input$fbmlist_table_new_rows_all #comand to get filtered values
    row_mtlist_selection <- dplyr::intersect(row_select,row_filter)
    row_mtlist_selection <- sort(row_mtlist_selection)
    
  }) 
  
  
  output$fbmlist_choosen_table_new  <- DT::renderDataTable({
    
    #print(input$foo)
    
    index <- gmtl_row_index_new()
    mtl_table <- gmtl_data_new()
    chosen_gmtl_table <-  mtl_table[index, ]
    chosen_gmtl_table
    
  }, options = list(searching = FALSE) )
  
  # Observers of fbmlist ----------------------------------------------------
  
  shiny::observeEvent(input$fbmlist_save_new,{
    
    index <- gmtl_row_index()
    mtl_table <- gmtl_data_new()
    chosen_gmtl_table <-  mtl_table[index, ]
    fbmlist_name_dbf  <- str_trim(string = input$fbmlist_create_name_new, side = "both")
    fbmlist_name_dbf  <- gsub("\\s+", "_", fbmlist_name_dbf)
    
    #All the files names
    db_files          <- file_path_sans_ext(mtl_files()$short_name)

    if(fbmlist_name_dbf %in% db_files) {
      
      shinyBS::createAlert(session, "alert_fbmlist_new", "fbdoneAlert", title = "Warning",style = "warning",
                           content = "This list already exists", append = FALSE)
      
    }
    else if(fbmlist_name_dbf==""){ 
      
      shinyBS::createAlert(session, "alert_fbmlist_new", "fbdoneAlert", title = "Warning",style = "warning",
                           content = "Please Type a Material List Name", append = FALSE)
      
    } 
    else {
      
      crop <- input$fbmlist_sel_crop_new
      
      if(crop=="potato")      {fbmlist_name_dbf <- paste("PT",fbmlist_name_dbf,sep = "_")}
      if(crop=="sweetpotato") {fbmlist_name_dbf <- paste("SP",fbmlist_name_dbf,sep = "_")} 
      
      #foreign::write.dbf(dataframe = chosen_gmtl_table, file = fbmlist_name_dbf, factor2char = FALSE)
      fbmlist_name_dbf <- paste(fbmlist_name_dbf,".rds",sep = "")
      saveRDS(chosen_gmtl_table,file = fbmlist_name_dbf)
      mtl_files()
      shinyBS::createAlert(session, "alert_fbmlist_new", "fbdoneAlert", title = "Sucessfully Created!",
                           content = "Material List successfully created!", append = FALSE)
      
      for(i in 1:75000){print(i)}
      
      shinyjs::js$refresh() 
      
    }
    
    
  })
  
  
}



