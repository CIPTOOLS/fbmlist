#' Stablish conection to Material List Data Base
#' @describeIn Read the table from M.List DB an write into the computer
#' @importFrom RMySQL dbDriver dbConnect dbSendQuery fetch
#' @importFrom foreign write.dbf
#' 
#' 
fbmlist_data <- function(){
  
    mtl_drv <- dbDriver("MySQL");
    mtl_con<-dbConnect(mtl_drv, user='cipmateriallist',password='c$?ZYmra2KgJvmH0616',host='176.34.248.121',dbname='cipmateriallist');
    mtl_query <- dbSendQuery(con, "select * from materiallist")
    mtl_data <- fetch(mtl_query, n = -1)
    foreign::write.dbf(mtl_data, "mlist_biomart.dbf")
    #
    
}






