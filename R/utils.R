#' Stablish conection to Material List Data Base
#' @describeIn Read the table from M.List DB an write into the computer
#' 
#' 
fbmlist_data <- function(){
  
#     mtl_drv <- dbDriver("MySQL");
#     mtl_con<-dbConnect(mtl_drv, user='cipmateriallist',password='c$?ZYmra2KgJvmH0616',host='176.34.248.121',dbname='cipmateriallist');
#     mtl_query <- dbSendQuery(mtl_con, "select * from materiallist")
#     mtl_data <- fetch(mtl_query, n = -1)
    
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user='dspotatotrials',password='ca7H=j~$V+p2G0715',host='176.34.248.121',dbname='datacippotato_martpot_trials');
    res <- dbSendQuery(con, "SELECT `CIPNUMBER`, `CULTVRNAME`, `COLNUMBER`, `FEMALE`, `MALE`, `PopulationGroup` FROM `dspotatotrials__dpassport__main`")
    dspotatotrials_dpassport <- fetch(res, n = -1)
    names(dspotatotrials_dpassport) <- c("Accesion_Number","Accesion_Name","Accesion_Code","Female_AcceNumb","Male_AcceNumb","Population")
    #write.dbf(dspotatotrials_dpassport,"dspotatotrials_dpassport.dbf")
    saveRDS(dspotatotrials_dpassport,file = "dspotatotrials_dpassport.rds")
    dbDisconnect(con)
 
    
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user='dssweettrials',password='c42=gFf8AfZnS0715',host='176.34.248.121',dbname='datacipsweet_martsweet_trials');
    res <- dbSendQuery(con, "SELECT `CIPNUMBER`, `CULTVRNAME`, `COLNUMBER`, `FEMALE`, `MALE`, `PopulationGroup` FROM `dssweettrials__dpassport__main`")
    dssweettrials_dpassport <- fetch(res, n = -1)
    names(dssweettrials_dpassport) <- c("Accesion_Number","Accesion_Name","Accesion_Code","Female_AcceNumb","Male_AcceNumb","Population")
    #write.dbf(dssweettrials_dpassport,"dssweettrials_dpassport.dbf")
    saveRDS(dssweettrials_dpassport,file = "dssweettrials_dpassport.rds")
    dbDisconnect(con)
    
    
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user='cippedigree',password='cF6Jr<tVW]dU60713',host='176.34.248.121',dbname='cippedigree');
    res <- dbSendQuery(con, "SELECT ped_family.pedNameCipnumber, ped_family.pedFemaleCipnumber, ped_family.pedFemale, ped_family.pedMaleCipnumber, ped_family.pedMale, ped_population.PedPopName, ped_family.pedCycle FROM ped_family INNER JOIN ped_population ON ped_family.ped_population_PedPopId = ped_population.PedPopId WHERE ped_family.Crop_CropId = 'SO' and ped_family.pedYear = '2016'")
    potato_pedigree <- fetch(res, n = -1)
    names(potato_pedigree) <- c("Accesion_Number","Female_AcceNumb","Female_codename","Male_AcceNumb","Male_codename","Population", "Cycle")
    #write.dbf(potato_pedigree,"potato_pedigree.dbf")
    saveRDS(potato_pedigree,file = "potato_pedigree.rds")
    dbDisconnect(con)
    
#     m <- dbDriver("MySQL");
#     con <- dbConnect(m,user='cippedigree',password='cF6Jr<tVW]dU60713',host='176.34.248.121',dbname='cippedigree');
#     res <- dbSendQuery(con, "SELECT ped_family.pedNameCipnumber, ped_family.pedFemaleCipnumber, ped_family.pedFemale, ped_family.pedMaleCipnumber, ped_family.pedMale, ped_population.PedPopName, ped_family.pedCycle FROM ped_family INNER JOIN ped_population ON ped_family.ped_population_PedPopId = ped_population.PedPopId WHERE ped_family.Crop_CropId = 'SO'")
#     potato_pedigree <- fetch(res, n = -1)
#     write.dbf(potato_pedigree,"potato_pedigree.dbf")
#     dbDisconnect(con)
 
    m <- dbDriver("MySQL");
    con <- dbConnect(m,user='cippedigree',password='cF6Jr<tVW]dU60713',host='176.34.248.121',dbname='cippedigree');
    res <- dbSendQuery(con, "SELECT ped_family.pedNameCipnumber, ped_family.pedFemaleCipnumber, ped_family.pedFemale, ped_family.pedMaleCipnumber, ped_family.pedMale, ped_population.PedPopName, ped_family.pedCycle FROM ped_family INNER JOIN ped_population ON ped_family.ped_population_PedPopId = ped_population.PedPopId WHERE ped_family.Crop_CropId = 'IP'")
    sweetpotato_pedigree <- fetch(res, n = -1)
    names(sweetpotato_pedigree) <- c("Accesion_Number","Female_AcceNumb","Female_codename","Male_AcceNumb","Male_codename","Population", "Cycle")
    #write.dbf(sweetpotato_pedigree,"sweetpotato_pedigree.dbf")
    saveRDS(sweetpotato_pedigree,file = "sweetpotato_pedigree.rds")
    dbDisconnect(con)
    #foreign::write.dbf(mtl_data, "mlist_biomart.dbf") 
    
}


#' List of the dbf files
#' @describeIn Read the table from M.List DB an write into the computer
#' 
#' 
   mtl_files <- function(){
  
      #dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".dbf|.rds")
      dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".rds")
      lg_dbf <- length(dbf_file_list)
      
      if(lg_dbf == 0){ gmtfiles <- "" }
      if(lg_dbf>0)   { 
          ignore_temps <- grepl(pattern = "~\\$",x =  dbf_file_list)
          dbf_file_list <-  dbf_file_list[!ignore_temps]
          short_name <- basename(dbf_file_list)
          gmtfiles <- data.frame(short_name, dbf_file_list, stringsAsFactors = FALSE)
          names(gmtfiles) <- c("short_name","full_name")
          gmtfiles
      }
      
      mtl_files <- gmtfiles
      mtl_files
  
}

# List of the dbf files
# @describeIn Read the table from M.List DB an write into the computer
# 

# ped_names <- function(){
#   
# #   "dspotatotrials_dpassport.dbf"
# #   "dssweettrials_dpassport.dbf"
#   
# #   "potato_pedigree.dbf"          
# #   "sweetpotato_pedigree.dbf"
# #   
# #   c("pedNameCip" ,  "CIPNUMBER" , "Accesion_Number")
# #   c("pedFemaleC" ,  "FEMALE"    , "Female_AcceNumb")
# #   c("pedMaleCip" ,  "MALE"      , "Male_AcceNumb")
# #   c("PedPopName" ,  "Population", "Population")
#   
# }
#   
  
    
  
  
  
  
  
  

