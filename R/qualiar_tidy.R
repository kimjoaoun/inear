#' Title
#'
#' @param data_location 
#'
#' @return
#' @export
#'
#' @examples
qualiar_tidy = function(data_location){
  
  #' .header_removal= function(data){
  #'   #' This function clean the names of the columns
  #'   data %<>% janitor::clean_names(case = "lower_camel")
  #'   data[which(grep("Data da Med", data[1])):nrow(data), ]
  #' }
  
  .where_inea = function(data){
    #' This function finds where is the INEA/Private Company Tag.
    data[grep("^INEA|^EMPRESA PRIVADA", data$...2):nrow(data), ]
  }
  
  xls_files = purrr::map(.x= data_location, .f= vroom::vroom) %>% 
    janitor::remove_empty(which = c('cols', 'rows')) %>% 
    purrr::map(.f = .where_inea)# Do a pre-cleaning
    
  
    
      
  
  
}
