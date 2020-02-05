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
  
  .get_location_stations <- function(data) {
    #' Essa funcao identifica em quais colunas comecam cada estacao
    # data = janitor::remove_empty(data, which = 'rows')
    stations <- data[1, ] # Pega a linha com as estações daquele arquivo
    
    .stations_start_loc <- function(stat_lines) {
      #'  Retorna a localizacao de onde cada estacao se inicia
      stat_lines <- colnames(stat_lines[which(!is.na(stat_lines[1, ]))])
      return(stat_lines)
    }
    
    estacoes_do_arquivo <- gsub(
      x = .stations_start_loc(stations), # stations_start_loc é utilizada aqui
      pattern = "\\.\\.\\.",
      replacement = ""
    )
    
    
    estacoes_do_arquivo= append(estacoes_do_arquivo, ncol(data))
    
    return(as.numeric(estacoes_do_arquivo))
  }
  
  .splitSecStEnd <- function(.x) {
    
    # create auxiliary dataframe
    # lead and calc "-1"
    resp <- tibble::tibble(
      st = .x,
      st.lead = dplyr::lead(st),
      end = st.lead - 1
    )
    
    # force final number
    resp[nrow(resp) - 1, ]$end <- resp[nrow(resp), ]$st
    
    # remove last row and auxiliary column
    resp <- resp[1:nrow(resp) - 1, -2]
    
    return(resp)
  }
  
  
  xls_files = purrr::map(.x= data_location, 
                         .f= vroom::vroom) %>% 
    janitor::remove_empty(
      which = c('cols', 
                'rows')) %>% 
    purrr::map(.f = .where_inea) # Do a pre-cleaning
  
  
  
  splitter = function(data){
    col_tbs = .get_location_stations(data) %>% .splitSecStEnd
    
    
    .spl = function(data, split_sec_output_a, split_sec_output_b){
      data[split_sec_output_a, split_sec_output_b]
    }
    
    spl_output = map2(.x= col_tbs[, 1], .y = col_tbs[, 2], .f= ~.spl(data, .x, .y))
    
    
      
    
  }
  
      
  
  
}
