#' qualiar_download
#'
#' This function downloads and unzips INEA's Qualiar (Air Quality Program) to a temporary folder.
#'
#' @param years  
#' @param ret If TRUE (Default) the function will return a string containing the temporary directory in which the files were saved. 
#'
#' @return
#' @export
#'
#' @examples
qualiar_download = function(years, ret = TRUE){
  
  message(
    sprintf("Downloading files for years: %s. Please wait, it might take a while..."), 
    toString(years)
    )

  temp = tempfile() %T>% dir.create()
  purrr::iwalk(.x = years,
               .f= ~download.file(
                 url = paste0("200.20.53.25/qualiar/Arquivos/", .x, ".zip"), 
                 destfile = paste0(temp, "/", .x, ".zip")))
  
  purrr::walk(.x= list.files(temp, pattern = ".zip$"), 
              .f = function(.x){unzip(x, exdir = paste0(temp, "/", "inear_files"))})
  
  message(sprintf("Os arquivos foram salvos em: %s", temp))
  
  if(ret == TRUE){
    return(temp)
  }
}

