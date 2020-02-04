#' qualiar_download
#'
#' This function downloads and unzips INEA's Qualiar (Air Quality Program) to a temporary folder.
#'
#' @param years Years which you want to collect the data.
#' @param ret If TRUE (Default) the function will return a string containing the temporary directory in which the files were saved. 
#'
#' @return A string with the location of the downloaded files.
#' @export 
#'
#' @examples 
#' qualiar_download(2010, ret= TRUE)

qualiar_download = function(years, ret = TRUE){
  message('Checking if INEA servers are online...')
  
  Sys.sleep(0.5) # Let INEA Toaster Server take a break, it might have much requests coming in.
  
  if(!RCurl::url.exists("200.20.53.25/qualiar/")){
    stop("Oops! Looks like INEA servers are offline, or you are not connected to the internet. 
         Check your connection and try again later.")
  } else {
    message("Establishing connection...")
  }
  
  
  message(
    sprintf("Downloading files for year(s): %s. Please wait, it might take a while...", 
    toString(years)))

  temp = tempfile() %T>% 
    dir.create()
  
  purrr::iwalk(.x = years,
               .f= ~download.file(
                 url = paste0("200.20.53.25/qualiar/Arquivos/", .x, ".zip"), 
                 destfile = paste0(temp, "/", .x, ".zip")
                 )
               )
  
  purrr::walk(.x= list.files(temp, pattern = ".zip$", full.names = TRUE), 
              .f = function(x){unzip(x, exdir = paste0(temp, "/inear_files")
                                     )
                }
              )
  
  message("Converting files to CSV, please wait...")
  
  suppressMessages(
    purrr::walk(.x = list.files(paste0(temp, "/inear_files"), full.names = TRUE, pattern = "(.xls|.xlsx)$"), 
               .f = ~rio::convert(.x, 
                                 out_file = gsub(x= .x, 
                                                 pattern=".(xls|xlsx)",
                                                 replacement = ".csv")
                                 )
               )
    )
  
   invisible(file.remove(list.files(paste0(temp, '/inear_files'), 
                         pattern=".(xls|xlsx)$", 
                         full.names = TRUE)))
  
  message(sprintf("Files saved in: %s", temp)) # Telling the user where the files are stored.
  
  if(ret == TRUE){
    return(sprintf("%s/inear_files", temp))
  }
}

