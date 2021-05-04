#' Extract ids from Prestashop Table
#'
#' This function enables you to extract the ids from a selected Prestashop table.
#' To do so, you just need to indicate the hostname, token and the table from where you want to extract ids.
#'
#' @param token Webservice Key.
#' @param hostname Hotname of your website. For example, si your URL is https://www.test.com/, the hostname is www.test.com. On the other hand, if your URL is https://test.com/ your hostname is test.com.
#' @param endpoint Indicates the table you want to retrieve data from. The webservice should have permissions.
#' @param verbose Whether you want to see the process of extraction.
#' @param date_min Minimum date to get data from in YY-MM-DD format.  Not required. If used, date_max is also required.
#' @param date_max Maximum date to get data from YY-MM-DD format. Not required. If used, date_min is also required.
#' @param date_add Column to use for filtering data by date. Defaul is date_add.
#' @param default_chunks In case too much info is asked, the number of date partitions that will be created for smoother extractions.
#' @return A vector with all the ids
#' @export
#'

presta_extract_ids = function(token, hostname, endpoint ="orders", date_min =NULL, date_max=NULL, date_column = "date_add", verbose = FALSE, default_chunks = 2){

    dates_compr = sum(c(is.null(date_min),is.null(date_max)))
    if(dates_compr == 1){stop('Please provide both date_min and dat_max')}
    if(dates_compr == 2){

      # Comprobamos si se puede hacer la petición de una sola vez
      url =  paste0("https://",hostname,"/api/",endpoint,"?ws_key=",token,"&output_format=JSON")

    } else{
      url =  paste0("https://",hostname,"/api/",endpoint,"?ws_key=",token,"&filter[",date_column,"]=[",date_min,",",date_max,"]&date=1&output_format=JSON")
    }

    x = httr::GET(url)

    # Si la petición da error, particionamos en dos y volvemos a hacer la peticion
    if(x$status_code != 200){
      if(dates_compr ==2){

        # Si no se han pasado fechas, fijamos la fecha actual y el principio de los tiempos
        start_date = as.Date("2000-01-01")
        end_date = Sys.Date()
        fechas = seq(start_date, end_date, by = as.numeric((end_date-start_date)/default_chunks))
      } else{
        fechas = seq(date_min, date_max, by = as.numeric((date_max-date_min)/default_chunks))
      }
      ids = list()
      for(i in 1:(length(fechas)-1)){

        if(verbose){message(paste('Too much data. Extracting data from', as.Date(fechas[i]),
                                  'to',as.Date(fechas[i+1])))}

        ids[[i]] = prueba(token, hostname, endpoint, date_min =fechas[i], date_max=fechas[i+1],
                          date_column, verbose, default_chunks)

      }
      x = as.vector(unlist(ids))
      return(x)
    }
    else{
      x = jsonlite::fromJSON(httr::content(x, type = "text", encoding = "UTF-8"))
      x = as.vector(unlist(x))
      return(x)
    }
  }
