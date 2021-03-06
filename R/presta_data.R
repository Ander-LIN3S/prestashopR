#' Extract order data from Prestashop
#'
#' This function enables you to extract data from the Orders table of Prestashop.
#' To do so, you just need to indicate the hostname, token and the orders of which you want to extract data from.
#'
#' @param token Webservice Key.
#' @param hostname Hotname of your website. For example, si your URL is https://www.test.com/, the hostname is www.test.com. On the other hand, if your URL is https://test.com/ your hostname is test.com.
#' @param endpoint Indicates the table you want to retrieve data from. The webservice should have permissions.
#' @param ids A vector including the ids that you want to extract. To extract all the data, set parameter to NULL.
#' @param group_dim The number of rows that will be retrieved in each API call. Set as default if you will retrieve a lot of data.
#' @param include_nested Whether to include related nested data, such as customer groups.
#' @param verbose Whether you want to see the process of extraction.
#' @return A dataframe with all extracted data
#' @export

presta_data = function(token, hostname, endpoint ="orders",id_column = "id" ,ids=NULL,  group_dim = 100,
                       include_nested = F, verbose = FALSE){

    if(is.null(ids)){
      ids = prestashopR::presta_extract_ids(token = token,
                                            hostname = hostname,
                                            endpoint = endpoint,
                                            verbose = verbose)
    }

    chunks = split(ids, ceiling(seq_along(ids)/group_dim))
    pb <- utils::txtProgressBar(min = 0, max = length(chunks), initial = 0, style = 3)

    data = list()
    if(include_nested){related = list()}

    for(i in 1:length(chunks)){

      # Hacemos la petición
      pedidos = paste0(chunks[[i]], collapse = "|")
      url = paste0("https://",hostname,"/api/",endpoint,"?ws_key=",token,"&filter[",id_column,"]=[", pedidos,"]&display=full&output_format=JSON")

      x = httr::GET(url)
      x = jsonlite::fromJSON(httr::content(x, type = "text", encoding = "UTF-8"))
      x = x[[1]]

      if(include_nested){
        related[[i]] = x[,sapply(x, class) %in% c("data.frame", "matrix","list")]
      }

      x = x[,!(sapply(x, class) %in% c("data.frame", "matrix","list"))]

      data[[i]] = x

      if(verbose == TRUE){
        print(paste('Round', i, 'finished. Rounds missing: ', length(chunks)-i))
      }

      utils::setTxtProgressBar(pb,i)
    }

    data = dplyr::bind_rows(data)

    if(include_nested){
      return(list(data, related))
    }else{
      return(data)
    }
  }
