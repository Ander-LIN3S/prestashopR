#' Extract ids from Prestashop Table
#'
#' This function enables you to extract the ids from a selected Prestashop table.
#' To do so, you just need to indicate the hostname, token and the table from where you want to extract ids.
#'
#' @param token Webservice Key.
#' @param hostname Hotname of your website. For example, si your URL is https://www.test.com/, the hostname is www.test.com. On the other hand, if your URL is https://test.com/ your hostname is test.com.
#' @param endpoint Indicates the table you want to retrieve data from. The webservice should have permissions.
#' @param verbose Whether you want to see the process of extraction.
#' @return A vector with all the ids
#' @export
#'

presta_extract_ids = function(token, hostname, endpoint ="orders", verbose = FALSE){
  url =  paste0("https://",hostname,"/api/",endpoint,"?ws_key=",token,"&output_format=JSON")
  x = httr::GET(url)
  x = jsonlite::fromJSON(httr::content(x, type = "text", encoding = "UTF-8"))
  x = as.vector(unlist(x))
  if(verbose==T){print("Extracted all order ids.")}
  return(x)
}
