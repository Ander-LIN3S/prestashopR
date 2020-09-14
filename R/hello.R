#' Extraer datos de clientes
#'
#' Esta función te permite extraer los datos de cualquier endpoint de  prestashop mediante su API.
#' Para ello, simplemente debes indicar el token, el hostname y los filtros que quieras aplicar.
#'
#' @param token Clave de tu webservice
#' @param hostname Nombre de tu página web. Por ejemplo, si la URL es https://www.prueba.com/, el hostname es www.prueba.com.
#' @param id_cliente Vector con el o los ids de clientes a extraer.
#' @param tam_grupo El número de clientes de los que se pedirá información con cada petición para evitar saturar el servidor.
#' @param verbose Indica si la función debería indicar cada paso de la extracción de datos.
#' @return Un dataframe con todos los datos extraidos.
#' @export

datos_pedido = function(token, hostname,id_cliente, tam_grupo = 100, verbose = FALSE){

  chunks = split(ids_de_cliente, ceiling(seq_along(ids_de_cliente)/tam_grupo))

  for(i in 1:length(chunks)){

    # Hacemos la petición
    pedidos = paste0(chunks[[i]], collapse = "|")
    url = paste0("https://",hostname,"/api/orders?ws_key=",token,"&filter[id_customer]=[", pedidos,"]&display=full&output_format=JSON")
    x = httr::GET(url)
    x = jsonlite::fromJSON(jsonlite::content(x, type = "text", encoding = "UTF-8"))
    x = x[[1]]
    x = x[,1:(length(x)-1)]

    if(i == 1){
      datos_pedido = x
    } else{
      datos_pedido = rbind(datos_pedido, x)
    }
    if(verbose == TRUE){
      print(paste('Ronda', i, 'terminada. Faltan', length(chunks)-i))
    }
  }

  return(datos_pedido)

}

