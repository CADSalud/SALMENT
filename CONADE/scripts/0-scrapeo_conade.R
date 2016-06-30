library(rvest)
library(dplyr)
library(parallel)

obten_campo <- function(p,xpath){
  p %>%
    html_nodes(xpath = xpath) %>%
    html_attr('value')
}
obten_checkbox <- function(p,xpath){
  aux <- p %>%
    html_nodes(xpath = xpath) %>%
    html_attr('checked')
  return(ifelse(is.na(aux),0,1))
}

xp_nombre <- '//*[@id="textfield"]'
xp_lat1 <- '//*[@id="lat_grados"]'
xp_lat2 <- '//*[@id="lat_minutos"]'
xp_lat3 <- '//*[@id="lat_segundos"]'
xp_lon1 <- '//*[@id="long_grados"]'
xp_lon2 <- '//*[@id="long_minutos"]'
xp_lon3 <- '//*[@id="long_segundos"]'
xp_propietario <- '//*[@id="textfield4"]'
xp_estado_uso <- '//*[@id="textfield5"]'
xp_registrador <- '//*[@id="textfield13"]'
xp_estado <- '//*[@id="textfield10"]'
xp_municipio <- '//*[@id="textfield14"]'
xp_calle <- '//*[@id="textfield15"]'
xp_colonia <- '//*[@id="textfield23"]'
xp_numext <- '//*[@id="textfield24"]'
xp_numint <- '//*[@id="textfield25"]'
xp_cp <- '//*[@id="textfield26"]'
xp_checkenfermeria <- '//*[@id="checkbox"]'
xp_checkguarderia <- '//*[@id="checkbox2"]'
xp_checkestacionamiento <- '//*[@id="checkbox3"]'
xp_checkescuela <- '//*[@id="checkbox4"]'
xp_checkjuegos <- '//*[@id="checkbox5"]'

obten_datos <- function(i){
  pag <- read_html(paste0('http://sistemas.conade.gob.mx/portalCenso/instalacion_2.aspx?IDInstalacion=',i))
  renglon <- data.frame(establecimiento = i,
               nombre = obten_campo(pag,xp_nombre),
               lat1 = obten_campo(pag,xp_lat1),
               lat2 = obten_campo(pag,xp_lat2),
               lat3 = obten_campo(pag,xp_lat3),
               lon1 = obten_campo(pag,xp_lon1),
               lon2 = obten_campo(pag,xp_lon2),
               lon3 = obten_campo(pag,xp_lon3),
               propietario = obten_campo(pag,xp_propietario),
               estado_uso = obten_campo(pag,xp_estado_uso),
               registrador = obten_campo(pag,xp_registrador),
               estado = obten_campo(pag,xp_estado),
               municipio = obten_campo(pag,xp_municipio),
               calle = obten_campo(pag,xp_calle),
               colonia = obten_campo(pag,xp_colonia),
               numext = obten_campo(pag,xp_numext),
               numint = obten_campo(pag,xp_numint),
               cp = obten_campo(pag,xp_cp),
               enfermeria = obten_checkbox(pag,xp_checkenfermeria),
               guarderia = obten_checkbox(pag,xp_checkguarderia),
               estacionamiento = obten_checkbox(pag,xp_checkestacionamiento),
               escuela = obten_checkbox(pag,xp_checkescuela),
               juegos = obten_checkbox(pag,xp_checkjuegos))
  espacios_deportivos <- pag %>% html_nodes(css = '.Body') %>% html_text()
  espacios_deportivos <- espacios_deportivos[7:length(espacios_deportivos)]
  espacios_deportivos <- as.data.frame(matrix(espacios_deportivos,nrow=2,ncol=5,byrow = T))
  colnames(espacios_deportivos) <- c('Cantidad','Espacio','Deporte Principal','Tipo','Aprovechamiento')
  espacios_deportivos$establecimiento <- i
  return(list(renglon,espacios_deportivos))
}


res1 <- mclapply(7863:19892, obten_datos, mc.cores = 4)

res11 <- lapply(res1, function(x){return(x[[1]])})
espacios_deportivos11 <- Reduce(bind_rows, res11)

res21 <- lapply(res1, function(x){return(x[[2]])})
espacios_deportivos21 <- Reduce(bind_rows, res21)

saveRDS(object = espacios_deportivos11, file = 'data/espacios_deportivos11.rds')
saveRDS(object = espacios_deportivos21, file = 'data/espacios_deportivos21.rds')

rm(res1,res11,espacios_deportivos11,res21,espacios_deportivos21)

res2 <- mclapply(19893:31922, obten_datos, mc.cores = 4)

res12 <- lapply(res2, function(x){return(x[[1]])})
espacios_deportivos12 <- Reduce(bind_rows, res12)

res22 <- lapply(res2, function(x){return(x[[2]])})
espacios_deportivos22 <- Reduce(bind_rows, res22)

saveRDS(object = espacios_deportivos12, file = 'data/espacios_deportivos12.rds')
saveRDS(object = espacios_deportivos22, file = 'data/espacios_deportivos22.rds')

rm(res2,res12,espacios_deportivos12,res22,espacios_deportivos22)

res3 <- mclapply(31923:43952, obten_datos, mc.cores = 4)

res13 <- lapply(res3, function(x){return(x[[1]])})
espacios_deportivos13 <- Reduce(bind_rows, res13)

res23 <- lapply(res3, function(x){return(x[[2]])})
espacios_deportivos23 <- Reduce(bind_rows, res23)

saveRDS(object = espacios_deportivos13, file = 'data/espacios_deportivos13.rds')
saveRDS(object = espacios_deportivos23, file = 'data/espacios_deportivos23.rds')

rm(res3,res13,espacios_deportivos13,res23,espacios_deportivos23)

res4 <- mclapply(43953:55982, obten_datos, mc.cores = 4)

res14 <- lapply(res4, function(x){return(x[[1]])})
espacios_deportivos14 <- Reduce(bind_rows, res14)

res24 <- lapply(res4, function(x){return(x[[2]])})
espacios_deportivos24 <- Reduce(bind_rows, res24)

saveRDS(object = espacios_deportivos14, file = 'data/espacios_deportivos14.rds')
saveRDS(object = espacios_deportivos24, file = 'data/espacios_deportivos24.rds')

rm(res4,res14,espacios_deportivos14,res24,espacios_deportivos24)
