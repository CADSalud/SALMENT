# library(haven)
# x<-read_spss(dir_adu)
# attributes(x$sexo)
# attributes(x$sexo)$labels
# dir_adu<-'../salud/Adultos.sav'
# adu<-read.spss(dir_adu,to.data.frame = T)
# dir_hog='../salud/Hogar.sav'
# hog=read.spss(dir_hog,to.data.frame = T)
# attributes(adu)$variable.labels

library(readr)
library(catalogo)

totales <- read_rds('../data/totales_depurado.rds')
catalogo <- read_rds('../data/catalogo.rds') %>% as.data.frame()
cols_totales <- colnames(totales)
cols_catalogo <- colnames(catalogo)
for(col in cols_totales){
  totales[[col]] <- iconv(totales[[col]],from='latin1',to='utf-8')
}
for(col in cols_catalogo){
  catalogo[[col]] <- iconv(catalogo[[col]],from='latin1',to='utf-8')
}


