paquetes <- lapply(c('dplyr','tidyr','readr','stringr','lubridate',
                     'RColorBrewer','lazyeval','ggplot2','foreign',
                     'gridExtra'),require,character.only=T)
mean(Reduce(rbind,paquetes))

adultos <- read.spss('Datos/Adultos.sav',to.data.frame = T, reencode = 'latin1')
str(adultos)

crea_catalogo <- function(tabla){
  cat <- attr(tabla, 'variable.labels')
  vars <- names(cat)
  names(cat_adultos) <- NULL
  data.frame(var=vars,descripcion=cat)
}

catalogo_adultos <- crea_catalogo(adultos)

vars_adultos_seleccionadas <- c('folio','intp','entidad','munici','locali',
                                'sexo','edad','')

adultos_1 <- adultos %>%
  select_(vars_adultos_seleccionadas)

antro <- read.spss('adultos_antropometria_2012.sav',to.data.frame = T,reencode = 'latin1')
variables_antro <- attr(antro,'variable.labels')
nombres <- names(variables_antro)
vars <- as.character(variables_antro)
vars[1] <- 'Folio del hogar'
catalogo_antro <- data.frame(nombres,vars)
anemia <- read.spss('anemia adultos mayores ensanut- 2012 nal.sav',
                    to.data.frame = T,reencode = 'latin1',
                    max.value.labels = 5, trim.factor.names = T)
variables_anemia <- attr(anemia,'variable.labels')

segalim <- read.spss('Seguridad_alimentaria.sav',to.data.frame = T, 
                     reencode = 'latin1')
variables_segalim <- attr(segalim,'variable.labels')
nombres <- names(variables_segalim)
vars <- as.character(variables_segalim)
vars[1] <- 'Folio del hogar'
catalogo_segalim <- data.frame(nombres,vars)


actfis <- read.spss('af_adultos_adole_2012.sav',to.data.frame=T,reencode='latin1')


frecuencia <- read.spss('Nutricion_distr_alimentos.sav',
                        to.data.frame = T,reencode='latin1')
