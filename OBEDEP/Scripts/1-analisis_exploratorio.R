library(png)
library(Cairo)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(RGraphics)
library(dplyr)
library(readr)
library(maptools)
library(lazyeval)
library(ggmap)

catalogo<-readRDS("Datos/catalogo.rds")
totales<-readRDS("Datos/totales_depurados.rds")
comparaciones<-c("sexo","Edades","est_urb","est_marg","entidad")
mayores<-c(paste("De",c(seq(60,70,10)),"a",c(seq(69,79,10)),sep = " "),"80 o mas")

##################################################
################ Graficas Totales ################
##################################################

p_totales=list()

for(i in 12:33){
  dat<-as.data.frame(na.omit(totales[,catalogo[i,1]]))
  names(dat)<-catalogo[i,1]
  p_totales[[i-11]]<-ggplot(dat)+aes_string(x=catalogo[i,1],fill=catalogo[i,1])+
    geom_bar(aes(y=..count../sum(..count..)))+
    scale_x_discrete(name="",labels=NULL,breaks=NULL)+
    scale_y_continuous(name="",labels=percent)+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_totales[[i-11]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Totales/pgta",i-11,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
}

for(i in 1:11){
  dat<-na.omit(totales[totales[,"Edades"]%in%mayores,c(catalogo[i,1],"sexo")])
  p_totales[[i+22]]<-ggplot(dat)+aes_string(x=catalogo[i,1],fill=catalogo[i,1])+
    geom_bar(aes(y=..count../sum(..count..)))+
    scale_x_discrete(name="",labels=NULL,breaks=NULL)+
    scale_y_continuous(name="",labels=percent)+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_totales[[i+22]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Totales/pgta",i+22,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
}

#################################################
################# Graficas Sexo #################
#################################################

p_sex=list()

for(i in 12:33){
  dat<-na.omit(totales[,c(catalogo[i,1],"sexo")])
  p_sex[[i-11]]<-ggplot(data=dat,aes(x=sexo))+
    aes_string(fill=catalogo[i,1])+
    geom_bar(position = 'fill')+
    xlab("")+coord_flip()+
    scale_y_continuous(name="",labels = percent)+
    ggtitle(label = "")+
    guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_sex[[i-11]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Sexo/pgta",i-11,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
  p_sex[[i-11]]<-p_sex[[i-11]]+theme(legend.position="none")
}

for(i in 1:11){
  dat<-na.omit(totales[totales[,"Edades"]%in%mayores,c(catalogo[i,1],"sexo")])
  p_sex[[i+22]]<-ggplot(data=dat,aes(x=sexo))+
    aes_string(fill=catalogo[i,1])+
    geom_bar(position = 'fill')+
    xlab("")+coord_flip()+
    scale_y_continuous(name="",labels = percent)+
    ggtitle(label = "")+
    guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_sex[[i+22]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Sexo/pgta",i+22,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
  p_sex[[i+22]]<-p_sex[[i+22]]+theme(legend.position="none")
}

#################################################
########### Graficas Tipo de Localidad ##########
#################################################

p_urb=list()

for(i in 12:33){
  dat<-na.omit(totales[,c(catalogo[i,1],"est_urb")])
  p_urb[[i-11]]<-ggplot(data=dat,aes(x=est_urb))+
    aes_string(fill=catalogo[i,1])+
    geom_bar(position = 'fill')+
    xlab("")+coord_flip()+
    scale_y_continuous(name="",labels = percent)+
    ggtitle(label = "")+
    guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_urb[[i-11]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Localidad/pgta",i,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
  p_urb[[i-11]]<-p_urb[[i-11]]+theme(legend.position="none")
}

for(i in 1:11){
  dat<-na.omit(totales[totales[,"Edades"]%in%mayores,c(catalogo[i,1],"est_urb")])
  p_urb[[i+22]]<-ggplot(data=dat,aes(x=est_urb))+
    aes_string(fill=catalogo[i,1])+
    geom_bar(position = 'fill')+
    xlab("")+coord_flip()+
    scale_y_continuous(name="",labels = percent)+
    ggtitle(label = "")+
    guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_urb[[i+22]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Localidad/pgta",i+22,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
  p_urb[[i+22]]<-p_urb[[i+22]]+theme(legend.position="none")
}

#################################################
############## Graficas Marginacion #############
#################################################

p_marg=list()

for(i in 12:33){
  dat<-na.omit(totales[,c(catalogo[i,1],"est_marg")])
  p_marg[[i-11]]<-ggplot(data=dat,aes(x=est_marg))+
    aes_string(fill=catalogo[i,1])+
    geom_bar(position = 'fill')+
    xlab("")+coord_flip()+
    scale_y_continuous(name="",labels = percent)+
    ggtitle(label = "")+
    guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_marg[[i-11]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Marginacion/pgta",i,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
  p_marg[[i-11]]<-p_marg[[i-11]]+theme(legend.position="none")
}

for(i in 1:11){
  dat<-na.omit(totales[totales[,"Edades"]%in%mayores,c(catalogo[i,1],"est_marg")])
  p_marg[[i+22]]<-ggplot(data=dat,aes(x=est_marg))+
    aes_string(fill=catalogo[i,1])+
    geom_bar(position = 'fill')+
    xlab("")+coord_flip()+
    scale_y_continuous(name="",labels = percent)+
    ggtitle(label = "")+
    guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_marg[[i+22]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Marginacion/pgta",i+22,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
  p_marg[[i+22]]<-p_marg[[i+22]]+theme(legend.position="none")
}

#################################################
############### Graficas de Edades ##############
#################################################

p_edades=list()

for(i in 12:33){
  dat<-na.omit(totales[,c(catalogo[i,1],"Edades")])
  p_edades[[i-11]]<-ggplot(data=dat,aes(x=Edades))+
    aes_string(fill=catalogo[i,1])+
    geom_bar(position = 'fill')+
    xlab("")+coord_flip()+
    scale_y_continuous(name="",labels = percent)+
    ggtitle(label = "")+
    guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_edades[[i-11]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Edades/pgta",i,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
  p_edades[[i-11]]<-p_edades[[i-11]]+theme(legend.position="none")
}

for(i in 1:11){
  dat<-na.omit(totales[totales[,"Edades"]%in%mayores,c(catalogo[i,1],"Edades")])
  p_edades[[i+22]]<-ggplot(data=dat,aes(x=Edades))+
    aes_string(fill=catalogo[i,1])+
    geom_bar(position = 'fill')+
    xlab("")+coord_flip()+
    scale_y_continuous(name="",labels = percent)+
    ggtitle(label = "")+
    guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
    scale_fill_brewer(palette = "Set1",name="Respuesta")+
    theme(text=element_text(face="bold"))
  m<-grid.arrange(p_edades[[i+22]], top=catalogo[i,2],bottom=catalogo[i,3])
  ggsave(paste0("Graficas/Edades/pgta",i+22,".png"), plot = m, w = 12, h = 8, units="in", type = "cairo-png")
  p_edades[[i+22]]<-p_edades[[i+22]]+theme(legend.position="none")
}


##################################################
############### Graficas Arregladas ##############
##################################################


logo<-readPNG('~/CAD/logo_cad.png')
logo<-rasterGrob(logo,interpolate = TRUE)

n=nullGrob()

for (i in 1:22) {
  m[[i]]<-grid.arrange(n,logo,arrangeGrob(p_totales[[i]],p_sex[[i]],p_urb[[i]],p_marg[[i]],p_edades[[i]],
                                  layout_matrix=cbind(c(5,5,2),c(1,1,3),c(1,1,4)),
                                  top=catalogo[i+11,2],bottom=textGrob(catalogo[i+11,3])),
               layout_matrix=cbind(c(1,3,1),c(1,3,1),c(2,1,1)),heights=c(0.6,7.8,0.1),widths=c(5,5,1))
  ggsave(paste0("Graficas/Arregladas/pgta",i,".png"), plot = m[[i]], w = 12, h = 8, units="in", type = "cairo-png")
}

for (i in 23:33) {
  m[[i]]<-grid.arrange(n,logo,arrangeGrob(p_totales[[i]],p_sex[[i]],p_urb[[i]],p_marg[[i]],p_edades[[i]],
                                     layout_matrix=cbind(c(5,5,2),c(1,1,3),c(1,1,4)),
                                     top=catalogo[i-22,2],bottom=textGrob(catalogo[i-22,3])),
                  layout_matrix=cbind(c(1,3,1),c(1,3,1),c(2,1,1)),heights=c(0.6,7.8,0.1),widths=c(5,5,1))
  ggsave(paste0("Graficas/Arregladas/pgta",i,".png"), plot = m[[i]], w = 12, h = 8, units="in", type = "cairo-png")
}

pdf("PDFs/Reporte_Ensanut_2012.pdf", width = 11, height = 8.5)
grid.arrange(m[[1]],top="1. Preguntas de Depresion")
for (i in 2:11) {
  plot.new()
  grid.draw(m[[i]])
}

grid.arrange(m[[12]],top="2. Preguntas de Accidentes")
plot.new()
grid.draw(m[[13]])

grid.arrange(m[[14]],top="3. Preguntas de Agresion y violencia")
plot.new()
grid.draw(m[[15]])
plot.new()
grid.draw(m[[16]])

grid.arrange(m[[17]],top="4. Preguntas de Factores de Riesgo")
for (i in 18:22) {
  plot.new()
  grid.draw(m[[i]])
}

grid.arrange(m[[23]],top="5. Preguntas de Autoestima Adultos Mayores")
for (i in 24:33) {
  plot.new()
  grid.draw(m[[i]])
}
dev.off()

##################################################
################ Graficas de Mapas ###############
##################################################

edo<-readShapeSpatial("estados_ligero/Mex_Edos")

edo@data$id<- rownames(edo@data)
edo_df<- edo %>%
  fortify(region="id") %>%
  mutate(id=as.numeric(id))

mapa<-list()
Mapa<-list()
val<-list()

logo<-readPNG('logo_cad.png')
logo<-rasterGrob(logo,interpolate = TRUE)
n=nullGrob()

for(i in c(2:8)){
  val[[i]]<- c("Insatisfecho(a)","Muy insatisfecho(a)")
}

for (i in c(1,9:11)) {
  val[[i]]<- c("No")
}

for (i in c(12:16,18)) {
  val[[i]]<- c("Un numero de veces considerable (3 - 4 dias)","Todo el tiempo o la mayoria del tiempo (5 - 7 dias)")
}

for (i in c(19:23,25,28)) {
  val[[i]]<- c("Si")
}


for(i in c(29,32:33)){
  val[[i]]<-c("Diario")
}

for(i in c(24,27)){
  val[[i]]<- c("alcohol?","drogas?","ambos?")
}

val[[17]]<-c("Rara vez o nunca (menos de un dia)","Pocas veces o alguna vez (1 - 2 dias)")
val[[26]]<-c("Intento de suicidio")
val[[30]]<- c("Diario","Primeros 5 miutos","Entre 6 y 30 minutos","Entre 31 y 60 minutos")
val[[31]]<- c("Nunca","Pocas veces")

for (i in 12:33) {
  resumen<-totales%>%
    group_by(entidad) %>%
    summarise_(Total = ~n(), Dep=interp(~sum(var%in%val[[i]], na.rm = TRUE),var=as.name(catalogo[i,1]))) %>%
    mutate(Porcentaje=Dep/Total,id=entidad-1,entidad=NULL) %>%
    right_join(edo_df,by="id")
  
  if (length(val[[i]])>1){
    Nota<-paste(val[[i]][1:length(val[[i]])-1],sep=", ", collapse=", ")
    Nota<-paste0(Nota," o ",val[[i]][length(val[[i]])])
    Nota<-paste("Porcentaje de personas por entidad que contestaron:",Nota)
  }else{
    Nota<-paste(val[[i]],sep=", ", collapse=", ")
    Nota<-paste("Porcentaje de personas por entidad que contestaron:",Nota)
  }
  
  
  mapa[[i]]<-ggplot(resumen,aes(long,lat,group=group))+
    geom_polygon(aes(fill=Porcentaje),color="black")+coord_map()+
    scale_fill_distiller(breaks=pretty_breaks(n=5),labels=percent,direction =1)+
    theme_nothing(legend = TRUE)+
    labs(title=paste0("\n ",catalogo[i,2],"\n"),fill="")
  
  Mapa[[i-11]]<-grid.arrange(arrangeGrob(n,logo,mapa[[i]],bottom=textGrob(Nota),
                                      layout_matrix=cbind(c(1,3,1),c(1,3,1),c(2,1,1)),heights=c(0.6,7.8,0.1),
                                      widths=c(5,5,1)),bottom=textGrob(catalogo[i,3]))
  
  ggsave(Mapa[[i-11]], file = paste0("Graficas/Mapas/mapa",i-11,".png"),
         width = 11, height = 8.5, type = "cairo-png")
}

for (i in 1:11) {
  resumen<-totales%>%
    group_by(entidad) %>%
    summarise_(Total = ~n(), Dep=interp(~sum(var%in%val[[i]], na.rm = TRUE),var=as.name(catalogo[i,1]))) %>%
    mutate(Porcentaje=Dep/Total,id=entidad-1,entidad=NULL) %>%
    right_join(edo_df,by="id")
  
  if (length(val[[i]])>1){
    Nota<-paste(val[[i]][1:length(val[[i]])-1],sep=", ", collapse=", ")
    Nota<-paste0(Nota," o ",val[[i]][length(val[[i]])])
    Nota<-paste("Porcentaje de personas por entidad que contestaron:",Nota)
  }else{
    Nota<-paste(val[[i]],sep=", ", collapse=", ")
    Nota<-paste("Porcentaje de personas por entidad que contestaron:",Nota)
  }
  
  
  mapa[[i]]<-ggplot(resumen,aes(long,lat,group=group))+
    geom_polygon(aes(fill=Porcentaje),color="black")+coord_map()+
    scale_fill_distiller(breaks=pretty_breaks(n=5),labels=percent,direction =1)+
    theme_nothing(legend = TRUE)+
    labs(title=paste0("\n ",catalogo[i,2],"\n"),fill="")
  
  Mapa[[i+22]]<-grid.arrange(arrangeGrob(n,logo,mapa[[i]],bottom=textGrob(Nota),
                                      layout_matrix=cbind(c(1,3,1),c(1,3,1),c(2,1,1)),heights=c(0.6,7.8,0.1),
                                      widths=c(5,5,1)),bottom=textGrob(catalogo[i,3]))
  
  ggsave(Mapa[[i+22]], file = paste0("Graficas/Mapas/mapa",i+22,".png"),
         width = 11, height = 8.5, type = "cairo-png")
}

pdf("PDFs/Entidad_Ensanut_2012.pdf", width = 11, height = 8.5)
grid.arrange(Mapa[[1]],top="1. Preguntas de Depresion")
for (i in 2:11) {
  plot.new()
  grid.draw(Mapa[[i]])
}

grid.arrange(Mapa[[12]],top="2. Preguntas de Accidentes")
plot.new()
grid.draw(Mapa[[13]])

grid.arrange(Mapa[[14]],top="3. Preguntas de Agresion y violencia")
plot.new()
grid.draw(Mapa[[15]])
plot.new()
grid.draw(Mapa[[16]])

grid.arrange(Mapa[[17]],top="4. Preguntas de Factores de Riesgo")
for (i in 18:22) {
  plot.new()
  grid.draw(Mapa[[i]])
}

grid.arrange(Mapa[[23]],top="5. Preguntas de Autoestima Adultos Mayores")
for (i in 24:33) {
  plot.new()
  grid.draw(Mapa[[i]])
}
dev.off()

rm(list=ls())