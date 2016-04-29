library(foreign)
library(Cairo)
library(dplyr)
library(ggplot2)
library(scales)

dir<-'Datos/adultos.sav'
adu<-read.spss(dir,to.data.frame = T,reencode = 'latin1')

adu$entidad<-as.numeric(adu$entidad)

pgtas_ind<-paste0("a201_",letters[1:7])
pgtas_diag<-paste0("a20",2:3)
comparaciones<-c("sexo","est_urb","entidad","edad")

Depresion<-adu[ !is.na(adu[["a201_a"]]) , c(pgtas_ind,pgtas_diag,comparaciones) ] %>%
            mutate_each(funs(as.numeric(.)-1),a201_a:a201_g) %>% mutate(a201_f=3-a201_f) %>%
            mutate(indice=rowSums(.[,1:7]),
                   posib_dep = as.numeric(cut( indice , breaks=c(-Inf,8,Inf) , labels=c(0,1) , right=F) )-1 ) %>%
            select(-one_of(pgtas_ind))

write.csv(Depresion, file = "Datos/infografia.csv")

variables <- attr(adu,'variable.labels')[c(pgtas_diag,comparaciones)]
nombres <- names(variables)
vars <- as.character(variables); vars[4]<-paste0(vars[4]," Metropolitano, urbano o rural")
vars[5]<-paste0(vars[5]," por orden alfabético. 1=AGS,..., 32=ZAC")
vars[7:8]<-c("Índice de depresion CESD-7","Señala si hay indicios de depresion. 0=No, 1=Sí")
nombres[7:8]<-c("indice","posib_dep")
catalogo_Depresion <- data.frame("Abreviacion"=nombres,"Significado"=vars)

sum(Depresion[["posib_dep"]])/length(Depresion[["posib_dep"]])
a<-prop.table(table(Depresion[,c("posib_dep","a202")]),1)
b<-prop.table(table(Depresion[Depresion[Depresion[,"posib_dep"]==1,"a202"]=="Sí","a203"]))
a[2,1]*b[1]

fig_edad<-ggplot(Depresion[Depresion[,"posib_dep"]==1,], aes(x=edad) ) + xlab("Edad")+
  geom_density()+ggtitle("Distribucion estimada de las personas con depresion por edad")+
  scale_y_continuous(name=" ",limits = c(0,0.05))

ggsave("Graficas/Infografia/edad_depresion.png",plot = fig_edad,w = 12, h = 8, units="in", type = "cairo-png")

fig_sex<-ggplot(Depresion,aes(x=sexo,fill=factor(posib_dep)))+
  geom_bar(position = 'fill')+
  xlab("")+coord_flip()+
  scale_y_continuous(name="",labels = percent)+
  ggtitle(label = "Depresión por género")+
  guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
  scale_fill_brewer(palette = "Set1",name="",labels=c("No muestra depresión","Muestra depresión"))+
  theme(text=element_text(face="bold"),legend.position="bottom",aspect.ratio=1)

ggsave("Graficas/Infografia/sexo_depresion.png",plot = fig_sex,w = 12, h = 8, units="in", type = "cairo-png")

fig_local<-ggplot(Depresion,aes(x=est_urb,fill=factor(posib_dep)))+
  geom_bar(position = 'fill')+
  xlab("")+coord_flip()+
  scale_y_continuous(name="",labels = percent)+
  ggtitle(label = "Depresión por tipo de localidad")+
  guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
  scale_fill_brewer(palette = "Set1",name="",labels=c("No muestra depresión","Muestra depresión"))+
  theme(text=element_text(face="bold"),legend.position="bottom",aspect.ratio=1)

ggsave("Graficas/Infografia/localidad_depresion.png",plot = fig_sex,w = 12, h = 8, units="in", type = "cairo-png")

# fig_local2<-ggplot(Depresion,aes(x=est_urb,fill=factor(a203)))+
#   geom_bar(position = 'fill')+
#   xlab("")+coord_flip()+
#   scale_y_continuous(name="",labels = percent)+
#   ggtitle(label = "Depresión por tipo de localidad")+
#   guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
#   scale_fill_brewer(palette = "Set1",name="",labels=c("No muestra depresión","Muestra depresión"))+
#   theme(text=element_text(face="bold"),legend.position="bottom",aspect.ratio=1)

prop.table(table(Depresion$est_urb,Depresion$a202),1)

prop.table(table(Depresion$est_urb,Depresion$a203),1)
