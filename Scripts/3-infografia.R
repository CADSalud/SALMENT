library(foreign)
library(dplyr)
library(ggplot2)
library(scales)
library(svglite)
library(maptools)

############################# Crear Tabla de Depresion #################################################################
# dir<-'Datos/adultos.sav'
# adu<-read.spss(dir,to.data.frame = T,reencode = 'latin1')
# 
# adu$entidad<-as.numeric(adu$entidad)
# 
# pgtas_ind<-paste0("a201_",letters[1:7])
# pgtas_diag<-paste0("a20",2:3)
# pgtas_stunk<-c("a101h","a101m","a101ah","a101am")
# comparaciones<-c("sexo","est_urb","entidad","edad")
# 
# Depresion<-adu[ !is.na(adu[["a201_a"]]) , c(pgtas_ind,pgtas_diag,pgtas_stunk,comparaciones) ] %>%
#             mutate_each(funs(as.numeric(.)-1),a201_a:a201_g) %>% mutate(a201_f=3-a201_f) %>%
#             mutate(indice=rowSums(.[,1:7]),
#                    posib_dep = as.numeric(cut( indice , breaks=c(-Inf,8,Inf) , labels=c(0,1) , right=F) )-1 ) %>%
#             mutate(StPer=ifelse(is.na(a101h),as.numeric(a101m),as.numeric(a101h)),
#                    StDes=ifelse(is.na(a101ah),as.numeric(a101am),as.numeric(a101ah)))%>%
#             mutate(StPer=ifelse(StPer==10,NA,StPer), StDes=ifelse(StDes==10,NA,StDes))%>%
#             select(-one_of( c(pgtas_ind,pgtas_stunk) ))
# 
# variables <- attr(adu,'variable.labels')[c(pgtas_diag,"a101h","a101ah",comparaciones)]
# nombres <- names(variables);nombres[nombres=="a101h"]="StPer";nombres[nombres=="a101ah"]="StDes";
# vars <- as.character(variables); vars[vars==variables[["est_urb"]]]<-paste0(vars[vars==variables[["est_urb"]]],". Metropolitano, urbano o rural")
# vars[ vars==variables[["entidad"]] ]<-paste0(vars[vars==variables[["entidad"]]]," por orden alfabético. 1=AGS,..., 32=ZAC")
# vars[ (length(vars)+1):(length(vars)+2) ]<-c("Índice de depresion CESD-7","Señala si hay indicios de depresion. 0=No, 1=Sí")
# nombres[ (length(vars)-1):length(vars) ]<-c("indice","posib_dep")
# catalogo_Depresion <- data.frame("Abreviacion"=nombres,"Significado"=vars)
# 
# 
# Depresion<-Depresion[,nombres]
# 
# write.csv(Depresion, file = "Datos/infografia.csv")
# saveRDS(Depresion, file = "Datos/dep.rds")
# 
# #######################################################################
 #################### Primera infografia ###############################
#######################################################################
Depresion<-readRDS(file="Datos/dep.rds")

sum(Depresion[["posib_dep"]])/length(Depresion[["posib_dep"]])
a<-prop.table(table(Depresion[,c("posib_dep","a202")]),1)
b<-prop.table(table(Depresion[Depresion[Depresion[,"posib_dep"]==1,"a202"]=="Sí","a203"]))
a[2,1]*b[1]

fig_edad<-ggplot(Depresion[Depresion[,"posib_dep"]==1,], aes(x=edad) ) + xlab("Edad")+
  geom_density()+ggtitle("Distribucion estimada de las personas con depresion por edad")+
  scale_y_continuous(name=" ",limits = c(0,0.05))

ggsave("Graficas/Infografia/edad_depresion.svg",plot = fig_edad,w = 12, h = 8, units="in")

fig_sex<-ggplot(Depresion,aes(x=sexo,fill=factor(posib_dep)))+
  geom_bar(position = 'fill')+
  xlab("")+coord_flip()+
  scale_y_continuous(name="",labels = percent)+
  ggtitle(label = "Depresión por género")+
  guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
  scale_fill_brewer(palette = "Set1",name="",labels=c("No muestra depresión","Muestra depresión"))+
  theme(text=element_text(face="bold"),legend.position="bottom",aspect.ratio=1)

ggsave("Graficas/Infografia/sexo_depresion.svg",plot = fig_sex,w = 12, h = 8, units="in")

fig_local<-ggplot(Depresion,aes(x=est_urb,fill=factor(posib_dep)))+
  geom_bar(position = 'fill')+
  xlab("")+coord_flip()+
  scale_y_continuous(name="",labels = percent)+
  ggtitle(label = "Depresión por tipo de localidad")+
  guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch"))+
  scale_fill_brewer(palette = "Set1",name="",labels=c("No muestra depresión","Muestra depresión"))+
  theme(text=element_text(face="bold"),legend.position="bottom",aspect.ratio=1)

ggsave("Graficas/Infografia/localidad_depresion.svg",plot = fig_sex,w = 12, h = 8, units="in")

prop.table(table(Depresion$est_urb,Depresion$a202),1)

prop.table(table(Depresion$est_urb,Depresion$a203),1)


Depresion<- Depresion[( !is.na(Depresion$StPer) ) & ( !is.na(Depresion$StDes) ),]


Depresion$posib_dep2 <- as.ordered(levels(as.factor(Depresion$posib_dep) ))
# levels(a$posib_dep2)<-list(`Posible Depresión`=1,`No hay indicios`=0)

stper<-ggplot( Depresion ,aes(x=StPer,fill=factor(1-posib_dep) ))+
  geom_bar(position="fill")+
  xlab("Silueta Percibida de Stunkard")+
  scale_y_continuous(name="")+scale_x_discrete(breaks=c(1:9))+
  ggtitle("Comparación de Stunkard percibida vs diagnóstico")+
  scale_fill_manual(values=c('dodgerblue4','lightgoldenrod1'),
                    name='Diagnóstico \n de depresión',labels=c("Posible Depresión","No hay indicios")) + 
  theme(legend.position="right",
        legend.title = element_text(colour="black", size=10, face="bold"),
        axis.title = element_text(size=10),
        axis.text = element_text(color='black'),
        title=element_text(size=11,face='bold'))
ggsave("Graficas/Hipotesis/stper.svg", plot = stper, w = 10, h = 8, units="in")


Depresion$Dif <- as.numeric(Depresion$StDes)-as.numeric(Depresion$StPer)
DifSt2<-ggplot(Depresion%>%filter(Dif%in%c('-8','-7','-6','-5','-4','-3','-2','-1','0')),
               aes(x=Dif,fill=factor(1-posib_dep) ))+
  geom_bar(position="fill")+
  xlab("Diferencia de posiciones entre Stunkard deseada y percibida")+
  scale_y_continuous(name="")+
  scale_fill_manual(values=c('dodgerblue4','lightgoldenrod1'),
                    name='Diagnóstico \n de depresión',labels=c("Posible Depresión","No hay indicios")) + 
  ggtitle("Comparación entre Stunkard percibida y deseada vs diagnóstico") + 
  theme(legend.position="right",
        legend.title = element_text(colour="black", size=10, face="bold"),
        axis.title = element_text(size=10),
        axis.text = element_text(color='black'),
        title=element_text(size=11,face='bold'))
ggsave("Graficas/Hipotesis/DifSt2.svg", plot = DifSt2, w = 8, h = 8, units="in")

# Mapas de índice y % de depresión por estado
edo<-readShapeSpatial("estados_ligero/Mex_Edos")

edo@data$id<- rownames(edo@data)
edo_df<- edo %>%
  fortify(region="id") %>%
  mutate(id=as.numeric(id))

Subdiag_2 <- Depresion[,c("entidad","indice")] %>% transmute(id=entidad-1)
Subdiag_2$indice <- Depresion$indice
Subdiag_3 <- Subdiag_2 %>% mutate(`Depresión` = 1*(indice > 7)) %>%
  #mutate_each(funs(scale),-id) %>%
  group_by(id) %>%
  summarise_each(funs(mean)) 

edo_subdiag <- left_join(edo_df, Subdiag_3, by = 'id') 

PorDepMap<-ggplot(data = edo_subdiag, aes(long, lat, group=group)) + 
  geom_polygon(aes(fill = `Depresión`, group = group),color='black') + 
  coord_fixed() + theme_nothing(legend = TRUE) +
  scale_fill_gradient2(low='green',mid='palegoldenrod',high='red',
                       breaks=pretty_breaks(), labels=percent,
                       midpoint = 0.17,name='Tasa de depresión\n estatal') +
  ggtitle('Depresión por entidad') +
  theme(legend.position="right",legend.key.height = unit(2, "cm"),
        legend.key.width = unit(0.9,'cm'),
        legend.title = element_text(colour="black", size=10, face="bold"),
        title=element_text(size=11,face='bold'))
ggsave("Graficas/Hipotesis/PorDepMap.svg", plot = PorDepMap, w = 12, h = 8, units="in")
