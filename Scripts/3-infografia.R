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

fig_edad <- Depresion %>% 
  select(edad,posib_dep) %>%
  group_by(edad) %>%
  summarise(np = n(), posib_dep = mean(posib_dep)) %>%
  filter(np >= 10 & edad <= 85) %>%
  ggplot(aes(x = edad, y = posib_dep)) + 
  xlab("Edad") +
  #geom_point(aes(size = np)) + 
  geom_smooth(method = 'loess', se = FALSE) + 
  scale_size(name='Número de \n encuestados',breaks=c(30,100,300,600,900,1200)) +
  scale_y_continuous(name="Porcentaje de deprimidos",labels = percent) +
  ggtitle("Porcentaje de deprimidos por edad") +
  theme_classic()

ggsave("Graficas/Infografia/edad_depresion.svg",plot = fig_edad,w = 12, h = 8, units="in")

fig_sex <- ggplot(Depresion,aes(x=sexo,fill=factor(posib_dep))) +
  geom_bar(position = 'fill') +
  xlab("") +
  coord_flip() +
  scale_y_continuous(name="",labels = percent) +
  ggtitle(label = "Depresión por género") +
  guides(fill = guide_legend(keywidth=0.2, keyheight=0.2, default.unit="inch")) +
  scale_fill_brewer(palette = "Set1",name="", labels=c("No muestra depresión","Muestra depresión")) +
  theme_classic() +
  theme(text=element_text(face="bold"),legend.position="bottom",aspect.ratio=1) 

ggsave("Graficas/Infografia/sexo_depresion.svg",plot = fig_sex,w = 12, h = 8, units="in")

fig_local <- ggplot(Depresion,aes(x=est_urb,fill=factor(posib_dep))) +
  geom_bar(position = 'fill') +
  xlab("") +
  coord_flip() +
  scale_y_continuous(name="",labels = percent) +
  ggtitle(label = "Depresión por tipo de localidad") +
  guides(fill=guide_legend(keywidth=0.2,keyheight=0.2,default.unit="inch")) +
  scale_fill_brewer(palette = "Set1",name="",labels=c("No muestra depresión","Muestra depresión")) +
  theme_classic() +
  theme(text=element_text(face="bold"),legend.position="bottom",aspect.ratio=1)

ggsave("Graficas/Infografia/localidad_depresion.svg",plot = fig_local,w = 12, h = 8, units="in")

prop.table(table(Depresion$est_urb,Depresion$a202),1)

prop.table(table(Depresion$est_urb,Depresion$a203),1)


Depresion<- Depresion[( !is.na(Depresion$StPer) ) & ( !is.na(Depresion$StDes) ),]

auxDep <- Depresion
auxDep$StPer <- as.factor(auxDep$StPer)
stper <- ggplot(auxDep, aes(x = StPer, fill = factor(1-posib_dep))) +
  geom_bar(position="fill") +
  xlab("Silueta Percibida de Stunkard") +
  scale_y_continuous(name="") +
  scale_x_discrete(breaks = c(1:9), labels = 1:9) +
  ggtitle("Comparación de Stunkard percibida vs diagnóstico") +
  scale_fill_manual(values=c('dodgerblue4','lightgoldenrod1'),
                    name='Diagnóstico \n de depresión',labels=c("Posible Depresión","No hay indicios")) + 
  theme_classic() +
  theme(legend.position="right",
        legend.title = element_text(colour="black", size=10, face="bold"),
        axis.title = element_text(size=10),
        axis.text = element_text(color='black'),
        title=element_text(size=11,face='bold'))
ggsave("Graficas/Infografia/stper.svg", plot = stper, w = 10, h = 8, units="in")


Depresion$Dif <- as.numeric(Depresion$StDes)-as.numeric(Depresion$StPer)
aux2 <- Depresion%>%filter(Dif%in%c('-8','-7','-6','-5','-4','-3','-2','-1','0'))
aux2$Dif <- as.factor(aux2$Dif)
DifSt2 <- ggplot(aux2,aes(x=Dif,fill=factor(1-posib_dep) ))+
  geom_bar(position="fill") +
  xlab("Diferencia de posiciones entre Stunkard deseada y percibida")+
  scale_y_continuous(name="") +
  scale_fill_manual(values=c('dodgerblue4','lightgoldenrod1'),
                    name='Diagnóstico \n de depresión',labels=c("Posible Depresión","No hay indicios")) + 
  ggtitle("Comparación entre Stunkard percibida y deseada vs diagnóstico") + 
  theme_classic() +
  theme(legend.position="right",
        legend.title = element_text(colour="black", size=10, face="bold"),
        axis.title = element_text(size=10),
        axis.text = element_text(color='black'),
        title=element_text(size=11,face='bold')) 
ggsave("Graficas/Infografia/DifSt2.svg", plot = DifSt2, w = 10, h = 8, units="in")

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

PorDepMap <- ggplot(data = edo_subdiag, aes(long, lat, group=group)) + 
  geom_polygon(aes(fill = `Depresión`, group = group),color='black') + 
  coord_fixed() +
  scale_fill_gradient2(low='aliceblue',mid='lightskyblue3',high='midnightblue',
                       breaks=pretty_breaks(), labels=percent,
                       midpoint = 0.17,name='Tasa de depresión\n estatal') +
  ggtitle('Depresión por entidad') +
  theme(legend.position="right",legend.key.height = unit(2, "cm"),
        legend.key.width = unit(0.9,'cm'),
        legend.title = element_text(colour="black", size=10, face="bold"),
        title=element_text(size=11,face='bold')) +
  theme_classic()
ggsave("Graficas/Infografia/PorDepMap.svg", plot = PorDepMap, w = 12, h = 8, units="in")
