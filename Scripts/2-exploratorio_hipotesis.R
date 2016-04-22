library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(png)
library(RGraphics)

totales<-readRDS("Datos/totales_depurado.rds")
catalogo<-readRDS("Datos/catalogo.rds")

logo<-readPNG('logo_cad.png')
logo<-rasterGrob(logo,interpolate = TRUE)
n=nullGrob()

Depresion=totales[ !is.na(totales[,12]) , c(12:18,34:35,40) ]

for(i in 1:7){
  Depresion[,i]<-as.numeric(Depresion[,i])-1
}


###############################################################
####################### Hipotesis 1 ###########################
###############################################################

Subdiag<-Depresion %>% transmute(indice=Reduce(`+`,.[1:7])) %>%
          mutate(indice_depresion = cut( indice , breaks=c(-Inf,8,Inf) , labels=c(0,1) , right=F ),
                 diag=totales[!is.na(totales[,12]),20], sexo=totales[!is.na(totales[,12]),36],
                 grupo_Edad=totales[!is.na(totales[,12]),37], edad=totales[!is.na(totales[,12]),41])

fig_s<-ggplot(Subdiag, aes(x=sexo,fill=diag) ) + xlab("")+
  geom_bar(position="fill")+ggtitle("Mosaicos de diagnostico contra sexo")+
  scale_y_continuous(name="",labels=percent)+coord_flip()

fig_e<-ggplot(Subdiag[Subdiag[,"diag"]!="No",], aes(x=edad) ) + xlab("Edad")+
  geom_density()+ggtitle("Distribucion estimada de las personas diagnosticadas con depresion por edad")+
  scale_y_continuous(name=" ")

fig_i<-ggplot(Subdiag, aes(x=indice,fill=diag) ) + xlab("Indice")+
  geom_bar(position="fill")+ggtitle("Mosaicos de diagnostico contra indice de depresion")+
  scale_y_continuous(name="",labels=percent)+theme(legend.position="none")

Hip1<-grid.arrange(fig_s,fig_e,fig_i,layout_matrix=cbind(c(2,2,3),c(2,2,3),c(1,1,3)))
Hip1<-grid.arrange(logo,n,Hip1, layout_matrix=cbind( c(2,3,2),c(2,3,2),c(1,2,2)) ,
                   heights=c(0.6,7.8,0.1),widths=c(5,5,1) )

ggsave("Graficas/Hipotesis/Hip1.png", plot = Hip1, w = 12, h = 8, units="in")#, type = "cairo-png")

###############################################################
####################### Hipotesis 2 ###########################
###############################################################

NoNAObeTot= (!is.na(totales[,12]))&((!is.na(totales[,34])))&((!is.na(totales[,35])))
NoNAObeDep= ( ( !is.na(Depresion[,8]) ) )&( ( !is.na(Depresion[,9]) ) )

ObeDep<-Depresion[NoNAObeDep , ]%>%
  transmute(indice=Reduce(`+`,.[1:7]),StPer=a101h,StDes=a101ah) %>%
  mutate(diag=totales[NoNAObeTot,20], sexo=totales[NoNAObeTot,36], edad=totales[NoNAObeTot,41])

DiPer<-as.data.frame( prop.table(table(ObeDep[,c(4,2)]),1) )

stper<-ggplot(DiPer,aes(x=StPer,y=Freq,fill=diag))+
  geom_bar(stat = "identity",position="dodge")+
  xlab("Silueta Percibida de Stunkard")+scale_y_continuous(name="")+
  ggtitle("Comparacion de la distribucion de la Stunkard percibida contra diagnostico")+theme(legend.position="none")


DiDes<-as.data.frame( prop.table(table(ObeDep[,c(4,3)]),1) )

stdes<-ggplot(DiDes,aes(x=StDes,y=Freq,fill=diag))+
  geom_bar(stat = "identity",position="dodge")+
  xlab("Silueta Deseada de Stunkard")+scale_y_continuous(name="")+
  ggtitle("Comparacion de la distribucion de la Stunkard deseada contra diagnostico")+theme(legend.position="none")

DifStu<-ObeDep[,c(4,3)]
DifStu[,2]=(as.numeric(ObeDep[,3])-as.numeric(ObeDep[,2]))
DifStu<-as.data.frame(prop.table(table(DifStu),1))

DifSt<-ggplot(DifStu,aes(x=StDes,y=Freq,fill=diag))+
  geom_bar(stat = "identity",position="dodge")+
  xlab("Diferencia de posiciones entre Stunkard deseada y percibida")+scale_y_continuous(name="")+
  ggtitle("Comparacion de la distribucion de las diferencia entre Stunkard percibida y deseada contra diagnostico")

Hip2<-grid.arrange(stper,stdes,DifSt,layout_matrix=cbind( c(1,1,3),c(2,2,3) ) )
Hip2<-grid.arrange(logo,n,Hip2, layout_matrix=cbind( c(2,3,2),c(2,3,2),c(1,2,2)) ,
                   heights=c(0.6,7.8,0.1),widths=c(5,5,1) )
ggsave("Graficas/Hipotesis/Hip2.png", plot = Hip2, w = 12, h = 8, units="in")

Sexper<-as.data.frame( prop.table(table(ObeDep[,c(5,2)]),1) )
SePer<-ggplot(Sexper,aes(x=StPer,y=Freq,fill=sexo))+
  geom_bar(stat = "identity",position="dodge")+
  xlab("Silueta Percibida de Stunkard")+scale_y_continuous(name="")+
  ggtitle("Comparacion de la distribucion de la Stunkard percibida contra sexo")
SePer<-grid.arrange(logo,n,SePer, layout_matrix=cbind( c(2,3,2),c(2,3,2),c(1,2,2)) ,
                   heights=c(0.6,7.8,0.1),widths=c(5,5,1) )
ggsave("Graficas/Hipotesis/Hip2b.png", plot = SePer, w = 12, h = 8, units="in")


Subdiag<-Depresion %>% transmute(indice=Reduce(`+`,.[1:7]))




