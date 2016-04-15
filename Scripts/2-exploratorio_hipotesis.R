library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

totales<-readRDS("Datos/totales_depurado.rds")
catalogo<-readRDS("Datos/catalogo.rds")

Depresion=totales[ !is.na(totales[,12]) , c(12:18,34:35) ]

for(i in 1:7){
  Depresion[,i]<-as.numeric(Depresion[,i])-1
}


Subdiag<-Depresion %>% transmute(indice=Reduce(`+`,.[1:7])) %>%
          mutate(indice_depresion = cut( indice , breaks=c(-Inf,8,Inf) , labels=c(0,1) , right=F ),
                 diag=totales[!is.na(totales[,12]),20], sexo=totales[!is.na(totales[,12]),36],
                 grupo_Edad=totales[!is.na(totales[,12]),37], edad=totales[!is.na(totales[,12]),41])

fig_s<-ggplot(Subdiag, aes(x=sexo,fill=diag) ) + xlab("")+
  geom_bar(position="fill")+ggtitle("Mosaicos de diagnostico contra sexo")+
  scale_y_continuous(name="",labels=percent)+coord_flip()

fig_e<-ggplot(Subdiag[Subdiag[,"diag"]=="Sí",], aes(x=edad) ) + xlab("Edad")+
  geom_density()+ggtitle("Distribucion estimada de las personas diagnosticadas con depresion por edad")+
  scale_y_continuous(name=" ")

fig_i<-ggplot(Subdiag, aes(x=indice,fill=diag) ) + xlab("Indice")+
  geom_bar(position="fill")+ggtitle("Mosaicos de diagnostico contra indice de depresion")+
  scale_y_continuous(name="",labels=percent)+theme(legend.position="none")

grid.arrange(fig_s,fig_e,fig_i,layout_matrix=cbind(c(2,2,3),c(2,2,3),c(1,1,3)))

NoNAObeTot= (!is.na(totales[,12]))&((!is.na(totales[,34])))&((!is.na(totales[,35])))
NoNAObeDep= ( ( !is.na(Depresion[,8]) ) )&( ( !is.na(Depresion[,9]) ) )

ObeDep<-Depresion[NoNAObeDep , ]%>%
  transmute(indice=Reduce(`+`,.[1:7]),StPer=a101h,StDes=a101ah) %>%
  mutate(diag=totales[NoNAObeTot,20], sexo=totales[NoNAObeTot,36], edad=totales[NoNAObeTot,41])

a<-as.data.frame( prop.table(table(ObeDep[,c(4,2)]),1) )

stper<-ggplot(a,aes(x=StPer,y=Freq,fill=diag))+
  geom_bar(stat = "identity",position="dodge")+
  xlab("Silueta Percibida de Stunkard")+scale_y_continuous(name="")+
  ggtitle("Comparacion de la distribucion del Stunkard percibido contra diagnostico")+theme(legend.position="none")


b<-as.data.frame( prop.table(table(ObeDep[,c(4,3)]),1) )

stdes<-ggplot(b,aes(x=StDes,y=Freq,fill=diag))+
  geom_bar(stat = "identity",position="dodge")+
  xlab("Silueta Deseada de Stunkard")+scale_y_continuous(name="")+
  ggtitle("Comparacion de la distribucion del Stunkard deseada contra diagnostico")+theme(legend.position="none")

c<-ObeDep[,c(4,3)]
c[,2]=(as.numeric(ObeDep[,3])-as.numeric(ObeDep[,2]))
c<-as.data.frame(prop.table(table(c),1))


DifSt<-ggplot(c,aes(x=StDes,y=Freq,fill=diag))+
  geom_bar(stat = "identity",position="dodge")+
  xlab("Diferencia de posiciones entre Stunkard deseado y percibido")+scale_y_continuous(name="")+
  ggtitle("Comparacion de la distribucion de las diferencia entre Stunkard percibido y deseado contra diagnostico")

grid.arrange(stper,stdes,DifSt,layout_matrix=cbind( c(1,1,3),c(2,2,3) ) )
