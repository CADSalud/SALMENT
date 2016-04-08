library(foreign)
library(dplyr)
library(readr)

dir<-'~/CAD/ENSANUT_2012/Datos/adultos.sav'
adu<-read.spss(dir)

adu$entidad<-as.numeric(adu$entidad)

########################################
############## Variables ###############
########################################

pgta_autest<-c("a14c01",paste0("a14c02",1:7),paste0("a14c0",3:5))
pgta_depre<-c(paste0("a201_",letters[1:8]),paste0("a20",2:4))
pgta_accid<-c("a1101","a1106","a1201","a1203","a1207")#,"a1207esp")
pgta_fcrie<-c("a1301","a1303a","a1304","a1306",paste0("a131",1:2))
pgta<-c(pgta_autest,pgta_depre,pgta_accid,pgta_fcrie)
nom_pgta<-sapply(pgta,function(x) attr(adu,"variable.labels")[x])
comparaciones<-c("sexo","Edades","est_urb","est_marg","entidad")

attr(adu,"variable.labels")["Edades"]<-"Edades"
edades<-c(paste("De",c(seq(20,70,10)),"a",c(seq(29,79,10)),sep = " "),"80 o mas")
adu$Edades<-as.factor(cut(adu$edad,c(seq(20,80,10),121),labels=edades,right=FALSE))

######################################
############## Totales ###############
######################################

totales<-as.data.frame(adu[c(pgta,comparaciones)])

levels(totales[,26])<-c(rep("Otro",11),"Intento de suicidio","Otro","NS/NR")
niveles<-c(rep("Diario",3),rep("Semanalmente",3),rep("Mensualmente",2),
           rep("Anualmente",4),"Actualmente no toma","NS/NR")
levels(totales[,32])<-niveles
niveles<-c("Diario",rep("Semanalmente",3),rep("Mensualmente",2),
           rep("Anualmente",4),"Menos que anualmente","Nunca","NS/NR")
levels(totales[,33])<-niveles

################################################
########### Sentido de las preguntas ###########
################################################

positivo<-c(1:16,18,26,31)
negativo<-c(17,19:25,27:30,32:33)

totales_sentido<-totales
for(i in negativo){
  totales_sentido[,i]<-factor(totales[,i],levels=rev(levels(totales[,i])))
}


################################################
############## Depurar respuestas ##############
################################################

for(j in 1:length(pgta)){
  for(i in length( levels( totales_sentido[ ,pgta[j] ] ) ):1 ){
    if(levels( totales_sentido[ ,pgta[j] ] )[i] %in% c("No responde","No sabe", "NS/NR")){
      levels(totales_sentido[ ,pgta[j] ] )[i]<-NA
    }
  }
}

saveRDS(totales_sentido,"Datos/totales_depurado.rds")

########################################
########### Notas Variables ############
########################################

rm(list=c("niveles","pgta_fcrie","pgta_accid","pgta_depre","pgta_autest","dir","hog","edades","adu"))

a="Pregunta contestada por adultos mayores de 60 anos sin sustituto/proxy; 17% de la muestra"
t="Pregunta contestada por toda la muestra"
ntas<-rep(a,11)

a="Pregunta contestada por adultos sin sustituto/proxy; 97% de la muestra"
b="Pregunta contestada por adultos con sustituto/proxy; 3% de la muestra"
d="Pregunta contestada por los adultos que dijeron sufrir o haber sufrido depresion, 10% de la muestra"
ntas<-c(ntas,rep(a,7),b,t,d,d)

a="Pregunta contestada por los adultos que dijeron haber sufrido un accidente, 5% de la muestra"
b="Pregunta contestada por los adultos que dijeron haber sufrido un da?o por agresion, 2% de la muestra"
ntas<-c(ntas,t,a,t,b,b)

a="Pregunta contestada por los adultos que alguna vez han fumado, 47% de la muestra"
b="Pregunta contestada por los adultos que actualmente fuman, 19% de la muestra"
d="Pregunta contestada por los adultos alguna vez han consumido una bebida alcoholica, 76% de la muestra"
ntas<-c(ntas,t,a,b,b,d,d)

catalogo<-cbind(pgta,nom_pgta,ntas)
saveRDS(catalogo,"Datos/catalogo.rds")

rm(list = ls())
