setwd("~/Documents/cad/ensanut2012/salud/")
system("ls")
library(foreign)
adultos <- read.spss('Adultos.sav')
adultos.df <- as.data.frame(adultos)
dim(adultos.df)
str(adultos.df)
summary(adultos.df$a301)
DIAB <- ifelse(adultos.df$a301=="S\xed",1,0)
adultos.df$diabetes <- factor(DIAB,levels=c(0,1),labels=c("No","Sí"))
summary(adultos.df$diabetes)

summary(adultos.df$sexo)
summary(adultos.df$edad)

mujeres <- adultos.df[adultos.df$sexo=="Mujer",]
hombres <- adultos.df[adultos.df$sexo=="Hombre",]



