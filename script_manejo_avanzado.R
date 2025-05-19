
#################################################
# SCRIPT CURSO MANEJO AVANZADO DE DATOS CON R
#################################################

# Local (mi ordenador)
# R version 4.4.0 (2024-04-24) -- "Puppy Cup"

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/Manejo_avanzado/MANEJO_AVANZADO_DATOS_R_ONLINE-main")


datos <- read.table("datos/datos.curso1.txt",header=T)

dim(datos)

head(datos)

str(datos)



# RECODIFICACION

# Recodificacion variable numerica

str(datos)
range(datos$"edad")
min(datos$"edad")
max(datos$"edad")
class(datos$"edad")

datos$"edad_gr" <- datos$"edad"
datos$"edad_gr"[datos$"edad"<20]<- "[0-20)"
datos$"edad_gr"[datos$"edad">=20 & datos$"edad"<40]<- "[20-40)"
datos$"edad_gr"[datos$"edad">=40 & datos$"edad"<60]<- "[40-60)"

table(datos$"edad_gr",exclude=NULL)
class(datos$"edad_gr")

# Recodificacion de variable caracter

datos$"estado.civil_new"<-datos$"estado.civil"
datos$"estado.civil_new"[datos$"estado.civil"%in%"Divorciado"] <- "divor"

table(datos$"estado.civil_new",exclude=NULL)
table(datos$"estado.civil",datos$"estado.civil_new",exclude=NULL)


# Recodificacion variable numerica con CUT

seq(0,100,20). # 0  20  40  60  80 100

datos$"edad_gr_cut" <- cut(x=datos$"edad", 
breaks=seq(0,100,20),  # c(0,20,50,60,80,100)
right=F,
include.lowest=T)

table(datos$"edad_gr_cut",exclude=NULL)



terciles <- quantile(datos$"edad",prob=seq(0,1,1/3)) # c(1,29,54,85)

datos$"edad_gr_ter" <- cut(x=datos$"edad", 
breaks=terciles,
right=F,
include.lowest=T)

rm(terciles) # borro el objeto terciles del workspace

table(datos$"edad_gr_ter",exclude=NULL)


cuartiles <- quantile(datos$"edad",prob=seq(0,1,1/4)) # 

datos$"edad_gr_cuar" <- cut(x=datos$"edad", 
breaks=cuartiles,
right=F,
include.lowest=T)

rm(terciles) # borro el objeto terciles del workspace
table(datos$"edad_gr_cuar",exclude=NULL)



class(datos$"edad_gr_ter")
datos$"edad_gr_ter_character" <- as.character(datos$"edad_gr_ter")

levels(datos$"edad_gr_ter")
table(datos$"edad_gr_ter",exclude=NULL)


# Recodificacion variable caracter con factor/levels...

datos$"estado.civil.factor"<-as.factor(datos$"estado.civil")

# Cambiar el orden de las etiquetas (factor)

levels(datos$"estado.civil.factor")

datos$"estado.civil.factor"<-factor(datos$"estado.civil.factor",
levels=c("Soltero","Casado","Divorciado"))
table(datos$"estado.civil.factor",exclude=NULL)

# Cambiar el valor de las etiquetas (levels)

levels(datos$"estado.civil.factor")[1] <- c("Sol")

levels(datos$"estado.civil.factor")[c(2,3)] <- c("Cas","Div")

# levels(datos$"estado.civil.factor")<-c("Sol","Cas","Div")

datos$"estado.nuevo"<-as.character(datos$"estado.civil.factor")


levels(datos$"edad_gr_ter")
levels(datos$"edad_gr_ter")<-c("jovenes","adultos","mayores")
levels(datos$"edad_gr_ter")

datos$"edad_gr_ter"<-factor(datos$"edad_gr_ter",levels=c("mayores","jovenes","adultos"))
table(datos$"edad_gr_ter")



# Combinaciones de variables


datos$"estado.civil"<-as.factor(datos$"estado.civil")
datos$"nivel.estudios" <- as.factor(datos$"nivel.estudios")

datos$"comb"<-interaction(datos$"estado.civil", datos$"nivel.estudios")

table(datos$"comb",exclude=NULL)


