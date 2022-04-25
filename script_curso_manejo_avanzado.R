


rm(list=ls())
gc()


setwd("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DATOS_R_ONLINE-main/datos/")

load("datos.curso1.RData")

# RECODIFICACION CLASICA VARIABLE CARACTER

table(datos$"estado.civil")

datos$"estado.civil.new" <- datos$"estado.civil"

datos$"estado.civil.new" [datos$"estado.civil.new"%in%"Casado"] <- "cas"

table(datos$estado.civil.new)


# RECODIFICACION VARIABLE CUANTITATIVA (CUT)

range(datos$"edad")
min(datos$"edad")
max(datos$"edad")

gr<-cut(x=datos$"edad", breaks=seq(0,80,20),right=F,include.lowest=T)
table(gr,exclude=NULL)

gr<-cut(x=datos$"edad", breaks=seq(0,100,20),right=F,include.lowest=T)
table(gr,exclude=NULL)

gr<-cut(x=datos$"edad", breaks=c(0,20,40,60,80,85),right=F,include.lowest=T)
table(gr,exclude=NULL)


datos$"gr.edad"<-gr

class(datos$"gr.edad")
levels(datos$"gr.edad")
table(datos$"gr.edad",exclude=NULL)

# VARIABLES TIPO FACTOR

class(datos$estado.civil)

table(datos$estado.civil)

estado.civil.factor <- as.factor(datos$estado.civil)

levels(estado.civil.factor)

# cambiar el orden de las categorias de una varible factor
nuevo.orden<- c("Divorciado", "Soltero", "Casado")
estado.civil.factor.new<- factor(estado.civil.factor,levels=nuevo.orden)
levels(estado.civil.factor.new)


# Recodificacion levels

levels(estado.civil.factor)

levels(estado.civil.factor) <- c("cas", "div", "sol")
table(estado.civil.factor)

levels(estado.civil.factor)[2]<-c("divo")
table(estado.civil.factor)

estado.civil.caracter <- as.character(estado.civil.factor)

# Combinacion de variables TIPO FACTOR

datos$"estado.civil.NEW"<-as.factor(datos$"estado.civil")
datos$"nivel.estudios.NEW" <- as.factor(datos$"nivel.estudios")

datos$"comb"<-interaction(datos$estado.civil.NEW, datos$nivel.estudios.NEW)
table(datos$"comb",exclude=NULL)




