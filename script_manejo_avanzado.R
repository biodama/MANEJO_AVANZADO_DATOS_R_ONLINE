########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

####################################
# RECODIFICACION DE VARIABLES
####################################

rm(list=ls())
gc()

setwd("C:\\Users\\usuario\\Desktop\\MANEJO_AVANZADO_DATOS_R_ONLINE-main\\datos\\datos\\")

load("datos.curso1.RData")


datos$"peso"[c(1,15,28)]<- 44.33

indice.registros <- c(1,15,28)
datos$"peso"[indice.registros]<- 44.33




table(datos$"estado.civil")
datos$"estado.civil.new" <- datos$"estado.civil"
datos$"estado.civil.new" [datos$"estado.civil.new"%in%"Casado"]<-"cas"
table(datos$estado.civil.new)
table(datos$"estado.civil",datos$estado.civil.new)






# VARIABLES NUMERICAS A CATEGORICAS

class(datos$"edad")

# datos$"edad"<-as.numeric(datos$"edad")

range (datos$"edad")

gr<-cut(x=datos$"edad", breaks=seq(0,80,20),right=F,include.lowest=T)

# gr<-cut(x=datos$"edad",breaks=c(0,20,40,60,80),right=F,include.lowest=T)

datos$"gr.edad"<-gr

class(datos$"gr.edad")

levels(datos$"gr.edad")

table(datos$"gr.edad",exclude=NULL)


datos$"edad_cat1"<-cut(x=datos$"edad", breaks=c(0,1,20,40,60,80,85),
right=F,include.lowest=T)

table(datos$edad_cat1,exclude=NULL)

datos$"edad_cat2"<-cut(x=datos$"edad", breaks=c(0,65,85),
right=F,include.lowest=T)

table(datos$edad_cat2,exclude=NULL)



# VARIABLES CARACTER A CATEGORICAS


# Ordenar categorias como uno quiera

table(datos$estado.civil)

sort(table(datos$estado.civil),decreasing=T)

estado.civil.factor <- as.factor(datos$"estado.civil")

levels(estado.civil.factor)

nuevo.orden<- c("Divorciado","Soltero", "Casado")

estado.civil.factor.new<- factor(estado.civil.factor,levels=nuevo.orden)

levels(estado.civil.factor.new)

# Recodificacion

table(estado.civil.factor)

levels(estado.civil.factor)

levels(estado.civil.factor)<-c("cas","div","sol")

table(estado.civil.factor)
 
levels(estado.civil.factor)[2]<-c("divo")
table(estado.civil.factor)


# Combinacion de categorias

class(datos$estado.civil)
class(datos$nivel.estudios)

datos$estado.civil   <- as.factor(datos$estado.civil)
datos$nivel.estudios <- as.factor(datos$nivel.estudios)


class(datos$estado.civil)
class(datos$nivel.estudios)


datos$comb <- interaction(datos$estado.civil, datos$nivel.estudios)

table(datos$comb,exclude=NULL)

class(datos$comb)

datos$estado.civil   <- as.character(datos$estado.civil)
datos$nivel.estudios <- as.character(datos$nivel.estudios)

class(datos$estado.civil)
class(datos$nivel.estudios)


# EJERCICIOS DE ESTA PARTE

# Manera 1

datos$nivel.estudios.1 <- datos$nivel.estudios

table(datos$nivel.estudios.1, exclude=NULL)

datos$nivel.estudios.1[datos$nivel.estudios%in%"Bajo"]<-"escuela"

table(datos$nivel.estudios.1, exclude=NULL)

# Manera 2

nivel.estudios.factor <- as.factor(datos$nivel.estudios)

class(nivel.estudios.factor)

levels(nivel.estudios.factor)

levels(nivel.estudios.factor)[2]<-c("escuela")

# levels(nivel.estudios.factor)<-c("Alto","escuela","Medio")

table(nivel.estudios.factor)

datos$"nivel.estudios.1"<-nivel.estudios.factor

table(datos$"nivel.estudios",datos$"nivel.estudios.1")

# Manera 3

datos$"nivel.estudios.1"<-as.factor(datos$nivel.estudios)
levels(datos$"nivel.estudios.1")[2]<-c("escuela")
datos$"nivel.estudios.1"<-as.character(datos$"nivel.estudios.1")







