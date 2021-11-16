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


datos$"edad_prueba"<-cut(x=datos$"edad", breaks=c(0,65,85),
right=F,include.lowest=F)
table(datos$"edad_prueba",exclude=NULL)

# Hacer sub-bases de datos

datos_altos<- datos[datos$"altura">160 ,    ]


# Secuencias no aleatorias


c(0,20,40,60,80,100)

seq(0,100,20)

rep(2,50)

c(0,1,seq(2,100,20))

c(0,1,rep(2,4),seq(5,20,2))




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


##############
# FECHAS
##############


# Formato tipo R
class(datos$fdiag_cm)

fechas.CM <- as.Date(datos$"fdiag_cm")
class(fechas.CM)

fechas.CM[1:6]

datos$"fechas.CM"<-as.Date(datos$"fdiag_cm")

check<-as.Date(c("1977-5-5"))
check
class(check)

# Otros formatos

class(datos$fdiag_cp) 

fechas.CP <- as.Date(datos$"fdiag_cp", format="%d.%m.%y")

class(fechas.CP) 

fechas.CP[1:6] 

datos$"fechas.CP" <- as.Date(datos$"fdiag_cp", format="%d.%m.%y")

?strptime


class(datos$fdef) 
fechas.DF <- as.Date(datos$"fdef",format="%Y.%m.%d")

class(fechas.DF)
fechas.DF

datos$"fechas.DF" <- as.Date(datos$"fdef",format="%Y.%m.%d")


# datos$"fdef" <- as.Date(datos$"fdef",format="%Y.%m.%d")

# Eliminacion de variables

# datos<-datos[,-c(12,13,14)]


# Errores

fechas.CP_prueba <- as.Date(datos$"fdiag_cp", format="%d/%m.%y")
fechas.CP_prueba

fechas.CP_prueba <- as.Date(datos$"fdiag_cp", format="%d.%m.%Y")
fechas.CP_prueba


# Operaciones con fechas

datos$"dias"<-c(datos$"fechas.DF" - datos$"fechas.CM")

class(datos$"dias")

head(datos[,c("ID","fechas.CM","fechas.CP","fechas.DF","dias")])


datos$"dias"<-as.numeric(datos$"dias")

class(datos$"dias")

head(datos)



# Diferencia de fechas (en años)


datos$"years"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days") / 365.25
attr(datos$"years","units")="years"


# Otros formatos


dates <- c("01jan1960", "02jan1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")


dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(x=dates, format="%m/%d/%y")


# Cambiar el formato de las fechas para exportacion

datos$DF_new <- as.character(format(datos$"fechas.DF",format="%m/%Y"))
head(datos[,c("ID","fechas.DF","DF_new")])

# Secuencias de fechas

seq(as.Date("1976-01-01"),to=as.Date("1976-03-01"),by="days")[1:10]

# Operaciones con fechas

datos$"fecha.DF_rev"<-datos$"fechas.DF" - 5







