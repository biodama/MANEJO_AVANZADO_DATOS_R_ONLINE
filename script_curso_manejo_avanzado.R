


rm(list=ls())
gc()


setwd("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DATOS_R_ONLINE-main/datos/")

load("datos.curso1.RData")

####################################################################
# RECODIFICACION CLASICA VARIABLE CARACTER
####################################################################

table(datos$"estado.civil")

datos$"estado.civil.new" <- datos$"estado.civil"

datos$"estado.civil.new" [datos$"estado.civil.new"%in%"Casado"] <- "cas"

table(datos$estado.civil.new)


####################################################################
# RECODIFICACION VARIABLE CUANTITATIVA (CUT)
####################################################################

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

####################################################################
# VARIABLES TIPO FACTOR
####################################################################

class(datos$estado.civil)

table(datos$estado.civil)

estado.civil.factor <- as.factor(datos$estado.civil)

levels(estado.civil.factor)


# Cambiar el orden de las categorias de una varible factor
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

################################################################
# Combinacion de variables TIPO FACTOR con missing
################################################################

class(datos$"estado.civil")
class(datos$"nivel.estudios")

sum(is.na(datos$"estado.civil"))
sum(is.na(datos$"nivel.estudios"))

# Para conocer los valores de una variable
unique(datos$"estado.civil")
table(datos$"estado.civil",exclude=NULL)


datos$"estado.civil.NEW"<-as.factor(datos$"estado.civil")
datos$"nivel.estudios.NEW" <- as.factor(datos$"nivel.estudios")

# meto unos missing aleatorios en las variables
datos$"estado.civil.NEW"[c(3,8,10)]<-NA
datos$"nivel.estudios.NEW"[c(11,23,45)]<-NA


datos$"comb"<-interaction(datos$estado.civil.NEW, datos$nivel.estudios.NEW)
unique(datos$"comb")
table(datos$"comb",exclude=NULL)


sample(letters)[1]

# 1. Crear una nueva variable llamada "nivel.estudios.1" en "datos" 
# (data.frame que se incorpora al workspace tras cargar “datos.curso1.RData”) 
# que sea igual a la variable nivel.estudios
# original pero sustituyendo los valores de nivel de estudios “Bajo” por “escuela”.

datos$"nivel.estudios.1" <- datos$"nivel.estudios"

datos$"nivel.estudios.1"[datos$"nivel.estudios.1"%in%"Bajo"]<-"Escuela"

head(datos)

class(datos$"nivel.estudios")
datos$"nivel.estudios.1" <- as.factor(datos$"nivel.estudios")
levels(datos$"nivel.estudios.1")[2]<-"Escuela"
table(datos$"nivel.estudios.1")


# 2. Crear una variable llamada “peso.grupos” en “datos” (data.frame que se incorpora al
# workspace tras cargar “datos.curso1.RData”) que clasifique a las personas las tres categorías 
# de peso siguientes: [55,60] (60,70] (70,80] (80,90]

datos$"peso.grupos"<-cut(x=datos$peso, breaks=seq(55,90,10), right=T,include.lowest = T)
table(datos$"peso.grupos")


# 3. Crear una variable tipo factor llamada “nivel.estudios.2” en “datos” (data.frame que se
# incorpora al workspace tras cargar “datos.curso1.RData”) a partir de la variable nivel.estudios
# original, donde el orden de las categorías sea el siguiente: (1) “Bajo” (2) “Medio” (3) “Alto”

class(datos$"nivel.estudios")

table(datos$"nivel.estudios")

datos$"nivel.estudios.factor" <- as.factor(datos$"nivel.estudios")

levels(datos$"nivel.estudios.factor")

nuevo.orden<-c("Bajo", "Medio", "Alto")
datos$"nivel.estudios.2"<-factor(datos$"nivel.estudios.factor",levels=nuevo.orden) 

levels(datos$"nivel.estudios.2")
table(datos$nivel.estudios.2)


#################################################################
# FECHAS
#################################################################

# Formato fecha perfecto para R

class(datos$"fdiag_cm")
sum(is.na(datos$"fdiag_cm"))
unique(datos$"fdiag_cm")

datos$"fechas.CM" <- as.Date(datos$"fdiag_cm")

class(datos$"fechas.CM")
sum(is.na(datos$"fechas.CM"))
datos$"fechas.CM"[1:6]


# Otro formato 10.07.88

class(datos$"fdiag_cp")
sum(is.na(datos$"fdiag_cp"))
unique(datos$"fdiag_cp")

datos$"fechas.CP" <- as.Date(datos$"fdiag_cp",format="%d.%m.%y")

class(datos$"fechas.CP")
sum(is.na(datos$"fechas.CP"))
unique(datos$"fechas.CP")


# Otro formato 2007.12.13

class(datos$"fdef")
sum(is.na(datos$"fdef"))
unique(datos$"fdef")


datos$"fechas.DF" <- as.Date(datos$"fdef",format="%Y.%m.%d")

class(datos$"fechas.DF")
sum(is.na(datos$"fechas.DF"))
unique(datos$"fechas.DF")



# Otros formatos especiales (CUIDADO!!!!!!!!!!)


dates <- c("01ene1960", "02ene1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")

dates <- c("01jan1960", "02jan1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")



###########################
# OPERACIONES CON FECHAS
###########################

datos$"dias"<-c(datos$"fechas.DF" - datos$"fechas.CM")
class(datos$"dias")
#datos$"dias"<-as.numeric(datos$"dias")

datos$"dias2"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days")

datos$"weeks"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="weeks")
 
datos$"years"<-difftime(datos$"fechas.DF", datos$"fechas.CM",units="days") / 365.25
attr(datos$"years","units")="years"

###########################
# RECODIFICAR FORMATO
###########################



datos$"DF_new"<- as.character(format(datos$"fechas.DF",format="%m/%Y"))


###########################
# SECUENCIAS TEMPORALES
###########################

dias<-seq(as.Date("2020-01-30"),to=as.Date("2021-05-01"),by="days")

datos_base<-data.frame(fecha=as.character(dias),stringsAsFactors=F)
str(datos_covid)

datos_covid<-data.frame(fecha=c("2020-01-30","2020-02-03","2020-02-05"),
casos=c(1,15,50),stringsAsFactors=F)

datos_finales<-merge(datos_base,datos_covid,by="fecha",all.x=T,all.y=F)
datos_finales$"casos"[is.na(datos_finales$"casos")]<-0

head(datos_finales,20)


###########################
# CARACTERES
###########################

# nchar() # numero de caracteres (de la librería gdata)

nchar(as.character(datos$ID))
table(nchar(as.character(datos$ID)))


# paste() # concatenar caracteres

x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
paste(x,collapse="/")


datos$"virginia"<-paste(datos$"sexo",datos$"estado.civil",datos$"diabetes",sep="***")

names(datos)[26]<-paste(names(datos)[3],names(datos)[4],names(datos)[9],sep="_")


# strsplit() # Dividir los elementos de un vector de caracteres en subcadenas de acuerdo con criterio

res<-strsplit(datos$"fdiag_cm",split="-")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)
unique(res$"year")
sort(unique(res$year))


res<-strsplit(datos$"fdef",split="[.]")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)

#Mirar
res<-strsplit(datos$"fdiag_cm",split="-",fixed=T) ????????????????



# substring() # Extraer o reemplazar subcadenas en un vector de caracteres.

nchar(as.character(datos$ID))
table(nchar(as.character(datos$ID)))
substring(datos$"ID",first=1,last=1)

datos$"ID_new"<-paste(datos$"ID",datos$"sexo",datos$estado.civil,sep="_")
table(nchar(datos$"ID_new"))
substring(datos$"ID_new",first=2,last=4)

# Cadenas inversas 

library(stringi)
stri_reverse(c("a", "ab", "abc"))


# Creacion de variable con mismo numero de caracteres
nchar(as.character(datos$ID))
table(nchar(as.character(datos$ID)))

datos$"ID_final"<-NA
datos$"ID_final"[nchar(datos$ID)==1]<-paste("00",datos$"ID"[nchar(datos$ID)==1],sep="")
datos$"ID_final"[nchar(datos$ID)==2]<-paste("0",datos$"ID"[nchar(datos$ID)==2],sep="")
datos$"ID_final"[nchar(datos$ID)==3]<-datos$"ID"[nchar(datos$ID)==3]


nchar(as.character(datos$"ID_final"))
table(nchar(as.character(datos$"ID_final")))


matriz<-matrix(rep(10,100),nrow=10)
matriz[1,3]<-1
matriz[1,10]<-1
matriz[2,9]<-1
matriz[nchar(matriz)==1]<-paste("AAA",matriz[nchar(matriz)==1],sep="")



# sub(), gsub() # buscar coincidencias y reemplazar dentro de un vector de caracteres

mirar <- c("abcdaabtd")
gsub(pattern="a", "**",mirar)
gsub(pattern="[.]","-",datos$"fdef")
gsub(pattern="[.]","",datos$"fdef")


#unique() # lista los elementos de un vector sin duplicados

unique(datos$"estado.civil")


# duplicated()

length(datos$"ID")
length(unique(datos$"ID"))

duplicated(datos$"ID")
table(duplicated(datos$"ID"))

datos$"ID"[2]<-datos$"ID"[1]

length(datos$"ID")
length(unique(datos$"ID"))

duplicated(datos$"ID")
table(duplicated(datos$"ID"))

datos[which(datos$"ID"%in%datos$ID[duplicated(datos$"ID")]),]

datos[which(duplicated(datos$"ID")),]

datos$duplicado<-duplicated(datos$"ID")


# Crear identificador con todas los valores de todas las variables

ID_completo<-apply(datos,1,function(x) paste(x,collapse="_"))
mirar<-duplicated(ID_completo)
table(mirar)

# grep y match


mirar<-c("AA","Aads","AAsdDD","fdsdf","rasdAADD","EEaa","AA")

grep("AA",mirar)
match("AA",mirar)

grep("aa",mirar)

mirar<-toupper(mirar) # toupper tolower
mirar<-gsub("Á","A",mirar)
mirar<-gsub("É","E",mirar)
mirar<-gsub("Í","I",mirar)
mirar<-gsub("Ó","O",mirar)
mirar<-gsub("Ú","U",mirar)


grep("CANCER",mirar)



# FUNCIONES


apply(datos[,c(6,7)],2,function(x) x/10)

datos[,6]
datos[,7]

apply(datos[,c(6,7)],1,function(x) x/10)
datos[1,c(6,7)]
datos[2,c(6,7)]
....


mi.funcion <- function(x){
	
	
	if(class(x)=="numeric"){
		
		res<-x/10
		
	}
	
	if(class(x)=="character"){
		
		res<-paste("OHHHH",x,sep="")
		
	}
	
	if(class(x)=="Date"){
		
		print("NO ES UNA VARIABLE CARACTER O NUMERICA Y NO HAGO NADA")
		res<-"NO SOY UNA VARIABLE TIPO CARACTER O NUMERICA"
	}
	
	return(res)
		
	
}



prepara_character <- function(var){
	
	variable<-var
	
	variable<-toupper(variable)
	
	variable<-gsub("Ó","O",variable)
	variable<-gsub("Í","I",variable)
	variable<-gsub("Á","A",variable)	
	variable<-gsub("É","E",variable)	
	variable<-gsub("Ú","U",variable)	
	
	return(variable)
	
}







