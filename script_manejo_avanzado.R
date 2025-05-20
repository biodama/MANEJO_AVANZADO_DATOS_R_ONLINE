
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


# Nivel de estudios

Bajo = "Escuela"
Medio = "Instituto"
Alto = "Universidad"

# Manera 1 (acceso a elementos)

datos$"nivel.estudios.1" <- datos$"nivel.estudios"
datos$"nivel.estudios.1"[datos$"nivel.estudios"%in%"Bajo"]<-"Escuela"
datos$"nivel.estudios.1"[datos$"nivel.estudios"%in%"Medio"]<-"Instituto"
datos$"nivel.estudios.1"[datos$"nivel.estudios"%in%"Alto"]<-"Universidad"
table(datos$"nivel.estudios",datos$"nivel.estudios.1",exclude=NULL)

# Manera 2 (levels)

class(datos$"nivel.estudios") # character
datos$"nivel.estudios.1"<-as.factor(datos$"nivel.estudios")
levels(datos$"nivel.estudios.1") # "Alto"  "Bajo"  "Medio"
levels(datos$"nivel.estudios.1")<-c("universidad","escuela","instituto")
table(datos$"nivel.estudios",datos$"nivel.estudios.1",exclude=NULL)


# Exportacion en formato excel

library("openxlsx") # install.packages("openxlsx")

sheets<-list()

sheets[[1]]<-datos
sheets[[2]]<-datos[,c("ID","estado.nuevo","comb","nivel.estudios.1")]

write.xlsx(sheets,file="/Users/pfernandezn/Desktop/datos_modificados.xlsx",
sheetName=c("datos_completos","datos_modificados"))



#################################################################
# FECHAS
#################################################################

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/Manejo_avanzado/MANEJO_AVANZADO_DATOS_R_ONLINE-main")

load("datos/datos.curso1.RData")

ls()

head(datos)

str(datos)


# Formato fecha perfecto para R

class(datos$"fdiag_cm")
sum(is.na(datos$"fdiag_cm"))
unique(datos$"fdiag_cm")

datos$"fecha.CM" <- as.Date(datos$"fdiag_cm")

sum(is.na(datos$"fecha.CM"))

str(datos)

# Otro formato 10.07.88

class(datos$"fdiag_cp")
sum(is.na(datos$"fdiag_cp"))
unique(datos$"fdiag_cp")

as.Date(datos$"fdiag_cp")

?strptime
# 28.06.02
datos$"fecha.CP" <- as.Date(datos$"fdiag_cp",format="%d.%m.%y")
sum(is.na(datos$"fecha.CP"))


# Otro formato (fecha de defuncion fdef)
# 1977.05.14

sum(is.na(datos$"fdef"))
datos$"fecha.DF" <- as.Date(datos$"fdef",format="%Y.%m.%d")
sum(is.na(datos$"fecha.DF"))


# Operaciones con fechas

datos$"dias.m"<-as.numeric(datos$"fecha.DF"-datos$"fecha.CM")

head(datos[,c("fecha.DF","fecha.CM","dias.m")])

datos$"weeks.m"<-difftime(datos$"fecha.DF", datos$"fecha.CM",units="weeks")

head(datos[,c("fecha.DF","fecha.CM","dias.m","weeks.m")])

datos$"fecha.cm.resta"<-datos$"fecha.CM"-2

head(datos[,c("fecha.CM","fecha.cm.resta")])


# Otros formatos

# WARNING
dates <- c("01ene1960", "02ene1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")

dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(x=dates, format="%m/%d/%y")


# Creacion de variable a nuestro gusto

datos$"fecha.cm.resta"<-datos$"fecha.CM"-2

#"2016-03-13"
#"13/03/2016"

sum(is.na(datos$"fecha.cm.resta"))
datos$"fecha_dm_resta_nueva"<-format(datos$"fecha.cm.resta",format="%d/%m/%Y")
sum(is.na(datos$"fecha_dm_resta_nueva"))

head(datos[,c("fecha.CM","fecha.cm.resta","fecha_dm_resta_nueva")])


datos$DF_new <- format(datos$"fecha.DF",format="%m/%Y")


# Secuencias de fechas

seq(as.Date("1976-01-01"),by="days",length=6)

fechas_calen<-seq(as.Date("1976-01-01"),to=as.Date("1976-01-15"),by="days")

datos_epi<-data.frame(FECHA=fechas_calen,casos=NA)

head(datos_epi)

# Otra cosa

datos$periodo<-NA
datos$periodo[datos$"fecha.DF"<="1977-05-15"]<-1


# Variable fecha con varios formatos


dates<-c("27/02/92","13/01/1999",
"02/27/92","02/27/92", "01/14/92", "02/28/92", "02/01/92")
sum(is.na(dates))


dates_new<-as.Date(x=dates, format="%m/%d/%y")
sum(is.na(dates_new))

dates[is.na(dates_new)]

# Iniciariamos la correcion de estas fechas con funciones de caracteres



###########################
# CARACTERES
###########################

# nchar() # numero de caracteres (de la librería gdata)

class(datos$"ID")
# nchar(as.character(datos$"ID"))
table(nchar(as.character(datos$"ID")))

head(datos[nchar(as.character(datos$"ID"))==1,])
head(datos[nchar(as.character(datos$"ID"))==2,])



# Paste

datos$"id_combinado"<-paste(datos$"ID",datos$"sexo",sep="***")

head(datos[,c("ID","sexo","id_combinado")])
paste0(datos$"ID","***",datos$"sexo")
paste0(datos$"ID","***",datos$"sexo","/",datos$"estado.civil")


datos$"estado.civil"<-as.factor(datos$"estado.civil")
datos$"nivel.estudios" <- as.factor(datos$"nivel.estudios")
datos$"comb"<-interaction(datos$"estado.civil", datos$"nivel.estudios")


datos$"estado.civil"<-as.character(datos$"estado.civil")
datos$"nivel.estudios" <- as.character(datos$"nivel.estudios")
datos$"comb_con_paste"<-paste0(datos$"estado.civil",".",datos$"nivel.estudios")




# strsplit

datos$"ID"<-as.character(datos$ID)
datos$"ID.new<"-paste(datos$"ID","a",sep="-")
datos$"ID.new"

res<-strsplit(datos$"ID.new",split="-",fixed=T)
res_data.frame<-do.call(rbind.data.frame, res)
table(res_data.frame[,2],exclude=NULL)
table(nchar(res_data.frame[,2]),exclude=NULL)


class(datos$"ID.new")
table(nchar(datos$"ID.new"))
table(unlist(strsplit(datos$"ID.new",split="")),exclude=NULL)

class(datos$"fdiag_cm")
table(nchar(datos$"fdiag_cm"))
table(unlist(strsplit(datos$"fdiag_cm",split="")),exclude=NULL)

res<-strsplit(datos$"fdiag_cm",split="-")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)

table(nchar(res$"year"))
table(res$"year")

table(nchar(res$"month"))
table(res$"month")

table(nchar(res$"day"))
table(res$"day")

datos<-cbind(datos,res)


# substring()

substring(datos$"fdiag_cm",first=1,last=4) # el año
substring(datos$"fdiag_cm",1,4)

substring(datos$"fdiag_cm",first=9,last=10) # el día


# sub y gsub

sub(pattern="-", replacement="/", datos$"fdiag_cm") # el primer - es el que cambia

gsub(pattern="-", replacement="/", datos$"fdiag_cm") # todo


# Repetidos / Duplicados

class(datos$"ID")
table(nchar(datos$"ID"))
table(unlist(strsplit(datos$"ID",split="")),exclude=NULL)

length(datos$"ID")
length(unique(datos$"ID"))

table(duplicated(datos$"ID"))

# Mas de una variable

datos$"check_dup"<-paste(datos$"ID",datos$"sexo",datos$"edad",sep="-")

table(duplicated(datos$"check_dup"))




datos$"ID"[3]<-137
datos$"ID"[5]<-137

table(duplicated(datos$"ID"))


# Unique

unique(datos$"estado.civil")
unique(datos$"cancer.prostata")

table(duplicated(datos$"ID"))
indice<-duplicated(datos$"ID")

unique(datos$"ID"[duplicated(datos$"ID")])

datos[duplicated(datos$"ID"),]
which(duplicated(datos$"ID"))


datos$"ID"[3]<-137
datos$"ID"[5]<-137
datos[datos$ID%in%"137",]
datos_ordenados<-datos[order(datos$"ID",datos$"fecha.CM"),]
datos_ordenados$"duplicado"<-duplicated(datos_ordenados$"ID")
datos_ordenados[datos_ordenados$"ID"%in%"137",c("ID","duplicado")]
datos_ordenados_filtrados<-datos_ordenados[-which(datos_ordenados$"duplicado"),]
datos_ordenados_filtrados[datos_ordenados_filtrados$"ID"%in%"137",c("ID","duplicado")]

datos$"ID_duplicado"<-paste0(datos$"ID",datos$"edad",datos$"sexo",datos$"estado.civil")
table(duplicated(datos$"ID_duplicado"))


# Duplicado perfecto
datos$"ID_duplicado"<-apply(datos,1,function(x) paste(x,collapse=""))
table(duplicated(datos$"ID_duplicado"))


# Match y grep
# tratamiento previo de la variable character a trabajar

datos$"busqueda"<-datos$"estado.civil"
datos$"busqueda"<-tolower(datos$"busqueda") # toupper
datos$"busqueda"<-gsub(pattern="ó", replacement="o", datos$"busqueda")
datos$"busqueda"<-gsub(pattern="í", replacement="i", datos$"busqueda")
datos$"busqueda"<-gsub(pattern="á", replacement="a", datos$"busqueda")
datos$"busqueda"<-gsub(pattern="ñ", replacement="n", datos$"busqueda")
datos$"busqueda"<-gsub(pattern="ä", replacement="a", datos$"busqueda")

match("casado",datos$"busqueda")
match("cas",datos$"busqueda")

indice<-grep("cas",datos$"busqueda")
head(datos[indice,])

datos$"obs"<-NA
datos$"obs"[1]<-"El paciente presentó unos síntomas raros y reporta cáncer y es Español"
datos$"obs"[2]<-"El paciente tenía cáncer de próstata y problemas cardiovasculares"
datos$"obs"[3]<-"El paciente no presentaba nada de nada"

datos$"obs_new"<-datos$"obs"
datos$"obs_new"<-tolower(datos$"obs_new") # toupper
datos$"obs_new"<-gsub(pattern="ó", replacement="o", datos$"obs_new")
datos$"obs_new"<-gsub(pattern="í", replacement="i", datos$"obs_new")
datos$"obs_new"<-gsub(pattern="á", replacement="a", datos$"obs_new")
datos$"obs_new"<-gsub(pattern="ñ", replacement="n", datos$"obs_new")

indice<-grep("cancer",datos$"obs_new")
datos$"cancer_obs"<-"No"
datos$"cancer_obs"[indice]<-"Si"


indice_cardio<-grep("cardio",datos$"obs_new")
indice_cancer<-grep("cancer",datos$"obs_new")

datos$"cancer_cardio_obs"<-"No"
datos$"cancer_cardio_obs"[unique(c(indice_cardio,indice_cancer))]<-"Si"



###############
# FUNCIONES
###############

datos_nuevos <- data.frame(ID=c(1,2,3),sexo=c("Mujer","Hombre","Mujer"))


tilde_out <- function(var){
	
	variable<-var
	
	variable<-toupper(variable)
	
	variable<-gsub("Ó","O",variable)
	variable<-gsub("Í","I",variable)
	variable<-gsub("Á","A",variable)	
	variable<-gsub("É","E",variable)	
	variable<-gsub("Ú","U",variable)	
	
	return(variable)

	
}

mirar<-c("camión","rotor","rueda","avión")
tilde_out(var=mirar)


# Llamar a funciones con source

rm(tilde_out)

source("/Users/pfernandezn/Desktop/mis_funciones.R")

datos$"obs"<-NA
datos$"obs"[1]<-"El paciente presentó unos síntomas raros y reporta cáncer y es Español"
datos$"obs"[2]<-"El paciente tenía cáncer de próstata y problemas cardiovasculares"
datos$"obs"[3]<-"El paciente no presentaba nada de nada"

datos$"obs_modificada"<-tilde_out_to_3(var=datos$"obs",to="lower")


datos_sin_tildes<-apply(datos,2,function(x) tilde_out_to_3(x,to="lower"))



###################################################################################

# install.packages("data.table")

library("data.table")

datos_new<-as.data.table(datos)

head(datos_new)

datos_new

str(datos_new)

datos_new$"imc1" <- datos_new$"peso"/c(datos_new$"altura"/100)^2

datos_new[,imc2:=peso/(altura/100)^2] # esto es para data.table











