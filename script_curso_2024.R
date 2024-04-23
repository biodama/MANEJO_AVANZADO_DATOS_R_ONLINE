####################################################################################################
#########################R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"#########################
####################################################################################################

#library
##openxlsx_4.2.5.2

#importar txt a mi workspace

datos <- read.table(file = "/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DATOS_R_ONLINE-main/datos/datos.curso1.txt",
										header = TRUE)
										
head (datos, 9)
tail (datos, 3)

getwd()
setwd("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DATOS_R_ONLINE-main")
datos <- read.table(file = "datos/datos.curso1.txt",
										header = TRUE)
										
										
####NO UTILIZR NUNCA FIX - PERDEMOS CONTROL DE LOS DATOS AL MODIFICAR LA TABLA
fix(datos)


#añadir columna(s) y fila(s)


###############################################
# QUITAR, PONER, .... PACKAGES
###############################################

install.packages("ggplot2")

library("ggplot2")

citation("ggplot2")

ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()

detach("package:ggplot2", unload = TRUE) #  Desconecto el paquete

# Ahora esto no se puede hacer porque la librería no está cargada
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()

remove.packages("ggplot2")

library("ggplot2") # Ya no esta en nuestro ordenador

# Vamos a instalar un paquete antiguo de ggplot2
# https://cran.r-project.org/src/contrib/Archive/ggplot2/
# ggplot2_0.5.7.tar.gz	2008-01-11 18:32 	1.8M

install.packages("/Users/pfernandezn/Desktop/ggplot2_0.5.7.tar.gz", repos = NULL, type="source")

library("ggplot2")

citation("ggplot2")

detach("package:ggplot2", unload = TRUE) #  Desconecto el paquete

remove.packages("ggplot2")

install.packages("ggplot2") # Vuelvo a instalar la nueva

library("ggplot2")

citation("ggplot2")




#####################################################################
# RECODIFICACIONES
#####################################################################

# Recodificacion variable numerica

str(datos)

range(datos$"edad")

min(datos$"edad")

max(datos$"edad")

class(datos$"edad")


datos$"edad_gr" <- datos$"edad"
datos$"edad_gr"[datos$"edad"<20]<- "[0-20)"
datos$"edad_gr"[datos$"edad">=20 & datos$"edad"<40]<- "[20-40)"
table(datos$"edad_gr",exclude=NULL)
class(datos$"edad_gr")


datos$"edad_gr_cut" <- cut(x=datos$"edad", breaks=seq(0,100,20),right=F,include.lowest=T)
datos$"edad_gr_cut_character" <- as.character(datos$"edad_gr_cut")

# as.numeric(datos$"edad_gr_cut")
levels(datos$"edad_gr_cut")
levels(datos$"edad_gr_cut")<-c("a","b","c","d","e")
levels(datos$"edad_gr_cut")[2]<-c("jovenes")
table(datos$"edad_gr_cut")

datos$"edad_gr_cut_new"<-factor(datos$"edad_gr_cut",levels=c("e","c","jovenes","d","a"))
table(datos$"edad_gr_cut_new")


datos$estado.civil
datos$estado.civil[datos$estado.civil%in%"Divorciado"]<-"Divorce"


datos$estado.civil.factor<-as.factor(datos$estado.civil)
levels(datos$estado.civil.factor)[2]<-"Divorce"

datos$estado.civil.character<-as.character(datos$estado.civil.factor)



# Combinaciones de variables

datos$"estado.civil"<-as.factor(datos$estado.civil)
datos$"nivel.estudios" <- as.factor(datos$nivel.estudios)

datos$"comb"<-interaction(datos$estado.civil, datos$nivel.estudios)

table(datos$"comb",exclude=NULL)



datos$"comb"[c(1,3,5)]<-NA
table(datos$"comb",exclude=NULL)

datos$"comb"<-as.character(datos$"comb")
datos$"comb"[is.na(datos$"comb")]<-"missing"
datos$"comb"<-as.factor(datos$"comb")
table(datos$"comb",exclude=NULL)

table(datos$"edad_gr_new",exclude=NULL)


# Ejercicios


datos$"nivel.estudios.1"

datos$"nivel.estudios.1" <- as.factor(datos$"nivel.estudios")
levels(datos$"nivel.estudios.1")
levels(datos$"nivel.estudios.1")[2]<-"escuela"
datos$"nivel.estudios.1"<-as.character(datos$"nivel.estudios.1")

table(datos$"nivel.estudios",datos$"nivel.estudios.1",exclude=NULL)



cut(datos$peso,breaks=c(55,60,70,80,90),include.lowest=F,right=F)



#################################################################
# FECHAS
#################################################################

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DATOS_R_ONLINE-main/datos/")

load("datos.curso1.RData")

ls()

head(datos)

str(datos)

# Formato fecha perfecto para R

class(datos$"fdiag_cm")
sum(is.na(datos$"fdiag_cm"))
unique(datos$"fdiag_cm")

datos$"fecha.CM" <- as.Date(datos$"fdiag_cm")

sum(is.na(datos$"fecha.CM"))

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

datos$"fecha.DF" <- as.Date(datos$"fdef",format="%Y.%m.%d")

# Operaciones con fechas

datos$"dias.m"<-as.numeric(datos$"fecha.DF"-datos$"fecha.CM")
head(datos[,c("fecha.DF","fecha.CM","dias.m")])

datos$"dias.m.2"<-difftime(datos$"fecha.DF", datos$"fecha.CM",units="days")
datos$"weeks.m"<-difftime(datos$"fecha.DF", datos$"fecha.CM",units="weeks")
datos$"years.m"<-difftime(datos$"fecha.DF", datos$"fecha.CM",units="days")/ 365.25

# Otros formatos

dates <- c("01ene1960", "02ene1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")


dates <- c("01jan1960", "02jan1960", "31mar1960", "30jul1960")
as.Date(x=dates, format="%d%b%Y")


dates <-c("01-enero-1960")
as.Date(x=dates, format="%d-%B-%Y")

dates <-c("01-january-1960")


dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(x=dates, format="%m/%d/%y")


datos$DF_new <- format(datos$"fecha.DF",format="%m/%Y")


fechas_calen<-seq(as.Date("1976-01-01"),to=as.Date("1976-01-15"),by="days")

datos_epi<-data.frame(FECHA=fechas_calen,casos=NA)

head(datos_epi)


datos$periodo<-NA
datos$periodo[datos$fecha.DF<="1977-05-15"]<-1

###########################
# CARACTERES
###########################

# nchar() # numero de caracteres (de la librería gdata)

nchar(as.character(datos$"ID"))
table(nchar(as.character(datos$"ID")))

# Paste

datos$"id_combinado"<-paste(datos$"ID",datos$"sexo",sep="***")

paste0(datos$"ID","***",datos$"sexo","/",datos$"estado.civil")

# strsplit

res<-strsplit(datos$"fdiag_cm",split="-")
res<-do.call(rbind.data.frame, res)
names(res)<-c("year","month","day")
head(res)

datos<-cbind(datos,res)


format(datos$"fecha.DF",format="%d")

# substring()

substring(datos$"fdiag_cm",first=1,last=4) # el año
substring(datos$"fdiag_cm",first=9,last=10) # el día

substring(datos$"fdiag_cm",1,4)

# sub y gsub

sub(pattern="-", replacement="/", datos$"fdiag_cm")

gsub(pattern="-", replacement="/", datos$"fdiag_cm") # todo

# Repetidos / Duplicados

length(datos$"ID")
length(unique(datos$"ID"))

unique(datos$"estado.civil")

unique(datos$"cancer.prostata")

datos$"ID"[3]<-137
datos$"ID"[5]<-137

table(duplicated(datos$"ID"))
indice<-duplicated(datos$"ID")

unique(datos$"ID"[duplicated(datos$"ID")])



datos_ordenados<-datos[order(datos$"ID",datos$"fecha.CM"),]

datos_ordenados$"duplicado"<-duplicated(datos_ordenados$"ID")

datos_ordenados_filtrados<-datos_ordenados[-which(datos_ordenados$"duplicado"),]


datos$ID_duplicado<-paste0(datos$"ID",datos$"edad",datos$"sexo",datos$"estado.civil")
table(duplicated(datos$"ID_duplicado"))

# Duplicado perfecto
datos$ID_duplicado<-apply(datos,1,function(x) paste(x,collapse=""))
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

indice<-grep("cas",datos$"busqueda")
datos[indice,]

datos$"obs"<-NA
datos$"obs"[1]<-"El paciente presentó unos síntomas raros y reporta cáncer y es Español"
datos$"obs"[2]<-"El paciente tenía cáncer de próstata y probleas cardiovasculares"
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


match("Casado",datos$"estado.civil")

grep("sa",datos$"estado.civil")

datos$"busqueda"<-"NO"
indice<-grep("Muj",ID2)
datos$busqueda[indice]<-"LO HE ENCONTRADO"

###############
# FUNCIONES
###############

datos_nuevos <- data.frame(ID=c(1,2,3),sexo=c("Mujer","Hombre","Mujer"))

c(1,2,3,4)


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

source("/Users/pfernandezn/Desktop/funciones.R")

mirar<-c("camión","rotor","rueda","avión")
tilde_out(var=mirar)

tilde_out_to_3(var=mirar,to="upper")

mirar<-c(1,2,3)
tilde_out_to_3(var=mirar,to="upper")

tilde_out_to_3(var=datos$"edad",to="upper")

pablo<-apply(datos[,c(1:2)],2,function(x) tilde_out_to_3(x,to="upper"))

datos$"ID_1"<-NULL

for(i in 1:4){
	
	print(i)
	variable_a_crear<-tilde_out_to_3(datos[,i],to="upper")
	datos$"variable_a_crear"<-variable_a_crear
	names(datos)[dim(datos)[2]]<-paste0(names(datos)[i],"_1")
	
}



