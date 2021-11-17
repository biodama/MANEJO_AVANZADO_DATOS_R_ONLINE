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




# Comprobacion de missing en transformacion a formato fecha

class(datos$"fdef")

datos$"fdef"[1:6]

datos$"fechas.DF"<-as.Date(datos$"fdef",format="%Y.%m.%d")

class(datos$"fechas.DF")

datos$"fechas.DF"[1:6]

head(datos[,c("ID","sexo","fdef","fechas.DF")],n=10)

sum(is.na(datos$"fdef"))
sum(is.na(datos$"fechas.DF"))

# datos$"fdef"[is.na(datos$"fdef")]

sum(is.na(datos$"fdef"[is.na(datos$"fechas.DF")]))
sum(is.na(datos$"fdef"))

datos$"fdef"[is.na(datos$"fechas.DF") & !is.na(datos$"fdef")]
datos$"ID"[is.na(datos$"fechas.DF") & !is.na(datos$"fdef")]

# recodificacion de las fechas de los registros raros

datos$"fdef"[1]<-"1977/05/14"

sum(is.na(datos$"fdef"))

sum(is.na(datos$"fechas.DF"))

sum(is.na(datos$"fdef"[is.na(datos$"fechas.DF")]))

datos$"fdef"[is.na(datos$"fechas.DF") & !is.na(datos$"fdef")]<-"1977.05.14"

datos$"fechas.DF"<-as.Date(datos$"fdef",format="%Y.%m.%d")

head(datos[,c("ID","sexo","fdef","fechas.DF")],n=10)



datos$"fdiag_cm"[1:6]

datos$"fechas.CM"<-as.Date(datos$"fdiag_cm")


datos$"ano"<-as.character(format(datos$"fechas.DF",format="%Y"))
head(datos)

datos$"mes"<-as.character(format(datos$"fechas.DF",format="%b"))



###################
# CARACTERES
###################


# nchar

datos$"fdef"[1:5]

table(nchar(datos$"fdef"),exclude=NULL)
unique(nchar(datos$"fdef"))

table(nchar(datos$"ID"),exclude=NULL)

datos$"ID"[nchar(datos$"ID")<3]

# paste

paste("a","b","c",sep="-")

datos$"ID_sexo"<-paste(datos$"ID",datos$"sexo",sep="-")

head(datos[,c("ID","sexo","ID_sexo")])


table(nchar(datos$"ID"),exclude=NULL)

datos$"ID"[nchar(datos$"ID")<3]

datos$"ID"[nchar(datos$"ID")==1]<-paste("00",datos$"ID"[nchar(datos$"ID")==1],sep="")

datos$"ID"[nchar(datos$"ID")==2]<-paste("0",datos$"ID"[nchar(datos$"ID")==2],sep="")

table(nchar(datos$"ID"),exclude=NULL)


datos$"ID_pais"<-paste(datos$"ID","-MA",sep="")


# strsplit

datos$"fdef"[1:6]
strsplit(datos$"fdef",split="[.]")
unique(unlist(strsplit(datos$"fdef",split="")))

# substring
"2006.08.06"

check<-substring(datos$"fdef",first=1,last=4)
table(check,exclude=NULL)

check<-substring(datos$"fdef",first=1,last=5)
table(check,exclude=NULL)

datos$"ano_def"<-substring(datos$"fdef",first=1,last=4)

# sub, gsub

datos$"fdef"[1:6]

check<-sub(pattern="[.]", "-",datos$"fdef")

check[1:6]

check2<-gsub(pattern="[.]", "-",datos$"fdef")

check2[1:6]

check3<-gsub(pattern="//", "-",check2)

# Eliminar el dia de la fecha de defunion

datos$"fdef_new1"<-substring(datos$"fdef",first=1,last=7)



# Unique

unique(nchar(datos$"fdef"))

# Duplicated

duplicated(datos$ID)

duplicated(c(1,1,2,3,4,5,6,7))


duplicated(c(1,2,3,4,1,5,6,7))

datos$ID[1]<-200

ids_duplicados<-datos$"ID"[duplicated(datos$"ID")]

datos[datos$"ID"%in%ids_duplicados,]


# match & grep

match("GEN",c("aghGENslkfsdlkfdsa","fsfdsf"))

tolower(c("aghGENslkfsdlkfdsa","fsfdsf"))


var1<-toupper(c("agh;GEN;slkfsdlkfdsa","gen","fsf;dsf","gen"))

match("GEN",var1)

grep("GEN",var1)

set.seed(10)
datos$"des"<-sample(c(rep("Diarrea;cancer",40),rep("Diarrea;fiebre",160)))

grep("CANCER",toupper(datos$"des"))

datos$"cancer"<-"no"
datos$"cancer"[grep("CANCER",toupper(datos$"des"))]<-"si"

table(datos$"cancer")

set.seed(20)
datos$"des2"<-sample(c(rep("Celiaquía;cáncer",40),
rep("Diarrea;saranpión",160)))

datos$"des2"<-toupper(datos$"des2")

unique(unlist(strsplit(datos$"des2",split="")))

datos$"des2"<-gsub("Ó","O",datos$"des2")
datos$"des2"<-gsub("Í","I",datos$"des2")
datos$"des2"<-gsub("Á","A",datos$"des2")

unique(unlist(strsplit(datos$"des2",split="")))

head(datos)



################################################################
# FUNCIONES
################################################################

superfuncion <- function(var){
	
	variable<-var
	
	variable<-toupper(variable)
	
	variable<-gsub("Ó","O",variable)
	variable<-gsub("Í","I",variable)
	variable<-gsub("Á","A",variable)	
	variable<-gsub("É","E",variable)	
	variable<-gsub("Ú","U",variable)	
	
	return(variable)
	
}


diagnostico<-c("Bueno","Pésimo","Para morirse",
"últimos segundos de vida")

superfuncion(var=diagnostico)

set.seed(20)
datos$"des2"<-sample(c(rep("Celiaquía;cáncer",40),
rep("Diarrea;saranpión",160)))


datos$"res_superfuncion"<-superfuncion(var=datos$"des2")




#####################################################################

# Hacer sub-bases de datos

datos_altos_1 <- datos[datos$"altura">160 ,    ]


datos_altos_2 <- subset(datos, altura > 160)


datos_altos_3 <- datos[datos$"altura">160 , c("altura","peso","ID") ]


datos_altos_4 <- subset(datos, altura > 160, select = c(ID, altura,peso))

datos$"altura"[c(3,5,10)]<-NA

sum(is.na(datos$"altura"))

datos_altos_5 <- datos[datos$"altura">160 & datos$"sexo"%in%"Hombre", c("altura","peso","ID") ]

sum(is.na(datos_altos_5$"altura"))

datos_altos_5<-datos_altos_5[-which(is.na(datos_altos_5$"altura")),]


#####################################################################

####################
# BUCLES
####################

# FOR

datos$"peso"[datos$"peso"<60]

datos$"altura"[1:6] + 5

resultado<-rep(NA,6)

for(i in c(1:6)){
	
	
	print(datos$"altura"[i]+5)
	resultado[i]<-datos$"altura"[i]+5
	
}


datos$"altura_for"<-NA
datos$"peso_for"<-NA
for(i in c(1:200)){
	
	
	print(i)
	
	datos$"altura_for"[i]<-datos$"altura"[i]+10
	datos$"peso_for"[i]<-datos$"peso"[i]+15
	
	print("paso al siguiente i")
	
}

datos$"altura_for"<-datos$"altura"+10
datos$"peso_for"<-datos$"peso"+15



# IF / ELSE

superfuncion2 <- function(var){
	
	if(class(var)=="numeric"){
		
		print("NO PUEDO HACER NADA; VARIABLE NUMERICA")
		
	}
	
	else{
		
		print("LA VARIABLE ES CARACTER Y RECODIFICO")
		
		variable<-var
	
		variable<-toupper(variable)
	
		variable<-gsub("Ó","O",variable)
		variable<-gsub("Í","I",variable)
		variable<-gsub("Á","A",variable)	
		variable<-gsub("É","E",variable)	
		variable<-gsub("Ú","U",variable)	
	
		return(variable)
		
	}
	
}


diagnostico<-c("Bueno","Pésimo","Para morirse",
"últimos segundos de vida")

diagnostico2<-c(1,2,3,4,5)

superfuncion2(var=diagnostico)
superfuncion2(var=diagnostico2)


superfuncion3 <- function(var){
	
	if(class(var)=="numeric"){
		
		print("NO PUEDO HACER NADA; VARIABLE NUMERICA")
		
	}
	
	if(class(var)=="factor"){
		
		print("LA VARIABLE ES FACTOR Y RECODIFICO ESPECIAL")
		
		variable<-var
		
		variable<-as.character(variable)
	
		variable<-toupper(variable)
	
		variable<-gsub("Ó","O",variable)
		variable<-gsub("Í","I",variable)
		variable<-gsub("Á","A",variable)	
		variable<-gsub("É","E",variable)	
		variable<-gsub("Ú","U",variable)	
		
		variable<-as.factor(variable)
		
		return(variable)
	
		
	}
	
	if(class(var)=="character"){
		
		print("LA VARIABLE ES CARACTER Y RECODIFICO")
		
		variable<-var
	
		variable<-toupper(variable)
	
		variable<-gsub("Ó","O",variable)
		variable<-gsub("Í","I",variable)
		variable<-gsub("Á","A",variable)	
		variable<-gsub("É","E",variable)	
		variable<-gsub("Ú","U",variable)	
	
		return(variable)
		
	}
	
}


estado.civil<-as.factor(c("casado","soltero","casado",
"divorciado","soltero"))
superfuncion3(var=estado.civil)


# IFELSE

datos$"peso"

datos$"peso_oms"<-ifelse(datos$"peso">65, yes="malo",no= "bueno")

datos$"altura_oms"<-ifelse(datos$"altura">150, yes="alto",no= "bajo")

table(datos$"altura_oms",exclude=NULL)


# APPLY

apply(datos[,c("peso","altura")] , 1 , function(x) x/10 )

dim(datos)

missing_registro<-apply(datos , 1 , function(x) sum(is.na(x))/33*100 )
missing_variable<-apply(datos , 2 , function(x) sum(is.na(x))/200*100 )

missing_registro<-apply(datos , 1 , function(x) sum(is.na(x))/dim(datos)[2]*100 )
missing_variable<-apply(datos , 2 , function(x) sum(is.na(x))/dim(datos)[1]*100 )

datos$"missing_registros"<-missing_registro

missing_registro_suma<-apply(datos , 1 , function(x) sum(is.na(x)))
missing_variable_suma<-apply(datos , 2 , function(x) sum(is.na(x)))


# 1 aplico a filas
# 2 aplico a columnas

# TAPPLY

tapply(datos$edad,datos$sexo, mean)

# SAPPLY

sapply(datos[,c(6)], function(x) x/10)

datos$"peso_new"<-sapply(datos$"peso", function(x) x/10)


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

datos$"fdef"


resultado<-strsplit(datos$"fdef",split="[.]")

datos$"ano_defun"<-NA
datos$"mes_defun"<-NA
datos$"dia_defun"<-NA
for(i in 1:dim(datos)[1]){
	
	print(i)
	
	res<-unlist(strsplit(datos$"fdef"[i],split="[.]"))
	
	if(is.na(datos$"fdef"[i])){
			
		datos$"ano_defun"[i]<-NA
		datos$"mes_defun"[i]<-NA
		datos$"dia_defun"[i]<-NA
	}
	if(!is.na(datos$"fdef"[i])){
			
		datos$"ano_defun"[i]<-res[1]
		datos$"mes_defun"[i]<-res[2]
		datos$"dia_defun"[i]<-res[3]		
		
	}
			
}

head(datos[,c("ID","fdef","dia_defun","mes_defun","ano_defun")])


#7. Crea una funcion llamada “depuracion” que revise una base de datos (como la del curso)
#y que si encuentra algún registro mujer, divorciada con nivel de estudios alto, muestre el
#siguiente mensaje “REVISA LA BASE DE DATOS” y proporcione los identificadores de los
#registros que cumplan esta condición.

depuracion <- function(x){
	
	
	mirar<-x[c( x$"sexo"%in%"Mujer" & x$nivel.estudios%in%"Alto" & x$estado.civil%in%"Divorciado"),]

	if(dim(mirar)[1]>0){

		print("REVISA LA BASE DE DATOS")

		ids_mirar<-mirar$"ID"

	}

	if(dim(mirar)[1]==0){
	
		ids_mirar<-NA

	}
	
	return(ids_mirar)
	
	}
	
depuracion(datos)

