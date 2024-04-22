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


