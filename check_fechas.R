

datos$"fechas.DF"<-as.Date(datos$"fdef",format="%Y.%m.%d")

# Suma de missing
sum(is.na(datos$"fdef"))
sum(is.na(datos$"fechas.DF"))


# Posicion de missing
indice1<-which(is.na(datos$fdef))
indice2<-which(is.na(datos$fechas.DF))

indice1[1:5]
indice2[1:5]


# Check de posicion de missing
sum(indice1==indice2)

############################################
# CUANDO HAY VARIOS FORMATOS EN LA FECHA
############################################

datos$"fdef.mal"<-datos$"fdef"
datos$"fdef.mal"[c(116,146)]<-c("99/04/22","83/07/15")
datos$"fdef.mal"[c(77)]<-c("04-05-2013")


datos$"fdef.mal"[1]

datos$"fecha.DF.mal"<-as.Date(datos$"fdef.mal",format="%Y.%m.%d")

# Suma de missing
sum(is.na(datos$"fdef.mal"))
sum(is.na(datos$"fecha.DF.mal"))

# Caracteres
longitud_caracteres<-nchar(datos$"fdef.mal")

datos$"fdef.mal"[longitud_caracteres<10 & !is.na(longitud_caracteres)]

datos$"fdef.mal"[longitud_caracteres<10 & !is.na(longitud_caracteres)]<-c("1999.04.22","1983.07.15")

datos$"fecha.DF.mal"<-as.Date(datos$"fdef.mal",format="%Y.%m.%d")
sum(is.na(datos$"fdef.mal"))
sum(is.na(datos$"fecha.DF.mal"))

datos[is.na(datos$"fecha.DF.mal") & !is.na(datos$"fdef.mal"),"fdef.mal"]<-c("2013.05.04")

datos$"fecha.DF.mal"<-as.Date(datos$"fdef.mal",format="%Y.%m.%d")
sum(is.na(datos$"fdef.mal"))
sum(is.na(datos$"fecha.DF.mal"))

##########################################################
# CUANDO HAY VARIOS FORMATOS EN LA FECHA (otra version)
##########################################################

datos$"fdef.mal"<-datos$"fdef"
datos$"fdef.mal"[c(116,146)]<-c("99/04/22","83/07/15")
datos$"fdef.mal"[c(77)]<-c("04-05-2013")

# Creo una variable fecha
datos$"fecha.DF.mal"<-as.Date(datos$"fdef.mal",format="%Y.%m.%d")

# Suma de missing
sum(is.na(datos$"fdef.mal"))
sum(is.na(datos$"fecha.DF.mal"))

# Identifico los valores raros
datos[is.na(datos$"fecha.DF.mal") & !is.na(datos$"fdef.mal"),"fdef.mal"]


# Recodifico los valores raros
datos[is.na(datos$"fecha.DF.mal") & !is.na(datos$"fdef.mal"),"fdef.mal"]<-c("2013.05.04",
"1999.04.22","1983.07.15")

# Creo una variable fecha
datos$"fecha.DF.mal"<-as.Date(datos$"fdef.mal",format="%Y.%m.%d")

# Suma de missing
sum(is.na(datos$"fdef.mal"))
sum(is.na(datos$"fecha.DF.mal"))



