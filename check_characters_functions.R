datos$ID[1:3]

datos$ID[nchar(datos$ID)%in%2]

datos$ID[nchar(datos$ID)%in%2] <- paste("0",datos$ID[nchar(datos$ID)%in%2],sep="")

datos$ID[nchar(datos$ID)%in%1] <- paste("00",datos$ID[nchar(datos$ID)%in%1],sep="")
table(nchar(datos$ID))


datos$"ID_sexo"<-paste(datos$ID, datos$sexo,datos$fdef , sep="_")

mirar<-strsplit(datos$"ID_sexo",split="_")

library("plyr")
mirar<-ldply(mirar)

datos$"ID_tras_split"<-mirar$"V1"



id_pequeño1<-substring(datos$"ID_sexo",first=1,last=2) 

id_pequeño2<-substring(datos$"ID_sexo",first=1,last=1) 

id_pequeño3<-substring(datos$"ID_sexo",first=2,last=3) 


unique(datos$"fumador")
table(datos$fumador,exclude=NULL)

datos$"fumador"<-toupper(datos$"fumador")


datos$ID.new1<-NA
datos$ID.new1[datos$sexo%in%"Hombre"]<-paste(datos$ID[datos$sexo%in%"Hombre"],"--h",sep="")
datos$ID.new1[datos$sexo%in%"Mujer"]<-paste(datos$ID[datos$sexo%in%"Mujer"],"--m",sep="")


datos$ID.new2<-substring(datos$ID.new1,2,3)

datos$ID.new3<-gsub("-",".",datos$ID.new1)

datos$ID.new3<-gsub(x=datos$ID.new1,replacement=".",pattern="-")

x<-c(1:5)
y<-c(1:5)

mi.funcion <- function(val1,val2,...){
		
		valores.x<-val1*5
		valores.y<-val2*6
		
		plot(x=valores.x,y=valores.y,...)
		
	}


ls()

mi.funcion(val1=x,val2=y)




