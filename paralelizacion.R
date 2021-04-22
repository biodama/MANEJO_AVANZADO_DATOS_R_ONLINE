

##################
# Version for
##################

rm(list=ls())
gc()

load("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DE_DATOS_ONLINE/datos/genes.grande.RData")

inicio<-Sys.time()

for(i in 2:dim(genes)[2]){
	
	print(i)	
	genes[,i]<-gsub(" ","",genes[,i])
	
	
	
}
final<-Sys.time()
tiempo1<-final-inicio
tiempo1  # Time difference of 17.78511 mins

head(genes[,c(1:10)])

##################
# Version apply
##################

rm(list=ls())
gc()

load("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DE_DATOS_ONLINE/datos/genes.grande.RData")

inicio<-Sys.time()
res<-apply(genes[,-1],2,function(x) gsub(" ","",x))
final<-Sys.time()
tiempo2<-final-inicio
tiempo2  # 20.57352 secs

head(genes[,c(1:10)])
head(res[,c(1:10)])

########################
# Version parallel
########################

rm(list=ls())
gc()

library("parallel")

load("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DE_DATOS_ONLINE/datos/genes.grande.RData")

numWorkers <- detectCores()-1

cluster <- makeCluster(numWorkers, type = "PSOCK")
#cl<-makeCluster(numWorkers, type = "MPI")

inicio<-Sys.time()
res<-clusterApply(cl = cluster, genes[,-1], function(x) gsub(" ","",x))
final<-Sys.time()
tiempo3<-final-inicio
tiempo3  # 34.56817 secs

stopCluster(cluster)

res.data.frame<-data.frame(res)
names(res.data.frame)<-names(genes)[-1]
head(genes[,c(1:10)])
head(res.data.frame[,c(1:10)])




##################################
# Version parallel (version list)
##################################

rm(list=ls())
gc()

library("parallel")

load("/Users/pfernandezn/Desktop/MANEJO_AVANZADO_DE_DATOS_ONLINE/datos/genes.grande.RData")

numWorkers <- detectCores()-1

cluster <- makeCluster(numWorkers, type = "PSOCK")
#cl<-makeCluster(numWorkers, type = "MPI")

geneslist<-list(genes[,-1])[[1]]

inicio<-Sys.time()
res<-parLapply(cl = cluster, geneslist, function(x) gsub(" ","",x))
final<-Sys.time()
tiempo4<-final-inicio
tiempo4 # 17.74182 secs

stopCluster(cluster)

res.data.frame<-data.frame(res)
names(res.data.frame)<-names(genes)[-1]
head(genes[,c(1:10)])
head(res.data.frame[,c(1:10)])
