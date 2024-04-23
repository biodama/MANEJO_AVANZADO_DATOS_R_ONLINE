
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

tilde_out_to_1 <- function(var,to="upper"){
	
	variable<-var
	
	if(to=="upper"){
	
		variable<-toupper(variable)
		variable<-gsub("Ó","O",variable)
		variable<-gsub("Í","I",variable)
		variable<-gsub("Á","A",variable)	
		variable<-gsub("É","E",variable)	
		variable<-gsub("Ú","U",variable)	
	
	}
	
	if(to=="lower"){
		
		variable<-tolower(variable)
		variable<-gsub("ó","o",variable)
		variable<-gsub("í","i",variable)
		variable<-gsub("á","a",variable)	
		variable<-gsub("é","e",variable)	
		variable<-gsub("ú","u",variable)	
		
	}
		
	return(variable)
	
}


tilde_out_to_2 <- function(var,to){
	
	variable<-var
	
	if(to=="upper"){
	
		variable<-toupper(variable)
		variable<-gsub("Ó","O",variable)
		variable<-gsub("Í","I",variable)
		variable<-gsub("Á","A",variable)	
		variable<-gsub("É","E",variable)	
		variable<-gsub("Ú","U",variable)	
	
	}
	
	if(to=="lower"){
		
		variable<-tolower(variable)
		variable<-gsub("ó","o",variable)
		variable<-gsub("í","i",variable)
		variable<-gsub("á","a",variable)	
		variable<-gsub("é","e",variable)	
		variable<-gsub("ú","u",variable)	
		
	}
		
	return(variable)
	
}

tilde_out_to_3 <- function(var,to){
	
	variable<-var
	
	if(class(variable)=="numeric"){
	#if(is.numeric(variable))	
		print("no hago nada porque es una variable numerica")
		variable<-as.numeric(variable)
		
	}	
	
	if(class(variable)=="character"){
	#if(is.character(variable))	
	
		if(to=="upper"){
	
			variable<-toupper(variable)
			variable<-gsub("Ó","O",variable)
			variable<-gsub("Í","I",variable)
			variable<-gsub("Á","A",variable)	
			variable<-gsub("É","E",variable)	
			variable<-gsub("Ú","U",variable)	
	
		}
	
		if(to=="lower"){
		
			variable<-tolower(variable)
			variable<-gsub("ó","o",variable)
			variable<-gsub("í","i",variable)
			variable<-gsub("á","a",variable)	
			variable<-gsub("é","e",variable)	
			variable<-gsub("ú","u",variable)	
		
		}
		
		
	}
	
	return(variable)
	
	
	
}

