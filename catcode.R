catcode=function(facvar, cgroup=as.character(levels(facvar)[1]), codetype="Dummy"){
	
	#Recode a factor variable with G (non-NA) groups into G-1 coded vectors for entry into regression.
	#facvar is a factor variable representing the groups of interest.
	#cgroup is the control group (or comparison group)of interest, entered as a character string.
	#cgroup must equal "Dummy", "Effect", or "HalfEffect".
	
	if(!is.factor(facvar)) stop("Factor variable must be a factor!")
	if(!is.character(cgroup)) stop("Control group must be specified as a character string in quotes")
	if(codetype != "Dummy" & codetype != "Effect" & codetype != "HalfEffect") stop ("codetype must be Dummy, Effect or HalfEffect.")
	
	if(length(!is.na(levels(facvar)))==2){
		if(codetype == "Dummy") NewVar=ifelse(is.na(facvar), NA, ifelse(facvar!=cgroup, 1, 0))
		if(codetype == "Effect") NewVar=ifelse(is.na(facvar), NA, ifelse(facvar!=cgroup, 1, -1))
		if(codetype == "HalfEffect") NewVar=ifelse(is.na(facvar), NA, ifelse(facvar!=cgroup, .5, -.5))
		
		return(NewVar)
		
	}else if(length(!is.na(levels(facvar)))>2){
		
		groups = levels(facvar)[levels(facvar) != cgroup]
		NewVars = matrix(,nrow=length(facvar), ncol=length(groups))
		colnames(NewVars) = paste(groups, codetype, sep=".")
	
		if(codetype=="Dummy") for(i in 1:length(groups)) NewVars[,i] = ifelse(is.na(facvar), NA, ifelse(facvar==groups[i], 1, 0))
		if(codetype=="Effect") for(i in 1:length(groups)) NewVars[ ,i] = ifelse(is.na(facvar), NA, ifelse(facvar==cgroup, -1, ifelse(facvar==groups[i], 1, 0)))
		if(codetype=="HalfEffect") for(i in 1:length(groups)) NewVars[ ,i] = ifelse(is.na(facvar), NA, ifelse(facvar==cgroup, -.5, ifelse(facvar==groups[i], .5, 0)))
		return(NewVars)		
	}
	 
}
