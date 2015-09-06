rm(list=ls())

if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
<<<<<<< HEAD
	inPath='~/Dropbox/Research/WardProjects/tensorZ/Data/toModel/';
	outPath='~/Dropbox/Research/WardProjects/tensorZ/Data/fromModel/';
	rPath='~/Research/WardProjects/tensorZ/R/';
	rFuncs=paste0(rPath, 'Funcs/')
}

# General functions/libraries
## See info on package versions and other session info
### at bottom of script
=======
	inPath='~/Dropbox/Research/WardProjects/tensor/Data/toModel/';
	outPath='~/Dropbox/Research/WardProjects/tensor/Data/fromModel/';
	rFuncs='~/Research/WardProjects/tensor/R/Funcs/';
}

if(Sys.info()['user']=='mw160' | Sys.info()['user']=='mw160'){
    inPath='~/Dropbox/tensor/Data/toModel/';
    outPath='~/Dropbox/tensor/Data/fromModel/';
    rFuncs='~/git/tensor/R/Funcs/';
}

# General functions/libraries
>>>>>>> origin/master
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	}
}

<<<<<<< HEAD
toLoad=c(
	'foreign' , 'countrycode' ,
	'reshape2', 'magrittr', 'dplyr', 'stringr','data.table', 'abind',
	'ggplot2', 'grid', 'xtable', 'tikzDevice',	
	'network'
	)
loadPkg(toLoad)

char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
substrRight = function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }

# Set a theme for gg
theme_set(theme_bw())

# Global params
seed=6886
set.seed(seed)

# File with helpful functions
source(paste0(rFuncs, 'sqlHelpers.R'))
source(paste0(rFuncs, 'mltrHelpers.R'))

# Session info
sessionInfo()
=======
toLoad=c('abind', 'RMySQL', 'ggplot2', 'reshape2', 
	'network', 'grid', 'xtable', 'tikzDevice')
loadPkg(toLoad)

# Set a theme for gg
theme_set(theme_bw())

# Relevant functions
source( paste0(rFuncs, '/tfunctions.r') )


# Other functions
mysqlSetup = function(user=NULL, pw=NULL, db=NULL, host="152.3.32.10") {
	tryCatch(conn <<- dbConnect(MySQL(), user=user, password=pw, 
		dbname=db, host=host), 
	error=function(e) warning("MySQL connection does not work") )
}

# Global params
seed=6886
>>>>>>> origin/master
