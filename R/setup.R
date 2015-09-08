rm(list=ls())

if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
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
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){ 
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  library(lib, character.only=TRUE)
	}
}

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