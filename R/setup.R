rm(list=ls())

if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){
	# dPath='~/Dropbox/Research/tensor/'
	dPath='/Volumes/Samsung_X5/Dropbox/Research/tensor/'
	inPath=paste0(dPath,'Data/toModel/')
	outPath=paste0(dPath,'Data/fromModel/')
	rFuncs='~/Research/tensor/R/Funcs/';
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
mysqlSetup = function(user=NULL, pw=NULL, db=NULL, host=NULL) {
	tryCatch(conn <<- dbConnect(MySQL(), user=user, password=pw, 
		dbname=db, host=host), 
	error=function(e) warning("MySQL connection does not work") )
}

# Global params
seed=6886