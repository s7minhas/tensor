if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/WardProjects/tensorZ/R/setup.R') }

# Create sample frame 
source(paste0(rPath, 'Data/getData/sampFrame.R'))

# Get list of cleaning scripts
files = paste0(rPath, 'Data/getData/') %>% list.files() # get cleaning files
files = files[substrRight(files, 2) == '.R'] # only R files
files = files[substr(files, 1, 3) == 'get'] # only get files
files = setdiff(files, 'getIQuad.R') # sql and parallel errors?
cleanScripts = paste0(
	paste0(rPath, 'Data/getData/'), files )

# Parameters for parallelization
toLoad = c('foreach', 'doParallel')
loadPkg(toLoad)
cl = makeCluster(8)
registerDoParallel(cl)

# Run cleaning scripts in parallel
foreach(script = cleanScripts) %dopar% { source(script) }
stopCluster(cl)

# Run iquad
system(paste0("Rscript ", paste0(rPath, 'Data/getData/getIQuad.R')))