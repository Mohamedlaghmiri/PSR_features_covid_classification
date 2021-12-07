

##########---- Audio feature transformation script
##########---- Phase space reconstruction
getwd()



setwd("C:/Users/Mohamed/Desktop/Coswara/")


files = as.data.frame(read.csv('psr_features.csv'))

library(parallelly)
library(foreach)
library(doParallel)
library(future)
library(doFuture)
library(snow)
library(doSNOW)
library(nonlinearTseries)
library(tseriesChaos)
ncores <- detectCores(logical = FALSE)        # Create parallel workers (8 cores -1). 
cl<-makeCluster(ncores-1) #file added for debugging purposes.
registerDoParallel(cl)


##########----Dataset is split in 8 functions (ncores = 8)
##########----to apply parallel multicore processing.


process2D <- function(i) {
  setwd("C:/Users/Mohamed/Desktop/Coswara/")
  library(nonlinearTseries)
  library(crqa)
  library(Rcpp)
  library(parallelly)
  library(tseriesChaos)
  library(foreach)
  library(doParallel)
  library(future)
  library(doFuture)
  library(snow)
  library(doSNOW)
  files = as.data.frame(read.csv('psr_features.csv'))
  data <- read.csv(files[i,])
  data <- as.numeric(unlist(data))
  if ((max(data) > 0.1) & (length(data)>20000)) {
    tau.ami <- timeLag(data, technique = "ami", lag.max = 100, do.plot = F)
    emb.dim = estimateEmbeddingDim(data, time.lag = tau.ami, max.embedding.dim = 15, do.plot = F)
    Takens <- buildTakens(data, emb.dim, tau.ami)
    paste0('Completed embedding unit: ',as.character(i))
    x <- Takens[,1]
    y <- Takens[,2]
    z <- Takens[,3]
    #u <- (1/3) * (x + y + z)
    v <- (1/sqrt(6)) * (x + y - 2*z)
    w <- (1/sqrt(2)) * (x - y)
    file <- data.frame(v, w)
    # save file
    write.csv(file, paste0('Extracted_data/2D-psr_all_variables/',as.character(substring(files[i,],28))), row.names = FALSE)
    print('done')
  }
}
foreach(i = 1:442865) %dopar% process2D(i) # specify STARTING point

