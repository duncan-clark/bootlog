#'
#' Simluates networks from a lolog model and fits the model to these networks
#' @param
#' @keywords 
#' @export
#' @examples
#' bootlog()

#Fits done under parallisation in estimates_test
#estimates_test returns the LOLOG fits.
bootlog <- function(lolog,
                    seed = 1,
                    fits = 10,
                    cores = 2,
                    MoreArgs = NULL){
  #MoreArgs are additional arguments needed for network fitting e.g. edge covariate matrices
  #load in MoreArgs
  if(!is.null(MoreArgs)){
    for(i in 1:length(MoreArgs)){
      assign(names(MoreArgs)[i],MoreArgs[[i]])
    }
  }
  
  #generate the network
  net <- simulate(lolog,nsim = 1,seed= seed)
  net <- lolog::as.network(net[[1]])
  
  #make sure network is what we want
  formula <- as.character(lolog$formula)
  formula[2] <- "net"
  formula <- as.formula(paste(formula[2],formula[1],formula[3]))
  
  # #Check that we can fit a lolog to this network:
  # if(is.null(tryCatch({lolog(formula)},
  #                     error = function(e){print("Unable to fit the LOLOG to the simulated network")
  #                       return(NULL)}))){return(NULL)}
  
  #remove original net to stop confusion
  rm(net)
  
  #generate multiple networks and fit LOLOGs
  sims <- simulate(lolog,nsim = fits,seed = seed)
  sims <- lapply(sims,function(x){lolog::as.network(x)})
  
  fit <- function(net,formula){
    net <<- net
    tmp <- tryCatch({lolog(formula,verbose = 0)},
                    error = function(e){return(NA)},
                    warning = function(w){if(w$message == "In lolog(formula) : Singular statistic covariance matrix. Using diagnoal."){return(NA)}}
    )
    rm(net,envir=globalenv())
    if(length(tmp)==0){return(NA)}
    if(is.na(tmp)){
      return(tmp)
    }else{return(tmp$theta)}
  }
  
  print(paste("The parallelisation started at ", Sys.time(),sep = ""))
  #if the fits are more that 1000 split up into batches of 1000
  cl <- parallel::makeCluster(cores)
  registerDoParallel(cl)
  
  fits <- foreach(i = (1:length(sims)), .inorder = FALSE, .packages = "lolog",.errorhandling = "pass") %dopar% {fit(sims[[i]],formula)}
  
  parallel::stopCluster(cl)
  print(paste("The parallelisation ended at ", Sys.time(),sep = ""))
  
  return(list(fits = fits,nets = sims))
}