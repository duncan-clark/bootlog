#'
#' Does bootstrapped SEs for LOLOG in a nice way
#' @param
#' @keywords 
#' @export
#' @examples
#' bootlog_process()


#This function uses bootlog and processes everything into nice results
bootlog_process <- function(lolog,
                            seed =1,
                            fits = seq(10,100,10),
                            cores = 2,
                            MoreArgs = NULL){
  #MoreArgs are additional arguments needed for network fitting e.g. edge covariate matrices
  #load in MoreArgs
  if(!is.null(MoreArgs)){
    for(i in 1:length(MoreArgs)){
      assign(names(MoreArgs)[i],MoreArgs[[i]])
    }
  }
  
  #Do parallelised bootstrapping
  total_fits <- sum(fits)
  tmp <- bootlog(lolog,fits = total_fits,cores = cores,MoreArgs = MoreArgs,seed = seed)
  results_fits <- tmp$fits
  results_nets <- tmp$nets
  
  for(i in fits){
    if(i== fits[1]){j <- 1}
    fits_tmp <- results_fits[j:(i+j-1)]
    j <- j + i
    
    fits_tmp <- fits_tmp[!is.na(fits_tmp)]
    
    #extract what we're interested in:
    mean = apply(do.call(rbind,fits_tmp),2,FUN = mean)
    sd =  apply(do.call(rbind,fits_tmp),2,FUN = sd)
    
    if(i==fits[1]){results <- rbind(mean,sd)}
    else{results <-rbind(results,mean,sd)}
  }
  
  #Process Results
  results <- as.data.frame(results)
  results$fits <- as.factor(rep(fits,each =2))
  results$type  <- as.factor(rep(c("mean","sd"),length(fits)))
  rownames(results) <- mapply(paste,rownames(results),results$fits)
  return(list(results = results,fits = results_fits,nets = results_nets ))
}