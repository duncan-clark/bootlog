#'
#' Plots the bootstrapped SEs in a nice way
#' @param
#' @keywords 
#' @export
#' @examples
#' bootlog_plot()

#This function takes in a results dataframe as produced by estimates_test_process and makes ggplots
bootlog_plot <- function(results,
                         lolog,
                         facet = TRUE){
  results_unmelted <- results
  #Melt Results for ggplot
  results = melt(results,id.vars= c("fits","type"))
  n <- length(summary(lolog)[,1]) 
  line = data.frame(type = as.factor(rep(c("mean","sd"),n)),
                    variable = rep(rownames(summary(lolog)),each = 2))
  line$value <- rep(0,n*2)
  line$value[seq(1,(n*2 -1),2)] <- summary(lolog)$theta[match(line$variable[line$type == "mean"],rownames(summary(lolog)))]
  line$value[seq(2,(n*2 ),2)] <- summary(lolog)$se[match(line$variable[line$type == "sd"],rownames(summary(lolog)))]
  line$linetype = 2
  
  if(facet){plot <- ggplot(data = results[results$type =="sd",],aes(x = fits,y = value,colour = variable,group =variable))+
    facet_grid(rows = vars(variable),scales = "free")+
    scale_colour_duncan()+
    geom_line(show.legend = TRUE)+
    theme(legend.title = element_blank())+
    geom_hline(data = line[line$type == "sd",],aes(yintercept = value,fill = "LOLOG Stated SE"),linetype = "dashed")+
    ggtitle("Plot of simulated standard errors with varying number of simulations.")
  return(list(results = results_unmelted,plots = plot))
  }else{
    for(i in unique(results$variable)){
      sd_line = line[line$type == "sd" & line$variable == i,]$value
      plot <- ggplot(data = results[results$type =="sd" & results$variable== i,],aes(x = fits,y = value,colour = variable,group =variable))+
        scale_colour_duncan()+
        geom_line(show.legend = TRUE)+
        theme(legend.title = element_blank())+
        coord_cartesian(ylim = (c(sd_line*0.1,sd_line*1.9)))+
        geom_hline(data = line[line$type == "sd" & line$variable == i,],aes(yintercept = value,fill = "LOLOG Stated SE"),linetype = "dashed")+
        ggtitle("Plot of simulated standard errors with varying number of simulations.")
      assign(paste("plot_",i,sep=""),plot)
    }
    plots <- lapply(unique(results$variable),function(x){eval(parse(text=paste("plot_",x,sep="")))})
    return(list(results = results_unmelted,plots = plots))
  }
}