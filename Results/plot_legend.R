plot_legend = function(stream, ...){
  plot_filename = paste(output_filepath, "/Results/Group/legend_",stream,".pdf",sep="")
  bursts   = readRDS(paste(output_filepath, "/Processed_Data/Group/", stream, "_bursts.rds", sep="")) %>% data.frame %>% subset(complete.cases(.))
  coverage = readRDS(paste(output_filepath, "/Processed_Data/Group/", stream, "_coverage.rds", sep="")) %>% data.frame
  patients = bursts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character %>% sort
  dark_colors = cols(length(patients),transparency=1-light_alpha)
  names = bursts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character
  pdf(plot_filename,width=8,height=6)
  par(mgp=c(2.5,1,0),cex.main=1.5,cex.lab=1.5)
  plot(0,0,col=NA,xlab="",ylab="",main="Legend", axes=F)
  legend("topleft",legend=names, col=dark_colors, pch=16,ncol=3,box.col="white",cex=2)
  box()
  dev.off()
}
  
  

