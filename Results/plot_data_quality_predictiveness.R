plot_data_quality_predictiveness = function(filters, ...){
  pdf(paste(output_filepath, "/Results/Group/Data_Quality_Predictiveness.pdf",sep=""),width=6,height=6.4)
  for(filter in 1:length(names(filters))){
    name_filter = gsub("_"," ",names(filters)[filter])
    print(name_filter)
    coefs=t(sapply(0:5,function(x){y=summary(model_data_quality_predictiveness(filters[[filter]], x))$coef[,3];sign(y)*-log10((1-pnorm(abs(y)))*2)}))
    colnames(coefs) = c("(int)", "Acc", "GPS", "View", "Sub", "Comp")
    
    plot(0,0,col=NA,yaxt="n",xaxt="n",xlab="Weeks Until Prediction",ylab="Data Quality Metric",xlim=c(1.04,7-.04),ylim=c(1.04,7-.04),axes=F,main=name_filter)
    axis(1,at=1:6+.5, labels = 6-1:6)
    axis(2,at=1:6+.5, labels = colnames(coefs))
    for(i in 1:6){
      for(j in 1:6){
        C=1-min(abs(coefs[i,j]),3)/3
        if(coefs[i,j] > 0)  polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=rgb(C,C,1),pch=15,cex=2,border=FALSE)
        if(coefs[i,j] <= 0) polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=rgb(1,C,C),pch=15,cex=2,border=FALSE)
        
        if(coefs[i,j] > 0)  polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=cols(1,start=.6,transparency=C,dirtiness = .05+.15*(1-C)),pch=15,cex=2,border=FALSE)
        if(coefs[i,j] <= 0) polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=cols(1,start=0, transparency=C,dirtiness = .05+.2*(1-C)),pch=15,cex=2,border=FALSE)
      }
    }
    for(i in 1:6){
      for(j in 1:6){
        if(abs(coefs[i,j]) > -log10(0.05)) polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=NA,lwd=2)
      }
    }
  }
  dev.off()
}

