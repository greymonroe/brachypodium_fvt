#plot_fvt_byspecies.R

#plot FVT

plot_fvt<-function(data, trait, real_trait_name, xaxis, saveplot=NULL, species=F){
  
  allmeans<-data.frame()
  mods<-list()
  sums<-list()
  for (sp in c("b_dist","b_sylv")){

    datasp<-subset(data, Sp == sp)
    mod<-lm(datasp[,trait]~Geno+Day_14+I(Day_14^2)+ns(Day_14, df=2)+Day_14*Geno+I(Day_14^2)*Geno+ns(Day_14, df=2)*Geno+Harv+Day_14, data=datasp)
    
    
    model_sel<-stepAIC(mod, direction = "both")
    mods[[sp]]<-model_sel
    newdata = data.frame(Sp=rep(sp, each=500), Geno=rep(unique(datasp$Geno), each=100), Harv=rep(paste0('harvest', rep(1:5, each=20)), times=5), Day_14=rep(rep(seq(0.3, 1, length.out = 20), times=5), times=5))
    newdata<-subset(newdata, Sp %in% datasp$Sp)
    
    pred<-predict(model_sel, newdata = newdata)
    pred<-cbind(pred, newdata)
    
    means<-pred %>% group_by(Geno, Sp, Day_14) %>% summarise(mean=mean(pred, na.rm=T))
    means<-data.frame(means)
    allmeans<-rbind(allmeans, means)
    
    sum<-summary(aov(model_sel))
    
    sums[[sp]]<-data.frame(sum[[1]])
    
  }
  means<-allmeans
  
  ylabel<-real_trait_name
  if(real_trait_name=="RWC") ylabel<-expression('RWC (%)')
  if(real_trait_name=="SLA") ylabel<-expression('SLA (mm'^2*'mg'^-1*')')
  if(real_trait_name=="Green Area") ylabel<-expression('Green area (pixels)')
  if(real_trait_name=="Shoot Mass") ylabel<-expression('Shoot mass (mg)')
  if(real_trait_name=="Root Mass") ylabel<-expression('Root mass (mg)')
  if(real_trait_name=="Root:Shoot") ylabel<-expression('Root : shoot mass ratio')
  if(real_trait_name=="Biomass") ylabel<-expression('Total biomass (mg)')
  if(real_trait_name=="C content") ylabel<-expression('C content (mg g'^-1*'leaf)')
  if(real_trait_name=="d13c") ylabel<-expression(delta^13*'C')
  if(real_trait_name=="N content") ylabel<-expression('N content (mg g'^-1*'leaf)')
  if(real_trait_name=="d15n") ylabel<-expression(delta^15*'N')
  if(real_trait_name=="C:N ratio") ylabel<-expression('C  : N content ratio')
  

  
    p<-ggplot(means, aes(x=Day_14*100, y=mean, group=Geno, col=Sp))+
      #geom_point(data=data, aes_string(y=trait), alpha=0.2, fill="gray", size=0.3, shape=16)+
      geom_point(alpha=0.9, fill="gray", size=.5, shape=16)+
      geom_line(lwd=.4, alpha=0.8)+
      geom_text_repel(data=filter(means, Day_14 == max(means$Day_14)[1]), aes(label=Geno), nudge_x      = 0.1*100,
                      direction    = "y",
                      hjust        = 0,
                      segment.size = 0.2,
                      segment.alpha=0.2,
                      size=1.2,
                      min.segment.length = unit(0, 'lines'))+
      geom_text_repel(data=filter(means, Day_14 == min(means$Day_14)[1]), aes(label=Geno), nudge_x      = -0.1*100,
                      direction    = "y",
                      hjust        = 1,
                      segment.size = 0.2,
                      segment.alpha=0.2,
                      size=1.2,
                      min.segment.length = unit(0, 'lines'))+
      scale_color_manual(values=c("dodgerblue","orange2"), labels=c("B. distachyon","B. sylvaticum"))+
      theme_classic(base_size = 5)+
      theme(legend.position = "none")+
      xlim(c(.1,1.2)*100)+
      labs(y=ylabel, x=expression('Soil moisture (%'[final]*')'))
  
  
  if(xaxis=="Trt"){
    if(length(unique(data$Trt))<3){
      
      mod<-lm(data[,trait]~Sp+Geno+Sp*Trt+Geno*Trt+Harv+Trt, data=data)
      
    } else{ mod<-lm(data[,trait]~Sp+Geno+Trt+I(Trt^2)+ns(Trt, df=2)+Trt*Sp+Sp*I(Trt^2)+ns(Trt, df=2)*Sp+Trt*Geno+I(Trt^2)*Geno+ns(Trt, df=2)*Geno+Harv+Trt, data=data)
    }
    model_sel<-stepAIC(mod, direction = "both")
    newdata = data.frame(Sp=rep(c("b_dist","b_sylv"), each=5*5*length(unique(data$Trt))), Geno=rep(unique(data$Geno), each=5*length(unique(data$Trt))), Harv=rep(paste0('harvest', rep(1:5, each=length(unique(data$Trt)))), times=10), Trt=rep(rep(unique(data$Trt), times=5), times=10))
    
    pred<-predict(model_sel, newdata = newdata)
    pred<-cbind(pred, newdata)
    
    means<-pred %>% group_by(Geno, Sp, Trt) %>% summarise(mean=mean(pred, na.rm=T))
    
    
    
    p<-ggplot(means, aes(x=Trt, y=mean, group=Geno, col=Sp))+
      geom_point(data=data, aes_string(y=trait), col="gray")+
      geom_line(lwd=1.5, alpha=0.5)+
      geom_label_repel(data=filter(means, Trt == max(means$Trt)[1]), aes(label=Geno), nudge_x = 2,
                       direction    = "y",
                       hjust        = 0,
                       segment.size = 0.2)+
      geom_label_repel(data=filter(means, Trt == min(means$Trt)[1]), aes(label=Geno), nudge_x   = -2,
                       direction    = "y",
                       hjust        = 1,
                       segment.size = 0.2)+
      scale_color_manual(values=c("dodgerblue","orange2"), labels=c("B. distachyon","B. sylvaticum"))+
      theme_classic()+
      theme(legend.position = "none")+
      labs(y=trait, x="Soil moisture (%)")+
      xlim(c(-2,22))
    
  }
  
  if (!is.null(saveplot)){
    pdf(paste0(saveplot,trait,"_",xaxis,".pdf"))
    plot(p)
    dev.off()
    
  }
  return(list(aov=sums, means=means, model=mods, plot=p))
  
}
