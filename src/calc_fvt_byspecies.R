#calc_fvt

traits<-c("Relative_WC", "SLA", "aboveground_greenarea", "Shoot_Mass", "Root_Mass","Shoot_Root_Ratio","biomass", "c_content","d13c", "n_content", "d15n", "c_n")
real_trait_names<-c("RWC","SLA","Green Area","Shoot Mass", "Root Mass", "Root:Shoot","Biomass", "C content","d13c", "N content","d15n",  "C:N ratio")

mod<-lm(datasheet$SLA~
          Geno+
          Day_14+
          I(Day_14^2)+
          ns(Day_14, df=2)+
          Day_14*Geno+
          I(Day_14^2)*Geno+
          ns(Day_14, df=2)*Geno+
          Harv+
          Day_14, data=datasheet)

model_selection_summaries<-data.frame(variables=row.names(data.frame(summary(aov(mod))[[1]])))

alldata<-data.frame(Geno=sort(unique(datasheet$Geno)), Sp=c("b_dist","b_sylv")[c(1,1,2,2,1,1,1,2,2,2)])
allmeans<-data.frame(Geno=rep(alldata$Geno, each=20),Sp=rep(alldata$Sp, each=20), Day_14=rep(seq(0.3, 1, length.out = 20), times=10))
curve_plots<-list()
out_models<-data.frame()
r2<-data.frame()

for(i in 1:length(traits)){
  
  trait<-traits[i]
  real_trait_name<-real_trait_names[i]
  out<-plot_fvt(data=datasheet, trait=trait, real_trait_name=real_trait_name, xaxis="Day_14")
  all_variables<-unlist(lapply(strsplit(gsub(" ","",as.character(model_selection_summaries$variables)), ":"), function(x) paste(sort(x), collapse = "")))
  r2<-rbind(r2, data.frame(trait, sp="b_dist", r2=summary(out$model$b_dist)$r.squared, AIC=AIC(out$model$b_dist)))
  r2<-rbind(r2, data.frame(trait, sp="b_sylv", r2=summary(out$model$b_sylv)$r.squared, AIC=AIC(out$model$b_sylv)))
  
  for (sp in c("b_dist","b_sylv")){
  model_variables<-unlist(lapply(strsplit(gsub(" ","",rownames(out$aov[[sp]])), ":"), function(x) paste(sort(x), collapse = "")))
  row.names(out$aov[[sp]])<-model_variables
  
  p_vals<-out$aov[[sp]]$Pr..F.
  
  out$aov[[sp]]$trait<-paste(trait, sp)
  out_models<-rbind(out_models, out$aov[[sp]])
  
  model_selection_summaries[,paste0(trait, sp)]<-
    ifelse(all_variables %in% model_variables, "x", "")
  
  model_selection_summaries[all_variables %in% model_variables[p_vals<0.05],paste0(trait, sp)]<-"x"
  model_selection_summaries[all_variables %in% model_variables[p_vals<0.001],paste0(trait, sp)]<-"x"
  model_selection_summaries[all_variables %in% model_variables[p_vals<0.00001],paste0(trait, sp)]<-"x"
  
  #model_selection_summaries[,paste0(trait, sp)]<-paste(model_selection_summaries[,paste0(trait, sp)], ifelse(all_variables %in% sig_variables, "", ""))
  }
  
  trait_data<-data.frame(out$means %>% spread(Day_14, mean))
  colnames(trait_data)<-c("Geno","Sp",paste(trait, 1:20, sep="_"))
  alldata<-merge(alldata, trait_data)
  
  colnames(out$means)[4]<-trait
  
  allmeans<-merge(allmeans, out$means)
  curve_plots[[i]]<-out$plot
}
alldata<-alldata[order(alldata$Sp),]

t_model_selection_summaries<-t(model_selection_summaries)
colnames(t_model_selection_summaries)<-c("G","E","E^2","s(E)","H","G*E","G*E^2","G*s(E)","res")
t_model_selection_summaries<-t_model_selection_summaries[-1,]
t_model_selection_summaries<-t_model_selection_summaries[,-ncol(t_model_selection_summaries)]
row.names(t_model_selection_summaries)<-paste0(rep(real_trait_names, each=2), " - ", rep(c("B. distachyon", "B. sylvaticum"), times=length(real_trait_names)), "")
t_model_selection_summaries_byspecies<-t_model_selection_summaries


pdf("../Figures/curves_byspecies.pdf", width=3.7, height=6.4)
plots<-plot_grid(plotlist = curve_plots, ncol = 2,labels="auto", label_size = 8, vjust=c(1,1,rep(0.5, length(curve_plots)-2)), hjust = c(0,0.5))
#allplots<-plot_grid(plots, pca_plots, ncol=2, rel_widths = c(2, 1))
print(plots)
dev.off()


# PCAS

#trait_data<-data.frame(out$means %>% spread(Day_14, Shoot_Root_Ratio))
#colnames(trait_data)<-c("Geno","Sp",paste(trait, 1:20, sep="_"))

df <- allmeans[4:ncol(allmeans)]
df_scaled<-scale(df)
pca<-prcomp(df_scaled)
pca_sum<-summary(pca)
pca_df<-data.frame(Sp=allmeans$Sp, Geno=allmeans$Geno, Size=1/allmeans$Day_14, Trt=allmeans$Day_14, PCA1=pca$x[,1], PCA2=pca$x[,2],PCA3=pca$x[,3], PCA4=pca$x[,4])
vectors_df<-data.frame(PCA1=pca$rotation[,1], PCA2=pca$rotation[,2], PCA3=pca$rotation[,3], PCA4=pca$rotation[,4], variable=names(pca$rotation[,2]))
vectors_df$variable<-real_trait_names
geno_df<-pca_df %>% filter(Trt==min(Trt))

pca_plot<-ggplot(pca_df, aes(x=PCA1, y=PCA2))+
  geom_point(aes( group=Geno, col=Sp, size=Size), alpha=0.3, shape=16)+
  scale_size_continuous(range = c(.5, 3), breaks=c(1,1.5,2,2.4,3.333333), labels=c(1, .66, .5, .4, .33), name = expression('Soil moisture (%'[final]*')'))+
  scale_color_manual(values=c("dodgerblue","orange2"), labels=c("B. distachyon","B. sylvaticum"))+
  geom_path(aes( group=Geno, col=Sp), size=0.2)+
  theme_classic(base_size = 6)+
  geom_label_repel(data=geno_df, aes(label=Geno, col=Sp), size=1)+
  labs(x=paste0("PC1 (",round(pca_sum$importance[2,1],digits = 2)*100, "%)"), y=paste0("PC2 (",round(pca_sum$importance[2,2],digits = 2)*100, "%)"))+
  guides(col=F)+
  theme(legend.position = "none", legend.key.size = unit(.1, 'points'))

pca1_vectors<-ggplot(vectors_df, aes(x=PCA1, y=PCA2))+
  geom_point(col="gray90", size=0.5)+
  geom_segment(data=vectors_df, col="gray90", xend=0, yend=0, aes( x=PCA1, y=PCA2), size=.25)+
  geom_text_repel(data=vectors_df,  col="black", aes(label=variable, x=PCA1, y=PCA2), segment.size = 0.1, segment.color="gray50", size=1)+
  theme_classic(base_size = 6)

pca_plot2<-ggplot(pca_df, aes(x=PCA3, y=PCA4, group=Geno, col=Sp))+
  geom_point(aes( group=Geno, col=Sp, size=Size), alpha=0.3, shape=16)+
  scale_size_continuous(range = c(.5, 3), breaks=c(1,1.5,2,2.4,3.333333), labels=c(1, .66, .5, .4, .33), name = expression('Soil moisture (%'[final]*')'))+
  scale_color_manual(values=c("dodgerblue","orange2"), labels=c("B. distachyon","B. sylvaticum"))+
  geom_path(aes( group=Geno, col=Sp), size=0.2)+
  theme_classic(base_size = 6)+
  geom_label_repel(data=geno_df, aes(label=Geno, col=Sp), size=1)+
  labs(x=paste0("PC3 (",round(pca_sum$importance[2,3],digits = 2)*100, "%)"), y=paste0("PC4 (",round(pca_sum$importance[2,4],digits = 2)*100, "%)"))+
  guides(col=F)+
  theme(legend.position = "none", legend.key.size = unit(.1, 'points'))

pca_plot2_legend<-ggplot(pca_df, aes(x=PCA3, y=PCA4, group=Geno, col=Sp))+
  geom_point(aes( group=Geno, col=Sp, size=Size*100), alpha=0.3, shape=16)+
  scale_size_continuous(range = c(.5, 3), breaks=c(1,1.5,2,2.4,3.333333)*100, labels=c(1, .66, .5, .4, .33)*100, name = expression('Soil moisture (%'[final]*')'))+
  scale_color_manual(values=c("dodgerblue","orange2"), labels=c("B. distachyon","B. sylvaticum"))+
  geom_path(aes( group=Geno, col=Sp), size=0.2)+
  theme_classic(base_size = 6)+
  geom_label_repel(data=geno_df, aes(label=Geno, col=Sp), size=1)+
  labs(x=paste0("PC3 (",round(pca_sum$importance[2,3],digits = 2)*100, "%)"), y=paste0("PC4 (",round(pca_sum$importance[2,4],digits = 2)*100, "%)"))+
  guides(col=F)+
  theme(legend.position = "top", legend.key.size = unit(.1, 'points'))

pca2_vectors<-ggplot(vectors_df, aes(x=PCA3, y=PCA4))+
  geom_point(col="gray90", size=0.25)+
  geom_segment(data=vectors_df, col="gray90", xend=0, yend=0, aes( x=PCA3, y=PCA4), size=.25)+
  geom_text_repel(data=vectors_df,  col="black", aes(label=variable, x=PCA3, y=PCA4), size=1)+
  theme_classic(base_size = 6)

pca_plot2.with.inset <-
  ggdraw() +
  draw_plot(pca_plot2) +
  draw_plot(pca2_vectors, x = .9, y = .65, width = .5, height = .5)

multitraitplasticity<-data.frame(Sp=allmeans$Sp, Geno=allmeans$Geno, Size=1/allmeans$Day_14, Trt=allmeans$Day_14)
multitraitplasticity<-cbind(multitraitplasticity, df_scaled)

plasts<-c()
for (geno in unique(multitraitplasticity$Geno)){
geno<-multitraitplasticity[multitraitplasticity$Geno==geno,5:ncol(multitraitplasticity)]
distmat<-as.matrix(dist(geno))
plast<-c(diag(distmat[-1,-ncol(distmat)]),"NA")
plasts<-c(plasts, plast)
}

multitraitplasticity$plasticity<-as.numeric(plasts)
multitraitplasticity$gp<-paste(multitraitplasticity$Sp, multitraitplasticity$Trt)

ttestout<-c()
for(i in unique(multitraitplasticity$Trt)[1:19]){
  sub<-multitraitplasticity[multitraitplasticity$Trt==i,]
  out<-t.test(sub$plast~sub$Sp)$p.value
  ttestout<-c(ttestout, out)
}

ttestdf<-data.frame(Trt=unique(multitraitplasticity$Trt)[1:19], p=ttestout)
ttestdf$sig<-ifelse(ttestdf$p<0.05, "*","")
ttestdf$gp=""
ttestdf$Sp=""

plasticityplot<-ggplot(multitraitplasticity, aes(x=Trt, y=plasticity, group=gp, col=Sp))+
  geom_boxplot(position="dodge", outlier.size = 0.5, size=0.25)+
  theme_classic(base_size = 6)+
  scale_color_manual(values=c("dodgerblue","orange2"), labels=c("B. distachyon","B. sylvaticum"))+
  theme(legend.position = "none")+
  labs(y=expression('Total plasticity ('*Delta*' across all traits)'), x=expression('Soil moisture (%'[final]*')'))+
  annotate(geom = "text", x=ttestdf$Trt, y=1.1, label=ttestdf$sig)

pdf("../Figures/multi_plast.pdf", width=3, height=3)
pca_plots<-plot_grid(pca_plot, pca_plot2,  pca1_vectors, pca2_vectors, nrow=2, labels=c("b","c","",""),label_size = 8, rel_heights=c(1,1,1,1))
pcaplot2<-plot_grid(cowplot::get_legend(pca_plot2_legend), pca_plots, ncol=1, rel_heights=c(0.2, 2))
allplots<-plot_grid(plasticityplot, pcaplot2, ncol=2, labels=c("a", ""), rel_widths = c(2, 2),label_size = 8)
print(plasticityplot)
dev.off()

pdf("../Figures/pca_byspecies.pdf", width=3.5, height=3.5)
pca_plots<-plot_grid(pca_plot, pca_plot2,  pca1_vectors, pca2_vectors, nrow=2, labels=c("a","b","",""),label_size = 8, rel_heights=c(1,1,1,1))
pcaplot2<-plot_grid(cowplot::get_legend(pca_plot2_legend), pca_plots, ncol=1, rel_heights=c(0.2, 2))
allplots<-plot_grid(plasticityplot, pcaplot2, ncol=2, labels=c("a", ""), rel_widths = c(2, 2),label_size = 8)
print(pca_plots)
dev.off()


#write.table(out_models, "../Tables/out_models.csv")
save(allmeans, file="../Data/allmean.rdata")

