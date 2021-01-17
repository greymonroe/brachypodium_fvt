library(lme4)
library(MuMIn)
library(tidyverse)
library(cowplot)
#https://uoftcoders.github.io/rcourse/lec09-model-selection.html




# prepare data ------------------------------------------------------------


library(ggrepel);library(ggplot2);library(car);library(lsmeans);library(mgcv);library(splines);library(dendextend);library(MASS);library(tidyverse)
datasheet<-read.csv('../Data/Brachy_datasheet.csv', stringsAsFactors=F)
datasheet[datasheet=="#VALUE!"]<-NA
datasheet$Shoot_Root_Ratio<-as.numeric(datasheet$Shoot_Root_Ratio)
datasheet$Day_14<-as.numeric(datasheet$Day_14)
datasheet$Relative_WC<-as.numeric(datasheet$Relative_WC)
datasheet$Shoot_Mass<-as.numeric(datasheet$Shoot_Mass)
datasheet$Root_Mass<-as.numeric(datasheet$Root_Mass)
datasheet$aboveground_greenarea<-as.numeric(datasheet$aboveground_greenarea)
datasheet$leaf_area<-as.numeric(datasheet$leaf_area)
leafarea<-read.csv("../Data/leafarea.csv")
leafarea$id<-as.numeric(gsub(".JPG","", leafarea$filename))
leafarea<-leafarea[leafarea$id %% 2 !=0,]
leafarea$leaf.area.cm.2[leafarea$id==297]<-NA

datasheet$leaf_area_cm<-leafarea$leaf.area.cm.2[match(datasheet$ID, leafarea$id)]
#plot(datasheet$leaf_area, datasheet$leaf_area_cm)
datasheet$SLA<-datasheet$leaf_area_cm*100/(datasheet$lf_dry_mass)
datasheet$biomass<-datasheet$Shoot_Mass+datasheet$Root_Mass
datasheet$Root_mass_ratio<-datasheet$Root_Mass/datasheet$biomass
c_isotope<-read.csv("../Data/Monroe Bd-d13-1,2,3.csv")
datasheet$d13c<-c_isotope$d13C[match(datasheet$ID, c_isotope$Sample.ID)]
datasheet$c_content<-(c_isotope$C.Amount..ug./c_isotope$Amount..mg.)[match(datasheet$ID, c_isotope$Sample.ID)]
datasheet$n_content<-(c_isotope$N.Amount..ug./c_isotope$Amount..mg.)[match(datasheet$ID, c_isotope$Sample.ID)]
datasheet$c_n<-(c_isotope$C.Amount..ug./c_isotope$N.Amount..ug.)[match(datasheet$ID, c_isotope$Sample.ID)]
datasheet$d15n<-(c_isotope$d15N)[match(datasheet$ID, c_isotope$Sample.ID)]
datasheet$water_change_13_to_14<-datasheet$Day_14*datasheet$Max_water-as.numeric(datasheet$Day_13)*datasheet$Max_water
datasheet$water_used<-(datasheet$Trt-datasheet$water_change_13_to_14)/datasheet$Shoot_Mass
datasheet$SLA_2<-datasheet$aboveground_greenarea/datasheet$Shoot_Mass



# plot one trait ----------------------------------------------------------



# data <- datasheet %>% as_tibble() %>% 
#   filter(Sp == 'b_dist') %>% 
#   select(Day_14, Geno, SLA,Harv) %>% 
#   filter(is.na(SLA) == F) %>% 
#   mutate_at('Geno', as.factor) %>% 
#   mutate_at('Harv', as.factor) 
# 
# data = subset(datasheet, Sp == 'b_dist')
# all_models<-lm(SLA~Geno+Day_14+I(Day_14^2)+ns(Day_14, df=2)+Day_14*Geno+I(Day_14^2)*Geno+ns(Day_14, df=2)*Geno+Harv+Day_14,data = data, na.action = 'na.fail')
# #options(na.action = "na.fail") # Required for dredge to run
# model_sel <- dredge(all_models, beta = F, evaluate = T, rank = AICc)
# options(na.action = "na.omit") # set back to default
# head(model_sel)
# model_avg <- model.avg(model_sel, subset = delta <= 2, fit = TRUE)
# 
# Sp = 'b_dist'
# newdata = data.frame(Sp=rep(sp, each=500), Geno=rep(unique(data$Geno), each=100), Harv=rep(paste0('harvest', rep(1:5, each=20)), times=5), Day_14=rep(rep(seq(0.3, 1, length.out = 20), times=5), times=5))
# 
# pred.se <- predict(model_avg, newdata = newdata, full= TRUE)
# 
# 
# newdata %>% mutate(SLA_pred = pred.se) %>% 
#   group_by(Geno, Sp, Day_14) %>% summarise(mean=mean(SLA_pred, na.rm=T)) %>% 
#   ungroup() %>% 
#   ggplot(aes(Day_14*100, mean, group = Geno))+
#   geom_line(aes(color = Geno),size = 2)+
#   #geom_point(size = 4,color = "dodgerblue")+
#   theme_classic()+
#   ylab('SLA_pred')+
#   xlab(expression('Soil moisture (%'[final]*')'))+
#   theme(axis.text=element_text(size=12, face = 'bold'),axis.title=element_text(size=14,face="bold"))

# 
# pred.se <- predict(model_avg, full= TRUE)
# 
# data %>% mutate(pred = pred.se) %>% 
#   ggplot(aes(Day_14*100, pred, group = Geno))+
#   #geom_line(aes(color = type),size = 1)+
#   geom_smooth(aes(color = Geno),alpha = .4,se = F, size = 2)+
#   #geom_point(size = 4,color = "dodgerblue")+
#   theme_classic()+
#   xlab(expression('Soil moisture (%'[final]*')'))+
#   theme(axis.text=element_text(size=12, face = 'bold'),axis.title=element_text(size=14,face="bold"))

# data %>% mutate(pred = pred.se) %>% 
#   group_by(Geno, Day_14) %>% summarise(pred=mean(pred, na.rm=T), SLA = mean(SLA, na.rm = T)) %>% 
#   pivot_longer(c(SLA,pred), names_to = 'type', values_to = 'SLA') %>% 
#   mutate(geno = Geno) %>% 
#   unite("type",c(Geno,type)) %>% 
#   ggplot(aes(Day_14*100, SLA, group = type))+
#   geom_line(aes(color = type),size = 1)+
#   #geom_smooth(aes(color = type),alpha = .4,se = F)+
#   #geom_point(size = 4,color = "dodgerblue")+
#   theme_bw()+
#   facet_grid(cols = vars(geno))+
#   xlab(expression('Soil moisture (%'[final]*')'))+
#   theme(axis.text=element_text(size=12, face = 'bold'),axis.title=element_text(size=14,face="bold"))

# 
# pred<-predict(model_sel) 
# 
# data %>% mutate(pred = pred) %>% 
#   group_by(Geno, Day_14) %>% summarise(pred=mean(pred, na.rm=T), SLA = mean(SLA, na.rm = T)) %>% 
#   pivot_longer(c(SLA,pred), names_to = 'type', values_to = 'SLA') %>% 
#   mutate(geno = Geno) %>% 
#   unite("type",c(Geno,type)) %>% 
#   ggplot(aes(Day_14*100, SLA, group = type))+
#   geom_line(aes(color = type),size = 1)+
#   #geom_smooth(aes(color = type),alpha = .4,se = F)+
#   #geom_point(size = 4,color = "dodgerblue")+
#   theme_bw()+
#   facet_grid(cols = vars(geno))+
#   xlab(expression('Soil moisture (%'[final]*')'))+
#   theme(axis.text=element_text(size=12, face = 'bold'),axis.title=element_text(size=14,face="bold"))



# plot all curve ----------------------------------------------------------
traits<-c("Relative_WC", "SLA", "aboveground_greenarea", "Shoot_Mass", "Root_Mass","Shoot_Root_Ratio","biomass", "c_content","d13c", "n_content", "d15n", "c_n")
sink("lm_averaged_summary.txt")
all_means<-data.frame()

curve_plots<-list()
for(i in 1:length(traits)){
  trait<-traits[i]
  all <- tibble()
  for (sp in c("b_dist","b_sylv")){
    print(trait)
    print(sp)
    
data <- datasheet %>% as_tibble() %>% 
  filter(Sp == sp) %>% 
  dplyr::select(Day_14, Geno, trait,Harv) %>% 
  filter(is.na(get(trait)) == F) %>% 
  mutate_at('Geno', as.factor) %>% 
  mutate_at('Harv', as.factor) 

all_models<-lmer(get(trait)~Geno+Day_14+I(Day_14^2)+ns(Day_14, df=2)+Day_14*Geno+I(Day_14^2)*Geno+ns(Day_14, df=2)*Geno+(1|Harv)+Day_14,data = data, na.action = 'na.fail', REML=F)
#all_models<-lmer(get(trait)~(1|Geno)+Day_14+I(Day_14^2)+ns(Day_14, df=2)+(0+Day_14|Geno)+(0+I(Day_14^2)|Geno)+(0+ns(Day_14, df=2)|Geno)+(1|Harv)+Day_14,data = data, na.action = 'na.fail', REML=F)

options(na.action = "na.fail") # Required for dredge to run
model_sel <- dredge(all_models, beta = F, evaluate = T, rank = AICc)
options(na.action = "na.omit") # set back to default



if (min(model_sel$delta[model_sel$delta!=0])<=2){
model_avg <- model.avg(model_sel, subset = delta <= 2, fit = TRUE)
print(summary(model_avg))
newdata <- data.frame(Sp=rep(sp, each=500), Geno=rep(unique(data$Geno), each=100), Harv=rep(paste0('harvest', rep(1:5, each=20)), times=5), Day_14=rep(rep(seq(0.3, 1, length.out = 20), times=5), times=5))
pred.se <- predict(model_avg, newdata = newdata, full= TRUE)
} else {pred.se <- predict(get.models(model_sel, subset = delta <= 2)$`1`, newdata = newdata, full= TRUE)}


newdata <- newdata %>% as_tibble() %>% mutate(pred = pred.se)

all <- bind_rows(newdata,all)
  }

  
plot <-all %>% 
  group_by(Geno, Sp, Day_14) %>% summarise(mean=mean(pred, na.rm=T)) %>% 
  ungroup() %>% view()

p <- ggplot(plot, aes(x = Day_14*100, y = mean, col=Sp))+
  geom_point(alpha=0.9, fill="gray", size=.5, shape=16)+
  geom_line(aes(group = Geno),lwd=.4, alpha=0.8, size = 1)+
  geom_text_repel(data=filter(plot, Day_14 == max(plot$Day_14)[1]), aes(label=Geno), nudge_x  = 0.1*100,
                  direction    = "y",
                  hjust        = 0,
                  segment.size = 0.2,
                  segment.alpha=0.2,
                  size=1.2,
                  min.segment.length = unit(0, 'lines'))+
  geom_text_repel(data=filter(plot, Day_14 == min(plot$Day_14)[1]), aes(label=Geno), nudge_x  = -0.1*100,
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
  labs(y=trait, x=expression('Soil moisture (%'[final]*')'))

  curve_plots[[i]] <- p
  
  
  all <- all %>% 
    group_by(Geno, Sp, Day_14) %>% summarise(mean = mean(pred, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(trait_name = rep(trait,200))
  all_means<-rbind(all_means, all)
  
  # geom_line(aes(color = Geno),size = 1)+
  # #geom_point(size = 4,color = "dodgerblue")+
  # theme_classic()+
  # ylab('SLA_pred')+
  # xlab(expression('Soil moisture (%'[final]*')'))+
  # theme(axis.text=element_text(size=12, face = 'bold'),axis.title=element_text(size=14,face="bold"))+
  # geom_text_repel(
  #   data = subset(plot, Day_14 == max(Day_14)),
  #   aes(label = Geno),
  #   direction    = "y",
  #   hjust        = 1,
  #   segment.size = 0.2,
  #   segment.alpha=0.2,
  #   size=2,
  #   min.segment.length = unit(0, 'lines')
  # ) +
  # scale_color_manual(values=c("dodgerblue","orange2"), labels=c("B. distachyon","B. sylvaticum"))


}


sink()
pdf("../Figures/curves_byspecies_new.pdf", width=3.7, height=6.4)
plots<-plot_grid(plotlist = curve_plots, ncol = 2,labels="auto", label_size = 8, vjust=c(1,1,rep(0.5, length(curve_plots)-2)), hjust = c(0,0.5))
#allplots<-plot_grid(plots, pca_plots, ncol=2, rel_widths = c(2, 1))
print(plots)
dev.off()


# # Get all means -----------------------------------------------------------
# 
# traits<-c("Relative_WC", "SLA", "aboveground_greenarea", "Shoot_Mass", "Root_Mass","Shoot_Root_Ratio","biomass", "c_content","d13c", "n_content", "d15n", "c_n")
# all_means<-data.frame()
# 
# for(i in 1:length(traits)){
#   trait<-traits[i]
#   all <- tibble()
#   for (sp in c("b_dist","b_sylv")){
# 
#     
#     data <- datasheet %>% as_tibble() %>% 
#       filter(Sp == sp) %>% 
#       dplyr::select(Day_14, Geno, trait,Harv) %>% 
#       filter(is.na(get(trait)) == F) %>% 
#       mutate_at('Geno', as.factor) %>% 
#       mutate_at('Harv', as.factor) 
#     
#     all_models<-lm(get(trait)~Geno+Day_14+I(Day_14^2)+ns(Day_14, df=2)+Day_14*Geno+I(Day_14^2)*Geno+ns(Day_14, df=2)*Geno+Harv+Day_14,data = data, na.action = 'na.fail')
#     
#     options(na.action = "na.fail") # Required for dredge to run
#     model_sel <- dredge(all_models, beta = F, evaluate = T, rank = AICc)
#     options(na.action = "na.omit") # set back to default
#     
#     model_avg <- model.avg(model_sel, subset = delta <= 2, fit = TRUE)
#     print(summary(model_avg))
#     newdata <- data.frame(Sp=rep(sp, each=500), Geno=rep(unique(data$Geno), each=100), Harv=rep(paste0('harvest', rep(1:5, each=20)), times=5), Day_14=rep(rep(seq(0.3, 1, length.out = 20), times=5), times=5))
#     pred.se <- predict(model_avg, newdata = newdata, full= TRUE)
#     newdata <- newdata %>% as_tibble() %>% mutate(pred = pred.se)
#     
#     all <- bind_rows(newdata,all)
#     }
#     
#  
#   all <- all %>% 
#     group_by(Geno, Sp, Day_14) %>% summarise(mean = mean(pred, na.rm=T)) %>% 
#     ungroup() %>% 
#     mutate(trait_name = rep(trait,200))
#   all_means<-rbind(all_means, all)
#   
#   
#   
# }


all_means<-all_means %>% pivot_wider(-c(mean,trait_name), names_from = 'trait_name', values_from = 'mean')

allmeans<-all_means

all_means$Day_14<-as.numeric(as.factor(all_means$Day_14))
all_means<-data.table(all_means)

all_data_new<-lapply(1:20, function(x){
  sub<-all_means[Day_14==x][, Day_14:=NULL]
  
  colnames(sub)[-1:-2]<-paste0(colnames(sub)[-1:-2],"_",x)
  return(sub)
  
})

all_data_new<-Reduce(merge,all_data_new)                                                                       
all_data<-all_data_new
all_data<-all_data[order(Sp)]
alldata<-all_data
alldata<-data.frame(alldata)

all_means %>% pivot_wider(-c(mean,trait_name), names_from = 'trait_name', values_from = 'mean') %>% 
  save(file = '../Data/all_means_averaging_model.rdata')

