#constraints_byspecies

traitlist<-traits[-which(traits=="Relative_WC")]
traitnames<-real_trait_names[-which(real_trait_names=="RWC")]

subdata<-alldata[,-grep("Relative_WC", colnames(alldata))]

emaxs.bdist<-c()
vts.bdist<-c()
nds.bdist<-c()
for (i in 1:20){
  subdata.bdist<-subdata[1:5,stringr::str_sub(colnames(subdata), start=-(1+nchar(i)))==paste0("_", i)]
  subdata.bdist.pc<-prcomp(cov(sweep(subdata.bdist,2,apply(subdata.bdist,2,mean),'/')))
  emaxs.bdist<-c(emaxs.bdist, sqrt(subdata.bdist.pc$sdev[1]^2))
  vts.bdist<-c(vts.bdist,sum(subdata.bdist.pc$sdev^2))
  nds.bdist<-c(nds.bdist, sum(subdata.bdist.pc$sdev^2)/subdata.bdist.pc$sdev[1]^2)
}

emaxs.bsylv<-c()
vts.bsylv<-c()
nds.bsylv<-c()
for (i in 1:20){
  subdata.bsylv<-subdata[6:10,stringr::str_sub(colnames(subdata), start=-(1+nchar(i)))==paste0("_", i)]
  subdata.bsylv.pc<-prcomp(cov(sweep(subdata.bsylv,2,apply(subdata.bsylv,2,mean),'/')))
  emaxs.bsylv<-c(emaxs.bsylv, sqrt(subdata.bsylv.pc$sdev[1]^2))
  vts.bsylv<-c(vts.bsylv,sum(subdata.bsylv.pc$sdev^2))
  nds.bsylv<-c(nds.bsylv, sum(subdata.bsylv.pc$sdev^2)/subdata.bsylv.pc$sdev[1]^2)
  
}

constraints.df<-data.frame(day_14=rep(seq(0.3, 1, length.out = 20), times=2), 
                           sp=rep(c("b_dist","b_sylv"), each=20), 
                           emax=c(emaxs.bdist,emaxs.bsylv), 
                           vt=c(vts.bdist,vts.bsylv),
                           nd=c(nds.bdist,nds.bsylv))

pdf("../Figures/constraints_byspecies.pdf", height=4.5, width=3)


nd.plot<-ggplot(constraints.df %>% filter(sp=="b_dist"), aes(x=day_14, y=nd, group=sp))+
  geom_point( size=.9, col="dodgerblue")+
  geom_line(alpha=0.8, size=.75, col="dodgerblue")+
  theme_classic(base_size = 6)+
  theme(legend.key.size = unit(.1, 'points'), legend.position = "none")+
  labs(x=expression("Soil Moisture (%"[final]*")"), y=expression('n'[D]))

nd.plot2<-ggplot(constraints.df %>% filter(sp=="b_sylv"), aes(x=day_14, y=nd, group=sp))+
  geom_point( size=.9, col="orange2")+
  geom_line(alpha=0.8, size=.75, col="orange2")+
  theme_classic(base_size = 6)+
  theme(legend.key.size = unit(.1, 'points'), legend.position = "none")+
  labs(x=expression("Soil Moisture (%"[final]*")"), y=expression('n'[D]))

emax.plot<-ggplot(constraints.df %>% filter(sp=="b_dist"), aes(x=day_14, y=emax, group=sp))+
  geom_point( size=.9, col="dodgerblue")+
  geom_line(alpha=0.8, size=.75, col="dodgerblue")+
  theme_classic(base_size = 6)+
  theme(legend.position = "none")+
  labs(x=expression("Soil Moisture (%"[final]*")"), y=expression('e'[max]))

emax.plot2<-ggplot(constraints.df %>% filter(sp=="b_sylv"), aes(x=day_14, y=emax, group=sp))+
  geom_point( size=.9, col="orange2")+
  geom_line(alpha=0.8, size=.75, col="orange2")+
  theme_classic(base_size = 6)+
  theme(legend.position = "none")+
  labs(x=expression("Soil Moisture (%"[final]*")"), y=expression('e'[max]))

vt.plot<-ggplot(constraints.df %>% filter(sp=="b_dist"), aes(x=day_14, y=vt, group=sp))+
  geom_point( size=.9, col="dodgerblue")+
  geom_line(alpha=0.8, size=.75, col="dodgerblue")+
  theme_classic(base_size = 6)+
  theme(legend.key.size = unit(.1, 'points'), legend.position = "none")+
  labs(x=expression("Soil Moisture (%"[final]*")"), y=expression('v'[T]))

vt.plot2<-ggplot(constraints.df %>% filter(sp=="b_sylv"), aes(x=day_14, y=vt, group=sp))+
  geom_point( size=.9, col="orange2")+
  geom_line(alpha=0.8, size=.75, col="orange2")+
  theme_classic(base_size = 6)+
  theme(legend.key.size = unit(.1, 'points'), legend.position = "none")+
  labs(x=expression("Soil Moisture (%"[final]*")"), y=expression('v'[T]))

plots1<-plot_grid(nd.plot, nd.plot2, emax.plot, emax.plot2, vt.plot, vt.plot2, ncol=2, labels="auto",label_size = 8)
print(plots1)

dev.off()


