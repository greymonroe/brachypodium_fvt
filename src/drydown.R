#drydown

drydown<-datasheet %>% dplyr::select(Trt, ID, contains("Day")) %>% group_by(Trt) %>% gather(key="Day",value="soilmoisture",-Trt,-ID)
drydown<-as.data.frame(drydown)
drydown$Day<-as.numeric(gsub("Day_", "", drydown$Day ))
drydown$soilmoisture<-as.numeric(drydown$soilmoisture)
drydown$order<-drydown$ID[sample(1:nrow(drydown))]
drydown$ID<-factor(drydown$ID, levels=unique(drydown$ID)[sample(1:length(unique(drydown$ID)))])

colorscale <- c(colorRampPalette(c("gray", "black"))((6)))

lines<-ggplot(drydown, aes(x=Day, y=soilmoisture*100, group=ID, col=factor(Trt)))+
  geom_line(alpha=0.3, size=.5)+
  ylim(c(18,110))+
  scale_color_manual(values = colorscale)+
  theme_classic(base_size = 6)+
  labs(y="Soil Moisture (%)")+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=c(5, 10, 14), labels = c("5","10","final"))

hist<-ggplot(datasheet, aes(x=Day_14*100, group=factor(Trt), fill=factor(Trt)))+
  geom_histogram(alpha=1, bins=50)+
  xlim(c(18,110))+
  scale_fill_manual(values = colorscale)+
  theme_classic(base_size = 6)+
  labs(x="")+
  coord_flip()+
  theme(legend.position = "none")+
  annotate(geom="text", y=20, x=110, label=expression('Soil moisture (%'[final]*')'), size=2)

genos<-ggplot(datasheet, aes(x=Geno, y=Day_14*100))+
  geom_point(aes(fill=factor(Trt)), position = position_jitter(w = 0.3, h = 0), alpha=0.6, size=1, shape=21, stroke=0)+
  scale_fill_manual(values = colorscale)+
  geom_boxplot(alpha=0, fill=NA, aes(col=Sp))+
  scale_color_manual("Species", values = c("dodgerblue","orange2"), labels=c("B. distachyon", "B. sylvaticum"))+
  theme_classic(base_size = 6)+
  ylim(c(18,110))+
  labs(x="Genotype", y="", fill = "Treatment \n (ml water/day)")+
  theme(axis.text.x = element_text(angle=0, colour = c("dodgerblue","orange2")[c(1,1,2,2,1,1,1,2,2,2)]), legend.key.size = unit(.1, 'points'))+
  annotate(geom="text", x=6, y=110, label=expression('Soil moisture (%'[final]*')'), size=2)

pdf("../Figures/DryDown_lines.pdf", height=2, width=7)

plots<-plot_grid(lines, hist, genos, ncol=3,labels=c("a","b","c"), rel_widths = c(.6,.6,1.5), label_size = 8)
#plots<-plot_grid(lines, genos, ncol=2,labels=c("a","b"), rel_widths = c(.6,1.5))

print(plots)

dev.off()



dry<-ggplot(datasheet, aes(x=Dry_wt))+
 geom_histogram(fill="gray70", bins=30)+
  theme_classic(base_size = 6)+
  labs(x="Pot dry (g)")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

wet<-ggplot(datasheet, aes(x=Field_cap_111215))+
  geom_histogram(fill="gray70", bins=30)+
  theme_classic(base_size = 6)+
  labs(x="Pot saturated (g)")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

water<-ggplot(datasheet, aes(x=Max_water))+
  geom_histogram(fill="gray70", bins=30)+
  theme_classic(base_size = 6)+
  labs(x="Pot max water (g)")+
  theme(axis.text.x = element_text(angle=45, hjust=1))
  

pdf("../Figures/pots.pdf", height=1.2, width=3)

plots<-plot_grid(dry, wet, water, ncol=3,labels=c("a","b","c"), label_size = 8)
#plots<-plot_grid(lines, genos, ncol=2,labels=c("a","b"), rel_widths = c(.6,1.5))

print(plots)

dev.off()

tailrwc<-mean(datasheet$Relative_WC[datasheet$Day_14<sort(datasheet$Day_14)[60]], na.rm=T)
dev.off()
