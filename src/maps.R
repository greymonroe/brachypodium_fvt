library(png)
library(jpeg)
library(grid)

pdf("../Figures/maps.pdf", width=6, heigh=4)

map_bdist<-rasterGrob(readPNG("../Figures/map_bdist.png"), interpolate=TRUE)
bdist<-rasterGrob(readJPEG("../Figures/bdist.jpg"), interpolate=TRUE)
map_bsylv<-rasterGrob(readPNG("../Figures/mab_bsylv.png"), interpolate=TRUE)
bsylv<-rasterGrob(readJPEG("../Figures/bsylv.jpeg"), interpolate=TRUE)

maps<-plot_grid(map_bdist, map_bsylv, labels = c("a","b"), ncol = 1)
pics<-plot_grid(bdist, bsylv, labels = c("c","d"), ncol = 1, hjust = -1)

plot<-plot_grid(maps, pics, ncol=2, rel_widths = c(1, .25), label_size = 8)
print(plot)

dev.off()
