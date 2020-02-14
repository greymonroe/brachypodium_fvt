#prepare data

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


