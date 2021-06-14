# Set directories----
rm(list=ls())

 
# Libraries required
install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)

library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(googlesheets4)

library(ggmap)
library(rgdal)
library(raster)
library(png)

library("ggspatial")
library("rnaturalearth")
library("rnaturalearthdata")
library(cowplot)
library(scatterpie)
library(colorspace)

# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
p.dir <- paste(w.dir, "plots", sep = '/')
dt.dir <- paste(w.dir, "tidy data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')
# Study name ----
study<-"2021-04_drop-camera_Habitat_FRDC" 



# functions for summarising data on plots----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

theme.larger.text<-theme(
  strip.text.x = element_text(size = 10,angle = 90),
  strip.text.y = element_text(size = 10),
  axis.title.x=element_text(vjust=-0.0, size=12),
  axis.title.y=element_text(vjust=0.0,size=12),
  axis.text.x=element_text(size = 10,angle = 90),
  axis.text.y=element_text(size=10),
  legend.title = element_text(family="TN",size=12),
  legend.text = element_text(family="TN",size=12))

theme.species<-theme(
  strip.text.x = element_text(size = 8,angle = 0),
  strip.text.y = element_text(size = 8),
  axis.title.x=element_text(vjust=-0.0, size=12),
  axis.title.y=element_text(vjust=0.0,size=12),
  axis.text.x=element_text(size=8),
  axis.text.y=element_text(size=8),
  legend.title = element_text(family="TN",size=11),
  legend.text = element_text(family="TN",size=11))


theme_collapse<-theme(      ## the commented values are from theme_grey
  panel.grid.major=element_line(colour = "white"), ## element_line(colour = "white")
  panel.grid.minor=element_line(colour = "white", size = 0.25),
  strip.text.x = element_text(size = 4,angle = 0),
  strip.text.y = element_text(size = 4),
  axis.title.x=element_text(vjust=-0.0, size=10),
  axis.title.y=element_text(vjust=0.0,size=10),
  axis.text.x=element_text(size=6),
  axis.text.y=element_text(size=6),
  legend.title = element_text(family="TN",size=6),
  legend.text = element_text(family="TN",size=4))



# Read in shapefile ----
setwd(s.dir)
dir()

shapefile <- readOGR(paste(s.dir, "WA_wgs84.shp", sep='/'))
shapefile_df <- fortify(shapefile)
plot(shapefile)
e <- drawExtent()
sh <- crop(shapefile, e)
plot(sh)
sh_df <- fortify(sh)


# read in maxn
setwd(dt.dir)
dir()

broad.hab <- read.csv("2021-04_FRDC_BOSS_Habitat._broad.habitat.csv") %>% #glimpse()
  rename(Macroalgae = broad.Macroalgae) %>%
  rename(Consolidated = broad.Consolidated) %>%
  rename(Seagrasses = broad.Seagrasses) %>%
  rename(Sponges = broad.Sponges) %>%
  rename(Corals = broad.Stony.corals) %>%
  rename(Sand = broad.Unconsolidated) %>%
  rename(Unknown = broad.Unknown) %>%
  mutate_at(c('sample', 'site', 'location', 'visibility', 'location'), as.factor) %>%
  mutate(date = as.Date(date, format = '%d/%m/%y')) %>%
  mutate(site.number = gsub('.{2}$', '', sample)) %>%
  glimpse()
  #mutate(sample=str_pad(sample,2,side="left",pad="0"))

#metadata <- read.csv("2020-01_Guardian-Ningaloo_stereoBRUVs.checked.metadata.csv")%>%
#  mutate(sample=str_pad(sample,2,side="left",pad="0"))

#habitat<-read_csv("2020-01_Guardian-Ningaloo_stereoBRUVs._habitat.csv" )%>%
 # ga.clean.names()


#### PIE PLOTS ----

# get colors ----
pal1 <- choose_palette()
pal11 <- pal1(9) #raster colors

pal2 <- choose_palette()
pal21 <- pal2(7)

pal3 <- c("#2A5676", "#336B88" ,"#3B809A" ,"#5095A8", "#67A9B6", "#7FBCC3", "#99CFD1", "#B3E1DE", "#D2EEEA",
          "#000000", "#006006", "#FFFF00", "#00F00F" ,"#FF6FF6" ,"#999999")


#reorder columns for plotting ----

names(broad.hab)

broad.hab1 <- broad.hab[,c(1:13,16,15,14,19,17,18,20,21)]


# General plot ----

ggplot() +
  geom_polygon(data = sh_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2) +
  #coord_cartesian(xlim = c(-32.29250317636435,-32.76632974729254), ylim =  c(115.42421499835982, 115.79944256928025)) +
  geom_scatterpie(aes(x=longitude, y=latitude, group = site), data = broad.hab, #%>% filter(site == 'Mandurah'), 
                  cols = c("Consolidated", "Macroalgae", "Seagrasses", "Sponges", "Corals", "Unknown"), color="black", alpha=.8,legend_name="Habitat")


# site specific plot ----

colors <- c( '#4575b4','#fee090', '#fdae61', '#f46d43','#DCDCDC', '#d73027', '#e0f3f8' )  


# Mandurah ----

# read raster ----
dir(r.dir)

r <- raster(paste(r.dir, "Sen2_c5_unsup.tif", sep='/'))
plot(r)
rdf <- raster::as.data.frame(r, xy=T)
head(rdf)

m <- ggplot() +
  geom_raster(data = rdf, aes(x=x, y=y, fill = factor(Sen2_c5_unsup)), alpha = 0.5) +
  #scale_fill_manual(values = pal11) +
  geom_polygon(data = sh_df, aes(x = long, y = lat, group = group), color = 'black', fill = 'grey90', size = .2) +
  coord_cartesian(xlim = c(115.53,  115.65), ylim =  c(-32.81, -32.65)) +
  geom_scatterpie(aes(x=longitude, y=latitude, group = site, r = 0.0035), data = broad.hab1 %>% filter(site == 'Mandurah'), 
                  cols = c("Seagrasses", "Macroalgae", "Sand", "Consolidated",  "Sponges", "Corals", "Unknown"), color="black", alpha=.8, legend_name="Habitat")+
  labs(title = "Mandurah", fill = "Habitat type") +
  scale_fill_manual(values = pal3) +
  xlab('Longitude') +
  ylab('Latitude') +
  #labs(size = "Percent cover")+
  theme_bw() +
  theme_collapse +
  theme.larger.text 

m

# Garden Island ----

gi <- ggplot() +
  geom_polygon(data = sh_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2) +
  coord_cartesian(xlim = c(115.58,   115.69994695733632), ylim =  c(-32.25862151126115, -32.130875860857905)) + 
  geom_scatterpie(aes(x=longitude, y=latitude, group = site, r = 0.0035), data = broad.hab %>% filter(site == 'Garden Island'), 
                  cols = c("Consolidated", "Macroalgae", "Seagrasses", "Sponges", "Corals", "Unknown"), color="black", alpha=.8, legend_name="Habitat")+
  labs(title = "Green Island", fill = "Habitat type") +
  scale_fill_manual(values = colors) +
  xlab('Longitude') +
  ylab('Latitude') +
  #labs(size = "Percent cover")+
  theme_bw() +
  theme_collapse +
  theme.larger.text 

gi


# Two Rocks ----

tr <- ggplot() +
  geom_polygon(data = sh_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2) +
  coord_cartesian(xlim = c(115.49, 115.65), ylim =  c(-31.6, -31.44)) + 
  geom_scatterpie(aes(x=longitude, y=latitude, group = site, r = 0.0035), data = broad.hab %>% filter(site == 'Two Rocks'), 
                  cols = c("Consolidated", "Macroalgae", "Seagrasses", "Sponges", "Corals", "Unknown"), color="black", alpha=.8, legend_name="Habitat")+
  labs(title = "Two Rocks", fill = "Habitat type") +
  scale_fill_manual(values = colors) +
  xlab('Longitude') +
  ylab('Latitude') +
  #labs(size = "Percent cover")+
  theme_bw() +
  theme_collapse +
  theme.larger.text 

tr


# Lancelin ----

ln <- ggplot() +
  geom_polygon(data = sh_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2) +
  coord_cartesian(xlim = c(115.49, 115.65), ylim =  c(-31.6, -31.44)) + 
  geom_scatterpie(aes(x=longitude, y=latitude, group = site, r = 0.0035), data = broad.hab %>% filter(site == 'Lancelin'), 
                  cols = c("Consolidated", "Macroalgae", "Seagrasses", "Sponges", "Corals", "Unknown"), color="black", alpha=.8, legend_name="Habitat")+
  labs(title = "Lancelin", fill = "Habitat type") +
  scale_fill_manual(values = colors) +
  xlab('Longitude') +
  ylab('Latitude') +
  #labs(size = "Percent cover")+
  theme_bw() +
  theme_collapse +
  theme.larger.text 

ln



# descriptive stats
raw.broad.h <- read.csv("2021-04_FRDC_BOSS_Habitat._broad.habitat.csv")

# total.number <-sum(raw.maxn$maxn) #9641 fish
# number.of.families <- length(unique(raw.maxn$family)) #53
# number.of.genus <- length(unique(raw.maxn$genus)) # 96
# number.of.species <- length(unique(raw.maxn$scientific)) #119


# Practice plots
#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

summary(broad.hab)
#------------------------Habitat Variables----------------------------------

#------Spatial Reef % -------
spatial.reef<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = site),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(broad.hab, Macroalgae==0), aes(longitude,latitude=Macroalgae),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(broad.hab, Macroalgae>0),aes(longitude,latitude=Macroalgae),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  #labs(size = "% cover\Macroalgae")+
  
  #annotate("text",x=193000, y=7606500,label="Total abundance",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  #coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))

spatial.reef

#-----Spatial Seagrasses  %---------
spatial.seagrasses<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(Macroalgae, seagrasses==0), aes(longitude,latitude,size=seagrasses),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(Macroalgae, seagrasses>0),aes(longitude,latitude,size=seagrasses),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(size = "% cover\nseagrasses")+

  #annotate("text",x=193000, y=7606500,label="Total abundance",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))

spatial.seagrasses

#------Spatial Sand %---------
spatial.unconsolidated<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(maxn, unconsolidated==0), aes(longitude,latitude,size=unconsolidated),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(maxn, unconsolidated>0),aes(longitude,latitude,size=unconsolidated),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(size = "% cover\nsand")+
  
  #annotate("text",x=193000, y=7606500,label="Total abundance",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))

spatial.unconsolidated

# Species specific ----
habitat.combined<-plot_grid(spatial.reef, spatial.seagrasses, spatial.unconsolidated, 
                            labels = c('a)', 'b)','c)'), label_size = 10,ncol=1)
ggsave("spatial.habitat.png",habitat.combined,dpi=500, width = 21, height = 23,units = "cm")


#----------Response Variables-------------

# SPECIES RICHNESS ----
spatial.sr<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(maxn, species.richness==0), aes(longitude,latitude,size=species.richness),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(maxn, species.richness>0),aes(longitude,latitude,size=species.richness),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  
  labs(size = "Species\nrichness")+
  
  #annotate("text",x=193000, y=7606500,label="Total abundance",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  #annotate("text", x = -Inf, y=-Inf, label = "Species richness",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))

spatial.sr

# TOTAL ANBUNDANCE ----
spatial.ta<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(maxn, total.abundance==0), aes(longitude,latitude,size=total.abundance),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(maxn, total.abundance>0),aes(longitude,latitude,size=total.abundance),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  
  labs(size = "Total\nabundance")+
  
  #annotate("text", x = -Inf, y=Inf, label = "Total abundance",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  #annotate("text",x=193000, y=7606500,label="Total abundance",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))

spatial.ta

#------Chrysophrys auratus --------------

spatial.auratus<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(maxn, Sparidae.Chrysophrys.auratus==0), aes(longitude,latitude,size=Sparidae.Chrysophrys.auratus),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(maxn, Sparidae.Chrysophrys.auratus>0),aes(longitude,latitude,size=Sparidae.Chrysophrys.auratus),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  
  labs(size = "Relative\nabundance")+
  #annotate("text", x = -Inf, y=Inf, label = "Chrysophrys auratus",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))

spatial.auratus

#------Coris auricularis --------------

spatial.auricularis<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(maxn, Labridae.Coris.auricularis==0), aes(longitude,latitude,size=Labridae.Coris.auricularis),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(maxn, Labridae.Coris.auricularis>0),aes(longitude,latitude,size=Labridae.Coris.auricularis),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  
  labs(size = "Relative\nabundance")+
 
  #annotate("text",x=193000, y=7606500,label="Total abundance",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  #annotate("text", x = -Inf, y=Inf, label = "Coris auricularis",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))

spatial.auricularis

#------Parequula melbournensis --------------

spatial.melbournensis<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(maxn, Gerreidae.Parequula.melbournensis==0), aes(longitude,latitude,size=Gerreidae.Parequula.melbournensis),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(maxn, Gerreidae.Parequula.melbournensis>0),aes(longitude,latitude,size=Gerreidae.Parequula.melbournensis),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  
  labs(size = "Relative\nabundance")+
  
  #annotate("text",x=193000, y=7606500,label="Total abundance",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  #annotate("text", x = -Inf, y=Inf, label = "Parequula melbournensis",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))#+
  #coord_cartesian(xlim = c(115.0,115.7), ylim = c(-33.7,-33.3),ratio=1)

spatial.melbournensis

#------Carangidae.Pseudocaranx.spp --------------

spatial.Pseudocaranx<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(maxn, Carangidae.Pseudocaranx.spp==0), aes(longitude,latitude,size=Carangidae.Pseudocaranx.spp),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(maxn, Carangidae.Pseudocaranx.spp>0),aes(longitude,latitude,size=Carangidae.Pseudocaranx.spp),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  
  labs(size = "Relative\nabundance")+
  
  #annotate("text",x=193000, y=7606500,label="Total abundance",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  #annotate("text", x = -Inf, y=Inf, label = "Pseudocaranx spp",vjust = 1, hjust = -.1,size=5,fontface="italic")+
  #(annotation_raster)(spp,xmin=0.275, xmax=0.38, ymin=0.627, ymax=0.675)+
  theme_bw()+
  theme_collapse+
  theme.larger.text+
  coord_fixed(ratio=1,xlim = c(115.0,115.7), ylim = c(-33.7,-33.3))


spatial.Pseudocaranx

# SAVE - Species richness and total abundance combined ----
setwd(plots.dir)
ta.sr<-plot_grid(spatial.ta, spatial.sr, labels = c('a)', 'b)'), label_size = 12,ncol=1)
ggsave("total.abundance.and.species.richness.potrait.png",ta.sr,dpi=300,width=11,height=17.5,unit="cm")
# 
# ta.sr<-plot_grid(spatial.ta, spatial.sr, labels = c('A', 'B'), label_size = 12,ncol=2)
# ggsave("total.abundance.and.species.richness.landscape1.png",ta.sr,dpi=300,width = 20, height = 5.5,unit="cm") 


# Species specific ----
species.combined<-plot_grid(spatial.sr, spatial.ta, 
                            spatial.auratus, spatial.auricularis, 
                            spatial.melbournensis,spatial.Pseudocaranx, 
                            labels = c('a)', 'b)','c)','d)','e)','f)'), label_size = 10,ncol=2)
ggsave("spatial.species.png",species.combined,dpi=500, width = 21, height = 23,units = "cm")

#-----combine plots

# Habitat bubble plots  ----
glimpse(habitat)

hab<-broad.hab%>%
  dplyr::select(sample,Macroalgae,Sand, latitude, longitude)%>%
  #dplyr::rename(bioturbated=bedform.bioturbated,none=bedform.none)%>%
  #left_join(metadata)%>%
  glimpse()

bedforms <- ggplot() +
  #geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .1) +
  geom_scatterpie(data = broad.hab, aes(x=longitude, y=latitude, r=150),
  #data=broad.hab, 
  cols=c("Consolidated", "Macroalgae", "Seagrasses", "Sponges", "Corals", "Unknown"), color="black", alpha=.8,legend_name="Habitat") +
  coord.equal() +
  xlab('Longitude') +
  ylab('Latitude') +
  labs(size = "Habitat type") +
  theme_bw() +
  theme_collapse +
  theme.larger.text 
bedforms

hab.sp<-habitat%>%
  left_join(metadata)

spatial.sand<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=filter(hab.sp,broad.unconsolidated==0),aes(longitude.zone50,latitude.zone50,size=broad.unconsolidated),shape=21,colour="dodgerblue4",fill="white",alpha=0.75)+
  geom_point(data=filter(hab.sp,broad.unconsolidated>0),aes(longitude.zone50,latitude.zone50,size=broad.unconsolidated),shape=21,colour="dodgerblue4",fill="dodgerblue2",alpha=0.75)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(size = "Percent cover")+
  annotate("text",x=193000, y=7606500,label="Unconsolidated (fine)",color="Black",hjust=0,family="TN",cex=3.5,fontface="italic")+
  theme_bw()+
  theme_collapse+
  theme.larger.text

spatial.sand

hab.combined<-plot_grid(spatial.sand, bedforms,
                             labels = c('A','B'), label_size = 12,ncol=2)


hab.combined2<-plot_grid(spatial.sand, bedforms,
                        labels = c('A','B'), label_size = 12,ncol=1)

setwd(plots.dir)
ggsave("hab.combined.png",hab.combined,dpi=300,width=15,height=5.75)
ggsave("hab.combined.potrait.png",hab.combined2,dpi=300,width=12,height=17.5,unit="cm")



# Basic Map 
spatial.deployments<-ggplot() +
  geom_polygon(data = shapefile_df, aes(x = long, y = lat, group = group),color = 'black', fill = 'grey90', size = .2)+
  geom_point(data=metadata,aes(longitude.zone50,latitude.zone50),shape=21,colour="black",fill="white",size=4)+
  xlab('Longitude')+
  ylab('Latitude')+
  theme_bw()+
  theme_collapse+
  theme.species

spatial.deployments
setwd(plots.dir)
ggsave("deployment.map.png",spatial.deployments,dpi=300)




