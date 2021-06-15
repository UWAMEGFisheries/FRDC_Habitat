# Set directories----
rm(list=ls())

 
# Libraries required
#install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
#library(GlobalArchive)

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
#e <- drawExtent()
e <- extent(112.7169 , 116.0445 , -33.37469 , -27.00236)
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
#pal1 <- choose_palette()
#pal11 <- pal1(9) #raster colors

#pal2 <- choose_palette()
#pal21 <- pal2(7)

pal3 <- c("#2A5676", "#336B88" ,"#3B809A" ,"#5095A8", "#67A9B6", "#7FBCC3", "#99CFD1", "#B3E1DE", "#D2EEEA",
          "#000000", "#006006", "#FFFF00", "#00F00F" ,"#FF6FF6" ,"#999999")


#reorder columns for plotting ----

names(broad.hab)

broad.hab1 <- broad.hab[,c(1:13,16,15,14,19,17,18,20,21)]





# Mandurah ----

# read raster ----
dir(r.dir)

r <- raster(paste(r.dir, "Sen2_c5_unsup.tif", sep='/'))
r
ext <- r@extent
ext@
plot(r)
rdf <- raster::as.data.frame(r, xy=T)
head(rdf)

m <- ggplot() +
  geom_raster(data = rdf, aes(x=x, y=y, fill = factor(Sen2_c5_unsup)), alpha = 0.5) +
  #scale_fill_manual(values = pal11) +
  geom_polygon(data = sh_df, aes(x = long, y = lat, group = group), color = 'black', fill = 'grey90', size = .2) +
  coord_cartesian(xlim = c(115.5495,  115.6273), ylim =  c(-32.79102, -32.66238 )) +
  geom_scatterpie(aes(x=longitude, y=latitude, group = site, r = 0.0015), data = broad.hab1 %>% filter(site == 'Mandurah'), 
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







