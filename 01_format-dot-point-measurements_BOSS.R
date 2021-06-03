##Script for cleaning BOSS habitat data prior to analyses
install.packages("here")
install.packages("rprojroot")
# Clear memory ----
rm(list=ls())

# Libraries required ----
library(here)
library(rprojroot)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(ggplot2)

# Set work directories----
working.dir<-setwd("//uniwa.uwa.edu.au/userhome/staff1/00104541/Desktop/GitHub/FRDC_Habitat")
raw.dir<-paste(working.dir,"raw data",sep="/") # links to folder called 'example raw data'
tidy.dir<-paste(working.dir,"tidy data",sep="/") # links to folder called 'example tidy data'


# Functions----
se <- function(x) sd(x)/sqrt(length(x))




# p.est <- mean(binary)
# variance <- (p.est*(1-p.est))/nrow(binary)
# std.dev <- sqrt(variance)



# Study name----

study <- "2021-04_FRDC_BOSS_Habitat"



# Read in metadata----
setwd(raw.dir)
metadata <- read_csv("Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, time.bottom, site, location, visibility, image) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview


# Load and format annotation data----
setwd(raw.dir)
dir()

habitat <- read.delim(paste(study,"Dot Point Measurements.txt",sep = "_"),header=T,skip=4,stringsAsFactors=FALSE)%>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"="","N"="","E"="","S"="","W"=""))) %>%# remove N,E,S,W from sample
  mutate(filename=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>% #keep filename but remove .jpg (need this for later to ensure unique ID)
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  mutate(filename=as.character(filename)) %>%
  select(filename,sample,image.row,image.col,broad,morphology,type,fieldofview,relief) %>% # select only these columns to keep
  glimpse() # preview



# Check number of points per image ----
number.of.annotations<-habitat%>%
  dplyr::group_by(filename)%>%
  dplyr::summarise(number.of.annotations=n()) %>% # count the number of annotations per image
glimpse()
wrong.number<-number.of.annotations%>%
  filter(!number.of.annotations==80) %>% 
  glimpse() # see images where there is too many or too little annotations (in this example there are none), go back into the *.TMObs file to fix this before re-exporting DO NOT FIX IN THE TXT FILE

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("sample")) # samples in the metadata that don't have a match in habitat


# Create %fov----
fov<-habitat%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col, filename))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(total.sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  mutate_at(vars(starts_with("fov")),funs(./total.sum*100))%>%
  dplyr::select(-c(total.sum))%>%
  dplyr::ungroup()%>%
  glimpse()

# Create relief----
relief<-habitat%>%
  dplyr::filter(!broad%in%c("Open Water","Unknown"))%>%
  dplyr::filter(!relief%in%c(""))%>%
  dplyr::select(-c(broad,morphology,type,fieldofview,image.row,image.col))%>%
  dplyr::mutate(relief.rank=ifelse(relief=="0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0,
                                   ifelse(relief=="1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,
                                          ifelse(relief=="2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,
                                                 ifelse(relief=="3. Good relief structure with some overhangs. >45 substrate slope.",3,
                                                        ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,
                                                               ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief)))))))%>%
  dplyr::select(-c(relief))%>%
  dplyr::mutate(relief.rank=as.numeric(relief.rank))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  dplyr::ungroup()%>%
  glimpse()


# CREATE catami point score------

broad<-habitat%>%
  dplyr::select(-c(morphology,type))%>%
  # filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  filter(!broad%in%c("",NA,"Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col,filename,fieldofview,relief))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  dplyr::mutate_each(funs(./Total.Sum*100), matches("broad"))%>%  
  dplyr::select(-Total.Sum)%>%
  dplyr::ungroup()%>%
  glimpse

# CREATE catami_morphology------
detailed<-habitat%>%
  dplyr::select(-c(fieldofview,relief))%>%
  dplyr::filter(!morphology%in%c("",NA,"Unknown"))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(morphology=paste("detailed",broad,morphology,type,sep = "."))%>%
  dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="")))%>%
  dplyr::select(-c(broad))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=morphology,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_if(is.numeric,sum,na.rm=TRUE)%>% 
  dplyr::mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  dplyr::group_by(sample)%>%
  dplyr::mutate_each(funs(./Total.Sum*100), matches("detailed"))%>%
  dplyr::select(-Total.Sum)%>%
  glimpse()


# Write habitat data----

setwd("//uniwa.uwa.edu.au/userhome/staff1/00104541/Desktop/GitHub/FRDC_Habitat/tidy data")
dir()

habitat.broad <- metadata%>%
  left_join(fov,by="sample")%>%
   left_join(broad,by="sample")


write.csv(habitat.broad,file=paste(study,"_broad.habitat.csv",sep = "."), row.names=FALSE)

habitat.detailed <- metadata%>%
  left_join(fov,by="sample")%>%
  #left_join(relief,by="sample")%>%
  left_join(detailed,by="sample")

write.csv(habitat.detailed,file=paste(study,"_detailed.habitat.csv",sep = "."), row.names=FALSE)





