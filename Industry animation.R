setwd("your loacation") # set your working directory

rm(list=ls())

library(readxl)
library(tidyverse)
library(maps)
library(rgdal)
library(viridis)
library(gganimate)


FDI_Industry <- read_excel("Data FDI Industry till Dec 17.xlsx")
FDI_data <- FDI_Industry[,c(2,5,6,9,11,12,13,14)]

FDI_data$`APPROVED DATE` <- substr(FDI_data$`APPROVED DATE`,1,4)
names(FDI_data)[1] <- "Year"
FDI_data$COUNTRY <- ifelse(FDI_data$COUNTRY == "INDIA","INDIA",ifelse(FDI_data$COUNTRY=="CHINA (MAINLAND)","CHINA","Others"))
NumberWise <- summarise(group_by(FDI_data, Year, DISTRICT, COUNTRY), Number= length(DISTRICT))


Nepal<- readOGR("Nepal Map/shape_files_of_districts_in_nepal.shp")
Nepal_Map <- fortify(Nepal)

#join Nepal to get district data
Nepal$id <- row.names(Nepal) #give an id variable to the shp imported file. This will match with the id after fortify
Nepal_Map <- left_join(Nepal_Map, Nepal@data) # keep all of Nepal_Map


#capital letter, rename and merge
Nepal_Map$dist_name <- toupper(Nepal_Map$dist_name)

#functions imported
source("**Directory**District Renaming.R") 

NumberWise<- function_dist_name(NumberWise,2)

names(Nepal_Map)[12] <- "DISTRICT"
colnames(NumberWise)[2] <- "DISTRICT"
NumberWise <- na.omit(NumberWise)


#functions imported
source("**Directory** District_Year.R") 

#new dataset called "data_set in imported
NumberCum <- left_join(data_set, NumberWise)

NumberCum$Number <- NumberCum$Number %>% replace_na(0)

#cumulative by group
NumberCum <- NumberCum %>%
  group_by(DISTRICT, COUNTRY) %>%
  mutate(cum_number = cumsum(Number))

#making different coordinates for India, China and others
Dist_shape <- summarise(group_by(Nepal_Map,DISTRICT), longI = mean(long), latI = mean(lat),
                 longC = median(long),latC= median(lat), longO =mean(mean(long),median(long)), latO=mean(mean(lat),median(lat))) 


all_data <- left_join(NumberCum, Dist_shape) #merge
all_data$long <- ifelse(all_data$COUNTRY== "INDIA", all_data$longI,ifelse(all_data$COUNTRY=="CHINA",all_data$longC,all_data$longO))
all_data$lat <- ifelse(all_data$COUNTRY== "INDIA", all_data$latI,ifelse(all_data$COUNTRY=="CHINA",all_data$latC,all_data$latO))
all_data <- all_data[,-c(6:11)]
all_data$cum_number<- ifelse(all_data$cum_number==0,NA,all_data$cum_number)
all_data <- all_data[all_data$COUNTRY != "Others" & all_data$Year <= 2074,]

#Map it

map <- ggplot() +
  geom_polygon(data = Nepal_Map, aes(x=long, y = lat, group = group), fill="white", color="grey50", alpha=0.9)+
  geom_point(data=all_data, aes(x=long, y=lat, size=cum_number, color = COUNTRY , alpha =0.7)) +
  scale_size_continuous(range=c(1,12))+
  labs( title = "Number of registered FDI in Nepal in each district by Year (BS)")

map

animate_map <- map + 
  transition_time(Year)+
  ease_aes('linear')+
  labs(subtitle = "Year: {round(frame_time,1)}")

animation <- animate(animate_map, height= 800, width=800, fps=15, duration = 30, end_pause = 60, res=100)
animation

anim_save("FDI by number2.mp4", animation)
  
gganimate(animation, "FDI flow.mp4")
