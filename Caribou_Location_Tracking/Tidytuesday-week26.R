{
library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(ggmap)  
}

tuesdata <- tidytuesdayR::tt_load('2020-06-23')
tuesdata <- tidytuesdayR::tt_load(2020, week = 26)
locations<-tuesdata$locations
individuals<-tuesdata$individuals 

data.frame(colSums(is.na(individuals)))

table(locations$season)



locations$ts_date<-as.Date(locations$timestamp)

locations<- locations %>% select(1:5,8,everything())

locations$ts_date<-ymd(locations$ts_date)

locations$ts_day<-day(locations$ts_date)
locations<- locations %>% select(1:6,9,everything())

locations$ts_month<-month(locations$ts_date ,label = T)
locations<- locations %>% select(1:7,10,everything())


locations$ts_year<-year(locations$timestamp)
locations<- locations %>% select(1:8,11,everything())


locations$ts_wday<-wday(locations$ts_date ,label = T)
locations<- locations %>% select(1:9,12,everything())

locations$ts_quarter<-quarter(locations$ts_date)
locations<- locations %>% select(1:10,13,everything())

locations$ts_time <- format(as.POSIXct(locations$timestamp, tz = ""),
                            format = "%H:%M:%S")
locations<- locations %>% select(1:11,14,everything())

hour(ymd_hms(locations$timestamp))


#create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))

# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

locations$timec <- cut(x=hour(ymd_hms(locations$timestamp)), breaks = breaks, labels = labels, include.lowest=TRUE)

locations<- locations %>% select(1:12,15,everything())


locations<-locations %>% group_by(animal_id) %>% mutate(animal_count=n())

locations<- locations %>% select(1:2,16,everything())

locations<-locations %>% group_by(ts_date) %>% mutate(date_count=n())

locations<-locations %>% select(1:7,17,everything())




loc<-make_bbox(locations$longitude, locations$latitude)


locmap<-get_map(location =loc,maptype="hybrid",source="google") 
     
caribous<-ggmap(ggmap = locmap)+
  geom_point(data=locations,aes(x=longitude , y=latitude, 
                 color=factor(timec)),
             show.legend = TRUE,
             inherit.aes = FALSE,
             alpha = 1,
             size = 2)+
  labs(title = "Caribous Tracking in Different Times of Day")+
  guides(col = guide_legend(override.aes = list(shape = 15, size = 5)))+
  facet_wrap(~season)+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank())



ggsave("times_of_day.jpeg", 
       plot = caribous,limitsize = F,
       width = 30, height = 30, units = "cm", dpi = 300 )
