{
library(tidyverse)
library(tmaptools)
library(gganimate)
library(RColorBrewer)
library(hrbrthemes)
library(lubridate)
library(ggmap)
library(revgeo)
library(ggedit)  
}
tuesdata <- tidytuesdayR::tt_load('2020-07-21')


animal_outcomes <- tuesdata$animal_outcomes
animal_complaints<-tuesdata$animal_complaints

brisbane_complaints<-tuesdata$brisbane_complaints

animal_outcomes1 <-animal_outcomes[, -12] %>% gather( "State", "Value", 4:11,convert=T,
                     factor_key=T)


#extract unique suburb data from animal_complaints and brisbane_complaints
loc_data<- as.data.frame(unique(c(animal_complaints$Suburb,brisbane_complaints$suburb)))
colnames(loc_data)[1]<-"Suburb"

#geecoding
loc_details <-
  geocode_OSM(paste(unique(loc_data$Suburb), "Australia",
                    sep = ","),
              details = T,
              as.data.frame = T)



loc_details <-
  loc_details %>% separate(query ,    into = c("Suburb", "Country") , sep = ",")


#reverse geocoding to get the state 
State<-revgeo(loc_details$lon,loc_details$lat, output='hash',item = "state")

loc_details$State<-State$state



#extract month, year, quarter from complaint date
{
  animal_complaints$rec_date<-  mdy(animal_complaints$`Date Received`)
  animal_complaints$Date_month<- month(animal_complaints$rec_date)
  animal_complaints$Date_year<-year(animal_complaints$rec_date)
  animal_complaints$Date_quarter<-quarter(animal_complaints$rec_date)
  animal_complaints$rec_date<-NULL
  animal_complaints<-animal_complaints %>%  select(1:3,6:8,everything())
}

#left join to match the suburb and location data
animal_complaints<-animal_complaints %>% left_join(loc_details,by="Suburb")




brisbane_complaints$date_range<-str_remove(brisbane_complaints$date_range,
                                           "cars-srsa-open-data-animal-related-complaints-")        
brisbane_complaints$date_range<-str_remove(brisbane_complaints$date_range,
                                           ".csv") 
brisbane_complaints$date_range<-str_replace_all(brisbane_complaints$date_range,"-to","")



brisbane_complaints$date_range<-str_replace_all(brisbane_complaints$date_range,"1st-quarter","jan-mar")


brisbane_complaints<-brisbane_complaints %>% left_join(loc_details,
                                                       by=c("suburb"="Suburb"))





#animal_outcomes

animal_cat <- c("Cats", "Dogs","Wildlife")
outcome_cat<-c("Euthanized","Rehomed")

ani_outcome <- animal_outcomes1 %>%
  filter(State == "NSW",
         outcome %in% outcome_cat)   %>%
  ggplot() +
  aes(x = year,
      y = log(Value),
      color = outcome) +
  geom_line(size = 1) +
  geom_text(aes(fontface = "bold", label = outcome)) +
  scale_x_continuous(breaks = seq(1999, 2018, 2)) +
  scale_color_brewer(palette = "Dark2") +
  theme_light() +
  labs(
    title = "New South Wales Animal Outcomes",
    subtitle = "Rehomed & Euthanized",
    x = "",
    y = "",
    caption = "source: RSPCA, Townsville Animal Complaints and Brisbane Open Data - Animal Complaints."
  ) +
  facet_wrap( ~ animal_type, nrow = 3, scale = "free_x") +
  theme(
    plot.caption = element_text(hjust = 0.5 , face = "bold"),
    legend.position = "none",
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10) ,
    axis.text.y = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "dodgerblue1"),
    strip.text.x = element_text(size = 14)
  ) + transition_reveal(year)

#save the animation
animate(ani_outcome, width = 30, height = 30, units = 'cm', res = 100,fps = 5)
anim_save(file="New South Wales Animal Outcomes.gif", animation = last_animation())


#animal_complaints

(ani_comp<-animal_complaints %>% drop_na(State) %>%  
  group_by(`Complaint Type`,State) %>% 
  summarise(counts=n()) %>% 
  ggplot(aes(x = "", y = counts, fill = `Complaint Type` )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = counts), size=3,
            fontface="bold",
            position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")+
  labs(
    title = "State Wise Animal Complaint Types",
    subtitle = "Cats & Dogs",
    x = "",
    y = "",
    caption = "source: RSPCA, Townsville Animal Complaints and Brisbane Open Data - Animal Complaints."
  )+guides(fill=guide_legend(ncol=1))+
  coord_polar(theta = "y") +
  facet_wrap(~ State , ncol = 2)  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_text(hjust = 0.5 , face = "bold"),
        strip.background = element_rect(fill = "lightseagreen"),
        strip.text.x = element_text(size = 14),
        legend.position=c(0.8,0.35),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()))
  
  

ggsave(plot=ani_comp,"State wise animal complaints.jpeg",units="cm", width=20, height=20, dpi=300)
