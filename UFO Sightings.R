#import data set
install.packages("mapview",dependencies = T)
library(mapview)
library(data.table)
library(lubridate)
library(tibble)
library(ggplot2)
library(dplyr)
library(plyr)
install.packages("ggmap",dependencies = T)
install.packages("geonames",dependencies = T)
library(geonames)
library(ggmap)
install.packages("maps",dependencies = T)
install.packages(c("sf","mapview"),dependencies = T)
install.packages("mapview'",dependencies = T)
library(mapview)
library(sf)
library(maps)
install.packages("ggthemes" ,dependencies = T)
library(ggthemes)
data<-fread(file.choose(),header = T,na.strings = c(""," ","NA"))
str(data)
data.frame(colSums(is.na(data)))

dt<-parse_date_time(data$date_time ,c('%m/%d/%Y %H:%M' ,'%m-%d-%Y %H:%M'))


dated<-as.Date(data$dt, format="%Y-%m-%d")
data$dated<-as.Date(data$dt, format="%Y-%m-%d")
data<-add_column(data,dated, .after = 2)

data<-add_column(data,dt,.before = 2)
daydt<-day(data$dt)
data.frame(is.na(countryn))

data<-add_column(data,daydt, .after = 2)
yeardt<-year(data$dt)
data<-add_column(data, yeardt, .before = 3)
monthdt<-month(data$dt)
data<-add_column(data, monthdt, .after = 3)
data$monthdt<-months(as.Date(data$dt))

data$wedaydt<-NULL
wedaydt<-wday(data$dt ,label = T ,abbr = T)
data<-add_column(data,wedaydt,.after = 5)
woydt<-week(data$dt)
data<-add_column(data,woydt,.after = 6)
qoydt<-quarter(data$dt)
data<-add_column(data,qoydt,.after = 7)
leapdt<-leap_year(data$dt)
data<-add_column(data, leapdt, .after = 8)
data$leapdt<-as.factor(data$leapdt)
hoddt<-hour(data$dt)
data<-add_column(data,hoddt,.after = 9)


args(case_when)
names(data)[10]<-"time"
data$time<-format(data$dt, "%H:%M")
data$time<-as.numeric(data$time)

data$time <- ymd_hms(data$dt)

#create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

timec <- cut(x=hour(data$time), breaks = breaks, labels = labels, include.lowest=TRUE)

data<-add_column(data, timec, .after = 9)

library(sqldf)

ufo <-sqldf("select yeardt , latitude, longitude ,count(*) from data group by(yeardt)")


install.packages("gganimate",dependencies = T)
install.packages("animation",dependencies = T)
library(gganimate)
library(ggthemes)
install.packages("rlang" ,dependencies = T)
library(rlang)
library(animation)

ani.options(interval = 0.2)
gganimate(map)

data$dated<-as.integer(data$dated)
str(ufo$data)


library(scales)
library(gridExtra)



map2 <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() + 
  geom_point(data = ufo, aes(x = longitude, y = latitude, size = Sightings_Count),
             colour = 'purple', alpha = .5 ,show.legend = F) +
  scale_size_continuous(range = c(1, 10), 
                        breaks = waiver()) +
  labs(title="UFO Sightings Around The World",subtitle = "Year: {frame_time}" ) +
  transition_time(yeardt) +
  shadow_wake(wake_length = 0.1, alpha = FALSE) +
  enter_fade() +
  exit_shrink() +
  ease_aes("linear")+
  shadow_mark(past=TRUE)+
  theme(plot.title = element_text(hjust=0.5, face="bold", size = 18),
        plot.subtitle = element_text(face="bold",size = 16)) 
  
animate(map2)

anim_save("ufo.gif", map2, fps = 12, type = "cairo", width = 800, height = 500)

library(ggplot2  )
library(rlang)
library(dplyr)
data$monthdt<-factor(data$monthdt, levels = month.name)
monthufodt$month_s<-as.integer(monthufodt$month_s)
monthufodt$monthdt<-as.factor(monthufodt$monthdt)
library(dplyr)
ggplot(
  data %>% group_by(monthdt) %>% dplyr::summarize(month_s = n()),
  aes(x = reorder(monthdt,-month_s) , y = month_s, fill = monthdt)
) +
  geom_bar(stat = "identity",
           position = "dodge" ,
           width = 0.5) +
  ggtitle("Month Wise UFO Sightings From 1906-2014")+
  labs(subtitle="Highest number of UFO's identified in the month of July")+
  geom_text(aes(label = month_s),
            vjust = 1.6,
            color = "black",
            size = 3.9) +
  labs(fill="Month")+
  theme_dark()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold", size=12),
        axis.text.x  = element_text(hjust=0.5, face="bold", size=10),
        plot.subtitle  = element_text(face="italic", size=10),
        legend.position = "none") 

ggplot(data %>% group_by(dated) %>% summarize(n=n()), aes(x=dated)) + 
  geom_line(aes(y=n))  
  
map3 <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() +
  geom_point(data, mapping = aes(x = longitude, y = latitude, color = timec))+
  ggtitle("UFO Sightings in Different Times of the Day")+
  labs(subtitle = "Most UFO's Identified in the Evening")+
  theme(legend.text  = element_text(face = "bold", size = 10))+
  theme(legend.position = "left" , legend.box = "bottom")+
  guides(shape = guide_legend(override.aes = list(size = 10)))+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))+
  theme(legend.title = element_blank())  



wtable<-data %>% group_by(wedaydt) %>% dplyr::summarise(Count= n())
map4<-ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() +
  geom_point(data,mapping=aes(x = longitude, y = latitude,color=wedaydt))+
  ggtitle("UFO Sightings in seven Days")+
  labs(subtitle = "Most UFO's Identified in Saturday")+
  theme(legend.text  = element_text(face = "bold", size = 10))+
  theme(legend.position = "left" , legend.box = "bottom")+
  guides(shape = guide_legend(override.aes = list(size = 10)))+
 guides(colour = guide_legend(override.aes = list(size=8)))+
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))+
  theme(legend.title = element_blank())  


#text analysis
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
        "cluster", "igraph", "fpc")

lapply(Needed, require, character.only=T)
install.packages(Needed, dependencies = T)
install.packages("tidytext",dependencies = T)
install.packages("wordcloud2",dependencies = T)
install.packages("ggwordcloud",dependencies = T)
library(ggwordcloud)
library(wordcloud2)
library(tidytext)




# Create a corpus using the Tweet variable
corpus = VCorpus(VectorSource(data$description))
# Convert the corpus to lowercase
corpus = tm_map(corpus, content_transformer(tolower))
# Remove punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)
# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)
# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
# Remove all English-language stopwords
corpus = tm_map(corpus, removeWords, stopwords("english"))
# Build a document-term matrix out of the corpus
dtm = DocumentTermMatrix(corpus)


dtmdf<-tidy(dtm)
library(dplyr)
df<-dtmdf %>% group_by(term) %>% summarise(freq=n())
names(df)[1]<-"word"
#word cloud
set.seed(1234)

img = png::readPNG(system.file("ufoicon.png"))
wordcloud2(df , color = "random-dark" ,shape = "circle" ,shuffle = T)
letterCloud(df,word = "UFO")
library(ggplot2)


df %>% group_by(word) %>% arrange(desc(freq)) %>% head(10)
wordufo<-ggplot(df %>% group_by(word) %>% arrange(desc(freq)) %>% head(10), 
                aes(x=reorder(word,-freq),y=freq))+
  geom_bar(stat = "identity" , fill="greenyellow")+
  ggtitle("Most Common Words to Descibe the UFO")+
  theme_dark()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold", size=12),
        axis.text.x  = element_text(hjust=0.5, face="bold", size=12))





