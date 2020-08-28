{
library(tidyverse)
library(ggmap)
library(lubridate)  
library(RColorBrewer)
library(randomcoloR) 
library(jpeg)
library(grid)
library(ggrepel)  
  
}

tuesdata <- tidytuesdayR::tt_load('2020-08-25')
chopped <- tuesdata$chopped


#Feature engineering air_date column

{ 
  chopped$air_date<-mdy(chopped$air_date)
  chopped$air_date_day <- day(chopped$air_date)#extract day
  chopped$air_date_month <- month(chopped$air_date,label = T)#extract month
  chopped$air_date_year <- year(chopped$air_date)#extract year
  chopped$air_date_quarter <- quarter(chopped$air_date)#extract quarter
  chopped$air_date_week <- week(chopped$air_date)#extract week
  chopped$air_date_wday <- wday(chopped$air_date , label=T )#extract weekday
  chopped$air_date_leap_year <- leap_year(chopped$air_date)#extract leap year info

  chopped<-chopped %>% select(1:7,22:28, everything())
}


#load image
cook_img <- readJPEG("lukas-blazek-f-TWhXOrLiU-unsplash (1).jpg")


#create unique colors  
n1 <- 15
palette1 <- distinctColorPalette(n1)  

#Longer and Shorter Time Broadcasted Seasons

(lo_sh_season <- chopped  %>%
  group_by(season) %>% mutate(
    start_date = min(air_date),
    end_date = max(air_date),
    duration_y = interval(start_date, end_date) %>%
      as.numeric('months'),
    cat = case_when(duration_y <= 3 ~ "Short Time",
                    duration_y >= 12 ~ "Long Time")
  ) %>% drop_na() %>%
  select(season, duration_y, cat) %>% unique() %>%
  ggplot(aes(x = factor(season), y = duration_y)) +
  annotation_custom(
    rasterGrob(
      cook_img,
      width = unit(1, "npc"),
      height = unit(1, "npc")
    ),-Inf,
    Inf,
    -Inf,
    Inf
  ) +
  geom_bar(
    stat = "identity",
    aes(fill = factor(season)),
    position = position_dodge(preserve = "single")
  ) +
  geom_text(
    aes(
      label = round(duration_y, 1),
      fontface = "bold",
      size = 9
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.9
  ) +
    
  scale_fill_manual(values = palette1) +
  labs(
    title = "Longer and Shorter Time Broadcasted Seasons",
    x = "Season",
    y = "Duration in Months",
    subtitle = "Season Episodes Aired more than a Year and Less than 4 Months",
    caption = "Source: IMDB-Kaggle | Jeffrey Braun"
  ) +
  facet_grid( ~ cat , scales = "free", space = "free") +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(face = "bold"),
    strip.background.x = element_rect(fill = "dodgerblue2",
                                      linetype = "blank"),
    strip.text.x       =   element_text(face = "bold",
                                        color = "white", size = 15),
    plot.background = element_rect(fill = "lightgoldenrod"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  )
)



#Save Image
ggsave(
  "Longer and Shorter Time Broadcasted Seasons.jpeg",
  plot = lo_sh_season,
  limitsize = F,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)


#load image
cook_img1 <- readJPEG("brooke-lark-29h2n639YDA-unsplash.jpg")

#Less than & Greater than 13 Episodes Seasons

(ep_le_gr <- chopped %>%
    group_by(season) %>%
    mutate(
      counts = n(),
      start_date = min(air_date_year),
      cat = case_when(counts < 13 ~ "<13 Episodes",
                      counts > 13 ~ ">13 Episodes")
    ) %>% select(season, counts, cat, start_date) %>%
    unique() %>% drop_na() %>%
    ggplot(aes(
      x = factor(season) ,
      y = counts ,
      fill = factor(season)
    )) +
    annotation_custom(
      rasterGrob(
        cook_img1,
        width = unit(1, "npc"),
        height = unit(1, "npc")
      ),-Inf,
      Inf,
      -Inf,
      Inf
    ) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(
        y = 0.4,
        label =  start_date,
        fontface = "bold",
        size = 3
      ),
      color = "black",
      position = position_dodge(width = 0.9)
      
    ) +
    geom_text(
      aes(
        label =  counts,
        fontface = "bold",
        size = 3
      ),
      color = "black",
      vjust = 0.9,
      position = position_dodge(width = 0.9)
      
    ) +
    scale_fill_brewer(palette =  "Paired") +
    labs(
      title = "Less than & Greater than 13 Episodes Seasons",
      x = "Season",
      y = "Episode Counts",
      subtitle = "Most of the Seasons had 13 Episodes Except the Following",
      caption = "Source: IMDB-Kaggle | Jeffrey Braun"
    ) +
    facet_grid(~ cat, scales = "free" , space = "free") +
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.caption = element_text(face = "bold"),
      strip.background.x = element_rect(fill = "dodgerblue2",
                                        linetype = "blank"),
      strip.text.x       =   element_text(face = "bold",
                                          color = "white", size = 15),
      plot.background = element_rect(fill = "lightgoldenrod"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    )
)


#Save Image

ggsave(
  "Less than & Greater than 13 episodes.jpeg",
  plot = ep_le_gr,
  limitsize = F,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)





#n2 <- 12
#palette2 <- distinctColorPalette(n2)  

#cook_img2 <- readJPEG("pexels-karolina-grabowska-4397725.jpg")

#Maximum Episode Rating in Each Month Over the Year

(max_epi_rat<- chopped %>%  drop_na() %>%  group_by(air_date_year,air_date_month) %>% 
  mutate(
    max_rat= max(episode_rating)) %>% select(air_date_year,
               air_date_month,max_rat) %>% 
  unique() %>% 
  ggplot(aes(x=air_date_month,y=max_rat,group=1,
             color="gold"))+
  # annotation_custom(
  #   rasterGrob(
  #     cook_img2,
  #     width = unit(1, "npc"),
  #     height = unit(1, "npc")
  #   ),-Inf,
  #   Inf,
  #   -Inf,
  #   Inf
  # ) +
  geom_line(size=1)+
  geom_point(size=2)+
  geom_text_repel(aes(label=max_rat),show.legend =F,
                   seed=889, fontface = "bold",
                   force = 30,alpha = 0.75, color="black",
                   segment.alpha = 0.7)+
  #scale_color_manual(values = palette2)+
  labs(
    title = "Maximum Episode Rating in Each Month Over the Year",
    x = "Month",
    y = "Episode Rating",
    subtitle = "From the Year 2009 to 2020",
    caption = "Source: IMDB-Kaggle | Jeffrey Braun"
  ) +
  facet_wrap(~air_date_year, scales = "free",drop=T,ncol = 3)+
  theme_dark()+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(face = "bold"),
        strip.background.x = element_rect(fill = "dodgerblue2",
                                          linetype = "blank"),
        strip.text.x       =   element_text(face = "bold",
                                            color = "white", size = 15),
        plot.background = element_rect(fill = "lightgoldenrod"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank()))


#Save Image

ggsave(
  "Maximum Episode Rating.jpeg",
  plot = max_epi_rat,
  limitsize = F,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)

