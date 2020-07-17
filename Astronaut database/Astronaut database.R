library(tidyverse)
library(ggrepel)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load('2020-07-14')


astronauts <- tuesdata$astronauts





#occupation column having mixed(upper+lower case) letters, so convert it into lowercase
astronauts$occupation <- tolower(astronauts$occupation)


#In occupation column ,change the space tourist as other (space tourist)
astronauts <- astronauts %>%
  mutate(occupation = recode(occupation,
                             `space tourist` = "other (space tourist)"))


#create the age(at the time of selction) column
astronauts$age_whn_sel <-
  astronauts$year_of_selection - astronauts$year_of_birth

#create the age(at the time of first mission) column
astronauts$age_fist_mis <-
  astronauts$year_of_mission - astronauts$year_of_birth


#create day column from total_eva_hrs
astronauts$eva_in_day <- astronauts$total_eva_hrs / 24


#re-arrange newly created columns
astronauts <-
  astronauts %>% select(1:11, 25, 12:15, 26, everything())


#Selected and on Mission in the Same Year

(
  astronaut <-
    astronauts %>% filter(astronauts$age_whn_sel == astronauts$age_fist_mis) %>%
    ggplot(aes(
      x = sex,
      y = age_whn_sel,
      fill = sex,
      show_guide  = F
    )) + scale_fill_manual(values = c("indianred2", "darkgoldenrod1")) +
    
    geom_point() +  geom_label_repel(
      aes(
        label = paste0(
          name,
          '(',
          age_whn_sel,
          ", ",
          year_of_mission,
          ')'
          ,
          "\n",
          nationality
        )
      ),
      size = 3,
      seed = 8891,
      fontface = "bold",
      force = 30,
      segment.alpha = 0.7
      
    ) +  guides(fill = guide_legend(
      title = "Gender",
      override.aes = aes(label = "")
    )) +
    theme_dark() +
    facet_grid(
      military_civilian ~ occupation ,
      scales = "free_y",
      space = "free_y"
    ) +
    labs(title = "Selected and on Mission in the Same Year",
         y = "Age") + theme(
           plot.background = element_blank(),
           panel.grid = element_blank(),
           legend.position = "bottom",
           axis.title.x = element_blank(),
           axis.text.x = element_blank(),
           axis.line.x = element_blank(),
           axis.ticks.x = element_blank(),
           plot.title = element_text(hjust = 0.5)
         )
)

ggsave(
  "Same Year Selection and Mission.jpeg",
  plot = astronaut,
  limitsize = F,
  width = 40,
  height = 40,
  units = "cm",
  dpi = 300
)



#Total Extravehicular Activity more than One and Half Days

(
eva<-astronauts %>% distinct(name, .keep_all = T) %>%
    filter(eva_in_day > 1.5) %>% ggplot(aes(
      x = sex,
      y = eva_in_day,
      fill = sex,
      show_guide  = F
    )) + scale_fill_manual(values = c("mediumpurple3", "darkkhaki")) +
    
    geom_point() +  geom_label_repel(
      aes(label = paste0(name,
                         '(',
                         nationality,
                         ')'
                         ,
                         "\n")),
      color = "black",
      size = 2.8,
      seed = 8891,
      fontface = "plain",
      force = 30,
      segment.alpha = 0.7
      
    ) +  guides(fill = guide_legend(
      title = "Gender",
      override.aes = aes(label = "")
    )) +
    theme_dark() +
    facet_grid(
      ~ military_civilian ~ occupation  ,
      scales = "free_x",
      space = "free_x"
    ) +
    labs(title = "Total Extravehicular Activity more than One and Half Days",
         y = "EVA in Days ") + theme(
           plot.background = element_blank(),
           panel.grid = element_blank(),
           legend.position = "bottom",
           axis.title.x = element_blank(),
           axis.text.x = element_blank(),
           axis.line.x = element_blank(),
           axis.ticks.x = element_blank(),
           plot.title = element_text(hjust = 0.5)
         )
)


ggsave(
  "EVA more than 1.5 days.jpeg",
  plot = eva,
  limitsize = F,
  width = 40,
  height = 40,
  units = "cm",
  dpi = 300
)
