{ 
  library(tidyverse)
  library(caret)
  library(plotly)
  library(data.table)
  library(psych)
  library(hrbrthemes)
  library(corrr)
  library(rpart)
  library(rpart.plot)
  library(cvms)
}


tuesdata <- tidytuesdayR::tt_load('2020-07-28')
tuesdata <- tidytuesdayR::tt_load(2020, week = 31)



penguins <- tuesdata$penguins
penguins_raw<- tuesdata$penguins_raw

body_mass<-penguins %>% drop_na()    %>% 
  ggplot(aes(y=body_mass_g,x=species,fill= species)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8)+
  geom_point(aes(y = body_mass_g, color = species), 
             position = position_jitter(width = .15), size = .5, 
             alpha = 5)+
  geom_boxplot(width=.1,
               outlier.fill  ="red", outlier.shape=23,
               outlier.size=2,inherit.aes = T
  ) +scale_fill_manual(values=c("darkorange", "orchid", "darkcyan"))+
  scale_color_manual(values=c("darkorange", "orchid", "darkcyan"))+
  stat_summary(fun = median, geom="point",colour="darkred", size=3,
               show.legend = FALSE) +coord_flip()+
  stat_summary(fun.data = fun_median,color="black", geom="text", vjust=-0.7)+
  labs(
    title = "Body Mass Distribution with Species ",
    subtitle = "Sex and Island",
    caption = "Source: Dr. Kristen Gorman, Dr. Allison Horst, and Dr. Alison Hill",
    y="Body Mass",
    x="Species"
    )+
  
  facet_grid(~sex~island)+
  theme(  
    legend.position = "none",
    plot.caption = element_text(hjust = 0.5 , face = "bold") ,
    strip.background.x =   element_rect(fill = "gold2"),
    strip.background.y =   element_rect(fill = "thistle2"),
    strip.text.x = element_text(size = 10, face="bold"),
    strip.text.y = element_text(size = 10, face="bold"),
    axis.title.x = element_text(face="bold",hjust = 0.5),
    axis.title.y = element_text(face="bold",vjust = 0.5),
    axis.text.y = element_text(face="bold"),
    axis.text.x = element_text(face="bold"),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank())     


ggsave(plot=body_mass,"Body Mass Distribution with Species.jpeg",units="cm", width=20, height=20, dpi=300)
