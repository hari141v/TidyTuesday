library(tidyverse)
library(ggbump)
library(ggpubr)
library(tidytext)
library(RColorBrewer)
library(ggpol)

tuesdata <- tidytuesdayR::tt_load('2020-08-04')

energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals


country_totals1 <- country_totals %>%
  gather(Year,Energy_in_GWh,5:7) %>% 
  spread(type,Energy_in_GWh) %>% 
  mutate(Total= rowSums(.[5:8]) )%>% 
  mutate(energy_lost= Total-`Total net production`) %>% 
  group_by(Year) %>% 
  arrange(Year,country_name,`Total net production`) %>% 
  mutate(rank = row_number(-`Total net production`)) %>% 
  mutate(Highest= ifelse(Exports>Imports,"Exports","Imports"))


im_to<-country_totals1 %>% 
  filter(Imports>`Total net production`) %>% 
  ggplot(aes(
    x = reorder_within( country_name,(Imports-`Total net production`),Year),
    y = Imports-`Total net production`,
    fill=Imports-`Total net production`
  )) +
  geom_col(position = "identity")+
  scale_fill_gradient(low = 'green', 
                      high = 'red',name="Energy in GWh ")+
  scale_x_reordered()+
  labs(
    title = "European Countries: Imports Higher than Total Production",
    caption = " Source: Eurostat")+
  facet_wrap(~Year,scales = "free",nrow=3)+
  theme(panel.background = element_rect(fill="papayawhip"),
        plot.background = element_rect(fill = "ivory2"),
        legend.position = "right",
        panel.grid = element_blank(),
        plot.caption = element_text(hjust =1,face = "bold"),
        strip.text.x = element_text(face = "bold",colour = "black" ,size=10),
        axis.text = element_text(face="bold"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())+coord_flip()

ggsave(plot=im_to,"European Countries: Imports Higher than Total Production.jpeg",units="cm", width=20, height=20, dpi=300)



el_et<-country_totals1 %>% filter(energy_lost>=10000) %>% 
  gather(cat,val,10:11) %>% drop_na() %>% 
  mutate(cat=recode(cat,"energy_lost"="Energy_Lost",
                    "Total"="Total_Production")) %>% 
  group_by(cat) %>% #mutate(prop=prop.table(val)) %>% 
  arrange(country_name,cat,val) %>% 
  ggplot()+
  geom_bar(aes(x=reorder(country_name,val),
               y=val,fill=cat),
           stat = "identity",width = 0.5,
           data=. %>% filter(cat=="Total_Production")) +
  geom_bar(aes(x=reorder(country_name,val),
               y=-val,fill=cat), 
           stat = "identity",width = 0.5,
           data=. %>% filter(cat=="Energy_Lost"))+
  scale_y_continuous(limits = c(-300000,680000))+
  facet_wrap(~Year,scales = "free")+
  scale_x_reordered()+
  labs(
    title = "Comparing Energy Lost and Total Energy Production of European Countries",
    subtitle = "Energy Lost Greater Than 10000 Gigawatt hours",
    caption = " Source: Eurostat"
  )+facet_wrap(~Year,scales = "free",nrow=3)+
  theme(panel.background = element_rect(fill="papayawhip"),
        plot.background = element_rect(fill = "ivory2"),
        legend.position = "bottom",
        panel.grid = element_blank(),
        legend.title = element_blank(),
        plot.caption = element_text(hjust =1,face = "bold"),
        strip.text.x = element_text(face = "bold",colour = "black" ,size=10),
        axis.text = element_text(face="bold"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())+coord_flip()

ggsave(plot=el_et,"Comparing Energy Lost and Total Energy.jpeg",units="cm", width=30, height=30, dpi=300)






