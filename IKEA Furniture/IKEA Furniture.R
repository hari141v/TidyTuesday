#load necessary packages

{
  library(tidyverse)
  library(tidytext)
}

#import data

ikea <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv'
  )


# Extract the numerical value from old price column

ikea$oldprice_val <- str_extract(ikea$old_price, "\\d(\\,|\\.)*\\d*")

#Remove comma

ikea$oldprice_val <- str_remove(ikea$oldprice_val, ",")

#Convert into numerical data type

ikea$oldprice_val <- as.numeric(ikea$oldprice_val)

#Rearrange the column

ikea <- ikea %>% select(1:6, 16, everything())


#separete the product details from short_description column

ikea <-
  ikea %>% separate(short_description, "type", sep = "," , remove = F)


#calculate discount percentage from old and current price value

ikea$discount_prcnt <-
  round((ikea$oldprice_val - ikea$price) / (ikea$oldprice_val) * 100, 1)

#Rearrange the column

ikea <- ikea %>% select(1:7, 18, everything())



#Remove the end of the line number in link column

ikea$link <- str_remove(ikea$link, "(-\\w\\d*\\/)")

#Extract the color deatils from link column

ikea$color <- str_extract(ikea$link, "\\w+$")

#Rearrange the column

ikea <- ikea %>%  select(1:10, 18, everything())







#30% and more Discount Products in Each Category


(
  disc_prod <- ikea %>% filter(discount_prcnt >= 30) %>%
    
    group_by(category, type) %>%
    select(category, type, discount_prcnt) %>% arrange(desc(discount_prcnt)) %>%
    ggplot(aes(
      x = reorder_within(type, discount_prcnt, category),
      y = round(discount_prcnt, 0) ,
      fill = category
    )) +
    geom_bar(
      width = 0.5,
      stat = "identity",
      position = position_dodge(0.2)
    ) +
    scale_x_reordered() +
    labs(
      x = "Products",
      y = "Discount%",
      title = "30% and more Discount Products in Each Category\n\n",
      caption = "Source: IKEA | Kaggle | TidyTuesday"
    ) +
    facet_wrap(
      ~ category,
      scales = "free",
      labeller = label_wrap_gen(multi_line = TRUE)
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(
        face = "bold",
        size = 7.5,
        colour = "yellow"
      ),
      strip.background = element_rect(fill = "steelblue"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = "10"),
      plot.caption = element_text(face = "bold", size = "10"),
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(face = "bold", size = 8.5)
    ) +
    coord_flip()
)


#Save the plot

ggsave(
  "30 prcnt and more Discount Products in Each Category.jpeg",
  plot = color_op,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)



#Products Color option in Each Category

#create a color pattern to detect the colors from color column

pat <-
  c(
    "white|black|purple|grey|beige|red|brown|birch|pine|yellow|green|blue|pink|turquoise|orange|anthracite"
  )



(
  color_op <- ikea %>% filter(str_detect(color, pat)) %>%
    group_by(category, color) %>%
    summarise(c = n()) %>%
    ggplot(aes(
      x =  reorder_within(color, c, category),
      y = c,
      fill = category
    )) +
    #geom_bar(stat = "identity" ,position = position_dodge (0.2))+
    geom_bar(
      width = 0.5,
      stat = "identity",
      position = position_dodge(width = 0.2)
    ) +
    scale_x_reordered() +
    labs(
      x = "Colors",
      y = "",
      title = "Products Color option in Each Category\n",
      caption = "Source: IKEA | Kaggle | TidyTuesday"
    ) +
    facet_wrap(
      ~ category,
      scales = "free",
      labeller = label_wrap_gen(multi_line = TRUE)
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(
        face = "bold",
        size = 10,
        colour = "yellow"
      ),
      strip.background = element_rect(fill = "steelblue"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = "10"),
      plot.caption = element_text(face = "bold", size = "10"),
      axis.ticks = element_blank(),
      axis.text.y  = element_text(face = "bold", size = 8.5),
      
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank()
    ) +
    coord_flip()
)


#Save the plot

ggsave(
  "Products Color option in Each Category.jpeg",
  plot = color_op,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)
