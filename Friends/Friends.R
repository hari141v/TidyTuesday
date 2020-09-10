#Load necessary Libraries
{
  library(tidyverse)
  library(tidyselect)
  library(tidytext)
  library(ggrepel)
  library(jpeg)
  library(grid)
  library(patchwork)
  library(extrafont)
  library(igraph)
  library(ggraph)
  library(tm)
  library(wordcloud)
  library(qdap) 
  library(plotrix) 
}  




#import the data

tuesdata <- tidytuesdayR::tt_load('2020-09-08')
friends <- tuesdata$friends
friends_emotions  <- tuesdata$friends_emotions
friends_info <- tuesdata$friends_info



#load last ninja font
font_import(pattern = "GABRWFFR",prompt =F )
loadfonts(device = "win")
#font_import(prompt = F)
windowsFonts()


# Import the Wallpaper
img <- readJPEG("friends10.jpg")

#filter text of Main Characters
df_word<-friends %>%
  filter(speaker %in%  c("Rachel Green",
                         "Monica Geller",
                          "Phoebe Buffay",
                          "Joey Tribbiani",
                          "Chandler Bing",
                          "Ross Geller")) %>% 
  unnest_tokens  (word, text) %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(speaker) %>%
  count(word) %>% 
  top_n(100, n) 


#Most used words by Main Characters

wc<-df_word %>% ggplot(aes(
  x = 1,
  y = 1,
  size = n,
  color = n,
  label = word
)) +
  geom_text_repel(family = "Gabriel Weiss' Friends Font",
                  segment.size = 0,
                  force = 20,
                  segment.alpha = 0) +
  scale_color_gradient(low = 'gold1',
                       high = 'gold1', guide = "none") +
  scale_size(range = c(2, 15), guide = FALSE) +
  facet_wrap( ~ speaker) +
  labs(title = "F . R . I . E . N . D . S\n",
       subtitle = "Most used words by Main Characters\n",
       caption = "Source:  friends R package  |  Emil Hvitfeldt") +
  theme_dark() +
  theme(
    text = element_text(size=18,family = "Gabriel Weiss' Friends Font"),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(face = "bold"),
    
    strip.background.x = element_rect(fill = "white",
                                      linetype = "blank"),
    strip.text.x       =   element_text(face = "bold",
                                        color = "red",
                                        size = 18),
    plot.background = element_rect(fill = "aquamarine3"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_blank()
  )

#save the plot

ggsave(plot=wc,"Most used words.jpeg",units="cm", width=30, height=30, dpi=300)



#extract chandler text

chan<-friends %>% filter(speaker %in%
                          "Chandler Bing") %>%
            select(text) %>% drop_na()

#extract monica text

moni<-friends %>% filter(speaker %in%
                           "Monica Geller") %>%
             select(text) %>% drop_na()


#combine the text

all_txt<-c(chan,moni)

#Create a vector
all_txt <- VectorSource(all_txt)


all_corpus <- VCorpus(all_txt)

# Add new stop words to clean_corpus()
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,stopwords("english"))
  return(corpus)
}



# Clean the corpus
all_clean <- clean_corpus(all_corpus)

#Stem document
corpus_text_all=tm_map(all_clean, stemDocument)

# Create all_tdm
all_tdm <- TermDocumentMatrix(corpus_text_all)

# Give the columns distinct names
colnames(all_tdm) <- c("Monica Geller", "Chandler Bing")

# Create all_m
all_m <- as.matrix(all_tdm)

# Create comparison cloud
comparison.cloud(all_m,
                 colors = c("orange", "blue"),
                 max.words = 50)


# Identify terms shared by both documents
common_words <- subset(
  all_m,
  all_m[, 1] > 0 & all_m[, 2] > 0
)

head(common_words)

# calc common words and difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)

#get the top 25 word from both document

top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))



# Make pyramid plot

pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 100,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("Monica Geller",
                            "Words",
                            "Chandler Bing"))



#save the plot

ggsave(plot=last_plot(),"Common words.jpeg",units="cm", width=30, height=30, dpi=300)


