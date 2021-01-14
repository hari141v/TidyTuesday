#load necessary packages

{
  library(tidyverse)
  library(stringi)
  library(RColorBrewer)
  library(udpipe)
  library(stopwords)
  library(data.table)
  library(BTM) #biterm topic modeling
  library(textplot) #biterm plot
  library(ggraph)
}

# read data grom github link

artwork <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv'
  )
artists <-
  readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")


#create lifespan column

artists$life_span <- artists$yearOfDeath - artists$yearOfBirth

#Re-arrange lifespan column

artists <- artists %>% select(1:6, 10, everything())

#seperate state and country from placeofbirth column

artists <-
  artists %>% separate(placeOfBirth, c("Brstate", "Brcountry"), sep = ",")

#some state name have accents(diacritics),let's change them into general ASCII format

artists$Brstate <-
  stri_trans_general(artists$Brstate, id = "Latin-ASCII")

artists$Brstate <- str_trim(artists$Brstate)


#remove some irrelevant string from title

pat = c("Study|Studies|Figures|Blank|title|not|known|Title|Page|blank|Sketch|Sketches")


# clean the title and stored it in new column

cleaned_title = artwork %>%
  mutate(title = str_remove_all(title, pat),
         title = str_remove_all(title, "\\W\\d")) %>% select(title)



# create dataframe with cleaned title and id

anno <-
  data.frame(doc_id = artwork$id,
             text = cleaned_title ,
             stringsAsFactors = FALSE)

# change column name

colnames(anno)[2] <- "text"

#load udpipe pretrained english model from local

udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

# Tokenising, Lemmatising, Tagging and Dependency Parsing of raw text using udpipe

anno    <- udpipe(anno, "english", trace = 10)

# convert the processed text into data table

biterms <- as.data.table(anno)

# Get the noun, adjective, verb from processed text

biterms <- biterms[, cooccurrence(
  x = lemma,
  relevant = upos %in% c("NOUN", "ADJ", "VERB") &
    nchar(lemma) > 2 &
    !lemma %in% stopwords("en"),
  skipgram = 2
),
by = list(doc_id)]




set.seed(123456)
#create train data
traindata <-
  subset(anno,
         upos %in% c("NOUN", "ADJ", "VERB") &
           !lemma %in% stopwords("en") & nchar(lemma) > 2)

traindata <- traindata[, c("doc_id", "lemma")]

#train a topic model with 9 topics using BTM function

model     <- BTM(
  traindata,
  biterms = biterms,
  k = 9,
  iter = 2000,
  background = TRUE,
  trace = 100,
  detailed = TRUE
)


# topic model plot for top 30 terms in each topic

btm_mod_plot <- plot(
  model,
  top_n = 30,
  which = 1:3,
  seed = 100,
  biterms = model$biterms$biterms
) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "",
    y = "",
    title = "BiTerm Topic Model on Artwork Title\n",
    subtitle = "Top 30 Terms in Each Topic",
    caption = "Source: Tate Art Museum | TidyTuesday"
  ) +
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,
        face = "bold",
        size = 12))


#Save the plot

ggsave(
  "BiTerm Topic Model on Artwork Title.jpeg",
  plot = btm_mod_plot,
  limitsize = F,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 300
)

