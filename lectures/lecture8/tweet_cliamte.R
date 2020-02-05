library(rtweet)
library(tidytext)
library(dplyr) # this should already be installed
library(stringr)
library(wordcloud2)
library(ggplot2) # this should already be installed 2
library(igraph)
library(ggraph)

token <- create_token(
  app = "geog215-coronavirus",
  consumer_key = "4UJa8OYqSNwyZqfTtNMNQj03D",
  consumer_secret = "kxI3PCwm08aVPp9rnYhrKs3TshNn6GcafmE7jo0UdwTjjiEG1X",
  access_token = "727055786-rU9vSEQsmWT3npnNWurU1Cf48zwaphDTa04d9n0U",
  access_secret = "sun9nRG4e0CZZ4grQkZlIvZ3w2HscrwpHezUMgzWxDZJC")
  
# Get tweets
climate_tweets <- search_tweets(q ="#climatechange", n = 10000, include_rts = FALSE, lang = "en")

climate_tweets <- climate_tweets %>%
  mutate(stripped_text = text %>% gsub("http.*","",.)) %>%
  mutate(stripped_text = stripped_text %>% gsub("https.*","",.)) %>%
  mutate(stripped_text = stripped_text %>% gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
       "", .))
  
# Unnest the words from the tweets
hmtTable <-  unnest_tokens(climate_tweets, word, stripped_text)

# Compare
climate_tweets$text[1:5]
hmtTable$word[1:5]

## Remove stop words - very common words such as "the", "of", etc
# Load the data of stop words
data(stop_words)
# anti_join is basically a "remove" command
hmtTable <-  anti_join(hmtTable, stop_words, by="word")

# plot the top 30 words -- notice any issues?
hmtTable %>%
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

# Remove other nonsense words
hmtTable <- filter(hmtTable, !word %in% c('t.co', 'https', 'handmaidstale', "handmaid's", 'season', 'episode', 'de', 'handmaidsonhulu',  'tvtime', 'watched', 'watching', 'watch', 'la', "it's", 'el', 'en', 'tv', 'je', 'ep', 'week', 'amp'))

# plot the top 30 words -- notice any issues?
hmtTable %>%
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

### Creating a word cloud
# Convert to a table of frequencies
hmtTable <-  count(hmtTable, word, sort = TRUE)


# Create word cloud with red text
redPalette <- c("#5c1010", "#6f0000", "#560d0d", "#c30101", "#940000")
wordcloud2(hmtTable, size=0.7, color=rep_len(redPalette, nrow(hmtTable)))

# we can remove the common keywords to look at some uncommon ones
hmtTable <- hmtTable %>% filter(!word %in% c("climate","change",
                                             "climateemergency"))

wordcloud2(hmtTable, size=1, color=rep_len(redPalette, nrow(hmtTable)))

climate_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of tweets with words related to coronavirus from past 7 days",
    subtitle = "Tweets aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## Create lat/lng variables using all available tweet and profile geo-location data
geo.climate_tweets <- lat_lng(climate_tweets)

climate_sf <- geo.climate_tweets %>%
  filter(!is.na(lng)) %>%
  st_as_sf(coords = c("lng","lat"),
                      crs = 4326)

## Get state boundaries
countries <- map_data("world")

## Subset only California
my_country<- countries %>%
  filter(region == "USA")

## Create base map
st_base <- ggplot(data = my_country, mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap() + 
  geom_polygon(color = "black", fill = "gray")

## Add tweets with geographic coordinates
st_base + 
  geom_point(data = geo.climate_tweets[which(!is.na(geo.ncov_tw$lng)),], aes(lng,lat,fill=NULL,group=NULL), col = rgb(0, .3, .7, .75), size=1) +
  coord_quickmap(xlim = c(-125,-66),  ylim = c(24,50))

#You can find your Country choice bounding box (xlim, ylim ) at
#<https://gist.github.com/graydon/11198540>
  