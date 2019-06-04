library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
library(ggplot2)
library(tidyverse)
library(tidytext)

reviews = read.csv('~/Desktop/5931/Project/reviews.csv', stringsAsFactors=F)
listings = read.csv('~/Desktop/5931/Project/listings.csv', stringsAsFactors = F)
#create a corpus
reviews_corp <- VCorpus(VectorSource(reviews$comments))
listings_corp <- VCorpus(VectorSource(listings$description))

#Preprocessing
#reviews
#corpus
reviews_corp <- tm_map(reviews_corp,  removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords('english')) %>%
  tm_map(removeWords, c("seattle","room",
                         "bed","home","two" , 
                         "need","apartment","like",
                         "street","one", 
                         "great","stay","place",
                         "house","place","will",
                         "felt","time","well",
                         "around","airbnb","day",
                         "next","time","just","also")) %>%
  tm_map(stemDocument) %>%
  tm_map(PlainTextDocument)

#listings
#corpus
listings_corp <- tm_map(listings_corp, removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords('english')) %>%
  tm_map(removeWords, c("seattle", "room",
                        "bed","home","two" , 
                        "need","bedroom",
                        "apartment","like",
                        "street","one","also",
                        "kitchen","bathroom",
                        "great","stay","place",
                        "house","place","will",
                        "felt","time","well",
                        "around","airbnb","day",
                        "next","time","just")) %>%
  tm_map(stemDocument) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)

#dataset
#cleaning the listing data
listings$price <- as.numeric(sub("\\$","", listings$price))
listings$neighbourhood_cleansed <- factor(listings$neighbourhood_cleansed)
listings$host_is_superhost <- factor(listings$host_is_superhost)

#Exploratory analysis
#Common key words
#Reviews
dtm_reviews <- DocumentTermMatrix(reviews_corp)
dtm_reviews <- removeSparseTerms(dtm_reviews, 0.99)
dtm_reviews
freq_reviews <- colSums(as.matrix(dtm_reviews))
df_reviews <- data.frame(word=names(freq_reviews), freq=freq_reviews)
df_freq_reviews <- subset(df_reviews, freq>=1000)

ggplot(aes(x=word, y=freq_reviews), data = df_freq_reviews)+
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#listings
dtm_listings <- DocumentTermMatrix(listings_corp)
dtm_listings <- removeSparseTerms(dtm_listings, 0.99)
dtm_listings
freq_listings <- colSums(as.matrix(dtm_listings)) 
df_listings <- data.frame(word=names(freq_listings), freq_listings=freq_listings)
df_freq_listings <- subset(df_listings, freq_listings>=1000)

ggplot(aes(x=word, y=freq_listings), data = df_freq_listings)+
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Frequency association
findAssocs(dtm_reviews, "downtown", corlimit=0.15)
findAssocs(dtm_listings, "downtown", corlimit=0.15)

#Word clouds
set.seed(142)
pal2 <- brewer.pal(8,"Set2")
#Reviews
wordcloud(reviews_corp, min.freq=1000, random.order=F, scale=c(5, .1), colors=pal2)
wordcloud(reviews_corp, max.words=100, random.order=F, rot.per=0.2, colors=pal2)
#listing
wordcloud(listings_corp, min.freq=50, random.order=F, colors=pal2)
wordcloud(listings_corp, max.words=100, random.order=F, rot.per=0.2, colors=pal2)

#Expensive vs affordable_listings
library(leaflet)
leaflet(data = listings) %>%
  addProviderTiles("Stamen.Watercolor") %>%
  addProviderTiles("Stamen.TonerHybrid") %>%
  addCircleMarkers(~longitude, ~latitude, radius = ifelse(listings$price > 100, 2, 0.2),
                   color = ifelse(listings$price > 100, "red", "green"),
                   fillOpacity = 0.4)

#Sentiment analysis
reviews_words <- reviews %>%
  select(listing_id, reviewer_id, reviewer_name, comments) %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

reviews_sentiment <- reviews_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(listing_id ) %>%
  summarize(sentiment = mean(afinn_score))

reviews_sentiment2 <- reviews_sentiment %>% 
  inner_join(listings,by=c("listing_id"="id")) %>%
  select(host_is_superhost, listing_id, sentiment)

ggplot(reviews_sentiment2, aes(x=sentiment))+
  geom_histogram(binwidth = 0.1, aes(fill = host_is_superhost))+
  labs(title="Histogram of AFIIN lexicon sentiment score",
       x="Mean AFIIN Score", y="Count") +
  theme_minimal()

#Hierarchical clustering
#reviews
d_reviews_1 <- dist(t(dtm_reviews), method="euclidian")
fit1 <- hclust(d=d_reviews_1, method="ward.D")
plot(fit1, hang=-1)
groups <- cutree(fit1, k=6) # "k=" defines the number of clusters you are using   
rect.hclust(fit1, k=6, border="red")
#listings
d_listings_1 <- dist(t(dtm_listings), method="euclidian") 
fit2 <- hclust(d=d_listings, method="ward.D")
plot(fit2, hang=-1)
groups <- cutree(fit2, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit2, k=6, border="red")

#K-means clustering
#Reviews
d_reviews_2 <- dist(t(dtm_reviews), method="euclidian")
kfit1 <- kmeans(d_reviews_3, 6)
REVIEWS = as.matrix(d_reviews_3)
clusplot(REVIEWS, kfit$cluster, color=T, shade=T, labels=6, lines=0)
#Listings
d_listings_2 <- dist(t(dtm_listings), method="euclidian")
kfit2 <- kmeans(d_listings_2, 6)
LISTINGS = as.matrix(d_listings_2)
clusplot(LISTINGS, kfit2$cluster, color=T, shade=T, labels=6, lines=0)


