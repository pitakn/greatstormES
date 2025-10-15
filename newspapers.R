install.packages(c("tm", "wordcloud","SnowballC"))
install.packages("quanteda")
library(tm)
library(wordcloud)
library(SnowballC)
library(quanteda)

library(dplyr)
library(ggplot2)
news<-read.csv("firstanalysis.csv"
)
View(news)
before<-news %>%
  filter (news$before.or.after.storm == "before")
View(before)

write.table(before, file = 'before_articles.csv')

before$year<-as.character(before$year)
ggplot(before, aes(x = year)) +
  geom_bar(width=0.75) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = NULL, x = "Year", y = "Count")

before$month<-factor(before$month, levels=month.name)

ggplot(before, aes(x = month)) +
  geom_bar(width=0.75, fill = "darkgreen") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = NULL, x = "Month article was published", y = "Count")

after<-news %>%
  filter (news$before.or.after.storm == "after")
View(after)

write.table(after, file = 'after_articles.csv')

after$year<-as.character(after$year)
ggplot(after, aes(x = year)) +
  geom_bar(width=0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = NULL, x = "Year", y = "Count")

after$month<-factor(after$month, levels=month.name)
after[6, "month"] <- "February"

ggplot(after, aes(x = month)) +
  geom_bar(width=0.5, fill="darkgreen") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = NULL, x = "Month article was published", y = "Count")


#word clouds for before and after newspaper articles
before <- Corpus(DirSource("Txt_before"))
inspect(before)
before <- tm_map(before, PlainTextDocument)

#remove extra spaces
before <- tm_map(before, stripWhitespace)

# Convert to lowercase
before <- tm_map(before, tolower)

# Remove conjunctions etc.
before <- tm_map(before, removeWords, stopwords("english"))

# Remove commas etc.
before <- tm_map(before, removePunctuation)

# Remove "tree," "trees" from word clouds
custom_remove <- c("tree", "trees", "-", "\"")  
before <- tm_map(before, removeWords, custom_remove)

#Remove suffixes to the common 'stem'
before <- tm_map(before, stemDocument)

inspect(before)
View(before)

wordcloud(before
          , scale=c(2.5,1)     # Set min and max scale
          , max.words=19      # Set top n words
          , random.order=TRUE # Words in decreasing freq
          , rot.per=0.2    # % of vertical words
          , use.r.layout=TRUE # Use C++ collision detection
          , colors=brewer.pal(8, "Dark2"))

#after
after <- Corpus(DirSource("Txt_after"))
inspect(after)
after <- tm_map(after, PlainTextDocument)

#remove extra spaces
after <- tm_map(after, stripWhitespace)

# Convert to lowercase
after <- tm_map(after, tolower)

# Remove conjunctions etc.
after <- tm_map(after, removeWords, stopwords("english"))

# Remove commas etc.
after <- tm_map(after, removePunctuation)

# Remove suffixes to the common 'stem'
after <- tm_map(after, stemDocument)

# Remove "tree," "trees" from word clouds
after <- tm_map(after, removeWords, custom_remove)

inspect(after)

#most frequent terms across all 10 articles

before_tdm<-TermDocumentMatrix(before)
nrow(as.matrix(before_tdm))
findMostFreqTerms(before_tdm, n = 21L)

after_tdm<-TermDocumentMatrix(after)
nrow(as.matrix(after_tdm))
findMostFreqTerms(after_tdm, n = 22L)

wordcloud(after
          , scale=c(2.5,1)     # Set min and max scale
          , max.words=22    # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Dark2"),
          title="After")

# Remove suffixes to the common 'stem'
afer <- tm_map(after, stemDocument)

# Boxplots
fig1<-read.csv("fig1.csv")
View(fig1)
at.x <- seq(0.5,by=1,length.out=2)
jpeg(file="newspapers.jpeg", width=600, height=900)
boxplot(fig1, par(mar=c(7, 5, 1, 1), cex.lab=2, cex.axis=2),
        col=c("lightgreen","#f584ea"), outcol="black",pch=16,cex=2,
        boxwex=.5, at=at.x, 
        ylab="Frequency of term 'tree'",
        xlab="Newspaper article publication date")
dev.off()
fig2<-read.csv("source.csv")
jpeg(file="source.jpeg", width=600, height=900)
boxplot(fig2, par(mar=c(7, 5, 1, 1), cex.lab=2, cex.axis=2),
        col=c("lightgreen","#f584ea"), outcol="black",pch=16,cex=2,
        boxwex=.5, at=at.x, 
        ylab="Frequency of term 'source'",
        xlab="Newspaper article publication date")
dev.off()