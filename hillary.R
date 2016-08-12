# Kaggle Project : Hilary Clinton's Emails

# Country vs Sentiment

# Sentiment analysis from ghassent's script:
# https://www.kaggle.com/ghassent/hillary-clinton-emails/sentiments-text-mining-and-more/discussion

# Country analysis from Olalekan's script:
# https://www.kaggle.com/ampaho/hillary-clinton-emails/countries-the-most-mentioned-by-hillary

library(RSQLite)
library(syuzhet) # for sentiment analysis
library(countrycode)
library(qdapDictionaries)
library(ggplot2)
library(dplyr)

db <- dbConnect(dbDriver("SQLite"), "../input/database.sqlite")

emails <- dbGetQuery(db, "SELECT ExtractedBodyText EmailBody FROM Emails e INNER JOIN Persons p ON e.SenderPersonId=P.Id WHERE p.Name='Hillary Clinton'  AND e.ExtractedBodyText != '' ORDER BY RANDOM()")

# country coding cleansing, courtesy Olalekan
count.occurences <- function(needle, haystack)
{
  sapply(regmatches(haystack, gregexpr(needle, haystack, ignore.case=T, perl=T)), length)
}
data(countrycode_data)
countrycode_data_without_atf <- countrycode_data[-83,]
countries <- countrycode_data_without_atf[, c("country.name", "regex", "iso2c", "iso3c")]
countries$other <- NA
countries[countries$country.name=="United Kingdom",]$other <- "UK"
words_to_remove <- rbind(DICTIONARY[nchar(DICTIONARY$word)==2,], DICTIONARY[nchar(DICTIONARY$word)==3,])
words_to_be_removed <- toupper(c(words_to_remove$word, "RE", "FM", "TV", "AL", "AQ", "LA", "BEN"))

# Now process all e-mails
allAnalysis <- NULL
for (n in 1:nrow(emails)) {
  if ((n %% 50) == 0) print(paste(trunc(100*n/nrow(emails)),"%"))
  email <- emails$EmailBody[n]
  sentiment <- get_nrc_sentiment(email)  
  
  for(i in 1:nrow(countries)) {
    n_occurences <- 0
    tmp <- 0
    # Find countries by matching a regexp
    if(!is.na(countries$regex[i])) {
      tmp <- count.occurences(countries$regex[i], email)
      n_occurences <- n_occurences + tmp
    }
    # Find countries by ]abbreviation
    for (countryAlt in c(3:5)) {
      if( (! (countries[i,countryAlt] %in% words_to_be_removed) ) && (!is.na(countries[i,countryAlt]))  ) {
        iso_boundary <- paste0("\\s", countries[i,countryAlt], "\\s")
        tmp <- count.occurences(iso_boundary, email)
        n_occurences <- n_occurences + tmp
      }
    }
    # Find countries by literal match
    if(tmp <= 0) {
      tmp <- count.occurences(countries$country.name[i], email)
      n_occurences <- n_occurences + tmp
    }
    
    if (n_occurences > 0) { # n_occurences can be > 1 in one email - relevant?
      country <- countries$country.name[i]
      if (is.null(allAnalysis)) {
        allAnalysis <- cbind(sentiment, country)
      } else {
        allAnalysis <- rbind(allAnalysis, cbind(sentiment, country))
      }
    }
  }
}

df <- group_by(allAnalysis, country) %>% 
  summarise(count=n(),
            Sentiment=sum(joy)+sum(trust)-sum(anger)-sum(disgust)-sum(fear)-sum(sadness),
            anger=sum(anger), 
            anticipation=sum(anticipation),
            disgust=sum(disgust), 
            fear=sum(fear), 
            joy=sum(joy), 
            sadness=sum(sadness),
            surprise=sum(surprise),
            trust=sum(trust), 
            negative=sum(negative), 
            positive=sum(positive))

df$Sentiment <- df$Sentiment / (max(df$Sentiment)-min(df$Sentiment))
p <- ggplot(filter(df, count > 2), aes(x=country, y=count, fill=Sentiment))+
  geom_bar(stat="identity", position = "identity")+
  labs(x=NULL, y="Frequency", title="Countries vs Sentiment from Hillary Clinton's e-mails") + 
  theme(axis.text.x = element_text(size  = 10, angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_gradient(low="red",high="green")

print(p)
