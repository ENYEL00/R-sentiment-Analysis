#Sentiment Analysis is a process of extracting opinions that have different polarities. 
#By polarities, we mean positive, negative or neutral.
#It is also known as opinion mining and polarity detection.
#With the help of sentiment analysis, 
#you can find out the nature of opinion that is reflected in documents, 
#websites, social media feed, etc. 
#Sentiment Analysis is a type of classification where the data is classified into different classes.
#These classes can be binary in nature (positive or negative) or,
#they can have multiple classes (happy, sad, angry, etc.).

# I will carry out sentiment analysis with R language in this project. 
#The dataset that I will use is provided by the R package ‘janeaustenR’.

install.packages('tidytext')
library(tidyr)
library(dplyr)
library(janeaustenr)
library(stringr)
library(tidytext)
library(ggplot2)
library(reshape2)
library(wordcloud)
sentiments
View(sentiments)

View(get_sentiments("bing"))

View(austen_books())

tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()%>%
  unnest_tokens(word, text)

View(tidy_data)


#We have performed the tidy operation on our text such that each row contains a single word.
#We will now make use of the “bing” lexicon to and implement filter() over the words that correspond to joy.
#We will use the book Emma and derive its words to implement out sentiment analysis model.

#Get the positive words of the bing lexicon and put it on a data frame variable
positive_senti <- get_sentiments("bing")%>%
  filter(sentiment == "positive")

View(positive_senti)

#Then match that variable with Emma book in the tidy_data data frame variable
#And count the words that makes match, and how many times there is in the tidy_data data frame
tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_senti)%>%
  count(word, sort = TRUE)



#From our above result, we observe many positive words like “good”, “happy”, “love” etc. 
#In the next step, we will use spread() function to segregate our data into separate columns of positive and negative sentiments. 
#We will then use the mutate() function to calculate the total sentiment, that is, the difference between positive and negative sentiment.

bing <- get_sentiments("bing")
View(bing)

Emma_sentiment <- tidy_data %>%
  inner_join(bing, relationship = "many-to-many") %>%
  count(book = "Emma", index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

View(Emma_sentiment)

#SINCE HERE, I'LL USE PLOTS

#In the next step, 
#we will visualize the words present in the book “Emma” based on their corresponding positive and negative scores.

ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE)


#Let us now proceed towards counting the most common positive and negative words that are present in the novel.

counting_words <- tidy_data %>%
  inner_join(bing, relationship = "many-to-many") %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)


#In the next step, we will perform visualization of our sentiment score.
#We will plot the scores along the axis that is labeled with both positive as well as negative words.
#We will use ggplot() function to visualize our data based on their scores.

counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill =  sentiment))+
  geom_col()+
  coord_flip()+
  labs(y = "Sentiment Score")

#In the final visualization, let us create a wordcloud that will delineate the most recurring positive and negative words.
#In particular, we will use the comparision.cloud() function to plot both negative and positive words in a single wordcloud


tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)
