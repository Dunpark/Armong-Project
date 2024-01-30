library(rvest)
library(dplyr)
library(tm)
library(wordcloud)
library(showtext)
library(ggplot2)
library(ggwordcloud)
library(udpipe)
library(quanteda) 
library(lattice) 
# Read the lines of the text file into a list
text_list <- readLines("sanabi_pos_text.txt") #1000 reviews

# Now 'text_list' is a list where each element corresponds to a line in the text file
all_texts <- unlist(text_list)  #list to one value of the sum of texts = text data of 1000 reviews from the top


## Print the review texts of the combined text
all_texts
cat(all_texts, sep = "\n") ##Printed text data for clear view #다 잘 들어갔는 지 확인됨

###워드클라우드 & 단어+빈도 표 만들기
# create function that gives the table of keywords and frequency from text data
preprocess_and_wordcloud <- function(sanabi_rev_text) {
  # Create a Corpus from the provided text
  docs <- Corpus(VectorSource(sanabi_rev_text))
  
  # Text preprocessing
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english"))
  
  toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, ' ', x))})
  docs <- tm_map(docs, toSpace, '–') 
  # Add more toSpace transformations as needed
  
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'ref', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'game', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'december', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'posted', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'good', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'amazing', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'great', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'year', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = 'lot', replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = "january|made|recommend|best|every|still|play|excellent|top|played|second|awesome|feel|makes|portions|strongly|got|sinthis|♥♥|around|buy|can|instead|interesting|parts|playing|regarding|say|since|think|though|try|worth|brilliant|met|one|probably|absolutely", replacement = '')
  docs <- tm_map(docs, content_transformer(gsub), pattern = "like|just|really|fun|get|well|sanabi|much|recommended|also|even|dont|ive|make|love|thing|bit", replacement = '')
  
  # Create Document-Term Matrix
  dtm <- DocumentTermMatrix(docs)
  
  # Extract individual unique words
  mywords <- dtm$dimnames$Terms
  
  # Create a word cloud
  m <- as.matrix(dtm)
  v <- sort(colSums(m), decreasing = TRUE)
  myNames <- names(v)
  dtmnew <- data.frame(word = myNames, freq = v)
  dtmnew <- subset(dtmnew, dtmnew$freq >= 70)
  keyword_count = dtmnew
  # Set seed for reproducibility
  set.seed(1234)
  
  output_list <- list(
    docs = docs,
    keyword_count = keyword_count 
  )
  
  return(output_list)
}

# Call the function with the provided text data and put it in a keyword_count
output_list <- preprocess_and_wordcloud(all_texts)
docs <- output_list$docs #위의 함수의 결과 output_list를 통해 docs추출해서 새로운 variable에 넣는 것
keyword_count1 <- output_list$keyword_count #얘는 dtmnew

#make a worldcloud using the dataframe 
ggplot(keyword_count1,
       aes(label = word,
           size = freq,
           col = freq)) +
  geom_text_wordcloud(area_corr = TRUE,
                      seed = 1234) +
  scale_radius(range = c(10, 40)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal()

####워드클라우드 바그래프 형식으로 빈도 구분
top_keywords <- head(keyword_count1, 10) #10 top keywords
ggplot(top_keywords, aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = freq), vjust = -0.5, size = 3, color = "black") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
        axis.text.y = element_text(color = "gray")) +
  labs(title = "Top 10 Keywords and Frequencies", x = "Keywords", y = "Frequency") 

###유의미한 단어들의 빈도통계 바그래프로 구성 

docs <- Corpus(VectorSource(all_texts))

# Now let's use UDpipe to show nouns or verbs in the wikipedia text
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = docs$content) # this takes a few minutes to run, it is annotating the text
x <- as.data.frame(x)

# Let's plot the frequencies of different Universal Parts of Speech (UPOS): nouns, verbs, adjectives, etc.
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")
### 주요키워드를 뽑아 카테고리화하하여 만든 표 구성하기 

###특정 키워드를 포함한 리뷰 찾기 - python에 포함

############## 거시적 Data Analysis - 게임간

#세가지 게임들의 긍정적리뷰에 공통적으로 존재하는 키워드들 확인
# Assuming your datasets are data frames with columns 'keywords' and 'freq' *Using 3 R files of each game
keywords1 <- keyword_count1$word
keywords2 <- keyword_count2$word
keywords3 <- keyword_count3$word

# Find common keywords
common_keywords_all <- Reduce(intersect, list(keywords1, keywords2, keywords3))
common_keywords_katanazeroSanabi <- Reduce(intersect, list(keywords1, keywords2))
common_keywords_Sanabiskull <- Reduce(intersect, list(keywords1,keywords3))
common_keywords_katanazeroskull <- Reduce(intersect, list(keywords2, keywords3))

common_keywords_all 
common_keywords_katanazeroSanabi
common_keywords_Sanabiskull
common_keywords_katanazeroskull

# Print the common keywords
print(common_keywords)



