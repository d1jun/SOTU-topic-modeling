# POLI 176 Final Project

# Load library
library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(stm)

setwd("~/Downloads/DSC161/Final")

# Read data
metadata <- read_csv("SOTU_WithText.csv")
#Look at data
metadata

#Create a corpus of the state of the union speeches
corpus_sotu <- corpus(metadata, text_field = "text")
corpus_sotu

#Create a document feature matrix (dfm)
toks <- corpus_sotu %>%
  tokens()
dfm <- dfm(toks)

#32,760 features, wow!
#Let's look at that in a word cloud
textplot_wordcloud(dfm, col="black")

#Some common pre-processing
toks <- tokens(corpus_sotu, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm <- dfm(toks)

#We can trim the dfm of rare words
dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.05, docfreq_type = "prop")
dfm_trimmed
# textplot_wordcloud(dfm_trimmed, col="black")

dfm_trimmed <- dfm_trimmed[metadata$party%in%c("Democratic", "Republican"),]
metadata <- metadata[metadata$party%in%c("Democratic", "Republican"),]

#STM
#Process the data to put it in STM format.  Textprocessor automatically does preprocessing
temp<-textProcessor(documents=metadata$text,metadata=metadata)
#prepDocuments removes words/docs that are now empty after preprocessing
out <- prepDocuments(temp$documents, temp$vocab, temp$meta)

#This takes a bit. You'd want to remove max.em.its -- this is just to make it shorter!
#Here we are using prevalence covariate party
model.stm <- stm(out$documents, out$vocab, K = 15, prevalence = ~party,
                 data = out$meta) 
model2.stm <- stm(out$documents, out$vocab, K = 15, prevalence = ~party+ s(year),
                 data = out$meta)

# Find most probable words in each topic
# Only subtle difference between the two model (with/without smoothed time indicator)
labelTopics(model.stm)
labelTopics(model2.stm)

#Get representative documents, still subtle differences
findThoughts(model.stm, texts=out$meta$text, topics=5, n=1)
findThoughts(model.stm, texts=out$meta$president, topics=5, n=10)
findThoughts(model.stm, texts=out$meta$year, topics=5, n=10)
findThoughts(model.stm, texts=out$meta$president, topics=14, n=10)
findThoughts(model.stm, texts=out$meta$year, topics=14, n=10)

findThoughts(model2.stm, texts=out$meta$text, topics=5, n=1)
findThoughts(model2.stm, texts=out$meta$president, topics=5, n=10)
findThoughts(model2.stm, texts=out$meta$year, topics=5, n=10)
findThoughts(model2.stm, texts=out$meta$president, topics=14, n=10)
findThoughts(model2.stm, texts=out$meta$year, topics=14, n=10)

#And most common topics
plot(model.stm, n=15)


#Estimate relationship between parties
model.stm.ee <- estimateEffect(1:15 ~ party, 
                               model.stm, meta = out$meta)
plot(model.stm.ee, "party", method="difference", 
     cov.value1="Democratic", cov.value2="Republican")

