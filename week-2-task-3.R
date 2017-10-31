# **********************************************************************************************
# Este script se elabora como respuesta al task 2 del proyecto final del curso de 
# Ciencia de Datos de Coursera.
#
# **********************************************************************************************


# Questions to consider

# 1 - Some words are more frequent than others - what are the distributions 
# of word frequencies?
# 2- What are the frequencies of 2-grams and 3-grams in the dataset?
# 3- How many unique words do you need in a frequency sorted dictionary to cover 50% of all 
# word instances in the language? 90%?
# 4- How do you evaluate how many of the words come from foreign languages?
# 5- Can you think of a way to increase the coverage -- identifying words that 
# may not be in the corpora or using a smaller number of words in the dictionary to cover the 
# same number of phrases?

#### Carga de Datos ####

library(tm)  

# El esquema de analisis que segui en esta tarea proviene de unas slides de Hugh Murrel "Data
# Mining with R", donde se recomienda el uso de las librerias (tm, wordcloud, fpc y igraph)

#### Loading the corpus from disk ####

# La siguiente sentencia consume mucha memoria y toma mucho tiempo, por lo tanto,
# opte por la opcion que continua.
#mycorpus  <- Corpus(DirSource("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt"),
#                  readerControl = list(language="en"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Cleaning the Corpus ####
twitter <- readLines("./Coursera-Swiftkey/final/en_US/en_US.twitter.txt")
blogs <- readLines("./Coursera-Swiftkey/final/en_US/en_US.blogs.txt")
news <- readLines("./Coursera-Swiftkey/final/en_US/en_US.news.txt")

# Tomo una muestra de cada archivo
nLines <- 5000;
set.seed(1984)
dat <- vector('character')
tmp <- sample(twitter, nLines, replace = FALSE)
dat <- c(dat, tmp)
tmp <- sample(blogs, nLines, replace = FALSE)
dat <- c(dat, tmp)
tmp <- sample(news, nLines, replace = FALSE)
dat <- c(dat, tmp)

rm(twitter, blogs, news)

saveRDS(dat, 'sample_text.rds')

# Contruimos el corpus
dat <- readRDS('sample_text.rds')

corpus <- VCorpus(VectorSource(dat))

# Elimino numeros
corpus <- tm_map(corpus, removeNumbers)

# Elimino puntuacion
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)

# Elimino espacio es blanco
corpus <- tm_map(corpus, stripWhitespace)

# Convierto a lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Elimino las stop words
corpus <- tm_map(corpus, content_transformer(removeWords), stopwords('english'))

# Stemming es un método para reducir una palabra a su raíz o (en inglés) a un stem. 
# Hay algunos algoritmos de stemming que ayudan en sistemas de recuperación de 
# información. Stemming aumenta el recall que es una medida sobre el número de 
# documentos que se pueden encontrar con una consulta.

#### EDA ####

# Creamos una matriz de terminos
(tdm <- TermDocumentMatrix(corpus))

# Imprimimos los terminos mas frecuentes, se ordean por orden alfabetico
(tt <- findFreqTerms(tdm, lowfreq = 150))

# Vemos los terminos por orden de frecuencias
termFrequency <- rowSums(as.matrix(tdm[tt,]))

# Grafico por orden de frecuencias
library(ggplot2)

barplot(termFrequency)

# Wordclouds
library(wordcloud)

tdmat <- as.matrix(tdm)

# Calculamos las frecuencias por palabras
v = sort(rowSums(tdmat), decreasing = TRUE)
d = data.frame(word=names(v), freq = v)

wordcloud(d$word, d$freq, min.freq = 300, random.color = TRUE, 
          colors = rainbow(7))







