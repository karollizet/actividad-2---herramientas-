
install.packages("tidytext")
install.packages("stringr")
install.packages("dplyr")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyverse")
install.packages("tm")
install.packages("conflicted")

library(tidytext)
library(stringr)
library(dplyr)
library(wordcloud)
library(ggplot2)
library(readr)
library(tidyverse)
library(tm)
library(conflicted)
library(ggplot2)


data <- data.frame(x = 1:10, y = (1:10)^2)
setwd("D:/Documentos Karoly/Desktop/seminarioR/Iniciacion_R-main")
list.files()

quijote <- read_lines("quijote-1605.txt")
head(quijote)


quijote_df <- tibble(linea = seq_along(quijote), texto = quijote)
quijote_palabras <- quijote_df %>%
  unnest_tokens(palabra, texto)
head(quijote_palabras)


palabra_importante <- "don"

conteo_palabra <- quijote_palabras %>%
  dplyr::filter(palabra == palabra_importante) %>%
  summarise(frecuencia = n())


total_palabras <- nrow(quijote_palabras)
proporcion <- conteo_palabra$frecuencia / total_palabras
cat("La palabra '", palabra_importante, "' aparece", conteo_palabra$frecuencia, 
    "veces y su proporción es:", round(proporcion, 6), "\n")


frecuencia_palabras <- quijote_palabras %>%
  count(palabra, sort = TRUE)
head(frecuencia_palabras)

conflicts_prefer(dplyr::filter)


stopwords_es <- stopwords("es")
frecuencia_filtrada <- frecuencia_palabras %>%
  filter(!palabra %in% stopwords_es)
head(frecuencia_filtrada)

top_20_palabras <- frecuencia_filtrada %>%
  slice_max(n, n = 20)

print(top_20_palabras)

ggplot(top_20_palabras, aes(x = reorder(palabra, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 palabras más frecuentes en El Quijote",
       x = "Palabras",
       y = "Frecuencia") +
  theme_minimal()

library(wordcloud)
wordcloud(words = frecuencia_filtrada$palabra, 
          freq = frecuencia_filtrada$n, 
          max.words = 100, 
          colors = brewer.pal(8, "Dark2"))
ggplot(top_20_palabras, aes(x = "", y = n, fill = palabra)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de palabras más frecuentes",
       fill = "Palabras") +
  theme_void()
ggplot(top_20_palabras, aes(x = reorder(palabra, -n), y = n, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Frecuencia de palabras más importantes",
       x = "Palabras",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





  















