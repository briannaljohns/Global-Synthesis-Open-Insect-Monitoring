library(litsearchr)
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)      


#import .ris from existing search on Web of Science;
results <- import_results(file="keypapers.ris")

# print(results)

# double check that .ris file loaded correctly
# nrow(results)
# colnames(results)
# results[1, "keywords"]

# Extract keywords
keywords <- extract_terms(keywords = results[, "keywords"], method = "tagged", min_n = 1)

keywords

# Extract key terms from titles
title_terms <- extract_terms(
  text=results[, "title"],
  method="fakerake",
  min_freq=2, min_n=1,)

title_terms

# Extract key terms from abstract

all_stopwords <- get_stopwords("English")

abstract <- extract_terms(
  text=results[, "abstract"],
  method="fakerake",
  min_freq=3, min_n=1,
  stopwords=all_stopwords)

abstract

all_terms <- unique(c(keywords, title_terms, abstract))

all_terms
