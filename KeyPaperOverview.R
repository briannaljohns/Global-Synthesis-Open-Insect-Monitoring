library(litsearchr)
library(dplyr)
library(ggraph)
library(igraph)
library(ggplot2)

#loading results
#import .ris from existing naive search on Web of Science
keypaper_results <- import_results(file="LitSearchRWoSNaiveResults.ris")

# #double check that .ris file loaded correctly
# nrow(naive_results)
# naive_results
# colnames(naive_results)
# naive_results[1, "title"]

#title terms
# get common stopwords from litsearchr
all_stopwords <- get_stopwords("English")
extract_terms(text=naive_results[, "title"], method="fakerake", min_freq=3, min_n=2, stopwords = all_stopwords) 

title_terms <- extract_terms(
  text=naive_results[, "title"],
  method="fakerake",
  min_freq=3, min_n=2,
  stopwords=all_stopwords
)

title_terms

terms <- unique(title_terms)

# network analysis
docs <- paste(naive_results[, "title"], naive_results[, "abstract"])
docs[1]

dfm <- create_dfm(elements=docs, features=terms)
dfm[1:3, 1:4]

# create network graph

g <- create_network(dfm, min_studies=3)

ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)

#rank search terms
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

cutoff_cum <- find_cutoff(g, method="cumulative", percent=0.8)

cutoff_cum

cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")

get_keywords(reduce_graph(g, cutoff_cum))

cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)
cutoff_change
cutoff_fig +
  geom_hline(yintercept=cutoff_change, linetype="dashed")

g_redux <- reduce_graph(g, cutoff_change[1])
selected_terms <- get_keywords(g_redux)

selected_terms