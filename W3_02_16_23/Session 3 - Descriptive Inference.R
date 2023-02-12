# TA: Elisa Wirsching
# Course: Text as Data
# Date: 16/02/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, Kevin Munger, Patrick Chester and Leslie Huang.


# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #

## Set up Quanteda 

# Clear Global Environment
rm(list = ls())

# Libraries

pacman::p_load(dplyr,
               quanteda,
               quanteda.corpora,
               quanteda.textstats,
               ggplot2) 

# gutenbergr: https://www.gutenberg.org/, https://cran.r-project.org/web/packages/gutenbergr/index.html 
# stylest: https://leslie-huang.github.io/stylest/

# ============================================================================= #
####                                HEAP'S LAW                               ####
# ============================================================================= #
# Token-type relationship in corpus
# How might pre-processing affect this relationship? 
# Think about reducing the dimensionality of the problem.

# M = kT^b

# M = vocab size (num of types)
# T = number of tokens

# k, b are constants
# 30 <= k <= 100
# 0.4 <= b <= 0.6

# 2.1 Example using data from the corpus of inaugural speeches
tokens <- tokens(data_corpus_inaugural, remove_punct = TRUE) 
Tee <- sum(lengths(tokens))

inaug_dfm <- dfm(tokens)
M <- nfeat(inaug_dfm)  # number of features = number of types

# Let's check using parameter values from MRS Ch. 5 for a corpus with more than 100,000 tokens

k <- 44
b <- .49

k * (Tee)^b
M

# Let's think about why (what types of texts are these?)

# New parameters

k <- 41
b <- 0.46

k * (Tee)^b

# ============================================================================= #
####                                ZIPF'S LAW                               ####
# ============================================================================= #
# Term frequency in corpus and rank
# tells us about distribution of terms
# intuition: frequency decreases rapidly with rank

# x-axis: log of ranks 1 through 100
# y-axis log of frequency of top 100 terms from the DFM

plot(log10(1:100), log10(topfeatures(inaug_dfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", 
     main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus")

# Fits a linear regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(inaug_dfm, 100)) ~ log10(1:100))

# Adds the fitted line from regression (red) and relationship predicted by Zipf's law (black) to the plot 
abline(regression, col = "red")
abline(a = regression$coefficients["(Intercept)"], b = -1, col = "black")

# Returns the 95% confidence intervals for the regression coefficients
confint(regression)

# Provides R-squared, F-test, and cofficient estimates from regression
summary(regression)

## Stopwords: do they affect Zipf's law?

mydfm <- tokens %>% 
  dfm() %>% 
  dfm_remove(pattern=stopwords("english"))

plot(log10(1:100), log10(topfeatures(mydfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", 
     main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus (w/o stopwords)")

# Regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(mydfm, 100)) ~ log10(1:100))
abline(regression, col = "red")
abline(a = regression$coefficients["(Intercept)"], b = -1, col = "black")
confint(regression)
summary(regression)

plot(1:100, topfeatures(inaug_dfm, 100),
     xlab = "rank", ylab = "frequency", 
     main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus")

plot(1:100, topfeatures(mydfm, 100),
     xlab = "rank", ylab = "frequency", 
     main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus (w/o stopwords)")



# ============================================================================= #
####                            MEASURING SIMILARITY                         ####
# ============================================================================= #
# This helps illustrate the value of the vector representation

# 6.1 Cosine similarity--take the dot product of two vectors
# cos = x*y/|x||y|
calculate_cosine_similarity <- function(vec1, vec2) { 
  nominator <- vec1 %*% vec2  # %*% specifies dot product rather than entry by entry multiplication (we could also do: sum(x * y))
  denominator <- sqrt(vec1 %*% vec1)*sqrt(vec2 %*% vec2)
  return(nominator/denominator)
}

# example 1
x <- c(1, 2, 3)
y <- c(1, 2, 3)

# what should we get?
calculate_cosine_similarity(x, y)

# example 2
a <- c(1, 2, 3)
b <- c(-1, -2, -3)

# what should we get?
calculate_cosine_similarity(a, b)

# Let's do it with texts
obama_text <- as.character(corpus_subset(data_corpus_inaugural, 
                                         President == "Obama"))
lincoln_text <- as.character(corpus_subset(data_corpus_inaugural, 
                                           President == "Lincoln"))

# Make a dfm of these two
obama_lincoln_dfm <- c(obama_text, lincoln_text) %>% 
  tokens() %>% 
  dfm() %>% 
  dfm_remove(stopwords("english")) %>% 
  dfm_wordstem()

# Calculate similarity
simil_obama_lincoln_wpp <- textstat_simil(obama_lincoln_dfm, 
                                          margin = "documents",
                                          method = "cosine")
as.matrix(simil_obama_lincoln_wpp)

# 6.2 Let's see how stopwords/stemming affect similarity

obama_lincoln_nopp <- c(obama_text, lincoln_text) %>% 
  tokens() %>% 
  dfm()

# Calculate similarity

simil_obama_lincoln_nopp <- textstat_simil(obama_lincoln_nopp, 
                                           margin = "documents", 
                                           method = "cosine")

as.matrix(simil_obama_lincoln_nopp)

# Make a dfm of several documents

several_inaug_dfm <- data_corpus_inaugural %>% 
  corpus_subset(Year > 1980) %>% 
  tokens() %>% 
  dfm() %>% 
  dfm_remove(stopwords("english")) %>% 
  dfm_wordstem()
  

# Specific comparisons with Obama's first inauguration speech

textstat_simil(several_inaug_dfm, 
               several_inaug_dfm[several_inaug_dfm@docvars$docname_=="2009-Obama",], 
               margin = "documents", 
               method = "correlation")

# Other options available: jaccard, etc.
?textstat_simil

# For distance metrics: textstat_dist
textstat_dist(several_inaug_dfm, 
               several_inaug_dfm[several_inaug_dfm@docvars$docname_=="2009-Obama",], 
               margin = "documents", 
               method = "manhattan")

# ============================================================================= #
####                                  STYLE                                  ####
# ============================================================================= #

rm(list = ls())

# Project Gutenberg: https://www.gutenberg.org/
###################################################
# collection of (machine readable) novels and other texts + they have an R package!
#install.packages("gutenbergr")
# for more info refer to: https://cran.r-project.org/web/packages/gutenbergr/vignettes/intro.html
library(gutenbergr)
gutenberg_works()

# what do they have by Jane Austen?
gutenberg_works() %>% filter(author == "Austen, Jane")

# download "Emma"
emma <- gutenberg_download(gutenberg_id = 158)
#emma <- gutenberg_download(jane_austen$gutenberg_id[jane_austen$title == "Emma"], 
# meta_fields = "title")  # add other meta information

# stylest package: estimate speaker (author) style distinctiveness (vis-a-vis other authors)
################################################################################################

# based on relative usage rate of terms --> posterior probability of authorship
# eta_{author,term} = log Pr(term|author)
# distinctiveness: expected value of log posterior odds ratio, i.e. E(Pr(author1|term) - Pr(author2|term))

# see vignette: https://leslie-huang.github.io/stylest/articles/stylest-vignette.html
# paper using this package: https://doi.org/10.1017/pan.2019.49 
#install.packages("stylest")
# source for this code: package vignette
library(stylest)

# data included in package
data(novels_excerpts)

# author list
unique(novels_excerpts$author)

# note how the data is organized
str(novels_excerpts)

# (1) select most informative (discriminative) features: uses n-fold cross-validation to identify the set of terms 
# that maximizes the model’s rate of predicting the speakers of out-of-sample texts
# (subsets vocab by frequency percentile)
filter <- corpus::text_filter(drop_punct = TRUE, 
                              drop_number = TRUE)  # pre-processing choices
set.seed(1984L)  # why set seed?
vocab_custom <- stylest_select_vocab(novels_excerpts$text, 
                                     novels_excerpts$author,  # fits n-fold cross-validation
                                     filter = filter, 
                                     smooth = 1, nfold = 10,
                                     cutoff_pcts = c(25, 50, 60, 70, 80, 90))

vocab_custom$cutoff_pct_best  # percentile with best prediction rate
vocab_custom$miss_pct  # rate of incorrectly predicted speakers of held-out texts
apply(vocab_custom$miss_pct, 2, mean)

# (2) subset features
vocab_subset <- stylest_terms(novels_excerpts$text, 
                              novels_excerpts$author, 
                              vocab_custom$cutoff_pct_best , 
                              filter = filter) # USE SAME FILTER

# (3) fit model with "optimal" percentile threshold (i.e. feature subset)
style_model <- stylest_fit(novels_excerpts$text, 
                           novels_excerpts$author, 
                           terms = vocab_subset, 
                           filter = filter)
# explore output
str(style_model)
term_usage <- style_model$rate
authors <- unique(novels_excerpts$author)
lapply(authors, function(x) head(term_usage[x,][order(-term_usage[x,])])) %>% 
  setNames(authors)
# distinctiveness of each term for authors
head(stylest_term_influence(style_model, 
                            novels_excerpts$text, 
                            novels_excerpts$author))  # influential terms


# (4) predict speaker of a new text
new_text <- emma$text[500:600] %>% paste(., collapse = "") 
pred <- stylest_predict(style_model, new_text)
pred$predicted
pred$log_probs



# ============================================================================= #
####                            LEXICAL DIVERSITY                            ####
# ============================================================================= #

rm(list = ls())
# Load in data: Irish budget proposals from 2008-2012 ----
# "speeches and document-level variables from the debate over the Irish budget".

data("data_corpus_irishbudgets")
irish_budget_texts <- as.character(data_corpus_irishbudgets)

# TTR (by hand)
####################
budget_dfm <- tokens(irish_budget_texts, 
                        remove_punct = TRUE) %>% dfm()

# Num tokens per document
num_tokens <- ntoken(budget_dfm)

num_types <- ntype(budget_dfm)

irish_budget_TTR <- num_types / num_tokens

head(irish_budget_TTR)

# Would you expect the budgets to become more or less diverse over time?
# Calculate TTR score by year, party 
###########################################

# by year
# textstat_lexdiv: "calculates the lexical diversity or complexity of text(s)" using any number of measures.'
TTR <- textstat_lexdiv(budget_dfm, measure = "TTR",
                       remove_numbers = F, remove_punct = F, remove_symbols = F)
aggregate(TTR$TTR, 
          by = list(docvars(data_corpus_irishbudgets)$year), 
          FUN = mean, na.rm = TRUE) %>% 
  setNames(c("year", "TTR"))

aggregate(TTR$TTR, 
          by = list(docvars(data_corpus_irishbudgets)$party), 
          FUN = mean, na.rm = TRUE) %>% 
  setNames(c("party", "TTR"))

# Thoughts on TTR

# ============================================================================= #
####                            COMPLEXITY MEASURES                          ####
# ============================================================================= #

# FRE (https://en.wikipedia.org/wiki/Flesch–Kincaid_readability_tests)
#########################################################################

textstat_readability(data_corpus_irishbudgets, "Flesch") %>% head()

corpus_y <- corpus_group(data_corpus_irishbudgets, groups = docvars(data_corpus_irishbudgets)$year)
textstat_readability(corpus_y, "Flesch") 

corpus_p <- corpus_group(data_corpus_irishbudgets, groups = docvars(data_corpus_irishbudgets)$party)
textstat_readability(corpus_p, "Flesch")

# Dale-Chall measure (https://en.wikipedia.org/wiki/Dale–Chall_readability_formula)
#######################################################################################

textstat_readability(data_corpus_irishbudgets, "Dale.Chall.old") %>% head()

textstat_readability(corpus_y, "Dale.Chall.old")

textstat_readability(corpus_p, measure = "Dale.Chall.old")

# let's compare each measure
#####################################

all_readability_measures <- textstat_readability(data_corpus_irishbudgets, 
                                                 c("Flesch", "Dale.Chall", 
                                                   "SMOG", "Coleman.Liau", 
                                                   "Fucks")) %>% 
  tidylog::drop_na()

readability_cor <- cor(all_readability_measures[,-1])
rownames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
colnames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
readability_cor

# ============================================================================= #
####                              BOOTSTRAPPING                              ####
# ============================================================================= #
# there are packages in R that help with bootstrapping: 
# e.g. https://cran.r-project.org/web/packages/boot/boot.pdf

# data prep: remove smaller parties (parties with only 1 document) & remove NA's
large_parties <- docvars(data_corpus_irishbudgets) %>% 
  group_by(party) %>% 
  tally() %>% 
  arrange(-n) %>% 
  filter(n > 1) %>% 
  select(party) %>% 
  unlist() %>% 
  unname()
irbudgetsCorpSub <- corpus_subset(data_corpus_irishbudgets, 
                                  (party %in% large_parties))
irbudgetsCorpSub <- irbudgetsCorpSub[-which(as.character(irbudgetsCorpSub)=="")]

# mean Flesch statistic per party
flesch_point <- irbudgetsCorpSub %>% 
  textstat_readability(measure = "Flesch") %>% 
  cbind(docvars(irbudgetsCorpSub)) %>% 
  group_by(party) %>% 
  summarise(mean_flesch = mean(Flesch)) %>% 
  setNames(c("party", "mean")) %>% 
  arrange(party)

# ggplot point estimate
ggplot(flesch_point, aes(x = party, y = mean, colour = party)) +
  geom_point() +
  coord_flip() + 
  theme_bw() + 
  scale_y_continuous(breaks=seq(floor(min(flesch_point$mean)), 
                                ceiling(max(flesch_point$mean)), 
                                by = 2)) +
  xlab("") + ylab("Mean Fleisch Score by Party") + 
  theme(legend.position = "none")

# We will use a loop to bootstrap a sample of documents and subsequently calculate standard errors
iters <- 10 # usually want to at least use 100

library(pbapply)
# build function to be used in bootstrapping
boot_flesch <- function(party_data){
  N <- nrow(party_data)
  bootstrap_sample <- sample_n(party_data, N, replace = TRUE)
  readability_results <- textstat_readability(bootstrap_sample$texts, measure = "Flesch")
  return(mean(readability_results$Flesch))
}

# apply function to each party
boot_flesch_by_party <- pblapply(large_parties, function(x){
  # subset corpus by party
  sub_data <- corpus_subset(irbudgetsCorpSub, party == x)
  # create data frame (easier to work with for bootstrapping)
  sub_data <- cbind(texts = as.character(sub_data),
                    docvars(sub_data)) %>% 
    as.data.frame()
  output_flesch <- lapply(1:iters, function(i) boot_flesch(sub_data))
  return(unlist(output_flesch))
})
names(boot_flesch_by_party) <- large_parties

# compute mean and std.errors
party_means <- lapply(boot_flesch_by_party, mean) %>% 
  unname() %>% 
  unlist()
party_ses <- lapply(boot_flesch_by_party, sd) %>% 
  unname() %>% 
  unlist() # bootstrap standard error = sample standard deviation bootstrap distribution

# Plot results--party
plot_dt <- tibble(party = large_parties, mean = party_means, ses = party_ses)

# confidence intervals
interval1 <- -qnorm((1-0.9)/2)   # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# ggplot point estimate + variance
ggplot(plot_dt, aes(colour = party)) +
  geom_linerange(aes(x = party, 
                     ymin = mean - ses*interval1, 
                     ymax = mean + ses*interval1), 
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = party, 
                      y = mean, 
                      ymin = mean - ses*interval2, 
                      ymax = mean + ses*interval2), 
                  lwd = 1/2, position = position_dodge(width = 1/2), 
                  shape = 21, fill = "WHITE") +
  coord_flip() + 
  theme_bw() + 
  scale_y_continuous(breaks=seq(floor(min(plot_dt$mean)), 
                                ceiling(max(plot_dt$mean)), 
                                by = 2)) +
  xlab("") + ylab("Mean Fleisch Score by Party") + 
  theme(legend.position = "none")

