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
               quanteda.textstats) 

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

# x-axis: log of ranks 1 through 100
# y-axis log of frequency of top 100 terms from the DFM

plot(log10(1:100), log10(topfeatures(inaug_dfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", 
     main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus")

# Fits a linear regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(inaug_dfm, 100)) ~ log10(1:100))

# Adds the fitted line from regression to the plot
abline(regression, col = "red")

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
confint(regression)
summary(regression)

# Zipf's law as a feature selection tool (e.g. http://www.jmlr.org/papers/volume3/forman03a/forman03a_full.pdf)

plot(1:100, topfeatures(inaug_dfm, 100),
     xlab = "rank", ylab = "frequency", 
     main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus")

plot(1:100, topfeatures(mydfm, 100),
     xlab = "rank", ylab = "frequency", 
     main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus (w/o stopwords)")



# ============================================================================= #
####                            KEYWORDS IN CONTEXT                          ####
# ============================================================================= #
## good way to summarize info about a topic

data_corpus_inaugural %>% 
  tokens() %>% 
  kwic("America", 3, case_insensitive = FALSE)

help(kwic)

# Suggested terms?



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

# Other options available: Manhattan distance, cosine, etc.
?textstat_simil

# ============================================================================= #
####                                  STYLE                                  ####
# ============================================================================= #

# 7.1 data collection (to be used in HW1)
rm(list = ls())
# 7.1 Project Gutenberg: http://www.gutenberg.org/wiki/Main_Page
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

# 7.2 stylest package: estimate speaker (author) style distinctiveness (vis-a-vis other authors)
# see vignette: https://github.com/leslie-huang/stylest/blob/master/vignettes/stylest-vignette.md
# early draft version of paper using this package: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3235506
#install.packages("stylest")
# source for this code: package vignette
library(stylest)

# data included in package
data(novels_excerpts)

# author list
unique(novels_excerpts$author)

# note how the data is organized
str(novels_excerpts)

# (1) select most informative (discriminative) features 
# (subsets vocab by frequency percentile)
filter <- corpus::text_filter(drop_punct = TRUE, 
                              drop_number = TRUE)  # pre-processing choices
set.seed(1984L)  # why set seed?
vocab_custom <- stylest_select_vocab(novels_excerpts$text, 
                                     novels_excerpts$author,  # fits n-fold cross-validation
                                     filter = filter, 
                                     smooth = 1, nfold = 10,
                                     cutoff_pcts = c(25, 50, 75, 99))

vocab_custom$cutoff_pct_best  # percentile with best prediction rate
vocab_custom$miss_pct  # rate of incorrectly predicted speakers of held-out texts

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
head(stylest_term_influence(style_model, 
                            novels_excerpts$text, 
                            novels_excerpts$author))  # influential terms

str(style_model)
authors <- unique(novels_excerpts$author)
term_usage <- style_model$rate
lapply(authors, function(x) head(term_usage[x,][order(-term_usage[x,])])) %>% 
  setNames(authors)

# (4) predict speaker of a new text
emma <- novels_excerpts %>% 
  filter(title == "Emma")
new_text <- emma$text[30:75] %>% paste(., collapse = "") 
pred <- stylest_predict(style_model, new_text)
pred$predicted
pred$log_probs



# ============================================================================= #
####                            LEXICAL DIVERSITY                            ####
# ============================================================================= #

# Load in data: Irish budget proposals from 2008-2012 ----
# "speeches and document-level variables from the debate over the Irish budget".

data("data_corpus_irishbudgets")
irish_budget_texts <- texts(data_corpus_irishbudgets)

# TTR 
#############
budget_tokens <- tokens(irish_budget_texts, 
                        remove_punct = TRUE) 

# Num tokens per document
num_tokens <- lengths(budget_tokens)

num_types <- ntype(budget_tokens)

irish_budget_TTR <- num_types / num_tokens

head(irish_budget_TTR)

# Would you expect the budgets to become more or less diverse over time?
# Mean per-document TTR scores by year, party
####################################################

TTR_by_year <- aggregate(irish_budget_TTR, 
                         by = list(data_corpus_irishbudgets[["year"]]$year), 
                         FUN = mean, na.rm = TRUE) %>% 
  setNames(c("year", "TTR"))

plot(TTR_by_year)

aggregate(irish_budget_TTR, 
          by = list(data_corpus_irishbudgets[["party"]]$party), 
          FUN = mean) %>% 
  setNames(c("party", "TTR"))


# Calculate TTR score by year, party 
###########################################

# by year
# textstat_lexdiv: "calculates the lexical diversity or complexity of text(s)" using any number of measures.'
TTR <- textstat_lexdiv(budget_tokens, measure = "TTR")
aggregate(TTR$TTR, 
          by = list(data_corpus_irishbudgets[["year"]]$year), 
          FUN = mean, na.rm = TRUE) %>% 
  setNames(c("year", "TTR"))

# Sidebar: using the "groups" parameter is how to group documents by a covariate -- note how this changes the ndocs of your corpus
aggregate(TTR$TTR, 
          by = list(data_corpus_irishbudgets[["party"]]$party), 
          FUN = mean, na.rm = TRUE) %>% 
  setNames(c("party", "TTR"))

# Thoughts on TTR

# ============================================================================= #
####                            COMPLEXITY MEASURES                          ####
# ============================================================================= #

# FRE (https://en.wikipedia.org/wiki/Flesch–Kincaid_readability_tests)
#########################################################################

textstat_readability(data_corpus_irishbudgets, "Flesch") %>% head()

textstat_readability(texts(data_corpus_irishbudgets, groups = "year"), "Flesch") 

textstat_readability(texts(data_corpus_irishbudgets, groups = "party"), "Flesch")

# Dale-Chall measure (https://en.wikipedia.org/wiki/Dale–Chall_readability_formula)
#######################################################################################

textstat_readability(data_corpus_irishbudgets, "Dale.Chall.old") %>% head()

textstat_readability(texts(data_corpus_irishbudgets, 
                           groups = "year"), "Dale.Chall.old")

textstat_readability(texts(data_corpus_irishbudgets, 
                           groups = "party"), measure = "Dale.Chall.old")

# let's compare each measure
#####################################

all_readability_measures <- textstat_readability(data_corpus_irishbudgets, 
                                                 c("Flesch", "Dale.Chall", 
                                                   "SMOG", "Coleman.Liau", 
                                                   "Fucks"))

readability_matrix <- cbind(all_readability_measures$Flesch, 
                            all_readability_measures$Dale.Chall, 
                            all_readability_measures$SMOG, 
                            all_readability_measures$Coleman.Liau, 
                            all_readability_measures$Fucks)

readability_cor <- cor(readability_matrix)
rownames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
colnames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
readability_cor

# ============================================================================= #
####                              BOOTSTRAPPING                              ####
# ============================================================================= #
# there are packages in R that help with bootstrapping: 
# e.g. https://cran.r-project.org/web/packages/boot/boot.pdf

# data prep: remove smaller parties (parties with only 1 document)
large_parties <- data_corpus_irishbudgets$documents %>% 
  group_by(party) %>% 
  tally() %>% 
  arrange(-n) %>% 
  filter(n > 1) %>% 
  select(party) %>% 
  unlist() %>% 
  unname()
irbudgetsCorpSub <- corpus_subset(data_corpus_irishbudgets, 
                                  (party %in% large_parties))

# convert corpus to df 
irbudgets_df <- irbudgetsCorpSub$documents %>% 
  select(texts, party, year) %>% 
  mutate(year = as.integer(year))

# Let's filter out any NAs
irbudgets_df <- na.omit(irbudgets_df)

# mean Flesch statistic per party
flesch_point <- irbudgets_df$texts %>% 
  textstat_readability(measure = "Flesch") %>% 
  group_by(irbudgets_df$party) %>% 
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

# We will use a loop to bootstrap a sample of texts and subsequently calculate standard errors
iters <- 10

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
  sub_data <- irbudgets_df %>% filter(party == x)
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

