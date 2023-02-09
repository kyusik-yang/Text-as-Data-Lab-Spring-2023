# TA: Elisa Wirsching
# Course: Text as Data
# Date: 9/02/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, Kevin Munger, Patrick Chester and Leslie Huang.

# other "similar" interesting packages: tm, tidytext

# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #

# 1.1 Workspace 
###################

# Clear Global Environment
rm(list = ls())

# 1.2 Installing quanteda
#################################

# Install the latest stable version of quanteda from CRAN
#install.packages("quanteda") # run this if you don't have quanteda already installed
library(quanteda)
# What version of quanteda do you have loaded? We will use the major new release quanteda 3.x
# How many threads (cores) are you using? See the printout in the console

pacman::p_load(dplyr,
               ggplot2,
               quanteda.textplots,
               quanteda.textstats,
               quanteda.corpora) 


# 1.3 Devtools and the quanteda corpus
#########################################

# Use devtools to install some sample data
#devtools::install_github("quanteda/quanteda.corpora")

# Load it into our environment
library(quanteda.corpora)

# Read about the data available: https://github.com/quanteda/quanteda.corpora

### Note: Quanteda is still under development so it is changing! 
# New features are being added but sometimes functions or function parameters are 
# deprecated or renamed. This includes the very basic functions in this code demo!
# For major changes in the last release see https://github.com/quanteda/quanteda/blob/master/NEWS.md#quanteda-30
# or see news(Version >= "2.9", package = "quanteda")

# This means that you may encounter many code examples, StackOverflow questions, 
# and websites with outdated documentation, etc. that include functions or options 
# that have been deprecated or renamed.

# 1.4 Managing dependencies
###################################

# If you want to ensure that your code for a project will not break when you update quanteda, 
# I recommend using a dependency manager for R called packrat so that you can specify 
# a dependency on a specific version of quanteda.
# Find out about setting up packrat here: https://rstudio.github.io/packrat/walkthrough.html

# 1.5 Versions of quanteda
#####################################

# to check version
packageVersion("quanteda")

# How would you get an older version of quanteda? 
# (For example, if you accidentally installed the dev version from GitHub but 
# you want to go back to the last stable release, or you want a legacy version 
# to support old code.)

# - Check the CRAN archive
# use the install_version function, e.g.:
# devtools::install_version("quanteda", version = "0.99.12", repos = "http://cran.us.r-project.org")

# If you want the latest dev version of quanteda, it's on GitHub, 
# but we will use the latest version from CRAN for stability/sanity reasons


# Concept review: Which of these are the same?
# token
# type
# feature
# word
# term

# ============================================================================= #
####                                CORPUS OBJECT                            ####
# ============================================================================= #

# quanteda's main input object is called a "corpus" 
# (a way of organizing text data: generally includes text + metadata)

# THERE ARE OTHER WAYS to organize text data
# TAKE A LOOK AT: https://www.tidytextmining.com/tidytext.html

# 1.1 load the State of the Union (SOTU) corpus and look at a summary 
##########################################################################

sotu <- data_corpus_sotu

# a corpus consists of: 
#   (1) documents: text + doc level data 
#   (2) corpus metadata 
#   (3) extras (settings)

head(docvars(sotu))  # document-level variables (called docvars)
dvars <- docvars(sotu)
meta(sotu)  # corpus-level variables

# ndoc identifies the number of documents in a corpus
ndocs <- ndoc(sotu)

# summary of the corpus (provides some summary statistics on the text combined with the metadata)
corpusinfo <- summary(sotu, n = ndocs)  # note n default is 100
head(corpusinfo)
class(corpusinfo)
# does tokens >= types always hold?

# quick visualization
token_plot <- ggplot(data = corpusinfo, 
                     aes(x = Date, 
                         y = Tokens, 
                         group = 1)) + 
  geom_line() + 
  geom_point() + 
  theme_bw()
token_plot

# 1.2 subset corpus 
########################
summary(corpus_subset(sotu, President == "Trump"))
trump_sotu <- corpus_subset(sotu, President == "Trump")
trump_sotu

# keep only the text of the the 2018 SOTU
trump_2018_text <- as.character(trump_sotu)[2]
# previous quanteda: texts(trump_sotu)[2]

# similar to
trump_2018_text <- trump_sotu[2] # note this is still a corpus object

# 1.3 Exploring corpus texts
####################################

# key words in context (KWIC)
# search for a word and allows us to view the contexts in which it occurs
kwic_america <- kwic(tokens(trump_sotu), 
                     pattern = "america", 
                     valuetype = "regex", 
                     window = 6)
head(kwic_america)

# with default options
kwic_america <- kwic(tokens(trump_sotu), pattern = "america")
head(kwic_america)


# we can also easily build our own corpus from text: http://quanteda.io/articles/quickstart.html#building-a-corpus-from-a-character-vector


# ============================================================================= #
####                            TOKENIZING & STEMMING                        ####
# ============================================================================= #

## 2.1 Tokenizing text 
############################
?tokens
# produces an intermediate object, consisting of a list of tokens in the form of 
# character vectors, where each element of the list corresponds to an input document.
tokenized_speech <- tokens(trump_2018_text)
str(tokenized_speech)
head(unname(unlist(tokenized_speech)), 20)

# alternative using only base R
tokenized_speech <- strsplit(trump_2018_text, " ")

# tokens() is deliberately conservative, meaning that it does not remove anything 
# from the text unless told to do so.
# remove punctuation when tokenizing
tokenized_speech <- tokens(trump_2018_text, remove_punct = TRUE)
head(unname(unlist(tokenized_speech)), 20)

# careful when working with non-English languages!

## 2.2 Stemming 
#########################
?tokens_wordstem
stemmed_speech <- tokens_wordstem(tokenized_speech)  # language is an argument
head(unname(unlist(stemmed_speech)), 20)

## 2.3 Ngrams 
################
?tokens_ngrams
tokenized_speech_ngrams <- tokens_ngrams(tokenized_speech)
head(unname(unlist(tokenized_speech_ngrams)), 20)
tail(unname(unlist(tokenized_speech_ngrams)), 20)

# we can also concatenate specific multi-word expressions and keep them as a single 
# feature in subsequent analyses
tokens("New York City is located in the United States.") %>%
  tokens_compound(pattern = phrase(c("New York City", "United States")))

## Types vs. Tokens
ntoken(trump_2018_text)
ntype(trump_2018_text)
tokens(trump_2018_text) %>% unlist() %>% unique() %>% length()



# ============================================================================= #
####                         DOCUMENT FEATURE MATRIX (~DTM)                  ####
# ============================================================================= #

# DOCUMENTS AS DISTRIBUTIONS
# We want to extract a matrix associating values for certain features with each document. 
# In quanteda, we use the dfm() function to produce such a matrix. 
# “dfm” is short for document-feature matrix, and always refers to documents in 
# rows and “features” as columns.

## 3.1 Creating a DFM 
############################
?dfm # note default options!
trump_2018_dfm <- tokens(trump_2018_text) %>% 
  dfm() 

# inspect the first few features
trump_2018_dfm[, 1:10]  # why 0% sparse?

# how many rows does this dfm have? 
dim(trump_2018_dfm)

# top features in dfm
topfeatures(trump_2018_dfm)

# Are all of these features relevant?
# Words?
# Punctuation (maybe!!! --> think what the goal is. Can theory help?)



# ============================================================================= #
####                        PREPROCESSING (~FEATURE ENGINEERING)             ####
# ============================================================================= #

# since last release: most preprocessing done prior to dfm 
?dfm  # see all options
# NOTE: lowercase argument is by default TRUE

# punctuation
trump_2018_dfm <- tokens(trump_2018_text, remove_punct = TRUE) %>% 
  dfm()
dim(trump_2018_dfm)
trump_2018_dfm[, 1:10]
topfeatures(trump_2018_dfm)

# stemming
trump_2018_dfm_s <- tokens(trump_2018_text, remove_punct = TRUE) %>% 
  tokens_wordstem() %>% 
  dfm()
dim(trump_2018_dfm_s)
trump_2018_dfm_s[, 1:10]

## 3.2 Stopwords 
#######################
# Stopwords are commonly words that (presumably) add little understanding to the 
# content of the document by themselves.
# The stopwords function takes a language as an input and produces a vector of 
# stopwords compiled from that language

stopwords("english")

# Fun fact: Quanteda also supports stopwords for english, SMART, danish, french, greek, hungarian, 
# norwegian, russian, swedish, catalan, dutch, finnish, german, italian, portuguese, spanish, and arabic

trump_2018_dfm_2 <- tokens(trump_2018_text, remove_punct = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("english"))

topfeatures(trump_2018_dfm)
topfeatures(trump_2018_dfm_2)

# wordclouds

textplot_wordcloud(trump_2018_dfm, max_words = 100)
textplot_wordcloud(trump_2018_dfm_2, max_words = 100)


# ============================================================================= #
####                        WEIGHTED DOCUMENT FEATURE MATRIX                 ####
# ============================================================================= #
# WHAT ARE WE WEIGHTING?

# Now we will create a DFM of all the SOTU speeches
full_dfm <- tokens(sotu, remove_punct = TRUE) %>% 
  dfm() %>% 
  dfm_remove(pattern = stopwords("english"))
head(full_dfm) # notice sparsity
topfeatures(full_dfm)
topfeatures(full_dfm[nrow(full_dfm),]) # for a specific document
topfeatures(full_dfm["Washington-1793",])

# 4.1 tfidf - Frequency weighting
####################################
?dfm_tfidf
weighted_dfm <- dfm_tfidf(full_dfm) # uses the absolute frequency of terms in each document
topfeatures(weighted_dfm)
topfeatures(weighted_dfm[nrow(weighted_dfm),])

# 4.2 tfidf - Relative frequency weighting
###############################################
normalized <- dfm_tfidf(full_dfm, scheme_tf = "prop") # Uses feature proportions within documents: divides each term by the total count of features in the document
topfeatures(normalized)
topfeatures(normalized[nrow(normalized),]) # why is ordering identical as frequency weighting?



# ============================================================================= #
####                                  COLLOCATIONS                           ####
# ============================================================================= #

# to identify usual multi-word expressions
# bigrams
?textstat_collocations
head(textstat_collocations(trump_2018_text))
textstat_collocations(trump_2018_text) %>% arrange(-lambda) %>% slice(1:5)

# If x is a tokens object and some tokens have been removed, this should be done 
# using [tokens_remove](x, pattern, padding = TRUE) so that counts will still be 
# accurate, but the pads will prevent those collocations from being scored.

# trigrams
head(textstat_collocations(trump_2018_text, size = 3)) %>% arrange(-lambda) %>% slice(1:5)

#ngrams up to trigrams
textstat_collocations(trump_2018_text, size=2:3) %>% arrange(-lambda)




# ============================================================================= #
####                            HANDLING OTHER LANGUAGES                     ####
# ============================================================================= #

# Most latin languages are easy to tokenize, since words can be segmented by 
# spaces (i.e. word boundaries). This is not true for logographic languages!
# We need a function that separates words even without the whitespace between them.
# Thanks to the stringi package and the integration of the ICU tokenizer,
# quanteda can handle MANY languages (all major languages in Unicode)

# Chinese example taken from: https://quanteda.io/articles/pkgdown/examples/chinese.html

# 6.1 Tokenizing Chinese text 
################################

corp <- quanteda.corpora::download(url = "https://www.dropbox.com/s/37ojd5knz1qeyul/data_corpus_chinesegovreport.rds?dl=1")

# Chinese stopwords
ch_stop <- stopwords("zh", source = "misc")
ch_stop

# tokenize
ch_toks <- corp %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = ch_stop)

# construct a dfm
ch_dfm <- dfm(ch_toks)
topfeatures(ch_dfm)

# load fonts for wordcloud

textplot_wordcloud(ch_dfm, min_count = 500, random_order = FALSE,
                   rotation = .25, max_words = 100,
                   min_size = 0.5, max_size = 2.8,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


# 6.2 Other languages
################################

corp <- data_corpus_udhr[c("eng", "deu_1996", "arb", "heb", "cmn_hans", "jpn")]
print(corp)

toks <- tokens(corp)
print(toks)

# check the quanteda tutorial for more examples: https://tutorials.quanteda.io/multilingual/



# ============================================================================= #
####                              REGULAR EXPRESSIONS                        ####
# ============================================================================= #
# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of
# cheatsheet for regex: 
#     https://github.com/rstudio/cheatsheets/blob/main/regex.pdf
#     https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf
# to test your regex expressions: https://regex101.com/ 
# or use stringr functions:

see <- function(string, rx) stringr::str_view_all(string, rx)
see("abc ABC 123\t.!?\\(){}\n", "a")
see("abc ABC 123\t.!?\\(){}\n", "\\.")
see("abc ABC 123\t.!?\\(){}\n", "[:digit:]")
see("abc ABC 123\t.!?\\(){}\n", "\\s")

# 7.1 Base R: grep package
################################
# grep: returns a vector of the indices of the elements of x that yielded a match
s_index <- grep(" s ", as.character(sotu))
head(s_index)
#see(as.character(sotu)[107], " s ")

thank_index <- grep("^Thank", as.character(sotu))
thank_index
see(as.character(sotu)[215], "^Thank")

# grepl: returns a logical vector (match or not for each element of x).
s_index <- grepl(" s ", as.character(sotu))
table(s_index)

# this returns every speech that contains " s " -- JUST THE LETTER S BY ITSELF
texts_with_s <- grep(" s ", as.character(sotu), value = TRUE)

# Here we create a vector of documents with " s " removed
texts_without_s <- gsub(" s ", "",  sotu)
texts_without_s[[107]] == sotu[[107]]
texts_without_s[[106]] == sotu[[106]]

# ALWAYS TEST FIRST
gsub(" s ", " ",  "hello how s are you")
grepl("^so", c("so today we", "never so today", "today never so"))


# 7.2 tidyverse's stringr (very flexibel!)
################################

# SUGGESTED PACKAGE to deal with regular expressions
library(stringr)
# https://github.com/rstudio/cheatsheets/blob/main/strings.pdf

# Detect a pattern 
str_detect(sotu, pattern = " s ")

# Extract first word
word(sotu, start = 1)

# Extract string before certain character
str_extract(sotu[[1]], "^(.+)(?= Representatives)") # excluding pattern
str_extract(sotu[[1]], "^(.+)Representatives") # including pattern

# in general for these problems: Google is your friend!



# ============================================================================= #
####                              PREPROCESSING CHOICES                      ####
# ============================================================================= #

#devtools::install_github("matthewjdenny/preText")

library(preText)

# Run at home (takes a few minutes to run)
# Example below taken from preText vignette: http://www.mjdenny.com/getting_started_with_preText.html

preprocessed_documents <- factorial_preprocessing(
  sotu[1:50],
  use_ngrams = FALSE,
  infrequent_term_threshold = 0.2,
  verbose = TRUE)

head(preprocessed_documents$choices)

preText_results <- preText(preprocessed_documents,
                           dataset_name = "SOTU Speeches",
                           distance_method = "cosine",
                           num_comparisons = 20,
                           verbose = TRUE)


preText_score_plot(preText_results)



# Questions?

# I recommend you check out: https://quanteda.io/articles/quickstart.html

