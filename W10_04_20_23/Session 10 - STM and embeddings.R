# TA: Elisa Wirsching
# Course: Text as Data
# Date: 04/20/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, and Leslie Huang.


# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #

# Loading packages

rm(list = ls())
set.seed(1234)

pacman::p_load(topicmodels,
               dplyr,
               quanteda)


# Supervised vs. Unsupervised
# topic-models: excellent for exploration
# supervised exploration of topics: classification on text snippets with keywords 
# semi-supervised approaches: https://github.com/gregversteeg/corex_topic
# see: https://medium.com/pew-research-center-decoded/overcoming-the-limitations-of-topic-models-with-a-semi-supervised-approach-b947374e0455


# ============================================================================= #
####                          STRUCTURAL TOPIC MODELS                        ####
# ============================================================================= #

library(stm)
# Loading data: Political blogs from the 2008 election on a conservative-liberal dimension
data(poliblog5k)
head(poliblog5k.meta)
head(poliblog5k.voc)
head(poliblog5k.docs)

# Fits an STM model with 3 topics
?stm
blog_stm <- stm(poliblog5k.docs, poliblog5k.voc, 3, 
                prevalence = ~rating + s(day), 
                data = poliblog5k.meta)

# A plot that summarizes the topics by what words occur most commonly in them
plot(blog_stm, type = "labels")

# A summary plot of the topics that ranks them by their average proportion in the corpus
plot(blog_stm, type = "summary")

# A visualization of what words are shared and distinctive to two topics
plot(blog_stm, type="perspectives", topics = c(1,2))

# Estimates a regression with topics as the dependent variable and metadata as the independent variables
# s() is a wrapper for bs() from the splines package
# A spline of degree D is a function formed by connecting polynomial segments of degree D
?estimateEffect
prep <- estimateEffect(1:3 ~ rating + s(day) , 
                       blog_stm, 
                       nsims = 25,
                       meta = poliblog5k.meta)

# Plots the distribution of topics over time
plot(prep, 
     "day", 
     blog_stm, 
     topics = c(1,2), 
     method = "continuous", 
     xaxt = "n", 
     xlab = "Date")

# Plots the Difference in coverage of the topics according to liberal or conservative ideology
plot(prep, 
     "rating", 
     model = blog_stm,
     method = "difference", 
     cov.value1 = "Conservative", 
     cov.value2 = "Liberal")



# ============================================================================= #
####                              WORD EMBEDDINGS                            ####
# ============================================================================= #

# Are word embeddings supervised or unsupervised?
# KEY DIFFERENCE between embeddings and other distributional semantic models we've seen: how we define context.
# Context in the case of word embeddings is defined by a window (usually symmetric) around the target word.
# GloVe vs. Word2Vec
# cool/intuitive intro to W2V: http://mccormickml.com/2016/04/19/word2vec-tutorial-the-skip-gram-model/

library(text2vec)

# choice parameters
WINDOW_SIZE <- 6
DIM <- 300
ITERS <- 10
MIN_COUNT <- 10

# load data
corp <- conText::cr_sample_corpus


#---------------------------------
# prepare data
#---------------------------------

# tokenizer as usual
toks <- tokens(corp, 
               remove_punct=T, 
               remove_symbols=T, 
               remove_numbers=T, 
               remove_separators=T) 
  
# only use features that appear at least X times in the corpus
feats <- dfm(toks, tolower=T, verbose = FALSE) %>% 
  dfm_trim(min_termfreq = MIN_COUNT) %>% 
  featnames()

# leave the pads so that non-adjacent words will not become adjacent
toks_feats <- tokens_select(toks,
                            feats,
                            padding = TRUE)
head(toks_feats)
  

#---------------------------------
# estimate glove model
#---------------------------------

# original paper: https://nlp.stanford.edu/pubs/glove.pdf

# construct the feature co-occurrence matrix for our tokens object
toks_fcm <- fcm(toks_feats, 
                context = "window", 
                window = WINDOW_SIZE, 
                count = "frequency", 
                tri = FALSE) # important to set tri = FALSE

head(toks_fcm)

# estimate glove model using text2vec
glove <- GlobalVectors$new(rank = DIM, 
                           x_max = 10,
                           learning_rate = 0.05)
wv_main <- glove$fit_transform(toks_fcm, 
                               n_iter = ITERS,
                               convergence_tol = 1e-3, 
                               n_threads = 2) # set to 'parallel::detectCores()' to use all available cores



#---------------------------------
# get output
#---------------------------------

# Note that model learns two sets of word vectors - main and context. 
dim(wv_main)
word_vectors_context <- glove$components

# While both of word-vectors matrices can be used as result it usually better 
# (idea from GloVe paper) to average or take a sum of main and context vector:
word_vectors <- wv_main + t(word_vectors_context) # word vectors

# features?
head(rownames(word_vectors))

# pretrained GLoVE embeddings
# download this from Brightspace for your homework
pretrained <- readRDS("../hw/HW3/data/glove.rds") # GloVe pretrained (https://nlp.stanford.edu/projects/glove/)
dim(pretrained)

# function to compute nearest neighbors
nearest_neighbors <- function(cue, embeds, N = 5, norm = "l2"){
  cos_sim <- sim2(x = embeds, y = embeds[cue, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # cue is always the nearest neighbor hence dropped
}

# e.g. 
nearest_neighbors("state", word_vectors, N = 10, norm = "l2")
nearest_neighbors("state", pretrained, N = 10, norm = "l2")

nearest_neighbors("welfare", word_vectors, N = 10, norm = "l2")
nearest_neighbors("welfare", pretrained, N = 10, norm = "l2")

nearest_neighbors("cat", word_vectors, N = 10, norm = "l2")
nearest_neighbors("cat", pretrained, N = 10, norm = "l2")

nearest_neighbors("street", word_vectors, N = 10, norm = "l2")
nearest_neighbors("street", pretrained, N = 10, norm = "l2")
