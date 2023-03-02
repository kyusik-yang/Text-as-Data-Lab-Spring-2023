# TA: Elisa Wirsching
# Course: Text as Data
# Date: 02/03/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, Kevin Munger, Patrick Chester and Leslie Huang.


# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #
# clear global environment
rm(list = ls())

# load required libraries
pacman::p_load(quanteda,
               quanteda.corpora,
               readtext,
               quanteda.textmodels,
               dplyr)

# ============================================================================= #
####                              NAIVE BAYES                                ####
# ============================================================================= #

#source of data: https://www.kaggle.com/rmisra/news-category-dataset#News_Category_Dataset_v2.json

# load data
news_data <- readRDS("W5_03_02_23/news_data.rds")
head(news_data)

# subset data and keep relevant variables
news_samp <- news_data %>% 
  filter(category %in% c("CRIME", "SPORTS")) %>% 
  select(headline, category) %>% 
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "CRIME"])
head(news_samp$text[news_samp$class == "SPORTS"])

# some pre-processing (the rest will let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
head(news_samp$text[news_samp$class == "SPORTS"])

# what's the distribution of classes?
prop.table(table(news_samp$class))

# split sample into training & test sets
# why are we doing this?
set.seed(1984L)
prop_train <- 0.8
ids <- 1:nrow(news_samp)
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)
ids_test <- ids[-ids_train]
train_set <- news_samp[ids_train,]
test_set <- news_samp[ids_test,]

# get dfm for each set
train_dfm <- tokens(train_set$text, remove_punct = TRUE) %>% 
  dfm() %>% 
  dfm_wordstem() %>% 
  dfm_remove(stopwords("en")) 
test_dfm <- tokens(test_set$text, remove_punct = TRUE) %>% 
  dfm() %>% 
  dfm_wordstem() %>% 
  dfm_remove(stopwords("en")) 

# how does this look?
as.matrix(train_dfm)[1:5,1:5]
# Are the features of these two DFMs necessarily the same? Yes/No Why?

# match test set dfm to train set dfm features
#?dfm_match
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))


# w/o smoothing 
# -------------------

# train model on the training set
nb_model <- textmodel_nb(train_dfm, 
                         train_set$class, 
                         smooth = 0, 
                         prior = "uniform")

# evaluate on test set
predicted_class <- predict(nb_model, newdata = test_dfm)

# baseline
baseline_acc <- max(prop.table(table(test_set$class)))

# get confusion matrix
cmat <- table(test_set$class, predicted_class)
cmat
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)


# w smoothing 
# -------------------

# train model on the training set using Laplace smoothing
nb_model_sm <- textmodel_nb(train_dfm, 
                            train_set$class, 
                            smooth = 1, 
                            prior = "uniform")

# evaluate on test set
predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm)

# get confusion matrix
cmat_sm <- table(test_set$class, predicted_class_sm)#
cmat_sm
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) # recall = TP / (TP + FN)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) # precision = TP / (TP + FP)
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)

# take a look at the most discriminant features (get some face validity)
posterior <- tibble(feature = colnames(nb_model_sm$param), 
                    post_CRIME = t(nb_model_sm$param)[,1],
                    post_SPORTS = t(nb_model_sm$param)[,2])

posterior %>% arrange(-post_SPORTS) %>% head(10)
posterior %>% arrange(-post_CRIME) %>% head(10)

# what does smoothing do? 
# Imposes a uniform prior on term frequency (each term occurs once for each class)
plot(nb_model$param[1,], nb_model_sm$param[1,], 
     xlim = c(0,0.005),
     ylim = c(0,0.005),
     xlab="No Smooth", 
     ylab="Smooth") + 
  abline(a = 0, b = 1, col = "red")





# ============================================================================= #
####                              WORD SCORES                                ####
# ============================================================================= #
# Read in conservative and labour manifestos
filenames <- list.files(path = "W5_03_02_23/cons_labour_manifestos")

# Party name and year are in the filename -- we can use regex to extract these to use as our docvars
party <- unlist(regmatches(unlist(filenames), gregexpr("^[[:alpha:]]{3}", unlist(filenames))))
year <- unlist(regmatches(unlist(filenames), gregexpr("[[:digit:]]+", unlist(filenames))))

# This is how you would make a corpus with docvars from this data
cons_labour_manifestos <- corpus(readtext("W5_03_02_23/cons_labour_manifestos/*.txt"))
docvars(cons_labour_manifestos, field = c("party", "year") ) <- data.frame(cbind(party, year))

# But we're going to use a dataframe
cons_labour_df <- tibble(text = as.character(cons_labour_manifestos),
                         party = party,
                         year = as.integer(year))
colnames(cons_labour_df)

# keep vars of interest
cons_labour_df <- cons_labour_df %>% 
  select(text, party) %>% 
  setNames(c("text", "class"))

# what's the class distribution?
prop.table(table(cons_labour_df$class))

# randomly sample a test speech
set.seed(1984L)
ids <- 1:nrow(cons_labour_df)
ids_test <- sample(ids, 5, replace = FALSE)
ids_train <- ids[-ids_test]
train_set <- cons_labour_df[ids_train,]
test_set <- cons_labour_df[ids_test,]

# create DFMs
train_dfm <- tokens(train_set$text, remove_punct = TRUE) %>% 
  dfm() %>% 
  dfm_remove(stopwords("en"))
test_dfm <- tokens(test_set$text, remove_punct = TRUE) %>% 
  dfm() %>% 
  dfm_remove(stopwords("en"))

# Word Score model w/o smoothing 
# ------------------------------------
ws_base <- textmodel_wordscores(train_dfm, 
                                y = (2 * as.numeric(train_set$class == "Lab")) - 1 
                                # Y variable must be coded on a binary x in {-1,1} scale, so -1 = Conservative and 1 = Labour
)
summary(ws_base)
coef(ws_base)

# Look at strongest features
lab_features <- sort(ws_base$wordscores, decreasing = TRUE)  # for labor
lab_features[1:10]

con_features <- sort(ws_base$wordscores, decreasing = FALSE)  # for conservative
con_features[1:10]

# Can also check the score for specific features
ws_base$wordscores[c("drugs", "minorities", "unemployment")]

# predict that last speech
test_set$class
(pred_ws <- predict(ws_base, newdata = test_dfm,
        rescaling = "none", level = 0.95, se.fit = T)) 
textplot_scale1d(pred_ws)

# Word Score model w smoothing 
# --------------------------------------
# https://quanteda.io/reference/dfm_weight.html

ws_sm <- textmodel_wordscores(train_dfm, 
                              y = (2 * as.numeric(train_set$class == "Lab")) - 1, # Y variable must be coded on a binary x in {-1,1} scale, so -1 = Conservative and 1 = Labour
                              smooth = 1
)

# Look at strongest features
lab_features_sm <- sort(ws_sm$wordscores, decreasing = TRUE)  # for labor
lab_features_sm[1:10]

con_features_sm <- sort(ws_sm$wordscores, decreasing = FALSE)  # for conservative
con_features_sm[1:10]

# predict that last speech
test_set$class
(pred_ws <- predict(ws_sm, newdata = test_dfm,
                    rescaling = "none", level = 0.95, se.fit = T)) 
textplot_scale1d(pred_ws)

# Smoothing  
plot(ws_base$wordscores, ws_sm$wordscores, xlim=c(-1, 1), ylim=c(-1, 1),
     xlab="No Smooth", ylab="Smooth")


# ============================================================================= #
####  APPLYING NAIVE BAYES AND WORD SCORES TO AMICUS TEXTS FROM EVANS ET AL  ####
# ============================================================================= #
rm(list = ls())

# Texts of amicus curiae briefs labelled as being either pro-petitioner or 
# pro-respondent in US Supreme court cases on affirmative action, 
# Bakke (1978) and Bollinger (2008), taken from Evans et al (2007).
# https://rdrr.io/github/quanteda/quanteda.data/man/data_corpus_amicus.html

# Loading data
data("data_corpus_amicus")
head(data_corpus_amicus)

# create dfm & look at data
amicus_dfm <- tokens(data_corpus_amicus) %>% 
  dfm()
head(amicus_dfm)
head(docvars(amicus_dfm), 10)
table(docvars(amicus_dfm)$testclass)

# naive bayes model 
# -----------------------

# train NB model
amNBmodel <- textmodel_nb(amicus_dfm, 
                          docvars(data_corpus_amicus, "trainclass")) 

# predict class label of test set
amNBpredict <- predict(amNBmodel)

# "confusion matrix": Naive Bayes
nb_cmat <- table(docvars(data_corpus_amicus, "testclass"), amNBpredict)
nb_cmat

# baseline accuracy
baseline_acc <- max(prop.table(table(docvars(data_corpus_amicus, "testclass"))))
baseline_acc

# get scores
nb_acc <- sum(diag(nb_cmat))/sum(nb_cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- nb_cmat[2,2]/sum(nb_cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- nb_cmat[2,2]/sum(nb_cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)

# wordscore model 
# --------------------

# create reference texts (1 for petitioner, -1 for respondent)
reference <- c(1, 1, -1, -1, rep(NA, 98)) # class labels

# train ws model
amWSmodel <- textmodel_wordscores(amicus_dfm, reference, smooth = 1)

# plot nb and ws scores
plot(amWSmodel$wordscores, c(1, -1) %*% amNBmodel$param, 
     xlab="Wordscore", ylab = "Linear Posterior Class Pr. Diff")

# let's look at predictions from our wordscores model
amWSpredict <- predict(amWSmodel)
amWSresults <- ifelse(amWSpredict > 0, "P", "R")

# "confusion matrix": wordscores
ws_cmat <- table(docvars(data_corpus_amicus, "testclass"), amWSresults)

# get scores
ws_acc <- sum(diag(ws_cmat))/sum(ws_cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
ws_recall <- ws_cmat[2,2]/sum(ws_cmat[2,]) # recall = TP / (TP + FN)
ws_precision <- ws_cmat[2,2]/sum(ws_cmat[,2]) # precision = TP / (TP + FP)
ws_f1 <- 2*(ws_recall*ws_precision)/(ws_recall + ws_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  ws_acc, "\n",
  "Recall:",  ws_recall, "\n",
  "Precision:",  ws_precision, "\n",
  "F1-score:", ws_f1
)

