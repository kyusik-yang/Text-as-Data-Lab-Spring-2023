# TA: Elisa Wirsching
# Course: Text as Data
# Date: 03/09/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, Kevin Munger, Patrick Chester and Leslie Huang.


# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #
# clear global environment
rm(list = ls())

set.seed(1234)

# EXCELLENT DOCUMENTATION https://topepo.github.io/caret/index.html

# load required libraries
pacman::p_load(dplyr,
               caret,
               quanteda)

# ============================================================================= #
####                        LOAD, CLEAN & INSPECT DATA                       ####
# ============================================================================= #
news_data <- readRDS("W6_03_09_23/news_data.rds")
table(news_data$category)

# let's work with 2 categories
news_samp <- news_data %>% filter(category %in% c("WEIRD NEWS", "GOOD NEWS")) %>% 
  select(headline, category) %>% setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "WEIRD NEWS"])
head(news_samp$text[news_samp$class == "GOOD NEWS"])

# some pre-processing (the rest will let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- recode(news_samp$class,  "WEIRD NEWS" = "weird", "GOOD NEWS" = "good")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
set.seed(1984)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL


# ============================================================================= #
####                  Support Vector Machine (SVM) using Caret               ####
# ============================================================================= #

# create document feature matrix
news_dfm <- tokens(news_samp$text, remove_punct = T) %>% 
  dfm() %>% 
  dfm_remove(stopwords("en")) %>% 
  dfm_wordstem() %>% convert("matrix")
dim(news_dfm)

# A. the caret package has its own partitioning function
ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options
trctrl <- trainControl(method = "none") #none: only fits one model to the entire training set

# C. train model (caret gives us access to many more options)
# see: https://topepo.github.io/caret/available-models.html

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)
# what do you think the warning message means?

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
svm_linear_cmat

# let's look at the SVM weights for our features
coefs <- svm_mod_linear$finalModel@coef[[1]]
mat <- svm_mod_linear$finalModel@xmatrix[[1]]

temp <- t(coefs %*% mat) 
head(temp[order(temp[,1]),], 10)
head(temp[order(-temp[,1]),], 10)

# svm - radial
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = test_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, test_y)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)


# ============================================================================= #
####                        Example with Cross Validation                    ####
# ============================================================================= #

# https://topepo.github.io/caret/model-training-and-tuning.html

# now we will tune our hyper parameters using cross-validation

trctrl <- trainControl(method = "cv",
                       number = 5)

# we need to define a grid of possible parameter values to search over
tunegrid <- expand.grid(.C = seq(from = 0.1, to = 5.1, by = 0.5))

# Also available: Leave One Out CV
#trctrl <- trainControl(method = "LOOCV", p = 0.8)

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        tuneGrid = tunegrid,
                        trControl = trctrl)
print(svm_mod_linear)
plot(svm_mod_linear)

# predict on heldout validation data
svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)

# confusion matrix on predictions
svm_linear_cmat

#----------------------------------------

trctrl <- trainControl(method = "cv",
                       number = 3)

# svm - radial #this takes a long time to run!
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = test_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, test_y)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)
