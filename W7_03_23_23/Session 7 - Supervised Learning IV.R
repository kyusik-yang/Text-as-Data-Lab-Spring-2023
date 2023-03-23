# TA: Elisa Wirsching
# Course: Text as Data
# Date: 03/23/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, Kevin Munger, Patrick Chester and Leslie Huang.


# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #
# clear global environment
rm(list = ls())

# load required libraries
pacman::p_load(dplyr,
               quanteda,
               randomForest,
               mlbench,
               caret,
               pbapply)

# ============================================================================= #
####                        LOAD, CLEAN & INSPECT DATA                       ####
# ============================================================================= #

news_data <- readRDS("W6_03_09_23/news_data.rds")
table(news_data$category)

# let's work with 2 categories
# remember the order of operations matters! We first select category, group by, and then sample 500 obs
set.seed(1234)
news_samp <- news_data %>% 
  filter(category %in% c("MONEY", "LATINO VOICES")) %>% 
  group_by(category) %>%
  sample_n(500) %>%  # sample 250 of each to reduce computation time (for lab purposes)
  ungroup() %>%
  select(headline, category) %>% 
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "MONEY"])
head(news_samp$text[news_samp$class == "LATINO VOICES"])

# some pre-processing (the rest we'll let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- recode(news_samp$class,  "MONEY" = "money", "LATINO VOICES" = "latino")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
set.seed(1984)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL


# ============================================================================= #
####                                  PREPARE DATA                           ####
# ============================================================================= #

# create document feature matrix, actually a MATRIX object this time!
# keep tokens that appear in at least 5 headlines
news_dfm <- tokens(news_samp$text, remove_punct = T) %>% 
  dfm() %>% 
  dfm_remove(stopwords("en")) %>% 
  dfm_wordstem() %>% 
  dfm_trim(min_termfreq = 5) %>% 
  convert("matrix")


ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels


# ============================================================================= #
####                                RANDOM FOREST                            ####
# ============================================================================= #

mtry <- sqrt(ncol(train_x))  # number of features to sample at each split
# ASIDE: how would we call an algorithm with ncol(train_x) instead of sqrt(ncol(train_x))?

ntree <- 51  # num of trees to grow
# more trees generally improve accuracy but at the cost of computation time
# odd numbers avoid ties (recall default aggregation is "majority voting")
set.seed(1984)
system.time(rf.base <- randomForest(x = train_x, 
                                    y = train_y, 
                                    ntree = ntree, 
                                    mtry = mtry, 
                                    importance = TRUE))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])

# print results
print(rf.base)

# plot importance
# gini impurity = how "pure" is given node ~ class distribution
# = 0 if all instances the node applies to are of the same class
# upper bound depends on number of instances
varImpPlot(rf.base, n.var = 10, main = "Variable Importance")

# Out of sample performance
?predict.randomForest

predict_test <- predict(rf.base, newdata = test_x)
confusionMatrix(data = predict_test, reference = test_y)


# ============================================================================= #
####                            5-FOLD CV RF - CARET                         ####
# ============================================================================= #

### !!!!!!!!!!!! ###
# follow-up from last time
# how is the CV best model selected?

# QUESTION: how do we define the values for hyperparameters to optimize over?
# ANSWER: we actually need to define a range of values! otherwise caret keeps the value fixed at its default

# QUESTION: what metric does caret use?
# ANSWER: "best" function used to select the optimal tuning parameter can be defined; 
# best simply chooses the tuning parameter associated with the largest (or lowest for "RMSE") performance.
?best

#---------------------------------------------------------------------
# first, let's use a random search for the mtry tuning parameter
#---------------------------------------------------------------------

# note that the RF model in caret calls randomForest, but it's wrapped in caret

trainControl <- trainControl(method = "cv", 
                             number = 5,
                             search = 'random')
metric <- "Accuracy"
mtry <- sqrt(ncol(train_x))
ntree <- 51  
set.seed(1984) # why are we setting this again?
system.time(rf.caret <- train(x = train_x, y = train_y, 
                              method = "rf", 
                              metric = metric, 
                              trControl = trainControl,
                              tuneLength = 5, # lower for lab purposes
                              ntree = ntree)
)

# print results
print(rf.caret)
plot(rf.caret)

# test performance
rf_predict <- predict(rf.caret, newdata = test_x)
confusionMatrix(rf_predict, reference = test_y)

# plot importance
varImpPlot(rf.caret$finalModel, n.var = 10, main = "Variable Importance")


#---------------------------------------------------------------------
# now, let's use a grid search over mtry
#---------------------------------------------------------------------

trainControl <- trainControl(method = "cv", 
                             number = 5,
                             search = "grid")

metric <- "Accuracy"
tunegrid <- expand.grid(.mtry = c(0.5*mtry, mtry, 1.5*mtry))  
# at the moment caret only allows tuning of mtry (partly b/c ntree is just a matter of computational constraints)
set.seed(1984)
system.time(rf.grid <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, 
                             tuneGrid = tunegrid, 
                             trControl = trainControl, 
                             ntree = ntree)
)
# print grid search results
print(rf.grid)
plot(rf.grid)

rf_predict <- predict(rf.grid, newdata = test_x)
confusionMatrix(rf_predict, reference = test_y)


#--------------------------------------------
# finally, we use manual tuning over ntree
#--------------------------------------------
# we have one value for mtry and we will train 3 models with different values for ntree
# we fix mtry to original value (for lab purposes), but since we could also allow this to be cross-validated with every ntree
# I keep the code for trainControl and tunegrid (see HW2, question 5b)

trainControl <- trainControl(method = "cv", 
                             number = 5,
                             search = "grid")
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry = mtry) 

rffunc <- function(size){
  set.seed(1984)
  fit <- train(x = train_x, 
               y = train_y, 
               method = "rf", 
               metric = metric, 
               tuneGrid = tunegrid, 
               trControl = trainControl, 
               ntree = size)
  return(fit)
}

out <- pblapply(c(1, 5, 51), rffunc)


# collect results & summarize
results <- resamples(list(rf1 = out[[1]], rf5 = out[[2]], rf51 = out[[3]]))
results[["values"]]
summary(results)

# test set accuracy
(cm <- confusionMatrix(predict(out[[1]], newdata = test_x), test_y))
# access the components of the results with the $ operator
cm$table
cm$overall

confusionMatrix(predict(out[[2]], newdata = test_x), test_y)
confusionMatrix(predict(out[[3]], newdata = test_x), test_y)

# box and whisker plots to compare models
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)



#--------------------------------------------
# some additional notes
#--------------------------------------------

# reminder: Kappa = Cohen's Kappa, compares observed accuracy with expected accuracy (think: baseline accuracy)

# for Random Forest, caret also allows you to use tuneRF():
# Starting with the default value of mtry, 
# search for the optimal value (with respect to Out-of-Bag error estimate) of mtry for randomForest.
?tuneRF