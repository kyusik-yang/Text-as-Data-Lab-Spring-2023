# TA: Elisa Wirsching
# Course: Text as Data
# Date: 03/30/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, and Leslie Huang.


# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #

# Loading packages

pacman::p_load(quanteda,
               quanteda.corpora,
               dplyr,
               lsa, # for latent semantic analysis
               factoextra, # makes it easy to work with PCA (great for visualization)
               text2vec, # for similarity measures
               quanteda.textmodels
               )

# ============================================================================= #
####                                    PCA                                  ####
# ============================================================================= #

# some terminology
# -------------------

# what are PCA (factor) loadings?
# what are PCA scores?


# Two functions in base R for PCA:
# ------------------------------------------

# see: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
?prcomp # uses the singular value decomposition approach: examines the covariances/correlations between individuals
?princomp # uses the spectral decomposition approach: examines the covariances/correlations between variables (need more individuals than variables)

# Remember to center your data! (default = TRUE) -- use scale() on your matrix beforehand, or the option in prcomp()
# And don't have any missing values!


# Example
# --------------

data("data_corpus_sotu")
SOTU <- corpus_subset(data_corpus_sotu, Date > "1900-01-01")

SOTU_dfm <- tokens(SOTU, remove_punct = T) %>% 
  dfm() %>% 
  dfm_remove(stopwords("en")) %>% 
  dfm_wordstem()

SOTU_mat <- convert(SOTU_dfm, to = "matrix") # convert to matrix

# run pca
# ----------------

SOTU_pca <- prcomp(SOTU_mat, center = TRUE, scale = TRUE)

# visualize eigenvalues (scree plot: shows percentage of variance explained by each dimension)
fviz_eig(SOTU_pca, addlabels = TRUE)

# Loadings for each feature: columns contain the eigenvectors of covariance matrix (PCs)
SOTU_pca$rotation[1:10, 1:5]
dim(SOTU_pca$rotation)

# Q: can we interpret the dimensions?
# -------------------------------------

# loadings for features 
pc_loadings <- SOTU_pca$rotation
# what do we expect this correlation to be?
cor(pc_loadings[,1], pc_loadings[,2])  # these should be orthogonal

# top loadings on PC1
# token loadings
N <- 10
pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,1])) %>% arrange(-loading)
pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))

# plot top tokens according to absolute loading values
ggplot(pc1_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc1_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens with Top Loadings on PC1") +
  scale_colour_grey(start = .3, end = .7) +
  theme(panel.background = element_blank())

# Inspect rotated data
# -----------------------------

# Value of the rotated data: your "new", dimensionality reduced data
rotdat <- SOTU_pca$x 
View(rotdat)  # each observation 

rotdat <- cbind(rotdat, docvars(SOTU)) %>% 
  as.data.frame() %>% 
  arrange(PC1) %>% 
  mutate(order = 1:nrow(rotdat))

# order presidents' scores on the first PC
ggplot(rotdat, aes(x = order, y = PC1, label = rownames(rotdat), color = party)) +
  geom_text(size = 2) +
  scale_color_manual(values = c("#013364","#cc0000")) +
  theme_bw()



# similarity in lower dimensional space 
# -----------------------------------------

# function computes cosine similarity between query and all documents and returns N most similar
nearest_neighbors <- function(query, low_dim_space, N = 5, norm = "l2"){
  cos_sim <- sim2(x = low_dim_space, y = low_dim_space[query, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # query is always the nearest neighbor hence dropped
}

# apply to document retrieval
nearest_neighbors(query = "Obama-2009", low_dim_space = SOTU_pca$x, N = 5, norm = "l2")
nearest_neighbors(query = "Reagan-1982", low_dim_space = SOTU_pca$x, N = 5, norm = "l2")

# Visualization resources:

# Tutorial from factoextra author about how to use his package to explore and visualize PCA results: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# See here for visualizing PCA with the ggbiplot library: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/


# ============================================================================= #
####   Latent Semantic Analysis (LSA) aka Latent Semantic Indexing (LSI)     ####
# ============================================================================= #

# Let's keep using the SOTU data from before
SOTU_mat_lsa <- convert(SOTU_dfm, to = "lsa") # convert to transposed matrix 
#(so terms are rows and columns are documents = TDM)
SOTU_mat_lsa <- lw_logtf(SOTU_mat_lsa) * gw_idf(SOTU_mat_lsa) # local - global weighting (akin to TFIDF)

# Create LSA weights using TDM
#lsa(myMatrix, dims = dimcalc_share(share = 0.8)) 
# share = fraction of the sum of the selected singular values over the sum of all singular values, default is 0.5
SOTU_lsa <- lsa(SOTU_mat_lsa)

# what do we expect this correlation to be?
cor(SOTU_lsa$tk[,1], SOTU_lsa$tk[,2])  # these should be orthogonal

View(SOTU_lsa)
# lsa_obj$tk = truncated term matrix from term vector matrix T (constituting left singular vectors from the SVD of the original matrix)
#   - similar to factor loadings in PCA
# lsa_obj$dk = truncated document matrix from document vector matrix D (constituting right singular vectors from the SVD of the original matrix)
#   - similar to scores in PCA
# lsa_obj$sk = singular values: Matrix of scaling values to ensure that multiplying these matrices reconstructs TDM
# see: https://cran.r-project.org/web/packages/lsa/lsa.pdf

# Lecture example uses dims = 5
SOTU_lsa_5 <- lsa(SOTU_mat_lsa, 5)

# display generated LSA space
#?as.textmatrix
SOTU_lsa_5_mat <- t(as.textmatrix(SOTU_lsa_5))

# Q: What are these documents about?
# Compare features for a few speeches
SOTU_dfm@Dimnames$docs[130]
topfeatures(SOTU_dfm[130,])

# With 5 dims:
sort(SOTU_lsa_5_mat[130,], decreasing=T)[1:10]

# With auto (43) dims:
# sort(t(as.textmatrix(SOTU_lsa))[130, ], decreasing = T)[1:10]

# Q: How are words related?
# associate(): a method to identify words that are most similar to other words using a LSA
#?associate
# uses cosine similarity between input term and other terms
SOTU_lsa_mat <- as.textmatrix(SOTU_lsa) 

oil <- associate(SOTU_lsa_mat, "oil", "cosine", threshold = .7)
oil[1:10]

health <- associate(SOTU_lsa_mat, "health", "cosine", threshold = .7)
health[1:10]

# Keep this in mind when we do topic models and embeddings!

# ============================================================================= #
####                                WordFish                                 ####
# ============================================================================= #
# one-dimensional text scaling method.
# unlike wordscores, it does not require reference texts
# underlying model: Poisson-IRT model

# How is it different from other approaches we've used for scaling?

# Read in conservative and labour manifestos (from Recitation 5)
# ------------------------------------------------------------------
files <- list.files("W5_03_02_23/cons_labour_manifestos", full.names=TRUE)
text <- lapply(files, readLines)
text <- unlist(lapply(text, function(x) paste(x, collapse = " ")))

# Name data
files <- gsub("^(.+)cons_labour_manifestos/", "", files )
files <- gsub(".txt", "", files )

# Create metadata
year <- strsplit(files, "[^0-9]+") %>% unlist()
year <- year[year!=""]

party <- strsplit(files, "[^A-z]+") %>% unlist()
party <- party[party!="a" & party!="b"]

#create data frame
man_df <- data.frame(year = factor(as.numeric(year)),
                     party = factor(party),
                     text = text,
                     stringsAsFactors = FALSE)

# add text labels
man_df$text_label <- paste(man_df$party, man_df$year, sep = "_")


lab_con_dfm <- tokens(man_df$text, remove_punct = T) %>% 
  dfm() %>% 
  dfm_remove(stopwords("en")) %>% 
  dfm_wordstem()

# fit wordfish
# ---------------------

lab_con_dfm@Dimnames$docs <- man_df$text_label 
# Setting the index on parties
files[1]
files[24]
?textmodel_wordfish
manifestos_fish <- textmodel_wordfish(lab_con_dfm, c(1,24)) # second parameter for global identification
# this implies more negative position estimates are associated with conservatives

# visualize one-dimensional scaling
textplot_scale1d(manifestos_fish)
textplot_scale1d(manifestos_fish, groups = man_df$party)

# Plot of document positions
plot(year[1:23], manifestos_fish$theta[1:23]) # These are the conservative manifestos
points(year[24:46], manifestos_fish$theta[24:46], pch = 8) # These are the Labour manifestos

plot(as.factor(party), manifestos_fish$theta)
ggplot(data.frame(theta = manifestos_fish$theta, party = as.factor(party)), 
       aes(theta, color = party)) +
  geom_density() +
  scale_color_hue(l = 45) +
  theme_bw()

# most important features--word effects in IRT model
words <- manifestos_fish$psi # values
names(words) <- manifestos_fish$features # the words

sort(words)[1:50] 
sort(words, decreasing=T)[1:50] 

# Guitar plot
weights <- manifestos_fish$beta

temp <- data.frame(words = names(words), fixedeffect = words, marginaleffect = weights)
# plot(weights, words)

ggplot(temp, aes(marginaleffect, fixedeffect)) +
  geom_text(data = temp %>% filter(abs(marginaleffect) > 3.5 | fixedeffect > 3),
            aes(marginaleffect,fixedeffect,label=words),
            size = 4,
            nudge_y = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw()

# also check out wordshoal!