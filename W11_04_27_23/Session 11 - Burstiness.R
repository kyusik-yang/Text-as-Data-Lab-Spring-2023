# TA: Elisa Wirsching
# Course: Text as Data
# Date: 04/27/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, and Leslie Huang.


# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #

# Loading packages

rm(list = ls())
set.seed(1234)

pacman::p_load(bursts,
               quanteda,
               readtext)

# ============================================================================= #
####                              BURSTY FUNCTION                            ####
# ============================================================================= #

# recap of burstiness

?kleinberg


# 1  Loading bursty function: a repurposing of some guts of kleinberg()

bursty <- function(word = "sioux", DTM, date) {
  word.vec <- DTM[, which(colnames(DTM) == word)]
  if(length(word.vec) == 0) {
    print(word, " does not exist in this corpus.")
  } 
  else {
    word.times <- c(0,which(as.vector(word.vec)>0))
    
    kl <- kleinberg(word.times, gamma = 0.5)
    kl$start <- date[kl$start+1]
    kl$end <- date[kl$end]
    max_level <- max(kl$level)
    
    plot(c(kl$start[1], kl$end[1]), c(1,max_level),
         type = "n", xlab = "Time", ylab = "Level", bty = "n",
         xlim = c(min(date), max(date)), ylim = c(1, max_level),
         yaxt = "n")
    axis(2, at = 1:max_level)
    
    for (i in 1:nrow(kl)) {
      if (kl$start[i] != kl$end[i]) {
        arrows(kl$start[i], kl$level[i], kl$end[i], kl$level[i], code = 3, angle = 90,
               length = 0.05)
      } 
      else {
        points(kl$start[i], kl$level[i])
      }
    }
    
    print(kl)
  }
  #note deviation from standard defaults bec don't have that much data
}


# 2 Let's use this on the Conservative and Labour manifestos
list.files("./W5_03_02_23/cons_labour_manifestos/")

# Loading data

manifesto <- readtext("./W5_03_02_23/cons_labour_manifestos/*.txt", 
                      docvarsfrom=c("filenames"))

manifesto_corpus <- corpus(manifesto)

# set year
manifesto_corpus$date <- as.numeric(gsub("[[:alpha:]]",
                                         "",
                                         manifesto_corpus$docvar1))

manifesto_dfm <- dfm(manifesto_corpus)

# 3.1 Evaluating the burstiness of several key words

bursty("thatcher", manifesto_dfm, manifesto_corpus$date)

bursty("churchill", manifesto_dfm, manifesto_corpus$date)

bursty("argentina", manifesto_dfm, manifesto_corpus$date)

