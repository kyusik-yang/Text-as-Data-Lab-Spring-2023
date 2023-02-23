# TA: Elisa Wirsching
# Course: Text as Data
# Date: 23/02/2023
# Lab adapted from: Lucia Motolinia, Pedro L. Rodriguez, Kevin Munger, Patrick Chester and Leslie Huang.


# ============================================================================= #
####                                SETTING UP                               ####
# ============================================================================= #

# clear global environment
rm(list = ls())

pacman::p_load(quanteda,
               quanteda.corpora,
               stringr)



# ============================================================================= #
####                              NON-ENGLISH TEXTS                          ####
# ============================================================================= #

# Non-English stopwords
# ---------------------------

stopwords(language = "es")
stopwords(language = "de")
stopwords(language = "ar", source = "misc")
stopwords(language = "zh", source = "misc")
?stopwords

# Text encoding
# ---------------------------

# What is text encoding?
# How do you figure out what kind you have (e.g. scraped text from the Internet)?
# What kind of encoding can R and/or quanteda handle?

# Some types of text encoding
# ---------------------------
# character encoding is a set of mappings between the bytes in the computer 
# and the characters in the character set.
# UTF-8
# ASCII (subset of UTF-8)
# Latin-1

# UTF-8 represents characters from European languages (English, Spanish, German, French, etc) 
# and some characters from Chinese/Japanese/Korean, plus emojis.

# Note: Text obtained from Internet sources can be messy. 
# Issues can especially arise when you are working with texts from multiple sources 
# and you end up with a mixture of encodings. This can cause the encoding to be 
# detected incorrectly when you read in the text.

# What encoding do you have?
# ---------------------------

# You can check with this function in base R
validUTF8("This is a sentence")

# You can use the package utf8(), written by Patrick Perry from NYU
# Read about it here: https://github.com/patperry/r-utf8
# install.packages("utf8")
library("utf8")

as_utf8("\xF0\x9F\x98\x8D")
# emojis unicodes: https://www.unicode.org/emoji/charts/full-emoji-list.html

# What if you get a weird character and you're not sure?
# ----------------------------------------------------------

# install.packages("stringi")
library("stringi")

# Use the encoding guesser to guess what this character is
stri_enc_detect("0x00E3")

# It's only a guess!

# What's ISO-8859-1?
# This is another name for the Latin-1 encoding. 

# How do you convert encodings?
# -------------------------------

test_str <- "São Paulo"
validUTF8(test_str)
converted_str <- iconv("São Paulo", from = "LATIN2", to = "UTF-8")

converted_str
validUTF8(converted_str)

# Looks the same right?

charToRaw(converted_str) # UTF-8 encoding
charToRaw(test_str) # Latin-1 encoding

# other languages
validUTF8("法律")
validUTF8("قانون")

# In most cases, your text will probably already be in UTF-8. (e.g. Wikipedia corpora are!) 
# In most cases, you want to convert your text to UTF-8, can handle most Latin and non-Latin script languages.

# The authors of quanteda have also written a package called readtext() 
# that can also deal with encodings in text corpora!


# ============================================================================= #
####                            MORE REGULAR EXPRESSIONS                     ####
# ============================================================================= #

# cheatsheet for regex: 
#     https://github.com/rstudio/cheatsheets/blob/main/regex.pdf
#     https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf
# to test your regex expressions: https://regex101.com/ 
# or use str_view() or str_view_all()

# Examples
words <- c("Washington Post", "NYT", "Wall Street Journal", "Peer-2-Peer", "Red State", "Cheese", "222", ",")

# Exploring by character type
# -------------------------------
#?grep
grep("\\w", words, value = T)  # Elements that have alphanumeric characters
grep("\\w{7}", words, value = T)  # Elements that have words that are at least 7 characters long
grep("\\d", words, value = T)  # Elements that contain numbers
grep("\\W", words, value = T)  # Elements that contain nonword characters (Including white space)

# note that grep returns the full element that matched the pattern (unlike grepl)

# Repetition
# -------------------------------

# ?: 0 or 1
# +: 1 or more
# *: 0 or more
# {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"

str_view(x, "CC?")
str_view(x, "CC*")
str_view(x, "CC+")
str_view(x, 'C[LX]') # [] stands for "one of"
str_view(x, 'C[LX]+')
str_view(x, "C{2}")
str_view(x, "C{2,3}")
str_view_all(x, "([^ ]+)")

# Groups
# -------------------------------

presidents <- c("Roosevelt-33", "Roosevelt-37", "Obama-2003")

# Use gsub to replace patterns with a string or stringr's str_replace(_all) and str_sub
# Parentheses can identify groups that can later be referenced by \\1 - \\2
gsub("(\\w+)-(\\d{2})", "\\1-19\\2", presidents) 

# We want to use the $ to indicate that the pattern should come at the end of the word, 
# to avoid the mismatch in Obama-192003
gsub("(\\w+)-(\\d{2})$", "\\1-19\\2", presidents) 

# Look-ahead and look-behind
# -------------------------------

x <- "The United States of America (U.S.A. or USA), commonly known as the United States (U.S. or US) or America, is a country primarily located in North America. It consists of 50 states, a federal district, five major unincorporated territories, nine Minor Outlying Islands, and 326 Indian reservations."
str_view_all(tolower(x), "united(?= states)")
str_view_all(tolower(x), "(?<!united )states")


# Note that regex expressions in R are similar to those in other languages 
# but there are some key differences

# More Resources:
# https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
# http://r4ds.had.co.nz/strings.html#matching-patterns-with-regular-expressions



# ============================================================================= #
####            SELECTING FEATURES FROM DFM USING REGULAR EXPRESSIONS        ####
# ============================================================================= #

# Using simple texts
# -------------------------------

testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with the newspaper from a a boy named Seamus, in his mouth."

# keep only words ending in "s"
tokens(testText) %>% 
  dfm() %>% 
  dfm_select(pattern =  "s$", valuetype = "regex")

testTweets <- c("2 + 2 = 4 #1984",
                "I thought you said the park? Why are we at the vet? #QuestionsFromPets",
                "Holy freeway #flooding Batman! #californiastorms taking their toll.")
# keep only hashtags i.e. expressions starting with a pound sign
tokens(testTweets) %>% 
  dfm() %>% 
  dfm_select(pattern = "^#", valuetype = "regex")



# Selecting features from a corpus
# ------------------------------------

data("data_corpus_irishbudgets")

# You can pass a list of words to the "select" parameter in dfm, but using 
# regular expressions can enable you to get all variants of a word
irishbudgets_dfm <- tokens(data_corpus_irishbudgets) %>% 
  dfm() %>% 
  dfm_select(pattern = c("tax|budg|auster"),
             valuetype = "regex")
featnames(irishbudgets_dfm)  





# ============================================================================= #
####                       LOAD DATA: CONSERVATIVE MANIFESTOS                ####
# ============================================================================= #
# read in the files
filenames <- list.files(path = "W4_02_23_23/conservative_manifestos", full.names=TRUE)
cons_manifestos <- lapply(filenames, readLines)
# because readLines returns a vector with each elements = lines
cons_manifestos <- sapply(cons_manifestos, function(x) paste(x, collapse = " "))

# get the date docvar from the filename
dates <- sapply(str_extract_all(filenames, pattern = "[[:digit:]]+"), "[[",5)
dates

manifestos_df <- data.frame(year = dates, text = cons_manifestos)


# ============================================================================= #
####                                  DICTIONARIES                           ####
# ============================================================================= #
# Here, dictionary = list of words, not the data structure.
# Python users: there is no dictionary object in R (Note: you can create dictionary-like objects using lists)

mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
             "New York City has raised a taxes: an income tax and a sales tax.")

mydict <- c("tax", "income", "capital", "gains", "inheritance")

mytexts %>% 
  tokens() %>% 
  dfm() %>% 
  dfm_select(mydict)

# Example: Laver Garry dictionary
# ------------------------------------

# https://rdrr.io/github/kbenoit/quanteda.dictionaries/man/data_dictionary_LaverGarry.html
# https://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/laver-garry-dictionary-of-policy-position/
lgdict <- dictionary(file = "W4_02_23_23/LaverGarry.cat", format = "wordstat")

# What's in this thing?
View(lgdict)

# Run the conservative manifestos through this dictionary
manifestos_lg <- manifestos_df$text %>% 
  tokens() %>% 
  dfm() %>% 
  dfm_lookup(lgdict)

# how does this look
as.matrix(manifestos_lg)[1:5, 1:5]
featnames(manifestos_lg)

# plot it
plot(manifestos_df$year, 
     manifestos_lg[,"CULTURE.SPORT"],
     xlab="Year", ylab="SPORTS", type="b", pch=19)

plot(manifestos_df$year, 
     manifestos_lg[,"VALUES.CONSERVATIVE"],
     xlab="Year", ylab="Conservative values", type="b", pch=19)

plot(manifestos_df$year, 
     manifestos_lg[,"INSTITUTIONS.CONSERVATIVE"] - manifestos_lg[,"INSTITUTIONS.RADICAL"],
     xlab="Year", ylab="Net Conservative Institutions", type="b", pch=19)

# RID Dictionary--Regressive Imagery Dictionary
# --------------------------------------------------

# https://www.kovcomp.co.uk/wordstat/RID.html
rid_dict <- dictionary(file = "W4_02_23_23/RID.cat", format = "wordstat")

data("data_corpus_sotu")

sotus_texts <- as.character(data_corpus_sotu)

# Get the docvars from the corpus object
year <- docvars(data_corpus_sotu)$Date
pres <- docvars(data_corpus_sotu)$President

sotu_rid_dfm <- data_corpus_sotu %>% 
  tokens() %>% 
  dfm() %>% 
  dfm_lookup(rid_dict)

# Look at the categories
featnames(sotu_rid_dfm)

# Inspect the results graphically
plot(year, 
     sotu_rid_dfm[,"PRIMARY.REGR_KNOL.NARCISSISM"],
     xlab="Year", ylab="Narcissism", type="b", pch=19)

plot(year, 
     sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.FIRE"] + 
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.ASCEND"] +
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.DESCENT"] +
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.DEPTH"] + 
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.HEIGHT"] + 
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.WATER"],
     xlab="Year", ylab="Icarian-ness", type="b", pch=19)

