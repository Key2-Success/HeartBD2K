library(tm)
library(qdap)
library(qdapTools)
library(stringi)
library(stringr)
library(purrr)


# load in distinct dataframes as well as distinct RN (chosen from random sample of 1000)
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/middleaged vs gender/dtm_female_middleaged_RN.Rdata")
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/middleaged vs gender/male_middleaged_RN.Rdata")
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/middleaged vs gender/dtm_male_middleaged_RN.Rdata")
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/middleaged vs gender/female_middleaged_RN.Rdata")

# creating mydf (create female first)
dtm_tf2 <- weightTfIdf(dtm_female_middleaged_RN)
m2 <- as.matrix(dtm_tf2)
rownames(m2) <- 1:nrow(m2)
norm_eucl <- function(m2) m2/apply(m2, MARGIN = 1, FUN = function(x) sum(x^2)^0.5) # normalize vectors so Euclidean distance makes sense
m_norm2 <- norm_eucl(m2)
m_norm2 <- m_norm2[, order(colSums(-m_norm2))]
m_norm2 <- t(m_norm2)
m_norm2[is.na(m_norm2)] <- 0
m_norm2 <- m_norm2[order(-rowSums(m_norm2)), ] # orders them 
mydf <- as.data.frame(names(as.data.frame(m2)))
save(mydf, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/middleaged vs gender/mydf_female_middleaged_RN.Rdata")
mydf_fem <- mydf

# creating mydf (create female first)
dtm_tf2 <- weightTfIdf(dtm_male_middleaged_RN)
m2 <- as.matrix(dtm_tf2)
rownames(m2) <- 1:nrow(m2)
norm_eucl <- function(m2) m2/apply(m2, MARGIN = 1, FUN = function(x) sum(x^2)^0.5) # normalize vectors so Euclidean distance makes sense
m_norm2 <- norm_eucl(m2)
m_norm2 <- m_norm2[, order(colSums(-m_norm2))]
m_norm2 <- t(m_norm2)
m_norm2[is.na(m_norm2)] <- 0
m_norm2 <- m_norm2[order(-rowSums(m_norm2)), ] # orders them 
mydf <- as.data.frame(names(as.data.frame(m2)))

save(mydf, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/middleaged vs gender/mydf_male_middleaged_RN.Rdata")
remove(dtm_female_middleaged_RN, dtm_male_middleaged_RN)

# combine all unique RN terms found in each
mydf <- rbind(mydf, mydf_fem)
remove(mydf_fem)
mydf <- mydf[, 1]
mydf <- as.data.frame(mydf)
names(mydf) <- "ID"
mydf$ID <- stri_replace_all_regex(str = mydf$ID, pattern = "\"", replacement = "")
mydf$ID <- stri_replace_all_regex(str = mydf$ID, pattern = "\'", replacement = "")
mydf$ID <- as.character(mydf$ID)
mydf <- subset(mydf, !duplicated(mydf))
names(mydf)[1] <- "word"

# clean up so exact match is used
mydf$word <- paste0("\\b", mydf$word)
mydf$test <- "\\b"
mydf$word <- paste(mydf$word, mydf$test, sep = "")
mydf <- mydf[ , -c(2)]
mydf <- as.data.frame(mydf)
names(mydf)[1] <- "word"
mydf$word <- as.character(mydf$word)

# find occurrences of each RN term
male_middleaged_RN$named_RN <- tolower(male_middleaged_RN$named_RN)
mydf$male_middleaged <- map_int(mydf$word, function(x){sum(str_detect(male_middleaged_RN$named_RN, pattern = x))})

female_middleaged_RN$named_RN <- tolower(female_middleaged_RN$named_RN)
mydf$female_middleaged <- map_int(mydf$word, function(x){sum(str_detect(female_middleaged_RN$named_RN, pattern = x))})

# calculates p value for 2 proportion z-test
stats_manual <- function(word)
{
  # calculate raw occurrences
  prop_f <- mydf[mydf$word == word, 3]
  prop_m <- mydf[mydf$word == word, 2]
  
  # calculate p-value
  p <- prop.test(x = c(prop_f, prop_m), n = c(dim(female_middleaged_RN)[1], dim(male_middleaged_RN)[1]))
  p <- p$p.value
  return (p)
}

# create dataframes to store p-values
df_pval <- data.frame()
df_pval2 <- data.frame()

# create for-loop for binding dataframe for p-values
for (i in 1:nrow(mydf))
{
  word <- mydf$word[i]
  df_pval <- data.frame(word, stats_manual(word))
  df_pval2 <- rbind(df_pval2, df_pval)
}

mydf <- cbind(mydf, df_pval2)
mydf <- mydf[ , c(1, 2, 3, 5)]
names(mydf)[4] <- "p-value"
remove(df_pval)

# calculate proportions for each term
options(scipen = 999) # disable scientific notation
mydf$prop_female <- mydf$female_middleaged/nrow(female_middleaged_RN)
mydf$prop_male <- mydf$male_middleaged/nrow(male_middleaged_RN)

# save raw occurrence counts
save(mydf, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/middleaged vs gender/mydf_middleaged_male-female_RN.Rdata")

# remove \\b
mydf2 <- mydf
mydf2 <- substr(mydf2$word, 1, nchar(mydf2$word)-2)
mydf2 <- as.data.frame(mydf2)
mydf2$mydf2 <- substring(mydf2$mydf2, 3)
mydf <- cbind(mydf2, mydf)
mydf <- mydf[ , c(1, 3, 4, 5, 6, 7)]
names(mydf)[1] <- "word"

# save p-value dataframes
options(scipen = 999)
mydf <- mydf[order(mydf$`p-value`), ]
mydf_sig <- mydf[mydf$`p-value` < 0.05, ]
mydf_sig <- mydf_sig[complete.cases(mydf_sig), ]

# create differences
for (i in 1:nrow(mydf_sig))
{
  mydf_sig$diffs[i] <- mydf_sig$prop_male[i] - mydf_sig$prop_female[i]
}

# order by most different
mydf_sig$most_diff <- abs(mydf_sig$diffs)
mydf_sig <- mydf_sig[order(-mydf_sig$most_diff), ]

save(mydf_sig, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/middleaged vs gender/sig_RN_middleaged_btwn_genders.Rdata")

