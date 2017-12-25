library(tm)
library(qdap)
library(qdapTools)
library(stringi)
library(stringr)
library(purrr)


# load in distinct dataframes as well as distinct MH (chosen from random sample of 1000)
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/adult vs gender/dtm_female_adult_MH.Rdata")
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/adult vs gender/male_adult_MH.Rdata")
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/adult vs gender/dtm_male_adult_MH.Rdata")
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/adult vs gender/female_adult_MH.Rdata")

# creating mydf (create female first)
dtm_tf2 <- weightTfIdf(dtm_female_adult_MH)
m2 <- as.matrix(dtm_tf2)
rownames(m2) <- 1:nrow(m2)
norm_eucl <- function(m2) m2/apply(m2, MARGIN = 1, FUN = function(x) sum(x^2)^0.5) # normalize vectors so Euclidean distance makes sense
m_norm2 <- norm_eucl(m2)
m_norm2 <- m_norm2[, order(colSums(-m_norm2))]
m_norm2 <- t(m_norm2)
m_norm2[is.na(m_norm2)] <- 0
c2 <- kmeans(m_norm2, 10) # cluster into 10 clusters
m_norm2 <- m_norm2[order(-rowSums(m_norm2)), ] # orders them 
pca <- prcomp((m_norm2))
dat.loadings <- pca$x[ , 1:2]
c <- kmeans(dat.loadings, centers = 10)
pca1 <- pca$x[ , 1]
pca2 <- pca$x[ , 2]
mydf <- data.frame(ID = names(pca1), PCA1 = pca1, PCA2 = pca2, Cluster = factor(c$cluster))
remove(dat.loadings, m_norm2, m2, c, c2, dtm_tf2, pca, pca1, pca2)

# do on female first
save(mydf, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/adult vs gender/mydf_female_adult_MH.Rdata")
mydf_fem <- mydf

# creating mydf (create female first)
dtm_tf2 <- weightTfIdf(dtm_male_adult_MH)
m2 <- as.matrix(dtm_tf2)
rownames(m2) <- 1:nrow(m2)
norm_eucl <- function(m2) m2/apply(m2, MARGIN = 1, FUN = function(x) sum(x^2)^0.5) # normalize vectors so Euclidean distance makes sense
m_norm2 <- norm_eucl(m2)
m_norm2 <- m_norm2[, order(colSums(-m_norm2))]
m_norm2 <- t(m_norm2)
m_norm2[is.na(m_norm2)] <- 0
c2 <- kmeans(m_norm2, 10) # cluster into 10 clusters
m_norm2 <- m_norm2[order(-rowSums(m_norm2)), ] # orders them 
pca <- prcomp((m_norm2))
dat.loadings <- pca$x[ , 1:2]
c <- kmeans(dat.loadings, centers = 10)
pca1 <- pca$x[ , 1]
pca2 <- pca$x[ , 2]
mydf <- data.frame(ID = names(pca1), PCA1 = pca1, PCA2 = pca2, Cluster = factor(c$cluster))
remove(dat.loadings, m_norm2, m2, c, c2, dtm_tf2, pca, pca1, pca2)

save(mydf, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/adult vs gender/mydf_male_adult_MH.Rdata")
remove(dtm_female_adult_MH, dtm_male_adult_MH)

# combine all unique MH terms found in each
mydf <- rbind(mydf, mydf_fem)
remove(mydf_fem)
mydf <- mydf[ , 1]
mydf <- as.data.frame(mydf)
mydf$mydf <- as.character(mydf$mydf)
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

# find occurrences of each MH term
male_adult_MH$MH2 <- tolower(male_adult_MH$MH2)
mydf$male_adult <- map_int(mydf$word, function(x){sum(str_detect(male_adult_MH$MH2, pattern = x))})

female_adult_MH$MH2 <- tolower(female_adult_MH$MH2)
mydf$female_adult <- map_int(mydf$word, function(x){sum(str_detect(female_adult_MH$MH2, pattern = x))})

# calculates p value for 2 proportion z-test
stats_manual <- function(word)
{
  # calculate raw occurrences
  prop_f <- mydf[mydf$word == word, 3]
  prop_m <- mydf[mydf$word == word, 2]
  
  # calculate p-value
  p <- prop.test(x = c(prop_f, prop_m), n = c(dim(female_adult_MH)[1], dim(male_adult_MH)[1]))
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
mydf$prop_female <- mydf$female_adult/nrow(female_adult_MH)
mydf$prop_male <- mydf$male_adult/nrow(male_adult_MH)

# save raw occurrence counts
save(mydf, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/adult vs gender/mydf_adult_male-female_MH.Rdata")

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

save(mydf_sig, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/adult vs gender/sig_MH_adult_btwn_genders.Rdata")

