library(tm)
library(qdap)
library(qdapTools)
library(stringi)
library(stringr)
library(purrr)


# load in distinct dataframes as well as all RN
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/male_nondiabetes_RN.Rdata")
load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/male_diabetes_RN.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/dtm_diabetes-nondiabetes_RN.Rdata")

# use only all RN
mydf <- test_sub
remove(test_sub)

# change so only comma separated
mydf$word <- stri_replace_all_regex(mydf$word, pattern = ", ", replacement = " ")
mydf$word <- stri_replace_all_regex(mydf$word, pattern = "\\*\\b", replacement = "")
mydf$word <- stri_replace_all_regex(mydf$word, pattern = "\\b/\\b", replacement = " ")
mydf$word <- stri_replace_all_regex(mydf$word, pattern = "&", replacement = "and")
mydf$word <- stri_replace_all_regex(mydf$word, pattern = "-", replacement = " ")
mydf$word <- stri_replace_all_regex(mydf$word, pattern = "_", replacement = ",")

# clean up so exact match is used
mydf$word <- paste0("\\b", mydf$word)
mydf$test <- "\\b"
mydf$word <- paste(mydf$word, mydf$test, sep = "")
mydf <- mydf[ , -c(2)]
mydf <- as.data.frame(mydf)
names(mydf)[1] <- "word"
mydf$word <- as.character(mydf$word)

# remove special characters
mydf$word <- stri_replace_all_regex(str = mydf$word, pattern = "\\(", replacement = " ")
mydf$word <- stri_replace_all_regex(str = mydf$word, pattern = "\\)/", replacement = " ")
mydf$word <- stri_replace_all_regex(str = mydf$word, pattern = "\\/", replacement = " ")
mydf$word <- stri_replace_all_regex(str = mydf$word, pattern = "\\)", replacement = " ")

# find occurrences of each RN term
mydf$word <- tolower(mydf$word)
male_diabetes_RN$RN2 <- tolower(male_diabetes_RN$named_RN)
male_nondiabetes_RN$RN2 <- tolower(male_nondiabetes_RN$named_RN)

mydf$male_diabetes <- map_int(mydf$word, function(x){sum(str_detect(male_diabetes_RN$RN2, pattern = x))})
mydf$male_nondiabetes <- map_int(mydf$word, function(x){sum(str_detect(male_nondiabetes_RN$RN2, pattern = x))})

# only mydf RN
mydf <- subset(mydf, !duplicated(mydf$word))

# calculates p value for 2 proportion z-test
stats_manual <- function(word)
{
  # calculate raw occurrences
  prop_diabetes <- mydf[mydf$word == word, 2]
  prop_nondiabetes <- mydf[mydf$word == word, 3]
  
  # calculate p-value
  p <- prop.test(x = c(prop_diabetes, prop_nondiabetes), n = c(dim(male_diabetes_RN)[1], dim(male_nondiabetes_RN)[1]))
  p <- p$p.value
  return (p)
}

# create dataframes to store p-values
df_pval <- data.frame()
df_pval2 <- data.frame()

# remove rows with all 0s
mydf <- mydf[(mydf$male_diabetes + mydf$male_nondiabetes != 0), ]

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
mydf$prop_diabetes <- mydf$male_diabetes/nrow(male_diabetes_RN)
mydf$prop_nondiabetes <- mydf$male_nondiabetes/nrow(male_nondiabetes_RN)

# remove \\b
mydf2 <- mydf
mydf2 <- substr(mydf2$word, 1, nchar(mydf2$word)-2)
mydf2 <- as.data.frame(mydf2)
mydf2$mydf2 <- substring(mydf2$mydf2, 3)
mydf <- cbind(mydf2, mydf)
mydf <- mydf[ , c(1, 3, 4, 5, 6, 7)]
names(mydf)[1] <- "word"

# save raw occurrence counts
save(mydf, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/mydf_male_diabetes-nondiabetes_RN.Rdata")

# save p-value dataframes
options(scipen = 999)
mydf <- mydf[order(mydf$`p-value`), ]
mydf_sig <- mydf[mydf$`p-value` < 0.05, ]
mydf_sig <- mydf_sig[complete.cases(mydf_sig), ]

# create differences
for (i in 1:nrow(mydf_sig))
{
  mydf_sig$diffs[i] <- mydf_sig$prop_nondiabetes[i] - mydf_sig$prop_diabetes[i]
}

# order by most different
mydf_sig$most_diff <- abs(mydf_sig$diffs)
mydf_sig <- mydf_sig[order(-mydf_sig$most_diff), ]

save(mydf_sig, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/sig_RN_male_btwn_diabetes-nondiabetes.Rdata")

