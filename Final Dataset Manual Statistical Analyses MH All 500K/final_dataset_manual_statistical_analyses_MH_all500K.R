library(tm)
library(qdap)
library(qdapTools)
library(stringi)
library(stringr)


# load in 132k (those with RN) case files
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_clean_MH_all.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/female_corr_topallMH.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/male_corr_topallMH.Rdata")

# statistical analyses
female_df <- test[test$female == "female", ]
male_df <- test[test$male == "male", ]

# find raw occurrences of 2136 RN terms
occurrences_female <- function(word)
{
  # inserts \\b in the beginning
  word <- paste0("\\b", word)
  
  # inserts \\b at the end
  n <- nchar(word)
  word <- paste(substr(word, 1, n), "\\b", sep = "")
  
  occurrences <- sum(grepl(word, female_df$MH2, ignore.case = TRUE))
  
  return (occurrences)
}

occurrences_male <- function(word)
{
  # inserts \\b in the beginning
  word <- paste0("\\b", word)
  
  # inserts \\b at the end
  n <- nchar(word)
  word <- paste(substr(word, 1, n), "\\b", sep = "")
  
  occurrences <- sum(grepl(word, male_df$MH2, ignore.case = TRUE))
  
  return (occurrences)
}

# calculate occurrences manually
df_female <- data.frame()
df_female2 <- data.frame()
df_male <- data.frame()
df_male2 <- data.frame()

# create df for occurrences female
for (i in 1:nrow(female))
{
  word <- female$Var2[i]
  df_female <- data.frame(word, occurrences_female(word))
  df_female2 <- rbind(df_female2, df_female)
}

df_female <- occurrences_female(female$Var2)
df_female <- as.data.frame(df_female)

# create df for occurrences male
for (i in 1:nrow(male))
{
  word <- male$Var2[i]
  df_male <- data.frame(word, occurrences_male(word))
  
  df_male2 <- rbind(df_male2, df_male)
}

# calculates p value for 2 proportion z-test
stats_manual <- function(word)
{
  # calculate raw occurrences
  prop_f <- df_female2[df_female2$word == word, 2]
  prop_m <- df_male2[df_male2$word == word, 2]
  
  # calculate p-value
  p <- prop.test(x = c(prop_f, prop_m), n = c(dim(female_df)[1], dim(male_df)[1]))
  p <- p$p.value
  return (p)
}

# create dataframes to store p-values
df_pval <- data.frame()
df_pval2 <- data.frame()

# create for-loop for binding dataframe for p-values
for (i in 1:nrow(female))
{
  word <- female$Var2[i]
  df_pval <- data.frame(word, stats_manual(word))
  df_pval2 <- rbind(df_pval2, df_pval)
}

# calculate proportions for each term
options(scipen = 999) # disable scientific notation
df_female2$prop <- df_female2$occurrences_female.word./nrow(female_df)
df_male2$prop <- df_male2$occurrences_male.word./nrow(male_df)

save(df_male2, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/df_male2_MH_all.Rdata")
save(df_female2, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/df_female2_MH_all.Rdata")


options(scipen = 999)
save(df_pval2, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/df_pval2_MH.Rdata")
df_pval2 <- df_pval2[order(df_pval2$stats_manual.word.), ]
df_pval2_sig <- df_pval2[df_pval2$stats_manual.word. <= 0.05, ]
df_pval2_sig <- df_pval2_sig[complete.cases(df_pval2_sig), ]
save(df_pval2_sig, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/sig_MH_parsed_z-test_all_RN.Rdata")


# misc text mining

# look at top occurrences (not parsed though)
freq_terms(text.var = female_df$MH2, top = 25)
female$Var2 <- as.character(female$Var2)
termmatrix <- sapply(female$Var2, function(x){str_count(female_df$MH2, x)})

# manually look at stats
word = "arrythmia"
p <- prop.test(x = c(sum(grepl(word, female_df$MH2, ignore.case = TRUE)), 
                     sum(grepl(word, male_df$MH2, ignore.case = TRUE))), n = c(212147, 257100))
data.frame(word, p$estimate, p$p.value)

               
