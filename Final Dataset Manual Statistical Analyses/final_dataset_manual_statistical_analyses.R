library(reshape2)


load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub_RN.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/female_corr_topallRN.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/male_corr_topallRN.Rdata")

# statistical analyses
female_df <- test2[test2$female == "female", ]
male_df <- test2[test2$male == "male", ]

save(female_df, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/female_df_RN.Rdata")
save(male_df, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/male_df_RN.Rdata")

occurrences_female <- function(word)
{
  # inserts \\b in the beginning
  word <- paste0("\\b", word)

  # inserts \\b at the end
  n <- nchar(word)
  word <- paste(substr(word, 1, n), "\\b", sep = "")
  
  occurrences <- sum(grepl(word, female_df$RN_demo, ignore.case = TRUE))
  
  return (occurrences)
}

occurrences_male <- function(word)
{
  # inserts \\b in the beginning
  word <- paste0("\\b", word)
  
  # inserts \\b at the end
  n <- nchar(word)
  word <- paste(substr(word, 1, n), "\\b", sep = "")
  
  occurrences <- sum(grepl(word, male_df$RN_demo, ignore.case = TRUE))
  
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

# create df for occurrences male
for (i in 1:nrow(male))
{
  word <- male$Var2[i]
  df_male <- data.frame(word, occurrences_male(word))
  df_male2 <- rbind(df_male2, df_male)
}

# calculates proportions for each term
df_female2$prop <- df_female2$occurrences_female.word./nrow(female_df)
df_male2$prop <- df_male2$occurrences_male.word./nrow(male_df)

# calculates p value for 2 proportion z-test
stats_manual <- function(word)
{
  # calculate proportions
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

df_pval2 <- df_pval2[order(df_pval2$stats_manual.word.), ]
df_pval2_sig <- df_pval2[df_pval2$stats_manual.word. <= 0.05, ]
df_pval2_sig <- df_pval2_sig[complete.cases(df_pval2_sig), ]
save(df_pval2_sig, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/sig_RN_parsed_z-test.Rdata")
  