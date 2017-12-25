library(stringi)
library(ggplot2)
library(tm)
library(qdap)
library(qdapTools)


load("~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN vs Gender/final_RN_test_clean.Rdata")

# keep only relevant variables
test <- test_clean[ , c(1:2, 8:10)]
remove(test_clean)

# clean MH terms - replace so that in the end only commas are special chars that separate entries
test$MH2 <- stri_replace_all_regex(test$MH, pattern = ", ", replacement = " ")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "\\*\\b", replacement = "")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "\\b/\\b", replacement = " ")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "&", replacement = "and")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "-", replacement = " ")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "_", replacement = ",")

# reorder variables
test <- test[, c(1, 6, 3, 4, 5)]

# adds in diabetes demographics
test$diabetes <- ifelse(grepl("\\bDiabetes\\b", test$MH2) > 0, "diabetes", "nondiabetes")

# clean dataframe by removing special characters
test$MH2 <- stri_replace_all_regex(str = test$MH2, pattern = "\\(", replacement = " ")
test$MH2 <- stri_replace_all_regex(str = test$MH2, pattern = "\\)/", replacement = " ")
test$MH2 <- stri_replace_all_regex(str = test$MH2, pattern = "\\/", replacement = " ")
test$MH2 <- stri_replace_all_regex(str = test$MH2, pattern = "\\)", replacement = " ")

# save dataframe
save (test, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/final_clean_RN_diabetes-nondiabetes.Rdata")

# create 12 distinct dataframes for each combined demographic
female_diabetes_RN <- test[test$female == "female" & test$diabetes == "diabetes", c(1, 3)]
female_nondiabetes_RN <- test[test$female == "female" & test$diabetes == "nondiabetes", c(1, 3)]
male_diabetes_RN <- test[test$male == "male" & test$diabetes == "diabetes", c(1, 3)]
male_nondiabetes_RN <- test[test$male == "male" & test$diabetes == "nondiabetes", c(1, 3)]

# save distinct dataframes
save(female_diabetes_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/female_diabetes_RN.Rdata")
save(female_nondiabetes_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/female_nondiabetes_RN.Rdata")
save(male_diabetes_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/male_diabetes_RN.Rdata")
save(male_nondiabetes_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/male_nondiabetes_RN.Rdata")

# look at distribution of all the 12 demographics
occurrences <- c(669, 46823, 759, 52523)

names(occurrences) <- c("female diabetes", "female nondiabetes", "male diabetes", "male nondiabetes")
barplot(occurrences)
df <- data.frame(demographics, occurrences)

ggplot(data = df, aes(demographics, occurrences)) + geom_bar()

load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/aged vs gender/male_aged_RN.Rdata")

# create "dtm" with all RNs
test_sub <- test[sample(1000), ]
test_sub <- as.data.frame(test[ , 3])
test_sub <- mutate(test_sub, var = str_split(named_RN, ', ')) %>%
  unnest() %>%
  distinct()
test_sub <- subset(test_sub, !duplicated(var))
test_sub <- as.data.frame(test_sub[ , 2])
names(test_sub) <- "word"

save(test_sub, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Diabetes vs Gender/dtm_diabetes-nondiabetes_RN.Rdata")
