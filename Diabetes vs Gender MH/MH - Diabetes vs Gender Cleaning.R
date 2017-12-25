library(stringi)
library(ggplot2)
library(tm)
library(qdap)
library(qdapTools)
library(stringr)
library(tidyverse)


load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH vs Gender/final_data_clean_MH_all.Rdata")

# only keep relevant variables
test <- test[, -c(2:3)]

# adds in diabetes demographics
test$diabetes <- ifelse(grepl("\\bDiabetes\\b", test$MH2) > 0, "diabetes", "nondiabetes")

# clean dataframe by removing special characters
test$MH2 <- stri_replace_all_regex(str = test$MH2, pattern = "\\(", replacement = " ")
test$MH2 <- stri_replace_all_regex(str = test$MH2, pattern = "\\)/", replacement = " ")
test$MH2 <- stri_replace_all_regex(str = test$MH2, pattern = "\\/", replacement = " ")
test$MH2 <- stri_replace_all_regex(str = test$MH2, pattern = "\\)", replacement = " ")

# save dataframe
save (test, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/final_clean_MH_diabetes_vs_gender.Rdata")


# create 4 distinct dataframes for each combined demographic
female_diabetes_MH <- test[test$female == "female" & test$diabetes == "diabetes", c(1:2)]
female_nondiabetes_MH <- test[test$female == "female" & test$diabetes == "nondiabetes", c(1:2)]
male_diabetes_MH <- test[test$male == "male" & test$diabetes == "diabetes", c(1:2)]
male_nondiabetes_MH <- test[test$male == "male" & test$diabetes == "nondiabetes", c(1:2)]

# save distinct dataframes
save(female_diabetes_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/female_diabetes_MH.Rdata")
save(female_nondiabetes_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/female_nondiabetes_MH.Rdata")
save(male_diabetes_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/male_diabetes_MH.Rdata")
save(male_nondiabetes_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/male_nondiabetes_MH.Rdata")

# look at distribution of all the 4 demographics
occurrences <- c(2128, 210019,
                 2438, 254662)

names(occurrences) <- c("female diabetes", "female nondiabetes",
                        "male diabetes", "male nondiabetes")

barplot(occurrences)
df <- data.frame(demographics, occurrences)

ggplot(data = df, aes(demographics, occurrences)) + geom_bar()

# create "dtm" with all MH terms per demographic
dtm_female_nondiabetes_MH <- female_nondiabetes_MH[sample(1000), ]
dtm_female_nondiabetes_MH <- as.data.frame(female_nondiabetes_MH[ , 2])
dtm_female_nondiabetes_MH <- mutate(dtm_female_nondiabetes_MH, var = str_split(MH2, ', ')) %>%
  unnest() %>%
  distinct()
dtm_female_nondiabetes_MH <- subset(dtm_female_nondiabetes_MH, !duplicated(var))
dtm_female_nondiabetes_MH <- as.data.frame(dtm_female_nondiabetes_MH[ , 2])
names(dtm_female_nondiabetes_MH) <- "word"

# save dataframes
save(dtm_female_diabetes_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/dtm_male_diabetes_MH.Rdata")
save(dtm_female_nondiabetes_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/dtm_male_diabetes_MH.Rdata")
save(dtm_male_diabetes_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/dtm_male_diabetes_MH.Rdata")
save(dtm_male_nondiabetes_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/dtm_male_nondiabetes_MH.Rdata")

