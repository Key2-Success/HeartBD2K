library(stringi)
library(ggplot2)
library(tm)
library(qdap)
library(qdapTools)


load("~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN vs Gender/final_RN_test_clean.Rdata")

# keep only relevant variables
test <- test_clean[ , c(1:3, 8:10)]
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

# adds in age demographics
test$infant <- ifelse(grepl("\\bInfant\\b", test$MH2) > 0, "infant", "")
test$child <- ifelse(grepl("\\bChild\\b", test$MH2) > 0, "child", "")
test$adolescent <- ifelse(grepl("\\bAdolescent\\b", test$MH2) > 0, "adolescent", "")
test$young_adult <- ifelse(grepl("\\bYoung Adult\\b", test$MH2) > 0, "young adult", "")
test$adult <- ifelse((grepl("\\bAdult\\b", test$MH2) > 0), "adult", "")
test$adult <- paste0(test$adult, test$young_adult)
test$adult <- ifelse(test$adult == "adult", "adult", "") # removes young adult from adult

# substitute "Middle Aged" with "middle aged" so it doesn't get confused with "Aged"
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "Middle Aged", replacement = "middle aged")
test$middle_aged <- ifelse(grepl("\\bmiddle aged\\b", test$MH2) > 0, "middle aged", "")
test$aged <- ifelse(grepl("\\bAged\\b", test$MH2) > 0, "aged", "") # aged, aged 80 and over, and frail elderly all taken care of
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "middle aged", replacement = "Middle Aged")

# save dataframe
save (test, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/final_clean_RN_gender_vs_age.Rdata")

# create 12 distinct dataframes for each combined demographic
female_infant_RN <- test[test$female == "female" & test$infant == "infant", c(1, 4)]
female_child_RN <- test[test$female == "female" & test$child == "child", c(1, 4)]
female_adolescent_RN <- test[test$female == "female" & test$adolescent == "adolescent", c(1, 4)]
female_youngadult_RN <- test[test$female == "female" & test$young_adult == "young adult", c(1, 4)]
female_adult_RN <- test[test$female == "female" & test$adult == "adult", c(1, 4)]
female_middleaged_RN <- test[test$female == "female" & test$middle_aged == "middle aged", c(1, 4)]
female_aged_RN <- test[test$female == "female" & test$aged == "aged", c(1, 4)]

male_infant_RN <- test[test$male == "male" & test$infant == "infant", c(1, 4)]
male_child_RN <- test[test$male == "male" & test$child == "child", c(1, 4)]
male_adolescent_RN <- test[test$male == "male" & test$adolescent == "adolescent", c(1, 4)]
male_youngadult_RN <- test[test$male == "male" & test$young_adult == "young adult", c(1, 4)]
male_adult_RN <- test[test$male == "male" & test$adult == "adult", c(1, 4)]
male_middleaged_RN <- test[test$male == "male" & test$middle_aged == "middle aged", c(1, 4)]
male_aged_RN <- test[test$male == "male" & test$aged == "aged", c(1, 4)]

# save distinct dataframes
save(female_infant_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/female_infant_RN.Rdata")
save(female_child_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/female_child_RN.Rdata")
save(female_adolescent_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/female_adolescent_RN.Rdata")
save(female_youngadult_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/female_youngadult_RN.Rdata")
save(female_adult_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/adult vs gender/female_adult_RN.Rdata")
save(female_middleaged_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/female_middleaged_RN.Rdata")
save(female_aged_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/female_aged_RN.Rdata")

save(male_infant_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/male_infant_RN.Rdata")
save(male_child_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/male_child_RN.Rdata")
save(male_adolescent_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/male_adolescent_RN.Rdata")
save(male_youngadult_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/male_youngadult_RN.Rdata")
save(male_adult_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/adult vs gender/male_adult_RN.Rdata")
save(male_middleaged_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/male_middleaged_RN.Rdata")
save(male_aged_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/male_aged_RN.Rdata")

# look at distribution of all the 12 demographics
occurrences <- c(3520, 4060, 3900, 920, 18860, 17329, 12766,
                 3550, 4645, 3865, 853, 18400, 21810, 14691)

names(occurrences) <- c("female infant", "female child", "female adolescent", "female young adult", "female adult", "female middle aged", "female aged",
                        "male infant", "male child", "male adolescent", "male young adult", "male adult", "male middle aged", "male aged")

barplot(occurrences)
df <- data.frame(demographics, occurrences)

ggplot(data = df, aes(demographics, occurrences)) + geom_bar()

load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/aged vs gender/male_aged_RN.Rdata")

# create corpus with phrases kept together based off https://stackoverflow.com/questions/24038498/corpus-build-with-phrases
set.seed(1)
dat <- male_adult_RN[ , 2]
dat <- dat[sample(10000), ]
dat <- as.data.frame(dat)
colnames(dat) <- c("text")
dat <- as.data.frame(dat)

# create 2 variables to combine into 1
dat$docs <- "doc"
dat$num <- ""
for (i in 1:nrow(dat))
{
  dat$num[i] <- i
}

# combine both variables
dat$docs <- paste(dat$docs, dat$num, sep = "")
dat <- dat[ , -c(3)]

x <- sub_holder(", ", dat$text)

dtm_male_adult_RN <- apply_as_tm(t(wfm(x$unhold(gsub(" ", "~~", x$output)), dat$docs)), 
                                  weightTfIdf, to.qdap = FALSE)

save(dtm_female_infant_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_female_infant_RN.Rdata")
save(dtm_female_child_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_female_child_RN.Rdata")
save(dtm_female_adolescent_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_female_adolescent_RN.Rdata")
save(dtm_female_youngadult_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_female_youngadult_RN.Rdata")
save(dtm_female_adult_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/adult vs gender/dtm_female_adult_RN.Rdata")
save(dtm_female_middleaged_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_female_middleaged_RN.Rdata")
save(dtm_female_aged_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_female_aged_RN.Rdata")

save(dtm_male_infant_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_male_infant_RN.Rdata")
save(dtm_male_child_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_male_child_RN.Rdata")
save(dtm_male_adolescent_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_male_adolescent_RN.Rdata")
save(dtm_male_youngadult_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_male_youngadult_RN.Rdata")
save(dtm_male_adult_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/adult vs gender/dtm_male_adult_RN.Rdata")
save(dtm_male_middleaged_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/dtm_male_middleaged_RN.Rdata")
save(dtm_male_aged_RN, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/aged vs gender/dtm_male_aged_RN.Rdata")
