library(stringi)
library(ggplot2)
library(tm)
library(qdap)
library(qdapTools)


load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH vs Gender/final_data_clean_MH_all.Rdata")

# only keep relevant variables
test <- test[, -c(2:3)]

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
save (test, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/final_clean_MH_gender_vs_age.Rdata")


# create 14 distinct dataframes for each combined demographic
female_infant_MH <- test[test$female == "female" & test$infant == "infant", c(1:2)]
female_child_MH <- test[test$female == "female" & test$child == "child", c(1:2)]
female_adolescent_MH <- test[test$female == "female" & test$adolescent == "adolescent", c(1:2)]
female_youngadult_MH <- test[test$female == "female" & test$young_adult == "young adult", c(1:2)]
female_adult_MH <- test[test$female == "female" & test$adult == "adult", c(1:2)]
female_middleaged_MH <- test[test$female == "female" & test$middle_aged == "middle aged", c(1:2)]
female_aged_MH <- test[test$female == "female" & test$aged == "aged", c(1:2)]

male_infant_MH <- test[test$male == "male" & test$infant == "infant", c(1:2)]
male_child_MH <- test[test$male == "male" & test$child == "child", c(1:2)]
male_adolescent_MH <- test[test$male == "male" & test$adolescent == "adolescent", c(1:2)]
male_youngadult_MH <- test[test$male == "male" & test$young_adult == "young adult", c(1:2)]
male_adult_MH <- test[test$male == "male" & test$adult == "adult", c(1:2)]
male_middleaged_MH <- test[test$male == "male" & test$middle_aged == "middle aged", c(1:2)]
male_aged_MH <- test[test$male == "male" & test$aged == "aged", c(1:2)]

# save distinct dataframes
save(female_infant_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/female_infant_MH.Rdata")
save(female_child_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/female_child_MH.Rdata")
save(female_adolescent_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/female_adolescent_MH.Rdata")
save(female_youngadult_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/female_youngadult_MH.Rdata")
save(female_adult_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/female_adult_MH.Rdata")
save(female_middleaged_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/female_middleaged_MH.Rdata")
save(female_aged_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/female_aged_MH.Rdata")

save(male_infant_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/male_infant_MH.Rdata")
save(male_child_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/male_child_MH.Rdata")
save(male_adolescent_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/male_adolescent_MH.Rdata")
save(male_youngadult_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/male_youngadult_MH.Rdata")
save(male_adult_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/male_adult_MH.Rdata")
save(male_middleaged_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/male_middleaged_MH.Rdata")
save(male_aged_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/male_aged_MH.Rdata")

# look at distribution of all the 14 demographics
occurrences <- c(18212, 20788, 18761, 3887, 82764, 75574, 55950,
                 18468, 23831, 21037, 4302, 88515, 101666, 67673)

names(occurrences) <- c("female infant", "female child", "female adolescent", "female young adult", "female adult", "female middle aged", "female aged",
                        "male infant", "male child", "male adolescent", "male young adult", "male adult", "male middle aged", "male aged")

barplot(occurrences)
df <- data.frame(demographics, occurrences)

ggplot(data = df, aes(demographics, occurrences)) + geom_bar()


# create corpus with phrases kept together based off https://stackoverflow.com/questions/24038498/corpus-build-with-phrases
set.seed(1)
dat <- female_adult_MH[ , 2]
colnames(dat) <- c("text")
dat <- dat[sample(1000), ]

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

dtm_female_adult_MH <- apply_as_tm(t(wfm(x$unhold(gsub(" ", "~~", x$output)), dat$docs)), 
                         weightTfIdf, to.qdap = FALSE)

save(dtm_female_infant_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_female_infant_MH.Rdata")
save(dtm_female_child_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_female_child_MH.Rdata")
save(dtm_female_adolescent_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_female_adolescent_MH.Rdata")
save(dtm_female_youngadult_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_female_youngadult_MH.Rdata")
save(dtm_female_adult_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_female_adult_MH.Rdata")
save(dtm_female_middleaged_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_female_middleaged_MH.Rdata")
save(dtm_female_aged_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_female_aged_MH.Rdata")

save(dtm_male_infant_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_male_infant_MH.Rdata")
save(dtm_male_child_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_male_child_MH.Rdata")
save(dtm_male_adolescent_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_male_adolescent_MH.Rdata")
save(dtm_male_youngadult_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_male_youngadult_MH.Rdata")
save(dtm_male_adult_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_male_adult_MH.Rdata")
save(dtm_male_middleaged_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_male_middleaged_MH.Rdata")
save(dtm_male_aged_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/dtm_male_aged_MH.Rdata")
