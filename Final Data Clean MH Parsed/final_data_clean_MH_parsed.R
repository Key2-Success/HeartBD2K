library(stringi)
library(tm)
library(gofastr)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(RColorBrewer)
library(qdap)
library(qdapTools)
library(RNewsflow)

load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_all.Rdata")

# -------------- clean original dataframe

# reduce from 428362 to 428036 based on which case reports have MH
test <- test[!is.na(test$MH), ]

# replace so that in the end only commas are special chars that separate entries
test$MH2 <- stri_replace_all_regex(test$MH, pattern = ", ", replacement = " ")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "\\*\\b", replacement = "")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "\\b/\\b", replacement = " ")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "&", replacement = "and")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "-", replacement = " ")
test$MH2 <- stri_replace_all_regex(test$MH2, pattern = "_", replacement = ",")

# create columns for female and male
test$female <- ""
test$male <- ""

# INEFFICIENT, SLOW CODE 
# columns display whether Male or Female is present in MH terms
# for (i in 1:nrow(test))
# {
#   test$male[i] <- sum(grepl("\\bMale\\b", test$MH2[i]))
#   test$female[i] <- sum(grepl("\\bFemale\\b", test$MH2[i]))
#   if (test$male[i] > 0)
#   {
#     test[i, "male"] <- "male"
#   }
#   else
#   {
#     test[i, "male"] <- ""
#   }
#   if (test$female[i] > 0)
#   {
#     test[i, "female"] <- "female"
#   }
#   else
#   {
#     test[i, "female"] <- ""
#   }
# }

# faster code!!! wow! vectorized operations are awesome
test$male <- ifelse(grepl("\\bMale\\b", test$MH2) > 0, "male", "")
test$female <- ifelse(grepl("\\bFemale\\b", test$MH2) > 0, "female", "")

# save cleaned dataframe
save (test, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_clean_MH_all.Rdata")

# create corpus with phrases kept together based off https://stackoverflow.com/questions/24038498/corpus-build-with-phrases
dat <- test_sub[ , 2]
colnames(dat) <- c("text")

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

dat <- dat[1:1000, ]

x <- sub_holder(", ", dat$text)

MH_parsed <- apply_as_tm(t(wfm(x$unhold(gsub(" ", "~~", x$output)), dat$docs)), 
                         weightTfIdf, to.qdap = FALSE)

save(MH_parsed, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH_parsed_tdm.Rdata")

# save dataframes
save(test_sub, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub_MH.Rdata")
