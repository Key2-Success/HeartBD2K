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


load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data.Rdata")

# -------------- first dataframe where all RNs are included and phrases are not kept together

# create new column without _ separating each entry
test$MH_clean <- stri_replace_all_regex(test$MH, pattern = '_ ', " ")

# new column of cleaned out RN
test$all_RN <- ""
for(i in 1:nrow(test))
{
  test[i, "all_RN"] <- gsub("[\\(\\)]", "", regmatches(test[i, "RN"], gregexpr("\\(.*?\\)", test[i, "RN"])))
  if (test[i, "all_RN"] == "character0")
  {
    test[i, "all_RN"] <- test[i, "RN"]
  }
}

# idea 2 for general and named RNs
test$general_RN <- ""
test$named_RN <- ""

test$RN_temp <- gsub("^[0] "," general_RN",test$RN) # replace leading 0s w/ general_RN
test$RN_temp <- gsub(" [0] "," general_RN",test$RN_temp) # replace other " 0 "
test$RN_temp <- gsub(" \\("," named_RN(",test$RN_temp) # replace rest w/ named_RN

test$named_RN <- regmatches(test$RN_temp,gregexpr("(?<=named_RN\\().*?(?=\\))", test$RN_temp, perl=TRUE))
test$general_RN <- regmatches(test$RN_temp,gregexpr("(?<=general_RN\\().*?(?=\\))", test$RN_temp, perl=TRUE))
test$RN_temp <- NULL

test$named_RN <- unlist(lapply(test$named_RN, function(x) ifelse(is.null(x), NA, paste0(x, collapse = ", "))))
test$general_RN <- unlist(lapply(test$general_RN, function(x) ifelse(is.null(x), NA, paste0(x, collapse = ", "))))

# create a new column without the c" and " list structure
test$all_RN_clean <- stri_replace_all_regex(test$all_RN, pattern = '^c', "")
test$all_RN_clean <- stri_replace_all_regex(test$all_RN, pattern = '"', "")
test$all_RN_clean <- gsub("^c", "", test$all_RN_clean)

# save dataframe
save(test, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data.Rdata")

# work on subset
set.seed(1)
test_sub <- test[sample(nrow(test), 10000), ]

# create columns for female and male
test_sub$female <- ""
test_sub$male <- ""

# columns display whether Male or Female is present in MH terms
for (i in 1:nrow(test_sub))
{
  test_sub$male[i] <- sum(grepl("\\bMale\\b", test_sub$MH_clean[i]))
  test_sub$female[i] <- sum(grepl("\\bFemale\\b", test_sub$MH_clean[i]))
  if (test_sub$male[i] > 0)
  {
    test_sub[i, "male"] <- "male"
  }
  else
  {
    test_sub[i, "male"] <- ""
  }
  if (test_sub$female[i] > 0)
  {
    test_sub[i, "female"] <- "female"
  }
  else
  {
    test_sub[i, "female"] <- ""
  }
}

# repeat gender frequency for RN
test_sub$RN_demo <- paste(test_sub$all_RN_clean, test_sub$female, sep = ", ")
test_sub$RN_demo <- paste(test_sub$RN_demo, test_sub$male, sep = ", ")



# -------------- second dataframe (only named RNs where phrases are kept together)

# remove all general RN terms
test2 <- test[!(test$named_RN == ""), ]

# keep words together with ~ and separate entries with spaces
test2$named_RN <- stri_replace_all_regex(test2$named_RN, pattern = ", ", replacement = "?")
test2$named_RN <- stri_replace_all_regex(test2$named_RN, pattern = " ", replacement = "~")
test2$named_RN <- stri_replace_all_regex(test2$named_RN, pattern = "\\?", replacement = " ")
test2$named_RN <- stri_replace_all_regex(test2$named_RN, pattern = ",", replacement = "~")
test2$named_RN <- stri_replace_all_regex(test2$named_RN, pattern = "-", replacement = "~")

set.seed(1)
test2 <- test2[sample(nrow(test2), 10000), ]

# create columns for female and male
test2$female <- ""
test2$male <- ""

# columns display whether Male or Female is present in MH terms
for (i in 1:nrow(test2))
{
  test2$male[i] <- sum(grepl("\\bMale\\b", test2$MH_clean[i]))
  test2$female[i] <- sum(grepl("\\bFemale\\b", test2$MH_clean[i]))
  if (test2$male[i] > 0)
  {
    test2[i, "male"] <- "male"
  }
  else
  {
    test2[i, "male"] <- ""
  }
  if (test2$female[i] > 0)
  {
    test2[i, "female"] <- "female"
  }
  else
  {
    test2[i, "female"] <- ""
  }
}

# repeat gender frequency for RN
test2$RN_demo <- paste(test2$named_RN, test2$female, sep = ", ")
test2$RN_demo <- paste(test2$RN_demo, test2$male, sep = ", ")


# create corpus with phrases kept together based off https://stackoverflow.com/questions/24038498/corpus-build-with-phrases
dat <- test2[ , 11]
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

x <- sub_holder(", ", dat$text)

RN_parsed <- apply_as_tm(t(wfm(x$unhold(gsub(" ", "~~", x$output)), dat$docs)), 
            weightTfIdf, to.qdap=FALSE)

save(RN_parsed, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN_parsed_tdm.Rdata")

# save dataframes
save(test_sub, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub.Rdata")
save(test2, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub_RN.Rdata")
