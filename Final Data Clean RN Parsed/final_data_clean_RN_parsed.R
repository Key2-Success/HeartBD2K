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

load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub.Rdata")

# -------------- second dataframe (only named RNs where phrases are kept together)

# replace so that in the end only commas are special chars that separate entries
test_sub$MH <- stri_replace_all_regex(test_sub$MH, pattern = ", ", replacement = " ")
test_sub$MH <- stri_replace_all_regex(test_sub$MH, pattern = "\\*\\b", replacement = "")
test_sub$MH <- stri_replace_all_regex(test_sub$MH, pattern = "\\b/\\b", replacement = " ")
test_sub$MH <- stri_replace_all_regex(test_sub$MH, pattern = "&", replacement = "and")
test_sub$MH <- stri_replace_all_regex(test_sub$MH, pattern = "-", replacement = " ")
test_sub$MH <- stri_replace_all_regex(test_sub$MH, pattern = "_", replacement = ",")

# create columns for female and male
test_sub$female <- ""
test_sub$male <- ""

# columns display whether Male or Female is present in MH terms
for (i in 1:nrow(test_sub))
{
  test_sub$male[i] <- sum(grepl("\\bMale\\b", test_sub$MH[i]))
  test_sub$female[i] <- sum(grepl("\\bFemale\\b", test_sub$MH[i]))
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

dat <- dat[1:3000, ]

x <- sub_holder(", ", dat$text)

MH_parsed <- apply_as_tm(t(wfm(x$unhold(gsub(" ", "~~", x$output)), dat$docs)), 
                         weightTfIdf, to.qdap=FALSE)

save(RN_parsed, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN_parsed_tdm.Rdata")

# save dataframes
save(test_sub, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub.Rdata")
save(test_sub, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub_RN.Rdata")
