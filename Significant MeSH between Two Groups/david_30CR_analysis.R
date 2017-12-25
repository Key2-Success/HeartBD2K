library(dplyr)
library(readr)
library(data.table)
library(reshape)
library(dplyr)
library(tidyr)
library(xlsx)
library(stringi)
library(stringr)
library(tidyverse)

## extract all PMIDs from the .xlsx file names
# names <- list.files(path = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/David's Data/HFrEF/.")
# names <- names[-1]
# library(stringi)
# names <- stri_replace_all_regex(names, "-metadata-JZ.xlsx", "")
# names <- stri_replace_all_regex(names, substr(names, 1, 7), "")
# names <- as.data.frame(names)

david <- read_delim(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/David's Data/HFrEF 50 CR.txt", delim = "\n", col_names = FALSE)

# keep only PMID, MH
names(david) <- "V1"
david <- david[substr(david$V1, 1, 6) == "MH  - ", ]

# replace MH with ""
david$V1 <- stri_replace_all_regex(str = david$V1, pattern = "MH  - ", "")

# find unique MH
unique <- mutate(david, var = str_split(V1, '\n')) %>% 
  unnest() %>% distinct() 

unique <- subset(unique, !duplicated(var))
unique <- as.data.frame(unique[ , 1])
names(unique) <- "word"
unique$word <- as.character(unique$word)

# change so only comma separated
unique$word <- stri_replace_all_regex(unique$word, pattern = ", ", replacement = " ")
unique$word <- stri_replace_all_regex(unique$word, pattern = "\\*\\b", replacement = "")
unique$word <- stri_replace_all_regex(unique$word, pattern = "\\b/\\b", replacement = " ")
unique$word <- stri_replace_all_regex(unique$word, pattern = "&", replacement = "and")
unique$word <- stri_replace_all_regex(unique$word, pattern = "-", replacement = " ")
unique$word <- stri_replace_all_regex(unique$word, pattern = "_", replacement = ",")

# clean up so exact match is used
unique$word <- paste0("\\b", unique$word)
unique$test <- "\\b"
unique$word <- paste(unique$word, unique$test, sep = "")
unique <- unique[ , -c(2)]
unique <- as.data.frame(unique)
names(unique)[1] <- "word"
unique$word <- as.character(unique$word)

# change so only comma separated
david$V1 <- stri_replace_all_regex(david$V1, pattern = ", ", replacement = " ")
david$V1 <- stri_replace_all_regex(david$V1, pattern = "\\*\\b", replacement = "")
david$V1 <- stri_replace_all_regex(david$V1, pattern = "\\b/\\b", replacement = " ")
david$V1 <- stri_replace_all_regex(david$V1, pattern = "&", replacement = "and")
david$V1 <- stri_replace_all_regex(david$V1, pattern = "-", replacement = " ")
david$V1 <- stri_replace_all_regex(david$V1, pattern = "_", replacement = ",")

# count occurrences
unique$count <- map_int(unique$word, function(x){sum(str_detect(david$V1, pattern = x))})
unique <- unique[order(-unique$count), ]

# remove \\b
mydf2 <- unique
mydf2 <- substr(mydf2$word, 1, nchar(mydf2$word)-2)
mydf2 <- as.data.frame(mydf2)
mydf2$mydf2 <- substring(mydf2$mydf2, 3)
mydf <- cbind(mydf2, unique)
mydf <- mydf[ , -2]

# clean up data
names(mydf) <- c("MeSH Terms", "Occurrences")
mydf <- subset(mydf, !duplicated(mydf))

# remove unneccessary dataframes
remove(david)
remove(mydf2)
remove(unique)

# save files
write.xlsx(mydf, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/David's Data/david_HFrEF 50 CR.xlsx")
save(mydf, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/David's Data/david_HFrEF 50 CR.Rdata")


# ---- which MH terms are significant between 2 groups

# reorganize files
mydf$prop <- mydf$Occurrences/50
HFrEF <- mydf

diastolic_HF <- mydf
remove(mydf)
diastolic_HF$prop <- diastolic_HF$Occurrences/84

# create new df
test <- full_join(diastolic_HF, HFrEF, "MeSH Terms") # all MeSH terms present in both
test[is.na(test)] <- 0

# find and remove those that don't satisfy chi square conditions
count <- numeric(702) # dim of test

for (i in 1:nrow(test))
{
  a <- c(test$Occurrences.x[i], test$Occurrences.y[i])
  b <- c(84, 50) # dim of each df
  m <- matrix(c(a, b-a), ncol = 2)
  if (sum(chisq.test(m)$expected > 5) != 4)
  {
   count <- append(count, i)
  }
}

test <- test[-count, ]

# find p-values
for (i in 1:nrow(test))
{
  # calculate raw occurrences
  prop_1 <- test[i, 2]
  prop_2 <- test[i, 4]
  
  # calculate p-value
  p <- prop.test(x = c(prop_1, prop_2), n = c(84, 50), correct = TRUE)
  p <- p$p.value
  test$pvaltest[i] <- p
}

test <- test[test$pval <= 0.10, ]

a <- c(15, 2)
b <- c(84, 50)
m <- matrix(c(a, b-a), ncol=2)
chisq.test(m)
chisq.test(m)$expected
