library(readr)
library(data.table)
library(reshape)
library(dplyr)
library(tidyr)
library(xlsx)


# read in medline file
single_jack_raw <- read_delim("~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/Harry's Data/single_jack_raw.txt", delim = "\n", col_names = F)
test <- single_jack_raw

# manually read in the .tsv that shows correct classification
merge <- single_or_multiple_pt2
remove(single_or_multiple_pt2)

# if error in first step: manually read in the medline file by pasting into excel and creating .csv
test <- single_or_multiple_raw_pt2
remove(single_or_multiple_raw_pt2)

# recreate correct labels
myFunction1 <- function(dt) 
{
  require(zoo)
  require(stringi)
  require(data.table)
  d <- copy(dt)
  d[, v6 := substr(X1, 1, 6)]
  # d[, v3 := substr(v6, 1, 3)]
  # d[, emty := ifelse(v3 == "   ", T, F)]
  d[v6 == "      ", v6 := NA]
  d[, v6 := na.locf(v6, na.rm = F)]
  d[is.na(v6), v6 := "      "]
  stri_sub(d$X1, 1, 6) <- d$v6
  d[, "X1", with = F]
}

test <- as.data.table(test)
test <- myFunction1(test)

# keep only PMID, DA, TI, JT, MH, RN, PST (for ID purposes)
test <- test[substr(test$X1, 1, 4) == "PMID" | substr(test$X1, 1, 2) == "DA" |
               substr(test$X1, 1, 2) == "TI" | substr(test$X1, 1, 2) == "JT" | substr(test$X1, 1, 2) == "RN" |
               substr(test$X1, 1, 2) == "MH" |substr(test$X1, 1, 3) == "PST", ]

# remove MHDA
test <- filter(test, !grepl(pattern = "MHDA", x = test$X1))

# coerce tibble to data table
setDT(test)

# reorder variables suggestion 1
test <- test[, old_order := 1:.N]
pst_index <- c(0, which(grepl("^PST", test$X1)))
test <- test[, grp := unlist(lapply(1:(length(pst_index)-1), function(x) rep(x, times = (pst_index[x+1] - pst_index[x]))))]
test <- test[ , -c("old_order")]

# rename column
names(test) <- c("All", "group")

# create column names
test$Variable <- sub("-.*", '', test$All)

# extract only the data
test$Value <- sub(".*- ", '', test$All)

# reshape from wide to long, thereby collapsing strings if necessary
test <- test %>% 
  group_by(group, Variable) %>% 
  summarise(Value=paste(gsub(' ', ' ', Value), collapse='_ ')) %>% 
  spread(Variable, Value)

# clean up dataframe
test <- test[ , -c(1, 6)] # deletes group and PST
test <- test[ , c(4, 6, 2, 1, 3, 5)]
colnames(test) <- c("PMID", "TI", "JT", "DA", "MH", "RN") # removes spaces in names

# change class
test$PMID <- as.factor(test$PMID)
test$TI <- as.character(test$TI)
test$JT <- as.factor(test$JT)
test$DA <- as.Date(test$DA, "%Y%m%d")
test$MH <- as.character(test$MH)
test$RN <- as.character(test$RN)

# join and clean up
test <- as.data.frame(test)
merge$PMID <- as.factor(merge$PMID)
test <- test[ , c(1, 5)]
test <- left_join(test, merge)

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

# faster code!!! wow! vectorized operations are awesome
test$male <- ifelse(grepl("\\bMale\\b", test$MH2) > 0, "male", "")
test$female <- ifelse(grepl("\\bFemale\\b", test$MH2) > 0, "female", "")

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

# reorder variables
test <- test[ , c(1, 3, 4:12)]

# bind the first 40 by first manually reading in other 40
names(total)[12] <- "Number of Patients"
test <- rbind(test, total)
remove(total)

# make variables binary
test$predict <- "Single"
test$predict <- ifelse((test$female == "female" && test$male == "male"), "Multiple", "test")
test$female <- ifelse(test$female == "female", 1, 0)
test$male <- ifelse(test$male == "male", 1, 0)
test$infant <- ifelse(test$infant == "infant", 1, 0)
test$child <- ifelse(test$child == "child", 1, 0)
test$adolescent <- ifelse(test$adolescent == "adolescent", 1, 0)
test$young_adult <- ifelse(test$young_adult == "young adult", 1, 0)
test$adult <- ifelse(test$adult == "adult", 1, 0)
test$middle_aged <- ifelse(test$middle_aged == "middle aged", 1, 0)
test$aged <- ifelse(test$aged == "aged", 1, 0)

# make predictions
for (i in 1:nrow(test))
{
  if ((test$female[i] == 0 && test$male[i] == 0))# && sum(test$infant[i] + test$child[i] + test$adolescent[i] + test$young_adult[i] + test$adult[i] + test$middle_aged[i] + test$aged[i]) > 1)
  {
    test$predict[i] <- "Multiple"
  }
  if (sum(test$infant[i] + test$child[i] + test$adolescent[i] + test$young_adult[i] + test$adult[i] + test$middle_aged[i] + test$aged[i]) > 1)
  {
    test$predict[i] <- "Multiple"
  }
  if (test$female[i] == 1 && test$male[i] == 0 && test$infant[i] == 1 && (sum(test$infant[i] + test$child[i] + test$adolescent[i] + test$young_adult[i] + test$adult[i] + test$middle_aged[i] + test$aged[i]) < 4))
  {
    test$predict[i] <- "Single"
  }
  if ((test$female[i] == 1 && test$male[i] == 1))
  {
    test$predict[i] <- "Multiple"
  }
}

# establish counters
accurate_percent <- 0
actually_single <- 0
actually_multiple <- 0
test$wrong <- 0

# check accuracy
for (i in 1:nrow(test))
{
  if (test$`Number of Patients`[i] == test$predict[i]) # counts how many are accurate
  {
    accurate_percent <- accurate_percent + 1
  }
  if (test$`Number of Patients`[i] == "Single" && test$predict[i] == "Multiple")
  {
    actually_single <- actually_single + 1
  }
  if (test$`Number of Patients`[i] == "Multiple" && test$predict[i] == "Single")
  {
    actually_multiple <- actually_multiple + 1
  }
  if (test$`Number of Patients`[i] != test$predict[i]) # variable that flags incorrect
  {
    test$wrong[i] <- 1
  }
}
accurate_percent <- (accurate_percent/(dim(test)[1]))*100
errors <- dim(test)[1] - (dim(test)[1]*accurate_percent)/100


# save
save(test, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/Harry's Data/286_single_multiple_classification.Rdata")
