# steps:
# on line 15, change from where you are reading in your textfile
# on line 85, change from where you are reading in the counts file
# on line 99, change to where you want to save your excel file


library(readr)
library(data.table)
library(reshape)
library(dplyr)
library(tidyr)
library(xlsx)

# read in raw data
test <- read_delim(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/David's Data/all_case_reports_8-8-17.txt", delim = "\n", col_names = FALSE)
save(test, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/David's Data/all_case_reports_8-8-17.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/David's Data/all_case_reports_8-8-17.Rdata")

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

# keep only PMID, MH
test <- test[substr(test$X1, 1, 4) == "PMID" | substr(test$X1, 1, 2) == "MH", ]

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

# change class
test$PMID <- as.factor(test$PMID)
test$MH <- as.character(test$MH)

# save
save(test, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/David's Data/all_case_reports_clean_8-8-17.Rdata")
