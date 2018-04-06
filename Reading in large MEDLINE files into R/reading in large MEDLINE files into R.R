# steps:
# on line 14, change from where you are reading in your textfile
# on line 75, change where you want to save the dataframe


library(readr)
library(data.table)
library(reshape)
library(dplyr)
library(tidyr)
library(xlsx)

# read in raw data
test <- read_delim(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_case_reports_May_23_2017_428362_out.txt/all_case_reports_May_23_2017_428362_out.txt", delim = "\n", col_names = FALSE)

# --- run the next few lines only if PMID is not attached to the top of the dataframe
# shows indices of each PMID
a <- as.data.frame(which(grepl("^PMID", test$X1)))
colnames(a) <- "X1"

# bind first entry's PMID to top of dataframe
first_pmid <- test[a[1, 1], ]
test <- rbind(first_pmid, test)

# shows indices of each PMID after the bind
a <- as.data.frame(lapply(a, FUN = function(x) x+1)) # recalibrate indices by adding 1
test <- test[-a[2, 1], 1] # remove duplicate 1st PMID
remove(first_pmid)
remove(a)

# --- resume normal code                                  
# keep only PMID, MH, RN, PST
test <- test[substr(test$X1, 1, 4) == "PMID" | substr(test$X1, 1, 2) == "MH" |
               substr(test$X1, 1, 2) == "RN" | substr(test$X1, 1, 3) == "PST", ] # 6 mil

# remove MHDA
test <- filter(test, !grepl(pattern = "MHDA", x = test$X1)) # 5.6 mil

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
  summarise(Value = paste(gsub(' ', ' ', Value), collapse='_ ')) %>% 
  spread(Variable, Value) 
 
# clean up dataframe
test <- test[ , -which(names(test) %in% c("group", "PST"))]
test <- test[ , c(2, 1, 3)]
colnames(test) <- c("PMID", "MH", "RN") # rename because names have spaces in them
#test <- test[complete.cases(test), ] # 132k

# change class
test$PMID <- as.factor(test$PMID)
test$MH <- as.character(test$MH)
test$RN <- as.character(test$RN)

# save dataframe
save(test, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_all.Rdata")
