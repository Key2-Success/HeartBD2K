# ----- reading and converting the MEDLINE file to a dataframe ----- #

# load in packages
library(readr)
library(data.table)
library(reshape)
library(dplyr)
library(tidyr)
library(xlsx)
library(stringr)
library(purrr)

# read in raw data
HFpEF <- read_delim(file = "HFpEF 97.txt", delim = "/n", col_names = FALSE)
HFrEF <- read_delim(file = "HFrEF 204.txt", delim = "/n", col_names = FALSE)

# test <- HFpEF # after done with HFpEF switch to HFrEF
test <- HFrEF

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

# keep only PMID, RN
test <- test[substr(test$X1, 1, 2) == "RN", ]

# replace RN with ""
test$X1 <- stri_replace_all_regex(str = test$X1, pattern = "RN  - ", "")

# find unique RN
unique <- mutate(test, var = str_split(X1, '\n')) %>% unnest() %>% distinct() 
unique <- subset(unique, !duplicated(var))
unique <- as.data.frame(unique[ , 1])
names(unique) <- "word"
unique$word <- as.character(unique$word)

# assign to HFpEF, and now re-do all for HFpEF
p <- test
unique_p <- unique

# assign to HFrEF
r <- test
unique_r <- unique

# combine all uniques
unique <- rbind(unique_p, unique_r)
unique <- subset(unique, !duplicated(word))

# only extract between the characters
unique_new <- sub("\\).*", "", sub(".*\\(", "", unique$word)) 
unique_new <- as.data.frame(unique_new)

# make character
p$X1 <- as.character(p$X1)
unique_new$unique_new <- as.character(unique_new$unique_new)

# find word occurrences across both HFpEF and HFrEF1
unique_new$p <- map_int(unique_new$unique_new, function(x){sum(str_detect(p$X1, pattern = x))})
unique_new$r <- map_int(unique_new$unique_new, function(x){sum(str_detect(r$X1, pattern = x))})

# ----- calculating p-value ----- #

# count of each p and r CR
sum(substr(HFpEF$X1, 1, 4) == "PMID") # 94 HFpEF cases
sum(substr(HFrEF$X1, 1, 4) == "PMID") # 203 HFrEF cases

unique_new$p_prop <- unique_new$p/94
unique_new$r_prop <- unique_new$r/203

# find and remove those that don't satisfy chi square conditions
count <- numeric(214) # dim of unique_new

for (i in 1:nrow(unique_new))
{
  a <- c(unique_new$p[i], unique_new$r[i])
  b <- c(94, 203) # dim of each df
  m <- matrix(c(a, b-a), ncol = 2)
  if (sum(chisq.test(m)$expected > 5) != 4)
  {
    count <- append(count, i)
  }
}

unique_new2 <- unique_new[-count, ]

# find p-values
for (i in 1:nrow(unique_new))
{
  # calculate raw occurrences
  prop_1 <- unique_new[i, 2]
  prop_2 <- unique_new[i, 3]
  
  # calculate p-value
  p <- prop.test(x = c(prop_1, prop_2), n = c(94, 203), correct = TRUE)
  p <- p$p.value
  unique_new$pvalue[i] <- p
}
