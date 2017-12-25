library(varhandle)
library(xlsx)
library(dplyr)
library(stringi)
library(stringr)


# load in files
load("Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/John's Data/Merge All/cancer_0.Rdata")
impacts <- read.xlsx(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/John's Data/2016 Impact Factor Full List.xlsx", sheetIndex = 1)

# remove unneccessary variables
impacts <- impacts[-1 , -c(4, 7, 8, 9)]
impacts <- impacts[ , -c(3, 5)]

# clean up labels
names(impacts) <- as.matrix(impacts[1, ])
impacts <- impacts[-1, ]
impacts[] <- lapply(impacts, function(x) type.convert(as.character(x)))

# clean up variables' classes
impacts$Rank <- unfactor(impacts$Rank)
impacts$Rank <- as.numeric(impacts$Rank)

impacts$`Full Journal Title` <- as.factor(impacts$`Full Journal Title`)

impacts$`Journal Impact Factor` <- unfactor(impacts$`Journal Impact Factor`)
impacts$`Journal Impact Factor` <- as.numeric(impacts$`Journal Impact Factor`)

# add in RN named only
test$RN <- stri_replace_all_regex(test$RN, "_", "")
test$RN_temp <- gsub("^[0] "," general_RN",test$RN) # replace leading 0s w/ general_RN
test$RN_temp <- gsub(" [0] "," general_RN",test$RN_temp) # replace other " 0 "
test$RN_temp <- gsub(" \\("," named_RN(",test$RN_temp) # replace rest w/ named_RN

test$named_RN <- regmatches(test$RN_temp,gregexpr("(?<=named_RN\\().*?(?=\\))", test$RN_temp, perl=TRUE))
test$RN_temp <- NULL
test$named_RN <- unlist(lapply(test$named_RN, function(x) ifelse(is.null(x), NA, paste0(x, collapse = ", "))))

test <- test[ , c(1, 2, 3, 4, 5, 10, 7, 8, 9)] # replace RN with named RN

# make all journals lower case for ease of matching
impacts$`Full Journal Title` <- tolower(impacts$`Full Journal Title`)
test$JT <- tolower(test$JT)
test$journal <- tolower(test$journal)

# rename journals to ones found in impacts file
test$journal <- ifelse(test$journal == "n engl j med", "new england journal of medicine", test$journal)
test$journal <- ifelse(test$journal == "ann oncol", "annals of oncology", test$journal)
test$JT <- ifelse(test$journal == "ca: a cancer journal for clinicians", "ca-a cancer journal for clinicians", test$JT)
test$journal <- ifelse(test$journal == "acta oncol", "acta oncologica", test$journal)
test$JT <- ifelse(test$JT == "ajnr. american journal of neuroradiology", "american journal of neuroradiology", test$JT)
test$JT <- stri_replace_all_regex(str = test$JT, pattern = "ajr. ", replacement = "")
test$JT <- ifelse(test$JT == "archives of dermatology", "archives of dermatological research", test$JT)
test$JT <- ifelse(test$JT == "jama", "jama-journal of the american medical association", test$JT)
test$journal <- ifelse(test$journal == "virchows arch", "virchows archiv", test$journal)
test$journal <- ifelse(test$journal == "lancet oncol", "lancet oncology", test$journal)
test$journal <- ifelse(test$journal == "lancet neurol", "lancet neurology", test$journal)
test$journal <- ifelse(test$journal == "lancet infect dis", "lancet infectious diseases", test$journal)
impacts$`Full Journal Title` <- stri_replace_all_regex(str = impacts$`Full Journal Title`, pattern = "qjm-an international journal of medicine", replacement = "qjm")
test$JT <- ifelse(substr(test$JT, 1, 3) == "the", sub("the ", "", test$JT), test$JT)
test$journal <- ifelse(substr(test$journal, 1, 3) == "the", sub("the ", "", test$journal), test$journal)
impacts$`Full Journal Title` <- ifelse(substr(impacts$`Full Journal Title`, 1, 3) == "the", sub("the ", "", impacts$`Full Journal Title`), impacts$`Full Journal Title`)
test$journal <- ifelse(substr(test$journal, 1, 2) == "j ", sub("j ", "journal of ", test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bthorac\\b", test$journal), gsub(pattern = "\\bthorac\\b", replacement = "thoracic", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\boncol\\b", test$journal), gsub(pattern = "\\boncol\\b", replacement = "oncology", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\binfect\\b", test$journal), gsub(pattern = "\\binfect\\b", replacement = "infectious", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bdis\\b", test$journal), gsub(pattern = "\\bdis\\b", replacement = "diseases", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bhematol\\b", test$journal), gsub(pattern = "\\bhematol\\b", replacement = "hematology", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bclin\\b", test$journal), gsub(pattern = "\\bclin\\b", replacement = "clinical", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bdiscov\\b", test$journal), gsub(pattern = "\\bdiscov\\b", replacement = "discovery", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bdermatol\\b", test$journal), gsub(pattern = "\\bdermatol\\b", replacement = "dermatology", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bnat\\b", test$journal), gsub(pattern = "\\bnat\\b", replacement = "nature", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bgenet\\b", test$journal), gsub(pattern = "\\bgenet\\b", replacement = "genetics", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bimmunol\\b", test$journal), gsub(pattern = "\\bimmunol\\b", replacement = "immunology", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bres\\b", test$journal), gsub(pattern = "\\bres\\b", replacement = "research", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bmol\\b", test$journal), gsub(pattern = "\\bmol\\b", replacement = "molecular", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bther\\b", test$journal), gsub(pattern = "\\bther\\b", replacement = "therapy", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bsurg\\b", test$journal), gsub(pattern = "\\bsurg\\b", replacement = "surgery", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bann\\b", test$journal), gsub(pattern = "\\bann\\b", replacement = "annals", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bintern\\b", test$journal), gsub(pattern = "\\bintern\\b", replacement = "internal", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bmed\\b", test$journal), gsub(pattern = "\\bmed\\b", replacement = "medicine", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\binteract\\b", test$journal), gsub(pattern = "\\binteract\\b", replacement = "interactive", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bcardiovasc\\b", test$journal), gsub(pattern = "\\bcardiovasc\\b", replacement = "cardiovascular", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bneurosurg\\b", test$journal), gsub(pattern = "\\bneurosurg\\b", replacement = "neurosurgery", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bendocrinol\\b", test$journal), gsub(pattern = "\\bendocrinol\\b", replacement = "endocrinology", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bmetab\\b", test$journal), gsub(pattern = "\\bmetab\\b", replacement = "metabolism", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bmod\\b", test$journal), gsub(pattern = "\\bmod\\b", replacement = "modern", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bpathol\\b", test$journal), gsub(pattern = "\\bpathol\\b", replacement = "pathology", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bneurosci\\b", test$journal), gsub(pattern = "\\bneurosci\\b", replacement = "neuroscience", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bhum\\b", test$journal), gsub(pattern = "\\bhum\\b", replacement = "human", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\boncol\\b", test$journal), gsub(pattern = "\\boncol\\b", replacement = "oncology", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bint\\b", test$journal), gsub(pattern = "\\bint\\b", replacement = "international", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\b j \\b", test$journal), gsub(pattern = "\\b j \\b", replacement = " journal of ", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\bam\\b", test$journal), gsub(pattern = "\\bam\\b", replacement = "american", x = test$journal), test$journal)
test$journal <- ifelse(grepl(pattern = "\\beur\\b", test$journal), gsub(pattern = "\\beur\\b", replacement = "european", x = test$journal), test$journal)
test$JT <- ifelse(test$JT == "journal of clinical oncology : official journal of the american society of clinical oncology", "journal of clinical oncology", test$JT)
impacts$`Full Journal Title` <- stri_replace_all_regex(str = impacts$`Full Journal Title`, pattern = "bmj-british medical journal", replacement = "british medical journal")

# removes between parantheses
test$journal <- gsub( " *\\(.*?\\) *", "", test$journal)
test$JT <- gsub( " *\\(.*?\\) *", "", test$JT)
impacts$`Full Journal Title` <- gsub( " *\\(.*?\\) *", "", impacts$`Full Journal Title`)
test$JT <- stri_replace_all_regex(test$JT, "bmj case reports", "british medical journal")
test$JT <- stri_replace_all_regex(test$JT, "bmj", "british medical journal")
impacts$`Full Journal Title` <- stri_replace_all_regex(impacts$`Full Journal Title`, "journal of bone and joint surgery-american volume", "journal of bone and joint surgery. american volume")
impacts$`Full Journal Title` <- stri_replace_all_regex(impacts$`Full Journal Title`, "otolaryngology-head and neck surgery", "otolaryngology--head and neck surgery")
test$JT <- stri_replace_all_regex(test$JT, "oral surgery oral medicine oral pathology and oral radiology", "oral surgery oral medicine oral pathology oral radiology")

# removes colons
test$journal <- gsub(":.*", "", test$journal)
test$JT <- gsub(":.*", "", test$JT)
impacts$`Full Journal Title` <- gsub(":.*", "", impacts$`Full Journal Title`)

# removes commas
impacts$`Full Journal Title` <- stri_replace_all_regex(str = impacts$`Full Journal Title`, pattern = ",", replacement = "")
test$journal <- stri_replace_all_regex(str = test$journal, pattern = ",", replacement = "")
test$JT <- stri_replace_all_regex(str = test$JT, pattern = ",", replacement = "")

# removes trailing white spaces
test$journal <- gsub(" $","", test$journal, perl = T)
test$JT <- gsub(" $","", test$JT, perl = T)
impacts$`Full Journal Title` <- gsub(" $","", impacts$`Full Journal Title`, perl = T)

impacts$`Full Journal Title` <- stri_replace_all_regex(str = impacts$`Full Journal Title`, pattern = "&", replacement = "and")
test$journal <- stri_replace_all_regex(str = test$journal, pattern = "&", replacement = "and")
test$JT <- stri_replace_all_regex(str = test$JT, pattern = "&", replacement = "and")

# merge factor with journal name
test <- left_join(test, impacts, by = c("JT" = "Full Journal Title"))
test <- left_join(test, impacts, by = c("journal" = "Full Journal Title"))
test <- test[ , -c(10, 12)]

# if one column is na, replace with other column
test$impact_factor <- ifelse(is.na(test$`Journal Impact Factor.x`), test$`Journal Impact Factor.y`, test$`Journal Impact Factor.x`)
test <- test[ , -c(10, 11)]

# change MH so only separated by commas
test$MH <- stri_replace_all_regex(test$MH, pattern = ", ", replacement = " ")
test$MH <- stri_replace_all_regex(test$MH, pattern = "\\*\\b", replacement = "")
test$MH <- stri_replace_all_regex(test$MH, pattern = "\\b/\\b", replacement = " ")
test$MH <- stri_replace_all_regex(test$MH, pattern = "&", replacement = "and")
test$MH <- stri_replace_all_regex(test$MH, pattern = "-", replacement = " ")
test$MH <- stri_replace_all_regex(test$MH, pattern = "_", replacement = ",")

# add new MH count variable
test$num_MH <- str_count(test$MH, pattern = ", ") + 1

# only unique rows
test <- subset(test, !duplicated(test$PMID))

# create multiple dataframes to merge later
cancer_0 <- test
remove(test)

save(cancer_0, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/John's Data/Merge All/cancer_0.Rdata")
