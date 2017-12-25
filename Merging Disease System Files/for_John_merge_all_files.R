library(dplyr)
library(stringi)


# read in all files
filenames <- list.files(path = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/John's Data/Merge All/", 
                        pattern = "*.Rdata", full.names = TRUE)

for (i in 1:31)
{
  load(filenames[i])
}

  # merge all files
hematological_0 <- hematology_0
hematological_10 <- hematology_10
endocrinological_0 <- endocrinology_0
endocrinological_10 <- endocrinology_10

all <- do.call("rbind", list(cancer_0, cancer_10, cardiovascular_0, cardiovascular_10, digestive_0, digestive_10,
                      endocrinological_0, endocrinological_10, hematological_0, hematological_10, infectious_0, infectious_10, 
                      musculoskeletal_and_rheumatic_0, musculoskeletal_and_rheumatic_10, nephrological_and_urological_0, 
                      nephrological_and_urological_10, neurological_0, neurological_10, obstetrical_and_gynecological_0, 
                      obstetrical_and_gynecological_10, ophthalmological_0, ophthalmological_10, oral_and_maxillofacial_0, oral_and_maxillofacial_10, otorhinolaryngological_0, 
                      otorhinolaryngological_10, respiratory_0, respiratory_10, trauma_0, trauma_10))

# save file
save(all, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/John's Data/Merge All/all.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/John's Data/Merge All/all.Rdata")

# merge by subjects
final <- all %>%
  group_by(PMID) %>% 
  dplyr::mutate(subject = toString(unique(subject))) %>% 
  distinct(PMID, .keep_all = TRUE)

# remove secondary journal name
final <- final[ , -c(8)]

# change word endings
final$subject <- stri_replace_all_regex(str = final$subject, pattern = "endocrinology", replacement = "endocrinological")
final$subject <- stri_replace_all_regex(str = final$subject, pattern = "hematology", replacement = "hematological")

# create new binary variables for each disease group
final$cancer <- ifelse(grepl(pattern = "cancer", x = final$subject, ignore.case = TRUE), 1, 0)
final$cardiovascular <- ifelse(grepl(pattern = "cardiovascular", x = final$subject, ignore.case = TRUE), 1, 0)
final$digestive <- ifelse(grepl(pattern = "digestive", x = final$subject, ignore.case = TRUE), 1, 0)
final$endocrinological <- ifelse(grepl(pattern = "endocrinological", x = final$subject, ignore.case = TRUE), 1, 0)
final$hematological <- ifelse(grepl(pattern = "hematological", x = final$subject, ignore.case = TRUE), 1, 0)
final$infectious <- ifelse(grepl(pattern = "infectious", x = final$subject, ignore.case = TRUE), 1, 0)
final$musculoskeletal_and_rheumatic <- ifelse(grepl(pattern = "musculoskeletal and rheumatic", x = final$subject, ignore.case = TRUE), 1, 0)
final$nephrological_and_urological <- ifelse(grepl(pattern = "nephrological and urological", x = final$subject, ignore.case = TRUE), 1, 0)
final$neurological <- ifelse(grepl(pattern = "neurological", x = final$subject, ignore.case = TRUE), 1, 0)
final$obstetrical_and_gynecological <- ifelse(grepl(pattern = "obstetrical and gynecological", x = final$subject, ignore.case = TRUE), 1, 0)
final$ophthalmological <- ifelse(grepl(pattern = "ophthalmological", x = final$subject, ignore.case = TRUE), 1, 0)
final$oral_and_maxillofacial <- ifelse(grepl(pattern = "oral and maxillofacial", x = final$subject, ignore.case = TRUE), 1, 0)
final$otorhinolaryngological <- ifelse(grepl(pattern = "otorhinolaryngological", x = final$subject, ignore.case = TRUE), 1, 0)
final$respiratory <- ifelse(grepl(pattern = "respiratory", x = final$subject, ignore.case = TRUE), 1, 0)
final$trauma <- ifelse(grepl(pattern = "trauma", x = final$subject, ignore.case = TRUE), 1, 0)

# change to binary/factor class
final$cancer <- as.factor(final$cancer)
final$cardiovascular <- as.factor(final$cardiovascular)
final$digestive <- as.factor(final$digestive)
final$endocrinological <- as.factor(final$endocrinological)
final$hematological <- as.factor(final$hematological)
final$infectious <- as.factor(final$infectious)
final$musculoskeletal_and_rheumatic <- as.factor(final$musculoskeletal_and_rheumatic)
final$nephrological_and_urological <- as.factor(final$nephrological_and_urological)
final$neurological <- as.factor(final$neurological)
final$obstetrical_and_gynecological <- as.factor(final$obstetrical_and_gynecological)
final$ophthalmological <- as.factor(final$ophthalmological)
final$oral_and_maxillofacial <- as.factor(final$oral_and_maxillofacial)
final$otorhinolaryngological <- as.factor(final$otorhinolaryngological)
final$respiratory <- as.factor(final$respiratory)
final$trauma <- as.factor(final$trauma)
  
# save file
save(final, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/John's Data/Merge All/all.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/John's Data/Merge All/all.Rdata")
