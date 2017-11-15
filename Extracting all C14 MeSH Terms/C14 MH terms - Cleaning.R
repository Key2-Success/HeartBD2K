cv_MH <- read.delim(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/C14 MH terms.txt", header = FALSE)

# change to character
cv_MH$V1 <- as.character(cv_MH$V1)

# extract until the space before the [
for (i in 1:nrow(cv_MH))
{
  n <- unlist(gregexpr("\\[", cv_MH$V1[i]))-2
  cv_MH$V1[i] <- substr(cv_MH$V1[i], 1, n)  
}

# save all C14 MeSH terms
save(cv_MH, file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/cv_MH.Rdata")
