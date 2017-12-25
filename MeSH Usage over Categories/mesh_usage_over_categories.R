library(ggplot2)
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_merged_dataframe.Rdata")

output <- function(word, category)
{
  # inserts \\b in the beginning
  word <- paste0("\\b", word)
  category <- paste0("\\b", category)

  # inserts \\b at the end
  n <- nchar(word)
  word <- paste(substr(word, 1, n), "\\b", sep = "")
  m <- nchar(category)
  category <- paste(substr(category, 1, m), "\\b", sep = "")

  # indices of category
  cat_ind <- grep(pattern = category, x = total$MH, ignore.case = TRUE)
  
  # indices of word
  word_ind <- grep(pattern = word, x = total$MH, ignore.case = TRUE)
  
  # occurences of category
  cat_ind_length <- length(cat_ind)
  
  # proportion of word in category
  prop_word_in_cat <- (length(which(word_ind %in% cat_ind))/cat_ind_length)*100
  percent <- prop_word_in_cat
  
  # change names in df so it doesn't have \\b ......do it
  # cat_length <- length(category)
  #category <- substr(category, 4, )
  
  # create a dataframe that can have values added onto it to create plots
  row <- data.frame(percent, category, word)
  
  # change last two columns from factors to characters
  df[-1] <- lapply(df[-1], as.character)

  return (row)
}

# output(Male, Lung) # but should change to character
output("Male", "Lung")

df <- data.frame()

# enter here to bind to dataframe! (word, [within] category)
df <- rbind(df, output("Cardiomyopathy", "Male"))

# remove the \\b
df[-1] <- lapply(df[-1], gsub, pattern = "\\b", replacement = "", fixed = TRUE)

ggplot(df, aes(category, percent)) + 
  geom_bar(aes(fill = word), position = "dodge", stat = "identity") + 
  scale_x_discrete(name = "Category") + scale_y_continuous(name = "Percent", breaks = seq(0, 100, 10)) +
  ggtitle("% of Occurrence of Male vs Female in Cardiovascular Case Reports over Different MH terms")

# look at types of heart diseases under mesh term browser: https://meshb-prev.nlm.nih.gov/#/record/ui?ui=D006331
# DONE: remember learn how to read in arguments with spaces
# DONE: make sure caps case is not an issue 
# DONE: compare to lower case (done) but also read in using exact match using \\b stuff or ^
