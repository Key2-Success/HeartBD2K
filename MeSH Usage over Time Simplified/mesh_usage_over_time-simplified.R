load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_merged_dataframe.Rdata")

# since functions work individually, try to combine all into one function
phrasearch <- function(word)
{
  # counts search frequency
  # converts argument into character
  count_test <- word #deparse(substitute(word))
  
  # will give result in tabular format. now just need to extract
  a <- with(total, lengths(by(MH, EDAT_Year, grep, pattern = count_test)))
  
  # everything below just extracts the data and removes year.
  # table function will say how many times to use the for loop
  num_years <- table(total$EDAT_Year)
  
  # create a new vector of length for # years to store ans in
  b <- length(num_years)
  values <- numeric(b)
  
  for (i in 1:length(num_years))
  {
    values[i] <- a[[i]]
  }
  
  # counts total years
  # create vector that will contain values of length of num years
  c <- length(num_years)
  values2 <- numeric(c)
  
  # loops through the table that contains number of years
  for (i in 1:length(num_years))
  {
    values2[i] <- table(total$EDAT_Year)[[i]]       
  }
  
  # counts percent
  # create vector
  percent <- vector()
  percent <- values/values2
  
  # plots graph
  # plot graph of years vs percent
  plot(x = c(1978, 1980, 1983), y = values/values2, xlab = "Year", ylab = "Percent", 
       main = paste("% of Occurrence of --- in Cardiovascular Case Reports over Time"),
       type = "b")  
}

# enter here and re-run entire code!
phrasearch(Aneurysm)
phrasearch("Cardiac Output")

# to do:
# DONE: read in function w space in argument
# read in all years manually and then by separating if too long and then try this function
# DONE: change x variable to categorical/diseases aka find index of articles that contain drug or disease name
# DONE: find within those indices which contain the symptom
# can even do this over time but first do separately
# use t-tests? tukey method? to find those statistically significant - or mo's method where you compare to avg?