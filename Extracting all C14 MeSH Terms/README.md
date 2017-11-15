## **Extracting all C14 MeSH Terms**

### **Background**
For my R Shiny app, I compare MeSH terms between two population demographics. However, because this method looked at every single MeSH term mentioned in each case report, there were way too many comparisons to make. Our lab specializes in cardiovascular disease, so we chose to cut down the MeSH terms to only those related to cardiovascular disease. From https://meshb.nlm.nih.gov/treeView, you can see that it corresponds to the C14 heading.

### **Content**
At the time of this small project, I did not know how to do web scraping. Thus, I simply manually copied all of the C14 MeSH terms and saved them as a .txt file. I then did some regular expression to remove the unique identifiers of each MeSH term and to only extract the MeSH term. I have included the C14 .txt file as well.
