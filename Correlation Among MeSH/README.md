## **Correlation Among MeSH**

### **Background**
Each case report is associated with multiple MeSH (MEdical Subject Headings...examples include patient demographics, symptoms, diagnosis, treatments, etc) terms. The goal of this project was to cluster similar MeSH terms to visualize with MeSH terms are related to each other.

### **Content**
The code is a bit messy, because I was trying out multiple methods. I first created a corpus of all of the MeSH terms, and then converted it into a document-term matrix. I played around with some of the functions from the `tm` package, and then finally made a correlation plot of the MeSH terms. The thicker the lines between two MeSH terms, the higher the correlation - but this is also demonstrated by its coefficient value (which, to interpret, means, for an arbitrary example: "The MeSH term pregnancy appears with the MeSH term complications 60% of the time (extracted from the value 0.6, where the value is a range from 0 to 1)." In addition, there is a PCA plot created via the `ggplot2` package, that clusters the most similar MeSH terms via K-means clustering. We visualize on two dimensions for simplicity's sake. I have also included the sample dataframe I was using for this particular code.
