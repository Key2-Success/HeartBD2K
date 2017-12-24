# Reading in large MEDLINE files into R

## Background
My goal in this lab was to analyze cardiovascular case reports, which are all in MEDLINE formats. Therefore, I had to devise a way to convert MEDLINE files (which are in .txt format) into .Rdata. This data comes from https://www.ncbi.nlm.nih.gov/pubmed/?term=cardiovascular+case+reports#, where you must "Send to --> File --> Format (MEDLINE), Sort by (Most Recent) --> Create File."

## Content
I have included my code to complete the above task, as well as a sample .txt file which shows how the MEDLINE format is in a very unanalyzable format. Interestingly, the first time I attempted this project, I was a beginner-intermediate in R, and thus it took me 3 weeks. Over the summer, when I had to read in larger datasets, my code was not able to handle it. Within a day, I was able to optimize my solution to handle big data.
