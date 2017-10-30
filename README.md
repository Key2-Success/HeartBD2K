# HeartBD2K
All of my projects from my data science research internship at the HeartBD2K lab at UCLA are contained here.

### **R Shiny App**

**Background**
This project was essentially a culmination of my summer internship. This site allows a clinician, for instance, to compare two different demographics and find which drugs (RN terms) and MeSH terms (which include phrases such as diseases, demographics, and symptoms) are most statistically distinct between each demographic. The purpose of this project was to centralize all of the back-end analyzing, instead of manually running scrips for each demographic of interest.

**Content**
The resulting website includes the following tabs
- *Home* to input the two demographics as well as more information about the project
- *Heat Maps* to show which MesH and RN terms are statistically distinct between each demographic and by how much
- *Tree Maps* to represent the top MeSH terms within each demographic by size and color (color represents the distinctness to that search)
- *PCA Plots* to spatially visualize how similar each MeSH term is to each other within each demographic
- *Correlation Plots* to demonstrate how MeSH terms are correlated within each demographic

I have included screenshots of the website of each tab in the `screenshots` folder.

**Future Work**
# done: scale up RN frequency in MH_RN variable and try pca plot
# done: try correlation matrix/corrgram
# done: heat map between RN and MH
# done: in PCA plot, change color for MH and RN distinction
# done: read in multiple words for a phrase for MH and RN (try q_dtm [has keep.hyphen = TRUE], in gofastr) (try https://stackoverflow.com/questions/24038498/corpus-build-with-phrases)
# done: for fun and giggles, ^ replace with I or some letter and try it out since special characters are annoying
# done: read in 500k case reports and filter out by missing RN files
# to do: find hclust or other way of measuring distance for heatmap
# done: read in 500k maybe by filtering it down to only PMID, RN, and MH
# done: look into sql/sql lite or data.table/dplyr
# done: make heat map that has gender/demographics on x axis and on y-axis top MH or RN terms (2 different plots)
# done: make heat map ^ but two different ones based on distinct MH/RN for each gender
# done: use either euclidean distance from PCA plot for each gender or do assoc() to do heat map
# done: by july 10/11 have heat map read
# done: find statistical significance...if 4 counts of aspiring and 3 in male...this is not significant...maybe use value from heat map and use 2 sample mean t-test
# to do: try running recreating labels without the n > 1 if statement
# to do: try running recreating labels w stringi instead of sub
# to do: learn if you can figure out how long a code will run before it's done executing
# done: read in entire 24 million obs into dataframe overnight
# done: trim down dataframe by removing 0 () RN terms (general ones)
# done: create an image more representative of the work we're doing
# done: read in the 90k test (from the reduced RN terms) 10k at a time
# done: instead of using 2 mean prop z test on sample 10k data do it on full one
# to do: compare multiple 10k (for RN) and 1k (for MH) runs to see if terms are consistent/sub is representative
# to do: if mtx code too slow, filter out to cluster that matches demographics and manually calculate distance from each male and female to each MH
# done: output as svg and send to harry
# done: send numerical and process of all graphs to harry as well
# done: enlarge y-axis text size on graphs
# done: for MH terms, compare against 500 k one
# done: use feature reduction for MH terms to only use cardiovascular MH terms
# done: do ^ by using MH tree syntax for which you need to ask harry about
# done: use the 5607 MH terms created by 3000 case reports to do the RN procedure on
# done: be able to create a vector of every single MH or RN term instead of PCA/mtx and find frequency (harry suggests removing most and least frequent)
# done: if none of those help, then just create another 3000 case report subset and compare MH terms
# done: analyze what exactly the 3000 case report part does what is it choosing exactly...first? top?
# done: may have to create another 10k case report for RN too if can't create vector (just ask on stackoverflow)
# done: organize all your files in folder
# done: organize what each file contains (which steps) as well as what each dataframe is
# done: create R script to read in only cardiovascular MH terms
# done: read in all MH terms 3k at a time until the 500k l o l
# done: do heat map/demographic data on age and gender combined (female middle aged, male infant, etc)
# done: look if any of the MH/RN terms are highly correlated for instance infant and pregnancy
# done: add age demographic (infant, child, adolescent, young adult, middle aged adult aged adult) as separate categories
# done: combine ^ age demographics with gender demographics
# done: look at diabetes mellitus vs non diabetes mellitus patients (also combine other demographics)
# done: look at obesity MH terms as well (and combine other demographics)
# done: look into known cardiovascular cofactors
# done: be able to identify if two terms are related to each other, find another term that's also related to them both (use the hierarchial clustering plot you've made)
# done: these heat maps may have to be separate or combined depending on aesthetics and top MH terms
# done: do all the above demographics with RN terms too 
# to do: look at MH usage over time
# done: look at 6 age groups within female and male's MH terms (do prior heat map against gender and age groups as well as all combined as well as dtm correlation plot on each subgroup)
# to do: do do pca plot on all parsed
# to do: do pca plot on demographic data
# to do: do dtm correlation plot on all parsed
# to do: do dtm correlation plot on demographic data
# done: add in adult demographic (that doesn't include young adult)
# done: read in dtm for RN - male aged
# done: re-do MH for female aged and its plot bc I think you used male and check other ages too
# done: check if inf messed up MH values bc it wasn't changed to -inf for -inf values
# done: look into dif RN/MH dictionaries
# done: re-do current RN script on child, infant (takes into account the " and the ')
# to do: remove cases with NAs in both variables
# to do: fix named_RN parsed where all ) gets cut off
# to do: allow user to choose # case reports, MH terms, and RN terms
# to do: add more plots on different tabs
# to do: try out sankey diagram
# to do: try out cosine similarity 
# to do: jaccard index on the two subsets
# to do: error handling
# to do: improve UI
# to do: optimize speed
# to do: change middle-aged to middle aged
# to do: re-read in correct RN terms (those with ) at end are all getting cut off)
# to do: fix "can't take in sample greater than population when replace = F" error
# to do: add gif spinner when plots are loading
# to do: get inspired by the ucla pubgraph project
# to do: implement tabs for RN features too
# to do: look into tree maps and use color and size feature
# to do: look into riverplot/sankey images
# to do: add confidence intervals on the jaccard index lol based on sample size
# to do: extend left panel grey box to the bottom
# to do: treemap on one page
# to do: change scale of sliderInput for number of case reports
# done: remove "doc" from PCA plot tab
# to do: make pca plot interactive so it can be hovered upon (plotly?)
# to do: make sure the color of treemap is correct logically
# to do: make sure the correlation plot is actually outputting top MeSH terms??
# to do: rewrite as "odds ratio"
# to do: plotly pca zoom in 
# to do: reduce redundancy by tree nodes
# to do: only single patients
# to do: determine case report size based on number of entries in search
# to do: topic modeling on MH 
# to do: remove searched MH terms in outputs (heart failure vs stroke)
# to do: i think pca plot left side top slider controls both plots oops
# to do: for david create MH heat map for two disease comparison as well as hf preserved between genders
# to do: add in underscore for correlation plot

