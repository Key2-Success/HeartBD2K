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
to do: find hclust or other way of measuring distance for heatmap
to do: learn if you can figure out how long a code will run before it's done executing
to do: compare multiple 10k (for RN) and 1k (for MH) runs to see if terms are consistent/sub is representative
to do: if mtx code too slow, filter out to cluster that matches demographics and manually calculate distance from each male and female to each MH
to do: look at MH usage over time
to do: do pca plot on demographic data
to do: do dtm correlation plot on all parsed
to do: do dtm correlation plot on demographic data
to do: fix named_RN parsed where all ) gets cut off
to do: error handling
to do: improve UI
to do: optimize speed
to do: change middle-aged to middle aged
to do: re-read in correct RN terms (those with ) at end are all getting cut off)
to do: fix "can't take in sample greater than population when replace = F" error
to do: add gif spinner when plots are loading
to do: get inspired by the ucla pubgraph project
to do: implement tabs for RN features too
to do: add confidence intervals on the jaccard index based on sample size
to do: extend left panel grey box to the bottom
to do: treemap on one page
to do: change scale of sliderInput for number of case reports
to do: make pca plot interactive so it can be hovered upon (plotly?)
to do: make sure the color of treemap is correct logically
to do: make sure the correlation plot is actually outputting top MeSH terms
to do: rewrite prevalence factors as "odds ratio"
to do: try plotly pca zoom in
to do: reduce redundancy via tree nodes
to do: use only single patients
to do: determine case report size needed for statistical significance based on number of entries in search
to do: topic modeling on MH to cluster/reduce
to do: remove searched MH terms in outputs (heart failure vs stroke)
to do: i think pca plot left side top slider controls both plots
to do: add in underscore for correlation plot to read in phrases
