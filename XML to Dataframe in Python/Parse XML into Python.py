# import necessary libraries
import xml.etree.cElementTree as etree
import pandas as pd


# ----- READING IN XML -----

# read in all PMIDs, abstracts, and MH list wise
# PMID and abstract
pmid_abstract = []
for event, element in etree.iterparse("pubmed_result.xml"):
    if element.tag in ["PMID", "AbstractText"]:
        pmid_abstract.append(element.text)
    element.clear()
    
# PMID and MH
pmid_MH = []
for event, element in etree.iterparse("pubmed_result.xml"):
    if element.tag in ["PMID", "DescriptorName"]:
        pmid_MH.append(element.text)
    element.clear()
    

# view a bit of the lists
pmid_abstract[:200]
pmid_MH[:200]

# ----- CONVERT FROM LISTS TO NESTED LISTS -----

# convert list to nested lists so that each nested list is for a unique PMID
# PMID with abstract
final_abstract = []

for x in pmid_abstract:
    if x.isdigit():
        sub = [x]
        final_abstract.append(sub)
    else:
        sub.append(x)
        
# PMID with MH
final_MH = []

for x in pmid_MH:
    if x.isdigit():
        sub = [x]
        final_MH.append(sub)
    else:
        sub.append(x)
      
        
# view a bit of the lists
final_abstract[:200]
final_MH[1000:2000]


# how many case reports have abstracts (not just empty PMID)
at_least_one = 0
for pmid in final_abstract:
    if (len(pmid) > 1):
        at_least_one += 1 # 4602 case reports with abstracts

    
# ----- CONVERT FROM NESTED LISTS TO DATAFRAME -----

# convert list to a dataframe and extract PMID
# abstract
abstract = pd.DataFrame({"col":final_abstract})
abstract["PMID"] = ""

for index, row in abstract.iterrows():
    abstract["PMID"][index] = abstract["col"][index][0]
    
# MH
MH = pd.DataFrame({"all":final_MH})
MH["PMID"] = ""

for index, row in MH.iterrows():
    MH["PMID"][index] = MH["col"][index][0]
    
# exploratory analysis only: finds max # of nested lists
max = 0
for index, row in abstract.iterrows():
    if (len(abstract["col"][index]) > max):
        max = len(abstract["col"][index])
        
    
# ----- CLEAN UP DATAFRAME -----

# remove 1st list (PMID) and join all strings together
# abstract
for index, row in abstract.iterrows():
    del abstract["col"][index][0]
for index, row in abstract.iterrows():
    abstract["col"][index] = ''.join(abstract["col"][index])

# MH
for index, row in MH.iterrows():
    del MH["col"][index][0]
for index, row in MH.iterrows():
    MH["col"][index] = ''.join(MH["col"][index])


# ----- MERGE ABSTRACT AND MH -----
data = abstract.join(MH, on = ["PMID", "PMID"])
