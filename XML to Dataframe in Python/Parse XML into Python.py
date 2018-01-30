# import necessary libraries
import xml.etree.cElementTree as etree
import pandas as pd

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

# convert list to nested lists so that each nested list is for a unique PMID
# PMID with abastract
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
    
    
# how many case reports have abstracts (not just empty PMID)
at_least_one = 0
for pmid in final_abstract:
    if (len(pmid) > 1):
        at_least_one += 1
    

# convert list to a dataframe
# abstract
abstract = pd.DataFrame({"col":final_abstract})
abstract["PMID"] = abstract.col.str.extract("(^\d*)")
abstract["col"][1].list.extract("(^\d*)")

# MH
MH = pd.DataFrame({"col":final_MH})
MH["PMID"] = MH.col.str.extract("(^\d*)")
MH["col"][1].list.extract("(^\d*)")

# split nested lists into multiple variables

# find max # of nested lists
max = 0
for index, row in abstract.iterrows():
    if (len(abstract["col"][index]) > max):
        max = len(abstract["col"][index])
        
# add PMID variable
# abstract
for index, row in abstract.iterrows():
    abstract["PMID"][index] = abstract["col"][index][0]
    
# MH
for index, row in MH.iterrows():
    MH["PMID"][index] = MH["col"][index][0]
    
    
# create abstract variable
abstract["abstract"] == ""
temp_abstract = ""
for index, row in abstract.iterrows():
    for list_val in range(0, len(abstract["col"][index])):
        if "." in abstract["col"][index][list_val]:
            temp_abstract += abstract["col"][index][list_val]
    abstract["abstract"][index] = temp_abstract
    temp_abstract = ""
    
# create MH variable
MH["MH"] == ""
temp_MH = ""
for index, row in MH.iterrows():
    for list_val in range(0, len(MH["col"][index])):
        if "." in MH["col"][index][list_val]:
            temp_MH += MH["col"][index][list_val]
    MH["MH"][index] = temp_MH
    temp_MH = ""
    

