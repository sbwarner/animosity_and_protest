# animosity_and_protest
Replication materials for "Partisan Animosity and Protest Participation in the United States"

The files here can be used, in conjunction with publicly-available data, to reproduce all figures and tables from the main text of the article. 

Specifically, this repository contains six scripts in the R programming language:

*anes_panels - script to reproduce Table 1, which analyses the 2016-2020 ANES panel
*pew_panels - script to reproduce Models 1-2 in Table 2, which use the Pew American Trends Panel waves 53, 68, and 89.
*pew_teaparty - script to reproduce Model 3 in Table 3, which uses the 2014 Pew Political Typology Survey.
*maps - script to reproduce Figure 1, which maps partisan animosity and identity in US counties using the contextual_vars.RData file
*ces_collation - script to collate the 2018, 2020, and 2022 waves of the Cooperative Election Survey and prepare key variables
*ces_models - script to reproduce Table 3, which uses the CES data to model contextual effect of partisan animosity on protest

It also contains two data files:

*contextual_vars.RData - an R environment with information on partisan makeup and levels of partisan identity by US county in 2017, 2019, and 2021 as described in the main text
*zip2county.csv - a data crosswalk to be used with the ces_collation script that matches respondent's ZIP codes to their county of residence

The survey data to be used with these scripts should be obtained from their original sources. Users can find them at the following websites:
*https://electionstudies.org/data-center/2016-2020-panel-merged-file/
*https://www.pewresearch.org/american-trends-panel-datasets/
*https://www.pewresearch.org/dataset/2014-political-polarization-survey/
*https://dataverse.harvard.edu/dataverse/cces
