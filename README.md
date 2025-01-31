# Occupation_Recode

This is the repository for our work coding 2019 and 2021 occupations. 

The data folder includes: 
1. Original stata files from the Canada election studies.
2. The unique occupations from each of the CES files. 
3. Files ending in _noc that have been merged with our NOC codes.  These are the files that can be remerged back into any original CES file (2019 phone, 2019 web and 2021)
4. Files that scraped the NOC job titles (4-digit and 5-digit) as well as sample job titles and job descriptions (in French and English)
5. Files that have all the unique occupations pulled from the three CES files. 
5. A file that has our assigned NOC codes (occupations_coded.xlsx)
5. A file that has our assigned NOC codes as well as clear government employees added in (e.g. occupations_coded_government.xlsx) 

The R Scripts are actually quite simple. 

`1_data_import.R` Imports all the files 
`1_problem_with_encodings.R` is a beast. Somewhere in the 2019 Web stata file from the CES is a botched character encoding that can cause hellish problems. Be wary. But the code in there is useful.
`2_get_NOC_codes.R` and `3_get_NOC_codes.R` are I think the same thing. One day I will check that out. They scrape the NOC codes and the sample job titles for each NOC codes.
`3_get_NOC_codes.R` and `3_get_NOC_codes_descriptions.R` might also be equivalent.
`2_merge_diagnose_execute.R` imports the coded occupations file, and merges it with each CES data set and runs some tests and then spits out `.rdata`, `.sav` and `.dta` files.
