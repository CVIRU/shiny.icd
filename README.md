##  Project: Shiny app using package 'icd'
### Authors: Davit Sargsyan, Yi Yang, Seunghoon Jung
### Created: 04/06/2018  

---

## Table of Contents
[Workflow](#flow)   
[Daily Logs](#log)   
[Results](#results)   
[References](#ref)   

## Workflow<a name="flow"></a>
1. ***app.R***: current version of the Shiny app    

## Daily Logs<a name="log"></a>
### 09/15/2018
* Isolated convert button; added selection of ICD and diagnoses grouping from the mapping file (select columns)

### 09/14/2018
* Completed conversion ICD-9 -> comorbidities; added a table and a download option.

### 07/21/2018
* Updated packages **icd** and ** icd.data** on CVI computer (left) to versions  3.2.1 and 1.0.1 respectively. NOTE: Cannot update it on teh server - **devtools** needed but must be installed by the admin as teh server is missing some Linux packages.      
* Switched to shiny Dashboard.          
* Added tabs: mapping and convert.            

### 06/01/2018
* Temporaty patch for DX 0413. See GitHub Issue 4.

### 05/25/2018
* Cleaned ICD-9 diagnosis and procedure codes for shiny app      
* Created *icd9_pcs_chapters.RData*, *icd9_pcs_chapters.RData* and *icd9_pcs_chapters.RData* data and [uploaded to GitHub](https://github.com/jackwasey/icd.data/issues/3)

### 05/24/2018
* Switched to icd Version 3.2.0 (developer) and icd.data version 1.0.1. Added functions to merge different versions of ICD data (currently, V23-V32). did the same for ICD-9 procedure codes sourcing form Excel files and manually creating chapters, etc.

### 04/27/2018
* Added ICD-9 procedure codes. NOTE: 'major' category is just a copy of 'sub-chapter', too many labels to create by hand. Find a full table online and use it.
* Opened new issue [#143](https://github.com/jackwasey/icd/issues/143) on J. Wasey's 'icd' GitHub page asking for procedure codes

### 04/06/2018
* Project created

## References<a name="ref"></a>
### ICD-9 Code Hierarchy
[icd9data.com](http://www.icd9data.com/2012/Volume3/default.htm)

### Data Source
[Center for Medicare and Medicaid](https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html)

## Package Source Code
[Jack O. Wasey's GitHub Page](https://github.com/jackwasey/icd)