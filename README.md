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
### 04/27/2018
* Added ICD-9 procedure codes. NOTE: 'major' category is just a copy of 'sub-chapter', too many labels to create by hand. Find a full table online and use it.
* Opend new issue (#143)[https://github.com/jackwasey/icd/issues/143] on J. Wasey's 'icd' GitHub page asking for procedure codes

### 04/06/2018
* Project created

## References<a name="ref"></a>
### ICD-9 Code Hierarchy
(icd9data.com)[http://www.icd9data.com/2012/Volume3/default.htm]

### Data Source
(Center for Medicare and Medicaid)[https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html]

## Package Source Code
(Jack O. Wasey's GitHub Page)[https://github.com/jackwasey/icd]