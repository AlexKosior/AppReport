---
title: "AppReport Documetation"
author: "Alex Kosior"
date: "October 22, 2015"
output: html_document
---
 
##Documentation

```
## [1] "Disclaimer: The AppReport is a property of Jovago.com, Africa Internet Group. I am merely its author."
```
   
  The AppReport documentation is a document, which describes all the processes and steps taken, while producing the App Report at Jovago.com. 

This documentation contains six chapters, which describe all of the codes and results produced by the report. The following are included the app report and consequently in the documentation.

5. Basic information
1. By device distribution
2. Landing page
3. Exit page
4. Funnel
6. Helpful people who enabled the building of this Report and its documentation.

##Basic information
####The data
The App Report is based on the data collected by the application Jovago.com Hotel Booking, collected by Google Big Query and downloaded into the table on mySQL server thanks to Joao. The table name is INSERT TABLE NAME. 

The data in the table is downloaded by the SQL query, also written by Joao. You can find it below.


```r
print("INSERT SQL QUERY HERE")
```

```
## [1] "INSERT SQL QUERY HERE"
```

####The software

The creation of the AppReport was conducted through the following software:
-MS Windows 8.1 x64
-R 3.2.2 x64
-R Studio

####The programming languages  
-R
-jsQuery

####The libraries  
In order to create the report and perform the analysis, the usage of the following packages is required.

-Shiny (shiny) - which renders tables and enables publishing in the web
-reshape2 - which enables the building of the pivot tables
-some SQL LIBRARY INSERT THE CORRECT NAME
-knitr - which converts R markdown into Markdown and enables HTML conversion
