# DDL_Internship_Project
This is the repository of my internship project at the DDL lab in Lyon.

My model mesure gender bias in Indeed french job offers.

# Contact
For more information about the research methodology and for questions regarding collaboration, please contact: bennadjifella@yahoo.fr & marc.allassonniere-tang@mnhn.fr

# Data
we scraped our data from [Indeed Canada](https://emplois.ca.indeed.com/) .
This data helped us calculate gender inequality in job offers

# Get started 
- In order to scrap data from Indeed Canada and Indeed France, please refer to [Scraping_ca.R](https://github.com/FellaBennadji/DDL_Internship_Project/blob/main/Scrap/Scraping_ca.R) and [Scraping_fr.R](https://github.com/FellaBennadji/DDL_Internship_Project/blob/main/Scrap/Scraping_fr.R) under [Scrap](https://github.com/FellaBennadji/DDL_Internship_Project/tree/main/Scrap) folder
- To analyse data, please refer to [Second_model.R](https://github.com/FellaBennadji/DDL_Internship_Project/blob/main/Models/Second_model.R) under [Models](https://github.com/FellaBennadji/DDL_Internship_Project/tree/main/Models). We have another model less performing under [First_model.R](https://github.com/FellaBennadji/DDL_Internship_Project/blob/main/Models/First_model.R)
- [Data_raw](https://github.com/FellaBennadji/DDL_Internship_Project/tree/main/Data_raw) contain our raw data: [Indeed_search_CA.csv](https://github.com/FellaBennadji/DDL_Internship_Project/blob/main/Data_raw/Indeed_search_CA.csv) and manually annotated data under [qualitative result.csv](https://github.com/FellaBennadji/DDL_Internship_Project/blob/main/Data_raw/qualitative%20result.csv)
- For the internship report, please check Internship_report

# Build in 
For this project, we used the following:

main tools :
- [R](https://www.r-project.org/) - Main language
- Excel

main libraries:
- Tidyverse
- Dyplr
- Stringr
- Xml2
- Rvest
- Ggplot2
- party

# Improvements
The extraction model rule-based of this project may be replaced in the futur by a ML model.
