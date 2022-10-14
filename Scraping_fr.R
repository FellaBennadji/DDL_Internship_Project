library(tidyverse)
library(rvest)
library(xml2)

page_result_start <- 70 # starting page 
page_result_end <- 90 # last page results / j'ai changé ici pour avoir plus de pages
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)

# set an empty data frame to store the output
full_df <- data.frame()

for(i in seq_along(page_results)) {
  
  # assign the url
  first_page_url <- "https://fr.indeed.com/jobs?q=secr%C3%A9taire&l=france&vjk=0ec64e282b50f5c6"
  
  url <- paste0(first_page_url, "&start=", page_results[i])
  # extract the page with results
  page <- xml2::read_html(url) 
  # Sys.sleep pauses R for two seconds before it resumes
  # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
  Sys.sleep(2)
  
  # get the job title
  job_title <- page %>% 
    # find the div chunk with the job title
    rvest::html_nodes(xpath = '//div[@class="heading4 color-text-primary singleLineTitle tapItem-gutter"]') %>%
    # find the span chunk with the job title
    rvest::html_nodes("span") %>%
    # extract the attribute of title
    rvest::html_attr("title")
  
  # removing NAs
  job_title <- job_title[!is.na(job_title)]
  
  
  # get the company name
  company_name <- page %>% 
    # extract the span chunk with company name
    rvest::html_nodes(xpath = '//span[@class="companyName"]')  %>% 
    # extract its text
    rvest::html_text() %>%
    # remove white spaces on both sides
    stringi::stri_trim_both() 
  
  
  # get job location
  job_location <- page %>% 
    # extract div chunk with company location
    rvest::html_nodes(xpath = '//div[@class="companyLocation"]')%>% 
    # extract its text
    rvest::html_text() %>%
    # remove white spaces on both sides
    stringi::stri_trim_both()
  
  
  # get links
  links <- page %>% 
    # extract the anchors 'a' chunks
    rvest::html_nodes("a") %>% 
    # extract the href link attributes
    html_attr("href") %>% 
    # only keep the one annotated as rc or company
    str_subset("^/company|^/pagead|^/rc")
  
  # create an empty vector to store the descriptions
  job_description <- c()
  # for each link of the results
  for(x in seq_along(links)) {
    # get the page
    url <- paste0("https://fr.indeed.com", links[x])
    page <- xml2::read_html(url)
    Sys.sleep(1)
    
    job_description[[x]] <- page %>%
      # find the div chunk with the job description
      rvest::html_nodes(xpath = '//div[@id="jobDescriptionText"]') %>%
      # extract its text
      rvest::html_text() %>%
      # remove white spaces on both sides
      stringi::stri_trim_both()
  }
  
  # save the links as a vector
  links <- paste0("https://fr.indeed.com", links)
  
  # combine the extracted information for the page of results
  df <- cbind(job_title, company_name, job_location, job_description, links) %>%
    as.data.frame()
  # combine the results from a page of results with the other pages of results
  full_df <- rbind(full_df, df)
}

# adjust the column names
colnames(full_df) <- c("Title","Company","Location","Description","Links")
full_df <- full_df %>% 
  mutate(across(everything(), as.character))

# saving the output
full_df %>% write.csv("data_raw/secretary.csv",
                      row.names = FALSE,
                      fileEncoding = "UTF-8")

###############################################################
# second loop for another job search

# set an empty data frame to store the output
full_df <- data.frame()

for(i in seq_along(page_results)) {
  
  # assign the url
  first_page_url <- "https://fr.indeed.com/jobs?q=conducteur%20routier&l=france&vjk=891e5086833c5e88"
  
  url <- paste0(first_page_url, "&start=", page_results[i])
  # extract the page with results
  page <- xml2::read_html(url) 
  # Sys.sleep pauses R for two seconds before it resumes
  # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
  Sys.sleep(2)
  
  # get the job title
  job_title <- page %>% 
    # find the div chunk with the job title
    rvest::html_nodes(xpath = '//div[@class="heading4 color-text-primary singleLineTitle tapItem-gutter"]') %>%
    # find the span chunk with the job title
    rvest::html_nodes("span") %>%
    # extract the attribute of title
    rvest::html_attr("title")
  
  # removing NAs
  job_title <- job_title[!is.na(job_title)]
  
  
  # get the company name
  company_name <- page %>% 
    # extract the span chunk with company name
    rvest::html_nodes(xpath = '//span[@class="companyName"]')  %>% 
    # extract its text
    rvest::html_text() %>%
    # remove white spaces on both sides
    stringi::stri_trim_both() 
  
  
  # get job location
  job_location <- page %>% 
    # extract div chunk with company location
    rvest::html_nodes(xpath = '//div[@class="companyLocation"]')%>% 
    # extract its text
    rvest::html_text() %>%
    # remove white spaces on both sides
    stringi::stri_trim_both()
  
  
  # get links
  links <- page %>% 
    # extract the anchors 'a' chunks
    rvest::html_nodes("a") %>% 
    # extract the href link attributes
    html_attr("href") %>% 
    # only keep the one annotated as rc or company
    str_subset("^/company|^/pagead|^/rc")
  
  # create an empty vector to store the descriptions
  job_description <- c()
  # for each link of the results
  for(x in seq_along(links)) {
    # get the page
    url <- paste0("https://fr.indeed.com", links[x])
    page <- xml2::read_html(url)
    Sys.sleep(1)
    
    job_description[[x]] <- page %>%
      # find the div chunk with the job description
      rvest::html_nodes(xpath = '//div[@id="jobDescriptionText"]') %>%
      # extract its text
      rvest::html_text() %>%
      # remove white spaces on both sides
      stringi::stri_trim_both()
  }
  
  # save the links as a vector
  links <- paste0("https://fr.indeed.com", links)
  
  # combine the extracted information for the page of results
  df <- cbind(job_title, company_name, job_location, job_description, links) %>%
    as.data.frame()
  # combine the results from a page of results with the other pages of results
  full_df <- rbind(full_df, df)
}


# adjust the column names
colnames(full_df) <- c("Title","Company","Location","Description","Links")
full_df <- full_df %>% 
  mutate(across(everything(), as.character))


# visual check
view(full_df)


# saving the output
full_df %>% write.csv(paste0("data_raw/driver.csv"),
                      row.names = FALSE,
                      fileEncoding = "UTF-8")
