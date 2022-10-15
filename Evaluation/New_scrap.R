library(tidyverse)
library(rvest)
library(xml2)

page_result_start <- 10 # starting page 
page_result_end <- 100 # last page results
page_results <- seq(from = page_result_start, to = page_result_end, by = 10)

# set an empty data frame to store the output
full_df <- NULL %>% data.frame()

# create a vector of the professions to look at
professions <- c("director")

# create a vector of the links
professions.links <- c("https://emplois.ca.indeed.com/emplois?q=DIRECTEUR&l=Canada&from=searchOnHP&redirected=1&vjk=380c5938f1e5a521")


# loop for professions
for(z in 1:length(professions)){
  
  # loop for page results
  for(i in seq_along(page_results)) {
    
    # assign the url
    first_page_url <- professions.links[z]
    
    url <- paste0(first_page_url, "&start=", page_results[i])
    # extract the page with results
    page <- xml2::read_html(url) 
    # Sys.sleep pauses R for two seconds before it resumes
    # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
    Sys.sleep(2)
    
    # get the job title
    job_title <- page %>% 
      # find the div chunk with the job title
      rvest::html_nodes(xpath = '//div[@class="job_seen_beacon"]') %>%
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
    
    
    # combine the extracted information for the page of results
    df <- cbind(# adding which main job it is 
      professions[z],
      # adding extracted info
      job_title, company_name, job_location) %>%
      as.data.frame()
    
    # add an if statement to avoid empty data
    if(ncol(df) == 4){
      # combine the results from a page of results with the other pages of results
      full_df <- rbind(full_df, df)
    }else{
      # do nothing if there is not data from that page
    }
  } # loop for page results
  
} # loop for professions

# adjust the column names
colnames(full_df) <- c("Profession","Title","Company","Location")
# adjust the character format
full_df <- full_df %>% 
  mutate(across(everything(), as.character))


# visual check
view(full_df)


# saving the output
full_df %>% write.csv("Indeed_director.csv",
                      row.names = FALSE,
                      fileEncoding = "UTF-8")
