#This script is to scape the new Google Trends data from Google Trends 

#get libraries
library(tidyverse)
        library(gtrendsR)

#get the terms that I want to extract

        illness_adjectives <- c("coughing", "itch", "diarrhoea", "lame")
        pet_nouns <- c("dog", "puppies")

#save in a dataframe and create column of full term names
        terms <- expand.grid(pet_nouns = pet_nouns, terms = illness_adjectives) %>%
                 mutate(search_term = paste(pet_nouns, terms)) %>%
                 select(search_term)


#Function that runs gtrends query based on each term in df above and 
#Stores date from iot, ibr and ibc dataframes 

        get_gt_data <- function(search_term, geo ="GB", start_date="2010-01-01", end_date = "2019-01-01") {
                
                #get time period in format that gtrends function wants
                time_period <- paste(start_date, end_date, collapse = " ")
                
                #run the gtrends function over the current term in dataframe given
                results <- gtrends(search_term[1], geo=geo, time=time_period)
                
                #If the dataframes are not null extract the relevant data and save a new column with 
                #the type of datframe it is storing (e.g interest over time). This will
                #allow the dataset to be easily subsetted
                if(is.null(results$interest_over_time) == FALSE) {
                        iot_results <- results$interest_over_time %>%
                                mutate(result_type = "interest_over_time",
                                       variable = as.character(date)) %>%
                                select(result_type, variable, hits, keyword)
                }
                
                if(is.null(results$interest_by_region) == FALSE) {
                        ibr_results <- results$interest_by_region %>%
                                mutate(result_type = "interest_by_region") %>%
                                select(result_type, variable = location, hits, keyword)
                }
                 
                if(is.null(results$interest_by_city) == FALSE) {
                        ibc_results <- results$interest_by_city %>%
                                mutate(result_type = "interest_by_city") %>%
                                select(result_type, variable = location, hits, keyword)
                }
                
                #if the dataframes are null create a dataframe with the same structure
                #but containing NAs so it will bind properly
                if(is.null(results$interest_over_time) == TRUE) {
                        iot_results <- data_frame(result_type = "interest_over_time",
                                                  variable = NA, 
                                                  hits = NA, 
                                                  keyword = search_term[1])
                }
                
                if(is.null(results$interest_by_region) == TRUE) {
                        ibr_results <- data_frame(result_type = "interest_by_region",
                                                  variable = NA, 
                                                  hits = NA, 
                                                  keyword = search_term[1])
                }
                
                if(is.null(results$interest_by_city) == TRUE) {
                        ibc_results <- data_frame(result_type = "interest_by_city",
                                                 variable = NA, 
                                                 hits = NA, 
                                                 keyword = search_term[1])
                }
                
                #bind the dataframes together into one and return the results        
                all_results <- rbind(iot_results, ibr_results, ibc_results)
                return(all_results)
        }

#Map the function above to the datframe of search terms that I created
        gt_data <- terms %>%
                pmap_df(., get_gt_data)

#create a filename with today's date and save the csv
        data_filename <- paste("trends_data/gt-data-", Sys.Date(), ".csv", sep = "")
        write_csv(gt_data, data_filename)






