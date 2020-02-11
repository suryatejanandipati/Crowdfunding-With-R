rm(list = ls()) ##removes everything from global enviroment
library(readr)
library(janitor)
library(tidyverse)
library(magrittr)
library(lubridate)
library(rworldmap)    ## map
library(classInt)     ## map: create intervals
library(RColorBrewer) ## map: color palette



fp_data <- read_csv("ks-projects-201801.csv") ## read csv into var

fp_data$deadline <- mdy(fp_data$deadline) #changing column from character to date

#this doesn't work
#fp_data$name  %>%  parse_character(.,locale=locale(encoding="windows-1250"))#trying to removing the 
#special characters from name variable

fp_data$name <- gsub("[^\x20-\x7E]", "", fp_data$name) #removes all non english characters

fp_data$name  %>% guess_encoding(.) #testing to see what character set is in the tibble, only ASCII :)

##########################################################################################################
fp_data2 <- fp_data %>%
  separate(., launched, into = c("launched_date", "launched_time"), sep = " ")# separated the launched 
#variable into launched date and time
fp_data2$launched_date <- mdy(fp_data2$launched_date) #changing column from character to date

fp_data2$launched_time <- parse_time(fp_data2$launched_time) # change to time

##268758 Id:437647360 has Quotes and comma between country variable
fp_data2$country <- gsub(",","",fp_data2$country) # Removed comma 
fp_data2$country <- substr(fp_data2$country,1,2)  #removed Quotes


###################################################################################
# Running summaries on the N0 observations
table(fp_data2$country) # 3797 with N0

fp_data3 <- fp_data2 %>% filter(country !="N0")  # create dataset without N0
N0_data <- fp_data2 %>% filter(country == "N0")  # create dataset of N0

table(fp_data3$state) # no undefined in dataset without N0

table(N0_data$state) # 3562 undefined, probably want to remove those observations, but keep rest
table(N0_data$currency)

fp_data4 <- fp_data2 %>% filter(state != "undefined") # removed undefined rows

table(fp_data4$country) # 235 N0

fp_data5 <- fp_data4 %>% filter(country != "N0") # remove N0
table(fp_data5$country)

N0_data2 <- fp_data4 %>% filter(country == "N0")  # create dataset of N0

table(N0_data2$state)

# Try to match currency to country, need full name of currency
# currency info: https://www.easymarkets.com/int/learn-centre/discover-trading/currency-acronyms-and-abbreviations/
currency <- read_csv("currency.csv")

# Need to join country_names2 with fp_data
currency %>% 
  count(`Acronym/AbbreviationAcr./Abb.`) %>%
  filter(n > 1)

# Don't have a primary key
currency$`Country/Currency`[duplicated(currency$`Country/Currency`)] # 6 Duplicates
currency2 <- currency[!duplicated(currency$`Country/Currency`), ]   # Remove duplicates

# Now have primary key
currency2 %>% 
  count(`Acronym/AbbreviationAcr./Abb.`) %>%
  filter(n > 1)

N0_data2 %>%
  count(`currency`) %>%
  filter(n > 1)

N0_data3 <- N0_data2 %>%
  left_join(currency2, by = c("currency" = "Acronym/AbbreviationAcr./Abb."))


table(N0_data3$`Country/Currency`) # 24 Euro, won't be able to match exactly

N0_data4 <- N0_data3 %>% filter(currency != "EUR") # remove observations with euros
  
# Need to do a for loop to replace N0 with country name
for(i in 1:length(N0_data4$ID)) {
 if(N0_data4$currency[i] == "USD") N0_data4$country[i] = "US"
 else if (N0_data4$currency[i] == "GBP") N0_data4$country[i] = "GB"
 else if (N0_data4$currency[i] == "AUD") N0_data4$country[i] = "AU"
 else if (N0_data4$currency[i] == "CAD") N0_data4$country[i] = "CA"
 else if (N0_data4$currency[i] == "NOK") N0_data4$country[i] = "NO"
 else if (N0_data4$currency[i] == "DKK") N0_data4$country[i] = "DK"
 else N0_data4$country[i] = "SE"
}

table(N0_data4$country) # no more N0
N0_data5 <- N0_data4 %>% select(-"Country/Currency") #remove last column

# Join updated N0 countries to full data set
fp_data6 <- rbind(N0_data5, fp_data5)

table(fp_data6$country)
table(fp_data6$launched_date)

fp_data7 <- fp_data6 %>% filter(., launched_date > 1980)
fp_data8 <- fp_data7 %>% select(-pledged, -launched_time, -`usd pledged`, -category)
fp_data8$days_between_goal <- as.numeric(fp_data8$deadline - fp_data8$launched_date)
fp_data8$under_over_goal <- fp_data8$usd_pledged_real - fp_data8$usd_goal_real
fp_data8$month <- as.numeric(substring(fp_data8$launched_date, 6, 7))


#write_csv(fp_data6,"project.csv") ##exported into csv file.
write_csv(fp_data8,"project_final.csv") ##exported into csv file.

###########################################
# checking how many NAs there are by variable
# NA_table <- fp_data6 %>% 
#   summarize(ID_na = sum(is.na(ID)),
#             name_na = sum(is.na(name)),
#             category_na = sum(is.na(category)),
#             main_cat_na = sum(is.na(main_category)),
#             currency_na = sum(is.na(currency)),
#             deadline_na = sum(is.na(deadline)),
#             gaol_na = sum(is.na(goal)),
#             launch_date_na = sum(is.na(launched_date)),
#             launch_time_na = sum(is.na(launched_time)),
#             pledged_na = sum(is.na(pledged)),
#             state_na = sum(is.na(state)),
#             back_na = sum(is.na(backers)),
#             country_na = sum(is.na(country)),
#             usd_pl_na = sum(is.na(`usd pledged`)),
#             usd_real_na = sum(is.na(usd_pledged_real)),
#             usd_goal_na = sum(is.na(usd_goal_real)))

# 4 observations don't have names for the projects
# 211 have NA on usd pledged (all with N0 country)

####################################################################################

#Barchart
# par(mar=c(7,4,4,2)) #setting margins so everyting fits
# table(fp_data6$state) %>% barplot(., las=2) #rotated text so it can be read

#fp_data <- fp_data2 #keeping it clean
#rm(fp_data2)

#####################################################################################################

#something is not working correctly with the column "use pledged" you cann't do calculations on it
#all of the other columns with numbers you can do calculations on

## Elena: added "na.rm = T", this resolves the issue and gives output
# glimpse(fp_data6)
# mean(fp_data$`usd pledged`, na.rm = T) 
# max(fp_data$`usd pledged`, na.rm = T)

#####################################################################################################
## Elena

# Countries are abbreviations, need to get full name
# Full name from https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 (copied and pasted table to excel)

# columns <- c("code", "country name", "year", "ccTLD", "ISO 3166-2", "notes")
# country_names <- read_csv("Country_codes.csv", skip = 1, col_names = columns)
# 
# country_names$`country name` <- gsub("[^\x20-\x7E]", "", country_names$`country name`)#removes all non english characters
# 
# country_names2 <- country_names %>% 
#   select(code, `country name`) %>%      # keep only first 2 columns
#   na.omit                               # Remove empty rows
# 
# # Need to join country_names2 with fp_data
# country_names2 %>% 
#   count(`code`) %>%
#   filter(n > 1)
# 
# fp_data6 %>%
#   count(`country`) %>%
#   filter(n > 1)
# 
# fp_data7 <- fp_data6 %>%
#   left_join(country_names2, by = c("country" = "code"))


# ###############################################################################################
# # Elena: Maps
# 
# # Country Barchart
# par(mar=c(7,4,4,2)) #setting margins so everyting fits
# table(fp_data6$country) %>% sort(decreasing = T) %>% 
#   barplot(., main = "Crowdfunding Project: Country of Origin", ylim = c(0,300000),las=2)
# 
# ## There is one country code "N,0" which only shows up when the state of the project is "undefined"
# total <- fp_data6 %>%
#   group_by(country) %>%
#   summarize(total = sum(`usd pledged`, na.rm = T))
# 
# 
# 
# map <- total %>% 
#   joinCountryData2Map(nameJoinColumn = "country", joinCode = "ISO2", verbose = T)
# 
# # Entire world map
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# #getting class intervals using a 'jenks' classification in classInt package
# classInt <- classInt::classIntervals( map[["total"]], n=6, style="jenks")
# catMethod = classInt[["brks"]]
# #getting a colour scheme from the RColorBrewer package
#  colorPalette <- RColorBrewer::brewer.pal(6,'Reds')
# #calling mapCountryData with the parameters from classInt and RColorBrewer
#  mapParams <- mapCountryData( map,
#                                 nameColumnToPlot="total",
#                                 mapTitle = "Crowdfunding Projects: Total USD Pledged",
#                                 addLegend=FALSE,
#                                 catMethod = catMethod,
#                                 colourPalette = colorPalette,
#                                 borderCol = "grey40")
#  do.call( addMapLegend
#             , c( mapParams,
#                    legendLabels="all",
#                    legendWidth=0.5,
#                    legendIntervals="data",
#                    legendMar = 2 ) )
# 
# # zoom in on world regions: North America
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# mapCountryData( map,
#                 mapTitle = "Crowdfunding Projects: Total USD Pledged in North America",
#                 nameColumnToPlot="total",
#                 addLegend=FALSE,
#                 catMethod = catMethod,
#                 colourPalette = colorPalette,
#                 borderCol = "grey40",
#                 mapRegion = "north america")
# 
# do.call( addMapLegend,
#          c( mapParams,
#               legendLabels="all",
#               legendWidth=0.5,
#               legendIntervals="data",
#               legendMar = 2 ) )
# 
# 
# # zoom in on world regions: Oceania
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# mapCountryData( map,
#                 mapTitle = "Crowdfunding Projects: Total USD Pledged in Oceania",
#                 nameColumnToPlot="total",
#                 addLegend=FALSE,
#                 catMethod = catMethod,
#                 colourPalette = colorPalette,
#                 borderCol = "grey40",
#                 mapRegion = "oceania")
# 
# do.call( addMapLegend,
#          c( mapParams,
#               legendLabels="all",
#               legendWidth=0.5,
#               legendIntervals="data",
#               legendMar = 2 ) )
# 
# 
# # zoom in on world regions: Asia
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# mapCountryData( map,
#                 mapTitle = "Crowdfunding Projects: Total USD Pledged in Asia",
#                 nameColumnToPlot="total",
#                 addLegend=FALSE,
#                 catMethod = catMethod,
#                 colourPalette = colorPalette,
#                 borderCol = "grey40",
#                 mapRegion = "asia")
# 
# do.call( addMapLegend,
#          c( mapParams,
#               legendLabels="all",
#               legendWidth=0.5,
#               legendIntervals="data",
#               legendMar = 2 ) )
# 
# 
# # zoom in on world regions: Eurasia
# par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# mapCountryData( map,
#                 mapTitle = "Crowdfunding Projects: Total USD Pledged in Eurasia",
#                 nameColumnToPlot="total",
#                 addLegend=FALSE,
#                 catMethod = catMethod,
#                 colourPalette = colorPalette,
#                 borderCol = "grey40",
#                 mapRegion = "eurasia")
# 
# do.call( addMapLegend,
#          c( mapParams,
#               legendLabels="all",
#               legendWidth=0.5,
#               legendIntervals="data",
#               legendMar = 2 ) )
# 
# ##########################################################################################
# # Elena
# # Country Barchart 2 (excludes top 2 countries)
# par(mar=c(7,4,4,2)) #setting margins so everyting fits
# country <- (table(fp_data6$country) %>% sort(decreasing = T))
#   barplot(country[-c(1:2)], main = "Crowdfunding Project: Country of Origin", ylim = c(0,15000),las=2)
# 
# # USD Pledged by Country and State of project
# (Country_state <- fp_data6 %>%
#     group_by(country, state) %>%
#     summarize(total = sum(`usd pledged`, na.rm = T)))
