library(tidyverse)
library(ggplot2)
library(reshape2)
library(fastDummies)  ## create dummy variables
library(car)          ## variable selection
library(rworldmap)    ## map
library(classInt)     ## map: create intervals
library(RColorBrewer) ## map: color palette


fp_data <- read_csv("project.csv") ## read csv into var

#head(fp_data)

##########################################################################################

##7 rows had launch dates in 1970 - removing them.
fp_data <- fp_data %>% filter(., launched_date > 1980)

##removing unneeded columns
fp_data <- fp_data %>% select(-pledged, -launched_time, -`usd pledged`, -category)

#############################################################################################

##creating days between launch and deadline column
fp_data$days_between_goal <- as.numeric(fp_data$deadline - fp_data$launched_date)

##creating under/over goal column
fp_data$under_over_goal <- fp_data$usd_pledged_real - fp_data$usd_goal_real

## creating numeric variable for month project launched
fp_data$month <- as.numeric(substring(fp_data$launched_date, 6, 7)) 

##########################################################################################

##pulling out quantative vars for correlation test
correlations <- fp_data %>% 
  select(goal, usd_pledged_real, days_between_goal, backers, month) %>% 
  cor() %>%
  melt()


## Construct a heat map of correlations sorted in descending order
ggplot(data = correlations) +
  geom_tile(mapping = aes(x = reorder(Var1, desc(value)), y = reorder(Var2, value), fill = value)) +
  scale_fill_gradient2(low = "red", high = "steelblue", mid = "white", limit = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = "Correlation") +
  geom_text(mapping = aes(x = Var1, y = Var2, label = round(value, 3))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################################################################################################



#################################################################################################################

##barplot of main catagories
ggplot(data = fp_data) +
  geom_bar(mapping = aes(x = main_category)) +
  coord_flip() + ##fiped graph so it's a little easier to read
  labs(title = "Break Down by Main Category",
       x = NULL, y = "Count", ##these look wrong, but the graph is flipped on it's side
       caption = "Source: Kickstarter Platform") +
  theme(axis.ticks = element_blank())

##subsetting higher grossing categorys so the graph looks better
sub_set_high <- subset(fp_data, main_category == "Games" | main_category == "Design"
                       | main_category == "Technology" | main_category == "Film & Video")


##subsetting lower grossing categorys so the graph looks better
sub_set_low <- subset(fp_data, main_category == "Art" | main_category == "Comics"
                      | main_category == "Crafts" | main_category == "Dance"
                      | main_category == "Fashion" | main_category == "Food"
                      | main_category == "Journalism" | main_category == "Music"
                      | main_category == "Photography" | main_category == "Publishing"
                      | main_category == "Theater")

## all categorys
## Mark thins this graph looks bad so that's the reason for the subsetting
ggplot(fp_data, aes(x = main_category, y = (usd_pledged_real / 1000000))) + 
  geom_bar(stat="identity", width=.5, fill="dodgerblue4") + 
  labs(title="Total Dollars by Main Category",
       x = "Category", y = "USD in Millions",
       caption="Source: Kickstarter Platform") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Plot of 200 mill +
ggplot(sub_set_high, aes(x = main_category, y = (usd_pledged_real / 1000000))) + 
  geom_bar(stat = "identity", width = .5, fill = "dodgerblue4") + 
  labs(title="Total Dollars by Main Category",
       subtitle = "Category Total Over $200,000,000",
       x = "Category", y = "USD in Millions",
       caption="Source: Kickstarter Platform") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Plot of less than 200 mill
ggplot(sub_set_low, aes(x = main_category, y = (usd_pledged_real / 1000000))) + 
  geom_bar(stat = "identity", width = .5, fill = "dodgerblue4") + 
  labs(title = "Total Dollars by Main Category",
       subtitle = "Category Total Under $200,000,000",
       x = "Category", y = "USD in Millions",
       caption = "Source: Kickstarter Platform") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plot of project state sucessful vs failed ...
ggplot(data = fp_data) + 
  geom_bar(mapping = aes(x = state), fill = "dodgerblue4") +
  labs(title = "Project State", x = "State of Project", y = "Count",
       caption = "Source: Kickstarter Platform") +
  theme(axis.text.x = element_text(vjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#########################################

# subset data to only successful/failed projects
fp_data2 <- fp_data %>% filter(state == "successful"| state == "failed")

fp_data2$state_dummy <- ifelse(fp_data2$state == "successful", 1, 0)

# Dummy variables
# Main category and currency
fp_data3 <- fp_data2 %>% dummy_cols(select_columns = c("main_category","currency","country"))

# Film & Video, USD, US baselines
fp_data3 <- fp_data3 %>% select(-`main_category_Film & Video`, - currency_USD, -country_US)

################
tab <- table(fp_data2$main_category,fp_data2$state)
barplot(tab, beside = T, legend = T)

chisq.test(tab)
chisq.test(tab)$residuals


tab2 <- table(fp_data2$currency,fp_data2$state)
barplot(tab2, beside = T, legend = T)

chisq.test(tab2)
chisq.test(tab2)$residuals


tab3 <- table(fp_data2$country,fp_data2$state)
barplot(tab3, beside = T, legend = T)

chisq.test(tab3)
chisq.test(tab3)$residuals


################
## Will partition data into a training and test sets

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(fp_data3), size = floor(.75*nrow(fp_data3)), replace = F)
train <- fp_data3[sample, ]
test  <- fp_data3[-sample, ]

## TRAINING SET logistic regression with currencies 
log_state <- glm(state_dummy ~ as.factor(main_category_Music)	+ as.factor(`main_category_Technology`) +	
                   as.factor(main_category_Publishing)	+ as.factor(main_category_Food)	+ as.factor(main_category_Crafts)	+ 
                   as.factor(main_category_Games)	+ as.factor(main_category_Design)	+ as.factor(main_category_Comics)	+ 
                   as.factor(main_category_Fashion)	+ as.factor(main_category_Theater)	+ as.factor(main_category_Art)	+ 
                   as.factor(main_category_Photography)	+ as.factor(main_category_Dance)	+ as.factor(main_category_Journalism)	+ 
                   as.factor(currency_GBP)	+ as.factor(currency_CAD)	+ as.factor(currency_NOK)	+ as.factor(currency_DKK)	+ as.factor(currency_SEK)	+ 
                   as.factor(currency_AUD)	+ as.factor(currency_EUR)	+ as.factor(currency_MXN)	+ as.factor(currency_NZD) +	as.factor(currency_CHF) +	
                   as.factor(currency_HKD) +	as.factor(currency_SGD) +	as.factor(currency_JPY) + usd_goal_real + days_between_goal + month,
                 family=binomial("logit"), data = train)
# Getting warning of separation

vif(log_state) ## Check for multicollinearity 
summary(log_state)


##  DO NOT RUN THIS CODE - TAKES A LONG TIME TO RUN  ##
#log.reg.aic.backward <- step(log_state, direction = "backward", trace = FALSE)
#summary(log.reg.aic.backward)


## Final logistic regression model with currencies
log_state2 <- glm(state_dummy ~ as.factor(main_category_Music)	+ as.factor(`main_category_Technology`) +	
                    as.factor(main_category_Publishing)	+ as.factor(main_category_Food)	+ as.factor(main_category_Crafts)	+ 
                    as.factor(main_category_Games)	+ as.factor(main_category_Design)	+ as.factor(main_category_Comics)	+ 
                    as.factor(main_category_Fashion)	+ as.factor(main_category_Theater)	+ as.factor(main_category_Art)	+ 
                    as.factor(main_category_Photography)	+ as.factor(main_category_Dance)	+ as.factor(main_category_Journalism)	+ 
                    as.factor(currency_GBP)	+ as.factor(currency_CAD)	+ as.factor(currency_NOK)	+ as.factor(currency_SEK)	+ 
                    as.factor(currency_AUD)	+ as.factor(currency_EUR)	+ as.factor(currency_MXN)	+ as.factor(currency_NZD) +	as.factor(currency_CHF) +	
                    as.factor(currency_HKD) + usd_goal_real + days_between_goal + month,
                  family=binomial("logit"), data = train)

summary(log_state2)

# Testing how well model predicts if successful/failed
probs1 <- predict(log_state2,type="response")

pred1 <- rep("0",248740)
pred1[probs1>.5]=1 #if prob>0.5 predict SUCCESSFUL


table(pred1,train$state_dummy) #confusion matrix

#pred1==train$state_dummy
mean(pred1==train$state_dummy) #about 64.7% predictions correct


#roc curve
library(pROC)
g <- roc(state_dummy ~ probs1, data = train)
plot(g) # Not great

# Testing how well model predicts if successful/failed on TEST SET
pred_test <- predict(log_state2, test, type = "response")

pred_1 <- rep("0",82914)
pred_1[pred_test>.5]=1 #if prob>0.5 predict SUCCESSFUL

table(pred_1,test$state_dummy) #confusion matrix

mean(pred_1==test$state_dummy) #about 64.3% predictions correct

# ROC curve
g2 <- roc(state_dummy ~ pred_test, data = test)
plot(g2) # Not great
 

## logistic regression with currencies (using entire dataset)
# log_state <- glm(state_dummy ~ as.factor(main_category_Music)	+ as.factor(`main_category_Technology`) +	
#                    as.factor(main_category_Publishing)	+ as.factor(main_category_Food)	+ as.factor(main_category_Crafts)	+ 
#                    as.factor(main_category_Games)	+ as.factor(main_category_Design)	+ as.factor(main_category_Comics)	+ 
#                    as.factor(main_category_Fashion)	+ as.factor(main_category_Theater)	+ as.factor(main_category_Art)	+ 
#                    as.factor(main_category_Photography)	+ as.factor(main_category_Dance)	+ as.factor(main_category_Journalism)	+ 
#                    as.factor(currency_GBP)	+ as.factor(currency_CAD)	+ as.factor(currency_NOK)	+ as.factor(currency_DKK)	+ as.factor(currency_SEK)	+ 
#                    as.factor(currency_AUD)	+ as.factor(currency_EUR)	+ as.factor(currency_MXN)	+ as.factor(currency_NZD) +	as.factor(currency_CHF) +	
#                    as.factor(currency_HKD) +	as.factor(currency_SGD) +	as.factor(currency_JPY) + usd_goal_real + days_between_goal + month,
#                  family=binomial("logit"), data = fp_data3)
# Getting warning of separation

#vif(log_state) ## Check for multicollinearity 
#summary(log_state)

##  DO NOT RUN THIS CODE - TAKES A LONG TIME TO RUN  ##
#log.reg.aic.backward <- step(log_state, direction = "backward", trace = FALSE)
#summary(log.reg.aic.backward)


# ## Final logistic regression model with currencies
# log_state2 <- glm(state_dummy ~ as.factor(main_category_Music)	+ as.factor(`main_category_Technology`) +	
#                    as.factor(main_category_Publishing)	+ as.factor(main_category_Food)	+ as.factor(main_category_Crafts)	+ 
#                    as.factor(main_category_Games)	+ as.factor(main_category_Design)	+ as.factor(main_category_Comics)	+ 
#                    as.factor(main_category_Fashion)	+ as.factor(main_category_Theater)	+ as.factor(main_category_Art)	+ 
#                    as.factor(main_category_Photography)	+ as.factor(main_category_Dance)	+ as.factor(main_category_Journalism)	+ 
#                    as.factor(currency_GBP)	+ as.factor(currency_CAD)	+ as.factor(currency_NOK)	+ as.factor(currency_SEK)	+ 
#                    as.factor(currency_AUD)	+ as.factor(currency_EUR)	+ as.factor(currency_MXN)	+ as.factor(currency_NZD) +	as.factor(currency_CHF) +	
#                    as.factor(currency_HKD) + usd_goal_real + days_between_goal + month,
#                  family=binomial("logit"), data = fp_data3)
# 
# summary(log_state2)
# 
# # Testing how well model predicts if successful/failed
# probs1 <- predict(log_state2,type="response")
# 
# pred1 <- rep("0",331654)
# pred1[probs1>.5]=1 #if prob>0.5 predict SUCCESSFUL
# 
# 
# table(pred1,fp_data3$state_dummy) #confusion matrix
# 
# #pred1==fp_data3$state_dummy
# mean(pred1==fp_data3$state_dummy) #about 64.6% predictions correct


##########
## logistic regression with countries
log_country <- glm(state_dummy ~ as.factor(main_category_Music)	+ as.factor(`main_category_Technology`) +	
                   as.factor(main_category_Publishing)	+ as.factor(main_category_Food)	+ as.factor(main_category_Crafts)	+ 
                   as.factor(main_category_Games)	+ as.factor(main_category_Design)	+ as.factor(main_category_Comics)	+ 
                   as.factor(main_category_Fashion)	+ as.factor(main_category_Theater)	+ as.factor(main_category_Art)	+ 
                   as.factor(main_category_Photography)	+ as.factor(main_category_Dance)	+ as.factor(main_category_Journalism)	+ 
                   as.factor(country_AT)	+ as.factor(country_AU)	+ as.factor(country_BE)	+ as.factor(country_CA)	+ as.factor(country_CH)	+ 
                   as.factor(country_DE)	+ as.factor(country_DK)	+ as.factor(country_ES)	+ as.factor(country_FR) +	as.factor(country_GB) +	
                   as.factor(country_HK) +	as.factor(country_IE) +	as.factor(country_IT) +	as.factor(country_JP) +	as.factor(country_LU)  +
                   as.factor(country_MX) +	as.factor(country_NL) +	as.factor(country_NO) +	as.factor(country_NZ) +	as.factor(country_SE)+	
                   as.factor(country_SG) + usd_goal_real + days_between_goal + month,
                 family=binomial("logit"), data = fp_data3)

vif(log_country) ## Check for multicollinearity 
summary(log_country)


##  DO NOT RUN THIS CODE - TAKES A LONG TIME TO RUN  ##
#log.reg.aic.backward <- step(log_state, direction = "backward", trace = FALSE)
#summary(log.reg.aic.backward)

## Final logistic regression model with countries
log_country2 <- glm(state_dummy ~ as.factor(main_category_Music)	+ as.factor(`main_category_Technology`) +	
                    as.factor(main_category_Publishing)	+ as.factor(main_category_Food)	+ as.factor(main_category_Crafts)	+ 
                    as.factor(main_category_Games)	+ as.factor(main_category_Design)	+ as.factor(main_category_Comics)	+ 
                    as.factor(main_category_Fashion)	+ as.factor(main_category_Theater)	+ as.factor(main_category_Art)	+ 
                    as.factor(main_category_Photography)	+ as.factor(main_category_Dance)	+ as.factor(main_category_Journalism)	+ 
                    as.factor(country_AT)	+ as.factor(country_AU)	+ as.factor(country_BE)	+ as.factor(country_CA)	+ as.factor(country_CH)	+ 
                    as.factor(country_DE) + as.factor(country_ES) +	as.factor(country_GB) +	as.factor(country_HK) +	as.factor(country_IE) +	
                    as.factor(country_IT) + as.factor(country_MX) +	as.factor(country_NL) +	as.factor(country_NO) +	as.factor(country_NZ) +	
                    as.factor(country_SE) + as.factor(country_SG) + usd_goal_real + days_between_goal + month,
                   family=binomial("logit"), data = fp_data3)

summary(log_country2)

# Testing how well model predicts if successful/failed
probs2 <- predict(log_country2,type="response")

pred2 <- rep("0",331654)
pred2[probs2>.5]=1 #if prob>0.5 predict SUCCESSFUL


table(pred2,fp_data3$state_dummy) #confusion matrix

#pred2==fp_data3$state_dummy
mean(pred2==fp_data3$state_dummy) #about 64.6% predictions correct


################################################################################################

# LINEAR DISCRIMINANT ANALYSIS (LDA)

## Final logistic regression model with currencies
library(MASS)
lda_curr <- lda(state_dummy ~ as.factor(main_category_Music)	+ as.factor(`main_category_Technology`) +	
                    as.factor(main_category_Publishing)	+ as.factor(main_category_Food)	+ as.factor(main_category_Crafts)	+ 
                    as.factor(main_category_Games)	+ as.factor(main_category_Design)	+ as.factor(main_category_Comics)	+ 
                    as.factor(main_category_Fashion)	+ as.factor(main_category_Theater)	+ as.factor(main_category_Art)	+ 
                    as.factor(main_category_Photography)	+ as.factor(main_category_Dance)	+ as.factor(main_category_Journalism)	+ 
                    as.factor(currency_GBP)	+ as.factor(currency_CAD)	+ as.factor(currency_NOK)	+ as.factor(currency_SEK)	+ 
                    as.factor(currency_AUD)	+ as.factor(currency_EUR)	+ as.factor(currency_MXN)	+ as.factor(currency_NZD) +	as.factor(currency_CHF) +	
                    as.factor(currency_HKD) + usd_goal_real + days_between_goal + month, data = train)

lda_curr

lda_pred <- predict(lda_curr, test, type = "response")

table(lda_pred$class,test$state_dummy)

mean(lda_pred$class==test$state_dummy) #slightly less than logistic

l <- lda_pred[3]
# ROC curve
g3 <- roc(state_dummy ~ lda_pred$x, data = test)
plot(g3) # Not great


###############################################################################################
# Elena: Maps of Million USD Pledged

# Country Barchart
par(mar=c(7,4,4,2)) #setting margins so everyting fits
table(fp_data$country) %>% sort(decreasing = T) %>% 
  barplot(., main = "Crowdfunding Project: Country of Origin", ylim = c(0,300000),las=2)

## There is one country code "N,0" which only shows up when the state of the project is "undefined"
total <- fp_data %>%
  group_by(country) %>%
  summarize(total = sum(usd_pledged_real, na.rm = T)/1000000) # in Million $



map <- total %>% 
  joinCountryData2Map(nameJoinColumn = "country", joinCode = "ISO2", verbose = T)

# Entire world map
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
#getting class intervals using a 'jenks' classification in classInt package
classInt <- classInt::classIntervals( map[["total"]], n=8, style="jenks")
catMethod = classInt[["brks"]]
#getting a colour scheme from the RColorBrewer package
colorPalette <- RColorBrewer::brewer.pal(8,'Reds')
#calling mapCountryData with the parameters from classInt and RColorBrewer
mapParams <- mapCountryData( map,
                             nameColumnToPlot="total",
                             mapTitle = "Crowdfunding Projects: Total Million USD Pledged",
                             addLegend=FALSE,
                             catMethod = catMethod,
                             colourPalette = colorPalette,
                             borderCol = "grey40")
do.call( addMapLegend
         , c( mapParams,
              legendLabels="all",
              legendWidth=0.5,
              legendIntervals="page",
              legendMar = 2 ) )

# zoom in on world regions: North America
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData( map,
                mapTitle = "Crowdfunding Projects: Total Million USD Pledged in North America",
                nameColumnToPlot="total",
                addLegend=FALSE,
                catMethod = catMethod,
                colourPalette = colorPalette,
                borderCol = "grey40",
                mapRegion = "north america")

do.call( addMapLegend,
         c( mapParams,
            legendLabels="all",
            legendWidth=0.5,
            legendIntervals="page",
            legendMar = 2 ) )


# zoom in on world regions: Oceania
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData( map,
                mapTitle = "Crowdfunding Projects: Total Million USD Pledged in Oceania",
                nameColumnToPlot="total",
                addLegend=FALSE,
                catMethod = catMethod,
                colourPalette = colorPalette,
                borderCol = "grey40",
                mapRegion = "oceania")

do.call( addMapLegend,
         c( mapParams,
            legendLabels="all",
            legendWidth=0.5,
            legendIntervals="page",
            legendMar = 2 ) )


# zoom in on world regions: Asia
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData( map,
                mapTitle = "Crowdfunding Projects: Total Million USD Pledged in Asia",
                nameColumnToPlot="total",
                addLegend=FALSE,
                catMethod = catMethod,
                colourPalette = colorPalette,
                borderCol = "grey40",
                mapRegion = "asia")

do.call( addMapLegend,
         c( mapParams,
            legendLabels="all",
            legendWidth=0.5,
            legendIntervals="page",
            legendMar = 2 ) )


# zoom in on world regions: Eurasia
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData( map,
                mapTitle = "Crowdfunding Projects: Total Million USD Pledged in Eurasia",
                nameColumnToPlot="total",
                addLegend=FALSE,
                catMethod = catMethod,
                colourPalette = colorPalette,
                borderCol = "grey40",
                mapRegion = "eurasia")

do.call( addMapLegend,
         c( mapParams,
            legendLabels="all",
            legendWidth=0.5,
            legendIntervals="page",
            legendMar = 2 ) )

################################################################

# MAP OF NUMBER OF PROJECTS BY COUNTRY #
total_proj <- fp_data$country %>% table()
count_proj <- tibble(total_proj)
total_proj <- tibble(country = rownames(total_proj), count = count_proj$total_proj)


map_count <- total_proj %>% 
  joinCountryData2Map(nameJoinColumn = "country", joinCode = "ISO2", verbose = T)

# Entire world map
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
#getting class intervals using a 'jenks' classification in classInt package
classInt <- classInt::classIntervals( map_count[["count"]], n=8, style="jenks")
catMethod <- classInt[["brks"]]
#getting a colour scheme from the RColorBrewer package
colorPalette <- RColorBrewer::brewer.pal(8,'Reds')
#calling mapCountryData with the parameters from classInt and RColorBrewer
mapParams <- mapCountryData( map_count,
                             nameColumnToPlot="count",
                             mapTitle = "Crowdfunding: Total Projects per Country",
                             addLegend=FALSE,
                             catMethod = catMethod,
                             colourPalette = colorPalette,
                             borderCol = "grey40")
do.call( addMapLegend
         , c( mapParams,
              legendLabels="all",
              legendWidth=0.5,
              legendIntervals="page",
              legendMar = 2 ) )
