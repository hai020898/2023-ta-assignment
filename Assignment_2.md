Assignment_2
================
2023-01-24

examiner_ids = read.csv(“examiner_ids.csv”) attach(examiner_ids)
\#install.packages(“gender”) \#install.packages(“wru”)
\#install.packages(“lubridate”) library(lubridate) library(wru)
library(gender) require(dplyr) library(ggplot2) library(corrplot)
library(methods) library(tidyverse) require(lmtest) library(caTools)
library(knitr)

\#Separate first name and last name examiner_ids =
separate(examiner_ids, examiner_name, c(“last_name”, “first_name”), sep
= “,”) examiner_ids = separate(examiner_ids, first_name, c(“first_name”,
“middle_name”), sep = ” “) examiner_ids \<- examiner_ids %\>%
select(-first_name) examiner_ids \<- examiner_ids %\>% rename(first_name
= middle_name) attach(examiner_ids)

\#Populate gender using gender package

gender_df \<- gender::gender(examiner_ids\$first_name) %\>%
select(first_name = name, gender)

merge=merge(examiner_ids, gender_df, by = “first_name”) merge_unique =
unique(merge, by=“new_pid”) merge_unique \<- merge_unique %\>%
rename(surname = last_name)

\#Populate race using wru package. The race with the highest probability
is assigned to the ID merge_race = predict_race(voter.file =
merge_unique, surname.only = TRUE) find_max_col \<- function(row) {
max_val \<- max(row\[c(‘pred.whi’, ‘pred.bla’,
‘pred.his’,‘pred.asi’,‘pred.oth’)\]) return(names(row)\[which(row ==
max_val)\]) }

# Apply the custom function to each row of the data frame

merge_race\$max_col \<- apply(merge_race, 1, find_max_col)

# drop all the prediction

merge_race_dropped \<- subset(merge_race, select = -c(pred.whi,
pred.bla, pred.his,pred.asi,pred.oth))

# change Max_col into race

merge_race_dropped \<- merge_race_dropped %\>% rename(race = max_col)

# Change the value in race column into actual race

merge_race_dropped \<- merge_race_dropped %\>% mutate(race = case_when(
race == “pred.whi” \~ “white”, race == “pred.asi” \~ “asia”, race ==
“pred.his” \~ “hispanic”, race == “pred.bla” \~ “black”, race ==
“pred.oth” \~ “other”, TRUE \~ race )) examiner_gs =
read.csv(“examiner_gs.csv”) attach(examiner_gs) \# Change all NA values
in end_date to the final days of 2016
examiner_gs$end_date <- ifelse(is.na(examiner_gs$end_date),
“06/30/2017”,
examiner_gs$end_date) # Change data type into date data type examiner_gs$end_date
\<-
as.Date(examiner_gs$end_date, format = "%m/%d/%Y") examiner_gs$start_date
\<- as.Date(examiner_gs\$start_date, format = “%m/%d/%Y”)

attach(examiner_gs)

# Calculate days between start_date and end_date

examiner_gs$days_between <- difftime(examiner_gs$end_date,
examiner_gs$start_date, unit="days") class(examiner_gs$days_between)

# Drop all observations with days_between = 0

examiner_gs \<- examiner_gs %\>% filter(days_between != 0) examiner_gs
\<- examiner_gs %\>% filter(days_between \> 0)

# Sum the days_between by new_pid

examiner_sum \<- examiner_gs %\>% group_by(new_pid) %\>%
summarise(sum_days = sum(days_between,na.rm = TRUE))

# Merge the 2 datasets to create the final dataset

examiner_merge=merge(merge_race_dropped, examiner_sum, by = “new_pid”)

# Create a new column called Current to indicate turnover or not. We are assuming that those who do not have patex_id have left

examiner_merge \<- examiner_merge %\>% mutate(current =
ifelse(is.na(patex_id),0,1))

# Convert sum_days into numeric

examiner_merge$sum_days = as.character(examiner_merge$sum_days)
examiner_merge$sum_days = as.numeric(examiner_merge$sum_days)
class(examiner_merge\$sum_days)

attach(examiner_merge)

# Exploratory analysis visualization

ggplot(examiner_merge, aes(x = gender, fill = factor(current))) +
geom_bar(position = “dodge”) + labs(x = “Gender”, y = “Count”, fill =
“Current”)

ggplot(examiner_merge, aes(x = race, fill = factor(current))) +
geom_bar(position = “dodge”) + labs(x = “Race”, y = “Count”, fill =
“Current”)

ggplot(examiner_merge, aes(x = sum_days, y = current, color = gender)) +
geom_point() + labs(x = “Tenure”, y = “Current”, color = “Gender”)

ggplot(examiner_merge, aes(x = factor(current), y = sum_days)) +
geom_boxplot() + labs(x = “Current”, y = “Tenure”)

# Split training set and test set

set.seed(123) split \<- sample.split(examiner_merge\$current, SplitRatio
= 0.7) train_set \<- subset(examiner_merge, split == TRUE) test_set \<-
subset(examiner_merge, split == FALSE)

# Run the logistic regression on training set

model \<- glm((current) \~ sum_days + as.factor(race) +
as.factor(gender), data = train_set, family = binomial()) summary(model)

# Prediction on test set

predictions \<- predict(model, newdata = test_set, type = “response”)
predictions \<- ifelse(predictions \> 0.5, 1, 0) \# threshold for
classification predictions \<- as.factor(predictions)

# Calculate the accuracy score

require(caret)  
cm\<-confusionMatrix(data=predictions,
reference=as.factor(test_set\$current))

Accuracy\<-round(cm\$overall\[1\],2)

library(arrow) \# importing “app_data_sample.parquet”

app_data_sample \<-
read_parquet(“C:/Users/nguye/OneDrive/Documents/MMA/Winter 2023/Talent
Analytics/Assignment2/USPTO_data/app_data_sample.parquet”,as_data_frame=TRUE)
attach(app_data_sample)

# Creating the quater column; Dates for Q1: January 1 – March 31. Dates for Q2: April 1 – June 3. Dates for Q3: July 1 – September 30. Dates for Q4: October 1 – December 31.

library(lubridate) library(dplyr) app_data_sample=app_data_sample %\>%
mutate(year= year(filing_date),quarter = quarter(filing_date))

merged_data \<- merge(examiner_gs, app_data_sample, by.x = “patex_id”,
by.y = “examiner_id”, all.x=TRUE)

merged_data$current <- ifelse(is.na(merged_data$examiner_id),“No”,“Yes”)

if (app_data_sample$examiner_id %in% examiner_ids$patex_id ){
app_data_sample$current == "YES"} else {  app_data_sample$current ==
“NO” }

app_data_sample$examiner_id_in_examiner_gs <- app_data_sample$examiner_id
%in% merge_unique\$patex_id

app_data_sample%\>% group_by(examiner_id_in_examiner_gs) %\>%
summarize(n())

# this create a new dataset called newdata2, with only some columns from app_sample_data and the columns newly created

newdata2 \<- app_data_sample %\>% mutate(year= year(filing_date),quarter
= quarter(filing_date)) %\>% mutate(Quarter = paste0(year, “-Q”,
quarter)) %\>% group_by(examiner_id, Quarter) %\>%
mutate(number_of_new_application = n(), number_of_abandonned_application
= sum(!is.na(abandon_date)), application_allowed = sum(disposal_type ==
“ISS”), application_in_process = sum(disposal_type == “PEND”)) %\>%
\#Add any column needed to the following code to add it to the
newdataset select(Quarter, quarter,
year,number_of_new_application,number_of_abandonned_application,examiner_id,
examiner_name_last,examiner_name_first,
application_allowed,application_in_process, examiner_art_unit)%\>%
distinct(Quarter, examiner_id,.keep_all=TRUE)

# Calculate a table with number of people in each art unit in each quarter

newdata3 = newdata2 %\>% group_by(Quarter, examiner_art_unit) %\>%
summarise(number_of_people_in_art_unit = n())

# Join the 2 tables

newdata4 = left_join(newdata2, newdata3, by = c(“Quarter” = “Quarter”,
“examiner_art_unit” = “examiner_art_unit”))

# Calculate the number of each gender for each unit in each quarter

merged_table \<- merge(newdata4, examiner_merge, by.x = “examiner_id”,
by.y = “patex_id”) gender_unit = merged_table %\>% filter(gender ==
“female”) %\>% group_by(examiner_art_unit, Quarter) %\>% summarize(count
= n())

# Add the number_of_female_in_art_unit column

newdata5 = left_join(newdata4, gender_unit, by = c(“Quarter” =
“Quarter”, “examiner_art_unit” = “examiner_art_unit”))
colnames(newdata5)\[colnames(newdata5) == “count”\] \<-
“number_of_female_in_art_unit”

# Calculate number of each race for each art unit in each quarter

race_unit = merged_table %\>% group_by(examiner_art_unit, Quarter,race)
%\>% summarize(count = n())

reshaped_data \<- race_unit %\>% spread(race, count)
reshaped_data\[is.na(reshaped_data)\] \<- 0

# join them to the table

newdata7 = left_join(newdata5, reshaped_data, by = c(“Quarter” =
“Quarter”, “examiner_art_unit” = “examiner_art_unit”))

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

## Including Plots

You can also embed plots, for example:

![](Assignment_2_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
