library(MASS)
library(mosaic)
library(knitr)
library(broom)
library(ggplot2)
library(plyr)
library(survey)

# Create sample proportion tables for Owning/Renting by Race and Coop/Cond status by Race (weighted)
# 2011, 2014, 2017

data = read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Q-line/NYCHVS/tables/11_14_17_subset.csv")
data$race <- as.character(data$race)
data$race <- revalue(data$race, c("1"="White", "2"="Black", "3"="Hispanic", "4"="Others"))
data$coopcond <- as.character(data$coopcond)
data$coopcond <- revalue(data$coopcond, c("1"="Neither", "2"="Condo", "3"="Co-op", "4"="Unknown", "8"="Not Reported"))
data$owned <- as.character(data$owned)
data$owned <- revalue(data$owned, c("0"="Renting", "1"="Owning"))


year1 = subset(data, year==2011, select=c(owned, year, coopcond, race, agperswt))
year2 = subset(data, year==2014, select=c(owned, year, coopcond, race, agperswt))
year3 = subset(data, year==2017, select=c(owned, year, coopcond, race, agperswt))

datasvy1 = svydesign(ids = ~0, data = year1, weights = year1$agperswt)
table1svy_owned = svytable(formula = ~owned + race, design = datasvy1)
table1_owned = prop.table(addmargins(table1svy_owned, margin=2), margin=2)
table1_owned = ftable(table1_owned, row.vars="race", col.vars="owned")
table1svy_coopcond = svytable(formula = ~coopcond + race, design = datasvy1)
table1_coopcond = prop.table(addmargins(table1svy_coopcond, margin=2), margin=2)
table1_coopcond = ftable(table1_coopcond, row.vars="race", col.vars="coopcond")
table1svy_owning = svytable(formula = ~owned + coopcond, design = datasvy1)
table1_owning = prop.table(addmargins(table1svy_owning, margin=2), margin=2)

datasvy2 = svydesign(ids = ~0, data = year2, weights = year2$agperswt)
table2svy_owned = svytable(formula = ~owned + race, design = datasvy2)
table2_owned = prop.table(addmargins(table2svy_owned, margin=2), margin=2)
table2_owned = ftable(table2_owned, row.vars="race", col.vars="owned")
table2svy_coopcond = svytable(formula = ~coopcond + race, design = datasvy2)
table2_coopcond = prop.table(addmargins(table2svy_coopcond, margin=2), margin=2)
table2_coopcond = ftable(table2_coopcond, row.vars="race", col.vars="coopcond")

datasvy3 = svydesign(ids = ~0, data = year3, weights = year3$agperswt)
table3svy_owned = svytable(formula = ~owned + race, design = datasvy3)
table3_owned = prop.table(addmargins(table3svy_owned, margin=2), margin=2)
table3_owned = ftable(table3_owned, row.vars="race", col.vars="owned")
table3svy_coopcond = svytable(formula = ~coopcond + race, design = datasvy3)
table3_coopcond = prop.table(addmargins(table3svy_coopcond, margin=2), margin=2)
table3_coopcond = ftable(table3_coopcond, row.vars="race", col.vars="coopcond")


