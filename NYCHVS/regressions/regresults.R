# Regression to determine the effect of race*UES on weighted mean property value

data1 = read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Q-line/NYCHVS/regressions/02-08_owners.csv")
data1$subboroughname = as.character(data1$subboroughname)
data1$subboroughcode <- revalue(data1$subboroughname, c("Greenwich Village/Financial District"="1", 
                                                        "Lower East Side/Chinatown"="2", 
                                                        "Chelsea/Clinton/Midtown"="3", 
                                                        "Stuyvesant Town/Turtle Bay"="4",
                                                        "Upper West Side"="5",
                                                        "Upper East Side"="6",
                                                        "Morningside Heights/Hamilton Heights"="7",
                                                        "Central Harlem"="8",
                                                        "East Harlem"="9",
                                                        "Washington Heights/Inwood"="10"))

data2 = read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Q-line/NYCHVS/regressions/11_14_17_owners.csv")
data2$subboroughcode = as.character(data2$subboroughcode)
data2$subboroughname = revalue(data2$subboroughcode, c("1"="Greenwich Village/Financial District", 
                                                       "2"="Lower East Side/Chinatown", 
                                                       "3"="Chelsea/Clinton/Midtown", 
                                                       "4"="Stuyvesant Town/Turtle Bay",
                                                       "5"="Upper West Side",
                                                       "6"="Upper East Side",
                                                       "7"="Morningside Heights/Hamilton Heights",
                                                       "8"="Central Harlem",
                                                       "9"="East Harlem",
                                                       "10"="Washington Heights/Inwood"))
data2$race = as.character(data2$race)
data2$race = revalue(data2$race, c("1"="White",
                                   "2"="Black",
                                   "3"="Hispanic",
                                   "4"="Others"))

data_all = rbind(data1, data2)
data_all = subset(data_all, value != 9999999)
data_all = transform(data_all, UES = ifelse(subboroughcode==6, "Yes", "No"))
data_all$coopcond = revalue(as.character(data_all$coopcond), c("1" = "Neither",
                                                 "2" = "Condo",
                                                 "3" = "Co-op",
                                                 "4" = "Unknown/Not Reported",
                                                 "8" = "Unknown/Not Reported"))
data_all$elevator = revalue(as.character(data_all$elevator), c("1" = "Yes",
                                                               "2" = "No"))

fcols = c("bdrms","condition","coopcond","elevator","year","income_tile")
data_all[fcols] = lapply(data_all[fcols], factor)

# Not weighted
model1 = lm(log(value) ~ bdrms + condition + coopcond + elevator + income_tile + year + race*UES, data=data_all)
summary(model1)

# Weighted
model2 = lm(log(value) ~ bdrms + condition + coopcond + elevator + income_tile + year + race*UES, 
            data=data_all, weight=agperswt)
summary(model2)


