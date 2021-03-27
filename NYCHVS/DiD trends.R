library(Synth)
library(plyr)
library(ggplot2)

# Plot for median property value over time, UES vs. UWS

data1 = read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Q-line/NYCHVS/synthetic/02-08_owners_weighted.csv")
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
data1 = data1[,c(1,2,7,9,4,6,5,8,3)]
data2 = read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Q-line/NYCHVS/synthetic/11_14_17_owners_weighted.csv")
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
data_dummy = fastDummies::dummy_cols(data_all, select_columns = "race")
data_dummy = fastDummies::dummy_cols(data_dummy, select_columns = "income_tile")
data_dummy = fastDummies::dummy_cols(data_dummy, select_columns = "bdrms")
data_dummy$subboroughcode = as.numeric(data_dummy$subboroughcode)


# Split Dataframe
sublists = split(data_dummy,list(data_dummy$subboroughcode, data_dummy$year))

# Function
match_and_wtdmean = function(vec){
  for (x in sublists){
    for (i in 1:ncol(x)){
      if (identical(x[,i], vec)){
        wtdmean = weighted.mean(vec, x$agperswt)
        return(wtdmean)
      }
    }
  }
}

data_wtdmean = aggregate(.~subboroughcode+subboroughname+year, data_dummy, function(n){match_and_wtdmean(n)})

data_UESUWS = subset(data_wtdmean, subboroughcode==5 | subboroughcode==6)

ggplot(data_UESUWS, aes(year, value, color = subboroughname)) + geom_line() + theme(legend.position="bottom")
