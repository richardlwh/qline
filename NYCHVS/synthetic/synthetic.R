library(Synth)
library(plyr)
library(ggplot2)

# Construct synthetic control for Upper East Side

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

# For Testing Only
data_sub = subset(data_dummy, subboroughcode==3 & year==2002)
data_svy = svydesign(ids = ~0, data = data_sub, weights = data_sub$agperswt)
data_wtd = svymean(~income_tile, design = data_svy)
data_wtd

# Race Composition over time (Weighted)
wtdmean_UES = subset(data_wtdmean, subboroughcode==6)
wtdmean_Others = subset(data_wtdmean, subboroughcode!=6)

ggplot() + 
  geom_line(aes(x=year, y=race_Black, color=subboroughname), data = wtdmean_UES) + 
  geom_point(aes(x=year, y=race_Black, color=subboroughname), data = wtdmean_UES) +
  geom_text(aes(x=year, y=race_Black, label=round(race_Black, digits=3)), data = wtdmean_UES, vjust = 2) +
  geom_line(aes(x=year, y=race_Black, group=subboroughname), data = wtdmean_Others, colour = alpha("grey", 0.7)) +
  theme(legend.position="bottom") +
  xlab("Year") + ylab("Proportion of Black residents")

ggplot() + 
  geom_line(aes(x=year, y=race_White, color=subboroughname), data = wtdmean_UES) + 
  geom_point(aes(x=year, y=race_White, color=subboroughname), data = wtdmean_UES) +
  geom_text(aes(x=year, y=race_White, label=round(race_White, digits=3)), data = wtdmean_UES, vjust = 2) +
  geom_line(aes(x=year, y=race_White, group=subboroughname), data = wtdmean_Others, colour = alpha("grey", 0.7)) +
  theme(legend.position="bottom") +
  xlab("Year") + ylab("Proportion of White residents")


# Sythetic Control (Weighted)
prepped = dataprep(foo = data_wtdmean,
                   predictors = c("bdrms", "non_bdrms", "race_Black", "race_Hispanic",
                                  "race_Others", "race_White", "income_tile_1", "income_tile_2",
                                  "income_tile_3", "income_tile_4", "income_tile_5",
                                  "bdrms_1", "bdrms_2", "bdrms_3", "bdrms_4", "bdrms_5",
                                  "bdrms_6"),
                   predictors.op = "mean",
                   time.predictors.prior = c(2002, 2005),
                   dependent = "value",
                   unit.variable = "subboroughcode",
                   unit.names.variable = "subboroughname",
                   time.variable = "year",
                   treatment.identifier = 6,
                   controls.identifier = c(1:5, 7:10),
                   time.optimize.ssr = c(2002, 2005),
                   time.plot = c(2002, 2005, 2008, 2011, 2014, 2017))


synth.out = synth(data.prep.obj = prepped, method = "BFGS")
gaps = prepped$Y1plot - (prepped$Y0plot %*% synth.out$solution.w)
synth.tables = synth.tab(dataprep.res = prepped,
                         synth.res = synth.out)

path.plot(synth.res=synth.out,dataprep.res = prepped, 
          Ylab="average property value ($)",Xlab="year",
          Legend = c("Upper East Side",
                     "synthetic Upper East Side"),
          Legend.position = "bottomright")

gaps.plot(synth.res = synth.out, dataprep.res = prepped,
          Ylab = "gap in average property value ($)", Xlab= "year",
          Main = NA)

# Leave UWS out

prepped1 = dataprep(foo = data_test,
                    predictors = c("bdrms", "non_bdrms", "race_Black", "race_Hispanic",
                                   "race_Others", "race_White", "income_tile_1", "income_tile_2",
                                   "income_tile_3", "income_tile_4", "income_tile_5"),
                    predictors.op = "mean",
                    time.predictors.prior = c(2002, 2005),
                    dependent = "value",
                    unit.variable = "subboroughcode",
                    unit.names.variable = "subboroughname",
                    time.variable = "year",
                    treatment.identifier = 6,
                    controls.identifier = c(1:4, 7:10),
                    time.optimize.ssr = c(2002, 2005),
                    time.plot = c(2002, 2005, 2008, 2011, 2014, 2017))


synth.out = synth(data.prep.obj = prepped1, method = "BFGS")
gaps = prepped1$Y1plot - (prepped1$Y0plot %*% synth.out$solution.w)
synth.tables = synth.tab(dataprep.res = prepped1,
                         synth.res = synth.out)

path.plot(synth.res=synth.out,dataprep.res = prepped1, 
          Ylab="average property value ($)",Xlab="year",
          Legend = c("Upper East Side",
                     "synthetic Upper East Side leave-one-out"),
          Legend.position = "bottomright")

gaps.plot(synth.res = synth.out, dataprep.res = prepped1,
          Ylab = "gap in average property value ($)", Xlab= "year",
          Main = NA)

# Variance

data_test2 = aggregate(.~subboroughcode+subboroughname+year, data_dummy[c(1,4,7,8)], var)
data_test$valuevar = data_test2$value

prepped2 = dataprep(foo = data_test,
                   predictors = c("bdrms", "non_bdrms", "race_Black", "race_Hispanic",
                                  "race_Others", "race_White", "income_tile_1", "income_tile_2",
                                  "income_tile_3", "income_tile_4", "income_tile_5", "valuevar"),
                   predictors.op = "mean",
                   time.predictors.prior = c(2002, 2005),
                   dependent = "value",
                   unit.variable = "subboroughcode",
                   unit.names.variable = "subboroughname",
                   time.variable = "year",
                   treatment.identifier = 6,
                   controls.identifier = c(1:5, 7:10),
                   time.optimize.ssr = c(2002, 2005),
                   time.plot = c(2002, 2005, 2008, 2011, 2014, 2017))


synth.out = synth(data.prep.obj = prepped2, method = "BFGS")
gaps = prepped2$Y1plot - (prepped2$Y0plot %*% synth.out$solution.w)
synth.tables = synth.tab(dataprep.res = prepped2,
                         synth.res = synth.out)

path.plot(synth.res=synth.out,dataprep.res = prepped2, 
          Ylab="average property value ($)",Xlab="year",
          Legend = c("Upper East Side",
                     "synthetic Upper East Side"),
          Legend.position = "bottomright")

gaps.plot(synth.res = synth.out, dataprep.res = prepped2,
          Ylab = "gap in average property value ($)", Xlab= "year",
          Main = NA)

