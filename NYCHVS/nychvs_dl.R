# Download NYCHVS data for 2002 - 2011
# Convert to CSV form, merge files

library("lodown")
nychvs_cat <- get_catalog( "nychvs", output_dir = file.path("/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020" , "NYCHVS" ) )
lodown( "nychvs" , output_dir = file.path("/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020", "NYCHVS" ) )
nychvs_cat <- subset( nychvs_cat , year == 2014 )
nychvs_cat <- lodown( "nychvs" , nychvs_cat )

setwd("/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020/NYCHVS/2011")

for (f in Sys.glob('*.rds'))
{
  write.csv(readRDS(f), file = gsub('rds$', 'csv', f))
}

occ_02 <- read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020/NYCHVS/2002/occ.csv")
occ_05 <- read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020/NYCHVS/2005/occ.csv")
occ_08 <- read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020/NYCHVS/2008/occ.csv")
occ_11 <- read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020/NYCHVS/2011/occ.csv")
vars <- c("value", "rentm", "bdrms", "rooms", "condition", "coopcond", "rentm", 
          "elevator", "agperswt", "borough", "subboro", "yhincome", "hhrace2")
occ_02 <- occ_02[vars]
occ_02$year <- 2002
occ_05 <- occ_05[vars]
occ_05$year <- 2005
occ_08 <- occ_08[vars]
occ_08$year <- 2008
occ_11 <- occ_11[vars]
occ_11$year <- 2011
compiled <- rbind(occ_02, occ_05, occ_08, occ_11)
write.csv(compiled, "/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020/NYCHVS/all_years_subset.csv")

renters <- read.csv("/Users/whlu/Documents/Terra Nova/UChicago/Summer 2020/NYCHVS/all_years_renters.csv")
wls <- lm(rentm ~ bdrms + rooms + as.factor(condition) + as.factor(coopcond) + as.factor(elevator), 
          data = renters, weights = agperswt)
ggplot(renters, aes(x = wls$fitted.values, y = rstudent(wls))) +
  geom_point() + xlab("Fitted Values") +
  ylab("Studentized Residual") + geom_hline(yintercept = 0,
                                            col = 2)
qqnorm(rstudent(wls))
qqline(rstudent(wls))