#### exploration of infestation extent data in EDDMapS
###  Mitch O'Neill
###  created: 8/3/2017
###  last updated: 8/4/2017

edd <- read.table("file:///C:/Users/mwone/Google Drive/Fall 2016/EDDMapS_data_expl_2017/EDDMaps_plants_10_18_2016.csv", header = T, sep = ",", quote= "\"", 
                  comment.char= "", stringsAsFactors = F, strip.white = T)
## this is the full invasive plant dataset EDDMaps, obtained by Bethany Bradley on October 18, 2016

edd$ReporterFULLName <- paste(edd$ReporterFName, edd$ReporterLName, sep = " ") ## paste first and last name into full name
## for later work with reporter names

unique(edd$infestedAreaInAcres)
## convert ifested and gross area to numeric for exploration, NULLs coerced to NA
edd$infestedAreaInAcres <- as.numeric(edd$infestedAreaInAcres)
edd$grossAreaInAcres <- as.numeric(edd$grossAreaInAcres)

a <- length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)]) ## no. of records with both infested and gross
b <- length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & is.na(edd$grossAreaInAcres)])  ## no. of records with infested but not gross
c <- length(edd$infestedAreaInAcres[is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)])  ## no. of records with gross but not infested
d <- length(edd$infestedAreaInAcres[is.na(edd$infestedAreaInAcres) & is.na(edd$grossAreaInAcres)])   ## no. of records with neither infested nor gross
e <- length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) | !is.na(edd$grossAreaInAcres)]) ## no. of records with infested area, gross area, or both

  
a+b+c+d == length(edd$infestedAreaInAcres)
d+e == length(edd$infestedAreaInAcres)
e == a+b+c
## all TRUE if the code worked properly

## proportion of data with any extent data
e/length(edd$infestedAreaInAcres)

## of the records with any extent data
a/e ## proportion with both infested and gross
b/e ## proporton with infested only
c/e ## proportion with gross only

a+b ## no. records with infested area
a+c ## no. records gross area

quantile(edd$infestedAreaInAcres, na.rm = T)
## are there many negatives and zeroes
length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & edd$infestedAreaInAcres < 0])
## 22 negatives --> exclude?
length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & edd$infestedAreaInAcres == 0])
## 6191 zeroes --> exclude?
length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & edd$infestedAreaInAcres <= 0])/(a+b)
## 0.9% of infested area data is less than or equal to zero
length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & edd$infestedAreaInAcres > 0])
## 715,603 OK records
quantile(edd$infestedAreaInAcres[edd$infestedAreaInAcres >0], na.rm = T)


quantile(edd$grossAreaInAcres, na.rm = T)
## are there many negatives and zeroes?
length(edd$grossAreaInAcres[!is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres < 0])
## 26 negatives --> exclude?
length(edd$grossAreaInAcres[!is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres == 0])
## 29594 zeroes --> exclude?
length(edd$grossAreaInAcres[!is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres <= 0])/(a+c)
## 8.6% of infested area data is less than or equal to zero
length(edd$grossAreaInAcres[!is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres > 0])
## 314,237 OK records
quantile(edd$grossAreaInAcres[edd$grossAreaInAcres >0], na.rm = T)


## compare gross and infested area for records where both values are reported.
## according to metadata definitions, infested area should not be greater than gross area
## Quality assurance: exclude records where infested or gross area are equal to or less than zero.
edd1 <- edd[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$infestedAreaInAcres > 0 & edd$grossAreaInAcres> 0,]

quantile(edd1$infestedAreaInAcres/edd1$grossAreaInAcres)
## sometimes infested is greater than gross

f <- length(edd1$infestedAreaInAcres[edd1$infestedAreaInAcres < edd1$grossAreaInAcres])  #where infested area is less than gross (OK)
g <- length(edd1$infestedAreaInAcres[edd1$infestedAreaInAcres == edd1$grossAreaInAcres]) #where infested area equals gross (OK)
h <- length(edd1$infestedAreaInAcres[edd1$infestedAreaInAcres > edd1$grossAreaInAcres])  #where infested area is greater than gross (VIOLATES METADATA))
i <- length(edd1$infestedAreaInAcres) ## total

f+g+h == i
## TRUE if code worked

f/i ## proportion where infested area is less than gross (OK)
g/i ## proportion where infested area equals gross (OK)
h/i ## proportion where infested area is greater than gross (VIOLATES METADATA))

edd1 <- edd1[edd1$infestedAreaInAcres <= edd1$grossAreaInAcres, ]
## only taking records where infested area is less than or equal to gross area (to follow metadata definitions)

quantile(edd1$infestedAreaInAcres)
quantile(edd1$grossAreaInAcres)
## very large values are present

par(mfrow=c(1,2))
hist(log(edd1$infestedAreaInAcres), xlab= "log(Infested Area in Acres)")
hist(log(edd1$grossAreaInAcres), xlab= "log(Gross Area in Acres)")
## note the positive skew

## who is reporting these large values?
summary(as.factor(edd1$ReporterFULLName[edd1$infestedAreaInAcres > 100]))



length(edd1$ReporterFULLName[edd1$ReporterFULLName == "US Army Corps of Engineers Ombil Database" & edd1$infestedAreaInAcres > 100])/
  length(edd1$ReporterFULLName[edd1$infestedAreaInAcres > 100])
## US Army corps is reporting most of these values over 100

## how are US Army corps values distributed
quantile(edd1$infestedAreaInAcres[edd1$ReporterFULLName == "US Army Corps of Engineers Ombil Database"])
quantile(edd1$grossAreaInAcres[edd1$ReporterFULLName == "US Army Corps of Engineers Ombil Database"])
## most values are unexpectedly high, with no entries below 1 acre

par(mfrow=c(1,2))
hist(log(edd1$infestedAreaInAcres[edd1$ReporterFULLName == "US Army Corps of Engineers Ombil Database"]), xlab= "log(Infested Area in Acres)")
hist(log(edd1$grossAreaInAcres[edd1$ReporterFULLName == "US Army Corps of Engineers Ombil Database"]), xlab= "log(Gross Area in Acres)")

length(edd1$infestedAreaInAcres[edd1$grossAreaInAcres > 20])/length(edd1$infestedAreaInAcres)
## about 1/10 records still unusually high

## quality assurance: exclude US Army Corps observations
edd1 <- edd1[edd1$ReporterFULLName != "US Army Corps of Engineers Ombil Database", ]

## who else is reporting big values?
summary(as.factor(edd1$ReporterFULLName[edd1$infestedAreaInAcres > 100]))


##United States Forest Service  Intermountain Region 88
##Mark Twain National Forest 61
(88 + 61)/ length(edd1$ReporterFULLName[edd1$infestedAreaInAcres > 100])

quantile(edd1$infestedAreaInAcres[edd1$ReporterFULLName == "United States Forest Service  Intermountain Region"])
quantile(edd1$grossAreaInAcres[edd1$ReporterFULLName == "United States Forest Service  Intermountain Region"])
## THis reporter reports large as well as reasonable values

quantile(edd1$infestedAreaInAcres[edd1$ReporterFULLName == "Mark Twain National Forest"])
quantile(edd1$grossAreaInAcres[edd1$ReporterFULLName == "Mark Twain National Forest"])
## THis reporter reports large as well as reasonable values

par(mfrow=c(1,2))
hist(log(edd1$infestedAreaInAcres), xlab= "log(Infested Area in Acres)")
hist(log(edd1$grossAreaInAcres), xlab= "log(Gross Area in Acres)")
## right skew problem is reduced the removal of the US Army Corps

quantile(edd1$infestedAreaInAcres)
quantile(edd1$grossAreaInAcres)
## large numbers still present


length(edd$infestedAreaInAcres[edd$infestedAreaInAcres > 100 & edd$infestedAreaInAcres <= edd$grossAreaInAcres & edd$infestedAreaInAcres > 0 & edd$grossAreaInAcres > 0 & !is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)& edd$ReporterFULLName != "US Army Corps of Engineers Ombil Database"])
## 236 over 100
length(edd$infestedAreaInAcres[edd$infestedAreaInAcres <= 1 & edd$infestedAreaInAcres <= edd$grossAreaInAcres & edd$infestedAreaInAcres > 0 & edd$grossAreaInAcres > 0 & !is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)])/
length(edd$infestedAreaInAcres[edd$infestedAreaInAcres <= edd$grossAreaInAcres & edd$infestedAreaInAcres > 0 & edd$grossAreaInAcres > 0 & !is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)])
## 89.2% 1 or less



## re-visit records where only infested area is given; i.e. where we can't compare it to gross area

## these observations have positive infested area that is less than or equal to positive gross area
edd$confirmed[edd$infestedAreaInAcres < edd$grossAreaInAcres & edd$infestedAreaInAcres > 0 & edd$grossAreaInAcres > 0 & !is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)] <- "confirmed" 

## these observaions have positive infested area data, but no gross area to compare it to
edd$confirmed[!is.na(edd$infestedAreaInAcres) & edd$infestedAreaInAcres > 0 & (is.na(edd$grossAreaInAcres) | edd$grossAreaInAcres <= 0)] <- "not confirmed"

## exclude army corps observations
edd$confirmed[edd$ReporterFULLName == "US Army Corps of Engineers Ombil Database"] <- "exclude"
summary(as.factor(edd$confirmed))



## compare the distribution of unconfirmed infested area to confirmed gross and infested area
par(mfrow=c(3,1))
hist(log(edd$infestedAreaInAcres[edd$confirmed == "confirmed"]), main= "Confirmed Infested Area", xlim = range(-20:20), xlab = "log(infested area in acres)", breaks = 20)
hist(log(edd$infestedAreaInAcres[edd$confirmed == "not confirmed"]), main= "Unconfirmed Infested Area", xlim = range(-20:20), xlab = "log(infested area in acres)", breaks = 20)
hist(log(edd$grossAreaInAcres[edd$confirmed == "confirmed"]), main= "Confirmed Gross Area", xlim = range(-20:20), xlab = "log(gross area in acres)", breaks = 20)
length(edd$infestedAreaInAcres[edd$confirmed == "not confirmed" & !is.na(edd$infestedAreaInAcres)])

quantile(edd$infestedAreaInAcres[edd$confirmed == "confirmed"], na.rm = T)
quantile(edd$infestedAreaInAcres[edd$confirmed == "not confirmed"], na.rm = T)
quantile(edd$grossAreaInAcres[edd$confirmed == "confirmed"], na.rm = T)



## briefly exploring infested acres units
unique(edd$Infestedareaunits[edd$infestedAreaInAcres > 100])
head(edd[edd$ReporterFULLName== "US Army Corps of Engineers Ombil Database",])

eddb <- edd[edd$infestedAreaInAcres > 100 & !is.na(edd$infestedAreaInAcres) &edd$ReporterFULLName== "US Army Corps of Engineers Ombil Database", ]
head(cbind(eddb$infestedAreaInAcres, eddb$Infestedarea, eddb$Infestedareaunits, eddb$Grossareaunits), 250)
unique(eddb$Infestedareaunits)
## All of these reporters seem to be reporting in acres




##################################################################################
##### starting back from beginning, exploring in different way

##################################################################
## where only gross area is provided, what is the range of values? 
##################################################################
quantile(edd$grossAreaInAcres[is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)], na.rm =T)
## are there many negatives and zeroes?

length(edd$grossAreaInAcres[is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres < 0])
## 16 negatives, exclude them?

length(edd$grossAreaInAcres[is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres == 0])
## 197 zeroes, exclude them?

quantile(edd$grossAreaInAcres[is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres > 0], na.rm =T)


##################################################################
## where only infested area is provided, what is the range of values?
##################################################################
quantile(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & is.na(edd$grossAreaInAcres)], na.rm =T)
## are there many negatives and zeroes?

length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & is.na(edd$grossAreaInAcres) & edd$infestedAreaInAcres < 0])
## 8 negatives, exclude them?

length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & is.na(edd$grossAreaInAcres) & edd$infestedAreaInAcres == 0])
## 5,815 zeroes, exclude them?
quantile(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & is.na(edd$grossAreaInAcres) & edd$infestedAreaInAcres > 0], na.rm =T)


##################################################################
## where both are provided, what is the range of values for infested area?
##################################################################
quantile(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)], na.rm =T)
## are there many negatives and zeroes?

length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$infestedAreaInAcres < 0])
## 14 negatives, exclude them?

length(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$infestedAreaInAcres == 0])
## 376, exclude them?

quantile(edd$infestedAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$infestedAreaInAcres > 0], na.rm =T)

##################################################################
## where both are provided, what is the range of values for gross area?
##################################################################
quantile(edd$grossAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres)], na.rm =T)
## are there many negatives and zeroes?

length(edd$grossAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres < 0])
## 10 negatives, exclude them?

length(edd$grossAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres == 0])
## 29397, exclude them?

quantile(edd$grossAreaInAcres[!is.na(edd$infestedAreaInAcres) & !is.na(edd$grossAreaInAcres) & edd$grossAreaInAcres > 0], na.rm =T)

