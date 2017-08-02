#######################################################################
#Author: Ceara Sweetser
#Date: July 20th 2017
#The purpose of this program is to tease apart the meanings and true values
#of some of the columns fomr the eddmaps data set in order to compare 
#abundance metrics.
#######################################################################

#eddmaps<-read.csv("C:/Users/Localadmin/Desktop/Abundance correlations/eddmaps_prepped_07_18_17.csv", as.is=TRUE)

#creating a column with just the numeric values found in invasivecover
#eddmaps$invasivecoverNUMERIC<-as.numeric(eddmaps$invasivecover)

#creating a column with just character values found in invasivecover
#eddmaps$invasivecoverCHAR[is.na(eddmaps$invasivecoverNUMERIC)]<-eddmaps$invasivecover[is.na(eddmaps$invasivecoverNUMERIC)]

#Looking at the count and contributors to the numeric values over 100
#over.count<-length(which(eddmaps$invasivecoverNUMERIC>100)) #10
#sources.over<-eddmaps$ReporterFULLName[which(eddmaps$invasivecoverNUMERIC>100)]
#unique(sources.over)
  #There are 3 reporters: Appalachian National Scenic Trail, Alaska Exotic Plant Information
    #clearinghouse databse, and John Odell

#Looking at the count and contributors to the numeric values less than 1
#under.count<-length(which(eddmaps$invasivecoverNUMERIC<1)) #40975
#sources.under<-eddmaps$ReporterFULLName[which(eddmaps$invasivecoverNUMERIC<1)]
#unique(sources.under)  
  #There are 44 unique contributors to those values under 1. All three from
    #The sources.over are also found in sources.under
  #I wanted to see if anyone consistently contributed to the under.count values
  #u<-unique(sources.under)
  #u[which.max(tabulate(match(sources.under, u)))] #This yielded the Alaska exotic plants information clearinghouse database
  #length(which(eddmaps$invasivecover<1 & eddmaps$ReporterFULLName=="Appalachian National Scenic Trail")) #102

eddmaps<-read.csv("C:/Users/Localadmin/Desktop/Abundance correlations/eddmaps_updated.csv", as.is=TRUE)
# 
# #"Binning" the entries in invasivecoverCHAR
# unique(eddmaps$invasivecoverCHAR)
# for (i in 1:length(eddmaps$invasivecoverCHAR)){
#   if (!is.na(eddmaps$invasivecoverCHAR[i])){
#     #binning the values we are most sure about
#   if (eddmaps$invasivecoverCHAR[i]=="low" | eddmaps$invasivecoverCHAR[i]=="Low"| eddmaps$invasivecoverCHAR[i]=="5%"| eddmaps$invasivecoverCHAR[i]=="3%"| eddmaps$invasivecoverCHAR[i]=="Low       " | eddmaps$invasivecoverCHAR[i]=="Lo        " | eddmaps$invasivecoverCHAR[i]=="Low (1-5%)"|eddmaps$invasivecoverCHAR[i]=="Low (1.0 - 5.0%)"| eddmaps$invasivecoverCHAR[i]=="(L) Low = 1%-5%"| eddmaps$invasivecoverCHAR[i]=="1-5%"| eddmaps$invasivecoverCHAR[i]=="1%"| eddmaps$invasivecoverCHAR[i]=="3.00%"| eddmaps$invasivecoverCHAR[i]=="4%"| eddmaps$invasivecoverCHAR[i]=="1-5"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"low"}
#   if (eddmaps$invasivecoverCHAR[i]=="high"|eddmaps$invasivecoverCHAR[i]=="High"|eddmaps$invasivecoverCHAR[i]=="HIgh"|eddmaps$invasivecoverCHAR[i]=="High:26-50%"|eddmaps$invasivecoverCHAR[i]=="High      "|eddmaps$invasivecoverCHAR[i]=="COVER C 26 to 50%"|eddmaps$invasivecoverCHAR[i]=="High:26-50%"|eddmaps$invasivecoverCHAR[i]=="26-50%"|eddmaps$invasivecoverCHAR[i]=="25-50%"|eddmaps$invasivecoverCHAR[i]=="37.50%"|eddmaps$invasivecoverCHAR[i]=="30%"|eddmaps$invasivecoverCHAR[i]=="40%"|eddmaps$invasivecoverCHAR[i]=="26-50"|eddmaps$invasivecoverCHAR[i]=="COVER C 25 to 50%"|eddmaps$invasivecoverCHAR[i]=="37.5%"|eddmaps$invasivecoverCHAR[i]=="45%"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"high"}
#   if (eddmaps$invasivecoverCHAR[i]=="trace"|eddmaps$invasivecoverCHAR[i]=="Trace"|eddmaps$invasivecoverCHAR[i]=="<1%"|eddmaps$invasivecoverCHAR[i]=="0.50%"|eddmaps$invasivecoverCHAR[i]==".5%"|eddmaps$invasivecoverCHAR[i]==".1%"|eddmaps$invasivecoverCHAR[i]=="(T) Trace = less than 1%"|eddmaps$invasivecoverCHAR[i]=="0%"|eddmaps$invasivecoverCHAR[i]=="Trace     "|eddmaps$invasivecoverCHAR[i]=="Trace (Less than 1%)"|eddmaps$invasivecoverCHAR[i]=="0.5%"|eddmaps$invasivecoverCHAR[i]=="00%"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"trace"}
#   if (eddmaps$invasivecoverCHAR[i]=="moderate"|eddmaps$invasivecoverCHAR[i]=="20%"|eddmaps$invasivecoverCHAR[i]=="15.00%"|eddmaps$invasivecoverCHAR[i]=="medium"|eddmaps$invasivecoverCHAR[i]=="15%"|eddmaps$invasivecoverCHAR[i]=="5.1-25%"|eddmaps$invasivecoverCHAR[i]=="6-25"|eddmaps$invasivecoverCHAR[i]=="Moderate:6-25%"|eddmaps$invasivecoverCHAR[i]=="10%       "|eddmaps$invasivecoverCHAR[i]=="Moderate (5-25%)"|eddmaps$invasivecoverCHAR[i]=="Medium"|eddmaps$invasivecoverCHAR[i]=="(M) = 5.1%-25%"|eddmaps$invasivecoverCHAR[i]=="Moderate"|eddmaps$invasivecoverCHAR[i]=="5-25%"|eddmaps$invasivecoverCHAR[i]=="10%"|eddmaps$invasivecoverCHAR[i]=="25%"|eddmaps$invasivecoverCHAR[i]=="6-25%"|eddmaps$invasivecoverCHAR[i]=="6To25"|eddmaps$invasivecoverCHAR[i]=="Moderate  "|eddmaps$invasivecoverCHAR[i]=="Moderate (5.1 - 25%)"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"moderate"}
#   if(eddmaps$invasivecoverCHAR[i]=="51-75%"|eddmaps$invasivecoverCHAR[i]==">75%"|eddmaps$invasivecoverCHAR[i]=="95-100%"|eddmaps$invasivecoverCHAR[i]=="75-95%"|eddmaps$invasivecoverCHAR[i]=="50-75%"|eddmaps$invasivecoverCHAR[i]=="50-80%"|eddmaps$invasivecoverCHAR[i]=="80-97.5%"|eddmaps$invasivecoverCHAR[i]=="85.00%"|eddmaps$invasivecoverCHAR[i]=="62.50%"|eddmaps$invasivecoverCHAR[i]=="97.50%"|eddmaps$invasivecoverCHAR[i]=="85%"|eddmaps$invasivecoverCHAR[i]=="70%"|eddmaps$invasivecoverCHAR[i]=="60%"|eddmaps$invasivecoverCHAR[i]=="80%"|eddmaps$invasivecoverCHAR[i]=="90%"|eddmaps$invasivecoverCHAR[i]=="95%"|eddmaps$invasivecoverCHAR[i]=="97.5%"|eddmaps$invasivecoverCHAR[i]=="100%"|eddmaps$invasivecoverCHAR[i]=="62.5%"|eddmaps$invasivecoverCHAR[i]=="75%"|eddmaps$invasivecoverCHAR[i]=="76-100%"|eddmaps$invasivecoverCHAR[i]=="75-100%"|eddmaps$invasivecoverCHAR[i]=="51-100"|eddmaps$invasivecoverCHAR[i]=="Majority:51-100%"|eddmaps$invasivecoverCHAR[i]=="76-100%   "|eddmaps$invasivecoverCHAR[i]=="COVER C >50% Geo"|eddmaps$invasivecoverCHAR[i]=="COVER C >50%"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"very high"}
#   if (eddmaps$invasivecoverCHAR[i]=="NULL"|eddmaps$invasivecoverCHAR[i]==""|eddmaps$invasivecoverCHAR[i]=="SelectOne"|eddmaps$invasivecoverCHAR[i]=="lessthan"|eddmaps$invasivecoverCHAR[i]=="none"|eddmaps$invasivecoverCHAR[i]=="unknown"|eddmaps$invasivecoverCHAR[i]=="N/A"|eddmaps$invasivecoverCHAR[i]=="          "|eddmaps$invasivecoverCHAR[i]=="Select One"|eddmaps$invasivecoverCHAR[i]==" "){eddmaps$invasivecoverCHAR.CLEAN[i]<-"NULL"}
#     #binning the in-between ranges
#   if(eddmaps$invasivecoverCHAR[i]=="trace-low"|eddmaps$invasivecoverCHAR[i]=="<2%"|eddmaps$invasivecoverCHAR[i]=="COVER C <2%"|eddmaps$invasivecoverCHAR[i]=="<2%                                               "){eddmaps$invasivecoverCHAR.CLEAN[i]<-"trace"}
#   if(eddmaps$invasivecoverCHAR[i]=="<5%"|eddmaps$invasivecoverCHAR[i]=="<2.5%"|eddmaps$invasivecoverCHAR[i]=="<5%,singl"|eddmaps$invasivecoverCHAR[i]=="<5%,numer"|eddmaps$invasivecoverCHAR[i]=="<5%,fewi"|eddmaps$invasivecoverCHAR[i]=="<5%,few("|eddmaps$invasivecoverCHAR[i]=="low-med"|eddmaps$invasivecoverCHAR[i]=="low-medium"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"low"}
#   if(eddmaps$invasivecoverCHAR[i]=="2.5-20%"|eddmaps$invasivecoverCHAR[i]=="<25%"|eddmaps$invasivecoverCHAR[i]=="0-25%"|eddmaps$invasivecoverCHAR[i]=="COVER C 2 to 25%"|eddmaps$invasivecoverCHAR[i]=="2-25%                                             "|eddmaps$invasivecoverCHAR[i]=="med-high"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"moderate"}
#   if(eddmaps$invasivecoverCHAR[i]=="24-75%"|eddmaps$invasivecoverCHAR[i]=="20-50%"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"high"}
#   if(eddmaps$invasivecoverCHAR[i]=="25.1-100%"|eddmaps$invasivecoverCHAR[i]==">25%"|eddmaps$invasivecoverCHAR[i]=="High (25-100%)"|eddmaps$invasivecoverCHAR[i]=="High (25.1-100%)"|eddmaps$invasivecoverCHAR[i]=="(H) High = 25.1%-100%"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"very high"}  
#     #binning the ranked values
#   if(eddmaps$invasivecoverCHAR[i]=="2=5-25"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"moderate"}
#   if(eddmaps$invasivecoverCHAR[i]=="1=<5%,n"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"low"}
#   if(eddmaps$invasivecoverCHAR[i]=="3=25-5"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"high"}
#   if(eddmaps$invasivecoverCHAR[i]=="4=50-7"|eddmaps$invasivecoverCHAR[i]=="5=75-1"){eddmaps$invasivecoverCHAR.CLEAN[i]<-"very high"}  
#   }
# }
# #populating the invasivecoverCHAR.CLEAN column based on the numeric column
# for (i in 1:length(eddmaps$invasivecoverNUMERIC)){
#   if(!is.na(eddmaps$invasivecoverNUMERIC[i])){
#     if (eddmaps$invasivecoverNUMERIC[i]>=0 & eddmaps$invasivecoverNUMERIC[i]<1){eddmaps$invasivecoverCHAR.CLEAN[i]<-"trace"}
#     if (eddmaps$invasivecoverNUMERIC[i]>=1 & eddmaps$invasivecoverNUMERIC[i]<=5){eddmaps$invasivecoverCHAR.CLEAN[i]<-"low"}
#     if (eddmaps$invasivecoverNUMERIC[i]>5 & eddmaps$invasivecoverNUMERIC[i]<=25){eddmaps$invasivecoverCHAR.CLEAN[i]<-"moderate"}
#     if (eddmaps$invasivecoverNUMERIC[i]>25 & eddmaps$invasivecoverNUMERIC[i]<=50){eddmaps$invasivecoverCHAR.CLEAN[i]<-"high"}
#     if (eddmaps$invasivecoverNUMERIC[i]>50 & eddmaps$invasivecoverNUMERIC[i]<=100){eddmaps$invasivecoverCHAR.CLEAN[i]<-"very high"}
#   }
# }
# 
# unique(eddmaps$invasivecoverCHAR[is.na(eddmaps$invasivecoverCHAR.CLEAN)]) #none left over so we have now binned everything
#write.csv(eddmaps, "C:/Users/Localadmin/Desktop/Abundance correlations/eddmaps_updated.csv")  

####all code above was run which is why it is commented out so that there are no more changes made to the columns####

###Looking at Questionable values and contributors###
#Just using the number column so if there was a .5% in the char column then we aren't looking at that
###Looking at the problematic invasivecover entries and those who contributed them
unique(eddmaps$ReporterFULLName[eddmaps$invasivecoverCHAR=="1=<5%,n" | eddmaps$invasivecoverCHAR=="2=5-25" |eddmaps$invasivecoverCHAR=="3=25-5"|eddmaps$invasivecoverCHAR=="4=50-7"|eddmaps$invasivecoverCHAR=="5=75-1"]) 
  #Resulted in NA and David Dick
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="David Dick"]) 
  #David Dick did report ranges that are acceptable as well as names of bins and the wonky format as shown above
unique(eddmaps$ReporterFULLName[eddmaps$invasivecoverNUMERIC<=5 & eddmaps$invasivecoverNUMERIC>=1])
  #There are 51 reporters who contribute to numbers greater than 1 and less than 5. David Dick is NOT one of them so we don't have to be worried that those are in fact rank numbers from him
length(which(eddmaps$invasivecoverNUMERIC<=5 & eddmaps$invasivecoverNUMERIC>=1))
  #There are 115,460 records that are between 1 and 5

#Looking to see if Reporters that contribute values less than 1 also contribute values greater than 1
less<-unique(eddmaps$ReporterFULLName[eddmaps$invasivecoverNUMERIC<=5 & eddmaps$invasivecoverNUMERIC>=1])
over<-unique(eddmaps$ReporterFULLName[eddmaps$invasivecoverNUMERIC>1 & eddmaps$invasivecoverNUMERIC<=100])
for (i in less){
  answer<-i %in% over
  print(answer)
  if (answer==FALSE){print(i)}
}
#There are 10 contributors that only contribute values less than 1. Perhaps they contributed acceptable values to the CHAR column instead of the numeric column so I will check there for each one
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Ryan Wersal"])
  #Ryan did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Robin Bond"])
  #Robin did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Matt Gower"])
  #Matt did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Michael S Howell"])
  #Michael did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Michael C Cox"])
  #Michael did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Jimmy D. Peeples"])
  #Jimmy did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Daniel Z. Reynolds"])
  #Daniel did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Evan O'Donnell"])
  #Evan did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Lesley C. Coats"])
  #Lesley did not contribute any values to invasivecoverCHAR
unique(eddmaps$invasivecoverCHAR[eddmaps$ReporterFULLName=="Kristin Follmer"])
  #Kristin contributed only NULL values to the CHAR column

#Looking at the numerical values contributed by these 10 who only give numbers under 1
ten_under<-c("Ryan Wersal","Robin Bond","Matt Gower","Michael S Howell","Michael C Cox","Jimmy D. Peeples","Daniel Z. Reynolds","Evan O'Donnell","Lesley C. Coats","Kristin Follmer")
unique(eddmaps$invasivecoverNUMERIC[eddmaps$ReporterFULLName %in% ten_under])
  #output--> 1.00 0.45 0.80 0.10 0.90 0.40 0.50 0.25 0.75 0.30 0.65 0.02 0.20 0.15 0.55 0.60 0.70 0.95 0.85   NA
  #They mostly all look like proportions

###The number of values in invasivecoverCHAR.CLEAN that are not null = 442,025

#Looking at reporters that only contributed values 1 2 3 4 or 5. 
unique(eddmaps$ReporterFULLName[eddmaps$invasivecoverNUMERIC==1 |eddmaps$invasivecoverNUMERIC==2|eddmaps$invasivecoverNUMERIC==3|eddmaps$invasivecoverNUMERIC==4|eddmaps$invasivecoverNUMERIC==5])
  #There were 50 reporters that contributed to one of these numbers
unique(eddmaps$ReporterFULLName[eddmaps$invasivecoverNUMERIC==1 & eddmaps$invasivecoverNUMERIC==2&eddmaps$invasivecoverNUMERIC==3&eddmaps$invasivecoverNUMERIC==4&eddmaps$invasivecoverNUMERIC==5])
  #No reporters that contributed 1 2 3 4 and 5. 
#Randomly selecting reporters to see if any of them contributed numbers other than 1-5 integers
unique(eddmaps$invasivecoverNUMERIC[eddmaps$ReporterFULLName=="Ann Stevens"])
  #Ann had entries other than 1-5
unique(eddmaps$invasivecoverNUMERIC[eddmaps$ReporterFULLName=="Tim Higgs"])
  #Tim had entries other than 1-5
unique(eddmaps$invasivecoverNUMERIC[eddmaps$ReporterFULLName=="United States Forest Service  Intermountain Region"])
  #US forest service had others
unique(eddmaps$invasivecoverNUMERIC[eddmaps$ReporterFULLName=="John Odell"])
  #John odell looks like he contributed proportions and percentages
