#### exploration of implied absence data (i.e. 0 abundance) in EDDMaps
###  Mitch O'Neill
###  created: 8/3/2017
##   last updated: 8/4/2017


edd <- read.table("file:///C:/Users/mwone/Google Drive/Fall 2016/EDDMapS_data_expl_2017/EDDMaps_plants_10_18_2016.csv", header = T, sep = ",", quote= "\"", 
                  comment.char= "", stringsAsFactors = F, strip.white = T)
## this is the full invasive plant dataset EDDMaps, obtained by Bethany Bradley on October 18, 2016

## convert ifested and gross area to numeric for exploration, NULLs coerced to NA
edd$infestedAreaInAcres <- as.numeric(edd$infestedAreaInAcres)
edd$grossAreaInAcres <- as.numeric(edd$grossAreaInAcres)

### Address the confounding of canopycover and percentcover
### from Rebekah Wallace:
## IF: Canopycover IS NOT NULL and percentcover IS NULL; THEN Canopycover is the area covered by the invasive species
## IF: Percentcover IS NOT NULL; THEN percentcover is the area covered by the invasive species and 
## canopycover (NULL or NOT NULL) would be the native vegetation's cover.

## this invasive cover code is designed to pull information on the cover of the invasive plant itself based on above rules
edd$invasivecover[edd$canopycover != "NULL" & edd$percentcover == "NULL"] <- edd$canopycover[edd$canopycover != "NULL" & edd$percentcover == "NULL"]

edd$invasivecover[edd$percentcover != "NULL"] <- edd$percentcover[edd$percentcover != "NULL"]

edd$invasivecover[edd$canopycover == "NULL" &  edd$percentcover == "NULL"] <- "NULL"

summary(edd$negative)

## scan for entries that may imply absence or NA
unique(edd$TXabundance)
unique(edd$grossAreaInAcres)
unique(edd$density)
unique(edd$AbundanceText)
unique(edd$invasivecover)
unique(edd$PercentPlotCovered)
unique(edd$stemcount)
unique(edd$NumberObserved)
unique(edd$Aggressiveness)


## flag potential/implied absences (0=occurence, 1 = potential absence)
edd$potential.absence <- 0 ## default is that a row is not absence data

## any records that have an entry in the abundance column that resembles absence/zero abundance
## assigned 1 (possible absence)
edd$potential.absence[edd$TXabundance == "None" | edd$TXabundance == "0" |
                        # where TXabundance implies absence
                        edd$grossAreaInAcres == 0 | edd$infestedAreaInAcres == 0 | 
                        # records were infested acres, gross acres, or both are zero
                        edd$density == "None" | edd$density == "0 PLANTS" | edd$density == "Trace, NONE FOUND" |
                        # where the entry for density implies absences
                        edd$AbundanceText == "None                          " |
                        # where entry for AbundanceText implies absences
                        edd$invasivecover == "00%" | edd$invasivecover == "none" | edd$invasivecover == "0" | edd$invasivecover == "0%        " | edd$invasivecover == "0         " |
                        # where canopycover/percentcover implies absence
                        edd$PercentPlotCovered == "0" | edd$PercentPlotCovered == "0,0" | edd$PercentPlotCovered == "00%" | edd$PercentPlotCovered == " 0%" |
                        # where percentplotcovered implies absence 
                        edd$stemcount == "0" | edd$stemcount == "0.000000" |
                        # where stemcount implies absence
                        edd$NumberObserved == "0"]  <- 1
                        ## where 0 plants were observed

summary(as.factor(edd$potential.absence[edd$negative==1])) ## some of the absences fall into potential=1 (other absences have all NAs)

edd$potential.absence[edd$negative == 1] <- 2 # these are definitely absences

length(edd$potential.absence[edd$potential.absence == 1]) ## how many potential absences
length(edd$potential.absence[edd$potential.absence == 1])/length(edd$potential.absence[edd$negative == 0]) ## proportion of occurences
## 41192, 2.5% of occurences




edd$contradiction <- 1 # contradicted by default; Next line identifies columns with no contradictions

edd$contradiction[(edd$potential.absence == 1) & 
                  ## where each abundance field must NOT contain nonzero abundance data (must be Zero or NA)
                  (edd$TXabundance == "None" | edd$TXabundance == "0" | edd$TXabundance == "NULL"| edd$TXabundance == "" | edd$TXabundance == "Select Abundance" ) &
                  # where TXabundance implies absence or is NULL
                  
                  (edd$grossAreaInAcres == 0 | edd$infestedAreaInAcres == 0 | is.na(edd$infestedAreaInAcres) | is.na(edd$grossAreaInAcres)) &
                  # records were infested acres, gross acres, or both are zero or NULL
                  
                  (edd$density == "None" | edd$density == "0 PLANTS" | edd$density == "Trace, NONE FOUND" |
                     edd$density == "NULL" | edd$density == "*" | edd$density == "Select Abundance" | edd$density == "" | edd$density == " " ) &
                  # where the entry for density implies absence or is NULL  
                  
                  (edd$AbundanceText == "None                          " | edd$AbundanceText == "NULL" ) &
                  # where entry for AbundanceText implies absences or is NULL
                  
                  (edd$invasivecover == "00%" | edd$invasivecover == "none" | edd$invasivecover == "0" | edd$invasivecover == "0%        " | edd$invasivecover == "0         " | edd$invasivecover == "Select One" |
                     edd$invasivecover == "NULL" | edd$invasivecover == " " | edd$invasivecover == "N/A" | edd$invasivecover == "unknown" | edd$invasivecover == "" | edd$invasivecover == "          " ) &
                  # where canopycover/percentcover implies absence
                  
                  (edd$PercentPlotCovered == "0" | edd$PercentPlotCovered == "0,0" | edd$PercentPlotCovered == "00%" | edd$PercentPlotCovered == " 0%" |
                     edd$PercentPlotCovered == "NULL" ) & # where percentplotcovered implies absence or is NULL
                  
                  (edd$stemcount == "0" | edd$stemcount == "0.000000" |
                     edd$stemcount == "NULL" | edd$stemcount == "unspecified" | edd$stemcount == "N/A" | edd$stemcount == " " | edd$stemcount == "" ) &
                  # where stemcount implies absence or is NULL
                  
                  (edd$NumberObserved == "0" |
                     edd$stemcount == "NULL" | edd$stemcount == "unknown" | edd$stemcount == "" | edd$stemcount == " " )] <- 0

length(edd$potential.absence[edd$potential.absence == 1 & edd$contradiction == 1])/
length(edd$potential.absence[edd$potential.absence == 1])
## 92.6% of implied absences are contradicted
length(edd$potential.absence[edd$potential.absence == 1 & edd$contradiction == 0])
## leaves 3032


## out of potential absences, check if the reporter uses the negative column

edd$ReporterFULLName <- paste(edd$ReporterFName, edd$ReporterLName, sep = " ")
## make a reporter full name field by pasting FName and LName

length(edd$ReporterFULLName[edd$ReporterFULLName == "NULL NULL"])
## NOTE: the 18 records with out Reporter names end up getting the unique name "NULL NULL"
## As none of these records use the "negative field", they end up passing through this quality control test
## (i.e. while records where the reporter has been shown to use the negative column in other records will be 
## excluded, these reporter-less records will not be excluded)
## NOTE: Reporter Names were used because about half of the observations do not use organization




#####################################################################################################
class(edd$dateentered)
head(as.Date(edd$dateentered))
head((edd$dateentered))
edd$dateenteredD <- as.Date(edd$dateentered)
summary(edd$dateenteredD)
min(edd$dateenteredD)

## loop through reporters to assess negative column use
rep.list <- as.list(unique(edd$ReporterFULLName[edd$potential.absence == 1]))


for (i in 1:length(rep.list)){
  
  
  
  ## if the reporter uses the absence colum
  if(length(edd$ReporterFULLName[edd$negative == 1 & edd$ReporterFULLName == rep.list[i]]) > 0){
    
    
    ## then a 1 is entered in the negative use column for all of that reporter's observations, AND
    edd$negUse[edd$ReporterFULLName == rep.list[i]] <- 1 
    
    
    ## for all potential absences from the reporter of the iteration, 
    edd$negUse2[edd$potential.absence == 1 & edd$ReporterFULLName == rep.list[i] & 
                  ## The ones entered earlier than than the first use of the negative column for that reporter are flagged as 0         
                  (edd$dateenteredD < min(edd$dateenteredD[edd$negative == 1 & edd$ReporterFULLName == rep.list[i]]))] <- 0 
    
    
    ## for all potential absences from the reporter of the iteration, 
    edd$negUse2[edd$potential.absence == 1 & edd$ReporterFULLName == rep.list[i] & 
                  ## The ones not entered earlier than than the first use of the negative column for that reporter are flagged as 1         
                  (edd$dateenteredD >= min(edd$dateenteredD[edd$negative == 1 & edd$ReporterFULLName == rep.list[i]]))] <- 1 
    
  } else { 
    
    ## if negative column is not used, then a 0 is entered in the negative use column
    edd$negUse[edd$ReporterFULLName == rep.list[i]] <- 0
    
  }
  
  
  
  
  print(i)
  
}




#### Review of absence data cleaning column definitions
####
####
#### potential.absences : (0) records with no indication of absence
####                      (1) records with an indication of absence in an abundance column
####                      (2) records with a (1) in the negative field
####
#### contradictions     : of the potential absences == 1 OR 2;
####                      (0) records with no contradictions
####                      (1) records with contradictions
####
#### negUse             : (0) the reporter of this record does not use the absence column
####                      (1) the reporter of this record uses the absence column
####
#### negUse2            : (0) this potential absence record was entered before the reporter used the column
####                      (1) this potential absence record was not entered before the reporter used the column
####

#### absences are:    potential.absences = 2   AND   contradictions = 0
####                  potential.absences = 1   AND   contradictions = 0   AND   negUse = 0
####                  potential.absences = 1   AND   contradictions = 0   AND   negUse = 1   AND   negUse2 = 0

#### using entry date does not save any data


length(edd$potential.absence[edd$potential.absence == 1 & edd$contradiction == 0 & edd$negUse == 0])
## number of non-contradicted implied absences reported by users who did not use the negative column
## 3022

length(edd$potential.absence[edd$potential.absence == 1 & edd$contradiction == 0 & edd$negUse == 0])/length(edd$potential.absence[edd$potential.absence == 1 & edd$contradiction == 0])
## proportion: .9967
       
length(edd$potential.absence[edd$potential.absence == 1 & edd$contradiction == 0 & edd$negUse == 1 & edd$negUse2 == 0]) 
## number of non-contradicted implied absences reported by reporters who had not yet used the negative column
## 0