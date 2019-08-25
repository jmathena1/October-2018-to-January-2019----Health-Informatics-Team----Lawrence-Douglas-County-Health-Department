### SUICIDE ANALYSIS SCRIPT #####

setwd("C:/Users/john.m/Documents/Projects/Suicide report")

library(NLP)
library(tm)
library(stringr)
library(sqldf)
library(rJava)
library(mallet)
library(XML)
library(wordcloud)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(gofastr)
library(tidyr)
library(data.table)
library(readtext)
library(xlsx)
library(rlang)
library(mtconnectR)
library(rlist)

# ICD10 QUERY ####
ICD10 <- read.csv(file = "attempts_ICD10.csv", stringsAsFactors = F)
##read in csv with all demographics regarding DG county residents admitted to hospitals####
##related to suicide####
ICD10$CaseNotes <- with(ICD10$CaseNotes, paste0(
  ICD10$TriageNotesOrig, ICD10$Chief_Complaint_Combo))
##combine triage notes and complaint notes to make topic modeling corpus larger####
colnames(ICD10)[colnames(ICD10)=="Age.Group"] <- "Age_Group"
##rename Age group column####
colnames(suicide_codes)[1] <-c("ICD_10_Code")
##rename column 1####
ICD10_deceased <- ICD10[ICD10$Disposition.Category=="DECEASED",]
ICD10<- ICD10[ICD10$Disposition.Category!="DECEASED",]
##filter out patients who died of suicide####

which(colnames(ICD10)=="DDConverted")
which(colnames(ICD10)=="ChiefComplaintUpdates")
which(colnames(ICD10)=="Diagnosis_Combo")
which(colnames(ICD10)=="Admit_Reason_Combo")
which(colnames(ICD10)=="Patient_City")

## patient city found under ICD10$Patient_City (column 152) ####
## ICD-10 codes found under ICD10$DDConverted (column 108) ####
## Complaint column found under ICD10$ChiefComplaintUpdates (column 54) ####
## Diagnosis column found under ICD10$Diagnosis_Combo (column 43) ####
## Admission reason found under ICD10$Admit_Reason_Combo (column 35) ####
## Age (column 19) ####
## Sex (column 18) ####
## identifying column is ICD10$EssenceID (column 16) ####
## Facility Name (column 4)####
## Time (column 3)####
## Date (column 2)####

vars <- c("EssenceID", "C_Unique_Patient_ID", "Date", "Time", 
          "Facility.Name", "Age", "Age_Group",
          "Sex", "Admit_Reason_Combo", "Diagnosis_Combo",
          "ChiefComplaintUpdates",
          "Discharge.Diagnosis", "Patient_City", "CaseNotes")
ICD10_working <- ICD10[vars]
#create subset with useful information####

ICD10_working$Date <- as.Date(ICD10_working$Date, format = "%m/%d/%Y")
#convert the date to format that makes R happy####

ICD10_working <- ICD10_working[order(ICD10_working$Date),]
#order the ICD10_working file by date####

ICD10_working$Patient_City[ICD10_working$Patient_City=="LAWRENCE"] <- "Lawrence"
ICD10_working$Patient_City[ICD10_working$Patient_City=="BALDWIN CITY"] <- "Baldwin City"
ICD10_working$Patient_City[ICD10_working$Patient_City=="BALDWIN"] <- "Baldwin City"
ICD10_working$Patient_City[ICD10_working$Patient_City=="EUDORA"] <- "Eudora"
ICD10_working$Patient_City[ICD10_working$Patient_City=="LECOMPTON"] <- "Lecompton"
ICD10_working$Patient_City[ICD10_working$Patient_City=="OVERBROOK"] <- "Overbrook"
#Some cells have the city entered in all caps so this series of commands fixes that####

ICD10_working$Age_Group[ICD10_working$Age<15] <- "<15"
ICD10_working$Age_Group[ICD10_working$Age >= 15 & ICD10_working$Age <= 17] <- "15-17"
ICD10_working$Age_Group[ICD10_working$Age >= 18 & ICD10_working$Age <= 19] <- "18-19"
ICD10_working$Age_Group[ICD10_working$Age >= 20 & ICD10_working$Age <= 24] <- "20-24"
ICD10_working$Age_Group[ICD10_working$Age >= 25 & ICD10_working$Age <= 29] <- "25-29"
ICD10_working$Age_Group[ICD10_working$Age >= 30 & ICD10_working$Age <= 34] <- "30-34"
ICD10_working$Age_Group[ICD10_working$Age >= 35 & ICD10_working$Age <= 39] <- "35-39"
ICD10_working$Age_Group[ICD10_working$Age >= 40 & ICD10_working$Age <= 44] <- "40-44"
ICD10_working$Age_Group[ICD10_working$Age > 44] <- "45+"
#create age groups based on column with ages of patients####

ICD10_agegroup_sex <- as.data.frame.matrix(table(ICD10_working$Age_Group, ICD10_working$Sex))
ICD10_residence <- as.data.frame(table(ICD10_working$Patient_City))
colnames(ICD10_residence) <- c("City", "# of Patients")
#Make frequency tables for age/sex and city of origin####

ICD10_working2 <- ICD10_working
#Make a copy of the data frame####

patient_id_ICD10 <- data.frame(table(ICD10$C_Unique_Patient_ID))
colnames(patient_id_ICD10) <- c("Patient ID", "# of cases")
#Make frequency table counting visits per patient####

repeats_ICD10 <- patient_id_ICD10[patient_id_ICD10$`# of cases`>1,]
repeats_ICD10 <- repeats_ICD10[order(repeats_ICD10$`# of cases`, decreasing = T),]
colnames(repeats_ICD10) <- c("Patient ID", "# of cases")
#Make table of patients who have multiple cases organized from most cases to least cases####

vars2 <- match(repeats_ICD10$`Patient ID`, ICD10$C_Unique_Patient_ID)
#create variable containing rows of patients that have multiple cases####

repeats_ICD10_dem <- ICD10[vars2,]
#create table with only repeat patients####
repeats_ICD10_dem <- repeats_ICD10_dem[vars]
#create table with only useful information about the repeat patients####

repeats_ICD10_dem <- sqldf('
                   select * from repeats_ICD10_dem
                   left join repeats_ICD10
                   on repeats_ICD10_dem.C_Unique_Patient_ID = repeats_ICD10.`Patient ID`
                     ')
repeats_ICD10_dem <- repeats_ICD10_dem[,-c(15)]
#create table with repeat patient demographics and their frequencies####
repeats_ICD10_dem_age_sex <- as.data.frame.matrix(table(repeats_ICD10_dem$Age_Group, 
                                                        repeats_ICD10_dem$Sex))
#create table of repeats by age and sex####

# CHIEF COMPLAINT QUERY ####

ChiefComplaint <- read.csv(file = "attempts_ChiefComplaint.csv", stringsAsFactors = F)
##read in csv with all demographics regarding DG county residents admitted to hospitals####
##related to suicide####
ChiefComplaint$CaseNotes <- with(ChiefComplaint$CaseNotes, paste0(
  ChiefComplaint$TriageNotesOrig, ChiefComplaint$Chief_Complaint_Combo))
##combine triage notes and complaint notes to make topic modeling corpus larger####
colnames(ChiefComplaint)[colnames(ChiefComplaint)=="Age.Group"] <- "Age_Group"
##rename Age group column####
ChiefComplaint_deceased <- ChiefComplaint[ChiefComplaint$Disposition.Category=="DECEASED",]
ChiefComplaint<- ChiefComplaint[ChiefComplaint$Disposition.Category!="DECEASED",]
##filter out patients who died of suicide####

which(colnames(ChiefComplaint)=="DDConverted")
which(colnames(ChiefComplaint)=="ChiefComplaintUpdates")
which(colnames(ChiefComplaint)=="Diagnosis_Combo")
which(colnames(ChiefComplaint)=="Admit_Reason_Combo")
which(colnames(ChiefComplaint)=="Patient_City")

## patient city found under ChiefComplaint$Patient_City (column 152) ####
## ICD-10 codes found under ChiefComplaint$DDConverted (column 108) ####
## Complaint column found under ChiefComplaint$ChiefComplaintUpdates (column 54) ####
## Diagnosis column found under ChiefComplaint$Diagnosis_Combo (column 43) ####
## Admission reason found under ChiefComplaint$Admit_Reason_Combo (column 35) ####
## Age (column 19) ####
## Sex (column 18) ####
## identifying column is ChiefComplaint$EssenceID (column 16) ####
## Facility Name (column 4) ####
## Time (column 3) ####
## Date (column 2) ####

vars <- c("EssenceID", "C_Unique_Patient_ID", "Date", "Time", 
          "Facility.Name", "Age", "Age_Group",
          "Sex", "Admit_Reason_Combo", "Diagnosis_Combo",
          "ChiefComplaintUpdates",
          "Discharge.Diagnosis", "Patient_City", "CaseNotes")
ChiefComplaint_working <- ChiefComplaint[vars]
#create subset with useful information####

ChiefComplaint_working$Date <- as.Date(ChiefComplaint_working$Date, format = "%m/%d/%Y")
#convert the date to format that makes R happy####

ChiefComplaint_working <- ChiefComplaint_working[order(ChiefComplaint_working$Date),]
#order the ChiefComplaint_working file by date####

ChiefComplaint_working$Patient_City[ChiefComplaint_working$Patient_City=="LAWRENCE"] <- "Lawrence"
ChiefComplaint_working$Patient_City[ChiefComplaint_working$Patient_City=="BALDWIN CITY"] <- "Baldwin City"
ChiefComplaint_working$Patient_City[ChiefComplaint_working$Patient_City=="EUDORA"] <- "Eudora"
ChiefComplaint_working$Patient_City[ChiefComplaint_working$Patient_City=="LECOMPTON"] <- "Lecompton"
ChiefComplaint_working$Patient_City[ChiefComplaint_working$Patient_City=="OVERBROOK"] <- "Overbrook"
#Some cells have the city entered in all caps so this series of commands fixes that####

ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age<15] <- "<15"
ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age >= 15 & ChiefComplaint_working$Age <= 17] <- "15-17"
ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age >= 18 & ChiefComplaint_working$Age <= 19] <- "18-19"
ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age >= 20 & ChiefComplaint_working$Age <= 24] <- "20-24"
ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age >= 25 & ChiefComplaint_working$Age <= 29] <- "25-29"
ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age >= 30 & ChiefComplaint_working$Age <= 34] <- "30-34"
ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age >= 35 & ChiefComplaint_working$Age <= 39] <- "35-39"
ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age >= 40 & ChiefComplaint_working$Age <= 44] <- "40-44"
ChiefComplaint_working$Age_Group[ChiefComplaint_working$Age > 44] <- "45+"
#create age groups based on column with ages of patients####

ChiefComplaint_agegroup_sex <- as.data.frame.matrix(table(ChiefComplaint_working$Age_Group, ChiefComplaint_working$Sex))
ChiefComplaint_residence <- as.data.frame(table(ChiefComplaint_working$Patient_City))
colnames(ChiefComplaint_residence) <- c("City", "# of Patients")
#Make frequency tables for age/sex and city of origin####

ChiefComplaint_working2 <- ChiefComplaint_working
#Make a copy of the data frame####

patient_id_ChiefComplaint <- data.frame(table(ChiefComplaint$C_Unique_Patient_ID))
colnames(patient_id_ChiefComplaint) <- c("Patient ID", "# of cases")
#Make frequency table counting visits per patient####

repeats_ChiefComplaint <- patient_id_ChiefComplaint[patient_id_ChiefComplaint$`# of cases`>1,]
repeats_ChiefComplaint <- repeats_ChiefComplaint[order(repeats_ChiefComplaint$`# of cases`, 
                                                       decreasing = T),]
colnames(repeats_ChiefComplaint) <- c("Patient ID", "# of cases")
#Make table of patients who have multiple cases organized from most cases to least cases####

vars2 <- match(repeats_ChiefComplaint$`Patient ID`, ChiefComplaint$C_Unique_Patient_ID)
#create variable containing rows of patients that have multiple cases####

repeats_ChiefComplaint_dem <- ChiefComplaint[vars2,]
#create table with only repeat patients####
repeats_ChiefComplaint_dem <- repeats_ChiefComplaint_dem[vars]
#create table with only useful information about the repeat patients####

repeats_ChiefComplaint_dem <- sqldf('
                     select * from repeats_ChiefComplaint_dem
                     left join repeats_ChiefComplaint
                     on repeats_ChiefComplaint_dem.C_Unique_Patient_ID = 
                     repeats_ChiefComplaint.`Patient ID`
                     ')
repeats_ChiefComplaint_dem <- repeats_ChiefComplaint_dem[,-c(15)]
#create table with repeat patient demographics and their frequencies####
repeats_ChiefComplaint_dem_age_sex <- as.data.frame.matrix(table
                                                           (repeats_ChiefComplaint_dem$Age_Group,
                                                            repeats_ChiefComplaint_dem$Sex))
#create table of repeats by age and sex####

# SUB SYNDROME QUERY ####
SubSyndrome <- read.csv(file = "attempts_SubSyndrome.csv", stringsAsFactors = F)
##read in csv with all demographics regarding DG county residents admitted to hospitals####
##related to suicide####
SubSyndrome$CaseNotes <- with(SubSyndrome$CaseNotes, paste0(
  SubSyndrome$TriageNotesOrig, SubSyndrome$Chief_Complaint_Combo))
##combine triage notes and complaint notes to make topic modeling corpus larger####
colnames(SubSyndrome)[colnames(SubSyndrome)=="Age.Group"] <- "Age_Group"
##rename Age group column####
colnames(suicide_codes)[1] <-c("ICD_10_Code")
##rename column 1####
SubSyndrome_deceased <- SubSyndrome[SubSyndrome$Disposition.Category=="DECEASED",]
SubSyndrome<- SubSyndrome[SubSyndrome$Disposition.Category!="DECEASED",]
##filter out patients who died of suicide####

which(colnames(SubSyndrome)=="DDConverted")
which(colnames(SubSyndrome)=="SubSyndromeUpdates")
which(colnames(SubSyndrome)=="Diagnosis_Combo")
which(colnames(SubSyndrome)=="Admit_Reason_Combo")
which(colnames(SubSyndrome)=="Patient_City")

## patient city found under SubSyndrome$Patient_City (column 152) ####
## ICD-10 codes found under SubSyndrome$DDConverted (column 108) ####
## Complaint column found under SubSyndrome$SubSyndromeUpdates (column 54) ####
## Diagnosis column found under SubSyndrome$Diagnosis_Combo (column 43) ####
## Admission reason found under SubSyndrome$Admit_Reason_Combo (column 35) ####
## Age (column 19) ####
## Sex (column 18) ####
## identifying column is SubSyndrome$EssenceID (column 16) ####
## Facility Name (column 4) ####
## Time (column 3) ####
## Date (column 2) ####

vars <- c("EssenceID", "C_Unique_Patient_ID", "Date", "Time", 
          "Facility.Name", "Age", "Age_Group",
          "Sex", "Admit_Reason_Combo", "Diagnosis_Combo",
          "ChiefComplaintUpdates",
          "Discharge.Diagnosis", "Patient_City", "CaseNotes")

SubSyndrome_working <- SubSyndrome[vars]
#create subset with useful information####

SubSyndrome_working$Date <- as.Date(SubSyndrome_working$Date, format = "%m/%d/%Y")
#convert the date to format that makes R happy####

SubSyndrome_working <- SubSyndrome_working[order(SubSyndrome_working$Date),]
#order the SubSyndrome_working file by date####

SubSyndrome_working$Patient_City[SubSyndrome_working$Patient_City=="LAWRENCE"] <- "Lawrence"
SubSyndrome_working$Patient_City[SubSyndrome_working$Patient_City=="BALDWIN CITY"] <- "Baldwin City"
SubSyndrome_working$Patient_City[SubSyndrome_working$Patient_City=="EUDORA"] <- "Eudora"
SubSyndrome_working$Patient_City[SubSyndrome_working$Patient_City=="LECOMPTON"] <- "Lecompton"
SubSyndrome_working$Patient_City[SubSyndrome_working$Patient_City=="OVERBROOK"] <- "Overbrook"
#Some cells have the city entered in all caps so this series of commands fixes that, could be shorter###

SubSyndrome_working$Age_Group[SubSyndrome_working$Age<15] <- "<15"
SubSyndrome_working$Age_Group[SubSyndrome_working$Age >= 15 & SubSyndrome_working$Age <= 17] <- "15-17"
SubSyndrome_working$Age_Group[SubSyndrome_working$Age >= 18 & SubSyndrome_working$Age <= 19] <- "18-19"
SubSyndrome_working$Age_Group[SubSyndrome_working$Age >= 20 & SubSyndrome_working$Age <= 24] <- "20-24"
SubSyndrome_working$Age_Group[SubSyndrome_working$Age >= 25 & SubSyndrome_working$Age <= 29] <- "25-29"
SubSyndrome_working$Age_Group[SubSyndrome_working$Age >= 30 & SubSyndrome_working$Age <= 34] <- "30-34"
SubSyndrome_working$Age_Group[SubSyndrome_working$Age >= 35 & SubSyndrome_working$Age <= 39] <- "35-39"
SubSyndrome_working$Age_Group[SubSyndrome_working$Age >= 40 & SubSyndrome_working$Age <= 44] <- "40-44"
SubSyndrome_working$Age_Group[SubSyndrome_working$Age > 44] <- "45+"
#create age groups based on column with ages of patients####

SubSyndrome_agegroup_sex <- as.data.frame.matrix(table(SubSyndrome_working$Age_Group, SubSyndrome_working$Sex))
SubSyndrome_residence <- as.data.frame(table(SubSyndrome_working$Patient_City))
colnames(SubSyndrome_residence) <- c("City", "# of Patients")
#Make frequency tables for age/sex and city of origin####

SubSyndrome_working2 <- SubSyndrome_working
#Make a copy of the data frame####

patient_id_SubSyndrome <- data.frame(table(SubSyndrome$C_Unique_Patient_ID))
colnames(patient_id_SubSyndrome) <- c("Patient ID", "# of cases")
#Make frequency table counting visits per patient####

repeats_SubSyndrome <- patient_id_SubSyndrome[patient_id_SubSyndrome$`# of cases`>1,]
repeats_SubSyndrome <- repeats_SubSyndrome[order(repeats_SubSyndrome$`# of cases`, 
                                                 decreasing = T),]
colnames(repeats_SubSyndrome) <- c("Patient ID", "# of cases")
#Make table of patients who have multiple cases organized from most cases to least cases####

vars2 <- match(repeats_SubSyndrome$`Patient ID`, SubSyndrome$C_Unique_Patient_ID)
#create variable containing rows of patients that have multiple cases####

repeats_SubSyndrome_dem <- SubSyndrome[vars2,]
#create table with only repeat patients####
repeats_SubSyndrome_dem <- repeats_SubSyndrome_dem[vars]
#create table with only useful information about the repeat patients####

repeats_SubSyndrome_dem <- sqldf('
                     select * from repeats_SubSyndrome_dem
                     left join repeats_SubSyndrome
                     on repeats_SubSyndrome_dem.C_Unique_Patient_ID = 
                     repeats_SubSyndrome.`Patient ID`
                     ')
repeats_SubSyndrome_dem <- repeats_SubSyndrome_dem[,-c(15)]
#create table with repeat patient demographics and their frequencies####
repeats_SubSyndrome_dem_age_sex <- as.data.frame.matrix(
                                                 table(repeats_SubSyndrome_dem$Age_Group, 
                                                       repeats_SubSyndrome_dem$Sex))
#create table of repeats by age and sex####

# Filter out duplicate cases ####

ICD10_working3 <- ICD10_working2[
  !ICD10_working2$EssenceID %in% ChiefComplaint_working2$EssenceID &
  !ICD10_working2$EssenceID %in% SubSyndrome_working2$EssenceID,
  ]
ICD10_working3$Query <- "ICD Codes"

ChiefComplaint_working3 <- ChiefComplaint_working2[
  !ChiefComplaint_working2$EssenceID %in% ICD10_working2$EssenceID & 
  !ChiefComplaint_working2$EssenceID %in% SubSyndrome_working2$EssenceID,
]
ChiefComplaint_working3$Query <- "ChiefComplaint" 

SubSyndrome_working3 <- SubSyndrome_working2[
  !SubSyndrome_working2$EssenceID %in% ICD10_working2$EssenceID &
  !SubSyndrome_working2$EssenceID %in% ICD10_working2$EssenceID,
]
SubSyndrome_working3$Query <- "SubSyndrome"

attempts <- do.call("rbind", list(ICD10_working3, ChiefComplaint_working3, 
                                  SubSyndrome_working3))
attempts_undup <- attempts[!duplicated(attempts$C_Unique_Patient_ID),]
attempts_overlap <- attempts[duplicated(attempts$C_Unique_Patient_ID),]  
# Combine unduplicated cases from each query into single data set ####
# Combine duplicated cases from each query into single data set ####

attempts_undup$Discharge.Diagnosis <- str_remove_all(
  attempts_undup$Discharge.Diagnosis, "[~]")
attempts_undup$Discharge.Diagnosis <- str_replace_all(
  attempts_undup$Discharge.Diagnosis, "[;]", " ")
attempts_undup$Discharge.Diagnosis <- 
  sapply(attempts_undup$Discharge.Diagnosis, 
         function(x) paste(unique(unlist(strsplit(x, " ")))))
#### clean up ICD10 codes to make counting easier ####

attempts_undup$Method[
  grepl("T14", names(attempts_undup$Discharge.Diagnosis)) |
  grepl("T3", names(attempts_undup$Discharge.Diagnosis))  |
  grepl("T4", names(attempts_undup$Discharge.Diagnosis))  |
  grepl("T5", names(attempts_undup$Discharge.Diagnosis))  | 
  grepl("T6", names(attempts_undup$Discharge.Diagnosis))  |
  grepl("T7", names(attempts_undup$Discharge.Diagnosis))      
] <- "Intentional Poisoning"

attempts_undup$Method[
  grepl("X7", names(attempts_undup$Discharge.Diagnosis)) |
  grepl("X8", names(attempts_undup$Discharge.Diagnosis))     
] <- "Self Harm with Object"

attempts_undup$Method[
  (grepl("X7", names(attempts_undup$Discharge.Diagnosis)) |
   grepl("X8", names(attempts_undup$Discharge.Diagnosis))) &
  (grepl("T", names(attempts_undup$Discharge.Diagnosis)))  
] <- "Poisoning and Object"
# Group encounters by method ####

codes_working <- removePunctuation(unique(unlist(attempts$Discharge.Diagnosis)))
codes_working  <- as.data.frame(table(codes_working))
colnames(codes_working) <- c("CODE", "Freq")

icd10_codes <- read.csv("suicide_codes2.csv", stringsAsFactors = F)

codes <- merge(x = codes_working, y = icd10_codes, by.x="CODE", by.y="CODE", all.x = T)
codes$CODE <- as.character(codes$CODE)
codes1 <- codes[grep("^T", codes$CODE),]
codes2 <- codes[grep("^X", codes$CODE),]

suicide_code_freqs <- do.call("rbind", list(codes1, codes2))
# Count occurences of each suicide related ICD-10 code ####

write.xlsx2(attempts_undup, file = 
              "C:/Users/john.m/Documents/Projects/.xlsx", sheetName = "data")

# MFW GRAPHS ####

#myCorpus <- readtext(file = "C:/Users/john.m/Documents/Suicide report/ICD10.tm")
myCorpus.1 <- attempts_undup$CaseNotes
corpus.1 <- VCorpus(VectorSource(myCorpus.1))
toSpace.1 <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
corpus.1 <- tm_map(corpus.1, toSpace.1, "-")
corpus.1 <- tm_map(corpus.1, toSpace.1, ";")
corpus.1 <- tm_map(corpus.1, removePunctuation)
corpus.1 <- tm_map(corpus.1, removeNumbers)
corpus.1 <- tm_map(corpus.1, removeWords, stopwords("english"))
corpus.1 <- tm_map(corpus.1, removeWords, c(
  "reports", "states", "Patient", "called", "last", "EMS", "night"))
corpus.1 <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, stemDocument)

dtm.1 <-DocumentTermMatrix(corpus.1)
rowTotals.1 <- apply(dtm.1 , 1, sum) #Find the sum of words in each Document
dtm.1 <- dtm[rowTotals.1> 0, ] #remove all docs without words

m.1 <- as.matrix(as.TermDocumentMatrix(dtm.1))
v.1 <- sort(rowSums.1(m),decreasing=TRUE)
d.1 <- data.frame(word = names(v.1),freq=v.1)
head(d.1, 10)

set.seed(1234)

barplot((d.1[1:10,]$freq)/length(d.1$word)*100, las = 2, names.arg = d.1[1:10,]$word,
        col ="lightblue", main ="Most frequent words for One Time Visitors",
        ylab = "Relative Word frequencies(%)")


#myCorpus <- readtext(file = "C:/Users/john.m/Documents/Suicide report/ICD10.tm")
myCorpus.2 <- attempts_repeats$CaseNotes
corpus.2 <- VCorpus(VectorSource(myCorpus.2))
toSpace.2 <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
corpus.2 <- tm_map(corpus.2, toSpace, "-")
corpus.2 <- tm_map(corpus.2, toSpace, ";")
corpus.2 <- tm_map(corpus.2, removePunctuation)
corpus.2 <- tm_map(corpus.2, removeNumbers)
corpus.2 <- tm_map(corpus.2, removeWords, stopwords("english"))
corpus.2 <- tm_map(corpus.2, removeWords, c(
  "reports", "states", "Patient", "called", "last", "EMS", "night"))
corpus.2 <- tm_map(corpus.2, stripWhitespace)
#corpus <- tm_map(corpus, stemDocument)

dtm.2 <-DocumentTermMatrix(corpus.2)
rowTotals.2 <- apply(dtm.2, 1, sum) #Find the sum of words in each Document
dtm.2   <- dtm.2[rowTotals.2 > 0, ] #remove all docs without words

m.2 <- as.matrix(as.TermDocumentMatrix(dtm.2))
v.2 <- sort(rowSums(m.2),decreasing=TRUE)
d.2 <- data.frame(word = names(v.2),freq=v.2)
head(d.2, 10)

set.seed(1234)

barplot((d.2[1:10,]$freq)/length(d.2$word)*100, las = 2, names.arg = d.2[1:10,]$word,
        col ="lightblue", main ="Most frequent words for Repeat Visitors",
        ylab = "Relative Word frequencies(%)")

