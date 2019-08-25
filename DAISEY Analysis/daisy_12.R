### Script for formatting INSIGHT data into CSVs for entry into DAISY ###

setwd("//dc03/Groups/INFORMATICS/John Mathena/Projects/Daisey_Analysis/2018_12")
# sets working directory to correct folder ####

# Step 1 - load any necessary libraries

library(dplyr)
library(digest)
library(RSQLite)
library(sqldf)
library(plyr)
library(gmodels)
library(data.table)
library(stringi)
library(lubridate)
library(devtools)
library(tab)
library(rapportools)
library(stringr)
library(rJava)
library(xlsx)
library(descr)

# Step 2 - import INSIGHT data into R

filename <- read.csv("dem_july_dec_rep.csv", stringsAsFactors = F)

# Step 3 - take out voids

filename2 <- read.csv("void_july_dec_rep.csv", stringsAsFactors = F)

filename2_working <- read.csv("void_july_dec_rep.csv", stringsAsFactors = F)
filename2_working <- filename2_working[order(filename2_working$Patient_no),]

filename3 <- sqldf('
                   select * from filename
                   left join filename2_working 
                   on filename.Encounter_no = filename2_working.Encounter_no')
filename3 <- filename3[order(filename3$Reason_void),]
filename4 <- filename3[which(is.na(filename3$Reason_void) == T), ]

# Step 4 - add description variable

filename5 <- filename4
filename5[, "description"] <- ""
filename5$description[filename5$Contra_end == "01"] <- "Oral Contraceptive"
filename5$description[filename5$Contra_end == "02"] <- "Diaphragm"
filename5$description[filename5$Contra_end == "03"] <- "Hormone Implant"
filename5$description[filename5$Contra_end == "04"] <- "Hormone Injection (3 month)"
filename5$description[filename5$Contra_end == "05"] <- "Cervical Cap"
filename5$description[filename5$Contra_end == "06"] <- "Female Sterilization"
filename5$description[filename5$Contra_end == "07"] <- "Sponge"
filename5$description[filename5$Contra_end == "08"] <- "Male Condom"
filename5$description[filename5$Contra_end == "09"] <- "Spermicide"
filename5$description[filename5$Contra_end == "10"] <- "IUD"
filename5$description[filename5$Contra_end == "11"] <- "NFP / FAM"
filename5$description[filename5$Contra_end == "12"] <- "Other"
filename5$description[filename5$Contra_end == "13"] <- "None (see Reason for No Contra)"
filename5$description[filename5$Contra_end == "14"] <- "Method Unknown"
filename5$description[filename5$Contra_end == "15"] <- "Ring"
filename5$description[filename5$Contra_end == "16"] <- "Patch"
filename5$description[filename5$Contra_end == "17"] <- "Abstinence"
filename5$description[filename5$Contra_end == "18"] <- "Female Condom"
filename5$description[filename5$Contra_end == "19"] <- "Male Rely on Female Method"
filename5$description[filename5$Contra_end == "20"] <- "Vasectomy"
filename5$description[filename5$Contra_end == "21"] <- "Hormone Injection (1 month)"
filename5$description[filename5$Contra_end == "22"] <- "DPMA - RGM"
filename5$description[filename5$Contra_end == "23"] <- "DPMA - LGM"
filename5$description[filename5$Contra_end == "24"] <- "DPMA - RD"
filename5$description[filename5$Contra_end == "25"] <- "DPMA - LD"
filename5$description[filename5$Contra_end == "26"] <- "DPMA - RVG"
filename5$description[filename5$Contra_end == "27"] <- "DPMA - LVG"
filename5$description[filename5$Contra_end == "DM"] <- "DMPA"
filename5$description[filename5$Contra_end == "LE"] <- "Levora"
filename5$description[filename5$Contra_end == "MC"] <- "Micronor"
filename5$description[filename5$Contra_end == "oc"] <- "Ortho-Cyclen"
filename5$description[filename5$Contra_end == "OD"] <- "Ortho Diaphragm"
filename5$description[filename5$Contra_end == "OT"] <- "Ortho-Tricyclen"
filename5$description[filename5$Contra_end == "28"] <- "Rely on Male Vasectomy"
filename5$description[filename5$Contra_end == "PO"] <- "Oral Contraceptive"
filename5$description[filename5$Contra_end == ""] <- "Unknown"

check1 <- filename5[is.na(filename5$description==T),]
ifelse(nrow(check1)>1, "error", "check")

check2 <- filename5[filename5$description=="DM"|
                      filename5$description=="LE"|
                      filename5$description=="MC"|
                      filename5$description=="MC"|
                      filename5$description=="oc"|
                      filename5$description=="OD"|
                      filename5$description=="OT"|
                      filename5$description=="PO", ]
ifelse(nrow(check2) > 1, "error", "check")

filename6 <- subset(filename5, select = -c(Reason_void))
filename6 <- filename6[, -(20:22)]

filename7 <- filename6[
  order(filename6$Patient_no),]

# Step 5 - unduplicate data

filename8 <- filename7[!duplicated(filename7$Patient_no), ] #unduplicate table using Patient_no
filename8$Reason_no_contraception[is.na(filename8$Reason_no_contraception)] <- "empty"
filename8[,"age_category"] <- ""
filename8[,"reason"] <- ""

#Step 6 - add age category

filename8$age_category[filename8$Age<15] <- "<15"
filename8$age_category[filename8$Age >= 15 & filename8$Age <= 17] <- "15-17"
filename8$age_category[filename8$Age >= 18 & filename8$Age <= 19] <- "18-19"
filename8$age_category[filename8$Age >= 20 & filename8$Age <= 24] <- "20-24"
filename8$age_category[filename8$Age >= 25 & filename8$Age <= 29] <- "25-29"
filename8$age_category[filename8$Age >= 30 & filename8$Age <= 34] <- "30-34"
filename8$age_category[filename8$Age >= 35 & filename8$Age <= 39] <- "35-39"
filename8$age_category[filename8$Age >= 40 & filename8$Age <= 44] <- "40-44"
filename8$age_category[filename8$Age >= 45] <- "45+"

# Step 7 - re-classify reasons

filename8$reason[filename8$Reason_no_contraception == 1] <- "Pregnant or seeking Pregnancy"
filename8$reason[filename8$Reason_no_contraception == 2] <- "Other reason"

filename8$description[filename8$reason == "Pregnant or seeking Pregnancy"] <- "Pregnant or seeking Pregnancy"
filename8$description[filename8$reason == "Other reason" & 
                        filename8$description == "None (see Reason for No Contra)"] <- "Other reason"

check1 <- filename8[is.na(filename8$description==T),]
ifelse(nrow(check1)>1, "error", "check")

check2 <- filename5[filename8$description=="DM"|
                      filename8$description=="LE"|
                      filename8$description=="MC"|
                      filename8$description=="MC"|
                      filename8$description=="oc"|
                      filename8$description=="OD"|
                      filename8$description=="OT"|
                      filename8$description=="PO", ]
ifelse(nrow(check2) > 1, "error", "check")

# Step 8 - re-classify data labels

## Race and Ethnicity

filename8$Race[filename8$Race=="I"] <- "American Indian"
filename8$Race[filename8$Race=="A"] <- "Asian"
filename8$Race[filename8$Race=="B"] <- "Black"
filename8$Race[filename8$Race=="P"] <- "Native Hawaiian or other Pacific Islander"
filename8$Race[filename8$Race=="W"] <- "White"
filename8$Race[is.empty(filename8$Race)==F & is.empty(filename8$Race_2) ==F] <- "More than one race"
filename8$Race[filename8$Race=="U"] <- "Unknown"

filename8$Ethnicity[filename8$Ethnicity=="H"] <- "Hispanic or Latino"
filename8$Ethnicity[filename8$Ethnicity=="NH"] <- "Not Hispanic or Latino"
filename8$Ethnicity[filename8$Ethnicity=="U"] <- "Unknown/Not Reported"
filename8$Ethnicity[filename8$Ethnicity=="M"] <- "Hispanic or Latino"
filename8$Ethnicity[filename8$Ethnicity=="CS"] <- "Hispanic or Latino"
filename8$Ethnicity[filename8$Ethnicity=="C"] <- "Hispanic or Latino"
filename8$Ethnicity[filename8$Ethnicity=="PR"] <- "Hispanic or Latino"

## Poverty based on reported income

filename8[,"pov"] <- ""
filename8[,"pov2"] <- ""
filename8[,"pov3"] <- ""
filename8[,"pov4"] <- ""

filename8$pov <- (filename8$Family_size * 4180) + 7880
filename8$pov2 <- filename8$pov *1.5
filename8$pov3 <- filename8$pov *2
filename8$pov4 <- filename8$pov * 2.5

filename8[,"poverty"] <- ""
filename8$poverty[filename8$Total_income <= filename8$pov] <- "100% and below"
filename8$poverty[filename8$pov < filename8$Total_income & 
                    filename8$Total_income<= filename8$pov2] <- "101%-150%"
filename8$poverty[filename8$pov2 < filename8$Total_income & 
                    filename8$Total_income<= filename8$pov3] <- "151%-200%"
filename8$poverty[filename8$pov3 < filename8$Total_income & 
                    filename8$Total_income<= filename8$pov4] <- "201%-250%"
filename8$poverty[filename8$Total_income >= filename8$pov4] <- "Over 250%"
filename8$poverty[filename8$Total_income == 999999] <- "Unknown/ not reported"

# Step 9 - create tables for Daisy

filename9 <- filename8

## make table for count by age
age_sex <- as.data.frame.matrix(table(filename9$age_category, filename9$Sex))
write.csv(age_sex, file = "undup_count_age.csv")

## make table for count by race and ethnicity
race_ethnicity_sex <- filename9[,c("Race","Sex","Ethnicity")] 
race_ethnicity_sexM <- race_ethnicity_sex[race_ethnicity_sex$Sex=="M",]
race_ethnicity_sexM <- as.data.frame.matrix(table(race_ethnicity_sexM$Race, race_ethnicity_sexM$Ethnicity))
race_ethnicity_sexF <- race_ethnicity_sex[race_ethnicity_sex$Sex=="F",]
race_ethnicity_sexF <- as.data.frame.matrix(table(race_ethnicity_sexF$Race, race_ethnicity_sexF$Ethnicity))
write.csv(race_ethnicity_sexM, file = "undup_count_race_male.csv")
write.csv(race_ethnicity_sexF, file = "undup_count_race_female.csv")

## make tables for contra by males and contra by females
contra_total <- filename9[,c("Sex", "description", "age_category")]
contra_males <- contra_total[contra_total$Sex == "M",]
contra_males <- contra_males[,c("description", "age_category")]
contra_males <- as.data.frame.matrix(
  table(contra_males$description, contra_males$age_category))

contra_males <- na.omit(contra_males)
write.csv(contra_males, file = "contra_males.csv")

contra_females <- contra_total[contra_total$Sex == "F",]
contra_females <- contra_females[,c("description", "age_category")]
contra_females <- table(contra_females$description, 
                        contra_females$age_category)
contra_females <- as.data.frame.matrix(contra_females)
contra_females <- na.omit(contra_females)
write.csv(contra_females, file = "contra_females.csv")

## make tables for poverty
poverty_analysis <- table(filename9$poverty)
write.csv(poverty_analysis, file = "poverty_analysis.csv")

## make tables for health insurance
filename10 <- read.csv(file = "health_insurance_dec_rep.csv")
filename10 <-filename10[!duplicated(filename10$Patient_no), ]

filename10$Payment_source[filename10$Payment_source==1 | 
                            filename10$Payment_source==6] <- "Medicaid"
filename10$Payment_source[filename10$Payment_source==2] <- "Private"
filename10$Payment_source[filename10$Payment_source==3] <- "No Coverage"
filename10$Payment_source[filename10$Payment_source==4] <- "Unknown"


insurance <- table(filename10$Payment_source)
write.csv(insurance, file = "insurance_analysis.csv")

## make tables for limited english proficiency
filename11 <- read.csv(file = "lep_dec_rep.csv")
filename11 <-filename11[!duplicated(filename11$Patient_no), ]

filename11$Limited_english_prof[filename11$Primary_language=="E"] <- "N"
table(filename11$Limited_english_prof)
lep_primary_lang <- as.data.frame.matrix(xtabs(~filename11$Limited_english_prof+filename11$Primary_language, data=filename11))
write.csv(lep_primary_lang, file = "lep_primary_lang.csv")

## make tables for pap smear
filename12 <- read.csv(file = "pap_dec_rep.csv")

pap_analysis <- table(filename12$Descriptive_result)
write.csv(pap_analysis, file = "pap_analysis_sep.csv")

## make tables for Breast Exams
filename13 <- read.csv(file = "breast_exam_dec_rep.csv")
breast_exam_analysis <- table(filename13$Exam_lab_code)
write.csv(breast_exam_analysis, file = "breast_exam_analysis_sep.csv")

## make tables for chlamydia
filename14 <- read.csv(file = "chlamydia_dec_rep.csv")
filename14[,"age_category"] <- ""

filename14$age_category[filename14$Age<15] <- "<15"
filename14$age_category[filename14$Age >= 15 & filename14$Age < 18] <- "15-17"
filename14$age_category[filename14$Age >= 18 & filename14$Age < 20] <- "18-19"
filename14$age_category[filename14$Age >= 20 & filename14$Age < 25] <- "20-24"
filename14$age_category[filename14$Age >= 25 & filename14$Age < 30] <- "25-29"
filename14$age_category[filename14$Age >= 30 & filename14$Age < 35] <- "30-34"
filename14$age_category[filename14$Age >= 35 & filename14$Age < 40] <- "35-39"
filename14$age_category[filename14$Age >= 40 & filename14$Age < 45] <- "40-44"
filename14$age_category[filename14$Age >= 45] <- "45+"

chlamydia_analysis <- filename14[,c(4,11)]
chlamydia_analysis <- as.data.frame.matrix(table(chlamydia_analysis$age_category, chlamydia_analysis$Sex))
write.csv(chlamydia_analysis, file = "chlamydia_analysis_sep_rep.csv")

## make tables for Gonnorhea, Syphillis, and HIV
filename15 <- read.csv(file = "hiv_dec_rep.csv")

filename15[,"Tests"] <- ""
filename15$Tests[is.empty(filename15$Lab.Result) == F] <- "Tests"
hiv_analysis <- as.data.frame.matrix(table(filename15$Sex, filename15$Tests))
write.csv(hiv_analysis, file = "hiv_analysis_sep_rep.csv")

filename16 <- read.csv(file = "gonn_dec_rep.csv")
filename16[,"Tests"] <- ""
filename16$Tests[is.empty(filename16$Lab.Result) == F] <- "Tests"
gonn_analysis <- as.data.frame.matrix(table(filename16$Sex, filename16$Tests))
write.csv(gonn_analysis, file = "gonn_analysis_sep_rep.csv")

filename17 <- read.csv(file = "syph_dec_rep.csv")
filename17[,"Tests"] <- ""
filename17$Tests[is.empty(filename17$Lab.Result) == F] <- "Tests"
syph_analysis <- table(filename17$Sex, filename17$Tests)
write.csv(syph_analysis, file = "syph_analysis_sep_rep.csv")

## make tables for FPProvider
filename18 <- read.csv(file = "oldfpprovider_dec_rep.csv", stringsAsFactors = F)
filename19 <- read.csv("void_july_dec_rep.csv", stringsAsFactors = F)
filename20 <- sqldf(
  'select * from filename18 left join filename19 on
  filename18.Encounter_no = filename19.Encounter_no')
filename20 <- filename20[order(filename20$Reason_void),]
filename21 <- filename20[which(is.na(filename20$Reason_void) == T), ]
filename22 <- filename21[,-c(10:13)] ### CHECK

filename23 <- read.csv(file="fpprovider_dec_rep.csv", stringsAsFactors = F)

which(is.na(filename22$Fp_provider_type))

providers1 <- table(filename22$Fp_provider_type, filename22$Primary_provider_code)
write.csv(providers1, file = "providers1.csv")

filename24 <- sqldf(
  'select * from filename22 left join filename23 on 
              filename22.Encounter_no = filename23.Encounter_no')
which(is.na(filename24$Provider_hours)) 

filename24 <- filename24[!filename24$Patient_no == 113245,]

filename24[,"Encounter_month"] <- ""
filename25 <- filename24
filename25$Encounter_date <- as.Date(filename25$Encounter_date, format = "%m/%d/%Y")
filename25$Encounter_month<- months(filename25$Encounter_date)

encounters_by_provider <- table(filename25$Fp_provider_type)
write.csv(encounters_by_provider, file = "encounters_by_provider.csv")
filename26 <- filename25
minutes_per_position <- filename26[,c("Fp_provider_type", "Provider_minutes", "Encounter_month")]
minutes_per_position <- minutes_per_position[order(minutes_per_position$Encounter_month),]

mpp_july <- minutes_per_position[minutes_per_position$Encounter_month=="July",]
mpp_july <- mpp_july[,-c(3)]
aprn_july_mins <- sum(mpp_july$Provider_minutes[mpp_july$Fp_provider_type=="APRN"])
rn_july_mins <- sum(mpp_july$Provider_minutes[mpp_july$Fp_provider_type=="RN"])
physician_july_mins <- sum(mpp_july$Provider_minutes[mpp_july$Fp_provider_type=="Physician"])

mpp_aug <- minutes_per_position[minutes_per_position$Encounter_month=="August",]
mpp_aug <- mpp_aug[,-c(3)]
aprn_aug_mins <- sum(mpp_aug$Provider_minutes[mpp_aug$Fp_provider_type=="APRN"])
rn_aug_mins <- sum(mpp_aug$Provider_minutes[mpp_aug$Fp_provider_type=="RN"])
physician_aug_mins <- sum(mpp_aug$Provider_minutes[mpp_aug$Fp_provider_type=="Physician"])

mpp_sep <- minutes_per_position[minutes_per_position$Encounter_month=="September",]                   
mpp_sep <- mpp_sep[,-c(3)]
aprn_sep_mins <- sum(mpp_sep$Provider_minutes[mpp_sep$Fp_provider_type=="APRN"])
rn_sep_mins <- sum(mpp_sep$Provider_minutes[mpp_sep$Fp_provider_type=="RN"])
physician_sep_mins <- sum(mpp_sep$Provider_minutes[mpp_sep$Fp_provider_type=="Physician"])

mpp_oct <- minutes_per_position[minutes_per_position$Encounter_month=="October",]                   
mpp_oct <- mpp_oct[,-c(3)]
aprn_oct_mins <- sum(mpp_oct$Provider_minutes[mpp_oct$Fp_provider_type=="APRN"])
rn_oct_mins <- sum(mpp_oct$Provider_minutes[mpp_oct$Fp_provider_type=="RN"])
physician_oct_mins <- sum(mpp_oct$Provider_minutes[mpp_oct$Fp_provider_type=="Physician"])

mpp_nov <- minutes_per_position[minutes_per_position$Encounter_month=="November",]                   
mpp_nov <- mpp_nov[,-c(3)]
aprn_nov_mins <- sum(mpp_nov$Provider_minutes[mpp_nov$Fp_provider_type=="APRN"])
rn_nov_mins <- sum(mpp_nov$Provider_minutes[mpp_nov$Fp_provider_type=="RN"])
physician_nov_mins <- sum(mpp_nov$Provider_minutes[mpp_nov$Fp_provider_type=="Physician"])

mpp_dec <- minutes_per_position[minutes_per_position$Encounter_month=="December",]                   
mpp_dec <- mpp_dec[,-c(3)]
aprn_dec_mins <- sum(mpp_dec$Provider_minutes[mpp_dec$Fp_provider_type=="APRN"])
rn_dec_mins <- sum(mpp_dec$Provider_minutes[mpp_dec$Fp_provider_type=="RN"])
physician_dec_mins <- sum(mpp_dec$Provider_minutes[mpp_dec$Fp_provider_type=="Physician"])

filename27 <- data.frame(
  "July-18" = c(aprn_july_mins, physician_july_mins, rn_july_mins),
  "Aug-18"= c(aprn_aug_mins, physician_aug_mins, rn_aug_mins),
  "Sep-18" = c(aprn_sep_mins, physician_sep_mins, rn_sep_mins),
  "Oct-18" = c(aprn_oct_mins, physician_oct_mins, rn_oct_mins),
  "Nov-18" = c(aprn_nov_mins, physician_nov_mins, rn_nov_mins),
  "Dec-18" = c(aprn_dec_mins, physician_dec_mins, rn_dec_mins))
row.names(filename27) <- c("APRN", "Physician", "RN")

write.csv(filename27, file="fpp_provider_analysis.csv")

# Step 10 - Put all csvs into single workbook (OPTIONAL)

write.xlsx(age_sex, file = "daisy_reporting_master.xlsx", sheetName = "Age", 
           col.names = T, row.names =  T, append = F)
write.xlsx(race_ethnicity_sexM, file = "daisy_reporting_master.xlsx", sheetName = "Race_Ethnicity_Males", 
           col.names = T, row.names = T, append = T)
write.xlsx(race_ethnicity_sexF, file = "daisy_reporting_master.xlsx", sheetName = "Race_Ethnicity_Females", 
           col.names = T, row.names = T, append = T)
write.xlsx(contra_males, file = "daisy_reporting_master.xlsx", sheetName = "Contra_Males", 
           col.names = T, row.names = T, append = T)
write.xlsx(contra_females, file = "daisy_reporting_master.xlsx", sheetName = "Contra_Females", 
           col.names = T, row.names = T, append = T)
write.xlsx(poverty_analysis, file = "daisy_reporting_master.xlsx", sheetName = "Poverty", 
           col.names = T, row.names = T, append = T)
write.xlsx(insurance, file = "daisy_reporting_master.xlsx", sheetName = "Insurance", 
           col.names = T, row.names = T, append = T)
write.xlsx(lep_primary_lang, file = "daisy_reporting_master.xlsx", sheetName = "LEP", 
           col.names = T, row.names = T, append = T)
write.xlsx(pap_analysis, file = "daisy_reporting_master.xlsx", sheetName = "Pap_Smear", 
           col.names = T, row.names = T, append = T)
write.xlsx(breast_exam_analysis, file = "daisy_reporting_master.xlsx", sheetName = "Breast_Exams", 
           col.names = T, row.names = T, append = T)
write.xlsx(chlamydia_analysis, file = "daisy_reporting_master.xlsx", sheetName = "Chlamydia", 
           col.names = T, row.names = T, append = T)
write.xlsx(hiv_analysis, file = "daisy_reporting_master.xlsx", sheetName = "HIV", 
           col.names = T, row.names = T, append = T)
write.xlsx(gonn_analysis, file = "daisy_reporting_master.xlsx", sheetName = "Gonorrhea", 
           col.names = T, row.names = T, append = T)
write.xlsx(syph_analysis, file = "daisy_reporting_master.xlsx", sheetName = "Syphilis", 
           col.names = T, row.names = T, append = T)
write.xlsx(filename27, file = "daisy_reporting_master.xlsx", sheetName = "FPProvider", 
           col.names = T, row.names = T, append = T)
write.xlsx(encounters_by_provider, file = "daisy_reporting_master.xlsx", sheetName = "Providers1", 
           col.names = T, row.names = T, append = T)
