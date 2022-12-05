# load libraries ----------------------------------------------------------
library("plyr")
library("data.table")
library("readxl")
library("stargazer")
library("tidyverse")
library("xlsx")
library("ggplot2")
library("reshape2")
library("here")
library("patchwork")
library("dplyr")
library("doBy")


# import data -------------------------------------------------------------

#create outputs folder
main_dir <- here::here()
sub_dir <- "Outputs"
ifelse(!dir.exists(file.path(main_dir, sub_dir)), dir.create(file.path(main_dir, sub_dir)), F)
rm("main_dir", "sub_dir")

#load questionnaire answers
surveyresponses <- read.xlsx(here::here("survey_responses.xlsx"), sheetName = "data", 
                             as.data.frame = T, header = T)


# remove outliers from imported variables ---------------------------------

#'variables to include: grp_consent.farm_info.layhens_no , grp_consent.farm_info.layhens_eggperday, 
#'grp_consent.farm_info.layhens_nocycle , grp_consent.farm_info.layhens_mortalityrate , grp_consent.farm_info.broiler_no , 
#'grp_consent.farm_info.broiler_nocycle , grp_consent.farm_info.broiler_mortalityrate , grp_consent.farm_info.broiler_weight , 
#'grp_consent.antibiotics_use.antibiotics_nouse , grp_consent.antibiotics_nodayuse , grp_consent.atb_expenseegg , 
#'grp_consent.atb_expensebroiler
#'
summary(surveyresponses[,"grp_consent.farm_info.layhens_no"]) #good

summary(surveyresponses[,"grp_consent.farm_info.layhens_eggperday"])
#some big numbers, but unsure if disproportionate to number of layers - easier to weed out eggs per bird

summary(surveyresponses[,"grp_consent.farm_info.layhens_nocyle"]) #good

summary(surveyresponses[,"grp_consent.farm_info.layhens_mortalityrate"])
surveyresponses$grp_consent.farm_info.layhens_mortalityrate[surveyresponses$grp_consent.farm_info.layhens_mortalityrate > 900] <- NA 
summary(surveyresponses[,"grp_consent.farm_info.layhens_mortalityrate"])
hist(surveyresponses$grp_consent.farm_info.layhens_mortalityrate)
#there's a 100 and a 200, a couple of 50s and 30s, and apart from that below 10. Took out the 
#999 but left the others as they could plausibly be super-high-mortality farms with 
#outbreaks

summary(surveyresponses[,"grp_consent.farm_info.broiler_no"]) #good

summary(surveyresponses[,"grp_consent.farm_info.broiler_nocyle"])
surveyresponses$grp_consent.farm_info.broiler_nocyle[surveyresponses$grp_consent.farm_info.broiler_nocyle > 900] <- NA
summary(surveyresponses[,"grp_consent.farm_info.broiler_nocyle"]) #good, now no more than 36

summary(surveyresponses[,"grp_consent.farm_info.broiler_mortalityrate"])
surveyresponses$grp_consent.farm_info.broiler_mortalityrate[surveyresponses$grp_consent.farm_info.broiler_mortalityrate > 900] <- NA
summary(surveyresponses[,"grp_consent.farm_info.broiler_mortalityrate"]) 
hist(surveyresponses[,"grp_consent.farm_info.broiler_mortalityrate"]) 
#a few very high mortality rates, but not implausible if there were outbreaks

summary(surveyresponses[,"grp_consent.farm_info.broiler_weight"])
surveyresponses$grp_consent.farm_info.broiler_weight[surveyresponses$grp_consent.farm_info.broiler_weight > 900] <- NA
summary(surveyresponses[,"grp_consent.farm_info.broiler_weight"]) #removed some VERY high outliers ....

summary(surveyresponses[,"grp_consent.antibiotics_use.antibiotics_nouse"])
surveyresponses$grp_consent.antibiotics_use.antibiotics_nouse[surveyresponses$grp_consent.antibiotics_use.antibiotics_nouse > 900] <- NA
summary(surveyresponses[,"grp_consent.antibiotics_use.antibiotics_nouse"]) 
#cool - again some very high numbers but not implausible if there was an outbreak

summary(surveyresponses[,"grp_consent.antibiotics_use.antibiotics_nodayuse"]) #good

summary(surveyresponses[,"grp_consent.atb_expenceegg"])
surveyresponses$grp_consent.atb_expenceegg[surveyresponses$grp_consent.atb_expenceegg == 999] <- NA
summary(surveyresponses[,"grp_consent.atb_expenceegg"])

summary(surveyresponses[,"grp_consent.atb_expencebroiler"])
surveyresponses$grp_consent.atb_expencebroiler[surveyresponses$grp_consent.atb_expencebroiler == 999] <- NA
summary(surveyresponses[,"grp_consent.atb_expencebroiler"])

# generating variables to use ---------------------------------------------


# farm ID -----------------------------------------------------------------
#create a simple ID number for each farm
surveyresponses <- tibble::rowid_to_column(surveyresponses, "ID")


# vaccine protocol --------------------------------------------------------
#dummy for having a vaccine protocol
surveyresponses$grp_consent.intern_biosecuriy.vacc_protocol <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.vacc_protocol)
surveyresponses$vacc_protocol <- as.numeric(0)
surveyresponses$vacc_protocol[surveyresponses$grp_consent.intern_biosecuriy.vacc_protocol == 1] <- 1

#dummy for having a vaccine protocol that is adhered to
surveyresponses$grp_consent.intern_biosecuriy.vacc_protocolrespect <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.vacc_protocolrespect)
surveyresponses$vacc_adhere <- as.numeric(0)
surveyresponses$vacc_adhere[surveyresponses$grp_consent.intern_biosecuriy.vacc_protocolrespect == 1] <- 1

##create overall vaccination score / 3
surveyresponses$vacc_score <- as.numeric(0)
surveyresponses$vacc_score[surveyresponses$vacc_protocol == 0] = 0
surveyresponses$vacc_score[surveyresponses$vacc_protocol == 1 & surveyresponses$vacc_adhere == 0] = 1
surveyresponses$vacc_score[surveyresponses$vacc_adhere == 1] = 2


# disinfection protocol ---------------------------------------------------
##create overall disinfection score / 3
surveyresponses$disinfect_score <- as.numeric(0)

surveyresponses$grp_consent.intern_biosecuriy.desinfection_protocol <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.desinfection_protocol)
surveyresponses$grp_consent.intern_biosecuriy.desinfection_protocolrespct <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.desinfection_protocolrespct)

surveyresponses$disinfect_score[surveyresponses$grp_consent.intern_biosecuriy.desinfection_protocol == 0] = 0
surveyresponses$disinfect_score[surveyresponses$grp_consent.intern_biosecuriy.desinfection_protocol == 1 & surveyresponses$grp_consent.intern_biosecuriy.desinfection_protocolrespct == 0] = 1
surveyresponses$disinfect_score[surveyresponses$grp_consent.intern_biosecuriy.desinfection_protocolrespct == 1] = 2


# internal biosecurity score ----------------------------------------------

surveyresponses$grp_consent.intern_biosecuriy.deadchicken_remove <- 
  revalue(surveyresponses$grp_consent.intern_biosecuriy.deadchicken_remove, 
          c("2" = "0.5"))

surveyresponses$grp_consent.intern_biosecuriy.watersystem_cleandesinfect <- 
  revalue(surveyresponses$grp_consent.intern_biosecuriy.watersystem_cleandesinfect, 
          c("1"="1", "2" = "0.5", "3" = "0"))

surveyresponses$grp_consent.intern_biosecuriy.feedsystem_cleandesinfect <- 
  revalue(surveyresponses$grp_consent.intern_biosecuriy.feedsystem_cleandesinfect, 
          c("1"="1", "2" = "0.5", "3" = "0"))

##express all internal biosecurity variables as numerics
surveyresponses$grp_consent.intern_biosecuriy.biosecurit_assesment <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.biosecurit_assesment)
surveyresponses$grp_consent.intern_biosecuriy.chichen_differentage <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.chichen_differentage)
# surveyresponses$grp_consent.intern_biosecuriy.crawlspace_duration <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.crawlspace_duration)
surveyresponses$grp_consent.intern_biosecuriy.deadchicken_remove <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.deadchicken_remove)
surveyresponses$grp_consent.intern_biosecuriy.farm_cleaning <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.farm_cleaning)
 
surveyresponses$grp_consent.intern_biosecuriy.farm_desinfection <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.farm_desinfection)
surveyresponses$grp_consent.intern_biosecuriy.watersystem_cleandesinfect <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.watersystem_cleandesinfect)
surveyresponses$grp_consent.intern_biosecuriy.feedsystem_cleandesinfect <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.feedsystem_cleandesinfect)
surveyresponses$grp_consent.intern_biosecuriy.several_chickenhouse <- as.numeric(surveyresponses$grp_consent.intern_biosecuriy.several_chickenhouse)

surveyresponses$disinfect_score <- surveyresponses$disinfect_score / max(surveyresponses$disinfect_score)

##create the score as the sum of those variables
surveyresponses$intern_bio_score <- as.numeric(0)
surveyresponses$intern_bio_score <- surveyresponses$grp_consent.intern_biosecuriy.biosecurit_assesment +
  surveyresponses$grp_consent.intern_biosecuriy.chichen_differentage + 
  #'surveyresponses$grp_consent.intern_biosecuriy.crawlspace_duration +
  surveyresponses$grp_consent.intern_biosecuriy.deadchicken_remove +
  surveyresponses$grp_consent.intern_biosecuriy.farm_cleaning +
  surveyresponses$disinfect_score +
  surveyresponses$grp_consent.intern_biosecuriy.farm_desinfection +
  surveyresponses$grp_consent.intern_biosecuriy.watersystem_cleandesinfect +
  surveyresponses$grp_consent.intern_biosecuriy.feedsystem_cleandesinfect +
  surveyresponses$grp_consent.intern_biosecuriy.several_chickenhouse

##express as a portion of the maximum possible score
surveyresponses$intern_bio_score <- surveyresponses$intern_bio_score / 9

hist(surveyresponses$intern_bio_score)
summary(surveyresponses$intern_bio_score)


# external biosecurity score ----------------------------------------------

#'we want:
#'131: 2 -> 0.5
#'132: 2 -> 0.5, 3 -> 0
#'133: 1 -> 0, 2 -> 0.5, 3 -> 1
#'134: 2 -> 0.5, 3 -> 0
#'135: 2 -> 0.5, 3 -> 0
#'136: 2 -> 0, 3 -> 0
#'137: 1 -> 0, 2 -> 0.5, 3 -> 1
#'138: good as is
#'139: good as is
#'140: good as is
#'141: 2 -> 0.5, 3 -> 0
#'142: good as is
#'143: good as is
#'144: good as is
#'145: 0 -> 1, 1 -> 0
#'146: 0 -> 1, 1 -> 0
#'147: don't include
#'148: keep as is
#'149: 0 -> 1, 1 -> 0
#'150: 2 -> 0.5, 3 -> 0
#'151: 2 -> 0.5, 3 -> 0
#'152: 1 -> 0, 2 -> 0.5, 3 -> 1  
#'153: 0 -> 1, 1 -> 0
#'154: 1 -> 0, 2 -> 0.5, 3 -> 1
#'155: 1 -> 0, 2 -> 0.5, 3 -> 1 

a <- c(131)
for(i in a){
  surveyresponses[,i] <- revalue(surveyresponses[,i], c("2" = "0.5"))
}

b <- c(132, 134, 135, 141, 150, 151)
for(i in b){
  surveyresponses[,i] <- revalue(surveyresponses[,i], c("2" = "0.5", "3" = "0"))
}

c <- c(133, 137, 152, 154, 155)
for(i in c){
  surveyresponses[,i] <- revalue(surveyresponses[,i], c("1" = "0", "2" = "0.5", "3" = "1"))
}

d <- c(136)
for(i in d){
  surveyresponses[,i] <- revalue(surveyresponses[,i], c("2" = "0", "3" = "0"))
}

e <- c(145, 146, 149, 153)
for(i in e){
  surveyresponses[,i] <- revalue(surveyresponses[,i], c("0" = "1", "1" = "0"))
}

##express all external biosecurity variables as numerics
which( colnames(surveyresponses)=="grp_consent.extern_biosecuriy.chick_provider" )
which( colnames(surveyresponses)=="grp_consent.extern_biosecuriy.neighbor_manure" )
surveyresponses[, c(131:155)] <- lapply(surveyresponses[, c(131:155)], as.numeric)

##create the external biosecurity score
surveyresponses$extern_bio_score <- as.numeric(0)
for(i in 1:nrow(surveyresponses)){
  surveyresponses$extern_bio_score[i] <- mean(na.exclude(as.numeric(surveyresponses[i,c(131:146, 148:155)])))
}

hist(surveyresponses$extern_bio_score)
summary(surveyresponses$extern_bio_score)

# total biosecurity score -------------------------------------------------
surveyresponses$total_bio_score <- 0.5 * (surveyresponses$intern_bio_score + surveyresponses$extern_bio_score)
hist(surveyresponses$total_bio_score)
summary(surveyresponses$total_bio_score)


# antibiotic usage --------------------------------------------------------

#use yes/no
surveyresponses$grp_consent.antibiotics_use.antibiotics_utilization
surveyresponses$AMU <- as.numeric(surveyresponses$grp_consent.antibiotics_use.antibiotics_utilization)

#create binary variables for each kind of AMU

#'reasons for use: 1 = prevent disease, 2 = treat existing illness,
#'3 = promote growth of birds, 4 = avoid clinical signs of disease if already infected,
#'5 = reduce subject's stress, 6 = others
which( colnames(surveyresponses)=="grp_consent.antibiotics_use.antibiotics_reasons" ) #65
surveyresponses$grp_consent.antibiotics_use.antibiotics_reasons

surveyresponses$use_clinical <- as.numeric(0)
surveyresponses$use1 <- as.numeric(0)
surveyresponses$use2 <- as.numeric(0)
surveyresponses$use3 <- as.numeric(0)
surveyresponses$use4 <- as.numeric(0)
surveyresponses$use5 <- as.numeric(0)
surveyresponses$use6 <- as.numeric(0)

use1 <- c(grep("1", surveyresponses[,65]))
for(i in use1){
  surveyresponses[i, "use1"] <- 1
}

use2 <- c(grep("2", surveyresponses[,65]))
for(i in use2){
  surveyresponses[i, "use2"] <- 1
}

use3 <- c(grep("3", surveyresponses[,65]))
for(i in use3){
  surveyresponses[i, "use3"] <- 1
}

use4 <- c(grep("4", surveyresponses[,65]))
for(i in use4){
  surveyresponses[i, "use4"] <- 1
}

use5 <- c(grep("5", surveyresponses[,65]))
for(i in use5){
  surveyresponses[i, "use5"] <- 1
}

use6 <- c(grep("6", surveyresponses[,65]))
for(i in use6){
  surveyresponses[i, "use6"] <- 1
}

for(i in 1:nrow(surveyresponses)){
  surveyresponses[i,"use_clinical"] <- max(surveyresponses[i,"use2"], surveyresponses[i,"use4"])
}

surveyresponses$use_agp <- as.numeric(0)
surveyresponses$use_agp <- surveyresponses$use3

surveyresponses$use_prophylaxis <- as.numeric(0)
surveyresponses$use_prophylaxis <- surveyresponses$use2

surveyresponses$use_nontherapeutic <- as.numeric(0)
for(i in 1:nrow(surveyresponses)){
  surveyresponses[i,"use_nontherapeutic"] <- max(surveyresponses[i,"use1"], surveyresponses[i,"use3"], 
                                                 surveyresponses[i,"use5"], surveyresponses[i,"use6"])
}

rm(use1, use2, use3, use4, use5, use6, i)

#use of antibiotics on healthy vs sick birds
which( colnames(surveyresponses)=="grp_consent.antibiotics_use.antibiotics_poultry" ) #68
surveyresponses$grp_consent.antibiotics_use.antibiotics_poultry
##create binary variables for each reason
#' 1 = healthy, 2 = sick, 1 and 2 = sick and healthy

surveyresponses$usesick <- as.numeric(0)
surveyresponses$usehealthy <- as.numeric(0)
surveyresponses$useboth <- as.numeric(0)

reason1 <- c(grep("1", surveyresponses[,68]))
for(i in reason1){
  surveyresponses[i, "usehealthy"] <- 1
}

reason2 <- c(grep("2", surveyresponses[,68]))
for(i in reason2){
  surveyresponses[i, "usesick"] <- 1
}

surveyresponses[,"useboth"] <- surveyresponses[,"usehealthy"] * surveyresponses[, "usesick"]

rm(i, reason1, reason2)

#quantity of AMU

##frequency of use 
surveyresponses$amu_times_per_cycle <- surveyresponses$grp_consent.antibiotics_use.antibiotics_nouse

##expenditure per flock

surveyresponses <- surveyresponses %>%
  mutate(grp_consent.atb_expencebroiler = if_else(is.na(grp_consent.atb_expencebroiler), 
                                                  0, grp_consent.atb_expencebroiler))

surveyresponses <- surveyresponses %>%
  mutate(grp_consent.atb_expenceegg = if_else(is.na(grp_consent.atb_expenceegg), 
                                                  0, grp_consent.atb_expenceegg))

surveyresponses$expense_per_flock_broilers <- as.numeric(0)
surveyresponses$expense_per_flock_layers <- as.numeric(0)

for(i in 1:nrow(surveyresponses)){
  surveyresponses[i,"expense_per_flock_broilers"] <- max(0, surveyresponses[i, "grp_consent.atb_expencebroiler"])
  surveyresponses[i,"expense_per_flock_layers"] <- max(0, surveyresponses[i, "grp_consent.atb_expenceegg"])
}

hist(surveyresponses$expense_per_flock_broilers)
hist(surveyresponses$expense_per_flock_layers)


# number of broilers and layers -------------------------------------------

surveyresponses$grp_consent.farm_info.layhens_no
surveyresponses$grp_consent.farm_info.broiler_no
  
surveyresponses$has_broilers <- as.numeric(0)
surveyresponses$has_layers <- as.numeric(0)
surveyresponses$has_both <- as.numeric(0)

surveyresponses$has_broilers[!is.na(surveyresponses$grp_consent.farm_info.broiler_no)] <- 1
surveyresponses$has_layers[!is.na(surveyresponses$grp_consent.farm_info.layhens_no)] <- 1
surveyresponses$has_both <- surveyresponses$has_broilers * surveyresponses$has_layers

for(i in 1:nrow(surveyresponses)){
  surveyresponses$number_broilers[i] <- max(0, surveyresponses$grp_consent.farm_info.broiler_no[i])
}

for(i in 1:nrow(surveyresponses)){
  surveyresponses$number_layerss[i] <- max(0, surveyresponses$grp_consent.farm_info.layhens_no[i])
}

#portion of broilers
surveyresponses <- surveyresponses %>%
  mutate(number_broilers = if_else(is.na(number_broilers), 0, number_broilers))
surveyresponses <- surveyresponses %>%
  mutate(number_layerss = if_else(is.na(number_layerss), 0, number_layerss))


surveyresponses$portion_broilers <- as.numeric(0)
surveyresponses$portion_broilers <- surveyresponses$number_broilers / (surveyresponses$number_broilers + 
                                                                         surveyresponses$number_layerss)

##expenditure per bird

surveyresponses$expense_per_broiler <- surveyresponses$expense_per_flock_broilers / surveyresponses$grp_consent.farm_info.broiler_no
surveyresponses$expense_per_layer <- surveyresponses$expense_per_flock_layers / surveyresponses$grp_consent.farm_info.layhens_no


# sensibilisation ---------------------------------------------------------
#has attended a course on livestock management or antibiotic use
surveyresponses$grp_consent.training_info.training <- as.numeric(surveyresponses$grp_consent.training_info.training)
surveyresponses$training <- surveyresponses$grp_consent.training_info.training

#training by topic
surveyresponses$training_antibiotics <- as.numeric(0)
surveyresponses$training_livestock <- as.numeric(0)
surveyresponses$training_both <- as.numeric(0)

which( colnames(surveyresponses) == "grp_consent.training_info.training_modules") #115
surveyresponses$grp_consent.training_info.training_modules
#1 = livestock modules, 2 = use of antibiotics

traininglivestock <- c(grep("1", surveyresponses[,115]))
for(i in traininglivestock){
  surveyresponses[i, "training_livestock"] <- 1
}

trainingabx <- c(grep("2", surveyresponses[,115]))
for(i in trainingabx){
  surveyresponses[i, "training_antibiotics"] <- 1
}

surveyresponses$training_both <- surveyresponses$training_antibiotics * surveyresponses$training_livestock

rm(i, trainingabx, traininglivestock)


# presence of other species -----------------------------------------------
surveyresponses$grp_consent.farm_info.species

surveyresponses$other_species <- as.numeric(0)
surveyresponses$other_species[surveyresponses$grp_consent.farm_info.species != "1"] <- 1


# farm size ---------------------------------------------------------------
surveyresponses$farmsize <- surveyresponses$number_broilers + surveyresponses$number_layerss


# Use of human drugs on animals -------------------------------------------
surveyresponses$human_medicine <- as.numeric(0)
surveyresponses$human_medicine <- surveyresponses$grp_consent.amr_knowledge.human_medicine


# AMR attitudes score -----------------------------------------------------

##express all external biosecurity variables as numerics
which( colnames(surveyresponses)=="grp_consent.attitudes.amr_aware" ) #108
which( colnames(surveyresponses)=="grp_consent.attitudes.atb_reduction" ) #112

##replace NAs with zeroes in col 112

surveyresponses <- surveyresponses %>%
  mutate(grp_consent.attitudes.atb_reduction = if_else(is.na(grp_consent.attitudes.atb_reduction), "0", grp_consent.attitudes.atb_reduction))

##express answers as numerics
surveyresponses[, c(108:112)] <- lapply(surveyresponses[, c(108:112)], as.numeric)

##express answers as portion of the maximum
for(i in 108:112){
  surveyresponses[,i] <- surveyresponses[,i] / max(na.exclude(surveyresponses[,i]))
}

##create the attitudes score
surveyresponses$attitude_score <- as.numeric(0)
for(i in 1:nrow(surveyresponses)){
  surveyresponses$attitude_score[i] <- mean(na.exclude(as.numeric(surveyresponses[i,108:112])))
}

hist(surveyresponses$attitude_score)
summary(surveyresponses$attitude_score)


# Incidence of disease ----------------------------------------------------
#load in disease dataset woooo
disease <- read.xlsx(here::here("survey_responses.xlsx"), sheetName = "grp_consent_disease_description", 
                     as.data.frame = T, header = T)

#remove rows with 999
disease <- disease[!(disease$grp_consent.disease_description.disease_freq == 999 |
                       disease$grp_consent.disease_description.treatment_duration == 999),]

#create variable for total days of disease
disease$diseasedays <- disease$grp_consent.disease_description.disease_freq *
  disease$grp_consent.disease_description.treatment_duration

disease <- summaryBy(grp_consent.disease_description.disease_freq + diseasedays ~ X_parent_index,
                         FUN = sum, data = disease)

disease <- plyr::rename(disease,
                  c("grp_consent.disease_description.disease_freq.sum" = "total_disease_incidents",
                    "diseasedays.sum" = "total_disease_days",
                    "X_parent_index" = "ID"
                    ))

#merge
surveyresponses <- merge(surveyresponses, disease, all = T)
rm(disease)

surveyresponses <- surveyresponses %>%
  mutate(total_disease_incidents = if_else(is.na(total_disease_incidents), 0, total_disease_incidents))

surveyresponses <- surveyresponses %>%
  mutate(total_disease_days = if_else(is.na(total_disease_days), 0, total_disease_days))

# Income, productivity and mortality --------------------------------------

##eggs per day
surveyresponses$eggs_per_day <- as.numeric(surveyresponses$grp_consent.farm_info.layhens_eggperday / 
                                             surveyresponses$number_layerss)


##broiler weight
surveyresponses$broiler_weight <- as.numeric(surveyresponses$grp_consent.farm_info.broiler_weight)

surveyresponses$weight_times_cycles <- as.numeric(surveyresponses$broiler_weight * 
                                                    surveyresponses$grp_consent.farm_info.broiler_nocyle)


##mortality
surveyresponses$layermortality <- as.numeric(surveyresponses$grp_consent.farm_info.layhens_mortalityrate)

surveyresponses$broilermortality <- as.numeric(surveyresponses$grp_consent.farm_info.broiler_mortalityrate)

surveyresponses$temp1 <- surveyresponses$portion_broilers * surveyresponses$broilermortality

surveyresponses$temp2 <- (1 - surveyresponses$portion_broilers) * surveyresponses$layermortality

surveyresponses <- surveyresponses %>%
  mutate(temp1 = if_else(is.na(temp1), 0, temp1))

surveyresponses <- surveyresponses %>%
  mutate(temp2 = if_else(is.na(temp2), 0, temp2))

surveyresponses$overallmortality <- surveyresponses$temp1 + surveyresponses$temp2

surveyresponses <- surveyresponses[ -c(231:232)]


# use determined by trained professional ----------------------------------

surveyresponses$grp_consent.amr_knowledge.drug_useresponsible
#we need anyone who selected "1", "2" or "3"

which( colnames(surveyresponses)=="grp_consent.amr_knowledge.drug_useresponsible" ) #89

surveyresponses$usevet <- as.numeric(0)
surveyresponses$useparavet <- as.numeric(0)
surveyresponses$uselivestockhelper <- as.numeric(0)

usevet <- c(grep("1", surveyresponses[,89]))
for(i in usevet){
  surveyresponses[i, "usevet"] <- 1
}

useparavet <- c(grep("2", surveyresponses[,89]))
for(i in useparavet){
  surveyresponses[i, "useparavet"] <- 1
}

uselivestockhelper <- as.numeric(c(grep("3", surveyresponses[,89])))
for(i in uselivestockhelper){
  surveyresponses[i, "uselivestockhelper"] <- 1
}

surveyresponses$useprofessional <- as.numeric(0)

for(i in 1:nrow(surveyresponses)){
  surveyresponses[i,"useprofessional"] <- max(surveyresponses[i,"usevet"], 
                                              surveyresponses[i,"useparavet"],
                                              surveyresponses[i,"uselivestockhelper"])
}

rm(usevet, useparavet, uselivestockhelper, i)

summary(surveyresponses$useprofessional) #84% had use advised by a professional

# length of production cycles ---------------------------------------------

surveyresponses$layercycles <- as.numeric(surveyresponses$grp_consent.farm_info.layhens_nocyle)
surveyresponses$broilercycles <- as.numeric(surveyresponses$grp_consent.farm_info.broiler_nocyle)


# create a variable for expense per bird ----------------------------------

surveyresponses$grp_consent.atb_expencebroiler
surveyresponses$grp_consent.atb_expenceegg
surveyresponses$farmsize

surveyresponses$expense_per_bird <- 
  (surveyresponses$grp_consent.atb_expencebroiler + surveyresponses$grp_consent.atb_expenceegg) /
  surveyresponses$farmsize

surveyresponses$expense_per_bird

surveyresponses$expense_per_broiler
surveyresponses$expense_per_layer

surveyresponses$AMU

# remove outliers from new variables --------------------------------------

#'variables to include: vacc_score , intern_bio_score , extern_bio_score , total_bio_score , AMU , 
#'use_nontherapeutic , usehealthy , amu_times_per_cycle , expense_per_flock_broilers , expense_per_flock_layers , 
#'number_broilers , number_layerss , portion_broilers , expense_per_broiler , expense_per_layer , farmsize , 
#'human_medicine , attitude_score , eggs_per_day , broiler_weight , weight_times_cycles , layermortality , 
#'broilermortality , overallmortality , layercycles , broilercycles

summary(surveyresponses[,"vacc_score"])

summary(surveyresponses[,"intern_bio_score"])

summary(surveyresponses[,"extern_bio_score"])

summary(surveyresponses[,"total_bio_score"])

summary(surveyresponses[,"AMU"])

summary(surveyresponses[,"use_nontherapeutic"])

summary(surveyresponses[,"usehealthy"])

summary(surveyresponses[,"amu_times_per_cycle"])

summary(surveyresponses[,"expense_per_flock_broilers"])

summary(surveyresponses[,"expense_per_flock_layers"])

summary(surveyresponses[,"number_broilers"])

summary(surveyresponses[,"number_layerss"])

summary(surveyresponses[,"portion_broilers"])

summary(surveyresponses[,"expense_per_broiler"])

summary(surveyresponses[,"expense_per_layer"])

summary(surveyresponses[,"farmsize"])
hist(surveyresponses$farmsize)
surveyresponses$farmsize[surveyresponses$farmsize <= 200]
surveyresponses$farmsize[surveyresponses$farmsize <= 10]
surveyresponses <- surveyresponses[surveyresponses$farmsize >= 10,]

summary(surveyresponses[,"human_medicine"])
surveyresponses$human_medicine <- as.numeric(surveyresponses$human_medicine)

summary(surveyresponses[,"attitude_score"])

summary(surveyresponses[,"eggs_per_day"])
hist(surveyresponses$eggs_per_day)
surveyresponses$eggs_per_day[surveyresponses$eggs_per_day > 10]
surveyresponses$eggs_per_day[surveyresponses$eggs_per_day > 10] <- NA

surveyresponses$eggs_per_day[surveyresponses$eggs_per_day > 2.5]
#seems like the two very high results (3.6 and 3.84) are plausible, just a bit high

summary(surveyresponses[,"broiler_weight"])

summary(surveyresponses[,"weight_times_cycles"])
hist(surveyresponses$weight_times_cycles)

summary(surveyresponses[,"layermortality"])
hist(surveyresponses$layermortality)

summary(surveyresponses[,"broilermortality"])

summary(surveyresponses[,"overallmortality"])

summary(surveyresponses[,"layercycles"])

summary(surveyresponses[,"broilercycles"])
hist(surveyresponses$broilercycles)
#' there are some going up to 24 and 30, and one that's 36. It seems a bit fishy
#' that a broiler could be raised and sold in only 10 days, but at the same time 
#' maybe some farms only concern themselves with one part of the production process.
#' This is a potential source of bias for sure though.

# Keep desired columns ----------------------------------------------------
which( colnames(surveyresponses)=="grp_consent.extern_biosecuriy.chick_provider" )
which( colnames(surveyresponses)=="grp_consent.extern_biosecuriy.neighbor_manure" )
# which( colnames(surveyresponses)=="grp_consent.intern_biosecuriy.biosecurit_assesment" )
# which( colnames(surveyresponses)=="grp_consent.intern_biosecuriy.farm_cleaning" )
which( colnames(surveyresponses)=="grp_consent.intern_biosecuriy.vacc_protocol" )
which( colnames(surveyresponses)=="grp_consent.intern_biosecuriy.several_chickenhouse" )
which( colnames(surveyresponses)=="vacc_protocol" )

#' For future reference, the columns for external biosecurity are 131:155
#' Internal biosecurity is 156:168
#' ID is 1
#' Newly created vars are 184-231
#' #'use determined by a responsible person is 235
#' number of production cycles are 236-237

surveyresponses <- surveyresponses[,c(1, 131:166, 184:231, 235:237)]

# Save new dataset --------------------------------------------------------
write.xlsx(surveyresponses, "AMUSE_cleaned.xlsx", replace)


