# load libraries ----------------------------------------------------------
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
library("Hmisc")
library("corrplot")
library("multcomp")
library("sampleSelection")


# import data -------------------------------------------------------------

#create outputs folder
main_dir <- here::here()
sub_dir <- "Outputs"
ifelse(!dir.exists(file.path(main_dir, sub_dir)), dir.create(file.path(main_dir, sub_dir)), F)
rm("main_dir", "sub_dir")

#load cleaned dataset
df <- read.xlsx(here::here("AMUSE_cleaned.xlsx"), 1,
                             as.data.frame = T, header = T)


# tidying -----------------------------------------------------------------
#remove (NA.) column
df <- df[,-1]

#make human_medicine a numeric
df$human_medicine <- as.numeric(df$human_medicine)

#turn NAs to zeroes in total_disease_incidents, total_disease_days and amu_times_per_cycle
df <- df %>%
  mutate(total_disease_days = if_else(is.na(total_disease_days), 0, total_disease_days))
df <- df %>%
  mutate(total_disease_incidents = if_else(is.na(total_disease_incidents), 0, total_disease_incidents))
df <- df %>%
  mutate(amu_times_per_cycle = if_else(is.na(amu_times_per_cycle), 0, amu_times_per_cycle))

#create an expense_per_bird variable

#create an expense_per_bird variable

df$temp1 <- df$expense_per_broiler * df$portion_broilers
df$temp2 <- df$expense_per_layer * (1 - df$portion_broilers)

df <- df %>%
  mutate(temp1 = if_else(is.na(temp1), 0, temp1))
df <- df %>%
  mutate(temp2 = if_else(is.na(temp2), 0, temp2))
df$expense_per_bird <- df$temp1 + df$temp2

df <- df[ , !names(df) %in% c("temp1", "temp2")]

#fix AMU quantity

count(df$expense_per_bird == 0)

df$expense_per_bird[df$expense_per_bird == 0] <- NA
df$expense_per_bird[df$AMU == 0 & is.na(df$expense_per_bird)] <- 0
sum(is.na(df$expense_per_bird)) 

count(df$expense_per_bird == 0) 
count(df$expense_per_broiler == 0)
count(df$expense_per_layer == 0) 

#expense_per_broiler and expense_per_layer are missing a couple of zeroes (6 to be precise)

df$expense_per_broiler[df$expense_per_bird == 0 & df$has_broilers == 1 & df$has_layers == 0] <- 0
df$expense_per_layer[df$expense_per_bird == 0 & df$has_broilers == 0 & df$has_layers == 1] <- 0

count(df$expense_per_bird == 0) 
count(df$expense_per_broiler == 0)
count(df$expense_per_layer == 0) 

# Fix what's going on with eggs per day -----------------------------------
df$eggs_per_day
hist(df$eggs_per_day)
dfeggs <- df[df$eggs_per_day < 0.5 & df$has_layers == 1,]
dfeggs$eggs_per_day
dfeggs <- df[df$eggs_per_day < 0.1 & df$has_layers == 1,]
dfeggs$eggs_per_day

df$eggs_per_day[df$eggs_per_day <= 0.5] <- NA

rm(dfeggs)

# Summary Statistics ------------------------------------------------------

summarystats <- matrix(rep(0), nrow = 11, ncol = 7)
colnames(summarystats) <- c("Var", "Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max")

which( colnames(df)=="intern_bio_score")
which( colnames(df)=="extern_bio_score")
which( colnames(df)=="total_bio_score")
which( colnames(df)=="attitude_score")
which( colnames(df)=="farmsize")
which( colnames(df)=="expense_per_broiler")
which( colnames(df)=="expense_per_layer")
which( colnames(df)=="expense_per_bird")
which( colnames(df)=="total_disease_incidents")
which( colnames(df)=="weight_times_cycles")
which( colnames(df)=="eggs_per_day")

varlist <- c(42,43,44,77,75,68,69,89,78,82,80)

summarystats[,1] <- c("intern_bio_score","extern_bio_score","bio_score_total",
                      "attitude_score","farmsize","expense_per_broiler",
                      "expense_per_layer", "expense_per_bird",
                      "total_disease_incidents", "weight_times_cycles","eggs_per_day")

for(i in 1:nrow(summarystats)){
  summarystats[i,2:7] <- as.numeric(summary(df[,varlist[i]])[1:6])
}

write.xlsx(summarystats, "Outputs/SumStats1.xlsx")

par(mfrow = c(1,2))
hist(df$farmsize[df$has_broilers == 1 & df$has_layers == 0],
     xlab = "Number of birds",
     ylab = "Frequency",
     main = "Size of Broiler Farms",
     xlim = c(0,35000))
hist(df$farmsize[df$has_broilers == 0 & df$has_layers == 1],
     xlab = "Number of birds",
     ylab = "Frequency",
     main = "Size of Layer Farms",
     xlim = c(0,35000))


par(mfrow = c(1,2))
plot(ecdf(df$farmsize[df$has_broilers == 1 & df$has_layers == 0]),
     xlab = "Number of birds",
     ylab = "Frequency",
     main = "Size of Broiler Farms",
     xlim = c(0,35000))
plot(ecdf(df$farmsize[df$has_broilers == 0 & df$has_layers == 1]),
     xlab = "Number of birds",
     ylab = "Frequency",
     main = "Size of Layer Farms",
     xlim = c(0,35000))

dev.off()


# Box plots ---------------------------------------------------------------

dfbioscore <- melt(df[,c(42:44,77)])

ggplot(dfbioscore, aes(x = variable, y = value)) +
  ggtitle("biosecurity and attitude scores") +
  geom_boxplot() 

dfAMU <- melt(df[,c(68:69,89)])

ggplot(dfAMU, aes(x = variable, y = value)) +
  ggtitle("expenditure on antimicrobials") +
  geom_boxplot()

dfdisease <- melt(df[,78])

ggplot(dfdisease, aes(x = "incidents of disease per flock per cycle", y = value)) +
  ggtitle("disease incidence") +
  geom_boxplot()

dfbroilerprod <- melt(df[,82])

ggplot(dfbroilerprod, aes(x = "broiler finishing weight (KG) times annual cycles", y = value)) +
  ggtitle("broiler productivity") +
  geom_boxplot()

dflayerprod <- melt(df[,80])

ggplot(dflayerprod, aes(x = "eggs per layer per day", y = value)) +
  ggtitle("layer productivity") +
  geom_boxplot()

# Correlation plots -------------------------------------------------------

#separate into broilers and layers
dfbroilers <- df[df$has_broilers == 1 & df$has_layers == 0,]
dflayers <- df[df$has_broilers == 0 & df$has_layers == 1,]

#choose vars
glimpse(df)
#'we want: vacc score, total biosec score, intern biosec score, extern bio score,
#'attended course on livestock management, awareness score, farm size, portion of broilers,
#'presence of other species
#'AND
#'expenditure per pop layers, exp per pop broilers, use of human abx, use on healthy birds,
#'incidents of disease per pop, weight * cycles, eggs per day
corrdf1 <- df[, c("vacc_score", "intern_bio_score", "extern_bio_score", "total_bio_score",
                        "attitude_score", "useprofessional", "farmsize", "portion_broilers", "other_species",
                        "expense_per_broiler", "expense_per_layer", "human_medicine", "usehealthy",
                        "total_disease_incidents", "weight_times_cycles", "eggs_per_day")]

corrdfbroil <- dfbroilers[, c("vacc_score", "intern_bio_score", "extern_bio_score", "total_bio_score",
                        "attitude_score", "useprofessional", "farmsize", "other_species",
                        "expense_per_bird", "human_medicine", "usehealthy",
                        "total_disease_incidents", "weight_times_cycles")]

corrdflay <- dflayers[, c("vacc_score", "intern_bio_score", "extern_bio_score", "total_bio_score",
                        "attitude_score", "farmsize", "useprofessional", "other_species",
                        "expense_per_bird", "human_medicine", "usehealthy",
                        "total_disease_incidents", "eggs_per_day")]

par(mfrow = c(2,2))
pdf("Outputs/corrplots.pdf", replace)
corrplot::corrplot(cor(corrdf1))
corrplot::corrplot(cor(corrdfbroil))
#corrplot::corrplot(cor(corrdflay[!is.na(corrdflay$eggs_per_day),]))
corrplot::corrplot(cor(corrdflay))
dev.off()

# Regression Analysis -----------------------------------------------------

# Parsimonious regressions ------------------------------------------------
parsreg1 <- lm(df$expense_per_bird ~ df$vacc_score)
parsreg2 <- lm(df$expense_per_bird ~ df$total_bio_score)
parsreg3 <- lm(df$expense_per_bird ~ df$attitude_score)

parsreg4 <- glm(df$usehealthy ~ df$vacc_score, family = "binomial")
parsreg5 <- glm(df$usehealthy ~ df$total_bio_score, family = "binomial")
parsreg6 <- glm(df$usehealthy ~ df$attitude_score, family = "binomial")

parsreg7ai <- lm(df$weight_times_cycles ~ df$vacc_score,
                data = df[df$has_broilers == 1 & df$has_layers == 0,])
parsreg7aii <- lm(df$weight_times_cycles ~ df$total_bio_score,
                 data = df[df$has_broilers == 1 & df$has_layers == 0,])
parsreg7aiii <- lm(df$weight_times_cycles ~ df$attitude_score,
                  data = df[df$has_broilers == 1 & df$has_layers == 0,])

parsreg7bi <- lm(df$eggs_per_day ~ df$vacc_score,
                data = df[df$has_broilers == 0 & df$has_layers == 1,])
parsreg7bii <- lm(df$eggs_per_day ~ df$total_bio_score,
                 data = df[df$has_broilers == 0 & df$has_layers == 1,])
parsreg7biii <- lm(df$eggs_per_day ~ df$attitude_score,
                 data = df[df$has_broilers == 0 & df$has_layers == 1,])

parsreg8i <- lm(df$total_disease_incidents ~ df$vacc_score)
parsreg8ii <- lm(df$total_disease_incidents ~ df$total_bio_score)
parsreg8iii <- lm(df$total_disease_incidents ~ df$attitude_score)

stargazer(parsreg1, parsreg2, parsreg3, parsreg4, parsreg5, parsreg6,
          parsreg7ai, parsreg7aii, parsreg7aiii,
          parsreg7bi, parsreg7bii, parsreg7biii,
          parsreg8i, parsreg8ii, parsreg8iii, type = "text", 
          out = "Outputs/parsregs.csv")

stargazer(parsreg1, parsreg2, parsreg3, parsreg4, parsreg5, parsreg6,
          parsreg7ai, parsreg7aii, parsreg7aiii,
          parsreg7bi, parsreg7bii, parsreg7biii,
          parsreg8i, parsreg8ii, parsreg8iii, type = "html", 
          out = "Outputs/parsregshtml.html")

# Individual biosecurity measures -----------------------------------------

# grp_consent.intern_biosecuriy.biosecurit_assesment
# grp_consent.intern_biosecuriy.chichen_differentage
# grp_consent.intern_biosecuriy.crawlspace_duration
# grp_consent.intern_biosecuriy.deadchicken_remove
# rp_consent.intern_biosecuriy.farm_cleaning
# disinfect_score

#choose the variables to use
which( colnames(df)=="grp_consent.extern_biosecuriy.chick_provider" )
which( colnames(df)=="grp_consent.intern_biosecuriy.desinfection_protocol" )
for(i in 2:37){
  df[,i] <- as.numeric(df[,i])
}

#'before correcting for false discovery rate or family-wise error rate, let's 
#'do the regular regressions

for(i in 2:37){
  assign(paste0("indivreg", i), lm(df$expense_per_bird ~ df[,i] + 
                                     df$total_disease_incidents +
                                     df$vacc_score +
                                     df$farmsize +
                                     df$portion_broilers))
} 

stargazer(indivreg2, indivreg3, indivreg4, indivreg5, indivreg6, 
          indivreg7, indivreg8, indivreg9, indivreg10, indivreg11, indivreg12, 
          indivreg13, indivreg14, indivreg15, indivreg16, indivreg17, indivreg18, 
          indivreg19, indivreg20, indivreg21, indivreg22, indivreg23, indivreg24, 
          indivreg25, indivreg26, indivreg27, indivreg28, indivreg29, indivreg30,
          indivreg31, indivreg32, indivreg33, indivreg34, indivreg35, indivreg36,
          indivreg37, type = "text", out = "Outputs/indivmeasures.csv")

stargazer(indivreg2, indivreg3, indivreg4, indivreg5, indivreg6, 
          indivreg7, indivreg8, indivreg9, indivreg10, indivreg11, indivreg12, 
          indivreg13, indivreg14, indivreg15, indivreg16, indivreg17, indivreg18, 
          indivreg19, indivreg20, indivreg21, indivreg22, indivreg23, indivreg24, 
          indivreg25, indivreg26, indivreg27, indivreg28, indivreg29, indivreg30,
          indivreg31, indivreg32, indivreg33, indivreg34, indivreg35, indivreg36,
          indivreg37, type = "html", out = "Outputs/indivmeasures.html")

# dftemp <- df[,c(2,5,15)]
# rm(dftemp)

#'significant: buying chicks from the same supplier (pos), hand washing (neg)
#'farmsize remained negatively related to AMU (although the extent varied a
#'little), and disease incidents remained positively associated with AMU (to 
#'similar degrees) in all cases except for moving broilers around the farm less 
#'often, which was (pos) associated with AMU but not quite significantly, 
#'and where the association with disease incidence disappeared.
#' 
#' This suggests that hand washing and using different suppliers supplier _could_ 
#' be linked to lower AMU, but these were only significant at the 10% level,
#' even without accounting for the false discovery rate. 
#'  
#'  Using the Bonferroni adjustement (quite conservative), we would need our 
#'  results to be significant at the 0.0028 level, which none were close to 
#'   
#'  We look at the false discovery rate using the Benjamini-Hochberg 
#'  'step-up' procedure below for the two significant results:



# Benjamini-Hochberg 'Step-Up' Procedure ----------------------------------

#extract p-values
pvalues <- rep(0,36)

for(i in 2:37){
  obj <- lm(df$expense_per_bird ~ df[,i] + 
                                     df$total_disease_incidents +
                                     df$vacc_score +
                                     df$farmsize +
                                     df$portion_broilers)
  
  pvalues[i-1] <- summary(obj)$coefficients[2,4] 
} 

round(cbind('fdr' = p.adjust(pvalues, method = 'fdr'), 
            'bon' = p.adjust(pvalues, method = 'bonferroni')),4)

#'as we can see, the false discovery rate is never lower than 91%, and the 
#'adjusted p-value is so high that it is approximated to 1. It is very difficult
#'to justify these results

# Heckman selection -------------------------------------------------------
#'these regressions will be like specifications 1-3, but there is a selection
#'function determining the presence of AMU. However, rather than letting the
#'selection function be identical to the second function, we will let the
#'presence of AMU be determined by variables that seem relevant based on the 
#'results so far (primarily farm size, disease incidents, and portion broilers,
#'but also disinfectprotocol, chick provider, attitudes, farm size, total bio score)

reg17 <- selection(df$AMU ~ df$farmsize + df$total_disease_incidents + df$portion_broilers,
                   df$expense_per_bird ~ df$vacc_score + df$farmsize + df$other_species + df$portion_broilers,
                   method = "2step")

reg18 <- selection(df$AMU ~ df$farmsize + df$total_disease_incidents + df$portion_broilers,
                   df$expense_per_bird ~ df$total_bio_score + df$farmsize + df$other_species + df$portion_broilers,
                   method = "2step")

reg19 <- selection(df$AMU ~ df$farmsize + df$total_disease_incidents + df$portion_broilers,
                   df$expense_per_bird ~ df$attitude_score + df$farmsize + df$other_species + df$portion_broilers,
                   method = "2step")

stargazer(reg17, reg18, reg19, type = "text", out = "Outputs/regs171819.csv")
stargazer(reg17, reg18, reg19, type = "html", out = "Outputs/regs171819.html")

#'larger farms still seem to use less, but our variables of interest are very
#'far from being statistically significant

# Interactions ------------------------------------------------------------

#'better biosecurity reducing the need for antibiotics in preventing disease or
#'maximising profitability

reg20a <- lm(weight_times_cycles ~ total_bio_score*expense_per_bird + 
               vacc_score + attitude_score +
               farmsize + other_species, data = df[df$has_broilers == 1 & df$has_layers == 0,])
reg20b <- lm(eggs_per_day ~ total_bio_score*expense_per_bird + 
               vacc_score + attitude_score +
               farmsize + other_species, data = df[df$has_broilers == 0 & df$has_layers == 1,])
reg21 <- lm(total_disease_incidents ~ total_bio_score*expense_per_bird + 
              vacc_score + attitude_score + farmsize + other_species + portion_broilers, data = df)

#'vaccination and biosecurity being substitutes in terms of disease management
reg22a <- lm(weight_times_cycles ~ total_bio_score*vacc_score + attitude_score +
               farmsize + other_species, data = df[df$has_broilers == 1 & df$has_layers == 0,])

reg22b <- lm(eggs_per_day ~ total_bio_score*vacc_score + attitude_score +
               farmsize + other_species, data = df[df$has_broilers == 0 & df$has_layers == 1,])

reg23 <- lm(total_disease_incidents ~ total_bio_score*vacc_score + attitude_score +
             farmsize + other_species + portion_broilers, data = df)

#'better awareness increasing the effectiveness of antibiotics as a disease management tool

reg24a <- lm(weight_times_cycles ~ attitude_score*expense_per_bird +
             farmsize + other_species, data = df[df$has_broilers == 1 & df$has_layers == 0,])

reg24b <- lm(eggs_per_day ~ attitude_score*expense_per_bird +
             farmsize + other_species, data = df[df$has_broilers == 0 & df$has_layers == 1,])

reg25 <- lm(total_disease_incidents ~ attitude_score*expense_per_bird +
            farmsize + other_species + portion_broilers, data = df)

stargazer(reg20a, reg20b, reg21, 
          reg22a, reg22b, reg23,
          reg24a, reg24b, reg25,
          type = "text", out = "Outputs/interactions.csv")

stargazer(reg20a, reg20b, reg21, 
          reg22a, reg22b, reg23,
          reg24a, reg24b, reg25,
          type = "html", out = "Outputs/interactions.html")


# Main results table --------------------------------------------------------

AMU1 <- lm(df$expense_per_bird ~ df$vacc_score + df$farmsize + df$other_species + df$portion_broilers)
AMU2 <- lm(df$expense_per_bird ~ df$total_bio_score + df$farmsize + df$other_species + df$portion_broilers)
AMU3 <- lm(df$expense_per_bird ~ df$attitude_score + df$farmsize + df$other_species + df$portion_broilers)
AMU4 <- lm(df$expense_per_bird ~ df$vacc_score + df$total_bio_score + df$attitude_score + df$farmsize + df$other_species + df$portion_broilers)

usehealthy1 <- glm(df$usehealthy ~ df$vacc_score + df$farmsize + df$other_species + df$portion_broilers, family = "binomial")
usehealthy2 <- glm(df$usehealthy ~ df$total_bio_score + df$farmsize + df$other_species + df$portion_broilers, family = "binomial")
usehealthy3 <- glm(df$usehealthy ~ df$attitude_score + df$farmsize + df$other_species + df$portion_broilers, family = "binomial")
usehealthy4 <- glm(df$usehealthy ~ df$vacc_score + df$total_bio_score + df$attitude_score + df$farmsize + df$other_species + df$portion_broilers, family = "binomial")

broilerprod1 <- lm(df$weight_times_cycles ~ df$vacc_score + df$expense_per_bird + df$farmsize + df$other_species, data = df[df$has_broilers == 1 & df$has_layers == 0,])
broilerprod2 <- lm(df$weight_times_cycles ~ df$total_bio_score + df$expense_per_bird + df$farmsize + df$other_species, data = df[df$has_broilers == 1 & df$has_layers == 0,])
broilerprod3 <- lm(df$weight_times_cycles ~ df$attitude_score + df$expense_per_bird + df$farmsize + df$other_species, data = df[df$has_broilers == 1 & df$has_layers == 0,])
broilerprod4 <- lm(df$weight_times_cycles ~ df$vacc_score + df$total_bio_score + df$attitude_score + df$expense_per_bird + df$farmsize + df$other_species, data = df[df$has_broilers == 1 & df$has_layers == 0,])

layerprod1 <- lm(df$eggs_per_day ~ df$vacc_score + df$expense_per_bird + df$farmsize + df$other_species, data = df[df$has_broilers == 0 & df$has_layers == 1,])
layerprod2 <- lm(df$eggs_per_day ~ df$total_bio_score + df$expense_per_bird + df$farmsize + df$other_species, data = df[df$has_broilers == 0 & df$has_layers == 1,])
layerprod3 <- lm(df$eggs_per_day ~ df$attitude_score + df$expense_per_bird + df$farmsize + df$other_species, data = df[df$has_broilers == 0 & df$has_layers == 1,])
layerprod4 <- lm(df$eggs_per_day ~ df$vacc_score + df$total_bio_score + df$attitude_score + df$expense_per_bird + df$farmsize + df$other_species, data = df[df$has_broilers == 0 & df$has_layers == 1,])

disease1 <- lm(df$total_disease_incidents ~ df$vacc_score + df$expense_per_bird + df$farmsize + df$other_species + df$portion_broilers)
disease2 <- lm(df$total_disease_incidents ~ df$total_bio_score + df$expense_per_bird + df$farmsize + df$other_species + df$portion_broilers)
disease3 <- lm(df$total_disease_incidents ~ df$attitude_score + df$expense_per_bird + df$farmsize + df$other_species + df$portion_broilers)
disease4 <- lm(df$total_disease_incidents ~ df$vacc_score + df$total_bio_score + df$attitude_score + df$expense_per_bird + df$farmsize + df$other_species + df$portion_broilers)

#visualisation
stargazer(AMU1, AMU2, AMU3, AMU4,
          type = "text", out = "Outputs/tableAMU.csv")

stargazer(AMU1, AMU2, AMU3, AMU4,
          type = "html", out = "Outputs/tableAMU.html")


stargazer(usehealthy1, usehealthy2, usehealthy3, usehealthy4,
          type = "text", out = "Outputs/tableusehealthy.csv")

# stargazer(usehealthy1, usehealthy2, usehealthy3, usehealthy4,
#           type = "text", report=("vc*p"))

stargazer(usehealthy1, usehealthy2, usehealthy3, usehealthy4,
          type = "html", out = "Outputs/tableusehealthy.html")


stargazer(broilerprod1, broilerprod2, broilerprod3, broilerprod4,
          type = "text", out = "Outputs/tablebroilerprod.csv")

stargazer(broilerprod1, broilerprod2, broilerprod3, broilerprod4,
          type = "html", out = "Outputs/tablebroilerprod.html")


stargazer(layerprod1, layerprod2, layerprod3, layerprod4,
          type = "text", out = "Outputs/tablelayerprod.csv")

stargazer(layerprod1, layerprod2, layerprod3, layerprod4,
          type = "html", out = "Outputs/tablelayerprod.html")


stargazer(disease1, disease2, disease3, disease4,
          type = "text", out = "Outputs/tabledisease.csv")

stargazer(disease1, disease2, disease3, disease4,
          type = "html", out = "Outputs/tabledisease.html")
