#Data comes the `NSDUH_2020.RData` file
#Located here: https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2020-nsduh-2020-ds0001

load("NSDUH_2020.RData")


#Variable definitions here: https://www.datafiles.samhsa.gov/sites/default/files/field-uploads-protected/studies/NSDUH-2020/NSDUH-2020-datasets/NSDUH-2020-DS0001/NSDUH-2020-DS0001-info/NSDUH-2020-DS0001-info-codebook.pdf

#Analysis variables (distress): spdyr = past-year serious psychological distress; spdmon = past-month serious distress;
#                   irhallucrec = Hallucinogen recency
#Control variables (socioeconomic):  "NEWRACE2", "AGE2", "irsex", "irmarit", "income", "eduhighcat", "COUTYP4" 
#Control variables (other drug use): "irinhalrec", "irmethamrec", "irtrqnmrec", 
#                                   "irstmnmrec", "irsednmrec", "irmjrc", "ircocrc", "ircrkrc", "irherrc"


#Create a data frame pulling out analysis variables and control variables (socioeconomic and other drugs)
drdf <- NSDUH_2020[c("irhallucrec", "spdyr", "spdmon", "AD_MDEA9", "adwrsatp", "NEWRACE2", "AGE2", "irsex", 
                     "irmarit", "income", "eduhighcat", "COUTYP4", "irinhalrec", "irmethamrec", "irtrqnmrec", 
                     "irstmnmrec", "irsednmrec", "irmjrc", "ircocrc", "ircrkrc", "irherrc")]

#Recode sex to a binary
drdf$sex <- ifelse(drdf$irsex == 1, "male", "female")

#Recode age into quartiles (0 to 18, 19 to 34, 35 to 49, 50 or older)
drdf$age <- ifelse(drdf$AGE2 <= 7, "0 to 18",
                   ifelse(drdf$AGE2 >= 8 & drdf$AGE2 <= 14, "19 to 34",
                          ifelse(drdf$AGE2 == 15, "35 to 49",
                                 ifelse(drdf$AGE2 >= 16, "50 or older", "N/A"))))

#Recode martial status as a binary
drdf$married <- ifelse(drdf$irmarit == 1, "married", "not married")


#Recode irhallucrec into "yes in past year", "not in past year"
drdf$psylastyear <- ifelse(drdf$irhallucrec == 1 | 
                             drdf$irhallucrec == 2,
                           1, 0)

#Remove rows where spdyr, spdmon are NA
drdf <- drdf[complete.cases(drdf), ]


#Recode irinhalrec into "yes in past year", "not in past year"
drdf$inhallastyear <- ifelse(drdf$irinhalrec == 1 | 
                               drdf$irinhalrec == 2,
                             1, 0)

#Recode irmethamrec into "yes in past year", "not in past year"
drdf$methlastyear <- ifelse(drdf$irmethamrec == 1 | 
                              drdf$irmethamrec == 2,
                            1, 0)

#Recode irtrqnmrec into "yes in past year", "not in past year"
drdf$trqlastyear <- ifelse(drdf$irtrqnmrec == 1 | 
                             drdf$irtrqnmrec == 2,
                           1, 0)

#Recode irstmnmrec into "yes in past year", "not in past year"
drdf$stmnlastyear <- ifelse(drdf$irstmnmrec == 1 | 
                              drdf$irstmnmrec == 2,
                            1, 0)

#Recode irsednmrec into "yes in past year", "not in past year"
drdf$sedlastyear <- ifelse(drdf$irsednmrec == 1 | 
                             drdf$irsednmrec == 2,
                           1, 0)

#Recode irmjrc into "yes in past year", "not in past year"
drdf$mjlastyear <- ifelse(drdf$irmjrc == 1 | 
                            drdf$irmjrc == 2,
                          1, 0)

#Recode ircocrc into "yes in past year", "not in past year"
drdf$coclastyear <- ifelse(drdf$ircocrc == 1 | 
                             drdf$ircocrc == 2,
                           1, 0)

#Recode ircrkrc into "yes in past year", "not in past year"
drdf$craklastyear <- ifelse(drdf$ircrkrc == 1 | 
                              drdf$ircrkrc == 2,
                            1, 0)

#Recode irherrc into "yes in past year", "not in past year"
drdf$herlastyear <- ifelse(drdf$irherrc == 1 | 
                             drdf$irherrc == 2,
                           1, 0)

#Logistic regression model with spdyr as dependent variable, psylastyear as independent variable of interest
#Adjusts for race, sex, age, married, income, urbanicity, education level, and past-year use of other drugs 
psymodel_spdyr <- glm(spdyr ~ psylastyear + 
        as.factor(NEWRACE2) + as.factor(sex) + as.factor(age) + as.factor(married) + 
        as.factor(income) + as.factor(eduhighcat) + as.factor(COUTYP4) + 
        inhallastyear + methlastyear + trqlastyear +
        stmnlastyear + sedlastyear + mjlastyear +
        coclastyear + craklastyear + herlastyear, 
      data = drdf, family = "binomial")

summary(psymodel_spdyr)
exp(psymodel_spdyr$coefficients)
exp(confint(psymodel_spdyr))

#Comparing to a simple model to make sure the control variables are sensible
psysimple <- glm(spdyr ~ psylastyear, data = drdf, family = "binomial")
summary(psysimple)


#Checking the past-month distress association too (SPDMON), which is what Jones looked at...
psymodel_spdmon <- glm(spdmon ~ psylastyear + 
                       as.factor(NEWRACE2) + as.factor(sex) + as.factor(age) + as.factor(married) + 
                       as.factor(income) + as.factor(eduhighcat) + as.factor(COUTYP4) + 
                       inhallastyear + methlastyear + trqlastyear +
                       stmnlastyear + sedlastyear + mjlastyear +
                       coclastyear + craklastyear + herlastyear, 
                     data = drdf, family = "binomial")

summary(psymodel_spdmon)
exp(psymodel_spdmon$coefficients)
exp(confint(psymodel_spdmon))




#Create a dataframe with just the suicide ideation cases 
drdfsui <- drdf[(drdf$AD_MDEA9 == 1 | drdf$AD_MDEA9 == 2), ]

#Recode AD_MDEA9 into "yes, recent suicidal thinking" or "no recent suicidal thinking" 
drdfsui$sui_thoughts <- ifelse(drdfsui$AD_MDEA9 == 1,
                           1, 0)

#Run this model 
psymodel_sui <- glm(sui_thoughts ~ psylastyear + 
                         as.factor(NEWRACE2) + as.factor(sex) + as.factor(age) + as.factor(married) + 
                         as.factor(income) + as.factor(eduhighcat) + as.factor(COUTYP4) + 
                         inhallastyear + methlastyear + trqlastyear +
                         stmnlastyear + sedlastyear + mjlastyear +
                         coclastyear + craklastyear + herlastyear, 
                       data = drdfsui, family = "binomial")

#Report out 
summary(psymodel_sui)
exp(psymodel_sui$coefficients)
exp(confint(psymodel_sui))


#Create a dataframe with just the suicide attempt cases
drdfsuiattempt <- drdf[(drdf$adwrsatp == 1 | drdf$adwrsatp == 2), ]

#Recode adwrsatp into "yes, recent suicide attempt" or "no recent suicide attempt" 
drdfsuiattempt$sui_attempt <- ifelse(drdfsuiattempt$adwrsatp == 1,
                               1, 0)

#Run this model 
psymodel_suiattempt <- glm(sui_attempt ~ psylastyear + 
                      as.factor(NEWRACE2) + as.factor(sex) + as.factor(age) + as.factor(married) + 
                      as.factor(income) + as.factor(eduhighcat) + as.factor(COUTYP4) + 
                      inhallastyear + methlastyear + trqlastyear +
                      stmnlastyear + sedlastyear + mjlastyear +
                      coclastyear + craklastyear + herlastyear, 
                    data = drdfsuiattempt, family = "binomial")

#Report out 
summary(psymodel_suiattempt)
exp(psymodel_suiattempt$coefficients)
exp(confint(psymodel_suiattempt))
