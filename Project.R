###########################################################################
# Team: The Walking Dread
# Project: 
# Team Members: Zaid Qadar, Sean Everett, Chuck Huggins, Jeremiah Hamilton
#
###########################################################################


# Set Working Directory
setwd('/Users/jeremiahhamilton/OneDrive/OSU/Programming for DS/Project')

# Read in data frame from the output of the SQL
accidentDF = read.csv('./FARS2017.csv', header = TRUE)

# Rename columns to user friendly values
colnames(accidentDF) = c('State_Case','Vehicle_No', 'Person_No','State','State_Desc','Month','Month_Desc','Accident_Time','Make','Make_Desc','Body_Type','Body_Type_Desc','Model_Year','Impact_Point','Impact_Desc','Manner_Collision','Manner_Collision_Desc','Age','Sex','Sex_Desc','Fatality','Fatality_Desc','Seat_Position','Seat_Position_Desc','Person_Type','Person_Type_Desc','Safety_Use','Safety_Use_Desc','Air_Bag','Air_Bag_Desc','Drinking','Drinking_Desc','Drugs','Drugs_Desc','DOA','DOA_Desc','Hospital','Hospital_Desc','EMS_Arrival_Time','Hospital_Arrival_Time','Speed_Limit','Travel_Speed','Driver_Height','Driver_Weight','Driver_Factor1','Driver_Factor1_Desc','Driver_Factor2','Driver_Factor2_Desc','Driver_Factor3','Driver_Factor3_Desc','Driver_Factor4','Driver_Factor4_Desc')

#############################################################################
#
#  Data Cleaning
#
#############################################################################


# Remove fatality values we're not concered with.
a_reduc = accidentDF[ ! accidentDF$Fatality %in% c(9,5,6), ]

# Next we remove the driver height and driver weight columns because we are studying all passengers.
a_reduc = subset(a_reduc, select=-c(Driver_Height,Driver_Weight))

##############################################################################################
# In this section, we assign descriptions to values that came over as NULL from the SQL source.
##############################################################################################

# Update the Body Type descriptions from NULL.

# Add the new values to the levels first.
levels(a_reduc$Body_Type_Desc) = c(levels(a_reduc$Body_Type_Desc), "Golf Cart","3-Door Coupe","Motor Scooter","Van-Based Bus GVWR>10,000 lbs","Light Pickup","Unenclosed 3-Wheel Motorcycle / Unenclosed Autocycle (1 Rear Wheel)","Recreational Off-Highway Vehicle","Not Reported")

# Assign the levels to the corresponding rows.
a_reduc$Body_Type_Desc[a_reduc$Body_Type==95] <- "Golf Cart"
a_reduc$Body_Type_Desc[a_reduc$Body_Type==17] <- "3-Door Coupe"
a_reduc$Body_Type_Desc[a_reduc$Body_Type==84] <- "Motor Scooter"
a_reduc$Body_Type_Desc[a_reduc$Body_Type==55] <- "Van-Based Bus GVWR>10,000 lbs"
a_reduc$Body_Type_Desc[a_reduc$Body_Type==34] <- "Light Pickup"
a_reduc$Body_Type_Desc[a_reduc$Body_Type==85] <- "Unenclosed 3-Wheel Motorcycle / Unenclosed Autocycle (1 Rear Wheel)"
a_reduc$Body_Type_Desc[a_reduc$Body_Type==96] <- "Recreational Off-Highway Vehicle"
a_reduc$Body_Type_Desc[a_reduc$Body_Type==98] <- "Not Reported"


# Update the Make descriptions from NULL.

# Add the new values to the levels.
levels(a_reduc$Make_Desc) = c(levels(a_reduc$Make_Desc),'SCION', 'Not Reported')

# Assign the levels to the corresponding rows.
a_reduc$Make_Desc[a_reduc$Make==67] <- "SCION"
a_reduc$Make_Desc[a_reduc$Make==97] <- "Not Reported"

# Update the Impact descriptions from NULL.

# Add the new values to the levels.
levels(a_reduc$Impact_Desc) = c(levels(a_reduc$Impact_Desc),'Not Reported',"Right-Back Side","Left","Other Objects Set-In- Motion","Right","Left-Back Side","Left-Front Side","Right-Front Side","Object Set in Motion, Unknown if Cargo/Vehicle Parts or Other")

# Assign the levels to the corresponding rows.
a_reduc$Impact_Desc[a_reduc$Impact_Point==83] <- "Right-Back Side"
a_reduc$Impact_Desc[a_reduc$Impact_Point==61] <- "Left"
a_reduc$Impact_Desc[a_reduc$Impact_Point==19] <- "Other Objects Set-In- Motion"
a_reduc$Impact_Desc[a_reduc$Impact_Point==81] <- "Right"
a_reduc$Impact_Desc[a_reduc$Impact_Point==63] <- "Left-Back Side"
a_reduc$Impact_Desc[a_reduc$Impact_Point==62] <- "Left-Front Side"
a_reduc$Impact_Desc[a_reduc$Impact_Point==82] <- "Right-Front Side"
a_reduc$Impact_Desc[a_reduc$Impact_Point==20] <- "Object Set in Motion, Unknown if Cargo/Vehicle Parts or Other"
a_reduc$Impact_Desc[a_reduc$Impact_Point==98] <- "Not Reported"


# Update the Manner of Collision descriptions from NULL.

# Add the new values to the levels.
levels(a_reduc$Manner_Collision_Desc) = c(levels(a_reduc$Manner_Collision_Desc), 'Not Reported')

# Assign the levels to the corresponding rows.
a_reduc$Manner_Collision_Desc[a_reduc$Manner_Collision==98] <- "Not Reported"

# Update the Sex descriptions from NULL.

# Add the new values to the levels.
levels(a_reduc$Sex_Desc) = c(levels(a_reduc$Sex_Desc), 'Not Reported')

# Assign the levels to the corresponding rows.
a_reduc$Sex_Desc[a_reduc$Sex==8] <- "Not Reported"

# Update the Seat Position descriptions from NULL.

# Add the new values to the levels.
levels(a_reduc$Seat_Position_Desc) = c(levels(a_reduc$Seat_Position_Desc), 'Not Reported')

# Assign the levels to the corresponding rows.
a_reduc$Seat_Position_Desc[a_reduc$Seat_Position==98] <- "Not Reported"

# Update the Safety Use descriptions from NULL.

# Add the new values to the levels.
levels(a_reduc$Safety_Use_Desc) = c(levels(a_reduc$Safety_Use_Desc), 'Not Reported',"No Helmet","Other","Unknown if Helmet Worn","Helmet, Unknown if DOT-Compliant","Helmet, Other than DOT-Compliant Motorcycle Helmet","None Used/Not Applicable")

# Assign the levels to the corresponding rows.
a_reduc$Safety_Use_Desc[a_reduc$Safety_Use==98] <- "Not Reported"
a_reduc$Safety_Use_Desc[a_reduc$Safety_Use==17] <- "No Helmet"
a_reduc$Safety_Use_Desc[a_reduc$Safety_Use==97] <- "Other"
a_reduc$Safety_Use_Desc[a_reduc$Safety_Use==29] <- "Unknown if Helmet Worn"
a_reduc$Safety_Use_Desc[a_reduc$Safety_Use==19] <- "Helmet, Unknown if DOT-Compliant"
a_reduc$Safety_Use_Desc[a_reduc$Safety_Use==16] <- "Helmet, Other than DOT-Compliant Motorcycle Helmet"
a_reduc$Safety_Use_Desc[a_reduc$Safety_Use==20] <- "None Used/Not Applicable"


# Update the Air Bag descriptions from NULL.

# Add the new values to the levels.
levels(a_reduc$Air_Bag_Desc) = c(levels(a_reduc$Air_Bag_Desc), 'Not Reported')

# Assign the levels to the corresponding rows.
a_reduc$Air_Bag_Desc[a_reduc$Air_Bag==98] <- "Not Reported"

# Update the Driver Factors 1-4 descriptions from NULL.

# Add the new values to the levels.
levels(a_reduc$Driver_Factor1_Desc) = c(levels(a_reduc$Driver_Factor1_Desc), "Object Interference with Vehicle Controls","Improper Management of Vehicle Controls","Alcohol and/or Drug Test Refused")


# Assign the levels to the corresponding rows.
a_reduc$Driver_Factor1_Desc[a_reduc$Driver_Factor1==55] <- "Improper Management of Vehicle Controls"
a_reduc$Driver_Factor1_Desc[a_reduc$Driver_Factor1==56] <- "Object Interference with Vehicle Controls"
a_reduc$Driver_Factor1_Desc[a_reduc$Driver_Factor1==60] <- "Alcohol and/or Drug Test Refused"

# Add the new values to the levels.
levels(a_reduc$Driver_Factor2_Desc) = c(levels(a_reduc$Driver_Factor2_Desc), "Object Interference with Vehicle Controls","Improper Management of Vehicle Controls")

# Assign the levels to the corresponding rows.
a_reduc$Driver_Factor2_Desc[a_reduc$Driver_Factor2==55] <- "Improper Management of Vehicle Controls"
a_reduc$Driver_Factor2_Desc[a_reduc$Driver_Factor2==60] <- "Object Interference with Vehicle Controls"

# Add the new values to the levels.
levels(a_reduc$Driver_Factor3_Desc) = c(levels(a_reduc$Driver_Factor3_Desc), "Object Interference with Vehicle Controls")

# Assign the levels to the corresponding rows.
a_reduc$Driver_Factor3_Desc[a_reduc$Driver_Factor3==60] <- "Object Interference with Vehicle Controls"

# Add the new values to the levels.
levels(a_reduc$Driver_Factor4_Desc) = c(levels(a_reduc$Driver_Factor4_Desc), "Improper Management of Vehicle Controls","Object Interference with Vehicle Controls")

# Assign the levels to the corresponding rows.
a_reduc$Driver_Factor4_Desc[a_reduc$Driver_Factor4==55] <- "Improper Management of Vehicle Controls"
a_reduc$Driver_Factor4_Desc[a_reduc$Driver_Factor4==60] <- "Object Interference with Vehicle Controls"



############################################################################################
#
# The next step in the clean up is to remove vehicle types that we are not studying.
# This includes motorcycles, various other motorized bikes, and off-road vehicles.  We are
# removing them to get a cleaner result set with our other columns.
#
############################################################################################

# list the current levels
levels(a_reduc$Body_Type_Desc)

# Choose the levels we want to remove from the data set.
removed_body_types = unique(a_reduc[a_reduc$Body_Type_Desc %in% c(
  'ATV (All-Terrain Vehicle; includes dune/swamp buggy - 3 or 4 wheels)', 
  'Golf Cart',
  'Motor Scooter',
  'Unenclosed 3-Wheel Motorcycle / Unenclosed Autocycle (1 Rear Wheel)',
  'Recreational Off-Highway Vehicle',
  'Snowmobile',
  'Three-wheel Motorcycle or Moped - not All-Terrain Vehicle',
  'Other vehicle type (includes go-cart, fork-lift, city street seeeper)',
  'Off-road Motorcycle (2-wheel)',
  'Motorcycle',
  'Moped (motorized bicycle)',
  'Farm equipment other than trucks',
  'Other motored cycle type (mini-bikes, motor scooters, pocket motorcycles, pocket bikes)',
  'Unknown motored cycle type',
  'Construction equipment other than trucks (includes graders)'), 'Body_Type'])

# Remove rows that have body types that we are not studying.
a_reduc = a_reduc[ ! a_reduc$Body_Type %in% removed_body_types, ]




###################################################################################################
#
#   Data Transformation
#
###################################################################################################




# We set occupant and driver fatalities to 1 and set all others to 0 because we are only concerned with 
# fatalities.
a_reduc$Fatality[a_reduc$Fatality<4] <- 0
a_reduc$Fatality[a_reduc$Fatality==4] <- 1



# Next we will consolidate body types to make our analysis easier.
# Body type levels will be consolidated down to categories of vehicles.
# We'll use Sedan, Truck, SUV, Van, Bus, Large, and Other


levels(a_reduc$Body_Type_Desc) <- list(Sedan=c('4-door sedan, hardtop', 
                                               '3-door/2-door hatchback',
                                               'Station Wagon (excluding van and truck based)',
                                               '2-door sedan,hardtop,coupe',
                                               'Convertible(excludes sun-roof,t-bar)',
                                               '5-door/4-door hatchback',
                                               'Sedan/Hardtop, number of doors unknown',
                                               '3-Door Coupe'
),
Truck=c(
  'Unknown (pickup style) light conventional truck type', 
  'Auto-based pickup (includes E1 Camino, Caballero, Ranchero, Subaru Brat,Rabbit Pickup)',
  'Medium.Heavy Pickup',
  'Unknown medium/heavy truck type',
  'Pickup with slide-in camper',
  'Camper or motorhome, unknown truck type',
  'Medium/heavy truck based motorhome',
  'Unknown truck type (light/medium/heavy)',
  'Unknown light truck type (not a pickup)',
  'Light Truck'
),
SUV=c('Compact utility (Jeep CJ-2-CJ-7, Scrambler, Golden Eagle, Renegade, Laredo, Wrangler, .....)', 
      'Large utility (includes Jeep Cherokee [83 and before], Ramcharger, Trailduster, Bronco-fullsize ..)',
      'Utility station wagon (includes suburban limousines, Suburban, Travellall, Grand Wagoneer)',
      'Utility, Unknown body type'),
Van=c('Minivan (Chrysler Town and Country, Caravan, Grand Caravan, Voyager, Grand Voyager, Mini-Ram, ...)', 
      'Large Van (B150-B350, Sportsman, Royal Maxiwagon, Ram, Tradesman, Voyager [83 and before], .....)',
      'Step van',
      'Other van type (Hi-Cube Van, Kary)',
      'Step van or walk-in van',
      'Unknown van type'),
Bus=c('Transit Bus (City Bus)', 
      'School Bus',
      'Cross Country/Intercity Bus (i.e., Greyhound)',
      'Van-Based Bus GVWR>10,000 lbs',
      'Other Bus Type',
      'Unknown Bus Type'),
Large=c('Cab chassis based (includes light stake, light dump, light tow, rescue vehicles)',
        'Single unit straight truck (GVWR > 26,000 lbs.)', 
        'Truck-tractor (Cab only, or with any number of trailing unit; any weight)',
        'Single unit straight truck (10,000 lbs < GVWR < or= 19,500 lbs)',
        'Single unit straight truck (19,500 lbs < GVWR < or= 26,000 lbs.)',
        'Single unit straight truck (GVWR unknown)',
        'Large Limousine-more than four side doors or stretched chassis',
        'Unknown if single unit or combination unit Heavy Truck (GVWR > 26,000)',
        'Unknown if single unit or combination unit Medium Truck (10,000 < GVWR < 26,000)'),
Other=c('Unknown body type',
        'Other or Unknown automobile type', 
        'Unknown light vehicle type (automobile, van, or light truck)',
        'NULL')

)

# Replace Body type values with leveled numbers
a_reduc$Body_Type[a_reduc$Body_Type_Desc == 'Sedan'] <- 1
a_reduc$Body_Type[a_reduc$Body_Type_Desc == 'Truck'] <- 2
a_reduc$Body_Type[a_reduc$Body_Type_Desc == 'SUV'] <- 3
a_reduc$Body_Type[a_reduc$Body_Type_Desc == 'Van'] <- 4
a_reduc$Body_Type[a_reduc$Body_Type_Desc == 'Bus'] <- 5
a_reduc$Body_Type[a_reduc$Body_Type_Desc == 'Large'] <- 6
a_reduc$Body_Type[a_reduc$Body_Type_Desc == 'Other'] <- 7


###############################################################################################
#
#   Data Reduction
#
###############################################################################################


#====================================
# Subsampling from a dataframe
# Select 50% of the data at random
# We are doing this because we should split our data
# Using using PCA followed by FA
#====================================

#Determine how many rows is 50%
split.num = round(nrow(a_reduc)*.50,0)

#Found out how many rows are in the dataframe
nrow(a_reduc)

#Range of data to sample
x = 1:43803	

#Perform splitting
a_reduc.pca.split = a_reduc[sample(x,split.num,replace=F),]
a_reduc.fa.split = a_reduc[sample(x,split.num,replace=F),]

########################################################
#
# Principle Component Analysis
#
########################################################


# perform PCA on accident data. We must use numeric data only
accident_PCA_init = subset(a_reduc.pca.split, select=c('State','Month','Make','Body_Type','Model_Year','Impact_Point','Manner_Collision','Age','Sex','Seat_Position','Person_Type','Safety_Use','Air_Bag','Drinking','Drugs','Hospital','Speed_Limit','Travel_Speed','Driver_Factor1','Driver_Factor2','Driver_Factor3','Driver_Factor4'))

# Run the PCA
accident_pca = princomp(accident_PCA_init,cor=TRUE)

# Access the eigenvalues
accident_pca$sdev^2

# From the results, it shows we have 7 values that are above 1. This suggests we take out 12
# variblese from our analysis.


# Plot the results of the PCA.
plot(accident_pca, main="Screeplot of PCA")

# The plot lines up with our earlier assessment.  The plot smooths out at the 8th variable.
# So, this suggests 7 variables, which is what we saw in the PCA numbers.

#######################################################
#
# Factor Analysis
#
########################################################


#Conduct a factor analysis We do this on numeric data.
accident_FA = factanal(~State+Month+Make+Body_Type+Model_Year+Impact_Point+Manner_Collision+Age+Sex+Seat_Position+Person_Type+Safety_Use+Air_Bag+Drinking+Drugs+Hospital+Speed_Limit+Travel_Speed+Driver_Factor1+Driver_Factor2+Driver_Factor3+Driver_Factor4, 
                       factors = 7,
                       rotation="promax",
                       scores="none",
                       lower = 0.08,
                       data=a_reduc.fa.split)
accident_FA

# In order for the factor analysis to run properly without error,
# I changed the rotation to promax and added a lower value of .08.
# The results were in line with the PCA findings. 
# 

# As as result, it looks like we have the following factors:

# Factor 1:
# A combination of Driver factors 2 through 4.

# Factor 2: Drinking and Drugs

# Factor 3: Age and Sex

# Factor 4: Seat Position

# Factor 5: Make

# Factor 6: Safety Use and Air Bag

# Factor 7: State

# The factor correlations show fairly high correlations between:
# State and Make

# In order to remove correlation among the variables, I would recommend removing
# State from the analysis.

colnames(a_reduc_tree)
colnames(a_reduc)



library(rpart)


library(plyr)

##install.packages("caTools")


#install.packages("DMwR")
library(DMwR)

  
  
  count(a_reduc_model, "Fatality")


a_reduc_model <-subset(a_reduc,select=c(Fatality,Make,Month, Impact_Point, Safety_Use, Sex,Manner_Collision, Speed_Limit, Air_Bag, Drugs, Drinking, Person_Type,Seat_Position, Body_Type, Model_Year, Driver_Factor1,Driver_Factor2, Driver_Factor3, Driver_Factor4,Age))
a_reduc_model$Fatality = as.factor(a_reduc_model$Fatality)

require(caTools)
set.seed(101) 

sample = sample.split(a_reduc_model$Fatality, SplitRatio = .60)

train = subset(a_reduc_model, sample == TRUE)
test  = subset(a_reduc_model, sample == FALSE)

count(test, "Fatality")
count(train, "Fatality")

library(DMwR)
set.seed(9560)
smote_train <- SMOTE(Fatality~ ., data  =train,perc.over=100, perc.under=200)  

count(smote_train, "Fatality")

library (psych)
describe(a_reduc_model)
describe (test)
describe (smote_train)




##install.packages("caret")
library(caret)

library(tree)
dt_tree_train = tree(Fatality~., data= smote_train, method="class")
                    
                    summary(dt_tree_train)
                    
                    plot(dt_tree_train)
                    text(dt_tree_train)
                    
                    library(ROCR)
                    dt_pred<-predict(dt_tree_train,test,type="class")
                    
                    
                    
                    
                    cm <- confusionMatrix(dt_pred, test$Fatality)
                    cm$table
                    fourfoldplot(cm$table)
                    accuracy <- sum(diag(cm$table))/sum(cm$table)
                    
                    library(ROCR)
                    pred1 = prediction(as.numeric(dt_pred), as.numeric(test$Fatality))
                    perf1 <- performance(pred1,"tpr","fpr")
                    plot(perf1, main = "DT_ROC")
                    
                    auc<-performance(pred1,"auc")
                    auc=auc@y.values[[1]]
                    

#### Perform logistic regression using the numberic values as a predictor of Fatalities


# We can't do this one because of Error: cannot allocate vector of size 10.7 Gb
#accident_reg1 = glm(Fatality ~ Make*Safety_Use*Sex*Speed_Limit*Air_Bag*Drugs*Drinking*Person_Type*Body_Type*Model_Year*Driver_Factor1*Driver_Factor2*Driver_Factor3*Driver_Factor4*State,
#                    binomial,
#                    data = a_reduc)
#summary(accident_reg1)
#anova(accident_reg1, test = "Chisq")


accident_reg_full = glm(Fatality ~ Make+Safety_Use+Sex+Speed_Limit+Air_Bag+Drugs+Drinking+Person_Type+Body_Type+Model_Year+Driver_Factor1+Driver_Factor2+Driver_Factor3+Driver_Factor4,
                    binomial,
                    data = a_reduc)
summary(accident_reg_full)
anova(accident_reg_full, test = "Chisq")

library(MASS)
accident_reg_step <- stepAIC(accident_reg_full, trace = FALSE)
accident_reg_step$anova

round(exp(cbind(OR = coef(accident_reg_step), confint(accident_reg_step))),4)


# Statistical Tests for Individual Predictors
library(survey)
regTermTest(accident_reg_full, "Make")
regTermTest(accident_reg_full, "Safety_Use")
regTermTest(accident_reg_full, "Sex")
regTermTest(accident_reg_full, "Speed_Limit")
regTermTest(accident_reg_full, "Air_Bag")
regTermTest(accident_reg_full, "Drugs")
regTermTest(accident_reg_full, "Drinking")
regTermTest(accident_reg_full, "Person_Type")
regTermTest(accident_reg_full, "Body_Type")
regTermTest(accident_reg_full, "Driver_Factor1")
regTermTest(accident_reg_full, "Driver_Factor2")
regTermTest(accident_reg_full, "Driver_Factor3")
regTermTest(accident_reg_full, "Driver_Factor4")

# another test
varImp(accident_reg_step)

# Goodness of fit:
accident_reg_two = glm(Fatality ~ Safety_Use+Speed_Limit+Drinking+Person_Type+Driver_Factor1,
                        binomial,
                        data = a_reduc)
summary(accident_reg_two)
anova(accident_reg_two, test = "Chisq")

anova(accident_reg_full, accident_reg_two, test ="Chisq")

library(lmtest)
lrtest(accident_reg_full, accident_reg_two)

# Use your model to make predictions, in this example newdata = test
pdata1 <- predict(accident_reg_full, newdata = test, type = "response")

library(e1071)

# use caret and compute a confusion matrix
cm_reg1 <- confusionMatrix(data = as.numeric(pdata1>0.5), reference = test$Fatality)

cm_reg1$table

accuracy_reg1 <- sum(diag(cm_reg1$table))/sum(cm_reg1$table)
accuracy_reg1


# use caret and compute a confusion matrix
pdata <- predict(accident_reg_two, newdata = test, type = "response")
cm_reg <- confusionMatrix(data = as.numeric(pdata>0.5), reference = test$Fatality)

cm_reg$table

accuracy_reg <- sum(diag(cm_reg$table))/sum(cm_reg$table)
accuracy_reg


library(ROCR)
pred1_reg = prediction(as.numeric(pdata), as.numeric(test$Fatality))
perf1_reg <- performance(pred1_reg,"tpr","fpr")
plot(perf1_reg, main = "REG_ROC")

auc_reg<-performance(pred1_reg,"auc")
auc_reg=auc_reg@y.values[[1]]

auc_reg
