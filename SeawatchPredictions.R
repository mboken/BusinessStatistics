# 8/20/2020
# Seawatch C
library(readxl)
library(dplyr)

SeaWatchC <- read_excel("2020/Summer II/Business Statistics/SeaWatchC.xls", 
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "text"))

# Drop columns that are not useful for modeling: City, Zipcode, Notes
seawatch_sample <- SeaWatchC %>% select(-c(CITY, AssoZIP,Notes))
seawatch_sample$POVPR <- as.numeric(seawatch_sample$POVPR)

# Transforming variables:
# Change percent college graduates to number of college graduates
# Change percent under the poverty line to number under the poverty line
# Change percent in manufacturing jobs to number
# Combine votes for Reagan, Carter, and Anderson into one "voter" column
# AdjGROSS: adjust gross income to account for inflation
seawatch_sample <- seawatch_sample %>% mutate(NumCollegeEducated = POP80*COLLPR/100,
                                              poverty = POVPR*POP80/100,
                                              employed=MFGPR*POP80/100,
                                              voters = REAG+CART+ANDR,
                                              AdjGROSS = (GROSS/CPI)*300)

# Find correlations
cor(seawatch_sample, use="complete.obs")

# First model:
# The highest correlation (ignoring canvas hours) to GROSS is NUMCOLLEGEEDUCATED
model1 <- lm(seawatch_sample$AdjGROSS ~ seawatch_sample$NumCollegeEducated)
summary(model1)

# Generate predictions based on initial model
seawatch_sample$predictions <- predict(model1, newdata=seawatch_sample)
seawatch_sample$resid <- seawatch_sample$AdjGROSS-seawatch_sample$predictions

# Look for the variable most highly correlated with the residual:
cor(seawatch_sample, use="complete.obs")

# The highest correlation with the residuals is VITIT: add variable to model2
model2 <- lm(seawatch_sample$AdjGROSS ~ seawatch_sample$NumCollegeEducated + 
               factor(seawatch_sample$VISIT))
summary(model2)

# calculate predictions and residuals for model2
seawatch_sample$predictions <- predict(model2, newdata=seawatch_sample)
seawatch_sample$resid <- seawatch_sample$AdjGROSS-seawatch_sample$predictions

#check correlations with residuals
cor(seawatch_sample, use="complete.obs")

# Add voters, household median income, and employed (mfg jobs) to the model
model3 <- lm(seawatch_sample$AdjGROSS ~ seawatch_sample$NumCollegeEducated + factor(seawatch_sample$VISIT) + seawatch_sample$voters + 
              + seawatch_sample$HHMEDI + seawatch_sample$employed)
summary(model3)

# account for heteroskedasticity between number of college graduates and Anderson voters by adding an interaction term
# account for heteroskedasticity between Anderson voters and number employed in mfg jobs by adding an interaction term
model4 <- lm(seawatch_sample$AdjGROSS ~ seawatch_sample$NumCollegeEducated*seawatch_sample$ANDR + factor(seawatch_sample$VISIT) + 
               + seawatch_sample$ANDR*seawatch_sample$employed + seawatch_sample$HHMEDI + factor(seawatch_sample$MOY))
summary(model4)

# Import Sea Watch D dataset (making predictions)
SeaWatchD <- read_excel("2020/Summer II/Business Statistics/SeaWatchD.xls", 
                        col_types = c("text", "text", "text", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

# Make sure the SeaWatchD columns match Sea Watch C
SeaD<-SeaWatchD %>% mutate(NumCollegeEducated = POP80*COLLPR/100, 
                           poverty = POVPR*POP80/100, 
                           employed=MFGPR*POP80/100, 
                           voters = REAG+CART+ANDR, 
                           MOY = 1, 
                           VISIT = 1,
)

# Make the model usable for predictions by using "attach"
attach(seawatch_sample)
model5 <- lm(AdjGROSS ~ NumCollegeEducated*ANDR + factor(VISIT) + 
               + ANDR*employed + HHMEDI)
summary(model5)
detach(seawatch_sample)

# Predict adjusted gross for Sea Watch D using our best model:
SeaD$AdjGROSS <- predict(model5,newdata = SeaD)

# Segment the dataframe into two: towns accessible from Greenwich,
#towns accessible from Bridgeport
GRN <- SeaD[SeaD$GRN==1 | SeaD$GRN == 2,]
BPT <- SeaD[SeaD$BPT==1 | SeaD$BPT == 2,]

# Find the satellite office with the highest revenue potential
grn_revenue <- sum(na.omit(GRN$AdjGROSS))
grn_revenue <- sum(na.omit(BPT$AdjGROSS))