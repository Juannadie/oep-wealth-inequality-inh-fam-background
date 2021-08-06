

dataukh <- readRDS(file = "data_rds/datauk-new-final-no-cpt-new-data7b.rds") ##


###### ADJUSTMENT ADDING SOME VARIABLES


##### Do we also know if the house was inherited??


### The variable for that is Hhown...


table (dataukh$HHownW1) #Most of the households were asked in Wave 1 and not re-asked later
table (dataukh$HHownW2)
table (dataukh$HHownW3)

#Let's create a dummy for having inheritance or gift and see if all people with given house have an inheritance

dataukh$inhdummy <- 0
dataukh$inhdummy[dataukh$inh>0] <- 1

table(dataukh$inhdummy)

table(dataukh$inhdummy, dataukh$HHownW1) #There is some house inherited and not claimed...
table(dataukh$inhdummy, dataukh$HHownW2) #There is some house inherited and not claimed...
table(dataukh$inhdummy, dataukh$HHownW3) #There is some house inherited and not claimed...


#We also have the year the house was bought/inherited

table (dataukh$HBuyYrW1)
table (dataukh$HBuyYrW2)
table (dataukh$HBuyYrW3)

#And we have the value of the house today (so no need to update)

summary (dataukh$HValueW1)
summary (dataukh$HValueW2)
summary (dataukh$HValueW3)

#And the variable including imputed values

summary (dataukh$HValueW1_i)
summary (dataukh$HValueW2_i)
summary (dataukh$HValuew3_i)

table (dataukh$HValueW1_iflag) #Some have been imputed, we use that


#We can also have the value of the house in brackets if they didn't provide the original amount:

summary(dataukh$HValBW1)
summary(dataukh$HValBW2)
summary(dataukh$HValBW3)

#And let's see how many of the inherited houses have a mortgage tabling those two variables

table(dataukh$MNumbW1,dataukh$HHownW1)
table(dataukh$MNumbW2,dataukh$HHownW2)
table(dataukh$MNumbW3,dataukh$HHownW3)

#We could see the value of that mortgage but it only affects 3 inherited houses that have a mortage (in wave 1), 0 in the rest

summary(dataukh$MValW1)
summary(dataukh$MValW1_i)

summary(dataukh$MValW1_i[dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0])
table(dataukh$MValW1_i[dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0]) #Only one person has not an inheritance reported, has received a house inherited, and has a mortgage.

## Let's do it with the brackets

summary(dataukh$MValBW1[dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0])
table(dataukh$MValBW1[dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0]) #No one has brackets, so we do not have to use the brackets for the mortgage later

table(dataukh$HValueW1_i[dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0])
table(dataukh$HValBW1[dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0])

#We convert the brackets of the main residence value to numbers

dataukh$brackethomeresidvalueW1 <- 0

dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == 'Less than \24360,000'] <- 30000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\24360,000 to \24399,999'] <- 75000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\243100,000 to \243149,999'] <- 125000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\243150,000 to \243199,999'] <- 175000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\243200,000 to \243249,999'] <- 225000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\243250,000 to \243299,999'] <- 275000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '243300,000 to \243349,999'] <- 325000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\243350,000 to \243399,999'] <- 375000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\243500,000 to \243749,999'] <- 625000
dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '243750,000 to \243999,999'] <- 875000

a <- mean (dataukh$HValueW1_i[dataukh$HValueW1_i>1000000]) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackethomeresidvalueW1 [dataukh$HValBW1 == '\2431 million or more'] <- a


#We convert the brackets of the main residence value to numbers Wave 2

dataukh$brackethomeresidvalueW2 <- 0

dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == 'Less than \24360,000'] <- 30000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\24360,000 to \24399,999'] <- 75000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\243100,000 to \243149,999'] <- 125000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\243150,000 to \243199,999'] <- 175000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\243200,000 to \243249,999'] <- 225000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\243250,000 to \243299,999'] <- 275000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '243300,000 to \243349,999'] <- 325000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\243350,000 to \243399,999'] <- 375000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\243500,000 to \243749,999'] <- 625000
dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '243750,000 to \243999,999'] <- 875000

a <- mean (dataukh$HValueW2_i[dataukh$HValueW2_i>1000000]) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackethomeresidvalueW2 [dataukh$HValBW2 == '\2431 million or more'] <- a

#We convert the brackets of the main residence value to numbers Wave 3

dataukh$brackethomeresidvalueW3 <- 0

dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == 'Less than \24360,000'] <- 30000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\24360,000 to \24399,999'] <- 75000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\243100,000 to \243149,999'] <- 125000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\243150,000 to \243199,999'] <- 175000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\243200,000 to \243249,999'] <- 225000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\243250,000 to \243299,999'] <- 275000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '243300,000 to \243349,999'] <- 325000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\243350,000 to \243399,999'] <- 375000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\243500,000 to \243749,999'] <- 625000
dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '243750,000 to \243999,999'] <- 875000

a <- mean (dataukh$HValuew3_i[dataukh$HValuew3_i>1000000]) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackethomeresidvalueW3 [dataukh$HValBW3 == '\2431 million or more'] <- a


#Now we can add the brackedted value and the normal value

dataukh$mainresidvalueW1 <- 0

dataukh$mainresidvalueW1[dataukh$HValueW1_i>0] <- dataukh$HValueW1_i[dataukh$HValueW1_i>0]
dataukh$mainresidvalueW1[dataukh$HValueW1_i<=0 & dataukh$brackethomeresidvalueW1>0] <- dataukh$brackethomeresidvalueW1[dataukh$HValueW1_i<=0 & dataukh$brackethomeresidvalueW1>0]

summary(dataukh$mainresidvalueW1)

###

dataukh$mainresidvalueW2 <- 0

dataukh$mainresidvalueW2[dataukh$HValueW2_i>0] <- dataukh$HValueW2_i[dataukh$HValueW2_i>0]
dataukh$mainresidvalueW2[dataukh$HValueW2_i<=0 & dataukh$brackethomeresidvalueW2>0] <- dataukh$brackethomeresidvalueW2[dataukh$HValueW2_i<=0 & dataukh$brackethomeresidvalueW2>0]

summary(dataukh$mainresidvalueW2)

###

dataukh$mainresidvalueW3 <- 0

dataukh$mainresidvalueW3[dataukh$HValuew3_i>0] <- dataukh$HValuew3_i[dataukh$HValuew3_i>0]
dataukh$mainresidvalueW3[dataukh$HValuew3_i<=0 & dataukh$brackethomeresidvalueW3>0] <- dataukh$brackethomeresidvalueW3[dataukh$HValuew3_i<=0 & dataukh$brackethomeresidvalueW3>0]

summary(dataukh$mainresidvalueW3)

### Now we have to calculate the mortgage on the residency, we do not need the brackets because for those we want to trace (people with inherited house but not reporting inheritance) no one reported bracketed mortgage
### We do it only for the first mortgage since is the only one that people with inherited houses may have

summary(dataukh$MValW1_i)

dataukh$nethousevalue1 <- dataukh$mainresidvalueW1 - dataukh$MValW1_i
dataukh$nethousevalue2 <- dataukh$mainresidvalueW2 - dataukh$MValW2_i
dataukh$nethousevalue3 <- dataukh$mainresidvalueW3 - dataukh$MValw3_i

#We update Waves 1 and 2 to Wave 3:

dataukh$nethousevalue1 <- dataukh$nethousevalue1 * 1.14 #2008-2012 multiplier 1.14
dataukh$nethousevalue2 <- dataukh$nethousevalue2 * 1.1  #2010-2012 multiplier 1.1


datauk$inherithome

## Finally we can add this to the total inheritances of the households fulfilling all the properties:

dataukh$inhh <- dataukh$inh

dataukh$inhh [dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0] <- dataukh$inh [dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0] + dataukh$nethousevalue1  [dataukh$HHownW1 == 'inherited-given' & dataukh$inhdummy == 0]
dataukh$inhh [dataukh$HHownW2 == 'inherited-given' & dataukh$inhdummy == 0] <- dataukh$inhh [dataukh$HHownW2 == 'inherited-given' & dataukh$inhdummy == 0] + dataukh$nethousevalue2  [dataukh$HHownW2 == 'inherited-given' & dataukh$inhdummy == 0]
dataukh$inhh [dataukh$HHownW3 == 'inherited-given' & dataukh$inhdummy == 0] <- dataukh$inhh [dataukh$HHownW3 == 'inherited-given' & dataukh$inhdummy == 0] + dataukh$nethousevalue3  [dataukh$HHownW3 == 'inherited-given' & dataukh$inhdummy == 0]

summary(dataukh$inh)
summary(dataukh$inhh)

summary(dataukh$inh[dataukh$inh>0])
summary(dataukh$inhh[dataukh$inhh>0])

NROW(dataukh$inh[dataukh$inh>0])
NROW(dataukh$inhh[dataukh$inhh>0]) #We have added a few observations and the amount has gone marginally up


###### NOW WE SEE IF THERE IS SOME INHERITED BUSINESS FOR THOSE WHO HAVE NO INHERITANCE AT ALL #######

table (dataukh$BstartW1)
table (dataukh$Bstart2W1)

#...

table(dataukh$inhdummy, dataukh$BstartW1) #There is some business inherited and not reported
table(dataukh$inhdummy, dataukh$Bstart2W1) #There is two second business inherited and not reported
table(dataukh$inhdummy, dataukh$Bstart3W1) #No third business inherited and not given

table(dataukh$inhdummy, dataukh$BstartW2) #There is a few business inherited and not reported
table(dataukh$inhdummy, dataukh$Bstart2W2) #There is no secon business inherited
table(dataukh$inhdummy, dataukh$Bstart3W2) #No third business inherited and not reported


table(dataukh$inhdummy, dataukh$Bstart1W3) #There is one business inherited/given and not reported
table(dataukh$inhdummy, dataukh$Bstart2W3) #There is one second business inherited and not reported
table(dataukh$inhdummy, dataukh$Bstart3W3) #No third business inherited and not given

##### NOW WE GET THE VALUE OF THE INHERITED BUSINESS

summary(dataukh$BvalW1)
summary(dataukh$BvalW1_i)
table(dataukh$BvalBW1)

##### Let's first convert the brackets into actual numbers ####

#We convert the brackets of the main residence value to numbers

dataukh$brackedbusinessvalueW1 <- 0

dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == 'Less than \243100'] <- 50
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\243100 to \2439,999'] <- 4950
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\24310,000 to \24324,999'] <- 17500
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\24325,000 to \24349,999'] <- 37500
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\24350,000 to \24399,999'] <- 75000
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\243100,000 to \243199,999'] <- 150000
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\243200,000 to \243299,999'] <- 250000
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\243300,000 to \243399,999'] <- 350000
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\243500,000 to \243999,999'] <- 750000

a <- mean ((dataukh$BvalW1_i[dataukh$BvalW1_i>1000000]), na.rm = T) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackedbusinessvalueW1 [dataukh$BvalBW1 == '\2431 million or more'] <- a

summary(dataukh$brackedbusinessvalueW1)


#We convert the brackets of the main residence value to numbers

dataukh$brackedbusinessvalue2W1 <- 0

dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == 'Less than \243100'] <- 50
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\243100 to \2439,999'] <- 4950
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\24310,000 to \24324,999'] <- 17500
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\24325,000 to \24349,999'] <- 37500
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\24350,000 to \24399,999'] <- 75000
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\243100,000 to \243199,999'] <- 150000
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\243200,000 to \243299,999'] <- 250000
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\243300,000 to \243399,999'] <- 350000
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\243500,000 to \243999,999'] <- 750000

a <- mean ((dataukh$Bval2W1_i[dataukh$Bval2W1_i>1000000]), na.rm = T) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackedbusinessvalue2W1 [dataukh$BvalB2W1 == '\2431 million or more'] <- a

summary(dataukh$brackedbusinessvalue2W1)

## There is no value for BValB3W1

#We convert the brackets of the main residence value to numbers

dataukh$brackedbusinessvalueW2<- 0

dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == 'Less than \243100'] <- 50
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\243100 to \2439,999'] <- 4950
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\24310,000 to \24324,999'] <- 17500
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\24325,000 to \24349,999'] <- 37500
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\24350,000 to \24399,999'] <- 75000
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\243100,000 to \243199,999'] <- 150000
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\243200,000 to \243299,999'] <- 250000
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\243300,000 to \243399,999'] <- 350000
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\243500,000 to \243999,999'] <- 750000

a <- mean ((dataukh$BvalW2[dataukh$BvalW2>1000000]), na.rm = T) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackedbusinessvalueW2 [dataukh$BvalBW2 == '\2431 million or more'] <- a

summary(dataukh$brackedbusinessvalueW2)

#We convert the brackets of the main residence value to numbers

dataukh$brackedbusinessvalue2W2<- 0

dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == 'Less than \243100'] <- 50
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\243100 to \2439,999'] <- 4950
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\24310,000 to \24324,999'] <- 17500
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\24325,000 to \24349,999'] <- 37500
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\24350,000 to \24399,999'] <- 75000
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\243100,000 to \243199,999'] <- 150000
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\243200,000 to \243299,999'] <- 250000
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\243300,000 to \243399,999'] <- 350000
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\243500,000 to \243999,999'] <- 750000

a <- mean ((dataukh$Bval2W2[dataukh$Bval2W2>1000000]), na.rm = T) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackedbusinessvalue2W2 [dataukh$BvalB2W2 == '\2431 million or more'] <- a

summary(dataukh$brackedbusinessvalue2W2)


#We convert the brackets of the main residence value to numbers

dataukh$brackedbusinessvalue3W2<- 0

dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == 'Less than \243100'] <- 50
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\243100 to \2439,999'] <- 4950
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\24310,000 to \24324,999'] <- 17500
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\24325,000 to \24349,999'] <- 37500
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\24350,000 to \24399,999'] <- 75000
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\243100,000 to \243199,999'] <- 150000
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\243200,000 to \243299,999'] <- 250000
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\243300,000 to \243399,999'] <- 350000
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\243500,000 to \243999,999'] <- 750000

a <- mean ((dataukh$Bval3W2[dataukh$Bval3W2>1000000]), na.rm = T) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackedbusinessvalue3W2 [dataukh$BvalB3W2 == '\2431 million or more'] <- a

summary(dataukh$brackedbusinessvalue3W2)


### Now third wave

#We convert the brackets of the main residence value to numbers

dataukh$brackedbusinessvalueW3<- 0

dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == 'Less than \243100'] <- 50
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\243100 to \2439,999'] <- 4950
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\24310,000 to \24324,999'] <- 17500
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\24325,000 to \24349,999'] <- 37500
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\24350,000 to \24399,999'] <- 75000
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\243100,000 to \243199,999'] <- 150000
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\243200,000 to \243299,999'] <- 250000
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\243300,000 to \243399,999'] <- 350000
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\243500,000 to \243999,999'] <- 750000

a <- mean ((dataukh$Bval1W3[dataukh$Bval2W3>1000000]), na.rm = T) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackedbusinessvalueW3 [dataukh$BvalB1W3 == '\2431 million or more'] <- a

summary(dataukh$brackedbusinessvalueW3)

#We convert the brackets of the main residence value to numbers

dataukh$brackedbusinessvalue2W3<- 0

dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == 'Less than \243100'] <- 50
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\243100 to \2439,999'] <- 4950
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\24310,000 to \24324,999'] <- 17500
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\24325,000 to \24349,999'] <- 37500
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\24350,000 to \24399,999'] <- 75000
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\243100,000 to \243199,999'] <- 150000
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\243200,000 to \243299,999'] <- 250000
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\243300,000 to \243399,999'] <- 350000
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\243500,000 to \243999,999'] <- 750000

a <- mean ((dataukh$Bval2W3[dataukh$Bval2W3>1000000]), na.rm = T) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackedbusinessvalue2W3 [dataukh$BvalB2W3 == '\2431 million or more'] <- a

summary(dataukh$brackedbusinessvalue2W3)


#We convert the brackets of the main residence value to numbers

dataukh$brackedbusinessvalue3W3<- 0

dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == 'Less than \243100'] <- 50
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\243100 to \2439,999'] <- 4950
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\24310,000 to \24324,999'] <- 17500
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\24325,000 to \24349,999'] <- 37500
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\24350,000 to \24399,999'] <- 75000
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\243100,000 to \243199,999'] <- 150000
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\243200,000 to \243299,999'] <- 250000
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\243300,000 to \243399,999'] <- 350000
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\243400,000 to \243499,999'] <- 450000
dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\243500,000 to \243999,999'] <- 750000

a <- mean ((dataukh$Bval3W3[dataukh$Bval3W3>1000000]), na.rm = T) #We assign the average value of houses greater than 1M to the bracket

dataukh$brackedbusinessvalue3W3 [dataukh$BvalB3W3 == '\2431 million or more'] <- a

summary(dataukh$brackedbusinessvalue3W3)

###### Now we add them

#Now we can add the brackedted value and the normal value

dataukh$businessvalueW1 <- 0
dataukh$businessvalueW1 [dataukh$BvalW1_i>0 & !is.na(dataukh$BvalW1_i)] <- dataukh$BvalW1_i[dataukh$BvalW1_i>0 & !is.na(dataukh$BvalW1_i)]
dataukh$businessvalueW1 [dataukh$BvalW1_i <= 0 & dataukh$brackedbusinessvalueW1 >0 ] <- dataukh$brackedbusinessvalueW1[dataukh$BvalW1_i <= 0 & dataukh$brackedbusinessvalueW1 >0]
summary(dataukh$businessvalueW1)

dataukh$businessvalue2W1 <- 0
dataukh$businessvalue2W1 [dataukh$Bval2W1_i>0 & !is.na(dataukh$Bval2W1_i)] <- dataukh$Bval2W1_i[dataukh$Bval2W1_i>0 & !is.na(dataukh$Bval2W1_i)]
dataukh$businessvalue2W1 [dataukh$Bval2W1_i <= 0 & dataukh$brackedbusinessvalue2W1 >0 ] <- dataukh$brackedbusinessvalue2W1[dataukh$Bval2W1_i <= 0 & dataukh$brackedbusinessvalue2W1 >0]
summary(dataukh$businessvalue2W1)

#No value for 3w1

#Let's aggregate Wave 2, bracket and the other variable...

dataukh$businessvalueW2 <- 0
dataukh$businessvalueW2 [dataukh$BvalW2>0 & !is.na(dataukh$BvalW2)] <- dataukh$BvalW2[dataukh$BvalW2>0 & !is.na(dataukh$BvalW2)]
dataukh$businessvalueW2 [dataukh$BvalW2 <= 0 & dataukh$brackedbusinessvalueW2 >0 ] <- dataukh$brackedbusinessvalueW2[dataukh$BvalW2 <= 0 & dataukh$brackedbusinessvalueW2 >0]
summary(dataukh$businessvalueW2)

dataukh$businessvalue2W2 <- 0
dataukh$businessvalue2W2 [dataukh$Bval2W2>0 & !is.na(dataukh$Bval2W2)] <- dataukh$Bval2W2[dataukh$Bval2W2>0 & !is.na(dataukh$Bval2W2)]
dataukh$businessvalue2W2 [dataukh$Bval2W2 <= 0 & dataukh$brackedbusinessvalue2W2 >0 ] <- dataukh$brackedbusinessvalue2W2[dataukh$Bval2W2 <= 0 & dataukh$brackedbusinessvalue2W2 >0]
summary(dataukh$businessvalue2W2)

dataukh$businessvalue3W2 <- 0
dataukh$businessvalue3W2 [dataukh$Bval3W2>0 & !is.na(dataukh$Bval3W2)] <- dataukh$Bval3W2[dataukh$Bval3W2>0 & !is.na(dataukh$Bval3W2)]
dataukh$businessvalue3W2 [dataukh$Bval3W2 <= 0 & dataukh$brackedbusinessvalue3W2 >0 ] <- dataukh$brackedbusinessvalue3W2[dataukh$Bval3W2 <= 0 & dataukh$brackedbusinessvalue3W2 >0]
summary(dataukh$businessvalue3W2)


#Let's aggregate Wave 3, bracket and the other variable...

dataukh$businessvalueW3 <- 0
dataukh$businessvalueW3 [dataukh$Bval1W3>0 & !is.na(dataukh$Bval1W3)] <- dataukh$Bval1W3[dataukh$Bval1W3>0 & !is.na(dataukh$Bval1W3)]
dataukh$businessvalueW3 [dataukh$Bval1W3 <= 0 & dataukh$brackedbusinessvalueW3 >0 ] <- dataukh$brackedbusinessvalueW3[dataukh$Bval1W3 <= 0 & dataukh$brackedbusinessvalueW3 >0]
summary(dataukh$businessvalueW3)

dataukh$businessvalue2W3 <- 0
dataukh$businessvalue2W3 [dataukh$Bval2W3>0 & !is.na(dataukh$Bval2W3)] <- dataukh$Bval2W3[dataukh$Bval2W3>0 & !is.na(dataukh$Bval2W3)]
dataukh$businessvalue2W3 [dataukh$Bval2W3 <= 0 & dataukh$brackedbusinessvalue2W3 >0 ] <- dataukh$brackedbusinessvalue2W3[dataukh$Bval2W3 <= 0 & dataukh$brackedbusinessvalue2W3 >0]
summary(dataukh$businessvalue2W3)

dataukh$businessvalue3W3 <- 0
dataukh$businessvalue3W3 [dataukh$Bval3W3>0 & !is.na(dataukh$Bval3W3)] <- dataukh$Bval3W3[dataukh$Bval3W3>0 & !is.na(dataukh$Bval3W3)]
dataukh$businessvalue3W3 [dataukh$Bval3W3 <= 0 & dataukh$brackedbusinessvalue3W3 >0 ] <- dataukh$brackedbusinessvalue3W3[dataukh$Bval3W3 <= 0 & dataukh$brackedbusinessvalue3W3 >0]
summary(dataukh$businessvalue3W3)

### NOW WE NEED TO ADD THOSE VALUES ONLY IF THEY WERE INHERITED ####

dataukh$businessvalueW1h <- 0
dataukh$businessvalueW1h[(dataukh$BstartW1 == 'Inherited'|dataukh$BstartW1 == 'Given') & dataukh$inhdummy == 0] <- dataukh$businessvalueW1[(dataukh$BstartW1 == 'Inherited'|dataukh$BstartW1 == 'Given') & dataukh$inhdummy == 0]*1.14
summary(dataukh$businessvalueW1)
summary(dataukh$businessvalueW1h)

table(dataukh$businessvalueW1h, dataukh$inhdummy)

table(dataukh$BstartW1[dataukh$BvalW1>0], dataukh$inhdummy[dataukh$BvalW1>0]) #There is very few with actual data

dataukh$businessvalue2W1h <- 0
dataukh$businessvalue2W1h[(dataukh$Bstart2W1 == 'Inherited'|dataukh$Bstart2W1 == 'Given') & dataukh$inhdummy == 0] <- dataukh$businessvalue2W1[(dataukh$Bstart2W1 == 'Inherited'|dataukh$Bstart2W1 == 'Given') & dataukh$inhdummy == 0]*1.14
summary(dataukh$businessvalue2W1)
summary(dataukh$businessvalue2W1h)

table(dataukh$businessvalue2W1h, dataukh$inhdummy)

table(dataukh$Bstart2W1[dataukh$Bval2W1>0], dataukh$inhdummy[dataukh$Bval2W1>0]) #There is very few with actual data

### NOW WITH WAVE 2 (THERE IS NOT A THIRD BUSINESS INHERITED WITH WAVE 1)

dataukh$businessvalueW2h <- 0
dataukh$businessvalueW2h[(dataukh$BstartW2 == 'Inherited/joined family business'|dataukh$BstartW2 == 'Given') & dataukh$inhdummy == 0] <- dataukh$businessvalueW2[(dataukh$BstartW2 == 'Inherited/joined family business'|dataukh$BstartW2 == 'Given') & dataukh$inhdummy == 0]*1.1
summary(dataukh$businessvalueW2)
summary(dataukh$businessvalueW2h)

table(dataukh$businessvalueW2h, dataukh$inhdummy)

table(dataukh$BstartW2[dataukh$BvalW2>0], dataukh$inhdummy[dataukh$BvalW2>0]) #There is very few with actual data

dataukh$businessvalue2W2h <- 0
dataukh$businessvalue2W2h[(dataukh$Bstart2W2 == 'Inherited/joined family business'|dataukh$Bstart2W2 == 'Given') & dataukh$inhdummy == 0] <- dataukh$businessvalue2W2[(dataukh$Bstart2W2 == 'Inherited/joined family business'|dataukh$Bstart2W2 == 'Given') & dataukh$inhdummy == 0]*1.1
summary(dataukh$businessvalue2W2)
summary(dataukh$businessvalue2W2h)

table(dataukh$businessvalue2W2h, dataukh$inhdummy)

table(dataukh$Bstart2W2[dataukh$Bval2W2>0], dataukh$inhdummy[dataukh$Bval2W2>0]) #There is very few with actual data

dataukh$businessvalue3W2h <- 0
dataukh$businessvalue3W2h[(dataukh$Bstart3W2 == 'Inherited/joined family business'|dataukh$Bstart3W2 == 'Given') & dataukh$inhdummy == 0] <- dataukh$businessvalue3W2[(dataukh$Bstart3W2 == 'Inherited/joined family business'|dataukh$Bstart3W2 == 'Given') & dataukh$inhdummy == 0]*1.1
summary(dataukh$businessvalue3W2)
summary(dataukh$businessvalue3W2h)

table(dataukh$businessvalue3W2h, dataukh$inhdummy)

table(dataukh$Bstart3W2[dataukh$Bval3W2>0], dataukh$inhdummy[dataukh$Bval3W2>0]) #There is very few with actual data


### NOW WITH WAVE 3 (THERE IS NOT A THIRD BUSINESS INHERITED WITH WAVE 1)

dataukh$businessvalueW3h <- 0
dataukh$businessvalueW3h[(dataukh$Bstart1W3 == 'Inherited/joined family business'|dataukh$Bstart1W3 == 'Given') & dataukh$inhdummy == 0] <- dataukh$businessvalueW3[(dataukh$Bstart1W3 == 'Inherited/joined family business'|dataukh$Bstart1W3 == 'Given') & dataukh$inhdummy == 0]
summary(dataukh$businessvalueW3)
summary(dataukh$businessvalueW3h)

table(dataukh$businessvalueW3h, dataukh$inhdummy)

table(dataukh$Bstart1W3[dataukh$Bval1W3>0], dataukh$inhdummy[dataukh$Bval1W3>0]) #There is very few with actual data

dataukh$businessvalue2W3h <- 0
dataukh$businessvalue2W3h[(dataukh$Bstart2W3 == 'Inherited/joined family business'|dataukh$Bstart2W3 == 'Given') & dataukh$inhdummy == 0] <- dataukh$businessvalue2W3[(dataukh$Bstart2W3 == 'Inherited/joined family business'|dataukh$Bstart2W3 == 'Given') & dataukh$inhdummy == 0]
summary(dataukh$businessvalue2W3)
summary(dataukh$businessvalue2W3h)

table(dataukh$businessvalue2W3h, dataukh$inhdummy)

table(dataukh$Bstart2W3[dataukh$Bval2W3>0], dataukh$inhdummy[dataukh$Bval2W3>0]) #There is very few with actual data

dataukh$businessvalue3W3h <- 0
dataukh$businessvalue3W3h[(dataukh$Bstart3W3 == 'Inherited/joined family business'|dataukh$Bstart3W3 == 'Given') & dataukh$inhdummy == 0] <- dataukh$businessvalue3W3[(dataukh$Bstart3W3 == 'Inherited/joined family business'|dataukh$Bstart3W3 == 'Given') & dataukh$inhdummy == 0]
summary(dataukh$businessvalue3W3)
summary(dataukh$businessvalue3W3h)

table(dataukh$businessvalue3W3h, dataukh$inhdummy)

table(dataukh$Bstart3W3[dataukh$Bval3W3>0], dataukh$inhdummy[dataukh$Bval3W3>0]) #There is very few with actual data


#### NOW WE CAN ADD IT (IT INCLUDES ONLY THE ONES FOR INHERITANCE)


dataukh$businessvalueallW1h <- dataukh$businessvalueW1h + dataukh$businessvalue2W1h
dataukh$businessvalueallW2h <- dataukh$businessvalueW2h + dataukh$businessvalue2W2h + dataukh$businessvalue3W2h
dataukh$businessvalueallW3h <- dataukh$businessvalueW3h + dataukh$businessvalue2W3h + dataukh$businessvalue3W3h

dataukh$businessvalueinh <- dataukh$businessvalueallW1h + dataukh$businessvalueallW2h + dataukh$businessvalueallW3h


##### And we add it to the value of inheritances ###

dataukh$inhnew <- dataukh$inhh + dataukh$businessvalueinh

summary(dataukh$inh)
summary(dataukh$inhh)
summary(dataukh$inhnew)

summary(dataukh$inh[dataukh$inh > 0])
summary(dataukh$inhh[dataukh$inhh > 0])
summary(dataukh$inhnew[dataukh$inhnew > 0])

dataukh$inh <- dataukh$inhnew




saveRDS(dataukh, file = "data_rds/datauk-new-final-no-cpt-new-data7c.rds")


