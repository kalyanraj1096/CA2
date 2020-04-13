# Initial step the dataset was downloaded from blackboard and move to the project working directory later it was added
# inside the github directory 

#Set the working directory or check the path
getwd()
setwd("C:/Users/Kalyan Raj/Documents/assign")


#question A

NIPOCO_dataframe <- read.csv("NIPostcodes.csv",header = F) #Creating a dataframe and the fucntion read.csv effects the dataframe in such a way to read in tabluar form

NIPOCO_dataframe ##Each variable will be named as V1, V2, V3,....V15

nrow(NIPOCO_dataframe) #The maximum number of rows to read from NIPOCO_dataframe

ncol(NIPOCO_dataframe) #The maximum number of columns to read from NIPOCO_dataframe

str(NIPOCO_dataframe) #Informaion and structure of dataframe

head(NIPOCO_dataframe, n=10) #To dispalay first 10 rows in a dataset


#question B     
#Change the appropraite column name in dataframe.
colnames(NIPOCO_dataframe) <- c("Organisation_Name","Sub-building_Name","Building_Name","Number","Primary_Thorfare",
                                "Alt_Thorfare","Secondary_Thorfare","Locality",
                                "Townland","Town","County","PostCode","X-Cordinates",
                                "Y-Cordinates","Primary_Key")



#question C
#Replace and recode all missing values.

library(mice) #To create mullitple equations we use MICE(Multivariate Imputation via Chained Equations) package

md.pattern(NIPOCO_dataframe) #This is used to display missing-data patterns
 
library(VIM) #We use VIM(Visualization And Imputation) to  display Missing Values 

missing_values <- aggr(NIPOCO_dataframe, prop = FALSE, number = TRUE)
NIPOCO_dataframe[NIPOCO_dataframe==""] <- NA
sum(is.na(NIPOCO_dataframe))
sum(!complete.cases(NIPOCO_dataframe))



#question D
#Missing counts is used to find out number of missing values
#sapply is useful to check the length of the list elements
Missing_Count <- sapply(NIPOCO_dataframe, function(y) sum(length(which(is.na(y)))))
Missing_Count <- data.frame(Missing_Count)
Missing_Count


#question E
NIPOCO_dataframe <- subset(NIPOCO_dataframe, select = c(15, 1:14))


#question F
#Create Limavady_data dataframe.
Limavady_data <- NIPOCO_dataframe[which(NIPOCO_dataframe$Locality == "LIMAVADY" | NIPOCO_dataframe$Townland == "LIMAVADY" & NIPOCO_dataframe$Town == "LIMAVADY"),]
Limavady_data


#question G
#the maximum number of rows to read from NIPOCO_dataframe
nrow(Limavady_data)
write.csv(Limavady_data,"Limavady.csv")
write.csv(NIPOCO_dataframe,"CleanNIPostcodeData.csv")




#Section 2.

#question A
getwd()
setwd("C:/Users/Kalyan Raj/Documents/assign/NI Crime Data")
list.files()
list.dirs()
files <- list.files(recursive = T)
dataset <- data.frame()
#read all the csv from the directory.
for(file in files)
{
  temp_dataset <- read.csv(file,header = T)
  dataset <- rbind(dataset, temp_dataset)
  rm(temp_dataset)
}
nrow(dataset)
head(dataset, n=10)
setwd("C:/Users/Kalyan Raj/Documents/assign/NI Crime Data")
write.csv(dataset,"AIINICrimeData.csv",row.names = F)
rm(dataset)


#qestion B
Crime_data <- read.csv("AIINICrimeData.csv")
head(Crime_data)
nrow(Crime_data)
Crime_data$Crime.ID <- NULL
Crime_data$Reported.by <- NULL
Crime_data$Falls.within <- NULL
Crime_data$LSOA.code <- NULL
Crime_data$LSOA.name <- NULL
Crime_data$Last.outcome.category <- NULL
Crime_data$Context <- NULL

summary(Crime_data,15)`12`

#question C

library(plyr)
Crime_data$Crime.type <- revalue(Crime_data$Crime.type,c("Anti-social behaviour" = "ASBO","Bicycle theft" = "BITH",
                                                         "Burglary" = "BURG","Criminal damage and arson" = "CDAR",
                                                         "Drugs" = "DRUG","Other Theft = OTTH","Public order" = "PUBO",
                                                         "Robbery" = "ROBY", "Shoplifting" = "SHOP","Theft from the person" = "THPR",
                                                         "Vehicle crime" = "VECR", "Violence and sexual offences" = "VISO",
                                                         "Other crime" = "OTCR","Other theft" = "OTTH","Possession of weapons" = "POW"))

summary(Crime_data$Crime.type,15)
write.csv(Crime_data,"AIINICrimeData.csv",row.names = F)
Final_Crime_data <-read.csv("AIINICrimeData.csv")
nrow(Crime_data)
head(Crime_data)
str(Crime_data)

temp_data <- read.csv("AIINICrimeData.csv")

attach(Crime_data)
plot(Crime_data$Crime.type,type='o',col = "Blue")



#question D 
#Using the plot() function describe the crime frequency rate.
attach(Final_Crime_data)
plot(Final_Crime_data$Crime.type,ylim=c(0,200000),col = rainbow(14),main = "Crime frequeny rate",
     xlab="Crime Type",ylab="Number of Crimes")
detach(Final_Crime_data)


#question E

# remove the On or near string from the location column.
Final_Crime_data$Location <- sub("On or near ","",Final_Crime_data$Location)
head(Final_Crime_data$Location, n=10)
Final_Crime_data$Location[Final_Crime_data$Location == ""] <- NA

#Pick the random sample of 5000 entiries using set seed function by omitting location null values.
#Remove Rows With Missing Values On Columns Specified
secondary_Crime_data <- na.omit(Final_Crime_data)

#Set the seed of R's random number generator used for creating simulations or random objects that can be reproduced
set.seed(100)
random_crime_sample <- secondary_Crime_data[sample(nrow(secondary_Crime_data),5000),]





