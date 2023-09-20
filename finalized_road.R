library("xlsx")
setwd("C:/Users/alien/OneDrive - Universiti Sains Malaysia/Desktop/CPC351/Project 351")
library(ggplot2)
library(plotrix)
library(e1071)
library(caret)
library(caTools)

Road_data <- read.csv("Road_Traffic_Accidents.csv")

summary(Road_data)

#transforming data type of variables 
Road_data$Accident.Date <- as.Date(Road_data$Accident.Date, tryFormats = c("%d/%m/%Y"))
Road_data$X1st.Road.Class <- as.numeric(Road_data$X1st.Road.Class)
Road_data$Road.Surface <- as.factor(Road_data$Road.Surface)
Road_data$Lighting.Conditions <- as.factor(Road_data$Lighting.Conditions)
Road_data$Weather.Conditions <- as.factor(Road_data$Weather.Conditions)
Road_data$Type.of.Vehicle <- as.factor(Road_data$Type.of.Vehicle)
Road_data$Casualty.Class <- as.factor(Road_data$Casualty.Class)
Road_data$Casualty.Severity <- as.factor(Road_data$Casualty.Severity)
Road_data$Sex.of.Casualty <- as.factor(Road_data$Sex.of.Casualty)
Road_data$Age.of.Casualty <- as.numeric(Road_data$Age.of.Casualty)

#removing some of the columns
Road_data$Reference.Number <- NULL
Road_data$Grid.Ref..Easting <- NULL
Road_data$Grid.Ref..Northing <- NULL
Road_data$Accident.Date <- NULL
Road_data$X1st.Road.Class...No <- NULL
Road_data$Local.Authority <- NULL
Road_data$Vehicle.Number <- NULL

#changing names of columns
names(Road_data) [1] <- "num_of_vehicles"
names(Road_data) [2] <- "time_24hr"
names(Road_data) [3] <- "first_road_class"
names(Road_data) [4] <- "road_surface"
names(Road_data) [5] <- "lighting_conditions"
names(Road_data) [6] <- "weather_conditions"
names(Road_data) [7] <- "type_of_vehicle"
names(Road_data) [8] <- "casualty_class"
names(Road_data) [9] <- "casualty_severity"
names(Road_data) [10] <- "sex_of_casualty"
names(Road_data) [11] <- "age_of_casualty"

#creating a new variable for time to put them on scale
Road_data$time_fix <- ifelse(Road_data$time_24hr >= 0 & Road_data$time_24hr < 400, 'Night',
                             ifelse(Road_data$time_24hr >= 400 & Road_data$time_24hr < 1200, 'Morning',
                                    ifelse(Road_data$time_24hr >= 1200 & Road_data$time_24hr < 1700, 'Afternoon',
                                           ifelse(Road_data$time_24hr >= 1700 & Road_data$time_24hr < 2100, 'Evening',
                                                  ifelse(Road_data$time_24hr >= 2100, 'Night', 'Others')))))
#converting the newly created variables into factors
Road_data$time_fix <- as.factor(Road_data$time_fix)

#created new variable called age_group which has age data as categorical
age<- vector(mode="character",
             length=length(Road_data$age_of_casualty))
age[Road_data$age_of_casualty<=18] <- "Tenagers"
age[Road_data$age_of_casualty>18 & Road_data$age_of_casualty<=60 ] <- "Adults"
age[Road_data$age_of_casualty>60] <- "Senior Citizen"
age_group<- factor(age,levels=c("Tenagers", "Adults", "Senior Citizen"),
                   ordered = TRUE)
print(age_group)
Road_data<-cbind(Road_data,age_group)

plottheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (18), hjust = 0.5),
                   legend.title = element_text(colour = "#656568",  face = "bold.italic", family = "Helvetica"), 
                   legend.text = element_text(face = "bold.italic",family = "Helvetica"), 
                   axis.title = element_text(family = "Helvetica", size = (14)),
                   axis.text = element_text(family = "Courier", size = (14)))

#Graph of Number of Casualities by Gender and Age Group
ggplot(Road_data, aes(age_group, fill=sex_of_casualty)) +
  geom_bar(position="dodge")+ labs(title= "Number of Casualities by Gender and Age Group",y="Number of Casualities", x = "Age Group", fill="Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  plottheme +
  geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#ee846d", "#73ff00"),labels = c("1"="Male","2"="Female"))

#Graph of Number of Casualities by Gender and Casualty Severity
ggplot(Road_data, aes(x=casualty_severity, fill=sex_of_casualty)) +
  geom_bar(position="dodge") +
  plottheme +
  labs(title= "Number of Casualities by Gender and Casualty Severity",y="Number of Casualities", x = "Casualty Severity", fill="Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#ff7300", "#f0bbd7"),labels = c("1"="Male","2"="Female"))

#Graph of Number of Casualities by Gender and Casualty Class
ggplot(Road_data, aes(x=casualty_class, fill=sex_of_casualty)) +
  geom_bar(position="dodge") +
  plottheme +
  labs(title= "Number of Casualities by Gender and Casualty Class",y="Number of Casualities", x = "Casualty Class", fill="Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#eda692", "#0800ff"),labels = c("1"="Male","2"="Female"))

#Graph of Number of Casualities by Casualty Severity and Casualty Class
ggplot(Road_data, aes(x=casualty_class, fill=casualty_severity)) +
  geom_bar(position="dodge") +
  plottheme +
  labs(title= "Number of Casualities by Casualty Severity and Casualty Class",y="Number of Casualities", x = "Casualty Class", fill="Casualty Severity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#fffa3d", "#03ff00", "#1b00ff"),labels = c("1"="Fatal","2"="Serious","3"="Slight"))

#Barplot for number of casualities during parts of day
ggplot(Road_data, aes(x=time_fix, fill=factor(time_fix))) +
  geom_bar(position="dodge") +
  plottheme +
  labs(title= "Barplot for number of casualities during parts of day",
       y="Number of Casualities", x = "Parts of day", color="time_fix")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(0.9))

#Grouped Barplot for number of casualties during part of day\nbased on casualty class
ggplot(Road_data, aes(y=time_fix, fill=casualty_class)) +
  geom_bar(position="dodge") +
  plottheme +
  labs(title= "Grouped Barplot for number of casualties during part of day\nbased on casualty class",
       x="Number of Casualities", y = "Parts of day") +
  geom_text(stat='count', aes(label=..count..), hjust=-1, position = position_dodge(0.9)) +  
  scale_fill_discrete(name="Casualty Class",
                      breaks=c("1", "2", "3"),
                      labels=c("Driver/Rider", "Vehicle/Pillion Passenger", "Pedestrian"))

#setting set value for model
set.seed(200)
#removing column 2 and 11 from Road_data
Road_data <- Road_data[-c(2,11)]
#spliting data into train 80% and test 20%
split <- sample.split(Road_data, SplitRatio = 0.80)
traindata <- subset(Road_data, split == "TRUE")
testdata <- subset(Road_data, split == "FALSE")

#Plotting graph that shows Ditribution of Casualty Severity Class
ggplot(traindata, aes(x= casualty_severity, fill=casualty_severity))+geom_bar()+labs(title= "Ditribution of Casualty Severity Class",y="Number of casualty", x = "Casualty Severity", fill="Casuality Severity") +  
  theme(plot.title = element_text(hjust = 0.5))

summary(testdata)
#training the naive bayes model using train data
nbModel <- naiveBayes(casualty_severity ~.,data = traindata,laplace = 0)
nbModel

#testing the modal using testdata
prediction <- predict(nbModel, newdata = testdata[-c(8)])
#printing confusion matrix
cm <- confusionMatrix(testdata$casualty_severity,prediction)
print(cm) 



#performing sampling on traindata and storing them in newdata variable
newdata <- upSample(x=traindata[-c(8)], y=traindata$casualty_severity)
print(table(newdata$Class))

summary(testdata)
#traning model using newdata
nbMode2 <- naiveBayes(Class ~.,data = newdata)
nbMode2

#testing the modal using testdata
prediction <- predict(nbMode2, newdata = testdata[-c(8)])
#printing confusion matrix
cm <- confusionMatrix(testdata$casualty_severity,prediction)
print(cm) 
