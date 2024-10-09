getwd()
setwd("c:/Users/User/Documents")

library(readxl)
customer <- read_excel("C:/Users/User/Desktop/QUB Classes/Semester 1/Data Management/Assignment 1/Data 1_Customer.xlsx")
motor <- read_excel("C:/Users/User/Desktop/QUB Classes/Semester 1/Data Management/Assignment 1/Data 2_Motor Policies.xlsx")
health <- read_excel("C:/Users/User/Desktop/QUB Classes/Semester 1/Data Management/Assignment 1/Data 3_Health Policies.xlsx")
travel <- read_excel("C:/Users/User/Desktop/QUB Classes/Semester 1/Data Management/Assignment 1/Data 4_Travel Policies.xlsx")

library(tidyverse)
abt <- customer %>%
  left_join(motor, by = "MotorID") %>%
  left_join(health, by = "HealthID") %>%
  left_join(travel, by = "TravelID")

summary(abt)
str(abt)
sum(is.na(abt))

unique(abt$Title)
unique(abt$Gender)
summary(abt$Age)
plot(abt$Age)
boxplot(abt$Age)
unique(abt$ComChannel)
summary(abt$DependentsKids)
plot(abt$DependentsKids)
null_CustomerID <- abt[is.na(abt$CustomerID), ]
hist(abt$veh_value)


abt$Title[abt$Title == "Mr"] <- "Mr."
abt$Gender[abt$Gender == "male"] <- "m"
abt$Gender[abt$Gender == "female"] <- "f"
abt <- abt[abt$Age >= 0 & abt$Age < 84, ]
abt$ComChannel[abt$ComChannel == "E"] <- "Email"
abt$ComChannel[abt$ComChannel == "P"] <- "Phone"
abt$ComChannel[abt$ComChannel == "S"] <- "SMS"
abt <- abt[abt$DependentsKids <= 10, ]
abt <- abt[abt$veh_value < 8, ]

abt$Title <- as.factor(abt$Title)
abt$Gender <- as.factor(abt$Gender)
abt$CardType <- as.factor(abt$CardType)
abt$Location <- as.factor(abt$Location)
abt$ComChannel <- as.factor(abt$ComChannel)
abt$MotorType <- as.factor(abt$MotorType)
abt$clm <- as.factor(abt$clm)
abt$v_body <- as.factor(abt$v_body)
abt$HealthType <- as.factor(abt$HealthType)
abt$TravelType <- as.factor(abt$TravelType)
abt$v_age <- as.factor(abt$v_age)

#remember to give your rationale for policy start end renaming
abt <- abt %>%
  rename(policyStart_health = policyStart.x, policyEnd_health = policyEnd, policyStart_motor = PolicyStart,
         policyEnd_motor = PolicyEnd.x, policyStart_travel = policyStart.y, policyEnd_travel = PolicyEnd.y)

abt <- abt[complete.cases(abt$CustomerID), ]

#Detailed Analysis

abt <- abt %>%
  mutate(motor_policy = factor(ifelse(!is.na(policyStart_motor), 1, 0), levels = c(0, 1)))
abt <- abt %>%
  mutate(health_policy = factor(ifelse(!is.na(policyStart_health), 1, 0), levels = c(0, 1)))
abt <- abt %>%
  mutate(travel_policy = factor(ifelse(!is.na(policyStart_travel), 1, 0), levels = c(0, 1)))

str(abt)
summary(abt)

#motor
cor(abt$Age[complete.cases(abt$Age, abt$veh_value)], abt$veh_value[complete.cases(abt$Age, abt$veh_value)])

cor(
  abt$Age[complete.cases(abt$Age, as.numeric(abt$v_age))],
  as.numeric(abt$v_age[complete.cases(abt$Age, as.numeric(abt$v_age))])
)

cor(
  abt$Age[complete.cases(abt$Age, abt$Numclaims)],
  abt$Numclaims[complete.cases(abt$Age, abt$Numclaims)]
)

mean(abt$Exposure, na.rm=TRUE)
ggplot(abt, aes(x = Gender, y = Exposure)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge", fill = "blue") +
  labs(title = "Average Exposure by Gender", x = "Gender", y = "Average Exposure")

ggplot(abt, aes(x = Age, y = Exposure)) +
  geom_point(stat = "summary", position = "dodge", fill = "blue") +
  geom_smooth(method = "lm", color = "red")
  labs(title = "Exposure by Age", x = "Age", y = "Exposure")

dev.off()

as.numeric(abt$Age)
as.numeric(abt$veh_value)
ggplot(abt, aes(x = Age, y = veh_value)) +
  geom_point() +
  labs(title = "Age and Vehicle Value", x = "Age", y = "veh_value")

as.numeric(abt$Age)
as.numeric(abt$v_age)
ggplot(abt, aes(x = Age, y = v_age)) +
  geom_point() + geom_smooth(method = 'lm', color = "blue")
  labs(title = "Age and Vehicle Age", x = "Age", y = "Vehicle Age")
  
ggplot(abt, aes(x = Age, fill = MotorType)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of Age by MotorType", x = "Age", y = "Frequency") +
  theme_minimal()
  
ggplot(abt, aes(x = Gender, y = veh_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Barplot of veh_value by Gender", x = "Gender", y = "veh_value")

ggplot(abt, aes(x = Gender, y = Numclaims)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Barplot of NumClaims by Gender", x = "Gender", y = "NumClaims")

#health

plot(abt$HealthType, abt$HealthDependentsAdults, main = "HealthType and HealthDependentsAdults", xlab = "HealthType", ylab = "HealthDependentsAdults")


plot(abt$HealthType, abt$DependentsKids, main = "HealthType and DependentsKids", xlab = "HealthType", ylab = "DependentsKids")

plot(abt$Age, abt$DependentsKids, main = "Age and DependentsKids", xlab = 'Age', ylab = 'DependentsKids')

plot(abt$Age, abt$HealthDependentsAdults, main = "Age and HealthDependentsAdults", xlab = 'Age', ylab= 'HealthDependents Adults')

plot(abt$Age, abt$HealthType, main = "Age and HealthType", xlab = "Age", ylab = "HealthType")

#travel

ggplot(abt, aes(x = TravelType, y = Age)) +
  geom_bar(position = "stack", stat = "summary",na.rm = TRUE, fill = "lightgreen") +
  labs(title = "Average Age by TravelType", x = "TravelType", y = "Average Age")
