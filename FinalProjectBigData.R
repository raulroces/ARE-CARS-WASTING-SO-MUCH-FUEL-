#PROJECT BIG DATA ANALITICS

#Installed necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#1.-Import the Data that is going to be used
engine <- read.csv("C:/Users/raulr/OneDrive/Escritorio/STU/FundofBigData/Project/Engines.csv")
View(engine)


#Since the appearance is not the required one. Dataframe should be modified
engine2 <- read.table( col.names = c("Dimensions Height","Dimensions Length","Dimensions Width","Engine Information Driveline","Engine Information Engine Type","Engine Information Hybrid","Engine Information Number of Forward Gears","Engine Information Transmission","Fuel Information City mpg","Fuel Information Fuel Type","Fuel Information Highway mpg","Identification Classification","Identification ID","Identification Make","Identification Model Year","Identification Year","Engine Information Engine Statistics Horsepower","Engine Information Engine Statistics Torque"),  skip = 1, sep = ",", text = unlist(engine, use.names = F))
View(engine2)
head(engine2)
#In order to do it a new dataframe is created (engine2), renaming the name of the columns and introducing the comma as separator

#Volume of the engine is introduced as a new variable 
engine_cm3 <- mutate(engine2, enginesize = Dimensions.Height * Dimensions.Length * Dimensions.Width)
View(engine_cm3)


#2.-Analyzing the data


df <- data.frame(table(engine_cm3$Fuel.Information.Fuel.Type))
df
#Different types of fuel used and the quantity the are used

ggplot(data = df, aes(x = "", y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Different Kind of fuel") +
  coord_polar("y") +
  theme_void()



#Car model with less fuel consumption can move more miles per gallon
abrev <- select(engine_cm3, Engine.Information.Engine.Type, Engine.Information.Driveline, Fuel.Information.Fuel.Type, Fuel.Information.City.mpg,Fuel.Information.Highway.mpg,Fuel.Information.Highway.mpg , Engine.Information.Engine.Statistics.Horsepower, Engine.Information.Engine.Statistics.Torque) 
View(abrev)
#Erase some variables to make the Dataframe easier to use

max_city <- arrange(abrev,desc(Fuel.Information.City.mpg))
View(head(max_city))
#List of the 6 cars that has less fuel consume (max 38 milles per gallon)


Fuel_city <- data.frame(table(max_city$Fuel.Information.City.mpg))
ggplot(data = Fuel_city) + 
  geom_col(mapping = aes(x =  Var1, y = Freq)) +
  labs(title = "Number of cars have a specific consumption fuel",x = "Miles per Gallon", y = "Number of cars")
#Colum graph that represent with what frequency a car has a fuel consume in the city


#Relation between fuel consume and number of gears in this sample
df3 <- data.frame(table(engine_cm3$Engine.Information.Number.of.Forward.Gears))
df3
#Number of cars that have 4, 5, 6, 7 and 8 gears respectively

engine_cm3_gear6 <- filter(engine_cm3,Engine.Information.Number.of.Forward.Gears == 6)
mean_gear6 = mean(engine_cm3_gear6$Fuel.Information.City.mpg, na.rm = TRUE)
mean_gear6

engine_cm3_gear4 <- filter(engine_cm3,Engine.Information.Number.of.Forward.Gears == 4)
mean_gear4 = mean(engine_cm3_gear4$Fuel.Information.City.mpg, na.rm = TRUE)
mean_gear4

engine_cm3_gear5 <- filter(engine_cm3,Engine.Information.Number.of.Forward.Gears == 5)
mean_gear5 = mean(engine_cm3_gear5$Fuel.Information.City.mpg, na.rm = TRUE)
mean_gear5

engine_cm3_gear7 <- filter(engine_cm3,Engine.Information.Number.of.Forward.Gears == 7)
mean_gear7 = mean(engine_cm3_gear7$Fuel.Information.City.mpg, na.rm = TRUE)
mean_gear7

engine_cm3_gear8 <- filter(engine_cm3,Engine.Information.Number.of.Forward.Gears == 8)
mean_gear8 = mean(engine_cm3_gear8$Fuel.Information.City.mpg, na.rm = TRUE)
mean_gear8

Fuel_gear <- data.frame("gear" = c("gear4", "gear5", "gear6", "gear7", "gear8"), "mean" = c(mean_gear4, mean_gear5, mean_gear6,mean_gear7,mean_gear8))

ggplot(data = Fuel_gear) + 
  geom_col(mapping = aes(x =  gear, y = mean))
#The fuel consume is almost equal between the different quantity of gears

ggplot(data = engine_cm3) + 
  geom_point(mapping = aes(x=enginesize, y=Fuel.Information.City.mpg, color = Fuel.Information.Fuel.Type)) + 
   labs(title = "City fuel consumption in function of the volume of the engine", x= "Engine Size", y= "City Fuel Consumption")
#In this graph is shown that the size of the engine does not matter in the fuel consumption but 
#but the type of fuel do. E85 is the one that less miles per gallon do.


#.1 SIMPLE REGRESSION
#I would like to know if variables as horsepower or torque affect the fuel consumption.

#To see if I can do a multiple regression a need to know the correlation between these two variables
cor(engine_cm3$Engine.Information.Engine.Statistics.Torque, engine_cm3$Engine.Information.Engine.Statistics.Horsepower)
#Since correlation is almost 0.94, means that as horsepower increase, torque increase too.
ggplot(data = engine_cm3) + 
  geom_point(mapping = aes(x=Engine.Information.Engine.Statistics.Torque, y=Engine.Information.Engine.Statistics.Horsepower)) + 
  labs(title = "Horsepower-Torque", x = "Torque", y = "Horsepower")


#Knowing that a simple linear regression, between torque and fuel consumption, is going to be done.
#First of all, it is necessary to check if response variable has a normal distribution
hist(engine_cm3$Fuel.Information.City.mpg) 
#Seeing the histogram could be said that response variable follows a normal distribution

#1.2 Linearity 
ggplot(data = engine_cm3) + 
  geom_point(mapping = aes(x=Engine.Information.Engine.Statistics.Torque, y=Fuel.Information.City.mpg)) + 
  labs(title = "City fuel consumption-Torque", x = "Torque", y = "City Fuel Consumption")
#Seeing the graph could be observed that the function follows a negative linear relation
#If torque increase miles per gallon decrease. 

ggplot(data = engine_cm3) + 
  geom_point(mapping = aes(x=Engine.Information.Engine.Statistics.Horsepower, y=Fuel.Information.City.mpg)) + 
  labs(title = "City fuel consume-Horsepower")
#Very similar negative linear relationship as the above graph

#1.3 Regression analysis
engine_cm3.lm <- lm(Fuel.Information.City.mpg ~ Engine.Information.Engine.Statistics.Torque, data = engine_cm3)

summary(engine_cm3.lm)
#Since p-value is really small, null hypothesis could be rejected. That means, there exist a negative linear
#relationship between torque and miles per gallon.
#Due to the high correlation between torque and horsepower could be said,
#For less horsepower less fuel consume


par(mfrow=c(2,2))
plot(engine_cm3.lm)


#Points lie reasonably close to the diagonal line on the plot then conclude that the "normality" assumption holds

#Lines of the residual graphs are not horizontal neither centrated on zero.
#this makes invalid the linear regression because data has outliers and bias, in other words
#has extreme values.
#points lie reasonably close to the diagonal line on the plot then conclude that the "normality" assumption holds






