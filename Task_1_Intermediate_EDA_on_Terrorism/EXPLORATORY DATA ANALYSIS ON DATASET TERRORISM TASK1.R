
library(dplyr)
library(ggplot2)
# Uploading Data :-

terror_data <- read.csv("E:/LGM_Data_science/Global_Terrorism.csv", stringsAsFactors = FALSE)


#Slicing Required Data :-
Year <- terror_data$iyear
Country <- terror_data$country_txt
Region <- terror_data$region_txt
City <- terror_data$city
State <- terror_data$provstate
Terror_group <- terror_data$gname
Attack_type <- terror_data$attacktype1_txt
Target_type <- terror_data$targtype1_txt
Weapon_type <- terror_data$weaptype1_txt
SUcess_rate <- terror_data$success


final_data <- data.frame(Year, Country, Region, City,State, Terror_group, Attack_type,
                         Target_type, Weapon_type, SUcess_rate)
#head(final_data)
#tail(final_data)

# Visualizing 

# Year Vs Attacks 
year_arr<- count(final_data,final_data$Year, sort = TRUE)

x <- year_arr$`final_data$Year`[1:7]

y<- year_arr$n[1:7]

f_year <- data.frame(x,y)

p<-ggplot(data=f_year, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("Year")+ylab("Number of Attacks")+labs(fill = "Year")+
  ggtitle("Yearwise Attacks")

p

# Country Vs Attacks 
Country_arr<- count(final_data,final_data$Country, sort = TRUE)

x <- Country_arr$`final_data$Country`[1:7]

y<- Country_arr$n[1:7]

f_Country <- data.frame(x,y)

p<-ggplot(data=f_Country, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("Countries")+
  ylab("Number of Attacks")+labs(fill = "Countries")+
  ggtitle("Countrywise Attacks")

p


# Region Vs Attacks 
Region_arr<- count(final_data,final_data$Region, sort = TRUE)

x <- Region_arr$`final_data$Region`[1:7]

y<- Region_arr$n[1:7]

f_Region <- data.frame(x,y)

p<-ggplot(data=f_Region, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("Regions")+ylab("Number of Attacks")+labs(fill = "Regions")+
  ggtitle("Region wise Attacks")

p


# City Vs Attacks 
City_arr<- count(final_data,final_data$City, sort = TRUE)

x <- City_arr$`final_data$City`[1:7]

y<- City_arr$n[1:7]

f_city <- data.frame(x,y)

p<-ggplot(data=f_city, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("Cities")+ylab("Number of Attacks")+labs(fill = "Cities")+
  ggtitle("City  wise Attacks")

p



# State Vs Attacks 
State_arr<- count(final_data,final_data$State, sort = TRUE)

x <- State_arr$`final_data$State`[1:7]

y<- State_arr$n

f_State <- data.frame(x,y)

p<-ggplot(data=f_State, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("State")+ylab("Number of Attacks")+labs(fill = "States")+
  ggtitle("State wise Attacks")

p

#Terror_group Vs Attacks

terror_grop_arrr<- count(final_data,final_data$Terror_group, sort = TRUE)

x <- terror_grop_arrr$`final_data$Terror_group`[1:3]

y<- terror_grop_arrr$n[1:3]

f_terror_group <- data.frame(x,y)

p<-ggplot(data=f_terror_group, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("Terror Groups")+ylab("Number of Attacks")+labs(fill = "Terror Groups")+
  ggtitle("Terror Groups Vs Number of Attacks")

p

#Attack_Type Vs Attacks

Attack_Type_arr<- count(final_data,final_data$Attack_type, sort = TRUE)

x <- Attack_Type_arr$`final_data$Attack_type`[1:5]

y<- Attack_Type_arr$n[1:5]

f_Attack_Type <- data.frame(x,y)

p<-ggplot(data=f_Attack_Type, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("Attack Type")+ylab("Number of Attacks")+labs(fill = "Attack Type")+
  ggtitle("Attack Type Vs Number of Attacks")

p

#Target_type Vs Attacks

Target_type_arr <- count(final_data,final_data$Target_type, sort = TRUE)

x <- Target_type_arr$`final_data$Target_type`[1:5]

y<- Target_type_arr$n[1:5]

f_Target_type <- data.frame(x,y)

p<-ggplot(data=f_Target_type, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("Target Type")+ylab("Number of Attacks")+labs(fill = "Target Type")+
  ggtitle("Target Type Vs Number of Attacks")

p



#Weapon_type Vs Attacks

Weapon_type_arr <- count(final_data,final_data$Weapon_type, sort = TRUE)

x <- Weapon_type_arr$`final_data$Weapon_type`[1:5]

y<- Weapon_type_arr$n[1:5]

f_Weapon_type <- data.frame(x,y)

p<-ggplot(data=f_Weapon_type, aes(x=x, y=y, fill = x)) +
  geom_bar(stat="identity")+xlab("Weapon Type")+ylab("Number of Attacks")+labs(fill = "Weapon Type")+
  ggtitle("Weapon Type Vs Number of Attacks")

p


#Success Rate Vs Attacks

Success_arr <- count(final_data,final_data$SUcess_rate, sort = TRUE)
x <- Success_arr$`final_data$SUcess_rate`
rate <- c("Sucess","failure")
y<- Success_arr$n
pie(y, rate, main = "Success and Failure of Attacks ", col = rainbow(length(x)))


# My conclusion :- 

# Most attacks in 2014

# Most Affected Country :- IRAQ

# Most Affected Region :- MIDDLE EAST AND NORTH AFRICA

# Most Affected City :- Bagadad 

# Most Affected State :- Bagadad 

# Most Active Terrorist group :- TALIBAN

# Most used Attack Type :- Armed Assault

# Maximum Targeted Type :- Private citizens and property  

# Most weapon types used :- Explosives 

# Terrorisms have very high sucess rate . 














