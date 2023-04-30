#LIM YAU JADE
#TP065086

###DATA IMPORT###

#load package
library(ggplot2)
library(crayon)
library(dplyr)
library(scales)
library(plotrix)
library(tidyverse)

#import dataset
rent = read.csv("C:\\Users\\kelly\\Desktop\\APU Year 2\\PFDA\\Assignment\\House_Rent_Dataset.csv",
                       header=TRUE)

rent

#View all data
View(rent)

#View the first six rows
head(rent)

#View the last six rows
tail(rent)



###CLEANING###

#check null value
is.null(rent)
anyNA(rent)

#check duplicated value
duplicated(rent)

###PRE-PROCESSING###

#check data type
str(rent)

#convert date
rent$Posted.On <- as.Date(rent$Posted.On,"%m/%d/%Y")

rent <- mutate(rent, DAY = as.numeric(format(rent$Posted.On, format = "%d")),
               MONTH = as.numeric(format(rent$Posted.On, format = "%m")),
               YEAR = as.numeric(format(rent$Posted.On, format = "%Y")))

#split the floor available and total floor
floor = data.frame(do.call("rbind", strsplit(as.character(rent$Floor), " out of ", fixed = TRUE)))
names(floor)=c("Floor_Available", "Total_Floor")
floor

#check floor structure
str(floor)

#find element in floor
unique(floor$Floor_Available)

#replace value in floor
floor$Floor_Available[floor$Floor_Available == "Ground"] <- "0"
floor$Floor_Available[floor$Floor_Available == "Upper Basement"] <- "0.75"
floor$Floor_Available[floor$Floor_Available == "Lower Basement"] <- "0.25"
floor$Total_Floor[floor$Total_Floor == "Ground"] <- "0"
unique(floor$Floor_Available)

#modify floor column in rent
rent['Floor'] = NULL
rent = cbind(rent, floor)

#replace value in point of contact
unique(rent$Point.of.Contact)
rent$Point.of.Contact[rent$Point.of.Contact == "Contact Owner"] <- "0"
rent$Point.of.Contact[rent$Point.of.Contact == "Contact Agent"] <- "1"
rent$Point.of.Contact[rent$Point.of.Contact == "Contact Builder"] <- "2"
rent$Point.of.Contact

#change data type
rent$Point.of.Contact <- as.numeric(rent$Point.of.Contact)
rent$Floor_Available <- as.numeric(rent$Floor_Available)
rent$Total_Floor <- as.numeric(rent$Total_Floor)

#assign header
names(rent)=c("DATE","BHK","RENTAL","HOUSE_SIZE","AREA_TYPE",
                  "LOCALITY","CITY","FURNISHING_STATUS","TENANT_PREFERRED",
                  "BATHROOM","POINT_OF_CONTACT","DAY","MONTH","YEAR","FLOOR_AVAILABLE","TOTAL_FLOOR")

#check data type
datatype = data.frame(DATA_TYPE = sapply(rent, class))
datatype
View(datatype)

#structure
str(rent)

#summarize data
summary(rent)


###DATA EXPLORATION###
#Column, row and dimension
ncol(rent)
nrow(rent)
dim(rent)

#TOTAL AREA TYPE
area_num = nlevels(factor(rent$AREA_TYPE))
paste("Total Area Type: ", area_num)
unique(rent$AREA_TYPE)

#Convert to list
area_col = duplicated(as.list(rent$AREA_TYPE))

#Remove duplicated column
area = as.data.frame(rent$AREA_TYPE[!area_col])
names(area) = c("Area Type")
View(area)

#TOTAL AREA LOCALITY
locality_num = nlevels(factor(rent$LOCALITY))
paste("Total Area Locality: ", locality_num)
unique(rent$LOCALITY)

#Convert to list
locality_col = duplicated(as.list(rent$LOCALITY))

#Remove duplicated column
locality = as.data.frame(rent$LOCALITY[!locality_col])
names(locality) = c("Area Locality")
View(locality)

#TOTAL CITY
city_num = nlevels(factor(rent$CITY))
paste("Total City: ", city_num)
unique(rent$CITY)

#Convert to list
city_col = duplicated(as.list(rent$CITY))

#Remove duplicated column
city = as.data.frame(rent$CITY[!city_col])
names(city) = c("City")
View(city)

#TOTAL TYPE OF FURNISHING STATUS
furnish_num = nlevels(factor(rent$FURNISHING_STATUS))
paste("Total Type of Furnishing Status: ", furnish_num)
unique(rent$FURNISHING_STATUS)

#Convert to list
furnish_col = duplicated(as.list(rent$FURNISHING_STATUS))

#Remove duplicated column
furnish = as.data.frame(rent$FURNISHING_STATUS[!furnish_col])
names(furnish) = c("Furnishing Status")
View(furnish)

#TOTAL TYPE OF TENANT PREFERRED
tenant_num = nlevels(factor(rent$TENANT_PREFERRED))
paste("Total Type of Tenant Preferred: ", tenant_num)
unique(rent$TENANT_PREFERRED)

#Convert to list
tenant_col = duplicated(as.list(rent$TENANT_PREFERRED))

#Remove duplicated column
tenant = as.data.frame(rent$TENANT_PREFERRED[!tenant_col])
names(tenant) = c("Tenant Preferred")
View(tenant)

#TOTAL TYPE OF POINT OF CONTACT
contact_num = nlevels(factor(rent$POINT_OF_CONTACT))
paste("Total Type of Point of Contact: ", contact_num)
unique(rent$POINT_OF_CONTACT)

#Convert to list
contact_col = duplicated(as.list(rent$POINT_OF_CONTACT))

#Remove duplicated column
contact = as.data.frame(rent$POINT_OF_CONTACT[!contact_col])
names(contact) = c("Point of Contact")
POINT_OF_CONTACT = c("Contact Owner", "Contact Agent", "Contact Builder")
contact <- cbind(POINT_OF_CONTACT, contact)
names(contact) <- c("Point of Contact", "Point of Contact Label")
View(contact)

#House Size Range Data
house_size_range <- rent %>%
  mutate(SIZE_RANGE = cut(HOUSE_SIZE,
                        seq(0,8000,2000))) %>%
  group_by(SIZE_RANGE) %>%
  dplyr::summarise(QUANTITY = n()) %>%
  as.data.frame()
house_size_range$SIZE_RANGE <- c("0-2000", "2001-4000", "4001-6000", "6001-8000")
house_size_range

#TOTAL TYPE OF BHK
bhk_num = nlevels(factor(rent$BHK))
paste("Total Type of BHK: ", bhk_num)
unique(rent$BHK)

#Convert to list
bhk_col = duplicated(as.list(rent$BHK))

#Remove duplicated column
bhk = as.data.frame(rent$BHK[!bhk_col])
names(bhk) = c("BHK")
View(bhk)

#TOTAL TYPE OF FLOOR AVAILABLE
floor_available_num = nlevels(factor(rent$FLOOR_AVAILABLE))
paste("Total Type of Floor Available: ", floor_available_num)
unique(rent$FLOOR_AVAILABLE)

#Convert to list
floor_available_col = duplicated(as.list(rent$FLOOR_AVAILABLE))

#Remove duplicated column
floor_available = as.data.frame(rent$FLOOR_AVAILABLE[!floor_available_col])
names(floor_available) = c("Floor Available")
View(floor_available)

#TOTAL TYPE OF TOTAL FLOOR
total_floor_num = nlevels(factor(rent$TOTAL_FLOOR))
paste("Total Type of Total Floor: ", total_floor_num)
unique(rent$TOTAL_FLOOR)

#Convert to list
total_floor_col = duplicated(as.list(rent$TOTAL_FLOOR))

#Remove duplicated column
total_floor = as.data.frame(rent$TOTAL_FLOOR[!total_floor_col])
names(total_floor) = c("Total Floor")
View(total_floor)

#TOTAL TYPE OF BATHROOM
bathroom_num = nlevels(factor(rent$BATHROOM))
paste("Total Type of Total Floor: ", bathroom_num)
unique(rent$BATHROOM)

#Convert to list
bathroom_col = duplicated(as.list(rent$BATHROOM))

#Remove duplicated column
bathroom = as.data.frame(rent$BATHROOM[!bathroom_col])
names(bathroom) = c("Floor Available")
View(bathroom)

#Rental Range Data
rental_range_1200_3500000 <- rent %>%
  mutate(RENTAL_RANGE = cut(RENTAL,
                            seq(1200,3500000,50000))) %>%
  group_by(RENTAL_RANGE) %>%
  dplyr::summarise(QUANTITY = n()) %>%
  as.data.frame()

rental_range_1200_3500000$RENTAL_RANGE <- c("1000-51000", "51000-101000", "101000-151000", 
                                            "151000-201000", "201000-251000", "251000-301000",
                                            "301000-351000","351000-401000","401000-451000",
                                            "451000-501000", "501000-551000","551000-601000",
                                            "601000-651000","651000-701000","801000-851000",
                                            "951000-1000000","1150000-1200000", "1200000-3500000")
rental_range_1200_3500000

rental_range_1200_1200000 <- rent %>%
  mutate(RENTAL_RANGE = cut(RENTAL,
                          seq(1200,1200000,50000))) %>%
  group_by(RENTAL_RANGE) %>%
  dplyr::summarise(QUANTITY = n()) %>%
  as.data.frame()

rental_range_1200_1200000$RENTAL_RANGE <- c("1000-51000", "51000-101000", "101000-151000", 
                             "151000-201000", "201000-251000", "251000-301000",
                             "301000-351000","351000-401000","401000-451000",
                             "451000-501000", "501000-551000","551000-601000",
                             "601000-651000","651000-701000","801000-851000",
                             "951000-1000000","1150000-1200000")
rental_range_1200_1200000

rental_range1_1200_1200000 <- rent %>%
  mutate(RENTAL_RANGE = cut(RENTAL,
                            seq(1000,1200000,100000))) %>%
  group_by(RENTAL_RANGE) %>%
  dplyr::summarise(QUANTITY = n()) %>%
  as.data.frame()
rental_range1_1200_1200000$RENTAL_RANGE <- c("1000-100000", "100000-201000", "201000-301000", 
                                "301000-401000","401000-501000","501000-601000",
                                "601000-701000","801000-901000",
                               "901000-1000000","1100000-1200000")
rental_range1_1200_1200000

rental_range_1000_10000 <- rent %>%
  mutate(RENTAL_RANGE = cut(RENTAL,
                            seq(1000,10000,1000))) %>%
  group_by(RENTAL_RANGE) %>%
  dplyr::summarise(QUANTITY = n()) %>%
  as.data.frame()
rental_range_1000_10000 <-rental_range_1000_10000 %>% drop_na()

rental_range_1000_10000$RENTAL_RANGE <- c("1000-2000", "2000-3000", "3000-4000", 
                                "4000-5000","5000-6000","6000-7000",
                                "7000-8000","8000-9000","9000-10000")
rental_range_1000_10000

#Force full display
options(scipen=999)
options(show.signif.stars=FALSE)

#Question 1: How does geographical location affect the house rent ?
#Analysis 1.1: The Distribution of House Available to Rent According to the Cities
#Pie Chart
house_city <- rent %>%
  group_by(CITY) %>%
  summarise(QUANTITY = n())
house_city

summary(house_city)

par(mar = c(2, 2, 2, 2))
pie(house_city$QUANTITY, paste(house_city$CITY,house_city$QUANTITY), 
    radius = 0.7, main = "The Distribution of House Available to Rent According to the Cities",
    col = topo.colors(length(house_city$CITY)), clockwise =  TRUE)
legend("topright", house_city$CITY, fill = topo.colors(length(house_city$CITY)), 
       pt.cex = 2, cex = 0.7, horiz = FALSE, inset = c( -0.3 , 0.35)) #Additional Features

#Bar Plot
ggplot(house_city, aes(x = CITY, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(house_city$CITY))) +
  ggtitle("The Distribution of House Available to Rent According to the Cities") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) + #Additional Features
  geom_text(aes(CITY, label = QUANTITY), vjust = 1, position = position_dodge(width = 0.1))
   

#Analysis 1.2: The Rent Distribution
#Bar Plot
par(mar = c(6, 6, 6, 6))
ggplot(rental_range_1200_3500000, aes(x = RENTAL_RANGE, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.5, color = "Black", 
           fill = topo.colors(length(rental_range_1200_3500000$RENTAL_RANGE))) +
  ggtitle("The Rent Distribution (Range Between 1000 - 3500000)") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6), 
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(RENTAL_RANGE, label = QUANTITY), vjust = -1, position = position_dodge(width = 0.1))

#Lollipop Chart for Range Between 1000 to 10000 #Additional Features
ggplot(rental_range_1000_10000, aes(x = RENTAL_RANGE, y = QUANTITY)) +
  geom_segment( aes(x = RENTAL_RANGE, xend = RENTAL_RANGE, y = 0, yend = QUANTITY), 
                colour = "Black") +
  geom_point( size = 12, color = "black", 
              fill = alpha(topo.colors(length(rental_range_1000_10000$RENTAL_RANGE)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = QUANTITY), color = "black", size = 3.5) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("The Rent Distribution (Range Between 1000 to 10000)")

#Analysis 1.3: The House Distribution in Each City According to Locality
#Analysis 1.3.1: The House Distribution in KolKata According to Locality
locality_city_kolkata <- rent %>%
  select(LOCALITY,CITY) %>%
  group_by(LOCALITY,CITY) %>%
  filter(CITY %in% c("Kolkata")) %>%
  summarise(QUANTITY = n())

locality_city_kolkata

locality_city_kolkata1 <- locality_city_kolkata %>%
  group_by(QUANTITY) %>%
  summarise(COUNT = n())

locality_city_kolkata1

ggplot(locality_city_kolkata1, aes(x = QUANTITY, y = COUNT)) +
  geom_segment( aes(x = QUANTITY, xend = QUANTITY, y = 0, yend = COUNT), colour = "Black") +
  geom_point( size = 12, color = "black", 
              fill = alpha(topo.colors(length(locality_city_kolkata1$QUANTITY)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = COUNT), color = "black", size = 3.5) +
  scale_x_continuous(breaks = seq(from = 0, to = 16, by = 1)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("The House Distribution in Kolkata According to Locality")

#Analysis 1.3.2: The House Distribution in Mumbai According to Locality
locality_city_mumbai <- rent %>%
  select(LOCALITY,CITY) %>%
  group_by(LOCALITY,CITY) %>%
  filter(CITY %in% c("Mumbai")) %>%
  summarise(QUANTITY = n())

locality_city_mumbai

locality_city_mumbai1 <- locality_city_mumbai %>%
  group_by(QUANTITY) %>%
  summarise(COUNT = n())

locality_city_mumbai1

ggplot(locality_city_mumbai1, aes(x = QUANTITY, y = COUNT)) +
  geom_segment( aes(x = QUANTITY, xend = QUANTITY, y = 0, yend = COUNT), colour = "Black") +
  geom_point( size = 12, color = "black", 
              fill = alpha(topo.colors(length(locality_city_mumbai1$QUANTITY)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = COUNT), color = "black", size = 3.5) +
  scale_x_continuous(breaks = seq(from = 0, to = 40, by = 5)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("The House Distribution in Mumbai According to Locality")

#Analysis 1.3.3: The House Distribution in Bangalore According to Locality
locality_city_bangalore <- rent %>%
  select(LOCALITY,CITY) %>%
  group_by(LOCALITY,CITY) %>%
  filter(CITY %in% c("Bangalore")) %>%
  summarise(QUANTITY = n())

locality_city_bangalore

locality_city_bangalore1 <- locality_city_bangalore %>%
  group_by(QUANTITY) %>%
  summarise(COUNT = n())

locality_city_bangalore1

ggplot(locality_city_bangalore1, aes(x = QUANTITY, y = COUNT)) +
  geom_segment( aes(x = QUANTITY, xend = QUANTITY, y = 0, yend = COUNT), colour = "Black") +
  geom_point( size = 12, color = "black", 
              fill = alpha(topo.colors(length(locality_city_bangalore1$QUANTITY)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = COUNT), color = "black", size = 3.5) +
  scale_x_continuous(breaks = seq(from = 0, to = 25, by = 5)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("The House Distribution in Bangalore According to Locality")

#Analysis 1.3.4: The House Distribution in Delhi According to Locality
locality_city_delhi <- rent %>%
  select(LOCALITY,CITY) %>%
  group_by(LOCALITY,CITY) %>%
  filter(CITY %in% c("Delhi")) %>%
  summarise(QUANTITY = n())

locality_city_delhi

locality_city_delhi1 <- locality_city_delhi %>%
  group_by(QUANTITY) %>%
  summarise(COUNT = n())

locality_city_delhi1

ggplot(locality_city_delhi1, aes(x = QUANTITY, y = COUNT)) +
  geom_segment( aes(x = QUANTITY, xend = QUANTITY, y = 0, yend = COUNT), colour = "Black") +
  geom_point( size = 12, color = "black", 
              fill = alpha(topo.colors(length(locality_city_delhi1$QUANTITY)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = COUNT), color = "black", size = 3.5) +
  scale_x_continuous(breaks = seq(from = 0, to = 22, by = 4)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("The House Distribution in Delhi According to Locality")

#Analysis 1.3.5: The House Distribution in Chennai According to Locality
locality_city_chennai <- rent %>%
  select(LOCALITY,CITY) %>%
  group_by(LOCALITY,CITY) %>%
  filter(CITY %in% c("Chennai")) %>%
  summarise(QUANTITY = n())

locality_city_chennai

locality_city_chennai1 <- locality_city_chennai %>%
  group_by(QUANTITY) %>%
  summarise(COUNT = n())

locality_city_chennai1

ggplot(locality_city_chennai1, aes(x = QUANTITY, y = COUNT)) +
  geom_segment( aes(x = QUANTITY, xend = QUANTITY, y = 0, yend = COUNT), colour = "Black") +
  geom_point( size = 12, color = "black", 
              fill = alpha(topo.colors(length(locality_city_chennai1$QUANTITY)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = COUNT), color = "black", size = 3.5) +
  scale_x_continuous(breaks = seq(from = 0, to = 25, by = 5)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("The House Distribution in Chennai According to Locality")

#Analysis 1.3.6: The House Distribution in Hyderabad According to Locality
locality_city_hyderabad <- rent %>%
  select(LOCALITY,CITY) %>%
  group_by(LOCALITY,CITY) %>%
  filter(CITY %in% c("Hyderabad")) %>%
  summarise(QUANTITY = n())

locality_city_hyderabad

locality_city_hyderabad1 <- locality_city_hyderabad %>%
  group_by(QUANTITY) %>%
  summarise(COUNT = n())

locality_city_hyderabad1

ggplot(locality_city_hyderabad1, aes(x = QUANTITY, y = COUNT)) +
  geom_segment( aes(x = QUANTITY, xend = QUANTITY, y = 0, yend = COUNT), colour = "Black") +
  geom_point( size = 12, color = "black",
              fill = alpha(topo.colors(length(locality_city_hyderabad1$QUANTITY)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = COUNT), color = "black", size = 3.5) +
  scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("The House Distribution in Hyderabad According to Locality")

#Analysis 1.4: Average House Rent According to City
avg_rent_city <- rent %>%
  group_by(CITY) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_city

#Bar Plot
ggplot(avg_rent_city, aes(x = CITY, y = AVG)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(avg_rent_city$CITY))) +
  ggtitle("Average House Rent According to City") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(CITY, label = AVG), position = position_dodge(width = 0.1))

#Analysis 1.5: The Top 10 House Availability to Rent According to Area Locality Distribution in Cities
# Suppress summaries info
options(dplyr.summarise.inform = FALSE)

locality_city <- rent %>%
  select(LOCALITY,CITY) %>%
  group_by(LOCALITY,CITY) %>%
  summarise(QUANTITY = n())
locality_city

top10_locality = head(arrange(locality_city,desc(QUANTITY), .group = "drop"),10)
par(mar=c(7,5,5,5))
top10_locality_bar <- barplot(top10_locality$QUANTITY ,ylab = "House Availablity",
                              border=F , names.arg=top10_locality$LOCALITY, 
                  las=2 , 
                  col=c("#4C00FF","#0080FF","#00FF4D","#0080FF","#E6FF00",
                        "#E6FF00","#4C00FF","#00FF4D","#FFE53C","#0080FF") , 
                  ylim=c(0,40) , 
                  main="The Top 10 Highest House Availability to Rent According 
                  to Area Locality Distribution in Cities" ) +
  abline(v=c(3.7 , 7.3 ,10.9 ) , col="grey")

  top10_locality_bar <- legend("topright", legend = c(unique(top10_locality$CITY)) , 
       col = c("#4C00FF","#0080FF","#00FF4D","#E6FF00","#FFE53C") , 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.5, horiz = FALSE, inset = c(- 0.22, 0))
  

#Question 2: How does the house condition affect the house rent ?
#Analysis 2.1: How does the quantity of BHK affect the house rent ?
#Analysis 2.1.1: The Quantity of Bedrooms, Halls and Kitchen
bhk_num <- rent %>%
  group_by(BHK) %>%
  summarise(QUANTITY = n())
bhk_num

#Bar Plot
ggplot(bhk_num, aes(x = BHK, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", fill = topo.colors(length(bhk_num$BHK))) +
  ggtitle("The Quantity of Bedrooms, Halls and Kitchen") + 
  scale_x_continuous(breaks = seq(from = 0, to = 6, by = 1)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(BHK, label = QUANTITY), position = position_dodge(width = 0.1))

#Analysis 2.1.2: Average Rent according to BHK
avg_rent_bhk <- rent %>%
  group_by(BHK) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_bhk$AVG <- as.integer(avg_rent_bhk$AVG)
avg_rent_bhk <- arrange(avg_rent_bhk, AVG)
avg_rent_bhk

ggplot(avg_rent_bhk, aes(x = BHK, y = AVG)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(avg_rent_bhk$BHK))) +
  scale_x_continuous(breaks = seq(from = 0, to = 6, by = 1)) +
  ggtitle("Average House Rent According to BHK") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(BHK, label = AVG), position = position_dodge(width = 0.1))

#Analysis 2.2: How does the quantity of Bathroom affect the house rent ?
#Analysis 2.2.1: The Quantity of Bathroom
bath_num <- rent %>%
  group_by(BATHROOM) %>%
  summarise(QUANTITY = n())
bath_num

ggplot(bath_num, aes(x = BATHROOM, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(bath_num$BATHROOM))) +
  ggtitle("The Quantity of Bathrooms") + 
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 1)) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(BATHROOM, label = QUANTITY), position = position_dodge(width = 0.1))

#Analysis 2.2.2: Average Rent according to Bathroom
avg_rent_bath <- rent %>%
  group_by(BATHROOM) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_bath$AVG <- as.integer(avg_rent_bath$AVG)

plot(avg_rent_bath$AVG, type = "o", xlab = "Bathroom", ylab = "Average Rental", 
     main = "Average Rent according to Bathroom", col = "black" )

#Analysis 2.3: How the numbers of rooms affect the rent ?
label <- c("bath", "bhk")
par(mar = c(2, 2, 2, 2))
plot1 <- plot(avg_rent_bath$AVG, type = "l", xlab = "Rooms", ylab = "Average Rental", 
              main = "Average Rent according to Rooms Available", col = "blue")
plot1 <- lines(avg_rent_bhk$AVG, type = "l", xlab = "Rooms", ylab = "Average Rental", 
              main = "Average Rent according to Rooms Available", col = "green")
plot1 <- legend("topright", legend = label, cex = 0.8, fill = c("blue", "green"))

#Analysis 2.4: How does the type of furnishing status affect the house rent ?
#Analysis 2.4.1: The Total Quantity of Each Type of Furnishing Status
house_furnish <- rent %>%
  group_by(FURNISHING_STATUS) %>%
  summarise(QUANTITY = n())
house_furnish

#Pie Chart
pie(house_furnish$QUANTITY, paste(house_furnish$FURNISHING_STATUS,house_furnish$QUANTITY), 
    radius = 0.7, main = "House Available to Rent Distribution According to Furnishing Status",
    col = topo.colors(length(house_furnish$FURNISHING_STATUS)), clockwise =  TRUE)
legend("topright", house_furnish$FURNISHING_STATUS, fill = 
         topo.colors(length(house_furnish$FURNISHING_STATUS)),
       pt.cex = 2, cex = 0.7, horiz = FALSE, inset = c( -0.3 , 0.35))
par(mar = c(2, 2, 2, 2))

#Bar Chart
ggplot(house_furnish, aes(x = FURNISHING_STATUS, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", fill = 
             topo.colors(length(house_furnish$FURNISHING_STATUS))) +
  ggtitle("The Quantity of Furnishing Status") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(FURNISHING_STATUS, label = QUANTITY), cex = 5, vjust = 1.5)

#Analysis 2.4.2: Average Rent according to Furnishing Status
avg_rent_furnish <- rent %>%
  group_by(FURNISHING_STATUS) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_furnish$AVG <- as.integer(avg_rent_furnish$AVG)
avg_rent_furnish <- arrange(avg_rent_furnish, AVG)

ggplot(avg_rent_furnish, aes(x = FURNISHING_STATUS, y = AVG)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 60000,by = 10000)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(avg_rent_furnish$FURNISHING_STATUS))) +
  ggtitle("Average House Rent According to Furnishing Status") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(FURNISHING_STATUS, label = AVG),  cex = 5, vjust = 1.5)

#Analysis 2.5: How does the number of floor available affect the house rent ?
#Analysis 2.5.1: The Total Quantity of Floor Available
house_floor_available <- rent %>%
  group_by(FLOOR_AVAILABLE) %>%
  summarise(QUANTITY = n())
house_floor_available

par(mar = c(4.5, 4.5, 3, 3))
plot(house_floor_available$FLOOR_AVAILABLE, house_floor_available$QUANTITY, 
     main ="The Total Quantity of Floor Available", xlab="Floor Available", 
     ylab="Quantity", col = "#4C00FF",pch = 18, cex = 2)

#Analysis 2.5.2: Average Rent according to Floor Available
avg_rent_floor_available <- rent %>%
  group_by(FLOOR_AVAILABLE) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_floor_available$AVG <- as.integer(avg_rent_floor_available$AVG)

summary(avg_rent_floor_available)

par(mar = c(5, 5, 4, 4))
plot(avg_rent_floor_available$AVG, type = "o", xlab = "Floor Available", 
     ylab = "Average Rental", 
     main = "Average Rent according to Floor Available", col = "#7845E3",
     pch = 17, cex = 2, lwd = 1.5)
abline(v=c(11.25, 24.50, 39.75) , col="#ABD1FB")
abline(h=c(16958, 71046 , 95516, 195563, 380000) , col="#C9B6D8")

#Analysis 2.6: How does the number of total floor available affect the house rent ?
#Analysis 2.6.1: The Total Quantity of Total Floor
house_total_floor <- rent %>%
  group_by(TOTAL_FLOOR) %>%
  summarise(QUANTITY = n())
house_total_floor

par(mar = c(4.5, 4.5, 3, 3))
plot(house_total_floor$TOTAL_FLOOR, house_total_floor$QUANTITY, 
     main ="The Total Quantity of Total Floor", xlab="Total Floor", 
     ylab="Quantity", col = "#0080FF", pch = 9, cex = 2)

#Analysis 2.6.2: Average Rent according to Total Floor
avg_rent_total_floor <- rent %>%
  group_by(TOTAL_FLOOR) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_total_floor$AVG <- as.integer(avg_rent_total_floor$AVG)

summary(avg_rent_total_floor)

par(mar = c(5, 5, 4, 4))
plot(avg_rent_total_floor$AVG, type = "o", xlab = "Total Floor", 
     ylab = "Average Rental", 
     main = "Average Rent according to Total Floor", col = "#4C00FF", pch = 18, cex = 2, lwd = 1.5)
abline(v=c(16.5, 33, 51.5) , col="#F8D8FC")
abline(h=c(15533, 65286 , 82915, 140000, 380000) , col="#BED0FB")

#Analysis 2.7: How does the house size affect the house rent ?
#Analysis 2.7.1: House Size Distribution
#Bar Plot
ggplot(house_size_range, aes(x = SIZE_RANGE, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(house_size_range$SIZE_RANGE))) +
  ggtitle("House Size Distribution") + 
  theme( plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(SIZE_RANGE, label = QUANTITY), vjust = -1, position = position_dodge(width = 0.1))

#Lollipop Chart for Range Between 1000 to 10000
ggplot(house_size_range, aes(x = SIZE_RANGE, y = QUANTITY)) +
  geom_segment( aes(x = SIZE_RANGE, xend = SIZE_RANGE, y = 0, yend = QUANTITY), colour = "Black") +
  geom_point( size = 12, color = "black", 
              fill = alpha(topo.colors(length(house_size_range$SIZE_RANGE)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = QUANTITY), color = "black", size = 3.5) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("House Size Distribution")
 
#Analysis 2.7.2: the Rent Distribution in Each City According to House Size
ggplot(rent, aes(x = RENTAL, y= HOUSE_SIZE, shape = CITY, colour = CITY))+ 
  ggtitle("The Rent Distribution in Each City According to House Size") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_point(size=3, color=topo.colors(length(rent$CITY)))

#Analysis 2.7.3: Average Rent according to House Size
avg_rent_size <- rent %>%
  group_by(HOUSE_SIZE) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_size$AVG <- as.integer(avg_rent_size$AVG)

summary(avg_rent_size)

par(mar = c(5, 5, 4, 4))
plot(avg_rent_size$AVG, type = "p", xlab = "House Size", 
     ylab = "Average Rental", 
     main = "Average Rent according to House Size", 
     col = alpha("#4C00FF",0.2), pch = 16, cex = 3, lwd = 1.5)
abline(v=c(605, 1015, 1566) , col="#F8D8FC")
abline(h=c(4500, 28833, 55972, 1200000) , col="#BED0FB")

#Analysis 2.8: How does the area type affect the house rent ?
#Analysis 2.8.1: Area Type Distribution
area_type_num <- rent %>%
  group_by(AREA_TYPE) %>%
  summarise(QUANTITY = n())
area_type_num

#Bar Plot
par(mar = c(4, 4, 4, 4))
ggplot(area_type_num, aes(x = AREA_TYPE, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(area_type_num$AREA_TYPE))) +
  ggtitle("The Area Type Distribution") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(AREA_TYPE, label = QUANTITY), vjust = -1, position = position_dodge(width = 0.1))

#Lolipop Chart for Range Between 1000 to 10000
ggplot(area_type_num, aes(x = AREA_TYPE, y = QUANTITY)) +
  geom_segment( aes(x = AREA_TYPE, xend = AREA_TYPE, y = 0, yend = QUANTITY), colour = "Black") +
  geom_point( size = 12, color = "black", 
              fill = alpha(topo.colors(length(area_type_num$AREA_TYPE)), 0.3),
              alpha = 0.7, shape = 22, stroke = 1) +
  geom_text(aes(label = QUANTITY), color = "black", size = 3.5) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  ggtitle("The Area Type Distribution")

#Analysis 2.8.2: Average Rent according to Area Type
avg_rent_area <- rent %>%
  group_by(AREA_TYPE) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_area$AVG <- as.integer(avg_rent_area$AVG)

ggplot(avg_rent_area, aes(x = AREA_TYPE, y = AVG)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 60000,by = 10000)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(avg_rent_area$AREA_TYPE))) +
  ggtitle("Average Rent according to Area Type") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(AREA_TYPE, label = AVG),  cex = 5, vjust = 1.5)



#Question 3: How does the relationship between the tenant and owner affect the house rent ?
#Analysis 3.1: How does the tenant preferred affect the house rent ?
#Analysis 3.1.1: The Quantity of Each Tenant Preferred
tenant_num <- rent %>%
  group_by(TENANT_PREFERRED) %>%
  summarise(QUANTITY = n())
tenant_num

#Pie Chart
par(mar = c(2, 2, 2, 2))
pie(tenant_num$QUANTITY, paste(tenant_num$TENANT_PREFERRED,tenant_num$QUANTITY), 
    radius = 0.7, main = "The Quantity of Each Type Of Point of Contact",
    col = topo.colors(length(tenant_num$TENANT_PREFERRED)), clockwise =  TRUE)
legend("topright", tenant_num$TENANT_PREFERRED, 
       fill = topo.colors(length(tenant_num$TENANT_PREFERRED)), 
       pt.cex = 2, cex = 0.7, horiz = FALSE, inset = c( -0.3 , 0.35))

#Bar Plot
par(mar = c(4, 4, 4, 4))
ggplot(tenant_num, aes(x = TENANT_PREFERRED, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black",
           fill = topo.colors(length(tenant_num$TENANT_PREFERRED))) +
  ggtitle("The Quantity of Each Type Of Point of Contact") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(TENANT_PREFERRED, label = QUANTITY), vjust = -1, 
            position = position_dodge(width = 0.1))

#Analysis 3.1.2: Average Rent according to Tenant Preferred
avg_rent_tp <- rent %>%
  group_by(TENANT_PREFERRED) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_tp$AVG <- as.integer(avg_rent_tp$AVG)

ggplot(avg_rent_tp, aes(x = TENANT_PREFERRED, y = AVG)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 60000,by = 10000)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(avg_rent_tp$TENANT_PREFERRED))) +
  ggtitle(" Average Rent according to Tenant Preferred") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(TENANT_PREFERRED, label = AVG),  cex = 5, vjust = 1.5)

#Analysis 3.2: How does the Point of Contact affect the house rent ?
#Analysis 3.2.1: The Quantity of Each Type Of Point of Contact
contact_num <- rent %>%
  group_by(POINT_OF_CONTACT) %>%
  summarise(QUANTITY = n())
contact_num['POINT_OF_CONTACT'] = NULL
contact_num  <- cbind(POINT_OF_CONTACT, contact_num)
contact_num

#Pie Chart
par(mar = c(2, 2, 2, 2))
pie(contact_num$QUANTITY, paste(contact_num$POINT_OF_CONTACT,contact_num$QUANTITY), 
    radius = 0.7, main = "The Quantity of Each Type Of Point of Contact",
    col = topo.colors(length(contact_num$POINT_OF_CONTACT)), clockwise =  TRUE)
legend("topright", contact_num$POINT_OF_CONTACT, 
       fill = topo.colors(length(contact_num$POINT_OF_CONTACT)), 
       pt.cex = 2, cex = 0.7, horiz = FALSE, inset = c( -0.3 , 0.35))

#Bar Plot
par(mar = c(4, 4, 4, 4))
ggplot(contact_num, aes(x = POINT_OF_CONTACT, y = QUANTITY)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(contact_num$POINT_OF_CONTACT))) +
  ggtitle("The Quantity of Each Type Of Point of Contact") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(POINT_OF_CONTACT, label = QUANTITY), vjust = -1, 
            position = position_dodge(width = 0.1))

#Analysis 3.2.2: Average Rent according to Point of Contact
avg_rent_poc <- rent %>%
  group_by(POINT_OF_CONTACT) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_poc$AVG <- as.integer(avg_rent_poc$AVG)

ggplot(avg_rent_poc, aes(x = POINT_OF_CONTACT, y = AVG)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 80000,by = 10000)) + 
  geom_bar(stat = "identity", width = 0.9, color = "Black", 
           fill = topo.colors(length(avg_rent_poc$POINT_OF_CONTACT))) +
  ggtitle("Average Rent according to Point of Contact") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_text(aes(POINT_OF_CONTACT, label = AVG),  cex = 5, vjust = 1.5)



#Question 4: What is the best day to rent a cheaper house ?
#Analysis 4.1: How does the day of house rent posted affect the house rent ?
#Analysis 4.1.1: The Quantity House Rent Post on Each Day 
date_num <- rent %>%
  group_by(DAY) %>%
  summarise(QUANTITY = n())
date_num

summary(date_num)

ggplot(date_num, aes(x = DAY, y = QUANTITY)) +
  geom_line( color= "#69b3a2", linewidth = 2, alpha = 0.9) + 
  scale_x_continuous(breaks = seq(from = 1, to = 31,  by = 1)) +
  scale_y_continuous(breaks = seq(from = 0, to = 500,  by = 50)) +
  ggtitle("The Quantity House Rent Post on Each Day") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

#Analysis 4.1.2: Average Rent according to Day
avg_rent_day <- rent %>%
    group_by(DAY) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_day$AVG <- as.integer(avg_rent_day$AVG)

ggplot(avg_rent_day, aes(x = DAY, y = AVG)) +
  geom_line( color= "#D27AE7", linewidth = 1.5, alpha = 0.9) + 
  scale_x_continuous(breaks = seq(from = 1, to = 31,  by = 1)) +
  scale_y_continuous(breaks = seq(from = 15000, to = 80000,  by = 4000)) +
  ggtitle("The Average Rent according to Day") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_hline(yintercept = mean(avg_rent_day$AVG, na.rm=TRUE), color = "#B4DCDC", lwd = 1, lty = 2) 

#Analysis 4.2: How does the month of house rent posted affect the house rent ?
#Analysis 4.2.1: The Quantity House Rent Post on Each Month
month_num <- rent %>%
  group_by(MONTH) %>%
  summarise(QUANTITY = n())
month_num

summary(month_num)

ggplot(month_num, aes(x = MONTH, y = QUANTITY)) +
  geom_line( color= "#69b3a2", linewidth = 2, alpha = 0.9) + 
  scale_x_continuous(breaks = seq(from = 0, to = 12,  by = 1)) +
  scale_y_continuous(breaks = seq(from = 0, to = 1900,  by = 100)) +
  ggtitle("The Quantity House Rent Post on Each Month") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

#Analysis 4.2.2: Average Rent according to Month
avg_rent_month <- rent %>%
  group_by(MONTH) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_month$AVG <- as.integer(avg_rent_month$AVG)

ggplot(avg_rent_month, aes(x = MONTH, y = AVG)) +
  geom_line( color= "#D27AE7", linewidth = 1.5, alpha = 0.9) + 
  scale_x_continuous(breaks = seq(from = 1, to = 31,  by = 1)) +
  scale_y_continuous(breaks = seq(from = 15000, to = 80000,  by = 4000)) +
  ggtitle("The Average Rent according to Month") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  geom_hline(yintercept = mean(avg_rent_month$AVG, na.rm=TRUE), 
             color = "#B4DCDC", lwd = 1, lty = 2) +
  geom_label(data = avg_rent_month, aes(x = MONTH, y = AVG, label = AVG),
             color= "#62AAC5", 
             size = 3.5, angle = 45, fontface = "bold")

#Analysis 4.3: How does the date of house rent posted affect the house rent ?
#Analysis 4.3.1: Which Date has the most house rent posted ?
date_num <- rent %>%
  group_by(DATE) %>%
  summarise(QUANTITY = n())
date_num

summary(date_num)

par(mar = c(4, 4, 4, 4))
ggplot(date_num, aes(x = DATE, y = QUANTITY)) +
  geom_line( color= "#69b3a2", linewidth = 0.5, alpha = 0.9) + 
  scale_y_continuous(breaks = seq(from = 0, to = 350,  by = 50)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
  ggtitle("The Quantity House Rent Post on Each Date") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  theme(axis.text.x=element_text(size = 7, angle = 60, hjust = 1)) +
  geom_hline(yintercept = mean(date_num$QUANTITY, na.rm=TRUE), 
             color = "#B4DCDC", lwd = 1, lty = 2) +
  geom_point( size = 2, color = "black", fill = alpha(topo.colors(length(date_num$DATE)), 0.3),
              alpha = 0.7, shape = 21, stroke = 1)

#Analysis 4.3.2: The Date with the Lowest Average Rent
avg_rent_date <- rent %>%
  group_by(DATE) %>%
  summarise(AVG = format(round(mean(RENTAL),1), nsmall = 1))
avg_rent_date <- arrange(avg_rent_date, AVG)
View(avg_rent_date)
