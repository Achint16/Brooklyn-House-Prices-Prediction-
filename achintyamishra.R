
#Statistical Analysis Final Assignment
#Work_Done_by: Achintya Mishra
#Work_Submmited_to: Prof. Jonathan Williams
#Date: 8-December-2023

#-------------------------------------------------------------------------------

#Importing Libraries 

library(dplyr)
library(tidyverse)
library(lubridate)

#Importing Data 

data1 <- read.csv('/Users/achintya16/Downloads/2016_brooklyn.csv')
data2 <- read.csv('/Users/achintya16/Downloads/2017_brooklyn.csv')
data3 <- read.csv('/Users/achintya16/Downloads/2018_brooklyn.csv')
data4 <- read.csv('/Users/achintya16/Downloads/2019_brooklyn.csv')
data5 <- read.csv('/Users/achintya16/Downloads/2020_brooklyn.csv')

#Mapping for Standardization

colnames(data1) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 

colnames(data2) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 

colnames(data3) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 

colnames(data4) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 

colnames(data5) = c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 

#---------------------------------------------------------------------------------------------------------------
# Data Cleaning 

#glimpse(complete_data)

# Parsing the date column and spliting it into year month and day 
# This code may not run if you run it as a whole, you may have to run is individually. 
# After splitting dates and combining it to complete_data. It runs smoothly. 

  data1$date <- mdy(data1$date)
    data1$year <- year(data1$date)
    data1$month <- month(data1$date)
    data1$day <- day(data1$date)
  
  data2$date <- mdy(data2$date)
    data2$year <- year(data2$date)
    data2$month <- month(data2$date)
    data2$day <- day(data2$date)
    
  data3$date <- mdy(data3$date)
    data3$year <- year(data3$date)
    data3$month <- month(data3$date)
    data3$day <- day(data3$date)
    
  data4$date <- mdy(data4$date)
    data4$year <- year(data4$date)
    data4$month <- month(data4$date)
    data4$day <- day(data4$date)
    
  data5$date <- mdy(data5$date)
    data5$year <- year(data5$date)
    data5$month <- month(data5$date)
    data5$day <- day(data5$date)

# Removing na or null values 

  #data1 <- na.omit(data1)
  #data2 <- na.omit(data2)
  #data3 <- na.omit(data3)
  #data4 <- na.omit(data4)
  #data5 <- na.omit(data5)
    

# Removing First 4 rows from Data1,2,3,4 and First 7 rows from Data5
  data1 <- data1 %>%
    slice(-(1:4))
  data2 <- data2 %>%
    slice(-(1:4))
  data3 <- data3 %>%
    slice(-(1:4))
  data4 <- data4 %>%
    slice(-(1:4))
  data5 <- data5 %>%
    slice(-(1:7))

#Combining Data 

library(dplyr)

complete_data <- bind_rows(data1, data2, data3, data4, data5)
  
filtered_data <- complete_data

#nrow(filtered_data)

#---------------------------------------------------------------------------------
#Dropping Ir-relevent Columns 
 
filtered_data$easement <- NULL
  
filtered_data$comunits <- NULL
  
filtered_data$date <- NULL
  
filtered_data$borough <- NULL
  
#summary(filtered_data)
  
#-----------------------------------------------------------------------------------------------------  
#Cleaning Price  
  
#na_check_vector <- is.na(as.numeric(filtered_data$price))
#print(unique(filtered_data$price[na_check_vector]))
  
filtered_data$price <- gsub("[[:punct:]]", "", filtered_data$price)
#nrow(filtered_data)
  
filtered_data<- filtered_data[filtered_data$price != "    ", ]
#nrow(filtered_data)
  
filtered_data$price <- as.numeric(filtered_data$price)
  
filtered_data<- filtered_data[filtered_data$price > 0, ]
#nrow(filtered_data)
  
  
#summary(filtered_data$price)
  
#unique(filtered_data$price)
#--------------------------------------------------------------------------------- 
#Cleaning Resunits
    
#Replacing - and missing vaules in reunits with 0 
    
filtered_data$resunits <- gsub("[[:punct:]]", "", filtered_data$resunits)
      
filtered_data <- filtered_data %>% 
  mutate(resunits = ifelse(resunits == '', 0, resunits))
  
filtered_data <- filtered_data %>% 
  mutate(resunits = ifelse(resunits == '    ', 0, resunits))
      
filtered_data$resunits <- as.numeric(filtered_data$resunits) 
  
filtered_data<- filtered_data[filtered_data$resunits == 1, ]
  
#summary(filtered_data$resunits)
#----------------------------------------------------------------------------------
#Cleaning Grossqft Units 
    
# na_check_vector <- is.na(as.numeric(filtered_data$grosssqft))
# print(unique(filtered_data$grosssqft[na_check_vector]))
  
filtered_data$grosssqft <- gsub("[[:punct:]]", "", filtered_data$grosssqft)
  
filtered_data<- filtered_data[filtered_data$grosssqft != "    ", ]
  
filtered_data$grosssqft <- as.numeric(filtered_data$grosssqft)
 
filtered_data<- filtered_data[filtered_data$grosssqft != 0, ]
  
#filtered_data <- filtered_data %>%
  #filter(price <= 1.5e+07)
  
   
#unique(filtered_data$grosssqft)  
  
#summary(filtered_data$grosssqft)
#-----------------------------------------------------------------------------------
#Cleaning Total Units 
    
#na_check_vector <- is.na(as.numeric(filtered_data$totunits))
#print(unique(filtered_data$totunits[na_check_vector]))
    
  filtered_data$totunits <- gsub("[[:punct:]]", "", filtered_data$totunits)
  
  filtered_data$totunits <- as.numeric(filtered_data$totunits)
  
  filtered_data<- filtered_data[filtered_data$totunits == 1, ]
  
#unique(filtered_data$totunits)  
  
#summary(filtered_data$totunits)

#---------------------------------------------------------------------------------------  
 #BBldclass at the time of Sale 
    
filtered_data$bldclasssale <- trimws(filtered_data$bldclasssale) #Trimming White Spaces

filtered_data <- filtered_data[grep("^[AR]", filtered_data$bldclasssale), ] #Filtering Building having Classes A & R 
    
    
#summary(filtered_data$bldclasscurr)
#unique(filtered_data$bldclasssale)
#--------------------------------------------------------------------------------------------    
#Additional Filters 
      
filtered_data$neighborhood <- trimws(filtered_data$neighborhood) #Trimming White spaces
    
filtered_data$taxclasssale<- as.numeric(filtered_data$taxclasssale)
    
filtered_data <- filtered_data %>%
  filter(grosssqft <= 8000)

#----------------------------------------------------------------------------------  
                            # Exploratory Data Analysis 
# Creating new data frame 
  df <- filtered_data
  
  df <- df %>%
    filter(price >= 10000 & price < 5000000)
      
# Arranging the data frame in ascending order based on price
  df <- df[order(df$price), ]

# Plotting scatter plot to find outliers and Structure of model  
  ggplot(df, aes(x = grosssqft, y = price)) +
    geom_point() +
    labs(title = "Scatter Plot of Price against Gross Square Feet", x = "Gross Square Feet", y = "Price")
 
# Visualizing Density 
  ggplot(df, aes(x = df$price)) +
    geom_density(fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Density Plot of Price", x = "Price", y = "Density")
#-------------------------------------------------------------------------------------------------------  
# Feature Selection 
    
#install.packages("caret")
#library(caret)
    
#ctrl <- rfeControl(functions = lmFuncs, method = "cv", number = 10)
#result <- rfe(numerical_variables[, -1], numerical_variables, sizes = c(1, 2, 3), rfeControl = ctrl)
  
#Print the results
#result
  
#Selected Features 
#The top 5 numerical variables (out of 12): price, taxclasssale, year, month, grossqft
#correlation_matrix <- cor(numerical_variables)
  
#Extracting correlations with respect to 'price'
#correlation_with_price <- correlation_matrix[,"price"]
  
#Sort columns based on absolute correlation values
#sorted_columns <- names(sort(abs(correlation_with_price), decreasing = TRUE))
  
#Display the most relevant columns
#print(sorted_columns)
#---------------------------------------------------------------------------------------------------
# Bins for ZipCode
  North <- c(11211, 11222, 11249)   # North
  East <- c( 11212, 11236, 11207, 11208)  # East
  South <- c(11234, 11224, 11223, 11232, 11209, 11214, 11228, 11204, 11203,11219, 11235,11210, 11230,11229)  # South 
  Central <- c(11201, 11231, 11211, 11237, 11206, 11217, 11220, 11239,  11205, 11216, 11218, 11215, 11221, 11226,11225, 11213, 11233, 11238)   # Central
  
  df$zip[df$zip %in% North] <- "northern_brooklyn"
  df$zip[df$zip %in% South] <- "southern_brooklyn"
  df$zip[df$zip %in% East] <- "eastern_brooklyn"
  df$zip[df$zip %in% Central] <- "central_brooklyn"
  
  
  
# Bins for Neighborhood
  
  neighborhoods <- c("BATH BEACH", "BAY RIDGE", "BEDFORD STUYVESANT", "BENSONHURST", "BERGEN BEACH", "BOERUM HILL",
                     "BOROUGH PARK", "BRIGHTON BEACH", "BROOKLYN HEIGHTS", "BROWNSVILLE", "BUSHWICK", "CANARSIE",
                     "CARROLL GARDENS", "CLINTON HILL", "COBBLE HILL", "COBBLE HILL-WEST", "CONEY ISLAND", "CROWN HEIGHTS",
                     "CYPRESS HILLS", "DOWNTOWN-FULTON MALL", "DYKER HEIGHTS", "EAST NEW YORK", "FLATBUSH-CENTRAL",
                     "FLATBUSH-EAST", "FLATBUSH-LEFFERTS GARDEN", "FLATBUSH-NORTH", "FLATLANDS", "FORT GREENE",
                     "GERRITSEN BEACH", "GOWANUS", "GRAVESEND", "GREENPOINT", "KENSINGTON", "MADISON", "MANHATTAN BEACH",
                     "MARINE PARK", "MIDWOOD", "MILL BASIN", "NAVY YARD", "OCEAN HILL", "OCEAN PARKWAY-NORTH",
                     "OCEAN PARKWAY-SOUTH", "OLD MILL BASIN", "PARK SLOPE", "PARK SLOPE SOUTH", "PROSPECT HEIGHTS",
                     "RED HOOK", "SEAGATE", "SHEEPSHEAD BAY", "SUNSET PARK", "WILLIAMSBURG-EAST", "WILLIAMSBURG-NORTH",
                     "WILLIAMSBURG-SOUTH", "WINDSOR TERRACE", "WYCKOFF HEIGHTS", "SPRING CREEK", "BUSH TERMINAL",
                     "DOWNTOWN-FULTON FERRY", "DOWNTOWN-METROTECH", "WILLIAMSBURG-CENTRAL")
  
  # Define regions
  southern_brooklyn <- c("BATH BEACH", "BENSONHURST", "BERGEN BEACH", "BOROUGH PARK", "BRIGHTON BEACH", "CANARSIE",
                         "DOWNTOWN-FULTON MALL", "DYKER HEIGHTS", "FLATBUSH-CENTRAL", "FLATBUSH-EAST", "FLATBUSH-LEFFERTS GARDEN",
                         "FLATBUSH-NORTH", "FLATLANDS", "GERRITSEN BEACH", "GRAVESEND", "MADISON", "MANHATTAN BEACH", "MARINE PARK",
                         "MIDWOOD", "MILL BASIN", "OCEAN HILL", "OCEAN PARKWAY-NORTH", "OCEAN PARKWAY-SOUTH", "OLD MILL BASIN",
                         "SHEEPSHEAD BAY", "CONEY ISLAND")
  
  central_brooklyn <- c("BEDFORD STUYVESANT", "BOERUM HILL", "BROOKLYN HEIGHTS", "CARROLL GARDENS", "CLINTON HILL", "COBBLE HILL",
                        "COBBLE HILL-WEST", "CROWN HEIGHTS", "DOWNTOWN-FULTON FERRY", "DOWNTOWN-METROTECH", "KENSINGTON", "PARK SLOPE",
                        "PARK SLOPE SOUTH", "PROSPECT HEIGHTS", "WINDSOR TERRACE")
  
  northern_brooklyn <- c("BAY RIDGE", "BUSHWICK", "DOWNTOWN-FULTON MALL", "DOWNTOWN-METROTECH", "DOWNTOWN-FULTON FERRY",
                         "WILLIAMSBURG-EAST", "WILLIAMSBURG-NORTH", "WILLIAMSBURG-SOUTH", "WILLIAMSBURG-CENTRAL", "GREENPOINT", "RED HOOK",
                         "WYCKOFF HEIGHTS", "FORT GREENE")
  
  eastern_brooklyn <- c("CYPRESS HILLS", "EAST NEW YORK", "GOWANUS", "NAVY YARD", "SPRING CREEK", "BUSH TERMINAL", "SUNSET PARK", "SEAGATE", "BROWNSVILLE")
  
  df$neighborhood[df$neighborhood %in% northern_brooklyn] <- "northern_brooklyn"
  df$neighborhood[df$neighborhood %in% southern_brooklyn] <- "southern_brooklyn"
  df$neighborhood[df$neighborhood %in% eastern_brooklyn] <- "eastern_brooklyn"
  df$neighborhood[df$neighborhood %in% central_brooklyn] <- "central_brooklyn"
#-------------------------------------------------------------------------------------------------------- 
# Creating quarters
    
  df$quarter <- cut(df$month, breaks = c(0, 3, 6, 9, 12), labels = c("Q1", "Q2", "Q3", "Q4"))
  
  date_data <- data.frame(
    year = c(2016, 2017, 2018, 2019, 2020),
    month = c(1, 4, 7, 10, 12)
  )
  
#--------------------------------------------------------------------------------------------------------  
# Selecting all the numerical values and Categorical values  
  
#data_model<- df
  
#numerical_variables <- df %>% select_if(is.numeric)
  
#factors <- df %>% select(where(is.factor))
  
#char <- df %>% select(where(is.character))
#-----------------------------------------------------------------------------------------------------
# Dropping Ir-relevant Columns 
  df$taxclasscurr <- NULL
  df$address <- NULL
  df$aptnum <- NULL
  df$resunits <- NULL
  df$bldclasscurr <- NULL
  df$totunits <- NULL
  df$day <- NULL
  
    
# Conversion of Units 
  df$landsqft <- gsub("[[:punct:]]", "", df$landsqft)
  df$landsqft <- as.numeric(df$landsqft)
  df$bldclasssale <- as.factor(df$bldclasssale)
  df$neighborhood <- as.factor(df$neighborhood)
  df$block <- as.numeric(df$block)
  df$lot <- as.numeric(df$lot)
  df$quarter <- as.numeric(df$quarter)
  
#---------------------------------------------------------------------------------------------------------  
# linear regression model

#model1 <- lm(price ~ factor(zip) * sqrt(grosssqft) + factor(df$neighborhood) + block + lot + quarter , data = df )
    
#summary(model1)          #R^2 : 0.5015 
                          #RMSE: 474800 
                          #DoF: 13
#---------------------------------------------------------    
#model2 <- lm(price ~ factor(zip) * block + neighborhood * sqrt(grosssqft)  + quarter+ bldclasssale + lot , data = df )

#summary(model2)      #R^2 :  0.5281
                      #RMSE : 462200
                      #DoF: 30
#------------------------------------------------    
#model3 <- lm(price ~ factor(zip) * sqrt(grosssqft) + factor(df$neighborhood) * sqrt(landsqft) + block * lot * sqrt(grosssqft) + quarter + bldclasssale, data = df )

#summary(model3)      #R^2: 0.547
                      #RMSE: 453000
                      #DoF: 35

#------------------------------------
#model4 <- lm(price ~ factor(zip) + sqrt(grosssqft) + factor(df$neighborhood)+ yrbuilt + sqrt(landsqft) + block  * sqrt(grosssqft) * sqrt(landsqft) + (quarter * year) + bldclasssale, data = df )

#summary(model4)
                      #R^2: 0.5521
                      #RMSE: 450400
                      #DoF: 31
#-------------------------------------
#model5 <- lm(price ~ factor(zip) + sqrt(grosssqft) * factor(df$neighborhood) + sqrt(landsqft) * zip + block  * sqrt(grosssqft) * sqrt(landsqft) + (quarter * year) + bldclasssale + zip * factor(neighborhood)  , data = df )

#summary(model5)
                      #R^2: 0.5564
                      #RMSE: 448400
                      #DoF: 40
#--------------------------------------
#FINAL BEST MODEL STAISFYING ALL CONDITIONS 
#After trying many models with different combinations. I tried buketing blocks which gave me the model satisfying all conditions
    
df$blockc <- cut(df$block,breaks = 7)
  
Final_model <- lm(price ~ factor(zip) * sqrt(grosssqft) + factor(df$neighborhood) + sqrt(landsqft) + blockc  * sqrt(grosssqft) + bldclasssale+quarter*year, data = df )

summary(Final_model)

rmse <- sqrt(mean((df$price - predict(Final_model, newdata=df))^2))     #R^2: 0.6102
                                                                        #RMSE: 419361.6
rmse                                                                    #DoF: 40
#-------------------------------------------------------------------------
# Residual analysis and Breusch-Pagan test
  
#install.packages("lmtest")
#library(lmtest)

#Run Breusch-Pagan test
  #bptest(Final_model)
  
#Obtain Residuals
  #residuals <- residuals(Final_model)
  
#Residual plot against predicted values
  #plot(fitted(Final_model), residuals, main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals")
  
#-------------------------------------------------------------------
# Final Assignment Part-2 

# Changing our Data Frame to Final_model_2   
#Final_model_2 <-df

# Creating another with column
#Final_model_2 <- Final_model_2 %>%
#  mutate(year_quarter = paste0(year, quarter))

#Final_model_2 <- Final_model_2 %>%
#  mutate(year_quarter = if_else(year_quarter %in% c("20203", "20204"), year_quarter, "other_value"))

  
#Final_model_2 <- lm(price ~ factor(zip) * sqrt(grosssqft) + factor(df$neighborhood) + sqrt(landsqft) + blockc  * sqrt(grosssqft) + bldclasssale+ year_quarter, data = Final_model_2 )

#summary(Final_model_2)

# year_quarter20204                               78681.4   
# Prices go up when going from Q3 to Q4 78681.4

#--------------------------------------------------------------------
# Saving File
  
saveRDS(list(model=Final_model, data=df), file='achintyamishra.RDS')
