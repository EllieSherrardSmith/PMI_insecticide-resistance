
############################################################
##
##  Converting country level data to formatted tables
##  Version 1
##  Author: Ellie Sherrard-Smith
##  Date: 12 March 2016
##
############################################################
library(lubridate)


########################################################
##  Function to clean and rearrange the data by
##     country
##     district
##     site
########################################################


IR_data <- function(data,country_name_districts,country_name_sites,country_name_countries) {
  
  x <- data$"Test.Completion.Date"
  data$date <- dmy(x)                             ## Convert the data into a date and yearly format from which you can work
  data$year <- substr(data$date, 1, 4)            ## for years
  data$month <- substr(data$date, 6, 7)           ## for months
  data$days <- yday(data$date) - 1                ## for days of the year
  
  data$Total.Number.of.Dead.Mosq.in.Control[is.na(data$Total.Number.of.Dead.Mosq.in.Control)] <- 0
  data$mortality = (((data$Total.Mosq.Dead.24hr) - 
                       (data$Total.Number.of.Dead.Mosq.in.Control))/
                      (100 - data$Total.Number.of.Dead.Mosq.in.Control)) * 100  
  ## Calculate the Mortality rate corrected for controls where they were completed
  ## This calculation is taken from WHO/CDS/NTF/WHOPES/GCDPP/2006.3 "Guidelines for 
  ## testing adulticides for indoor residual spraying and treatment of mosquito nets"
  
  ## SUMMARISE BY DISTRICT FOR
  ## 1. Anopheles gambiae s.l.
  ## 2. Anopheles funestus
  ## 3. other species
  
  an_1 <- subset(data, data$Species == unique(data$Species)[1])

  ## This bit is to create data frames (tables) into which we can store all the info we want
  mn_district      <- tapply(an_1$mortality[an_1$year == unique(an_1$year)[1]],
                             an_1$District[an_1$year == unique(an_1$year)[1]],mean,na.rm=TRUE)
  summary_districts <- summary_districts_N <- summary_districts_MIN <- summary_districts_MAX <- data.frame(template=names(mn_district),mean=mn_district)
  
  mn_sites      <- tapply(an_1$mortality[an_1$year == unique(an_1$year)[1]],
                          an_1$Collection.Site[an_1$year == unique(an_1$year)[1]],mean,na.rm=TRUE)
  summary_sites <- summary_sites_N <- summary_sites_MIN <- summary_sites_MAX <- data.frame(template=names(mn_sites),mean=mn_sites)
  
  mn_country      <- tapply(an_1$mortality[an_1$year == unique(an_1$year)[1]],
                            an_1$Country[an_1$year == unique(an_1$year)[1]],mean,na.rm=TRUE)
  summary_country <- summary_country_N <- summary_country_MIN <- summary_country_MAX <- data.frame(template=names(mn_country),mean=mn_country)
  
  
  for(i in 1:length(unique(an_1$year))){
    ## provides the mean mortality estimate for district level data for each year separately
    summary_districts[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                                   an_1$District[an_1$year == unique(an_1$year)[i]],mean,na.rm=TRUE))
    colnames(summary_districts)[i] <- paste("Mortality rate %",unique(an_1$year)[i],sep=" ")
    
    ## provides the mean number of mosquitoes tested for district level data for each year separately
    summary_districts_N[,i] <-  as.data.frame(tapply(an_1$Total.Number.of.Mosquitoes.Tested[an_1$year == unique(an_1$year)[i]],
                                                     an_1$District[an_1$year == unique(an_1$year)[i]],mean,na.rm=TRUE))
    colnames(summary_districts_N)[i] <- paste("N tested",unique(an_1$year)[i],sep=" ")
    
    ## provides the minimum mortality estimate across district level data for each year separately
    summary_districts_MIN[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                                       an_1$District[an_1$year == unique(an_1$year)[i]],min,na.rm=TRUE))
    colnames(summary_districts_MIN)[i] <- paste("Mortality rate % minimum",unique(an_1$year)[i],sep=" ")
    
    ## provides the maximum mortality estimate across district level data for each year separately
    summary_districts_MAX[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                                       an_1$District[an_1$year == unique(an_1$year)[i]],max,na.rm=TRUE))
    colnames(summary_districts_MAX)[i] <- paste("Mortality rate % maximum",unique(an_1$year)[i],sep=" ")
    
    
    ## Now repeated for site level
    ## provides the mean mortality estimate for district level data for each year separately
    summary_sites[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                               an_1$Collection.Site[an_1$year == unique(an_1$year)[i]],mean,na.rm=TRUE))
    colnames(summary_sites)[i] <- paste("Mortality rate %",unique(an_1$year)[i],sep=" ")
    
    ## provides the mean number of mosquitoes tested for district level data for each year separately
    summary_sites_N[,i] <-  as.data.frame(tapply(an_1$Total.Number.of.Mosquitoes.Tested[an_1$year == unique(an_1$year)[i]],
                                                 an_1$Collection.Site[an_1$year == unique(an_1$year)[i]],mean,na.rm=TRUE))
    colnames(summary_sites_N)[i] <- paste("N tested",unique(an_1$year)[i],sep=" ")
    
    ## provides the minimum mortality estimate across district level data for each year separately
    summary_sites_MIN[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                                   an_1$Collection.Site[an_1$year == unique(an_1$year)[i]],min,na.rm=TRUE))
    colnames(summary_sites_MIN)[i] <- paste("Mortality rate % minimum",unique(an_1$year)[i],sep=" ")
    
    ## provides the maximum mortality estimate across district level data for each year separately
    summary_sites_MAX[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                                   an_1$Collection.Site[an_1$year == unique(an_1$year)[i]],max,na.rm=TRUE))
    colnames(summary_sites_MAX)[i] <- paste("Mortality rate % maximum",unique(an_1$year)[i],sep=" ")
    
    
    
    ## Now repeated for country level
    ## provides the mean mortality estimate for district level data for each year separately
    summary_country[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                                 an_1$Country[an_1$year == unique(an_1$year)[i]],mean,na.rm=TRUE))
    colnames(summary_country)[i] <- paste("Mortality rate %",unique(an_1$year)[i],sep=" ")
    
    ## provides the mean number of mosquitoes tested for district level data for each year separately
    summary_country_N[,i] <-  as.data.frame(tapply(an_1$Total.Number.of.Mosquitoes.Tested[an_1$year == unique(an_1$year)[i]],
                                                   an_1$Country[an_1$year == unique(an_1$year)[i]],mean,na.rm=TRUE))
    colnames(summary_country_N)[i] <- paste("N tested",unique(an_1$year)[i],sep=" ")
    
    ## provides the minimum mortality estimate across district level data for each year separately
    summary_country_MIN[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                                     an_1$Country[an_1$year == unique(an_1$year)[i]],min,na.rm=TRUE))
    colnames(summary_country_MIN)[i] <- paste("Mortality rate % minimum",unique(an_1$year)[i],sep=" ")
    
    ## provides the maximum mortality estimate across district level data for each year separately
    summary_country_MAX[,i] <-  as.data.frame(tapply(an_1$mortality[an_1$year == unique(an_1$year)[i]],
                                                     an_1$Country[an_1$year == unique(an_1$year)[i]],max,na.rm=TRUE))
    colnames(summary_country_MAX)[i] <- paste("Mortality rate % maximum",unique(an_1$year)[i],sep=" ")
    
  }                                              
  
  data_prepped_districts_An_1 <- cbind(summary_districts,summary_districts_N,summary_districts_MIN,summary_districts_MAX)
  data_prepped_sites_An_1 <- cbind(summary_sites,summary_sites_N,summary_sites_MIN,summary_sites_MAX)
  data_prepped_country_An_1 <- cbind(summary_country,summary_country_N,summary_country_MIN,summary_country_MAX)
  
  FILE_NAME_D <-  paste(country_name_districts, ".csv", sep="")
  FILE_NAME_S <-  paste(country_name_sites, ".csv", sep="")
  FILE_NAME_C <-  paste(country_name_countries, ".csv", sep="")
  
  write.csv(data_prepped_districts_An_1,file = FILE_NAME_D)
  write.csv(data_prepped_sites_An_1,file = FILE_NAME_S)
  write.csv(data_prepped_country_An_1,file = FILE_NAME_C)
  ##saves the data wherever you define
  #return(c(data_prepped_districts,
  #         data_prepped_sites,
  #         data_prepped_country))              ##prints the data so you can double check it
}


########################################################
##  Data file
##
########################################################

## Madagascar has only got gambiae reported
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\PMI Entomology Data WHO tube test Madagascar.csv",header=TRUE)
###*** Change the name to reflect the conntry and where you wish to save the files ***###

IR_data(data,country_name_districts = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\districts_WHO tube test_An_1_Madagascar",
        country_name_sites = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\sites_WHO tube test_An_1_Madagascar",
        country_name_countries = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\countries_WHO tube test_An_1_Madagascar")

## Ghana has only got gambiae reported
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\PMI Entomology Data WHO tube test Ghana.csv",header=TRUE)
###*** Change the name to reflect the conntry and where you wish to save the files ***###

IR_data(data,country_name_districts = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\districts_WHO tube test_An_1_Ghana",
        country_name_sites = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\sites_WHO tube test_An_1_Ghana",
        country_name_countries = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\countries_WHO tube test_An_1_Ghana")

## Mali has only got gambiae reported
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\PMI Entomology Data WHO tube test Mali.csv",header=TRUE)
###*** Change the name to reflect the conntry and where you wish to save the files ***###

IR_data(data,country_name_districts = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\districts_WHO tube test_An_1_Mali",
        country_name_sites = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\sites_WHO tube test_An_1_Mali",
        country_name_countries = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\countries_WHO tube test_An_1_Mali")

## Mozambique has got gambiae and funestus so change 1 to 2 in the function to get funestus reported
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\PMI Entomology Data WHO tube test Mozambique.csv",header=TRUE)
###*** Change the name to reflect the conntry and where you wish to save the files ***###

IR_data(data,country_name_districts = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\districts_WHO tube test_An_2_Mozambique",
        country_name_sites = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\sites_WHO tube test_An_2_Mozambique",
        country_name_countries = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\countries_WHO tube test_An_2_Mozambique")

## Ethiopia has only got gambiae reported
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\PMI Entomology Data WHO tube test Ethiopia.csv",header=TRUE)
###*** Change the name to reflect the conntry and where you wish to save the files ***###

IR_data(data,country_name_districts = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\districts_WHO tube test_An_1_Ethiopia",
        country_name_sites = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\sites_WHO tube test_An_1_Ethiopia",
        country_name_countries = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\countries_WHO tube test_An_1_Ethiopia")
