
############################################################
##
##  Converting country level data to formatted tables
##  Version 1
##  Author: Ellie Sherrard-Smith
##  Date: 12 March 2016
##
############################################################
library(lubridate)

## TASK 1
## To convert countries raw data from Abt Associates into summary format 
## for IRS insecticide resistance tables for PMI


data_file <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\PMI Entomology Data WHO tube test Ethiopia.csv",header=TRUE)


IR_data <- function(data,country_name_districts,country_name_sites,country_name_countries) {
  
  x <- data$"Test.Completion.Date"
  data$date <- dmy(x)                             ## Convert the data into a date and yearly format from which you can work
  data$year <- substr(data$date, 1, 4)            ## for years
  data$month <- substr(data$date, 6, 7)           ## for months
  data$days <- yday(data$date) - 1                ## for days of the year
                                    
  data$Total.Number.of.Dead.Mosq.in.Control[is.na(data$Total.Number.of.Dead.Mosq.in.Control)] <- 0
  data$mortality = (((data$Total.number.of.exposed.Mosq.Dead.after.24hrs) - 
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

IR_data(data_file,country_name_districts = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\districts_WHO tube test_An_1_Ethiopia",
        country_name_sites = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\sites_WHO tube test_An_1_Ethiopia",
        country_name_countries = "C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Data_prepped WHO tube test\\countries_WHO tube test_An_1_Ethiopia")


## TASK 2
## To convert countries raw data from Abt Associates into summary data 
## on wall cone bioassays - decay rates

data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Ethiopia.csv",header=TRUE)
names(data)

##**** Pre function checks 
##  1. Confirm that the place names are uniform

## Sort the data.frame by the names column
data <- data[order(data$Site_District),]  

## Take a look 
data$Site_District
## Rename the mis-spelt columns as appropriate
data$Site_District_corrected <- as.factor(c(rep("Adama Zuria", 82),
                                  rep("Bako Tibe",79),
                                  rep("Chewaka",10),
                                  rep("Gobu Sayo",176),
                                  rep("Hawa Gelan",10),
                                  rep("Kersa",70),
                                  rep("Manasibu",10),
                                  rep("Omonada",10),
                                  rep("Sasiga",10),
                                  rep("Seka Chokorsa",12)))

summary(data$Site_District_corrected)

## Now repeat the above for the test sites
data <- data[order(data$Test.Site),]

levels(data$Test.Site)
levels(data$Test.Site)[5] <- "Gambell Tere"
levels(data$Test.Site)[5] <- "Gambell Tere"
levels(data$Test.Site)[5] <- "Gambell Tere"

levels(data$Test.Site)[6] <- "Kela Dabus"

levels(data$Test.Site)[7] <- "Lemle"
levels(data$Test.Site)[8] <- "Lemle"

levels(data$Test.Site)[9] <- "Tulu Sangota"

## 2. Confirm the dates are sensible i.e. the test date follows the spray date

data$date_sprayed <- dmy(data$"Spraying.Date")    ## Convert the data into a date and yearly format from which you can work
data$date_tested <- dmy(data$"Date.of.Test")
data$diff_in_days<- round(difftime(data$date_tested, data$date_sprayed, units = c("days"))) ## check in case of mistakes in data entry

data <- data[order(data$diff_in_days),]

## Check the data and correct as necessary
head(data[18:25,36:38])

data$date_sprayed[1:2] <- "2013-08-17 UTC"
data$date_tested[3:7] <- "2014-01-21"
data$date_tested[8:17] <- "2014-01-21"
data$date_tested[18:22] <- "2014-01-27"

data$diff_in_days<- round(difftime(data$date_tested, data$date_sprayed, units = c("days"))) ## check in case of mistakes in data entry

## 3. Species names
data$Species_corrected <- data$Species
levels(data$Species_corrected)[2] <- "An. arabiensis"

## 4. Wall type names
data$Wall_surface_corrected <- data$Wall_Surface.Type 
levels(data$Wall_surface_corrected)[4] <- "Mud"
levels(data$Wall_surface_corrected)[5] <- "Mud"
levels(data$Wall_surface_corrected)[5] <- "Mud"

summary(data$Wall_surface_corrected)

## 5. Insecticide
levels(data$Insecticide.Sprayed)[4] <- "Local water"
levels(data$Insecticide.Sprayed)[5] <- "Local water"

summary(data$Insecticide.Sprayed)

data_checked <- data
## Now can proceed with the analysis with corrected data
names(data_checked)

## For each species, by wall type, place and insecticide

  data_checked$"Number.Control.Mosquitos.Used"[is.na(data_checked$"Number.Control.Mosquitos.Used")] <- 0
  data_checked$"Number.Control.Dead.after.24.Hrs"[is.na(data_checked$"Number.Control.Dead.after.24.Hrs")] <- 0
  control <- (data_checked$"Number.Control.Dead.after.24.Hrs" / ifelse(data_checked$"Number.Control.Mosquitos.Used" > 0,
                                                                       data_checked$"Number.Control.Mosquitos.Used",1))
  treated <- (data_checked$"Total.Number.Test.Mosquitoes.Dead.after.24.Hrs"/data_checked$"Total.Number.of.Mosquitoes.Tested")
  data_checked$mortality = ((treated - control) / (1 - control))  * 100

write.csv(data_checked,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Ethiopia_cleaned.csv")

## The following can create data summarised by surface, insecticide used, wall surface sprayed, and district
## Could replace the district with site and then repeat for sites
for(j in 2:length(unique(data_checked$Insecticide.Sprayed))){
  per_month_spray <- array(dim = c(10,8,6))
   for(i in 2:length(unique(data_checked$Wall_surface_corrected))){

     data_Insecticide <- subset(data_checked, data_checked$Insecticide.Sprayed ==levels(data_checked$Insecticide.Sprayed)[j] &
                                  data_checked$Wall_surface_corrected == levels(data_checked$Wall_surface_corrected)[i])
          
    ## Bendiocarb
    per_month_spray[,1,i-1] <- tapply(data_Insecticide$mortality[data_Insecticide$diff_in_days < 2],
                                        data_Insecticide$Site_District_corrected[data_Insecticide$diff_in_days < 2],mean,na.rm=TRUE)
    per_month_spray[,2,i-1] <- tapply(data_Insecticide$mortality[data_Insecticide$diff_in_days > 2 & data_Insecticide$diff_in_days < 8],
                              data_Insecticide$Site_District_corrected[data_Insecticide$diff_in_days > 2 & data_Insecticide$diff_in_days < 8],mean,na.rm=TRUE)
    per_month_spray[,3,i-1] <- tapply(data_Insecticide$mortality[data_Insecticide$diff_in_days > 30 & data_Insecticide$diff_in_days < 40],
                              data_Insecticide$Site_District_corrected[data_Insecticide$diff_in_days > 30 & data_Insecticide$diff_in_days < 40],mean,na.rm=TRUE)
    per_month_spray[,4,i-1] <- tapply(data_Insecticide$mortality[data_Insecticide$diff_in_days > 40 & data_Insecticide$diff_in_days < 75],
                              data_Insecticide$Site_District_corrected[data_Insecticide$diff_in_days > 40 & data_Insecticide$diff_in_days < 75],mean,na.rm=TRUE)
    per_month_spray[,5,i-1] <- tapply(data_Insecticide$mortality[data_Insecticide$diff_in_days > 75 & data_Insecticide$diff_in_days < 102],
                              data_Insecticide$Site_District_corrected[data_Insecticide$diff_in_days > 75 & data_Insecticide$diff_in_days < 102],mean,na.rm=TRUE)
    per_month_spray[,6,i-1] <- tapply(data_Insecticide$mortality[data_Insecticide$diff_in_days > 102 & data_Insecticide$diff_in_days < 150],
                              data_Insecticide$Site_District_corrected[data_Insecticide$diff_in_days > 102 & data_Insecticide$diff_in_days < 150],mean,na.rm=TRUE)
    per_month_spray[,7,i-1] <- tapply(data_Insecticide$mortality[data_Insecticide$diff_in_days > 151 & data_Insecticide$diff_in_days < 190],
                              data_Insecticide$Site_District_corrected[data_Insecticide$diff_in_days > 151 & data_Insecticide$diff_in_days < 190],mean,na.rm=TRUE)
    per_month_spray[,8,i-1] <- tapply(data_Insecticide$mortality[data_Insecticide$diff_in_days > 190],
                              data_Insecticide$Site_District_corrected[data_Insecticide$diff_in_days > 190],mean,na.rm=TRUE)
    #per_month_Bendiocarb[,9,i-1] <- c(levels(data_Insecticide$Site_District_corrected))
    }
    
## Then you can pull out whichever is of interest
Bendio_mud <- as.data.frame(per_month_spray[,,3])
Bendio_dung <- as.data.frame(per_month_spray[,,2])
Bendio_paper <- as.data.frame(per_month_spray[,,5])
Bendio_painted <- as.data.frame(per_month_spray[,,4])
Bendio_smooth_painted <- as.data.frame(per_month_spray[,,6])
colnames(Bendio_mud) <- c("1 day", "2-7 days", "1 month", "2 months", "3 months", "4-5 months", "5-6 months"," more than 6 months")
colnames(Bendio_dung) <- c("1 day", "2-7 days", "1 month", "2 months", "3 months", "4-5 months", "5-6 months"," more than 6 months")
colnames(Bendio_painted) <- c("1 day", "2-7 days", "1 month", "2 months", "3 months", "4-5 months", "5-6 months"," more than 6 months")
colnames(Bendio_paper) <- c("1 day", "2-7 days", "1 month", "2 months", "3 months", "4-5 months", "5-6 months"," more than 6 months")
colnames(Bendio_smooth_painted) <- c("1 day", "2-7 days", "1 month", "2 months", "3 months", "4-5 months", "5-6 months"," more than 6 months")

col_names <- levels(data_Insecticide$Site_District_corrected)
cl <- rainbow(10)
txt_place <- c(100,96,92,88,84,80,76,72,68,64)
par(mfrow=c(2,3))
decay_plots_fun <- function(data_file,title,val_length){

    plot(as.numeric(data_file[1,]) ~ c(1,5,31,62,90,125,160,180), 
         xlim = c(0,200), ylim = c(0,100),frame=FALSE,
         xlab = "Time since spraying (days)",
         ylab = "Wall Cone Bio-assay Mortality rate %",
         main = print(title)
         )
  for( i in 1:val_length){
    lines(as.numeric(data_file[i,]) ~ c(1,5,31,62,90,125,160,180),col=cl[i]) 
    text(150,txt_place[i], print(col_names[i]),col=cl[i])
  }
}
type_names <- levels(data_checked$Insecticide.Sprayed)
type <- c("Mud", "Dung", "Paper","Painted","smooth painted")
decay_plots_fun(Bendio_mud,c(type_names[j-1], type[1]),length(levels(data_Insecticide$Site_District_corrected)))
decay_plots_fun(Bendio_dung,c(type_names[j-1], type[2]),length(levels(data_Insecticide$Site_District_corrected)))
decay_plots_fun(Bendio_paper,c(type_names[j-1], type[3]),length(levels(data_Insecticide$Site_District_corrected)))
decay_plots_fun(Bendio_painted,c(type_names[j-1], type[4]),length(levels(data_Insecticide$Site_District_corrected)))
decay_plots_fun(Bendio_smooth_painted,c(type_names[j-1], type[5]),length(levels(data_Insecticide$Site_District_corrected)))
    
} 

## No plot per day results for a given set of circumstances
## species of mosquito
## site
## insecticide

species      = levels(data_checked$Species_corrected)
site         = levels(data_checked$Site_District_corrected)
wall_surface = levels(data_checked$Wall_surface_corrected)
insecticide  = levels(data_checked$Insecticide.Sprayed)

par(mfrow = c(2,2))
for (k in 1:length(species)){
  for(j in 1:length(site)){
    for(i in 2:length(insecticide)){
      data_subset <- subset(data_checked, data_checked$Species_corrected == species[k] &
                          data_checked$Site_District_corrected == site[j] &
                          #data_checked$Wall_surface_corrected == wall_surface[3] &
                          data_checked$Insecticide.Sprayed == insecticide[i])
      dim(data_subset)

      title_obj = list(species, site, insecticide)

      plot(data_subset$mortality ~ data_subset$diff_in_days, 
          xlim = c(0,200), ylim = c(0,100),frame=FALSE,
          xlab = "Time since spraying (days)",
          ylab = "Wall Cone Bio-assay Mortality rate %",
          main = print(c(title_obj[[1]][k],title_obj[[2]][j],title_obj[[3]][i]))
          )

    }
  }
}





## TASK 3
## To convert countries raw data from Abt Associates into summary data 
## on coverage of different villages, communes and regions by mosquito species





dat_Sen <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\SENEGAL_cleaning.csv",header=TRUE)
dim(dat_Sen)

data1 <- c(dat_Sen[,3],dat_Sen[,4],dat_Sen[,5],dat_Sen[,6],dat_Sen[,7],dat_Sen[,8],dat_Sen[,9],dat_Sen[,10],
           dat_Sen[,11],dat_Sen[,12],dat_Sen[,13])
insecticides <- rep(colnames(dat_Sen[3:13]),each=41)
Districts <- rep(dat_Sen[,1],11)
Regions <- rep(dat_Sen[,2],11)
Year <- rep(dat_Sen[,14],11)
Latitude <- rep(dat_Sen[,15],11)
Longitude <- rep(dat_Sen[,16],11)
data2 <- data.frame(insecticides,Year,Districts, Regions,data1,Latitude,Longitude)

DATA_Sen = data2[complete.cases(data2),]

library(plyr)
DATA_Sen$insecticides <- revalue(DATA_Sen$insecticides, c("Deltaméthrine.0.05."="Deltamethrin 0.05%", 
                                 "Lambdacyhalothrine.0.05."="Lambda-cyhalothrin 0.05%",
                                 "Perméthrine.0.75." = "Permethrin 0.75%",
                                 "alphacypermethrin" = "Alphacypermethrin 0.05%",
                                 "Dieldrin" = "Dieldrin 4.0%",
                                 "Bendiocarb.0.1." = "Bendiocarb 0.1%",
                                 "Fenitrothion.1." = "Fenitrothion 1.0%",
                                 "Malathion.5.","Malathion 5.0%",
                                 "Pirimiphos.methyl" = "Pirimiphos methyl 0.25%",
                                 "cyfluthrin" = "Cyfluthrin 0.15%",
                                 "DDT.4." = "DDT 4.0%"))

DATA_Sen$Resistance <- ifelse(DATA_Sen$data1 < 90, "High", 
                              ifelse(DATA_Sen$data1 < 98 & DATA_Sen$data1 > 90, "Moderate",
                                     "Susceptible"))
DATA_Sen$Class <- ifelse(DATA_Sen$insecticides == "Deltamethrin 0.05%" |
                           DATA_Sen$insecticides == "Lambda-cyhalothrin 0.05%" |
                           DATA_Sen$insecticides == "Permethrin 0.75%" |
                           DATA_Sen$insecticides == "Alphacypermethrin 0.05%" |
                           DATA_Sen$insecticides ==  "Cyfluthrin 0.15%", "Pyrethroids",
                         ifelse(DATA_Sen$insecticides == "Dieldrin 4.0%" |
                                  DATA_Sen$insecticides == "DDT 4.0%", "Organochlorides",
                                ifelse(DATA_Sen$insecticides == "Fenitrothion 1.0%" |
                                         DATA_Sen$insecticides == "Malathion 5.0%" |
                                         DATA_Sen$insecticides == "Pirimiphos methyl 0.25%", "Organophosphates",
                                      "Carbomates")))

DATA_Sen$Res_type <- ifelse(DATA_Sen$Resistance == "Susceptible", "Susceptible",
                            ifelse(DATA_Sen$Resistance == "High","Confirmed Resistance","Possible resistance"))

DATA_Sen$Res_code <- ifelse(DATA_Sen$Resistance == "Susceptible", "S",
                            ifelse(DATA_Sen$Resistance == "High","R","PR"))
                                  

write.csv(DATA_Sen,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\DATA_Sen.csv")



dat_Zam <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\ZAMBIA_cleaning.csv",header=TRUE)

names(dat_Zam)
dim(dat_Zam)

data1 <- c(dat_Zam[,4],dat_Zam[,5],dat_Zam[,6],dat_Zam[,7],dat_Zam[,8],dat_Zam[,9],dat_Zam[,10],dat_Zam[,11])
data2_numbers <- c(dat_Zam[,14],dat_Zam[,15],dat_Zam[,16],dat_Zam[,17],dat_Zam[,18],dat_Zam[,19],dat_Zam[,20],dat_Zam[,21])
insecticides <- rep(colnames(dat_Zam[4:11]),each=60)
Districts <- rep(dat_Zam[,2],8)
Provinces <- rep(dat_Zam[,1],8)
Sites <- rep(dat_Zam[,3],8)
Year <- rep(dat_Zam[,12],8)
Species <- rep(dat_Zam[,13],8)
Latitude <- rep(dat_Zam[,22],8)
Longitude <- rep(dat_Zam[,23],8)
data2 <- data.frame(insecticides,Year,Species,Provinces,Districts,Sites,
                    data2_numbers,data1,Latitude,Longitude)

DATA_Zam = data2[complete.cases(data2),]

library(plyr)
DATA_Zam$insecticides <- revalue(DATA_Zam$insecticides, c("Delta"="Deltamethrin 0.05%", 
                                                          "lambdacyhalothrin"="Lambda-cyhalothrin 0.05%",
                                                          "Permeth" = "Permethrin 0.75%",
                                                          "alphacypermeth" = "Alphacypermethrin 0.05%",
                                                          "dieldrin" = "Dieldrin 4.0%",
                                                          "Bendio" = "Bendiocarb 0.1%",
                                                          "Pirimiphosmethyl" = "Pirimiphos methyl 0.25%",
                                                          "DDT" = "DDT 4.0%"))

DATA_Zam$Resistance <- ifelse(DATA_Zam$data1 < 90, "High", 
                              ifelse(DATA_Zam$data1 < 98 & DATA_Zam$data1 > 90, "Moderate",
                                     "Susceptible"))
DATA_Zam$Class <- ifelse(DATA_Zam$insecticides == "Deltamethrin 0.05%" |
                           DATA_Zam$insecticides == "Lambda-cyhalothrin 0.05%" |
                           DATA_Zam$insecticides == "Permethrin 0.75%" |
                           DATA_Zam$insecticides == "Alphacypermethrin 0.05%" |
                           DATA_Zam$insecticides ==  "Cyfluthrin 0.15%", "Pyrethroids",
                         ifelse(DATA_Zam$insecticides == "Dieldrin 4.0%" |
                                  DATA_Zam$insecticides == "DDT 4.0%", "Organochlorides",
                                ifelse(DATA_Zam$insecticides == "Fenitrothion 1.0%" |
                                         DATA_Zam$insecticides == "Malathion 5.0%" |
                                         DATA_Zam$insecticides == "Pirimiphos methyl 0.25%", "Organophosphates",
                                       "Carbomates")))

DATA_Zam$Res_type <- ifelse(DATA_Zam$Resistance == "Susceptible", "Susceptible",
                            ifelse(DATA_Zam$Resistance == "High","Confirmed Resistance","Possible resistance"))

DATA_Zam$Res_code <- ifelse(DATA_Zam$Resistance == "Susceptible", "S",
                            ifelse(DATA_Zam$Resistance == "High","R","PR"))

write.csv(DATA_Zam,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\DATA_Zam.csv")
