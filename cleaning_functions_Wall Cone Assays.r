
############################################################
##
##  Converting country level data to formatted tables
##  Version 1
##  Author: Ellie Sherrard-Smith
##  Date: 12 March 2016
##
############################################################
library(lubridate)
library(rstan)

## TASK 2
## To convert countries raw data from Abt Associates into summary data 
## on wall cone bioassays - decay rates

## Ethiopia
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

## Now plot per day results for a given set of circumstances
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
      for(m in 2:length(wall_surface)){
      data_subset <- subset(data_checked, data_checked$Species_corrected == species[k] &
                          data_checked$Site_District_corrected == site[j] &
                          data_checked$Insecticide.Sprayed == insecticide[i] &
                          data_checked$Wall_surface_corrected == wall_surface[m] )
      dim(data_subset)

      title_obj = list(species, site, insecticide,wall_surface)

      plot(data_subset$mortality ~ data_subset$diff_in_days, 
          xlim = c(0,200), ylim = c(0,100),frame=FALSE,
          xlab = "Time since spraying (days)",
          ylab = "Wall Cone Bio-assay Mortality rate %",
          main = print(c(title_obj[[1]][k],title_obj[[2]][j],title_obj[[3]][i],title_obj[[4]][m]))
          )
      }
    }
  }
}




## Ghana
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Ghana.csv",header=TRUE)
names(data)

##**** Pre function checks 
##  1. Confirm that the place names are uniform

## Sort the data.frame by the names column
data <- data[order(data$Site_District),]  

## Take a look 
data$Site_District
data$Site_District_corrected <- data$Site_District
summary(data$Site_District_corrected)

## Now repeat the above for the test sites
data <- data[order(data$Test.Site),]

levels(data$Test.Site)


## 2. Confirm the dates are sensible i.e. the test date follows the spray date

data$date_sprayed <- dmy(data$"Spraying.Date")    ## Convert the data into a date and yearly format from which you can work
data$date_tested <- dmy(data$"Date.of.Test")
data$diff_in_days<- round(difftime(data$date_tested, data$date_sprayed, units = c("days"))) ## check in case of mistakes in data entry

data <- data[order(data$diff_in_days),]

## Delete questionable data and correct as necessary
data <- subset(data, data$diff_in_days > 0)

## 3. Species names
data$Species_corrected <- data$Species
levels(data$Species_corrected)[1] <- "An. gambiae, Kisumu Strain"

## 4. Wall type names
data$Wall_surface_corrected <- data$Wall_Surface.Type 
levels(data$Wall_surface_corrected)

## 5. Insecticide
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

write.csv(data_checked,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Ghana_cleaned.csv")


## Now plot per day results for a given set of circumstances
## species of mosquito
## site
## insecticide

species      = levels(data_checked$Species_corrected)
site         = levels(data_checked$Site_District_corrected)
wall_surface = levels(data_checked$Wall_surface_corrected)
insecticide  = levels(data_checked$Insecticide.Sprayed)

par(mfrow = c(2,3))
for (k in 1:length(species)){
  for(j in 1:length(site)){
    for(i in 1:length(insecticide)){
      for (m in 1:length(wall_surface)){
      data_subset <- subset(data_checked, data_checked$Species_corrected == species[k] &
                              data_checked$Site_District_corrected == site[j] &
                              data_checked$Insecticide.Sprayed == insecticide[i] &
                              data_checked$Wall_surface_corrected == wall_surface[m])
      dim(data_subset)
      
      title_obj = list(species, site, insecticide,wall_surface)
      
      plot(data_subset$mortality ~ data_subset$diff_in_days, 
           xlim = c(0,200), ylim = c(0,100),frame=FALSE,
           xlab = "Time since spraying (days)",
           ylab = "Wall Cone Bio-assay Mortality rate %",
           main = print(c(title_obj[[1]][k],title_obj[[2]][j],title_obj[[3]][i],title_obj[[4]][m]))
      )
      }
    }
  }
}

data_pm_byd_gambKS <- subset(data_checked, data_checked$Species_corrected == species[1] &
                        data_checked$Site_District_corrected == site[1] &
                        #data_checked$Wall_surface_corrected == wall_surface[3] &
                        data_checked$Insecticide.Sprayed == insecticide[2])
data_pm_byd_gambKS <- subset(data_pm_byd_gambKS, data_pm_byd_gambKS$mortality > 0)
dim(data_pm_byd_gambKS)

data_pm_byd_gambsl <- subset(data_checked, data_checked$Species_corrected == species[2] &
                               data_checked$Site_District_corrected == site[1] &
                               #data_checked$Wall_surface_corrected == wall_surface[3] &
                               data_checked$Insecticide.Sprayed == insecticide[2])
data_pm_byd_gambsl <- subset(data_pm_byd_gambsl, data_pm_byd_gambsl$mortality > 0)
dim(data_pm_byd_gambsl)

data_alph_byd_gambKS <- subset(data_checked, data_checked$Species_corrected == species[1] &
                               data_checked$Site_District_corrected == site[1] &
                               #data_checked$Wall_surface_corrected == wall_surface[3] &
                               data_checked$Insecticide.Sprayed == insecticide[1])
data_alph_byd_gambKS <- subset(data_alph_byd_gambKS, data_alph_byd_gambKS$mortality > 0)
dim(data_alph_byd_gambKS)


data_list <- list(N = 342,
                  y = data_pm_byd_gambKS$mortality/100,
                  x = as.numeric(data_pm_byd_gambKS$diff_in_days))

test <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
              data=data_list, 
              sample_file = "Ghana_PM_BYD_gambiaeKS_output.csv",
              iter=1000, chains=4)
print(test)
params = extract(test);names(params)
traceplot(test, inc_warmup = TRUE)

data_list_1 <- list(N = 134,
                  y = data_pm_byd_gambsl$mortality/100,
                  x = as.numeric(data_pm_byd_gambsl$diff_in_days))

test1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
             data=data_list_1, 
             sample_file = "Ghana_PM_BYD_gambiaesl_output.csv",
             iter=1000, chains=4)
print(test1)
params1 = extract(test1);names(params1)
traceplot(test1, inc_warmup = TRUE)

data_list_2 <- list(N = 127,
                  y = data_alph_byd_gambKS$mortality/100,
                  x = as.numeric(data_alph_byd_gambKS$diff_in_days))

test2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                   data=data_list_2, 
                   sample_file = "Ghana_APLH_BYD_gambiaeKS_output.csv",
                   iter=1000, chains=4)
print(test2)
params2 = extract(test2);names(params2)
traceplot(test2, inc_warmup = TRUE)
nc = seq(0,400,1)

pred  <- (1 / (1 + mean(params$alpha) * exp(mean(params$beta) * nc)))
pred1 <- (1 / (1 + mean(params1$alpha) * exp(mean(params1$beta) * nc)))
pred2 <- (1 / (1 + mean(params2$alpha) * exp(mean(params2$beta) * nc)))

par(mfrow=c(1,1))
plot(data_pm_byd_gambKS$mortality ~ data_pm_byd_gambKS$diff_in_days, 
     xlim = c(0,400), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",yaxt = "n",
     ylab = "Wall Cone Bio-assay Mortality rate %",
     main = "Ghana, BYD district: Anopheles gambiae")
axis(2, at = seq(0,100,20), labels = seq(0,100,20), las = 2)
lines(pred*100 ~ nc)
segments(y0=0.4983851*100,y1=0.4983851*100,x0=0,x1=nc[385],lty=2,col="grey",lwd=2)
segments(y0=0,y1=0.4983851*100,x0=nc[334],x1=nc[334],lty=2,col="grey10",lwd=2)

points(data_pm_byd_gambsl$mortality ~ data_pm_byd_gambsl$diff_in_days, pch=20, col = "blue")
lines(pred1*100 ~ nc, col = "blue")

points(data_alph_byd_gambKS$mortality ~ data_alph_byd_gambKS$diff_in_days, pch=20, col="red")
lines(pred2*100 ~ nc, col = "red")

segments(y0=0,y1=0.4983851*100,x0=nc[255],x1=nc[255],lty=2,col="blue",lwd=2)
segments(y0=0,y1=0.4983299*100,x0=nc[385],x1=nc[385],lty=2,col="red",lwd=2)

legend(10,40,legend = c("Alpha cypermethrin: An. gambiae Kisumu Strain",
                        "Pirimiphos methyl: An. gambiae Kisumu Strain",
                        "Pirimiphos methyl: An. gambiae s.l."),
       col = c("red","black","blue"),
       pch = c(20, 1, 20),
       bty = "n",
       cex = 1.1)
text(nc[255],4,"254 days", col= "darkblue", cex.lab = 1.1)
text(nc[334],4,"333 days", col= "black", cex.lab = 1.1)
text(nc[385],4,"384 days", col= "darkred", cex.lab = 1.1)



## Mali
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Mali.csv",header=TRUE)
names(data)

##**** Pre function checks 
##  1. Confirm that the place names are uniform

## Sort the data.frame by the names column
data <- data[order(data$Site_District),]  

## Take a look 
levels(data$Site_District)
## Rename the mis-spelt columns as appropriate
levels(data$Site_District)[2] <- "Baraoueli"
levels(data$Site_District)[3] <- "Koulikoro"
levels(data$Site_District)[4] <- "Koulikoro"
data$Site_District_corrected <- data$Site_District 
summary(data$Site_District_corrected)

## Now repeat the above for the test sites
data <- data[order(data$Test.Site),]

levels(data$Test.Site)
levels(data$Test.Site)[3] <- "Konobougou"
levels(data$Test.Site)[4] <- "N'dendjila"
levels(data$Test.Site)[6] <- "Tigui/ Djigani wereba"
levels(data$Test.Site)[7] <- "Tigui/ Djigani wereba"
levels(data$Test.Site)[7] <- "Tigui/ Djigani wereba"
levels(data$Test.Site)[8] <- "Touna/ Dona Banida"


## 2. Confirm the dates are sensible i.e. the test date follows the spray date

data$date_sprayed <- dmy(data$"Spraying.Date")    ## Convert the data into a date and yearly format from which you can work
data$date_tested <- dmy(data$"Date.of.Test")
data$diff_in_days<- round(difftime(data$date_tested, data$date_sprayed, units = c("days"))) ## check in case of mistakes in data entry

data <- data[order(data$diff_in_days),]

## Check the data and correct as necessary
head(data[1:25,36:38])

data <- subset(data, data$diff_in_days > 0)

## 3. Species names
data$Species_corrected <- data$Species
levels(data$Species_corrected)[1] <- "An. gambiae, Baguineda  Strain"
levels(data$Species_corrected)[2] <- "An. gambiae, Baguineda  Strain"
levels(data$Species_corrected)[2] <- "An. gambiae, Baguineda  Strain"

## 4. Wall type names
data$Wall_surface_corrected <- data$Wall_Surface.Type 
levels(data$Wall_surface_corrected)[2] <- "Mud with cement"
levels(data$Wall_surface_corrected)[4] <- "Mud with cement"

summary(data$Wall_surface_corrected)

## 5. Insecticide
levels(data$Insecticide.Sprayed)

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

write.csv(data_checked,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Mali_cleaned.csv")

## Now plot per day results for a given set of circumstances
## species of mosquito
## site
## insecticide

species      = levels(data_checked$Species_corrected)
site         = levels(data_checked$Site_District_corrected)
wall_surface = levels(data_checked$Wall_surface_corrected)
insecticide  = levels(data_checked$Insecticide.Sprayed)

par(mfrow = c(2,3))
for (k in 1:length(species)){
  for(j in 1:length(site)){
    for(i in 1:length(insecticide)){
      for (m in 2:length(wall_surface)) {
      data_subset <- subset(data_checked, data_checked$Species_corrected == species[k] &
                              data_checked$Site_District_corrected == site[j] &
                              data_checked$Insecticide.Sprayed == insecticide[i] &
                              data_checked$Wall_surface_corrected == wall_surface[m] )
      dim(data_subset)
      
      title_obj = list(species, site, insecticide, wall_surface)
      
      plot(data_subset$mortality ~ data_subset$diff_in_days, 
           xlim = c(0,200), ylim = c(0,100),frame=FALSE,
           xlab = "Time since spraying (days)",
           ylab = "Wall Cone Bio-assay Mortality rate %",
           main = print(c(title_obj[[1]][k],title_obj[[2]][j],title_obj[[3]][i],title_obj[[4]][m]))
      )
      }
    }
  }
}

data_Bendiocarb <- subset(data_checked, data_checked$Insecticide.Sprayed == insecticide[1])


dim(data_Bendiocarb)

data_Mali <- list(N = 51,
                    y = c(data_Bendiocarb$mortality[1:11]/100,data_Bendiocarb$mortality[13:52]/100),
                    x = as.numeric(c(data_Bendiocarb$diff_in_days[1:11],data_Bendiocarb$diff_in_days[13:52])))

test_data_Mali <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
              data=data_Mali, 
              sample_file = "Mali_Bendiocarb_output.csv",
              iter=1000, chains=4)
print(test_data_Mali)
parm = extract(test_data_Mali);names(parm)
traceplot(test_data_Mali, inc_warmup = TRUE)
nc = seq(0,400,1)

pred_Mali  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

par(mfrow=c(1,1))
plot(c(data_Bendiocarb$mortality[1:11],data_Bendiocarb$mortality[13:52]) ~ 
       c(data_Bendiocarb$diff_in_days[1:11],data_Bendiocarb$diff_in_days[13:52]), 
     xlim = c(0,400), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",yaxt = "n",
     ylab = "Wall Cone Bio-assay Mortality rate %",
     main = "Mali: Anopheles gambiae and Bendiocarb")
axis(2, at = seq(0,100,20), labels = seq(0,100,20), las = 2)
lines(pred_Mali*100 ~ nc)
segments(y0=0.4820682*100,y1=0.4820682*100,x0=0,x1=nc[73],lty=2,col="grey10",lwd=2)
segments(y0=0,y1=0.4820682*100,x0=nc[73],x1=nc[73],lty=2,col="grey10",lwd=2)
text(nc[73],4,"72 days", col= "black", cex.lab = 1.1)

points(c(data_Bendiocarb$mortality[1:11][data_Bendiocarb$Site_District == "Baraoueli"],
         data_Bendiocarb$mortality[13:52][data_Bendiocarb$Site_District == "Baraoueli"])~
         c(data_Bendiocarb$diff_in_days[1:11][data_Bendiocarb$Site_District == "Baraoueli"],data_Bendiocarb$diff_in_days[13:52][data_Bendiocarb$Site_District == "Baraoueli"]),
       col = "red", pch = 20)

points(c(data_Bendiocarb$mortality[1:11][data_Bendiocarb$Site_District == "Bla"],
         data_Bendiocarb$mortality[13:52][data_Bendiocarb$Site_District == "Bla"])~
         c(data_Bendiocarb$diff_in_days[1:11][data_Bendiocarb$Site_District == "Bla"],data_Bendiocarb$diff_in_days[13:52][data_Bendiocarb$Site_District == "Bla"]),
       col = "blue", pch = 20)

legend(200,80,legend = c("Baroueli",
                        "Bla",
                        "Koulikoro"),
       col = c("red","blue","black"),
       pch = c(20, 20, 1),
       bty = "n",
       cex = 1.1)



## Mozambique
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Mozambique.csv",header=TRUE)
names(data)

##**** Pre function checks 
##  1. Confirm that the place names are uniform

## Sort the data.frame by the names column
data <- data[order(data$Site_District),]  

## Take a look 
levels(data$Site_District)
## Rename the mis-spelt columns as appropriate
levels(data$Site_District)[2] <- "Milange"
levels(data$Site_District)[2] <- "Mocuba"
levels(data$Site_District)[3] <- "Mocuba"
levels(data$Site_District)[3] <- "Mocuba"
levels(data$Site_District)[3] <- "Morrumbala"
levels(data$Site_District)[4] <- "Morrumbala"
levels(data$Site_District)[4] <- "Morrumbala"

data$Site_District_corrected <- data$Site_District 
summary(data$Site_District_corrected)

## Now repeat the above for the test sites
data <- data[order(data$Test.Site),]

levels(data$Test.Site)
levels(data$Test.Site)[2] <- "12 de Otubro"
levels(data$Test.Site)[2] <- "Chico"
levels(data$Test.Site)[3] <- "Coqueiro"
levels(data$Test.Site)[4] <- "Coqueiro"
levels(data$Test.Site)[4] <- "Samora Machel"
levels(data$Test.Site)[5] <- "Samora Machel"
levels(data$Test.Site)[5] <- "Samora Machel"


## 2. Confirm the dates are sensible i.e. the test date follows the spray date

data$date_sprayed <- dmy(data$"Spraying.Date")    ## Convert the data into a date and yearly format from which you can work
data$date_tested <- dmy(data$"Date.of.Test")
data$diff_in_days<- round(difftime(data$date_tested, data$date_sprayed, units = c("days"))) ## check in case of mistakes in data entry

data <- data[order(data$diff_in_days),]

## Check the data and correct as necessary
head(data[1:25,36:38])

data <- subset(data, data$diff_in_days > 0)

## 3. Species names
data$Species_corrected <- data$Species


## 4. Wall type names
data$Wall_surface_corrected <- data$Wall_Surface.Type 
levels(data$Wall_surface_corrected)

summary(data$Wall_surface_corrected)

## 5. Insecticide
levels(data$Insecticide.Sprayed)

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

write.csv(data_checked,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Mozambique_cleaned.csv")

## Now plot per day results for a given set of circumstances
## species of mosquito
## site
## insecticide

species      = levels(data_checked$Species_corrected)
site         = levels(data_checked$Site_District_corrected)
wall_surface = levels(data_checked$Wall_surface_corrected)
insecticide  = levels(data_checked$Insecticide.Sprayed)

par(mfrow = c(2,3))
for (k in 2:length(species)){
  for(j in 1:length(site)){
    for(i in 1:length(insecticide)){
      for(m in 1:length(wall_surface)) {
      data_subset <- subset(data_checked, data_checked$Species_corrected == species[k] &
                              data_checked$Site_District_corrected == site[j] &
                              data_checked$Insecticide.Sprayed == insecticide[i] &
                              data_checked$Wall_surface_corrected == wall_surface[m])
      dim(data_subset)
      
      title_obj = list(species, site, insecticide,wall_surface)
      
      plot(data_subset$mortality ~ data_subset$diff_in_days, 
           xlim = c(0,200), ylim = c(0,100),frame=FALSE,
           xlab = "Time since spraying (days)",
           ylab = "Wall Cone Bio-assay Mortality rate %",
           main = print(c(title_obj[[1]][k],title_obj[[2]][j],title_obj[[3]][i],title_obj[[4]][m]))
      )
      }
    }
  }
}

data_alpha_Mozo <- subset(data_checked, data_checked$Insecticide.Sprayed == insecticide[1])
data_delta_Mozo <- subset(data_checked, data_checked$Insecticide.Sprayed == insecticide[2])

dim(data_delta_Mozo)
dim(data_alpha_Mozo)

data_alpha_Mozo_list <- list(N = 23,
                             y = c(data_alpha_Mozo$mortality[1:18]/100,data_alpha_Mozo$mortality[20:24]/100),
                             x = as.numeric(c(data_alpha_Mozo$diff_in_days[1:18],data_alpha_Mozo$diff_in_days[20:24])))

test_alpha_Mozo <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                       data=data_alpha_Mozo_list, 
                       sample_file = "Mozo_Alpha_output.csv",
                       iter=1000, chains=4)
print(test_alpha_Mozo)
parm1 = extract(test_alpha_Mozo);names(parm1)
traceplot(test_alpha_Mozo, inc_warmup = TRUE)
nc = seq(0,400,1)

pred_alpha_Mozo  <- (1 / (1 + mean(parm1$alpha) * exp(mean(parm1$beta) * nc)))

data_delta_Mozo_list <- list(N = 179,
                             y = c(data_delta_Mozo$mortality[1:142]/100,data_delta_Mozo$mortality[144:180]/100),
                             x = as.numeric(c(data_delta_Mozo$diff_in_days[1:142],data_delta_Mozo$diff_in_days[144:180])))

test_delta_Mozo <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                        data=data_delta_Mozo_list, 
                        sample_file = "Mozo_Delta_output.csv",
                        iter=1000, chains=4)
print(test_delta_Mozo)
parm2 = extract(test_delta_Mozo);names(parm2)
traceplot(test_delta_Mozo, inc_warmup = TRUE)
nc = seq(0,400,1)

pred_delta_Mozo  <- (1 / (1 + mean(parm2$alpha) * exp(mean(parm2$beta) * nc)))

par(mfrow=c(1,1))
plot(c(data_alpha_Mozo$mortality[1:18],data_alpha_Mozo$mortality[20:24]) ~ 
       c(data_alpha_Mozo$diff_in_days[1:18],data_alpha_Mozo$diff_in_days[20:24]), 
     xlim = c(0,400), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",yaxt = "n",
     ylab = "Wall Cone Bio-assay Mortality rate %",
     main = "Mozambique: Anopheles sp.")
axis(2, at = seq(0,100,20), labels = seq(0,100,20), las = 2)
lines(pred_alpha_Mozo*100 ~ nc)
segments(y0=(pred_alpha_Mozo[1]/2)*100,y1=(pred_alpha_Mozo[1]/2)*100,x0=0,x1=nc[157],lty=2,col="grey10",lwd=2)
segments(y0=0,y1=(pred_alpha_Mozo[1]/2)*100,x0=nc[157],x1=nc[157],lty=2,col="grey10",lwd=2)
text(nc[157],4,"156 days", col= "black", cex.lab = 1.1)

points(c(data_delta_Mozo$mortality[1:142],data_delta_Mozo$mortality[144:180])~
         c(data_delta_Mozo$diff_in_days[1:142],data_delta_Mozo$diff_in_days[144:180]),
       col = "red", pch = 20)
lines(pred_delta_Mozo*100 ~ nc, col = "red")
segments(y0=(pred_delta_Mozo[1]/2)*100,y1=(pred_delta_Mozo[1]/2)*100,x0=0,x1=nc[389],lty=2,col="red",lwd=2)
segments(y0=0,y1=(pred_delta_Mozo[1]/2)*100,x0=nc[389],x1=nc[389],lty=2,col="red",lwd=2)
text(nc[389],4,"388 days", col= "darkred", cex.lab = 1.1)


legend(10,40,legend = c("Alpha-cypermethrin",
                         "Deltamethrin"),
       col = c("black","red"),
       pch = c(1,20),
       bty = "n",
       cex = 1.1)



## Madagascar
data <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Madagascar.csv",header=TRUE)
names(data)

##**** Pre function checks 
##  1. Confirm that the place names are uniform

## Sort the data.frame by the names column
data <- data[order(data$Site_District),]  

## Take a look 
levels(data$Site_District)
## Rename the mis-spelt columns as appropriate
levels(data$Site_District)[5] <- "Ampanihy"
levels(data$Site_District)[10] <- "Fenerive Est"

data$Site_District_corrected <- data$Site_District 
summary(data$Site_District_corrected)

## Now repeat the above for the test sites
data <- data[order(data$Test.Site),]

levels(data$Test.Site)
levels(data$Test.Site)[16] <- "Kiangara"
levels(data$Test.Site)[11] <- "Imrina Imady"
levels(data$Test.Site)[12] <- "Imrina Imady"
levels(data$Test.Site)[12] <- "Imrina Imady"


## 2. Confirm the dates are sensible i.e. the test date follows the spray date

data$date_sprayed <- dmy(data$"Spraying.Date")    ## Convert the data into a date and yearly format from which you can work
data$date_tested <- dmy(data$"Date.of.Test")
data$diff_in_days<- round(difftime(data$date_tested, data$date_sprayed, units = c("days"))) ## check in case of mistakes in data entry

data <- data[order(data$diff_in_days),]

## Check the data and correct as necessary
head(data[7:14,36:38])

data <- subset(data, data$diff_in_days > 0)

## 3. Species names
data$Species_corrected <- data$Species


## 4. Wall type names
data$Wall_surface_corrected <- data$Wall_Surface.Type 
levels(data$Wall_surface_corrected)
levels(data$Wall_surface_corrected)[5] <- "Ravenala"
levels(data$Wall_surface_corrected)[6] <- "Ravenala"
levels(data$Wall_surface_corrected)[6] <- "Thatch (Falafa)"
levels(data$Wall_surface_corrected)[3] <- "Thatch (Falafa)"
summary(data$Wall_surface_corrected)

## 5. Insecticide
levels(data$Insecticide.Sprayed)

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

write.csv(data_checked,"C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Madagascar_cleaned.csv")

## Now plot per day results for a given set of circumstances
## species of mosquito
## site
## insecticide

species      = levels(data_checked$Species_corrected)
site         = levels(data_checked$Site_District_corrected)
wall_surface = levels(data_checked$Wall_surface_corrected)
insecticide  = levels(data_checked$Insecticide.Sprayed)

par(mfrow = c(2,3))
for (k in 2:length(species)){
  for(j in 1:length(site)){
    for(i in 1:length(insecticide)){
      for(m in 1:length(wall_surface)) {
      data_subset <- subset(data_checked, data_checked$Species_corrected == species[k] &
                              data_checked$Site_District_corrected == site[j] &
                              data_checked$Insecticide.Sprayed == insecticide[i] &
                              data_checked$Wall_surface_corrected == wall_surface[m])
      dim(data_subset)
      
      title_obj = list(species, site, insecticide, wall_surface)
      
      plot(data_subset$mortality ~ data_subset$diff_in_days, 
           xlim = c(0,200), ylim = c(0,100),frame=FALSE,
           xlab = "Time since spraying (days)",
           ylab = "Wall Cone Bio-assay Mortality rate %",
           main = print(c(title_obj[[1]][k],title_obj[[2]][j],title_obj[[3]][i],title_obj[[4]][m]))
      )
      }
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



## Zambia
data_checked <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Zambia_inferred from reports.csv",header=TRUE)
names(data_checked)
species      = levels(data_checked$Species)
site         = levels(data_checked$Site_District)
wall_surface = levels(data_checked$Wall_Surface.Type)
insecticide  = levels(data_checked$Insecticide.Sprayed)

par(mfrow = c(1,2))
for (k in 1:length(species)){
  for(j in 1:length(site)){
    for(i in 1:length(wall_surface)){
      for(m in 1:length(insecticide)){
      data_subset <- subset(data_checked, data_checked$Species == species[k] &
                              data_checked$Site_District == site[j] &
                              data_checked$Wall_Surface.Type == wall_surface[i] &
                              data_checked$Insecticide.Sprayed == insecticide[m])
      dim(data_subset)
      
      title_obj = list(species, site, wall_surface,insecticide)
      
      plot(data_subset$mortality ~ data_subset$diff_in_days, 
           xlim = c(0,200), ylim = c(0,100),frame=FALSE,
           xlab = "Time since spraying (days)",
           ylab = "Wall Cone Bio-assay Mortality rate %",
           main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]][i],title_obj[[4]][m]))
      )
      }
    }
  }
}

data_piri <- subset(data_checked, data_checked$Insecticide.Sprayed == insecticide[1])


dim(data_piri)

data_zamb_cement <- list(N = 18,
                      y = c(data_piri$mortality[1:6]/100,data_piri$mortality[13:18]/100,data_piri$mortality[25:30]/100),
                      x = as.numeric(c(data_piri$diff_in_days[1:6],data_piri$diff_in_days[13:18],data_piri$diff_in_days[25:30])))

test_data_Zamb_C <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                       data=data_zamb_cement, 
                       sample_file = "Zamb_piri_CEMENT_output.csv",
                       iter=1000, chains=4)
print(test_data_Zamb_C)
parm = extract(test_data_Zamb_C);names(parm)
traceplot(test_data_Zamb_C, inc_warmup = TRUE)
nc = seq(0,400,1)

pred_Zamb_C  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

par(mfrow=c(1,1))
plot(100*c(data_piri$mortality[1:6]/100,data_piri$mortality[13:18]/100,data_piri$mortality[25:30]/100) ~ 
       c(data_piri$diff_in_days[1:6],data_piri$diff_in_days[13:18],data_piri$diff_in_days[25:30]), 
     xlim = c(0,400), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",yaxt = "n",
     ylab = "Wall Cone Bio-assay Mortality rate %",
     main = "Zambia: Anopheles gambiae and Pirimiphos methyl")
axis(2, at = seq(0,100,20), labels = seq(0,100,20), las = 2)
lines(pred_Zamb_C*100 ~ nc)
#segments(y0=0.4820682*100,y1=0.4820682*100,x0=0,x1=nc[73],lty=2,col="grey10",lwd=2)
#segments(y0=0,y1=0.4820682*100,x0=nc[73],x1=nc[73],lty=2,col="grey10",lwd=2)
text(nc[365],90,"More than 365 days", col= "black", cex.lab = 1.1)

data_zamb_MUD <- list(N = 18,
                         y = c(data_piri$mortality[7:12]/100,data_piri$mortality[19:24]/100,data_piri$mortality[31:36]/100),
                         x = as.numeric(c(data_piri$diff_in_days[7:12],data_piri$diff_in_days[19:24],data_piri$diff_in_days[31:36])))

test_data_Zamb_M <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                         data=data_zamb_MUD, 
                         sample_file = "Zamb_piri_MUD_output.csv",
                         iter=1000, chains=4)
print(test_data_Zamb_M)
parm = extract(test_data_Zamb_M);names(parm)
traceplot(test_data_Zamb_M, inc_warmup = TRUE)
nc = seq(0,400,1)

pred_Zamb_M  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

points(100*data_zamb_MUD$y ~ data_zamb_MUD$x,pch=17,col="red")
lines(pred_Zamb_M*100 ~ nc,col="red",lty = 2)
segments(y0=0.4961722*100,y1=0.4961722*100,x0=0,x1=nc[240],lty=2,col="darkred",lwd=2)
segments(y0=0,y1=0.4961722*100,x0=nc[240],x1=nc[240],lty=2,col="darkred",lwd=2)
text(nc[240],4,"240 days", col= "darkred", cex.lab = 1.1)

legend(300,80,legend = c("Cement walls",
                         "Mud walls"),
       col = c("black","red"),
       pch = c(1, 17),
       bty = "n",
       cex = 1.1)
