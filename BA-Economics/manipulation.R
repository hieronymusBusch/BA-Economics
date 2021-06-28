##########################################################
#                                                        #
# BA Econonomics:                                        #
# "The political economy of appliance replacement        #
#  programs for low-income households:                   #
#  An empirical study."                                  #
#                                                        #
# 2021/06                                                #
#                                                        #
# Alexander Busch (alexander.busch@stud.uni-hd.de)       #
#                                                        #
# Data Manipulation                                      #
#                                                        #
##########################################################


## read in libraries
library(sf)
library(readxl)
library(plyr)
library(stringr) 
library(ggpubr)
library(stargazer)

# for some reason tidyverse doesnt load packages?!

library(dplyr)
library(ggplot2)
library(purrr)

# disable scientific notation (1.25e+2 => 125)
options(scipen = 99)  

# set system messages to english
Sys.setenv(lang = "en_US")

## read in data frames & manipulate

setwd("C:/Users/alexa/Google Drive/Studium/Heidelberg/Economics/2021_Econ_Bachelorarbeit/R Skript BA Econ")

# dfds contains shapefile for German county-data, but also some waterways that need to be dropped 
dfds <- st_read("Data/shapefiles/250_NUTS3.shp")
dfds <- subset(dfds, select = c(3,4))
dfds <- dfds[-c(402:428),]

# dfdd contains the actual data on county-level
dfdd <- read_excel("Data/data-socioecon.xls", col_types = c("guess", "skip", "guess", rep("numeric",6)))

dfdd <- dfdd[-1,]
names(dfdd) <- c("KRS", "countyCity", 
                 "unemployment","turnout", "debtpc", 
                 "popDensity", "hhHousingSup","GDPpc")
# east-west dummy and state variables 
dfdd$east <- ifelse(
  str_detect(dfdd$KRS, "^12")|str_detect(dfdd$KRS, "^13")|
    str_detect(dfdd$KRS, "^14")|str_detect(dfdd$KRS, "^15")|
    str_detect(dfdd$KRS, "^16")|str_detect(dfdd$KRS, "^11")
  ,1,0)
dfdd$state <- 0
dfdd$state[str_detect(dfdd$KRS, "^01")] <- "SH"
dfdd$state[str_detect(dfdd$KRS, "^02")] <- "HH"
dfdd$state[str_detect(dfdd$KRS, "^03")] <- "NI"
dfdd$state[str_detect(dfdd$KRS, "^04")] <- "HB"
dfdd$state[str_detect(dfdd$KRS, "^13")] <- "MV"
dfdd$state[str_detect(dfdd$KRS, "^12")] <- "BB"
dfdd$state[str_detect(dfdd$KRS, "^11")] <- "BE"
dfdd$state[str_detect(dfdd$KRS, "^15")] <- "ST"
dfdd$state[str_detect(dfdd$KRS, "^14")] <- "SN"
dfdd$state[str_detect(dfdd$KRS, "^16")] <- "TH"
dfdd$state[str_detect(dfdd$KRS, "^05")] <- "NW"
dfdd$state[str_detect(dfdd$KRS, "^06")] <- "HE"
dfdd$state[str_detect(dfdd$KRS, "^07")] <- "RP"
dfdd$state[str_detect(dfdd$KRS, "^09")] <- "BY"
dfdd$state[str_detect(dfdd$KRS, "^10")] <- "SL"
dfdd$state[str_detect(dfdd$KRS, "^08")] <- "BW"

dfdd$city <- ifelse(dfdd$countyCity == "Landkreis",0,1)

#################### Schulden?! 

# dfmerge contains both referencing systems of df and regional politicall variables
dfmerge <- read_excel("Data/data-merge-countyPol.xlsx", sheet = 2, range = "A6:I478", 
                      col_types = c("guess", "skip", "guess", 
                                    "guess", "numeric", "numeric",
                                    "guess", "skip", "numeric"))
names(dfmerge) <- c("KRS","name", "councilParty", "councilFem", "councilChange",
                    "NUTS_CODE", "population")

# delete observations that are states, not counties, and other rows without data
dfmerge$KRSnew <- dfmerge$KRS
dfmerge$KRSnew <- as.numeric(as.character(dfmerge$KRSnew))
dfmerge <- subset(dfmerge, KRSnew > 999)
dfmerge$KRSnew <- NULL
dfmerge$councilLeft <- ifelse(dfmerge$councilParty == "SPD" | dfmerge$councilParty == "Linke" | 
                         dfmerge$councilParty == "Grüne", 1,0)

# Combine dfdd and dfmerge for data analysis 
dfdd <- merge(dfmerge, dfdd, by.dfmerge = KRS, by.dfdd = KRS)

# Read in data on female share in regional parliaments
dfcountyWomen <- read_excel("Data/data-countyWomen.xlsx", range = "A1:F2105", 
                     col_types = c("guess", "skip", "skip", "skip", "skip", 
                                   "numeric"))
names(dfcountyWomen) <- c("KRS", "parlFem")
dfcountyWomen <- dfcountyWomen[-c(1:1306),]
dfcountyWomen <- dfcountyWomen[-c(12:397),]
dfcountyWomen <- dfcountyWomen[-c(16:26),]
dfcountyWomen$KRS <- ifelse(dfcountyWomen$KRS < 10000, 
                            paste0("0", as.character(dfcountyWomen$KRS)),
                            as.character(dfcountyWomen$KRS))
dfdd <- merge(dfcountyWomen, dfdd, by.dfcountyWomen = KRS, by.dfdd = KRS)

# Read in Program data, delete commercial / welfare programs, and merge with dfdd
dfprog <- read_excel("Data/data-program.xlsx", sheet = 1, range = "A3:J145", 
                     col_types = c("skip", "guess", "skip", "numeric", "numeric", 
                                   "guess", "guess", "guess", "guess", "numeric"))
names(dfprog) <- c("NUTS_CODE", "response", "addSup", "addSupSource",
                   "addSupLOANamount", "addSupGRANTamountHH1", 
                   "addSupGRANTamountHH2", "inProgram")
dfprog <- dfprog[!(dfprog$addSupSource=="gov;welfOt" | dfprog$addSupSource=="welfOt"
                   | dfprog$addSupSource=="welfCh" | dfprog$addSupSource=="com"
                   | dfprog$addSupSource=="com;welfCh"),]
dfdd <- merge(dfdd, dfprog, by.dfdd = NUTS_CODE, by.dfprog = NUTS_CODE, all = TRUE)
dfdd <- dfdd[-274,]
dfdd["inProgram"][is.na(dfdd["inProgram"])] <- 0

# Data for Religion
    # some county-merges need to be accounted for
    # share of people of certain faiths computed
dfrel <- read_excel("Data/data-religion.xlsx", sheet = 3, range = "A11:DS12553", 
                     col_types = c("text", rep("skip",5), "text", rep("skip",112),rep("numeric",4)))
names(dfrel) <- c("KRS", "name", "zenPop", "catholic", "protestant","other")
dfrel <- subset(dfrel, str_length(KRS) == 5)

dfrel[21,3:6] <- dfrel[21,3:6] + dfrel[25,3:6]
dfrel[362,3:6] <- dfrel[362,3:6] + dfrel[359,3:6] + dfrel[345,3:6]
dfrel[361,3:6] <- dfrel[361,3:6] + dfrel[349,3:6] + dfrel[357,3:6]
dfrel[354,3:6] <- dfrel[354,3:6] + dfrel[360,3:6]
dfrel[352,3:6] <- dfrel[352,3:6] + dfrel[356,3:6] + dfrel[355,3:6] + dfrel[346,3:6]
dfrel[351,3:6] <- dfrel[351,3:6] + dfrel[353,3:6]
dfrel[358,3:6] <- dfrel[358,3:6] + dfrel[350,3:6]

dfrel[21,1] <- "03159"
dfrel[362,1] <- "13075"
dfrel[361,1] <- "13073"
dfrel[354,1] <- "13076"
dfrel[352,1] <- "13071"
dfrel[351,1] <- "13072"
dfrel[358,1] <- "13074"

dfrel <- dfrel[-c(359,345,349,357,360,356,355,346,353,350,25),]

dfrel$cathShare <- 0
dfrel$protShare <- 0
dfrel$othShare <- 0
dfrel$cathShare <- 100 * dfrel$catholic / dfrel$zenPop
dfrel$protShare <- 100 * dfrel$protestant / dfrel$zenPop
dfrel$othShare <- 100 * dfrel$other / dfrel$zenPop

dfrel <- dfrel[,-c(2:6)]
dfdd <- merge(dfdd, dfrel, by.dfdd = KRS, by.dfrel = KRS)

# further variables
dfdd$progState <- 0 
dfdd$progState <- ifelse(dfdd$state == "NW" | dfdd$state == "ST" | dfdd$state == "HH"
                         | dfdd$state == "BE", 1, 0)
dfdd$popDensityHun <- dfdd$popDensity / 100
dfdd$debtpcHun <- dfdd$debtpc / 100

# Finally, merge dfdd and dfds for maps
dfds <- merge(dfds, dfdd, by.dfds = NUTS_CODE, by.dfdd = NUTS_CODE)


# df with only counties participating in program & without states with state support

dfddProg <- subset(dfdd, inProgram == 1)
dfddProgNoSt <- subset(dfddProg, progState == 0)
