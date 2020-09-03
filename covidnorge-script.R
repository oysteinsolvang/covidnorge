# Creates dataset "COVID-19 i norske kommuner"
# Produces  "covidnorge.csv"

# Preamble
rm(list=ls())
setwd("/Users/oysteinsolvang/Dropbox/Akademia/02-Undervisning/COVID-datasett/02-Kommunedata/")
Sys.setlocale(locale="no_NO")

# Libraries
library(tidyverse) # Various data management
library(stargazer) # tables for codebook
library(stringi) # stri_sub function
library(PxWebApiData) # SSB API
library(readxl) # excel file import

## ID-variabler ##

# Variables kommunenavn and kommunenummer
data <- read.csv2("kommunenavndata.csv")
data <- data[,c(1,4)]
colnames(data) <- c("kommunenummer","kommunenavn")

# Variable fylkesnummer
data$fylkesnummer <- stri_sub(data$kommunenummer,1,2)
data$fylkesnummer <- as.integer(data$fylkesnummer)
# Fix error caused by Oslo's county number beginning with a '0'
data$fylkesnummer[data$kommunenummer==301] <- 3

# Variable fylkesnavn
d <- read.csv2('fylkesnavndata.csv',encoding="UTF-8")
# Remove obsolote counties:
d$Status <- ifelse(d$Status=="Retired",NA,d$Status)
d <- na.omit(d)
# Remove superflous variables
d <- d[,c(1,7)]
# Set colnames, merge and reorder columns
colnames(d) <- c("fylkesnummer","fylkesnavn")
data <- merge(data,d,by="fylkesnummer")
data <- data[,c(1,4,2,3)]


## SSB ##

# Variables over69, andelover69 and folketall

ssbdata1 <- ApiData("http://data.ssb.no/api/v0/en/table/07459",
        Region = TRUE,
        ContentsCode = "Personer1",
        Alder = TRUE,
        use_factors = TRUE,
        Tid = c(-1))

d <- ssbdata1
# Selecting variables of interest
d <- as.data.frame(d)
d <- d[,c(7:9,11:12)]
colnames(d) <- c("kommunenummer","Sex","Age","Time","Value")
# Long to wide
d <- spread(d, key = Sex, value = Value)
# Adding males and females, removing male and female variables
d$Value <- d[,4] + d[,5]
d <- d[,c(1:3,6)]
d$Age <- as.numeric(d$Age)
# Long to wide
d <- spread(d, key = Age, value = Value)
# Adding individuals under 70 into one variable
d$u70 <- as.numeric(apply(d[,3:71], 1, sum))
# Adding individuals over 70 into one variable
d$over69 <- as.numeric(apply(d[,72:108],1,sum))
# Adding all individuals to create population variable
d$folketall <- as.numeric(apply(d[,3:108],1,sum))
# Keeping only variables of interest
d <- d %>% select('kommunenummer','over69','folketall')
# Creating variable andelover69
d$andelover69 <- d$over69 / d$folketall
# Coercing kommunenummer as integer
d$kommunenummer <- as.integer(as.character(d$kommunenummer))
# Merging
data <- merge(data,d)


# Variable areal
# Downloaded 2020-07-23 15:28
ssbdata2 <- ApiData("http://data.ssb.no/api/v0/en/table/09280",
        Region=TRUE,
        ContentsCode = "Area",
        use_factors = FALSE,
        Tid = c(-1))

# Selecting variables of interest
d <- as.data.frame(ssbdata2)
d <- d[,c(6:7,9:10)]
# Long to wide
d <- spread(d, key=dataset.Arealtype, value=dataset.value)
# Merging freshwater and land area
d$areal <- d[,3] + d[,4]
# Selecting variables of interest
d <- d %>% select('dataset.Region','areal')
colnames(d) <- c('kommunenummer','areal')
# Coercing kommunenummer as integer
d$kommunenummer <- as.integer(d$kommunenummer)
# Merging
data <- merge(data,d)

# Variable sentralitetsindeks
rawdata <- read.csv2("sentralitetsindeksen.csv")
# Selecting variables of interest, renaming
d <- rawdata[,c(1,3)]
colnames(d) <- c("kommunenummer","sentralitetsindeks")
# Merging
data <- merge(data,d)

# Variable: innvandrere
d <- read.csv2("innvandrere.csv",skip=1)
# Extract kommunenummer (first four digits) from variable 'region'
d$kommunenummer <- str_sub(d$region,1,4)
# Select variables of interest
d <- d[,c(5,4)]
# Rename variables
names <- colnames(d)
names[2] <- c("innvandrere")
colnames(d) <- names
# Coerce 'kommunenummer' as numeric
d$kommunenummer <- as.numeric(d$kommunenummer)
# Coerce 'innvandrere' as numeric
d$innvandrere <- as.numeric(d$innvandrere)
# Mege
data <- merge(data,d,by="kommunenummer")


# Variable: skatteinntekter
d <- read.csv2("skatt.csv",skip=1)
# Extract kommunenummer (first four digits) from variable 'region'
d$kommunenummer <- str_sub(d$region,1,4)
# Select variables of interest
d <- d[,8:9]
# Coerce 'kommunenummer' as numeric
d$kommunenummer <- as.numeric(d$kommunenummer)
# Rename variable 'Skatt.2020M06, coerce to numeric
colnames(d) <- c("skatteinntekter","kommunenummer")
d$skatteinntekter <- as.numeric(d$skatteinntekter)
d <- na.omit(d)
# Mege
data <- merge(data,d,by="kommunenummer")




## Valgdirektoratet

# Variable: senterpartiet
rawdata <- read.csv2("valgresultat2019.csv")
# Selecting variables of interest
d <- rawdata[,c(3,7,9)]
# Removing parties other than Senterpartiet
d$Partikode <- ifelse(d$Partikode!="SP",NA,d$Partikode)
d <- na.omit(d)
# Selecting variables of interest, renaming
d <- d[,c(1,3)]
colnames(d) <- c("kommunenummer","senterpartiet")
# Merging -- using left_join due to NA's
data <- left_join(data,d)


## NAV

# Variable: ledighet
rawdata <- read.csv2("ledighet2.csv",skip=5)
# Selecting variables of interest
d <- rawdata[,1:3]
# Removing rows with miscellaneous content (sub-headers, header repetions, etc.)
d <- d[-c(1:3,5:8,32:35,62:65,107:110,162:165,212:215,239:242,268:271,315:318,357:360,400:410),]
# Renaming variables
colnames(d) <- c("kommune","jan","feb")
# Extracting kommunenummer from variable "kommune", making numeric
d$kommunenummer <- stri_sub(d$kommune,1,4)
d$jan <- as.numeric(sub(",", ".", d$jan, fixed = TRUE))
d$feb <- as.numeric(sub(",", ".", d$feb, fixed = TRUE))
# Creating variable ledighet
d$ledighet <- (d$jan + d$feb) /2
# Making kommunenummer numeric
d$kommunenummer <- as.numeric(d$kommunenummer)
# Selecting variables of interest
d <- d[,c(4,5)]
# Merging
data <- merge(data,d)


## Sametinget

# Variable: STN
d <- read.csv("stn.csv")
# Merging
data <- left_join(data,d, by="kommunenummer")
# Giving units outside the STN-area the value 0
data$STN[is.na(data$STN)] <- 0


## Folkehelseinstituttet

# Variable: tilfeller
# Data import
d <- read.csv2("2020-07-29-covidtilfeller.csv")
# Rename variables
colnames(d) <- c("kommunenavn","tilfeller")
# Merging
d2 <- data %>% select('kommunenavn','kommunenummer')
d2 <- left_join(d2,d,by="kommunenavn")
# Removing false observation on kommunenummer 3034
d2 <- d2[-123,]
# Removing variable kommunenavn
d2 <- d2[,2:3]
# Merging, continued
data <- merge(data,d2,by="kommunenummer")
# Coercing NA to 0, as per information from FHI
data$tilfeller[is.na(data$tilfeller)] <- 0

# Variable: tilfeller1000
data$tilfeller1000 <- data$tilfeller/data$folketall
data$tilfeller1000  <- data$tilfeller1000 * 1000


## Verdens Gang
# Variable: dode
d <- read.csv("2020-07-29-fataliteter.csv")
# Colnames
colnames(d) <- c("kommunenavn","dode")
# Merge
data <- left_join(data,d,by="kommunenavn")
# Coerce NA to 0
data$dode[is.na(data$dode)] <- 0



## Diverse
# Variable: passasjererutland
# Avinor data
rawdata <- read_excel("passasjerer-2019.xlsx",skip=6)
# Convert to table, remove non-relevant variables
d <- as.matrix(rawdata)
d <- d[,c(1,2,5,10)]
d <- as.data.frame(d)
# Rename variables
colnames(d) <- c("IATA","Flyplass","rute","charter")
# Add variables with scheduled and chartered flights
d$rute <- as.numeric(d$rute)
d$charter <- as.numeric(d$charter)
d$passasjererutland <- d$rute + d$charter
# Match municipalities to airport codes
# .. performed in separate script
# .. "iata-to-municipality.r"
rownames(d) <- NULL
write.csv(d,"air-travel-raw.csv")
# Imports matched data
d <- read.csv("air-travel-modified.csv")
d <- d[,-c(1)]
# Merge
data <- left_join(data,d,by="kommunenummer")
# Add data for Sandefjord
passasjerersandefjord <- 1693647 + 38937
data[data$kommunenummer==3804,18] <- passasjerersandefjord
# Data for Harstad/Narvik airport
evenes <- data[data$kommunenummer==5402,18]
evenes <- evenes/2
# Add to Harstad kommune
data[data$kommunenummer==5402,18] <- evenes
# Add to Narvik kommune
data[data$kommunenummer==1806,18] <- evenes
# Coerce NA to 0
data$passasjererutland[is.na(data$passasjererutland)] <- 0


# Variable: karantene
d <- read.csv2("karantene.csv")
# Coerce 'karantene' to numeric
d$karantene <- as.numeric(d$karantene)
# Coerce NA to 0
d$karantene[is.na(d$karantene)] <- 0
# Select variables
d <- d[,c(2,4)]
# Rename variables
colnames(d) <- c("kommunenummer","karantene")
# Merge
data <- merge(data,d,by="kommunenummer")

## variable: 'over1'
data$over1 <- ifelse(data$tilfeller1000 >= 1, 1,0)

# Coerce kommunenummer to character
data$kommunenummer <- as.character(data$kommunenummer)
data[1,1] <- c("0301")
# Coerce fylkesnummer to character
data$fylkesnummer <- as.character(data$fylkesnummer)
data$fylkesnummer[data$kommunenummer=="0301"] <- c("03")


# Reorder data frame
data <- data[,c(1,4,2,3,15,16,20,17,19,8,6,5,7,9,13,10,11,14,12,18)]

# Insert variable 'nordnorge'
data$nordnorge <- ifelse(data$fylkesnummer==54,1,
                  ifelse(data$fylkesnummer==18,1,0))
data <- data[,c(1:4,21,5:20)]


# TO SAVE DATA FRAME WHEN ENDING SESSION:
# write.csv(data,"data-temp.csv")

# TO RESUME SESSION:
# data <- read.csv("data-temp.csv",encoding="UTF-8")

# WHEN COMPLETE:
write.csv(data,"covidnorge.csv",row.names=FALSE)

# SAVE AS TAB SEP TXT FILE
# write.table(data,file="covidverden_tab.txt",sep="\t",fileEncoding="UTF-8")





## Writing tables for codebook ##

# SSB
df  <-  data %>% select("over69")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'over69'")), 'over69')
df  <-  data %>% select("folketall")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'folketall'")), "folketall")
df  <-  data %>% select("andelover69")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'andelover69'")), "andelover69")
df  <-  data %>% select("areal")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'areal'")), "areal")
df  <-  data %>% select("sentralitetsindeks")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'sentralitetsindeks'")), "sentralitetsindeks")
df  <-  data %>% select("innvandrere")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'innvandrere'")), "innvandrere")
df  <-  data %>% select("skatteinntekter")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'skatteinntekter'")), "skatteinntekter")


# Valgdirektoratet
df  <-  data %>% select("senterpartiet")
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'senterpartiet'")), "senterpartiet")

# NAV
df  <-  data %>% select(ledighet)
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'ledighet'")), "ledighet")

# SAMETINGET
df  <-  data %>% select('STN')
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'STN'")), "STN")

# FHI
df  <-  data %>% select('tilfeller')
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'tilfeller'")), "tilfeller")
df  <-  data %>% select('tilfeller1000')
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'tilfeller1000'")), "tilfeller1000")
df  <-  data %>% select('over1')
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'over1'")), "over1")

# DIVERSE
df  <-  data %>% select('passasjererutland')
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'passasjererutland'")), "passasjererutland")
df  <-  data %>% select('karantene')
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'karantene'")), "karantene")

# VG
df  <-  data %>% select('dode')
writeLines(capture.output(stargazer(df,summary=TRUE,title="Fordeling av verdier på variabelen 'dode'")), "dode")
