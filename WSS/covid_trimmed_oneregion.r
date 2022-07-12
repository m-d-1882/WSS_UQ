#!/usr/bin/env Rscript
#
# Weight, Scale and Shift (WSS) Code
#
# Copyright 2021 Graeme J Ackland, Mario Antonioletti, The University of Edinburgh,
#                James A Ackland, The University of Cambridge
#                David J Wallace.
#
# This code is made available under a GPL-3.0 License.
#
# Data used in making calculations is made available under an Open Government
# Licence. For more details see:
#
# http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
#


# From Jan 6th  2022 Need to multiply cases in Scotland by fraction which are LFT and not reported, because, Scotland
#  Enter this array by hand copied from https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/
# Another problem : Weekend behaviour of LFT is completely different to PCR


#  Italian data is here ... https://github.com/InPhyT/COVID19-Italy-Integrated-Surveillance-Data


# Read packages used by the script
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate, warn.conflicts = FALSE, quietly = TRUE)
library(zoo, warn.conflicts = FALSE, quietly = TRUE)
library(RColorBrewer, warn.conflicts = FALSE, quietly = TRUE)


source("CompartmentFunction_ensemble.R")
source("medrxiv.R")
source("Predictions.R")
source("age_pdfplot.R")
source("Weekend.R")
source("CC_write.R")
source("covidSimData.R")
# Set the working directory to be the same as to where the script is run from.
setwd(".")

# Turn off scientific notation.
options(scipen = 999)

#### Read data ####
# distributions and populations
covidsimAge<-covidSimData()
population<-getPop()
# Base URL to get the UK government data
baseurl <- "https://api.coronavirus.data.gov.uk/v2/data?"



#  Dates for the plots
plotdate <- as.Date(c(as.character(startdate),as.character(enddate)))
# Wanted to plot a Smooth spline discontinuous at
# UK lockdown Oct 31 (day 98) -Dec 2  (day 130) Jan 6 (day 165)  (day 1 = July 25)
lock1 <- as.integer(as.Date("2020/10/31")-startdate)
unlock1 <- as.integer(as.Date("2020/12/02")-startdate)
lock2 <- as.integer(as.Date("2021/01/06")-startdate)
unlock2 <- as.integer(as.Date("2021/03/08")-startdate)
unlock3 <- as.integer(as.Date("2021/04/12")-startdate)
test_delay <- 12
lock1 <- lock1+test_delay
unlock1 <- unlock1+test_delay
lock2 <- lock2+test_delay
unlock2 <- unlock2+test_delay
unlock3 <- unlock3+test_delay

sagedelay <- 16 # Delay in producing R-number, for plots
#  Initiate list for Hospital data
Hospital<-list()

# Total cases, deaths, tests England
casesurl <- paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=E92000001&",
                   "metric=newCasesBySpecimenDate&",
                   "metric=newDeaths28DaysByDeathDate&",
                   "metric=newPCRTestsByPublishDate&",
                   "metric=newPeopleVaccinatedFirstDoseByVaccinationDate&",
                   "metric=covidOccupiedMVBeds&",
                   "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_integer(),
                 col_integer(),  col_integer(), col_integer())

# Read the cases, deaths and tests data
comdat <-  read_csv(file = 'csv_files/casesurl.csv', col_types = coltypes)



# Transform the data
comdat <- comdat %>%  select(date,
                             allCases = newCasesBySpecimenDate,
                             allDeaths = newDeaths28DaysByDeathDate,
                             tests = newPCRTestsByPublishDate,
                             inputCases = newCasesBySpecimenDate,
                             fpCases = newCasesBySpecimenDate,
                             vaccines=newPeopleVaccinatedFirstDoseByVaccinationDate,
                             MVBeds=covidOccupiedMVBeds)%>%
  filter(date >= startdate &
           date <= enddate ) %>%
  arrange(date)

newurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumAdmissions&metric=hospitalCases&metric=newAdmissions&format=csv"

# Explicitly define the types for the columns  Need to repeat call because??? data download restriction
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"),
                 col_integer(),  col_integer(), col_integer())


tempdata<-read_csv(file = 'csv_files/newurl.csv', col_types = coltypes)

tempdata <-  tempdata %>%  select(date,
                            admissions = newAdmissions,
                             hospital = hospitalCases,
                             )%>%
  filter(date >= startdate & date <= enddate ) %>%   arrange(date)

comdat<-  cbind(comdat,tempdata[2:3])
rm(tempdata)
# All UK cases (to estimate pre-Sept England Cases)

ukcaseurl <- paste0(baseurl,
                    "areaType=overview&",
                    "metric=newPCRTestsByPublishDate&",
                    "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_double())

# Read the case data
ukcasedat <-  read_csv(file = 'csv_files/ukcaseurl.csv', col_types = coltypes)

# Transform the case data
ukcasedat <- ukcasedat %>%
             select(date = date, tests = newPCRTestsByPublishDate) %>%
             filter(date >= startdate & date <= enddate ) %>%
             arrange(date)

# cases by age
ageurl <- paste0(baseurl,
                 "areaType=nation&",
                 "areaCode=E92000001&",
                 "metric=newCasesBySpecimenDateAgeDemographics&",
                 "format=csv")

# Explicitly define the types for the columns
# Age is a character as it giving a range, e.g. 00_04, 05_09, ...
coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_character(),
                 col_integer(), col_integer(), col_number())

# read in the cases by age data
casedat <-  read_csv(file = 'csv_files/ageurl.csv', col_types = coltypes)

# Remap the ages column to be the header rows, remove the unassigned,
# 60+ and 00_59 columns, filter dates to be between the start and end
# dates and order the output by date
casedat <- casedat %>%
  select(date = date, age = age, values = cases) %>%
  pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
  select(-unassigned, -"60+", -"00_59") %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date)

vacdate="2020-12-08"
# vaccination by age
vacurl <- paste0(baseurl,
                 "areaType=nation&",
                 "areaCode=E92000001&",    # England
                 "metric=vaccinationsAgeDemographics&",
                 "format=csv")

# Explicitly define the types for the columns as same with cases
# Explicitly define the types for the columns
# Age is a character as it giving a range, e.g. 00_04, 05_09, ...
coltypes <- cols(areaCode=col_character(), areaName=col_character(),areaType=col_character(),
                 date=col_date(format="%Y-%m-%d"), age=col_character())

# Read in the vaccination data.
vacdat <-  read_csv(file = 'csv_files/vacurl.csv', col_types = coltypes)

# Transform the data to get vacdat compatible with casedat (must be a better way!).
vacdat <- vacdat %>%
  select(datetmp = date, age = age, values = cumVaccinationFirstDoseUptakeByVaccinationDatePercentage) %>%
  pivot_wider(id_cols = datetmp, names_from = age, values_from = values) %>%
  filter(datetmp >= vacdate  & datetmp <= enddate) %>%
  arrange(datetmp)

# Add vaccination data for the under 24s.

# casedat has age groups 00-04, 05-09, 10-14, 15-19, 20-24, rest are the same
# vacdat has age groups  16-17 18-24, rest are the same
vacdat<-cbind('20_24'=vacdat$'18_24',vacdat)
vacdat<-cbind('15_19'=0.4*vacdat$'18_24'+0.4*vacdat$'16_17',vacdat)
vacdat<-cbind('10_14'=0.0,vacdat)
vacdat<-cbind('05_09'=0.0,vacdat)
vacdat<-cbind('00_04'=0.0,vacdat)
vacdat<-cbind(date=vacdat$datetmp,vacdat)
vacdat$`18_24`<-NULL
vacdat$datetmp<-NULL
#  Extend vacdat to before programme started with zeroes
tmp<-NULL
tmp<-casedat %>% filter(date < vacdate)
tmp[2:20]=0.0

vacdat <- bind_rows(tmp,vacdat)
rm(tmp)

# convert to fraction
vacdat[2:length(vacdat)]<-vacdat[2:length(vacdat)]/100.0

# deaths by age
deathurl <- paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=E92000001&",
                   "metric=newDeaths28DaysByDeathDateAgeDemographics&",
                   "format=csv")

# Explicitly define the types for the columns
coltypes <- cols(areaCode=col_character(), areaName=col_character(),areaType=col_character(),
                 date=col_date(format="%Y-%m-%d"),age=col_character(),
                 deaths=col_number(), rollingSum=col_number(), rollingRate=col_number())

# Read the deaths by age data
deathdat <-  read_csv(file = 'csv_files/deathurl.csv', col_types = coltypes)

# Map the ages column to become column headings for the different age groups
# for dates between the start and end date inclusive and then ensure that we
# end up with the same columns as for the case data above.
deathdat <- deathdat %>%
  select(date = date, age = age, values = deaths) %>%
  pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
  select(-"60+", -"00_59") %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date) %>%
  select(names(casedat))#deaths by age

# Scotland data https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=S92000003&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&metric=newDeaths28DaysByPublishDate&format=csv
# https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/9393bd66-5012-4f01-9bc5-e7a10accacf4

scoturl <-  paste0(baseurl,
                   "areaType=nation&",
                   "areaCode=S92000003&",
                   "metric=newDeaths28DaysByDeathDate&",
                   "metric=newCasesBySpecimenDate&",
                   "metric=newDeaths28DaysByPublishDate&",
                   "format=csv")
coltypes <-  cols(
  date = col_date(format = "%Y-%m-%d"),
  newCasesBySpecimenDate = col_number(),
  newDeaths28DaysByPublishDate = col_number(),
  newDeaths28DaysByDeathDate = col_number()
)
#  Trying and failing to get data from PHS
#scotdeaths<- read.csv(file="https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id=9393bd66-5012-4f01-9bc5-e7a10accacf4")
#
# Data is not being returned as a CSV but JSON you have to use:
#
# library(jsonlite)
# d <- jsonlite::fromJSON("https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id=9393bd66-5012-4f01-9bc5-e7a10accacf4",
#                         flatten = TRUE)
#
# This still returns contents as a list so you will have to rummage around to extract the actual contents that you require in
# the data structure returned.

# Wales data https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=newCasesBySpecimenDate&metric=newDeaths28DaysByDeathDate&format=csv

walesurl <-  paste0(baseurl,
                    "areaType=nation&",
                    "areaCode=W92000004&",
                    "metric=newCasesBySpecimenDate&",
                    "metric=newDeaths28DaysByDeathDate&",
                    "format=csv")
coltypes <-  cols(
  date = col_date(format = "%Y-%m-%d"),
  newCasesBySpecimenDate = col_number(),
  newDeaths28DaysByDeathDate = col_number()
)

# Read in the Welsh deaths and case data
walesdat <-  read_csv(file = 'csv_files/walesurl.csv', col_types = coltypes)

# Transform the data
walesdat <- walesdat %>%  select(date,
                                 allCases = newCasesBySpecimenDate,
                                 allDeaths = newDeaths28DaysByDeathDate,
                                 inputCases = newCasesBySpecimenDate) %>%
                          filter(date >= startdate &
                                 date <= enddate ) %>%
                           arrange(date)

# Get the Northern Irish deaths and case data
NIurl <-  paste0(baseurl,
                 "areaType=nation&",
                 "areaCode=N92000002&",
                 "metric=newCasesBySpecimenDate&",
                 "metric=newDeaths28DaysByDeathDate&",
                 "format=csv")


# Read in the NI deaths and case data
NIdat <-  read_csv(file = 'csv_files/NIurl.csv', col_types = coltypes)

# Transform the data
NIdat <- NIdat %>%  select(date,
                           allCases = newCasesBySpecimenDate,
                           allDeaths = newDeaths28DaysByDeathDate,
                           inputCases = newCasesBySpecimenDate) %>%
                    filter(date >= startdate &
                           date <= enddate ) %>%
                    arrange(date)

# Regional data for deaths and cases by specimen date
regurl <- paste0(baseurl,
                 "areaType=region&",
                 "metric=newDeaths28DaysByDeathDate&",
                 "metric=newCasesBySpecimenDate&",
                 "format=csv")

# Specify the column types
coltypes <-  cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  newCasesBySpecimenDate = col_number(),
  newDeaths28DaysByDeathDate = col_number()
)

# Read in the regional case and death data
regdat <-  read_csv(file = 'csv_files/regurl.csv', col_types = coltypes)


# Transform the data
regcases <- regdat %>%  select(date, areaName, areaCode,
                               Cases = newCasesBySpecimenDate) %>%
                        pivot_wider(id_cols = date,
                                    names_from = areaName,
                                    values_from = Cases) %>%
                        filter(date >= startdate &
                               date <= enddate )%>%
                        arrange(date)

# Map the rows in the areaType column to become columns and map the death data
# to lie under the corresponding column.
regdeaths <- regdat %>%
             select(date, areaName, Deaths = newDeaths28DaysByDeathDate) %>%
             pivot_wider(id_cols = date,
                         names_from = areaName, values_from = Deaths) %>%
             filter(date >= startdate &
                    date <= enddate )%>%
             arrange(date)
regdeaths$NEY=regdeaths$`North East`+regdeaths$`Yorkshire and The Humber`
regdeaths$MD=regdeaths$`East Midlands`+regdeaths$`West Midlands`
# Get the demographic data for regions because can't download simultaneously with
# the death data.
regurl2 <- paste0(baseurl,
                  "areaType=region&",
                  "metric=newCasesBySpecimenDateAgeDemographics&",
                  "format=csv")
regurl3 <- paste0(baseurl,
                  "areaType=region&",
                  "metric=newDeaths28DaysByDeathDateAgeDemographics&",
                  "format=csv")

# Specify the column types
coltypes <- cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  age = col_character(),
  cases = col_number(),
  rollingSum = col_number(),
  rollingRate = col_number()
)

# Read in the regional case and death data by age and filter
# Transform the data - reduce the number of columns and filter the data to
# lie between specific dates.
regagedat <-  read_csv(file = 'csv_files/regurl2.csv', col_types = coltypes) %>%
  select(date, areaName, age, cases) %>%
  filter(date >= startdate &
           date <= enddate ) %>%
  filter(age!="unassigned") %>%
  arrange(date)

# And do it again for deaths to avoid 500 error Specify the column types
regurl3 <- paste0(baseurl,
                  "areaType=region&",
                  "metric=newDeaths28DaysByDeathDateAgeDemographics&",
                  "format=csv")
coltypes <- cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  age = col_character(),
  deaths = col_number(),
  rollingSum = col_number(),
  rollingRate = col_number()
)

regagedat3 <-  read_csv(file = 'csv_files/regurl3.csv', col_types = coltypes)  %>%
  select(date, areaName, age, deaths) %>%
  filter(date >= startdate &
           date <= enddate ) %>%
  arrange(date)
regagedat$deaths <- regagedat3$deaths
# Define the columns for the UK government R estimate data from a csv file
coltypes <- cols(
  Date = col_date(format = "%d/%m/%Y"), UK_LowerBound = col_number(),
  UK_UpperBound = col_number(), England_LowerBound = col_number(),
  England_UpperBound = col_number(), EEng_LowerBound = col_number(),
  EEng_UpperBound = col_number(), Lon_LowerBound = col_number(),
  Lon_UpperBound = col_number(), Mid_LowerBound = col_number(),
  Mid_UpperBound = col_number(), NEY_LowerBound = col_number(),
  NEY_UpperBound = col_number(), NW_LowerBound = col_number(),
  NW_UpperBound = col_number(), SE_LowerBound = col_number(),
  SE_UpperBound = col_number(), SW_LowerBound = col_number(),
  SW_UpperBound = col_number()
)

# Read in the data - this data is obtained by a different script.
Rest <- read_csv(file="data/R_estimate.csv", col_types = coltypes)


# Scottish URL from which to pull the data
dailycasesurl = "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/2dd8534b-0a6f-4744-9253-9565d62f96c2/download/trend_hb_20210610.csv"

# Column types  Amended 10/02/2022
coltypes <- cols(
  Date = col_date(format = "%Y%m%d"),
  HB = col_character(),
  HBName = col_character(),
  DailyPositive = col_double(),
  CumulativePositive = col_double(),
  DailyPositivePCROnly = col_double(),
  CumulativePositivePCROnly = col_double(),
  DailyPositiveLFDOnly = col_double(),
  CumulativePositiveLFDOnly = col_double(),
  DailyPositivePCRAndLFD = col_double(),
  CumulativePositivePCRAndLFD = col_double(),
  DailyDeaths = col_double(),
  CumulativeDeaths = col_double(),
  CrudeRateDeaths = col_double(),
  PositiveTests = col_double(),
  PositiveTestsLFDOnly = col_double(),
  HospitalAdmissions = col_double(),
  HospitalAdmissionsQF = col_character(),
  ICUAdmissions = col_double(),
  ICUAdmissionsQF = col_character(),
  PositivePillar1 = col_double(),
  PositivePillar2 = col_double()
)


# Get the data
scotdailycases = read_csv('csv_files/dailycasesurl.csv', col_types = coltypes)

# Make the NHS boards the columns - DailyPositives are the values of PCR+LFT from 10/02/2022
scotdailycases %>% select(date=Date,board=HBName, cases=DailyPositive)  %>%
  pivot_wider(names_from = board, values_from = cases) %>%
  filter(date >= startdate & date <= enddate )         %>%
  arrange(date) -> scotdailycasesbyboard

# Hospital data
scotdailycases %>% filter(HBName=="Scotland") %>%
  select(date = Date, newcritdat = ICUAdmissions, newsaridat = HospitalAdmissions) %>%
  filter(date >= startdate & date <= enddate) %>%
  arrange(date) -> jnk
Hospital$Scot$date<-as.Date(jnk$date)
Hospital$Scot$newcritdat<-jnk$newcritdat
Hospital$Scot$newsaridat<-jnk$newsaridat


# Make the NHS boards the columns - DailyPositives are the values
scotdailycasesbyboard <- scotdailycases   %>%
  select(date=Date,board = HBName,
         cases = DailyPositive)                 %>%
  pivot_wider(names_from = board,
              values_from = cases)              %>%
  filter(date >= startdate & date <= enddate )  %>%
  arrange(date)
# Make the NHS boards the columns - DailyPositives are the values
scotdailydeathsbyboard <- scotdailycases   %>%
  select(date=Date,board = HBName,
         deaths = DailyDeaths)                 %>%
  pivot_wider(names_from = board,
              values_from = deaths)              %>%
  filter(date >= startdate & date <= enddate )  %>%
  arrange(date)

# Join the scotdailycases with regcases by date
regcases <- inner_join(regcases, scotdailycasesbyboard, by = c("date" = "date"))

#### Get tests for England pre-Sept by taking the post-Sept fraction of all tests
#    that were in England (0.867), and set vaccines to zero
comdat$tests[1:58] <- as.integer(ukcasedat$tests[1:58] * 0.867)
comdat$vaccines[is.na(comdat$vaccines)] <- 0.0
#### Get the UK hospital data & Append data to tibble
####  MV beds = CRIT
####  hospitalCases = SARI+CRIT+CRITREC
HospitalUrl <- paste0(baseurl,
                      "areaType=overview&",
                      "metric=covidOccupiedMVBeds&",
                      "metric=hospitalCases&",
                      "metric=newAdmissions&",
                      "format=csv")

# Column types
coltypes <-  cols(
  areaCode = col_character(),
  areaName = col_character(),
  areaType = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  covidOccupiedMVBeds = col_double(),
  hospitalCases = col_double(),
  newAdmissions = col_double()
)

# Get the hospital data

jnk <- read_csv('csv_files/HospitalUrl.csv', col_types = coltypes)

Hospital$UK <- tibble()
Hospital$UK  <-  jnk %>%
                  select(date = date, saridat = hospitalCases, newsaridat = newAdmissions, critdat=covidOccupiedMVBeds) %>%
                  filter(date >= startdate & date <= enddate ) %>%
                  arrange(date)
Hospital$UK$saridat <- na.locf(Hospital$UK$saridat)
Hospital$UK$newsaridat <- na.locf(Hospital$UK$newsaridat)
Hospital$UK$critdat <- na.locf(Hospital$UK$critdat)


# Add the Welsh and Northern Ireland cases data,  no age stratifications 
# Problems can occur here if these are updates before/after UK

regcases$Wales <- walesdat$allCases[1:nrow(regcases)]
regcases$NI <- NIdat$allCases[1:nrow(regcases)]
na.locf(regcases$Wales)
na.locf(regcases$NI)


#remove random new Scottish "region"
within(regcases, rm("Golden Jubilee National Hospital"))->jnk
regcases<-jnk
# Remove the no longer needed input data  Keep Scottish data for SCottishData.R
rm(ukcasedat,jnk,coltypes,NIdat,walesdat,regagedat3)
rm(HospitalUrl,deathurl,casesurl,scoturl,walesurl,NIurl,ageurl,baseurl,regurl,regurl2,regurl3,ukcaseurl,vacurl)

# Plot all cases against date: Used for the paper, uncomment to recreate
if(interactive()){
    comdat %>%
    filter(startdate+150 <= date & date <= enddate) %>%
    mutate(rollmean = zoo::rollmean(allCases, k = 7, fill = NA)) %>%  # 7-day average
    ggplot(aes(x=date)) + geom_line(aes(y=allCases)) + geom_point(aes(y=allCases)) +
    geom_line(aes(y=rollmean), colour="pink",na.rm = TRUE, size=2, alpha=0.5) +
    xlab("Date") + ylab("All cases")
}

# Scotland tail correction.  Assumes we read in all but the last row
if(enddate == (Sys.Date()-1)){
  regcases[nrow(regcases),2:ncol(regcases)]=regcases[nrow(regcases),2:ncol(regcases)]*1.05
  regcases[nrow(regcases-1),2:ncol(regcases)]=regcases[nrow(regcases-1),2:ncol(regcases)]*1.005
  regcases[nrow(regcases),2:ncol(regcases)]=regcases[nrow(regcases),2:ncol(regcases)]*1.05
  regcases[nrow(regcases-1),2:ncol(regcases)]=regcases[nrow(regcases-1),2:ncol(regcases)]*1.005
}

# Add variant data to comdat  Kentfac tells us how much more lethal the variant is
# Numbers are fitted to death and hospitalisation data
comdat$Kent <- 0.0
comdat$India <- 0.0
comdat$Omicron <- 0.0

Kentdate <- as.integer(as.Date("2021/01/01")-startdate)

# Approximate Kent by logistic rise around 2021/01/01
# Same gen time, R+KEnttrans vs Wild  (transmission, NOT lethality factor)
for (i in 1:(nrow(comdat))){
  x= (i-Kentdate)*Kenttrans/genTime
  comdat$Kent[i]=1.0/(1.0+exp(-x))
}
Indiadate <- as.integer(as.Date("2021/05/15")-startdate)
# Approximate India by logistic rise around 2021/15/01: see covid19.sanger.
# Same genTime R+Indiatrans vs Kent  AY4.2 assumes same as India
for (i in 1:(nrow(comdat))){
  x= (i-Indiadate)*Indiatrans/genTime
  comdat$India[i]=1.0/(1.0+exp(-x))
}
Omicrondate <- as.integer(as.Date("2021/12/15")-startdate)
# Approximate Omicron by logistic rise around 2021/11/28: see covid19.sanger.
# Same genTime R+1 vs India
for (i in 1:(nrow(comdat))){
  x= (i-Omicrondate)*1.0/genTime
  comdat$Omicron[i]=1.0/(1.0+exp(-x))
}

rm(x)
# Kent is Kentfac worse, india is Indiafac worse

comdat$India<-comdat$India - comdat$Omicron
comdat$Kent<-comdat$Kent-comdat$India-comdat$Omicron
comdat$lethality<-1.0+ Kentfac*comdat$Kent + Indiafac*comdat$India + Omicronfac*comdat$Omicron

# Fix missing data to constant values
casedat <- na.locf(casedat)
comdat <- na.locf(comdat)
regcases <- na.locf(regcases)

# Remove weekend effect, assuming each weekday has same number of cases over the
# epidemic, and national averages hold regionally.  Also, smooth data through Xmas.
#  Pulled out comdat & scotdat to a function. Regions need to deal with tibble
comdat$allCases <- Weekend(comdat$allCases)
for (area in 2:length(regcases)){
  regcases[area]<-Weekend(regcases %>% pull(area))
}
for (iage in 2:length(casedat)){
  casedat[iage]<-Weekend(casedat %>% pull(iage))
}

# Build CrystalCast agegroups
xcastage <-casedat %>% select(`00_04`)
xcastage$'05_14' <-casedat$`05_09`+casedat$`10_14`
xcastage$'15_24' <-casedat$`15_19`+casedat$`20_24`
xcastage$'25_44' <-casedat$`25_29`+casedat$`30_34`+casedat$`35_39`+casedat$`40_44`
xcastage$'45_64' <-casedat$`45_49`+casedat$`50_54`+casedat$`55_59`+casedat$`60_64`
xcastage$'65_74' <-casedat$`65_69`+casedat$`70_74`
xcastage$'75+' <-casedat$`75_79`+casedat$`80_84`+casedat$`85_89`+casedat$`90+`

# Combination required to go from 9 to 7 English regions  regcases must be same length in all regions
regcases$NE_Yorks <- regcases$`North East` + regcases$`Yorkshire and The Humber`
regcases$Midlands <- regcases$`East Midlands` + regcases$`West Midlands`

regcases$England <- comdat$allCases[1:nrow(regcases)]

# Reorder regcases
regcases<-regcases[,c(1,2,3,4,5,6,7,9,10,8,23,26,27,11,12,13,14,15,16,17,18,19,20,21,22,24,25,28,29,30)]

# Set false positive adjustment at 0.004, extrapolate tests if the last few days are missing
comdat$fpCases <- comdat$allCases-0.004*as.integer(comdat$tests)

regcases$regions <- regcases$London + regcases$`South East` + regcases$`South West` +
                  regcases$NE_Yorks + regcases$Midlands + regcases$`North West` +
                  regcases$`East of England`

# Plot only if running interactively
if(interactive()){

  plot(comdat$inputCases,x=comdat$date,xlab="Date",ylab="Cases")
  lines(comdat$allCases,x=comdat$date, col="green",lwd=2)
  lines(comdat$fpCases, x=comdat$date,col="red",lwd=2)
  lines(regcases$regions, x=regcases$date,col="blue",lwd=2)

  # Same graph using ggplot - alpha sets a level of transparency
  # between 0 (opaque) to 1 (transparent)
  ggplot(comdat,aes(x=date)) +
    geom_point(aes(y=inputCases),alpha=0.5) +
    geom_line(aes(y=allCases), colour="green", size=1., alpha=0.5) +
    geom_line(aes(y=fpCases),colour="red", size=1., alpha=0.5)  +
    xlab("Dates") + ylab("Cases") +
    theme_bw()
}
##  Case Fatality ratio was determined from initial period 
##   this assumed original startdate 09/08/2020 so is deprecated
##RawCFR=colSums(deathdat[12:211,2:20])/colSums(casedat[1:200,2:20])


# Get mean age-related CFR across the whole pandemic, with adjustment for vaccination
# giving people who would have died.  Alternative would be time dependent CFR
# RawCFR=colSums(deathdat[2:20]/(1-vacdat[2:20]*vacCFR))/colSums(casedat[2:ncol(casedat)])



#  Compartment model now done with a function.  Last two inputs are indices giving date range
#  The compartments will not be correct until the cases have time to filter through all sections, which may be several months for, e.g oldCRITREC
 
compEng <- Compartment(casedat, covidsimAge, RawCFR, comdat,2,nrow(casedat))


# Do not unpack the values returned, access compartment quantities via comp$ list construct


# End of compartment section

# Monitoring plots
if(interactive()){
  # Diagnostic plots in ggplot format
  deathdat %>% rowwise()                               %>%
               mutate(totdeath = sum(c_across(2:20)))  %>%
               select(date,totdeath)                   %>%
               left_join(compEng$DEATH, by="date")        %>%
               mutate(totDEATH = sum(c_across(3:21)))  %>%
               select(date,totdeath, totDEATH)         %>%
               ggplot(aes(x = date, y = totdeath)) + geom_point(alpha = 0.25) +
               geom_line(aes(x = date, y = totDEATH), colour = "blue") +
               theme_bw() + ylab("Total Deaths") + xlab("Date")

  Hospital$UK %>% select(date, MVbeds = critdat) %>%
                     left_join(compEng$CRIT, by = "date")        %>%
                     rowwise()                                %>%
                     mutate(totCrit = sum(c_across(3:21)))    %>%
                     select(date, MVbeds, totCrit)            %>%
                     ggplot(aes(x = date, y = MVbeds)) + geom_point(alpha = 0.25) +
                     geom_line(aes(x = date, y = totCrit), colour = "blue") +
                     theme_bw() + ylab("Occupied MV beds") + xlab("Date")

  Hospital$UK %>% select(date, newsaridat)                       %>%
                     left_join(compEng$newSARI, by ="date")         %>%
                     rowwise()                                   %>%
                     mutate(totnewSARI = sum(c_across(3:21)))    %>%
                     select(date, newsaridat, totnewSARI)        %>%
                     ggplot(aes(x = date, y = newsaridat)) +
                     geom_point(alpha = 0.2) +
                     geom_line(aes(x = date, y = totnewSARI), colour = "blue") +
                     theme_bw() + ylab("New Admissions") + xlab("Date")

  Hospital$UK %>% select(date, saridat)                           %>%
                     left_join(compEng$SARI, by = "date")            %>%
                     rowwise()                                    %>%
                     mutate(totSARI = sum(c_across(3:21)))        %>%
                     select(date, saridat, totSARI)               %>%
                     left_join(compEng$CRIT, by = "date")            %>%
                     rowwise()                                    %>%
                     mutate(totSariCrit = sum(c_across(3:22)))    %>%
                     select(date, saridat, totSariCrit)           %>%
                     left_join(compEng$CRITREC, by = "date")         %>%
                     rowwise()                                    %>%
                     mutate(totSariCritRec = sum(c_across(3:22))) %>%
                     ggplot(aes(x = date, y = saridat)) +
                     geom_point(alpha = 0.2) +
                     geom_line(aes(x = date, y = totSariCritRec), colour = "blue") +
                     theme_bw() + ylab("Hospital cases") + xlab("Date")
}

# Smoothcasedat
smoothcases <- smooth.spline(comdat$allCases, df = 20)

# Create a vector to hold the results for various R-numbers
ninit <- as.numeric(1:nrow(comdat))/as.numeric(1:nrow(comdat))
dfR <- data.frame(x=1.0:length(comdat$date),
                  date=comdat$date, itoR=ninit, stratR=ninit, rawR=ninit,  fpR=ninit,  weeklyR=ninit,  bylogR=ninit,
                  ONS=ninit, p00=ninit,  p05=ninit,  p10=ninit,  p15=ninit,  p20=ninit,  p25=ninit,  p30=ninit,
                  p35=ninit,  p40=ninit,  p45=ninit,  p50=ninit,  p55=ninit,  p60=ninit,  p65=ninit,
                  p70=ninit,  p75=ninit,  p80=ninit,  p85=ninit,  p90=ninit, x05=ninit, x15=ninit,
                  x25=ninit, x45=ninit, x65=ninit, x75=ninit, regions=ninit, smoothcasesR=ninit)
# Ito, Stratanovitch and exponential calculus
# rawR averages cases over previous genTime days - assumes genTime is the same as infectious period

# Check if there are any zero cases in the data
if(any(compEng$CASE==0)){
  for(name in names(compEng$CASE)){
    if(any(compEng$CASE[name]==0)){
      warning("Zero values found for ",name," for the date(s) ",
              paste(compEng$CASE[["date"]][which(compEng$CASE[name]==0)],collapse = ", "),".")
    }
  }
}
rat <- regcases

#  Add ONS data to comdat$
approx(eng_prev,n=7*length(eng_prev))$y %>% tail(nrow(comdat))-> comdat$ons_prev
approx(scot_prev,n=7*length(scot_prev))$y%>% tail(nrow(comdat))-> comdat$scot_ons_prev



for(i in (2:nrow(regcases))    ){
  rat[i, 2:ncol(regcases)] <- 1 + log(regcases[i, 2:ncol(regcases)]/regcases[(i-1), 2:ncol(regcases)])*genTime
}

# Reset first row to 1, because there's no data
# Fix R=1 not NaN or Inf when previous cases are zero
# Its not really defined.  This generates a warning which we can ignore
rat[1, 2:ncol(regcases)] <- 1.0
rat[is.na(rat)] <- 1.0
rat[rat==Inf] <- 1.0
rat[rat==-Inf] <- 1.0

startplot <- rat$date[1]
endplot <- enddate


if(interactive()){
  plot(smooth.spline(rat$Scotland[startplot <= rat$date & rat$date <= endplot],df=14)$y,
       x=rat$date[startplot <= rat$date & rat$date <= endplot],
       ylim=c(0.7,1.40),xlab="Date",ylab="R, Scotland")

  rat %>% filter(startplot < date & date < endplot) %>%
    pivot_longer(!date,names_to = "Region", values_to="R") %>%
    ggplot(aes(x=date, y=R, colour=Region)) + coord_cartesian(ylim=c(0.8,1.5)) +
    geom_smooth(formula= y ~ x, method = "loess", span=0.3) +  guides(color = "none") +
    facet_wrap(vars(Region)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +xlab("Date")

  #  Plot UK nations and English regions
  rat[,c(1,2,3,4,5,6,7,8,9,10,11,12,13)]%>% filter(startplot < date & date < endplot) %>%
    pivot_longer(!date,names_to = "Region", values_to="R") %>%
    ggplot(aes(x=date, y=R, colour=Region)) +
    coord_cartesian(ylim=c(0.5,1.9))+ geom_smooth(formula= y ~ x, method = "loess", span=0.3) +
    guides(color = "none") + facet_wrap(vars(Region)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +xlab("Date")

  #  Plot Scottish regions
  rat[,c(1,11,14,15,16,17,18,19,20,21,22,23,24,25,26,27)]%>% filter(startplot < date & date < endplot) %>%
    pivot_longer(!date,names_to = "Region", values_to="R") %>%
    ggplot(aes(x=date, y=R, colour=Region)) +
    coord_cartesian(ylim=c(0.5,1.9))+ geom_smooth(formula= y ~ x, method = "loess", span=0.3) +
    guides(color = "none") + facet_wrap(vars(Region)) +
    theme(axis.text.x=element_text(angle=90,hjust=1)) +xlab("Date")

}


#  Generate R over all ages, with some options for the calculus  itoR is Ito, stratR is stratonovich, bylogR is harmonic Ito fpR includes false positive correction
#  Avoid zero cases in R-calculation
casedat[casedat == 0] <- 1

for(i in ((genTime+1):length(dfR$itoR))){
  dfR$itoR[i]=(1+(comdat$allCases[i]-comdat$allCases[i-1])*genTime/(comdat$allCases[i-1]))
  dfR$stratR[i]=1+ (comdat$allCases[i]-comdat$allCases[i-1])*genTime/mean(comdat$allCases[(i-1):i])
  dfR$fpR[i]=(1+(comdat$fpCases[i]-comdat$fpCases[i-1])*genTime/(comdat$fpCases[i-1]))
  dfR$bylogR[i]=1+log(comdat$allCases[i]/comdat$allCases[i-1])*genTime
  dfR$ONS[i]=1+log(comdat$ons_prev[i]/comdat$ons_prev[i-1])*genTime
  dfR$regions[i]=1+log(regcases$regions[i]/regcases$regions[i-1])*genTime
  dfR$p00[i]=1+log(casedat$'00_04'[i]/casedat$'00_04'[i-1])*genTime
  dfR$p05[i]=1+log(casedat$'05_09'[i]/casedat$'05_09'[i-1])*genTime
  dfR$p10[i]=1+log(casedat$'10_14'[i]/casedat$'10_14'[i-1])*genTime
  dfR$p15[i]=1+log(casedat$'15_19'[i]/casedat$'15_19'[i-1])*genTime
  dfR$p20[i]=1+log(casedat$'20_24'[i]/casedat$'20_24'[i-1])*genTime
  dfR$p25[i]=1+log(casedat$'25_29'[i]/casedat$'25_29'[i-1])*genTime
  dfR$p30[i]=1+log(casedat$'30_34'[i]/casedat$'30_34'[i-1])*genTime
  dfR$p35[i]=1+log(casedat$'35_39'[i]/casedat$'35_39'[i-1])*genTime
  dfR$p40[i]=1+log(casedat$'40_44'[i]/casedat$'40_44'[i-1])*genTime
  dfR$p45[i]=1+log(casedat$'45_49'[i]/casedat$'45_49'[i-1])*genTime
  dfR$p50[i]=1+log(casedat$'50_54'[i]/casedat$'50_54'[i-1])*genTime
  dfR$p55[i]=1+log(casedat$'55_59'[i]/casedat$'55_59'[i-1])*genTime
  dfR$p60[i]=1+log(casedat$'60_64'[i]/casedat$'60_64'[i-1])*genTime
  dfR$p65[i]=1+log(casedat$'65_69'[i]/casedat$'65_69'[i-1])*genTime
  dfR$p70[i]=1+log(casedat$'70_74'[i]/casedat$'70_74'[i-1])*genTime
  dfR$p75[i]=1+log(casedat$'75_79'[i]/casedat$'75_79'[i-1])*genTime
  dfR$p80[i]=1+log(casedat$'80_84'[i]/casedat$'80_84'[i-1])*genTime
  dfR$p85[i]=1+log(casedat$'85_89'[i]/casedat$'85_89'[i-1])*genTime
  dfR$p90[i]=1+log(casedat$'90+'[i]/casedat$'90+'[i-1])*genTime
#   Same from CrystalCast age groupings
   dfR$x05[i]=1+log(xcastage$`05_14`[i]/xcastage$`05_14`[i-1])*genTime
   dfR$x15[i]=1+log(xcastage$`15_24`[i]/xcastage$`15_24`[i-1])*genTime
   dfR$x25[i]=1+log(xcastage$`25_44`[i]/xcastage$`25_44`[i-1])*genTime
   dfR$x45[i]=1+log(xcastage$`45_64`[i]/xcastage$`45_64`[i-1])*genTime
   dfR$x65[i]=1+log(xcastage$`65_74`[i]/xcastage$`65_74`[i-1])*genTime
   dfR$x75[i]=1+log(xcastage$'75+'[i]/xcastage$'75+'[i-1])*genTime
   dfR$smoothcasesR[i]=1+log(smoothcases$y[i]/smoothcases$y[i-1])*genTime
}

dfR$smoothRlog <- smooth.spline(dfR$bylogR,df=20,w=sqrt(comdat$allCases))$y
dfR$smoothRito <- smooth.spline(dfR$itoR,df=20,w=sqrt(comdat$allCases))$y
dfR$smoothRstrat <- smooth.spline(dfR$stratR,df=20,w=sqrt(comdat$allCases))$y
dfR$loessR <- predict(loess(bylogR~x,data=dfR,span=0.25))
dfR[is.na(dfR)] <- 1.0
dfR[dfR == Inf] <- 1.0
dfR[dfR == -Inf] <- 1.0

# Set day 1, for plotting purposes
for (i in 3:nrow(dfR)){dfR[i,1] <- dfR[i,2]}

for(i in 4:(nrow(dfR)-3)){
    day1 <- i-3
    day7 <- i+3
    dfR$weeklyR[i] <- sum(dfR$itoR[day1:day7])/7.0
}

# End effect
dfR$weeklyR[length(dfR$weeklyR)] <- 1.0
dfR$weeklyR[length(dfR$weeklyR)-1] <- 1.0
dfR$weeklyR[length(dfR$weeklyR)-2] <- 1.0

# Plot various types of smoothing on the R data

# Making the time windows agree
Govdat <- Rest[Rest$Date >= min(comdat$date) & Rest$Date <= max(comdat$date),]

# Parameters for fitting splines and Loess
nospl <- 8
spdf <- length(dfR$rawR)/14
lospan <- 0.3

smoothweightR <- smooth.spline(dfR$bylogR,df=spdf,w=sqrt(comdat$allCases))$y
smoothweightRstrat <- smooth.spline(dfR$stratR,df=spdf,w=sqrt(comdat$allCases))$y
smoothweightRito <- smooth.spline(dfR$itoR,df=spdf,w=sqrt(comdat$allCases))$y
dfR$smoothweightRfp <- smooth.spline(dfR$fpR,df=spdf,w=sqrt(comdat$fpCases))$y
dfR$smoothONS <- smooth.spline(dfR$ONS,df=spdf,w=sqrt(comdat$ons_prev))$y
rat$smoothScotland <- smooth.spline(rat$Scotland,df=spdf,w=sqrt(regcases$Scotland))$y
rat$smoothNW <-smooth.spline(rat$`North West`,df=spdf,w=sqrt(regcases$`North West`))$y
rat$smoothNEY <-smooth.spline(rat$NE_Yorks,df=spdf,w=sqrt(regcases$NE_Yorks))$y
rat$smoothLon <-smooth.spline(rat$London,df=spdf,w=sqrt(regcases$London))$y
rat$smoothEE <-smooth.spline(rat$`East of England`,df=spdf,w=sqrt(regcases$`East of England`))$y
rat$smoothMD <-smooth.spline(rat$Midlands,df=spdf,w=sqrt(regcases$Midlands))$y
rat$smoothSE <-smooth.spline(rat$`South East`,df=spdf,w=sqrt(regcases$`South East`))$y
rat$smoothSW <-smooth.spline(rat$`South West`,df=spdf,w=sqrt(regcases$`South West`))$y
rat$smoothWales <-smooth.spline(rat$Wales,df=spdf,w=sqrt(regcases$Wales))$y
rat$smoothNI<-smooth.spline(rat$NI,df=spdf,w=sqrt(regcases$NI))$y
rat$smoothEngland<-smooth.spline(rat$England,df=spdf,w=sqrt(regcases$England))$y
jnkR=rat$`NHS Lothian`
jnkC=regcases$'NHS Lothian'
rat$smoothLothian <-smooth.spline(jnkR,df=spdf,w=sqrt(jnkC))$y
jnkR=rat$`NHS Greater Glasgow and Clyde`
jnkC=regcases$`NHS Greater Glasgow and Clyde`
rat$smoothGlasgow <-smooth.spline(jnkR,df=spdf,w=sqrt(jnkC))$y
rat$smoothLon <-smooth.spline(rat$London,df=spdf,w=sqrt(regcases$London))$y
#  Piecewise R between lockdowns only if its within the startdate-enddate period
if(lock1>1){
smoothR1<-smooth.spline(dfR$bylogR[1:(lock1-1)],df=lock1/14)
smoothR1$date<-dfR$date[1:lock1-1]
smoothR2<-smooth.spline(dfR$bylogR[lock1:(unlock1-1)],df=(unlock1-lock1)/14)
smoothR2$x=smoothR2$x+lock1
smoothR2$date<-dfR$date[lock1:unlock1-1]
smoothR3<-smooth.spline(dfR$bylogR[unlock1:(lock2-1)],df=(lock2-unlock1)/14)
smoothR3$x=smoothR3$x+unlock1
smoothR3$date<-dfR$date[unlock1:(lock2-1)]
smoothR4<-smooth.spline(dfR$bylogR[lock2:(unlock2-1)],df=(unlock2-lock2)/14)
smoothR4$x=smoothR4$x+unlock2
smoothR4$date<-dfR$date[lock2:(unlock2-1)]
smoothRend<-smooth.spline(dfR$bylogR[unlock2:length(dfR$date)],df=(length(dfR$date)-unlock2)/14)
smoothRend$x=smoothRend$x+unlock2
smoothRend$date<-dfR$date[unlock2:length(dfR$itoR)]
dfR$piecewise<-dfR$itoR
dfR$piecewise[1:(lock1-1)]=smoothR1$y
dfR$piecewise[lock1:(unlock1-1)]=smoothR2$y
dfR$piecewise[unlock1:(lock2-1)]=smoothR3$y
dfR$piecewise[lock2:(unlock2-1)]=smoothR4$y
dfR$piecewise[unlock2:length(dfR$itoR)]=smoothRend$y

rm(smoothR1,smoothR2,smoothR3,smoothR4,smoothRend,jnkR,jnkC)
# Plot R estimate vs data and fits discontinuous at lockdown
#  Have to move the Official R data back by 16 days !

#  All cases and Regions
if(interactive()){

  plot(dfR$smoothweightR,ylab="Regional R-number",xlab="Date",x=dfR$date)

  for (i in 8:17){
    lines(smooth.spline(na.omit(dfR[i]),df=20)$y,col=i,x=dfR$date[!is.na(dfR[i])])
  }

  # ggplot graph - create a temporary tibble
  d <- tibble(x = dfR$date,
              y = smooth.spline(na.omit(dfR[8]),df=12)$y,
              type = rep(names(dfR)[8],nrow(dfR)))

  # Populate the tibble
  for(i in names(dfR)[9:17]){
    d <- add_row(d,
                 x = dfR$date,
                 y = smooth.spline(na.omit(dfR[i]),df=12)$y,
                 type = rep(i, nrow(dfR)))
  }

  # Generate the graph
  data.frame(x=dfR$date, y=dfR$smoothweightR) %>%
    ggplot(aes(x, y)) + geom_point(alpha = 0.5) +
    theme_bw()  + xlab("Date") + ylab("Regional R-number") +
    geom_line(data=d,aes(x = x, y = y, colour = type, linetype = type) )

  # remove temporary tibble
  rm(d)

  plot(dfR$bylogR,x=dfR$date,ylab="R-number",xlab="",
       title("R, England"),ylim=c(0.6,1.6),xlim=plotdate,cex.lab=1.6, cex.axis=1.6, cex.main=1.6, cex.sub=1.6)
  lines(Rest$England_LowerBound,x=Rest$Date-sagedelay,lwd=2)
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay,lwd=2)
  if(exists("dfR$piecewise")){lines(dfR$piecewise,col="violet",lwd=3,x=dfR$date)}
  lines(dfR$smoothweightR,col="blue",lwd=3,x=dfR$date)
  lines(predict(loess(itoR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='green',x=dfR$date,lwd=3)
  #lines(predict(loess(itoR ~ x, data=dfR,span=0.3)),col='green',x=dfR$date)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='red',x=dfR$date,lwd=3)
  #lines(predict(loess(bylogR ~ x, data=dfR,span=0.3)),col='red',x=dfR$date)

  # ggplot version of the same plot
  dfR %>% ggplot(aes(x = date, y = bylogR)) + geom_point(alpha=0.5, na.rm = TRUE) +
    theme_bw() + ylab("R-number") + xlab("Date") + ylim(0.6, 1.6) +
    geom_ribbon(data = Rest,
                aes(x = Date - sagedelay,
                    ymin = England_LowerBound,
                    ymax = England_UpperBound,
                    fill = "red",
                    colour = "black"), alpha = 0.1, inherit.aes = FALSE, show.legend = FALSE) +
    geom_line(data = dfR, aes(x = date, y = piecewise), colour = "violet", size = 1.25) +
    geom_line(data = data.frame(x = dfR$date, y = dfR$smoothweightR), aes(x = x, y = y), colour = "blue", size = 1.25) +
    geom_line(data = data.frame(x = dfR$date,
                                y = predict(loess(itoR ~ x, data = dfR, span = 0.3, weight = sqrt(comdat$allCases)))),
              aes(x = x, y = y), colour = "green", size = 1.25) +
    geom_line(data = data.frame(x = dfR$date,
                                y = predict(loess(bylogR ~ x, data = dfR, span = 0.3, weight = sqrt(comdat$allCases)))),
              aes(x = x, y = y), colour = "red", size = 1.25) + ggtitle("R, England") +
    theme(plot.title = element_text(hjust = 0.5))


  plot(dfR$piecewise,x=dfR$smoothweightR,ylab="R-number",xlab="",
       title("R, England"),ylim=c(0.6,1.4),xlim=plotdate,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.6)
  lines(Rest$England_LowerBound,x=(Rest$Date-sagedelay))
  lines(y=Rest$England_UpperBound,x=Rest$Date-sagedelay)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.05,weight=sqrt(comdat$allCases))),col='red',x=dfR$date,lwd=2)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.1,weight=sqrt(comdat$allCases))),col='green',x=dfR$date,lwd=2)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.2,weight=sqrt(comdat$allCases))),col='blue',x=dfR$date,lwd=2)
  lines(predict(loess(bylogR ~ x, data=dfR,span=0.3,weight=sqrt(comdat$allCases))),col='violet',x=dfR$date,lwd=3)

  # ggplot version of the same plot
  dfR %>% ggplot(aes(x = date, y = piecewise)) + geom_point(alpha=0.5, na.rm = TRUE) +
          theme_bw() + ylab("R-number") + xlab("Date") + ylim(0.6, 1.4) + ggtitle("R, England") +
          theme(plot.title = element_text(hjust = 0.5)) +
          geom_ribbon(data = Rest,
                     aes(x = Date - sagedelay,
                        ymin = England_LowerBound,
                        ymax = England_UpperBound,
                        fill = "red",
                        colour = "black"), alpha = 0.05, inherit.aes = FALSE, show.legend = FALSE) +
          geom_line(data = data.frame(x = dfR$date,
                                      y = predict(loess(bylogR ~ x, data = dfR, span = 0.05, weight = sqrt(comdat$allCases)))),
                    aes(x = x, y = y), inherit.aes = FALSE, colour = "red", size = 1) +
          geom_line(data = data.frame(x = dfR$date,
                                      y = predict(loess(bylogR ~ x, data = dfR, span = 0.1, weight = sqrt(comdat$allCases)))),
                    aes(x = x, y = y), colour = "green", size = 1) +
          geom_line(data = data.frame(x = dfR$date,
                                     y = predict(loess(bylogR ~ x, data = dfR, span = 0.2, weight = sqrt(comdat$allCases)))),
                    aes(x = x, y = y), colour = "blue", size = 1) +
          geom_line(data = data.frame(x = dfR$date,
                                y = predict(loess(bylogR ~ x, data = dfR, span = 0.3, weight = sqrt(comdat$allCases)))),
              aes(x = x, y = y), colour = "violet", size = 1.25)

}# End piecewise R for 2020 lockdowns calculate and plot section
}# End interactive session


R_BestGuess <- list()
R_Quant <- list()


#  Daily stats are quite messed up since end of PCR testing.  Use smoothed R estimates for Quartiles
tmp <-estimate_R(rat$smoothEngland,dfR$date,comdat$allCases)
R_BestGuess$England <-tmp[1]
R_Quant$England <-tmp[2:6]


tmp <-estimate_R(rat$smoothScotland,rat$date,regcases$Scotland)
R_BestGuess$Scotland <-tmp[1]
R_Quant$Scotland <-tmp[2:6]

#tmp <-estimate_R(rat$smoothLon,rat$date,regcases$London)
R_BestGuess$Lon <- R_no #tmp[1]
R_Quant$Lon <- c(R_no*0.9,R_no*0.97,R_no*1.001,R_no*1.03,R_no*1.1) # assume +- 10%          #tmp[2:6]

tmp <-estimate_R(rat$smoothMD,rat$date,regcases$Midlands)
R_BestGuess$Midlands <-tmp[1]
R_Quant$Midlands <-tmp[2:6]

tmp <-estimate_R(rat$smoothNW,rat$date,regcases$'North West')
R_BestGuess$NW <-tmp[1]
R_Quant$NW <-tmp[2:6]

tmp <-estimate_R(rat$smoothNEY,rat$date,regcases$NE_Yorks)
R_BestGuess$NEY <-tmp[1]
R_Quant$NEY <-tmp[2:6]

tmp <-estimate_R(rat$smoothEE,rat$date,regcases$`East of England`)
R_BestGuess$EE <-tmp[1]
R_Quant$EE <-tmp[2:6]

tmp <-estimate_R(rat$smoothSE,rat$date,regcases$`South East`)
R_BestGuess$SE <-tmp[1]
R_Quant$SE <-tmp[2:6]
tmp <-estimate_R(rat$smoothSW,rat$date,regcases$`South West`)
R_BestGuess$SW <-tmp[1]
R_Quant$SW <-tmp[2:6]

tmp <-estimate_R(rat$smoothWales,rat$date,regcases$Wales)
R_BestGuess$Wales <-tmp[1]
R_Quant$Wales <-tmp[2:6]

tmp <-estimate_R(dfR$regions,dfR$date,regcases$England)
R_BestGuess$Regions <-tmp[1]
R_Quant$Regions <-tmp[2:6]

tmp <-estimate_R(rat$smoothNI,rat$date,regcases$NI)
R_BestGuess$NI <-tmp[1]
R_Quant$NI <-tmp[2:6]

tmp <-estimate_R(rat$smoothLothian,rat$date,regcases$NI)
R_BestGuess$Lothian <-tmp[1]
R_Quant$Lothian <-tmp[2:6]

tmp <-estimate_R(rat$smoothGlasgow,rat$date,regcases$NI)
R_BestGuess$Glasgow <-tmp[1]
R_Quant$Glasgow <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Ayrshire and Arran`,rat$date,regcases$NI)
R_BestGuess$Ayr <-tmp[1]
R_Quant$Ayr <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Borders`,rat$date,regcases$NI)
R_BestGuess$Borders<-tmp[1]
R_Quant$Borders <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Dumfries and Galloway`,rat$date,regcases$NI)
R_BestGuess$Dumfries <-tmp[1]
R_Quant$Dumfries <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Forth Valley`,rat$date,regcases$NI)
R_BestGuess$Forth <-tmp[1]
R_Quant$Forth <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Grampian`,rat$date,regcases$NI)
R_BestGuess$Gramp <-tmp[1]
R_Quant$Gramp <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Highland`,rat$date,regcases$NI)
R_BestGuess$Highland <-tmp[1]
R_Quant$Highland <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Fife`,rat$date,regcases$NI)
R_BestGuess$Fife <-tmp[1]
R_Quant$Fife <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Tayside`,rat$date,regcases$NI)
R_BestGuess$Tayside <-tmp[1]
R_Quant$Tayside <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Lanarkshire`,rat$date,regcases$NI)
R_BestGuess$Lanark<-tmp[1]
R_Quant$Lanark <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Orkney`,rat$date,regcases$NI)
R_BestGuess$Orkney <-tmp[1]
R_Quant$Orkney <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Western Isles`,rat$date,regcases$NI)
R_BestGuess$WI <-tmp[1]
R_Quant$WI <-tmp[2:6]

tmp <-estimate_R(rat$`NHS Shetland`,rat$date,regcases$NI)
R_BestGuess$Shetland <-tmp[1]
R_Quant$Shetland <-tmp[2:6]


##########   Age groups  ########

tmp <-estimate_R(dfR$p00,dfR$date,comdat$allCases)
R_BestGuess$x00 <-tmp[1]
R_Quant$x00 <-tmp[2:6]

tmp <-estimate_R(dfR$p05,dfR$date,comdat$allCases)
R_BestGuess$x05 <-tmp[1]
R_Quant$x05 <-tmp[2:6]

tmp <-estimate_R(dfR$p15,dfR$date,comdat$allCases)
R_BestGuess$x15 <-tmp[1]
R_Quant$x15 <-tmp[2:6]

tmp <-estimate_R(dfR$p25,dfR$date,comdat$allCases)
R_BestGuess$x25 <-tmp[1]
R_Quant$x25 <-tmp[2:6]

tmp <-estimate_R(dfR$p45,dfR$date,comdat$allCases)
R_BestGuess$x45 <-tmp[1]
R_Quant$x45 <-tmp[2:6]

tmp <-estimate_R(dfR$p65,dfR$date,comdat$allCases)
R_BestGuess$x65 <-tmp[1]
R_Quant$x65 <-tmp[2:6]

tmp <-estimate_R(dfR$p75,dfR$date,comdat$allCases)
R_BestGuess$x75 <-tmp[1]
R_Quant$x75 <-tmp[2:6]


if(interactive()){

  plot(dfR$smoothweightR,ylab="R-number",xlab="Day",ylim=c(0.5,1.6))
  #  Plot R continuous with many splines.
  for (ismooth in 4:30){
    #  lines(smooth.spline(dfR$bylogR,df=ismooth,w=sqrt(comdat$allCases)))
    lines(predict(loess(bylogR ~ x, data=dfR,span=(4.0/ismooth))),col='red')
  }

  # Temp tibble
  ismooth <-  4
  d <- tibble(x = seq_len(nrow(dfR)),
             y = predict(loess(bylogR ~ x, data=dfR,span=(4.0/ismooth))),
             type = rep(ismooth, nrow(dfR))
            )
  for (ismooth in 5:30){
    d <- add_row(d,x = seq_len(nrow(dfR)),
                 y = predict(loess(bylogR ~ x, data=dfR,span=(4.0/ismooth))),
                 type = rep(ismooth, nrow(dfR)))
  }

  # plot the data
  data.frame(x = dfR$date,y = dfR$smoothweightR) %>%
  ggplot(aes(x = x, y = y)) + geom_point(alpha = 0.25) +
  theme_bw() + xlab("Day") + ylab("R-number") +
  geom_line(data = d, aes(x = x, y = y, colour = factor(type)), alpha = 0.25, show.legend = FALSE)

  # Remove the temporary tibble
  rm(d)
}



# Plot Regional R data vs Government  spdf is spline smoothing factor, lospan for loess

#  various options to silence pdf writing
pdfpo <- FALSE

#  Plot age data
dfR[,c(2,9:27)]%>% filter(startplot < date & date < endplot) %>%
  pivot_longer(!date,names_to = "Age", values_to="R") %>%
  ggplot(aes(x=date, y=R, colour=Age)) +
  coord_cartesian(ylim=c(0.5,1.9))+ geom_smooth(formula = y ~ x, method = "loess", span=0.3) +
  guides(color = "none") + facet_wrap(vars(Age)) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + xlab("Date")

#  Function to generate plots of R by age

if(interactive()&pdfpo){age_pdfplot(dfR,Rest,sagedelay,comdat,spdf)}



# Reverse Engineer cases from R-number - requires stratonovich calculus to get reversibility
# Initializations

#  Use the same weekend-adjusted initial condition, regardless of smoothing effect
#  Start with the "correct" data

Predict <- data.frame(x=1.0:length(comdat$date),
                      date=comdat$date,
                      c=comdat$allCases,
Lin=comdat$allCases,
Raw=comdat$allCases,
SmoothRlog=comdat$allCases,
SmoothRito=comdat$allCases,
SmoothRstrat=comdat$allCases,
MeanR=comdat$allCases,
smoothcasesR=comdat$allCases
)
meanR=mean(dfR$stratR)
startpred=genTime+22
for(i in startpred:length(dfR$date)){
  Predict$c[i]=Predict$c[i-1]*exp((dfR$bylogR[i]-1)/genTime)
  Predict$Lin[i]=Predict$Lin[i-1]*(1.0+(dfR$itoR[i]-1)/genTime)
  Predict$SmoothRstrat[i]=Predict$SmoothRstrat[i-1]*(1.0+(dfR$stratR[i]-1)/genTime)
  Predict$MeanR[i]=Predict$MeanR[i-1]*(1.0+(meanR-1)/genTime)
  Predict$SmoothRlog[i]=Predict$SmoothRlog[i-1]*exp((dfR$smoothRlog[i]-1)/genTime)
  Predict$SmoothRito[i]=Predict$SmoothRito[i-1]*exp((dfR$smoothRito[i]-1)/genTime)
  Predict$smoothcasesR[i]=Predict$smoothcasesR[i-1]*exp((dfR$smoothcasesR[i]-1)/genTime)
    }
#  Averaging R is not the same as averaging e^R
#  Noise suppresses the growth rate in the model, Smoothed R grows too fast
#   Multiplier chosen to match final cases, because it is super-sensitive to noise in the initial day
#

Predict$smoothcasesR =Predict$smoothcasesR*sum(comdat$allCases)/sum(Predict$smoothcasesR)
Predict$SmoothRito =Predict$SmoothRito*sum(comdat$allCases)/sum(Predict$SmoothRito)
Predict$SmoothRlog=Predict$SmoothRlog*sum(comdat$allCases)/sum(Predict$SmoothRlog)
Predict$MeanR=Predict$MeanR*sum(comdat$allCases)/sum(Predict$MeanR)

if(interactive()&FALSE){

  sum(Predict$MeanR)
  sum(Predict$SmoothRlog)
  sum(Predict$SmoothRito)
  sum(Predict$smoothcasesR)

  plot(comdat$allCases,x=Predict$date,xlab="Date",ylab="Cases backdeduced from R")
  lines(Predict$c,x=Predict$date, col="black",lwd=2)
  lines(Predict$SmoothRlog,x=Predict$date, col="blue",lwd=2)
  lines(Predict$SmoothRito,x=Predict$date, col="violet",lwd=2)
  lines(Predict$MeanR,x=Predict$date, col="green",lwd=2)
  lines(Predict$smoothcasesR,x=Predict$date, col="red",lwd=2)

  dfR$meanR=meanR
  plot(dfR$bylogR,x=dfR$date,xlab="",ylab="R"
       ,xlim=c(dfR$date[(startpred)],dfR$date[350]),ylim=c(-1,3))
  lines(dfR$smoothRlog,x=dfR$date, col="blue",lwd=2)
  lines(dfR$smoothRito,x=dfR$date, col="violet",lwd=2)
  lines(dfR$meanR,x=dfR$date, col="green",lwd=2)
  lines(dfR$smoothcasesR,x=dfR$date, col="red",lwd=2)

  # Create a temporary tibble to plot the data
  tmpdat <- tibble(date = comdat$date,
                   c = Predict$c,
                   smoothcasesR = Predict$smoothcasesR,
                   SmoothRlog = Predict$SmoothRlog,
                   SmoothRito = Predict$SmoothRito,
                   MeanR = Predict$MeanR)

  # Plot the data
  ggplot(comdat, aes(x = date)) + geom_point(aes(y = allCases), alpha = 0.2) +
    geom_line(data = tmpdat, aes(x = date, y = c), colour = "black", alpha = 0.5, size = 1.1) +
    geom_line(data = tmpdat, aes(x = date, y = SmoothRlog), colour = "blue", alpha = 0.5, size = 1.1) +
    geom_line(data = tmpdat, aes(x = date, y = SmoothRito), colour = "violet", alpha = 0.5, size = 1.1) +
    geom_line(data = tmpdat, aes(x = date, y = MeanR), colour = "green", alpha = 0.5, size = 1.1) +
    geom_line(data = tmpdat, aes(x = date, y = smoothcasesR), colour = "red", alpha = 0.5, size = 1.1) +
    xlab("Date") + ylab("Cases backdeduced from R") + theme_bw()

  # Create a temporary tibble for plotting
  tmpdat <-  tibble(date = comdat$date,
                    c = Predict$c,
                    smoothcasesR = dfR$smoothcasesR,
                    SmoothRlog = dfR$smoothRlog,
                    SmoothRito = dfR$smoothRito,
                    bylogR = dfR$bylogR,
                    MeanR = meanR )
  # Plot the data
  ggplot(comdat, aes(x = date)) +
    geom_point(data = tmpdat, aes(x = date, y = bylogR), colour = "black",alpha = 0.25, na.rm = TRUE) +
    geom_line(data = tmpdat, aes(x = date, y = SmoothRlog), colour = "blue",alpha = 0.75, size = 1.25) +
    geom_line(data = tmpdat, aes(x = date, y = SmoothRito), colour = "violet",alpha = 0.75, size = 1.25) +
    geom_line(data = tmpdat, aes(x = date, y = MeanR), colour = "green",alpha = 0.75, size = 1.25) +
    geom_line(data = tmpdat, aes(x = date, y = smoothcasesR), colour = "red", alpha = 0.75, size = 1.25) +
    xlab("Date") + ylab("R") + theme_bw() + ylim(0.7, 1.4)

  # Remove the temporary array
  rm(tmpdat)

}

#####  Figures and analysis for https://www.medrxiv.org/content/10.1101/2021.04.14.21255385v1
# Date not encapsulated and may become broken because of hard coded dates
# Nothing should be returned or changed by this analysis

medrxiv<-FALSE
if(interactive()&medrxiv){medout<-MedrxivPaper()}

################################################################
###  Finally, Use all this to make predictions for England (Scotland & Regions in separate compartment.R code)
###Assume that R and lethality are constants

region="England"
predEng<-Predictions(compEng,R_BestGuess$England,predtime,population$England)


#  Compartment predictions removed to Predictions.R
#  Replicated the data because repeated calls to Predictions would increment comp

# Monitoring plots

#  crystal cast writing moved to Regional.R
#CC_write(predEng,"England",population$England[1],R_BestGuess$England,R_Quant$England,rat$smoothEngland)
#  Wales and NI awaiting age data wrangle

if(interactive()){
startplot=startdate+3
endplot=startdate+nrow(predEng$CASE)+predtime-3
PREV<-compEng$ILI[2:20]+compEng$SARI[2:20]+compEng$CRIT[2:20]+compEng$MILD[2:20]

plot(rowSums(predEng$CASE[2:20]))
lines(rowSums(PREV[1:19])/20)

plot(Hospital$UK$newsaridat,x=Hospital$UK$date, ylab="Hospital Admission",xlab="Date",xlim=c(startplot,endplot-180                                                                                                ))
lines(rowSums(compEng$newSARI[2:20]),x=compEng$newSARI$date,col="blue")
lines(rowSums(predEng$newSARI[2:20]),x=compEng$newSARI$date,col="red")
plot(Hospital$UK$saridat,x=Hospital$UK$date,ylab="Hospital Cases",xlab="Date",xlim=c((startplot),(endplot-200)))
lines(0.7*rowSums(predEng$SARI[2:20]+predEng$CRIT[2:20]+predEng$CRITREC[2:20]),x=predEng$SARI$date,col='red')

plot(rowSums(compEng$newMILD[2:20]+compEng$newILI[2:20]),xlim=c((startplot),(endplot-100)),col="blue",x=compEng$newMILD$date,type="l",xlab="Date",ylab="Cases")
plot(rowSums(predEng$CASE[2:20]),x=predEng$CASE$date)
lines(rowSums(compEng$newMILD[2:10]+compEng$newILI[2:10]),col="green",x=compEng$newMILD$date,type="l",xlab="Date",ylab="Cases")
lines(rowSums(compEng$newMILD[11:20]+compEng$newILI[11:20]),col="red",x=compEng$newMILD$date,type="l",xlab="Date",ylab="Cases")

plot(Hospital$UK$critdat,x=Hospital$UK$date,ylab="ICU Occupation",xlab="Date",xlim=c(startplot,endplot))
lines(rowSums(predEng$CRIT[2:20]),col="blue",x=predEng$CRIT$date)

plot(rowSums(predEng$DEATH[2:20]),col="blue",x=predEng$DEATH$date, type="l",ylab="Deaths"
     ,xlab="Date",xlim=c(startplot,endplot-60))
points(rowSums(deathdat[2:20]),x=deathdat$date)
}

