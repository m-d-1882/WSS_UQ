source("CC_write.R")
source("CC.R")
source("CompartmentFunction_ensemble.R")
source("Predictions.R")


getData <- function(dftmp) {
  out <- dftmp %>%
    select(date = date, age = age, values = cases) %>%
    pivot_wider(id_cols = date, names_from = age, values_from = values) %>%
    select(-unassigned, -"60+", -"00_59") %>%
    filter(date >= startdate & date <= enddate) %>%
    arrange(date)
  out$date<-as.Date(out$date)
  return(out)
}


gethData <- function(dftmp) {
  out <- dftmp %>%
    select(date = date, saridat = hospitalCases, newsaridat = newAdmissions) %>%
    filter(date >= startdate & date <= enddate) %>%
    arrange(date)
  out$date<-as.Date(out$date)
  return(na.locf(out))
}

#  Do the regions, after a full run of covid_trimmed
#NEurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
#NWurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000002&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
#YHurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000003&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
#EMurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000004&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
#WMurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000005&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
#EEurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000006&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
Lonurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000007&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
#SEurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000008&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
#SWurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000009&metric=newCasesBySpecimenDateAgeDemographics&format=csv"



coltypes <- cols(col_character(), col_character(),col_character(),
                 col_date(format="%Y-%m-%d"), col_character(),
                 col_integer(), col_integer(), col_number())
# Remap the ages column to be the header rows, remove the unassigned,
# 60+ and 00_59 columns, filter dates to be between the start and end
# dates and order the output by date


#NE <-  read.csv(file=NEurl)
#NW <-  read.csv(file=NWurl)
#YH <-  read.csv(file=YHurl)
#EM <-  read.csv(file=EMurl)
#WM <-  read.csv(file=WMurl)
#EE <-  read.csv(file=EEurl)
Lon <-  read.csv(file=Lonurl)
#SE <-  read.csv(file=SEurl)
#SW <-  read.csv(file=SWurl)


#NE<-getData(NE)
#NW<-getData(NW)
#YH<-getData(YH)
#EM<-getData(EM)
#WM<-getData(WM)
#EE<-getData(EE)
Lon<-getData(Lon)
#SE<-getData(SE)
#SW<-getData(SW)

#MD=EM
#NEY=YH
#MD[2:20]=EM[2:20]+WM[2:20]
#NEY[2:20]=NE[2:20]+YH[2:20]

#  Hospital data only available by 7 NHS regions.  obvs.

#SWhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000006&metric=hospitalCases&metric=newAdmissions&format=csv"
#EEhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000007&metric=hospitalCases&metric=newAdmissions&format=csv"
Lonhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000003&metric=hospitalCases&metric=newAdmissions&format=csv"
#MDhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000008&metric=hospitalCases&metric=newAdmissions&format=csv"
#SEhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000005&metric=hospitalCases&metric=newAdmissions&format=csv"
#NEYhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000009&metric=hospitalCases&metric=newAdmissions&format=csv"
#NWhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&areaCode=E40000010&metric=hospitalCases&metric=newAdmissions&format=csv"
#Scothospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=S92000003&metric=hospitalCases&metric=newAdmissions&format=csv"
#Waleshospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=hospitalCases&metric=newAdmissions&format=csv"
#NIhospurl="https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=N92000002&metric=hospitalCases&metric=newAdmissions&format=csv"
#hNEY <-  read.csv(file=NEYhospurl)
#hNW <-  read.csv(file=NWhospurl)
#hMD <-  read.csv(file=MDhospurl)
#hEE <-  read.csv(file=EEhospurl)
hLon <-  read.csv(file=Lonhospurl)
#hSE <-  read.csv(file=SEhospurl)
#hSW <-  read.csv(file=SWhospurl)
#hScot <- read.csv(file=Scothospurl)
#hWal <- read.csv(file=Waleshospurl)
#hNI <- read.csv(file=NIhospurl)
#Hospital$NEY<-gethData(hNEY)
#Hospital$NW<-gethData(hNW)
#Hospital$MD<-gethData(hMD)
#Hospital$EE<-gethData(hEE)
Hospital$Lon<-gethData(hLon)
#Hospital$SE<-gethData(hSE)
#Hospital$SW<-gethData(hSW)
#Hospital$Scot<-gethData(hScot)
#Hospital$Wal<-gethData(hWal)
#Hospital$NI<-gethData(hNI)

for (iage in 2:length(Lon)){
  Lon[iage]<-Weekend(Lon %>% pull(iage)) 
}
#for (iage in 2:length(NE)){
#  NE[iage]<-Weekend(NE %>% pull(iage)) 
#}
#for (iage in 2:length(NW)){
#  NW[iage]<-Weekend(NW %>% pull(iage)) 
#}
#for (iage in 2:length(YH)){
#  YH[iage]<-Weekend(YH %>% pull(iage)) 
#}
#for (iage in 2:length(WM)){
#  WM[iage]<-Weekend(WM %>% pull(iage)) 
#}
#for (iage in 2:length(EM)){
#  EM[iage]<-Weekend(EM %>% pull(iage)) 
#}
#for (iage in 2:length(EE)){
#  EE[iage]<-Weekend(EE %>% pull(iage)) 
#}
#for (iage in 2:length(SE)){
#  SE[iage]<-Weekend(SE %>% pull(iage)) 
#}
#for (iage in 2:length(SW)){
#  SW[iage]<-Weekend(SW %>% pull(iage)) 
#}
#for (iage in 2:length(MD)){
#  MD[iage]<-Weekend(MD %>% pull(iage)) 
#}
#for (iage in 2:length(NEY)){
#  NEY[iage]<-Weekend(NEY %>% pull(iage)) 
#}

Lon$date<-as.Date(Lon$date)
#NE$date<-as.Date(NE$date)
#NW$date<-as.Date(NW$date)
#YH$date<-as.Date(YH$date)
#EM$date<-as.Date(EM$date)
#WM$date<-as.Date(WM$date)
#EE$date<-as.Date(EE$date)
#SE$date<-as.Date(SE$date)
#SW$date<-as.Date(SW$date)
#MD$date<-as.Date(MD$date)
#NEY$date<-as.Date(NEY$date)

#  Still use R from 7 regions...  CFR and vaccinations are assumed from National stats

compLon<- Compartment(Lon,  covidsimAge, RawCFR, comdat,3,nrow(Lon))
predLon<-Predictions(compLon,R_BestGuess$Lon,predtime,population$Lon)

#compNW<- Compartment(NW,  covidsimAge, RawCFR, comdat,3,nrow(NW))
#predNW<-Predictions(compNW,R_BestGuess$NW,predtime,population$NW)
#
#compEE<- Compartment(EE,  covidsimAge, RawCFR, comdat,3,nrow(EE))
#predEE<-Predictions(compEE,R_BestGuess$EE,predtime,population$EE)
#
#compSE<- Compartment(SE,  covidsimAge, RawCFR, comdat,3,nrow(SE))
#predSE<-Predictions(compSE,R_BestGuess$SE,predtime,population$SE)
#
#compSW<- Compartment(SW,  covidsimAge, RawCFR, comdat,3,nrow(SW))
#predSW<-Predictions(compSW,R_BestGuess$SW,predtime,population$SW)
#
#compMD<- Compartment(MD,  covidsimAge, RawCFR, comdat,3,nrow(MD))
#predMD<-Predictions(compMD,R_BestGuess$Midlands,predtime,population$MD)
#compNEY<- Compartment(NEY,  covidsimAge, RawCFR, comdat,3,nrow(NEY))
#predNEY<-Predictions(compNEY,R_BestGuess$NEY,predtime,population$NEY)

#rm( SE, SW, EE, NEY, MD, Lon, NW, hSE, hSW, hEE, hNEY, hMD, hLon, hNW)

#  eliminate negative values stored beyond the complete data range 
#predEng$SARI[2:20][predEng$SARI[2:20] < 0] <- 0.0
#predScot$SARI[2:20][predScot$SARI[2:20] < 0] <- 0.0
#predEE$SARI[2:20][predEE$SARI[2:20] < 0] <- 0.0
predLon$SARI[2:20][predLon$SARI[2:20] < 0] <- 0.0
#predNW$SARI[2:20][predNW$SARI[2:20] < 0] <- 0.0
#predNEY$SARI[2:20][predNEY$SARI[2:20] < 0] <- 0.0
#predMD$SARI[2:20][predMD$SARI[2:20] < 0] <- 0.0
#predSW$SARI[2:20][predSW$SARI[2:20] < 0] <- 0.0
#predSE$SARI[2:20][predSE$SARI[2:20] < 0] <- 0.0
# recent scaling factors for MTPs, 
# With omicron and confirmatory PCR changes, shorten recent_timescale
total_time = min(nrow(deathdat),nrow(casedat),length(Hospital$Lon$date))

recent_time<-(total_time-7-reporting_delay):(total_time-reporting_delay-1)

#Ratios:  For MTPs we scale the various quantities to fit the most recent time data
#   This automatically compensates for any slowish-varying trends of increased virulence, better treatment waning immunity etc.
Hospital$Eng$newsaridat=#Hospital$NEY$newsaridat+Hospital$NW$newsaridat+
#  Hospital$MD$newsaridat+Hospital$EE$newsaridat+
#  Hospital$SE$newsaridat+Hospital$SW$newsaridat+
  Hospital$Lon$newsaridat
Hospital$Eng$saridat=#Hospital$NEY$saridat+Hospital$NW$saridat+
#  Hospital$MD$saridat+Hospital$EE$saridat+
#  Hospital$SE$saridat+Hospital$SW$saridat+
  Hospital$Lon$saridat
total_deaths=sum(deathdat[recent_time,2:20])
total_cases=sum(casedat[recent_time,2:20])

total_admissions=sum(Hospital$Eng$newsaridat[recent_time])
total_crit=sum(Hospital$UK$critdat[recent_time])
ratio <-list()
#  Nine english regions, dont double-count 11 NEY & 12 MD
#ratio$Eng$death=sum(predEng$DEATH[recent_time,2:20])/sum(regdeaths[recent_time,2:10])
#ratio$Eng$case=sum(predEng$CASE[recent_time,2:20])/total_cases
#ratio$Eng$newhosp=sum(rowSums(predEng$newSARI[recent_time,2:20]))/total_admissions
#ratio$Eng$hosp=sum(rowSums(predEng$SARI[recent_time,2:20]+predEng$CRIT[recent_time,2:20]+predEng$CRITREC[recent_time,2:20]))/sum(Hospital$Eng$saridat[recent_time])
#ratio$Eng$crit=sum(compEng$CRIT[recent_time,2:20])/total_crit


#CCEng=CC_write(predEng,"England",population$England[1],R_BestGuess$England,R_Quant$England,rat$smoothEngland,ratio$Eng,filename)

#ratio$Scot$death=sum(predScot$DEATH[recent_time,2:20])/sum(scotdeath[recent_time,2:20])
#ratio$Scot$hosp=sum(rowSums(predScot$SARI[recent_time,2:20]+predScot$CRIT[recent_time,2:20]+predScot$CRITREC[recent_time,2:20]))/sum(Hospital$Scot$saridat[recent_time])
#ratio$Scot$newhosp=sum(rowSums(predScot$newSARI[recent_time,2:20]))/sum(Hospital$Scot$newsaridat[recent_time])
#CCScot=CC_write(predScot,"Scotland",population$Scotland[1],R_BestGuess$Scotland,R_Quant$NW,rat$smoothScotland,ratio$Scot,filename)

#ratio$NW$death=sum(predNW$DEATH[recent_time,2:20])/sum(regdeaths$`North West`[recent_time])
#ratio$NW$hosp=sum(rowSums(predNW$SARI[recent_time,2:20]+predNW$CRIT[recent_time,2:20]+predNW$CRITREC[recent_time,2:20]))/sum(Hospital$NW$saridat[recent_time])
#ratio$NW$newhosp=sum(rowSums(predNW$newSARI[recent_time,2:20]))/sum(Hospital$NW$newsaridat[recent_time])
#CCNW=CC_write(predNW,"North West",population$NW[1],R_BestGuess$NW,R_Quant$NW,rat$smoothNW,ratio$NW,filename)

#ratio$NEY$death=sum(predNEY$DEATH[recent_time,2:20])/sum(regdeaths$NEY[recent_time])
#ratio$NEY$hosp=sum(rowSums(predNEY$SARI[recent_time,2:20]+predNEY$CRIT[recent_time,2:20]+predNEY$CRITREC[recent_time,2:20]))/sum(Hospital$NEY$saridat[recent_time])
#ratio$NEY$newhosp=sum(rowSums(predNEY$newSARI[recent_time,2:20]))/sum(Hospital$NEY$newsaridat[recent_time])
#CCNEY=CC_write(predNEY,"North East and Yorkshire",population$NEY[1],R_BestGuess$NEY,R_Quant$NEY,rat$smoothNEY,ratio$NEY,filename)

#ratio$MD$death=sum(predMD$DEATH[recent_time,2:20])/sum(regdeaths$MD[recent_time])
#ratio$MD$hosp=sum(rowSums(predMD$SARI[recent_time,2:20]+predMD$CRIT[recent_time,2:20]+predMD$CRITREC[recent_time,2:20]))/sum(Hospital$MD$saridat[recent_time])
#ratio$MD$newhosp=sum(rowSums(predMD$newSARI[recent_time,2:20]))/sum(Hospital$MD$newsaridat[recent_time])
#CCMD=CC_write(predMD,"Midlands",population$MD[1],R_BestGuess$Midlands,R_Quant$Midlands,rat$smoothMD,ratio$MD,filename)

ratio$Lon$death=sum(predLon$DEATH[recent_time,2:20])/sum(regdeaths$London[recent_time])
ratio$Lon$hosp=sum(rowSums(predLon$SARI[recent_time,2:20]+predLon$CRIT[recent_time,2:20]+predLon$CRITREC[recent_time,2:20]))/sum(Hospital$Lon$saridat[recent_time])
ratio$Lon$newhosp=sum(rowSums(predLon$newSARI[recent_time,2:20]))/sum(Hospital$Lon$newsaridat[recent_time])
CCLon=CC_write(predLon,"London",population$Lon[1],R_BestGuess$Lon,R_Quant$Lon,rat$smoothLon,ratio$Lon,filename)

#ratio$SW$death=sum(predSW$DEATH[recent_time,2:20])/sum(regdeaths$`South West`[recent_time])
#ratio$SW$hosp=sum(rowSums(predSW$SARI[recent_time,2:20]+predSW$CRIT[recent_time,2:20]+predSW$CRITREC[recent_time,2:20]))/sum(Hospital$SW$saridat[recent_time])
#ratio$SW$newhosp=sum(rowSums(predSW$newSARI[recent_time,2:20]))/sum(Hospital$SW$newsaridat[recent_time])
#CCSW=CC_write(predSW,"South West",population$SW[1],R_BestGuess$SW,R_Quant$SW,rat$smoothSW,ratio$SW,filename)

#ratio$SE$hosp=sum(rowSums(predSE$SARI[recent_time,2:20]+predSE$CRIT[recent_time,2:20]+predSE$CRITREC[recent_time,2:20]))/sum(Hospital$SE$saridat[recent_time])
#ratio$SE$death=sum(predSE$DEATH[recent_time,2:20])/sum(regdeaths$`South East`[recent_time])
#ratio$SE$newhosp=sum(rowSums(predSE$newSARI[recent_time,2:20]))/sum(Hospital$SE$newsaridat[recent_time])
#CCSE=CC_write(predSE,"South East",population$SE[1],R_BestGuess$SE,R_Quant$SE,rat$smoothSE,ratio$SE,filename)

#ratio$EE$hosp=sum(rowSums(predEE$SARI[recent_time,2:20]+predEE$CRIT[recent_time,2:20]+predEE$CRITREC[recent_time,2:20]))/sum(Hospital$EE$saridat[recent_time])
#ratio$EE$death=sum(predEE$DEATH[recent_time,2:20])/sum(regdeaths$`East of England`[recent_time])
#ratio$EE$newhosp=sum(rowSums(predEE$newSARI[recent_time,2:20]))/sum(Hospital$EE$newsaridat[recent_time])
#CCEE=CC_write(predEE,"East of England",population$EE[1],R_BestGuess$EE,R_Quant$EE,rat$smoothEE,ratio$EE,filename)


# Cludge for Wales, NI - only write R and growthrate, no MTPs - send predEng, CC_write will ignore it


#CCWal=CC_write(predEng,"Wales",population$Wales[1],R_BestGuess$Wales,R_Quant$Wales,rat$smoothWales,ratio$Eng,filename)
#CCNI=CC_write(predEng,"Northern Ireland",population$NI[1],R_BestGuess$NI,R_Quant$NI,rat$smoothNI,ratio$Eng,filename)


#Now combine all the sheets into one


#CC<-rbind(CCEng,CCScot,CCNW,CCNEY,CCMD,CCLon,CCSW,CCSE,CCEE,CCWal,CCNI)

#write.xlsx(CCLon, file = "correctedLon.xlsx", 
#           overwrite = TRUE,  sheetName = region, rowNames = FALSE)


#  Monitoring plots for MTP deaths
#if(interactive()){
#plotdate[2]<-plotdate[2]+50
#plot_date<-c(plotdate[1],plotdate[2])
#ymax = max(tail(rowSums(predMD$CASE[2:20]),n=100))*1.1
#plot(rowSums(predMD$CASE[2:20]),x=predMD$CASE$date,xlim=plot_date,cex.axis=0.7,ylab="Regional CASE",xlab="Date") 
#lines(rowSums(predNEY$CASE[2:20]),x=predNEY$CASE$date,xlim=plot_date,col="red") 
#lines(rowSums(predNW$CASE[2:20]),x=predNW$CASE$date,xlim=plot_date,col="blue")  
#lines(rowSums(predSW$CASE[2:20]),x=predSW$CASE$date,xlim=plot_date,col="green")  
#lines(rowSums(predSE$CASE[2:20]),x=predSE$CASE$date,xlim=plot_date,col="orange")  
#lines(rowSums(predEE$CASE[2:20]),x=predEE$CASE$date,xlim=plot_date,col="violet")  
#lines(rowSums(predLon$CASE[2:20]),x=predLon$CASE$date,xlim=plot_date,col="yellow")  
#
#plot(rowSums(predMD$DEATH[2:20]),x=predMD$DEATH$date,xlim=plot_date,cex.axis=0.7,ylab="Regional Death",xlab="Date") 
#lines(rowSums(predNEY$DEATH[2:20]),x=predNEY$DEATH$date,xlim=plot_date,col="red") 
#lines(rowSums(predNW$DEATH[2:20]),x=predNW$DEATH$date,xlim=plot_date,col="blue")  
#lines(rowSums(predSW$DEATH[2:20]),x=predSW$DEATH$date,xlim=plot_date,col="green")  
#lines(rowSums(predSE$DEATH[2:20]),x=predSE$DEATH$date,xlim=plot_date,col="orange")  
#lines(rowSums(predEE$DEATH[2:20]),x=predEE$DEATH$date,xlim=plot_date,col="violet")  
#lines(rowSums(predLon$DEATH[2:20]),x=predLon$DEATH$date,xlim=plot_date,col="yellow")  
#
#
#plot(Hospital$MD$newsaridat,x=as.Date(Hospital$MD$date),ylab="MD Hospital Admissions",xlab="Date",xlim=plot_date)
#lines(rowSums(predMD$newSARI[2:20])/ratio$MD$newhosp,x=predMD$newSARI$date)
#plot(y=Hospital$NW$newsaridat,x=Hospital$NW$date,ylab="NW Hospital Admissions",xlab="Date",xlim=plot_date)
#lines(rowSums(predNW$newSARI[2:20])/ratio$NW$newhosp,x=predNW$newSARI$date)
#plot(y=Hospital$NEY$newsaridat,x=Hospital$NEY$date,ylab="NEY Hospital Admissions",xlab="Date",xlim=plot_date)
#lines(rowSums(predNEY$newSARI[2:20])/ratio$NEY$newhosp,x=predNEY$newSARI$date)
#plot(y=Hospital$EE$newsaridat,x=Hospital$EE$date,ylab="EE Hospital Admissions",xlab="Date",xlim=plot_date)
#lines(rowSums(predEE$newSARI[2:20])/ratio$EE$newhosp,x=predEE$newSARI$date)
#plot(y=Hospital$SE$newsaridat,x=Hospital$SE$date,ylab="SE Hospital Admissions",xlab="Date",xlim=plot_date)
#lines(rowSums(predSE$newSARI[2:20])/ratio$SE$newhosp,x=predSE$newSARI$date)
#plot(y=Hospital$SW$newsaridat,x=Hospital$SW$date,ylab="SW Hospital Admissions",xlab="Date",xlim=plot_date)
#lines(rowSums(predSW$newSARI[2:20])/ratio$SW$newhosp,x=predSW$newSARI$date)
#
#plot(y=Hospital$Lon$newsaridat,x=Hospital$Lon$date,ylab="Lon Hospital Admissions",xlab="Date")
#lines(rowSums(predLon$newSARI[2:20])/ratio$Lon$newhosp,x=predLon$newSARI$date)
#plot(Hospital$Scot$newsaridat,ylab="Scotland Hospital Admissions",xlab="Date")
#lines(rowSums(predScot$newSARI[2:20])/ratio$Scot$newhosp)
#plot(y=Hospital$MD$newsaridat,x=Hospital$MD$date,ylab="MD Hospital Admissions",xlab="Date")
#lines(rowSums(predMD$newSARI[2:20])/ratio$MD$newhosp,x=predMD$newSARI$date)
#plot(Hospital$NEY$newsaridat,ylab="NE & Yorks Hospital Admissions",xlab="Date")
#lines(rowSums(predNEY$newSARI[2:20])/ratio$NEY$newhosp)
#plot(y=Hospital$SE$newsaridat,x=Hospital$SE$date,ylab="SE Hospital Admissions",xlab="Date")
#lines(rowSums(predSE$newSARI[2:20])/ratio$SE$newhosp,x=predSE$newSARI$date)
#plot(Hospital$NW$newsaridat,ylab="NW  Hospital Admissions",xlab="Date")
#lines(rowSums(predNW$newSARI[2:20])/ratio$NW$newhosp)
#plot(y=Hospital$EE$newsaridat,x=Hospital$EE$date,ylab="EE Hospital Admissions",xlab="Date")
#lines(rowSums(predEE$newSARI[2:20])/ratio$EE$newhosp,x=predSE$newSARI$date)
#plot(Hospital$SW$newsaridat,ylab="SW  Hospital Admissions",xlab="Date")
#lines(rowSums(predSW$newSARI[2:20])/ratio$SW$newhosp)
#
#
#plot(y=Hospital$Eng$saridat,x=Hospital$EE$date,ylab="England Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predEng$SARI[2:20]+predEng$CRIT[2:20]+predEng$CRITREC[2:20])/ratio$Eng$hosp,x=predEng$newSARI$date)
#
#plot(y=Hospital$Scot$saridat,x=Hospital$Scot$date,ylab="Scotland Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predScot$SARI[2:20]+predScot$CRIT[2:20]+predScot$CRITREC[2:20])/ratio$Scot$hosp,x=predScot$newSARI$date)
#plot(y=Hospital$MD$saridat,x=Hospital$MD$date,ylab="MD Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predMD$SARI[2:20]+predMD$CRIT[2:20]+predMD$CRITREC[2:20])/ratio$MD$hosp,x=predMD$newSARI$date)
#plot(y=Hospital$NW$saridat,x=Hospital$NW$date,ylab="NW Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predNW$SARI[2:20]+predNW$CRIT[2:20]+predNW$CRITREC[2:20])/ratio$NW$hosp,x=predNW$newSARI$date)
#plot(y=Hospital$NEY$saridat,x=Hospital$NEY$date,ylab="NEY Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predNEY$SARI[2:20]+predNEY$CRIT[2:20]+predNEY$CRITREC[2:20])/ratio$NEY$hosp,x=predNEY$newSARI$date)
#plot(y=Hospital$EE$saridat,x=Hospital$EE$date,ylab="EE Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predEE$SARI[2:20]+predEE$CRIT[2:20]+predEE$CRITREC[2:20])/ratio$EE$hosp,x=predEE$newSARI$date)
#plot(y=Hospital$SE$saridat,x=Hospital$SE$date,ylab="SE Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predSE$SARI[2:20]+predSE$CRIT[2:20]+predSE$CRITREC[2:20])/ratio$SE$hosp,x=predSE$newSARI$date)
#plot(y=Hospital$SW$saridat,x=Hospital$SW$date,ylab="SW Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predSW$SARI[2:20]+predSW$CRIT[2:20]+predSW$CRITREC[2:20])/ratio$SW$hosp,x=predSW$newSARI$date)
#plot(y=Hospital$Lon$saridat,x=Hospital$Lon$date,ylab="Lon Hospital Cases",xlab="Date",xlim=plot_date)
#lines(rowSums(predLon$SARI[2:20]+predLon$CRIT[2:20]+predLon$CRITREC[2:20])/ratio$Lon$hosp,x=predLon$newSARI$date)
#
#
#
##Admissions Uk total and by region
#sum(na.locf(Hospital$Eng$saridat))
#sum(na.locf(Hospital$UK$saridat))
#sum(na.locf(Hospital$NEY$saridat)+Hospital$NW$saridat+Hospital$EE$saridat+Hospital$MD$saridat+Hospital$Lon$saridat+Hospital$SE$saridat+Hospital$SW$saridat+Hospital$Scot$saridat+na.locf(Hospital$Wal$saridat)+na.locf(Hospital$NI$saridat))
#sum(rowSums(compEng$SARI[2:20]+compEng$CRIT[2:20]+compEng$CRITREC[2:20]))
#sum(na.locf(Hospital$Eng$newsaridat))
#sum(na.locf(compEng$newSARI[2:20]))
#
#}
#
#