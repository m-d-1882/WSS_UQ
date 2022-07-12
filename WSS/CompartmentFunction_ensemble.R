# Try to ensure data encapsulation - do not assume any values for parameters
# that not explicitly passed into the function. Also, do not assume that any values
# that are set in the function will be available in the space from where the
# function is called from. In order to pass back multiple data frames a list
# will be used to output multiple values. Modifications to the data passed in
# will only be made to copies of the data (R passes by value) and will not
# propagate to the data passed in.

# Function Definition - naming function variables to ensure it is not taking
# values from the calling space
# Inherits vacdat from covid_trimmed
Compartment <- function(cases, csimAge, rCFR, cdat, startc, endc){
  
  # Create a list to output various data frames
  out <- list()

  # CASE is the input cases which get WSS'ed.
  # CASE=cases produces estimates for the data used.
  CASE <- cases
 #  Lognormals gone to getParms
  
  #  Follow infections through ILI (Case) - SARI (Hospital) - Crit (ICU) - CritRecov (Hospital)- Deaths
  
  #  Zero dataframes.
  #  Follow these cases to the end of the CDFs
  lengthofdata <- nrow(CASE)
  lengthofspread <- length(ILIToRecovery)
  
  #extend ILI longer than cases to allow for predictions (eventually)
  ILI <- cases
  for (i in lengthofdata:(lengthofdata+lengthofspread+predtime) ){
    ILI[i,(2:ncol(ILI))] <-  0.0
    ILI[i,1] <- ILI$date[1]+i-1
  }
  cols <- names(ILI)[2:ncol(ILI)]
  ILI[cols] <-  0.0
  MILD <- ILI
  SARI <- ILI
  CRIT <- ILI
  CRITREC <- ILI
  RECOV <- ILI
  DEATH <- ILI
  
  # These are the new arrivals in each category.  NOT the increase.  Recov and death just increase
  # Initialize with day 1 in place
  newMILD <- MILD
  newILI <- ILI
  newSARI <- SARI
  newCRIT <- CRIT
  newCRITREC <- CRITREC
  oldMILD <- MILD
  oldILI <- ILI
  oldSARI <- SARI
  oldCRIT <- CRIT
  oldCRITREC <- CRITREC
  
  #  Set day 1.  This assumes - wrongly - that there were zero cases before,
  #              but should autocorrect as those cases get resolved
  
  #  csimAge has no date row, so need to use iage-1
  
  MILD[1,(2:ncol(MILD))] <- CASE[1,(2:ncol(CASE))]
  ILI[1,(2:ncol(ILI))] <- CASE[1,(2:ncol(CASE))]*csimAge$Prop_ILI_ByAge
  SARI[1,(2:ncol(SARI))] <- CASE[1,(2:ncol(CASE))]*csimAge$Prop_SARI_ByAge
  CRIT[1,(2:ncol(CRIT))] <- CASE[1,(2:ncol(CASE))]*csimAge$Prop_Critical_ByAge
  
  # Add new cases to Mild, ILI, SARI and CRIT people in each  age group.
  # Bring forward cases from yesterday
  # Current values will typically be negative, as they are sums of people leaving the compartment
  # Nobody changes age band.  Vectorize over distributions
  

  cdat$lethality<-na.locf(cdat$lethality)
    for (iday in (startc:endc)){
    # Update current vaccine/variant lethality if available    
    day_lethality<-cdat$lethality[min(iday,length(comdat$lethality))]
    xCFR <- rCFR*day_lethality/(1+rCFR*(day_lethality-1))
    pTtoI <- afac*xCFR^apow
    pItoS <- bfac*xCFR^bpow
    pStoD <- cfac*xCFR^cpow
    #  Entry to ventilation still from covidsim
    #  Redu
    pStoC <-  csimAge$Prop_Critical_ByAge /
      ( csimAge$Prop_Critical_ByAge + csimAge$Prop_SARI_ByAge )
    # All routes to death are the same, vary by age
    pCtoD <- pStoD
    pCRtoD <- pStoD
    # Rescale pStoD to allow for CRIT->CRITREC route
    pStoD <- pStoD - pStoC*(pCtoD+(1-pCtoD)*pCRtoD)
    
    
    # Proportions become variant dependent.
    # ILI is case driven, so extra infectivity is automatic from the data.
    # ILI -> SARI increases with variant.
    # CRIT is an NHS decision, not favoured for very old
    # Need to increase CFR without exceeding 1.
    # Note inverse lethality isnt a simple % as CFR cant be >1
    # Will have negative people  trouble if CFR>1
    
    xday <- iday+cdflength-1
    agerange <- (2:ncol(ILI))
    
    newMILD[iday,agerange] <- CASE[iday,agerange]*(1.0-pTtoI)+newMILD[iday,agerange]
    newILI[iday,agerange] <- CASE[iday,agerange]*  pTtoI    +newILI[iday,agerange]
    
    
 
#MtoR <- outer(as.numeric(newMILD[iday,agerange]),MildToRecovery,FUN="*")
#oldMILD[(iday:xday),agerange] <- oldMILD[(iday:xday),agerange]+MtoR
    MtoR = as.numeric(newMILD[iday-1,iage]) * MildToRecovery 
    oldMILD[(iday:xday),iage]=oldMILD[(iday:xday),iage]+MtoR
    #vacCFR <- 0.90 
    #Vaccine reduction in ILI-> SARI

    for (iage in agerange){
      # All todays new MILDs will all leave to REC across distribution
      # multiple by vaccination and its CFR reduction
      # ILI will go to SA/RI and REC
      day_vacdat=1.0-as.numeric(vacdat[min(iday,nrow(vacdat)),iage])*vacCFR
      ItoS <-  as.numeric(newILI[iday,iage] *     pItoS[iage-1]*day_vacdat ) *ILIToSARI
      # Replace with vaccine effect
      # ItoS = as.numeric(newILI[iday,iage] * pItoS[iage-1])  *ILIToSARI
      ItoR <-  as.numeric(newILI[iday,iage] *(1.0-pItoS[iage-1]*day_vacdat) ) *ILIToRecovery
      newSARI[(iday:xday),iage] <- newSARI[(iday:xday),iage]+ItoS
      oldILI[(iday:xday),iage] <- oldILI[(iday:xday),iage]+ItoR+ItoS
      # SARI will go to REC, DEATH, CRIT

      #  Assume vaccination only reduces ILI-> SARI  CFR is the StoD/StoC death rate by 0%
      # Once you are Severely Ill (hospitalised) chance of recovery is unaffected
      StoC <-  as.numeric(newSARI[iday,iage] *pStoC[iage-1]  )*SARIToCritical
      StoD <-  as.numeric(newSARI[iday,iage] *pStoD[iage-1]  )*SARIToDeath
      StoR <-  as.numeric(newSARI[iday,iage] *(1.0-pStoC[iage-1]-pStoD[iage-1]) )*SARIToRecovery
      newCRIT[(iday:xday),iage] <- newCRIT[(iday:xday),iage]+StoC
      oldSARI[(iday:xday),iage] <- oldSARI[(iday:xday),iage]+StoR+StoC+StoD
      
      # CRIT  goes to CRITREC DEATH
      CtoD <-  as.numeric(newCRIT[iday,iage]*pCtoD[(iage-1)]) *CriticalToDeath
      CtoCR <-  as.numeric(newCRIT[iday,iage]*(1.0-pCtoD[(iage-1)])) *CriticalToCritRecov
      newCRITREC[(iday:xday),iage] <- newCRITREC[(iday:xday),iage]+CtoCR
      oldCRIT[(iday:xday),iage] <- oldCRIT[(iday:xday),iage]+CtoD+CtoCR
      
      # CRITREC goes to RECOV
      CRtoR <-  as.numeric(newCRITREC[iday,iage]) *CritRecovToRecov
      oldCRITREC[(iday:xday),iage] <- oldCRITREC[(iday:xday),iage]+CRtoR
      # DEATH and RECOV are cumulative, again anticipating where "new" will end up.
      DEATH[(iday:xday),iage] <- DEATH[(iday:xday),iage]+CtoD+StoD
      RECOV[(iday:xday),iage] <- RECOV[(iday:xday),iage]+StoR+ItoR+MtoR+CRtoR
    }
    # Finally, vectorize & update todays totals: New cases + transfers from other compartments -
    # transfers to other compartments + leftover from yesterday
    MILD[iday,agerange] <- MILD[iday,agerange]+newMILD[iday,agerange]-oldMILD[iday,agerange]+MILD[(iday-1),agerange]
    ILI[iday,agerange] <- ILI[iday,agerange]+newILI[iday,agerange]-oldILI[iday,agerange]+ILI[(iday-1),agerange]
    SARI[iday,agerange] <- SARI[iday,agerange]+newSARI[iday,agerange]-oldSARI[iday,agerange]+SARI[(iday-1),agerange]
    CRIT[iday,agerange] <- CRIT[iday,agerange]+newCRIT[iday,agerange]-oldCRIT[iday,agerange]+CRIT[(iday-1),agerange]
    CRITREC[iday,agerange] <- CRITREC[iday,agerange]+newCRITREC[iday,agerange]-oldCRITREC[iday,agerange]+CRITREC[(iday-1),agerange]
  }
  
  # Pack anything that you want to use - anything not returned will not have a
  # value in the calling space.
  out$DEATH <- DEATH
  out$RECOV <- RECOV
  out$MILD <- MILD
  out$oldMILD <- oldMILD
  out$newMILD <- newMILD
  out$ILI <- ILI
  out$oldILI <- oldILI
  out$newILI <- newILI
  out$SARI <- SARI
  out$oldSARI <- oldSARI
  out$newSARI <- newSARI
  out$CRIT <- CRIT
  out$oldCRIT <- oldCRIT
  out$newCRIT <- newCRIT
  out$CRITREC <- CRITREC
  out$oldCRITREC <- oldCRITREC
  out$newCRITREC <- newCRITREC
  out$CASE <-  CASE
  out$pCtoD <- pCtoD
  out$pStoC <- pStoC
  out$pStoD <- pStoD
  out$pTtoI <- pTtoI
  out$pItoS <- pItoS
  out$MildToRecovery <- MildToRecovery
  out$xday <-  xday
  out$vacCFR <-  vacCFR

  return(out)
  
}# End of compartment function
