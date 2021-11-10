#Assumes inHg pressure measurements in excel sheet.
#If mmHg:
###Go into the `gather_data` function
###Change the line: press <- mean(sub$Pressure) * 25.4
###To: press <- mean(sub$Pressure)


#Runs assumming an excel sheet with the following columns, but will run even if some are missing: 
###N2.Ar, X40, X32, X28, O2.Ar, X29.28
###Column names are not case sensitive
library(readxl)


######EDIT THESE
MIMSdata <- "2019_06_11_10m_LD.xlsx"
saveFile <- "2020_11_02_FMP_final_noraw.csv"
source("mims_gas_functions.r")
######EDIT THESE
#
#
#

#Averages data, just from the excel document
gather_data <- function(excelfile){
  name <- read_excel(excelfile)
  datedf <- data.frame()
  for (j in unique(name$Samp)){
    sub <- name[(name$Samp == j),]
    colnames <- names(sub)
    start <- which(colnames == "Time")
    end <- which(colnames == "Sampleset") - 1
    data <- sub[,start:end]
    data$Time <- as.numeric(as.POSIXct(data$Time, format = "%m/%d/%Y %H:%M:%S"))
    
    #Sneak in the inHg to mmHg correction
    newdat <- data.frame(lapply(colMeans(data), type.convert), stringsAsFactors=FALSE)
    newdat$Time <- as.POSIXct(newdat$Time, origin = "1970-01-01")
    temp <- mean(sub$Temp)
    press <- mean(sub$Pressure) * 25.4
    metadat <- data.frame("Samp" = sub[["Samp"]][1], "SampleID" = sub[["SampleID"]][1], 
                          "Pressure" = press, "Temp" = temp, 
                          "Calibnum" = sub[["Calibnum"]][1],
                          "Depth" = sub[["Depth"]][1], 
                          "WatDens" = watdens(temp), "O2Sat" = osat1(temp, press), 
                          "N2Sat" = nsat(temp, press), "ArSat" = arsat(temp, press), 
                          "O2.ArSat" = osat1(temp, press)/arsat(temp, press),
                          "N2.ArSat" = nsat(temp, press)/arsat(temp, press))
    
    metadat$Calibnum[is.na(metadat$Calibnum)] <- 
      as.numeric(as.character(sub[["Sampnum"]][1][is.na(sub[["Calibnum"]][1])]))
    
    tempdf <- merge(metadat, newdat)
    
    datedf <- rbind(datedf, tempdf)
  }
  
  return(datedf)
}

avgdData <- gather_data(MIMSdata)

targCols <- c("N2.Ar", "X40", "X32", "X28")
satCols <- c("N2.ArSat", "ArSat", "O2Sat", "N2Sat")
#Checks to make sure targCols and satCols are equal lengths (should correspond 1:1)
if(length(targCols) != length(satCols)){
  print("targCols and satCols need to be the same length.")
}

#Calculations concentrations for the targCols and satCols lists above
getRatioGeneral <- function(df, targCol, satCol){
  newcolname <- paste0(targCol, ".Conc")
  
  if(length(which(!is.na(match(tolower(colnames(df)), 
                               tolower(targCol))))) == 1){
    
    #Pull out differences of each calibnum
    for (i in 1:length(unique(df$Calibnum))){
      sub <- df[df$Calibnum == i | df$Calibnum == i+1,]
      
      calibs <- sub[sub$Depth == "Stdl" | sub$Depth == "Stdh",]
      #Linear between low and high temps
      offsetfun <- lm(calibs[[satCol]] ~ 
                        calibs[, which(!is.na(match(tolower(colnames(calibs)), 
                                                    tolower(targCol))))])
      coeff1 <- coef(summary(offsetfun))[1]
      coeff2 <- coef(summary(offsetfun))[2]
      
      #Multiply currents
      sub[[newcolname]] <- 
        coeff1 + sub[, which(!is.na(match(tolower(colnames(sub)), 
                                          tolower(targCol))))]*coeff2
      
      df[[newcolname]][!is.na(base::match(df$Samp, sub$Samp))]<- sub[[newcolname]]
    }
    successTargs <- targCol
    failTargs <- NA
  }else{
    successTargs <- NA
    failTargs <- targCol
  }
  
  return(list(df, successTargs, failTargs))
}
#Calculates O2:Ar ratio
getRatioO2Ar <- function(df){
  newcolname <- "O2.Ar.Conc"
  
  if(length(which(!is.na(match(tolower(colnames(df)), 
                               tolower("O2.Ar"))))) == 1){
    
    for (i in 1:length(unique(df$Calibnum))){
      sub <- df[df$Calibnum == i | df$Calibnum == i+1,]
      
      calibs <- sub[sub$Depth == "Stdl" | sub$Depth == "Stdh",]
      #Linear between 0 and all temps
      yvals <- c(0, calibs[, which(!is.na(match(tolower(colnames(calibs)), 
                                                tolower("O2.ArSat"))))])
      xvals <- c(0, calibs[, which(!is.na(match(tolower(colnames(calibs)), 
                                                tolower("O2.Ar"))))])
      offsetfun <- lm(yvals ~ xvals)
      coeff1 <- coef(summary(offsetfun))[1]
      coeff2 <- coef(summary(offsetfun))[2]
      
      #Multiply currents
      sub[[newcolname]] <- coeff1 + sub[, which(!is.na(match(tolower(colnames(sub)), 
                                                             tolower("O2.Ar"))))]*coeff2
      
      df[[newcolname]][!is.na(base::match(df$Samp, sub$Samp))]<- sub[[newcolname]]
    }
    successTargs <- "O2.Ar"
    failTargs <- NA
  }else{
    successTargs <- NA
    failTargs <- "O2.Ar"
  }
  return(list(df, successTargs, failTargs))
}
#Calculates 29:28 ratio
getRatio29.28 <- function(df){
  newcolname <- "del15N"
  
  if(length(which(!is.na(match(tolower(colnames(df)), 
                               tolower("X29.28"))))) == 1){
    
    for (i in 1:length(unique(df$Calibnum))){
      sub <- df[df$Calibnum == i | df$Calibnum == i+1,]
      
      calibs <- sub[sub$Depth == "Stdl" | sub$Depth == "Stdh",]
      #Linear between 0 and all temps
      stdVals <- calibs[, which(!is.na(match(tolower(colnames(calibs)), 
                                             tolower("X29.28"))))]
      Rstd <- mean(stdVals)

      #Multiply currents
      sub[[newcolname]] <- (sub[, which(!is.na(match(tolower(colnames(sub)), 
                                                     tolower("X29.28"))))]/Rstd - 1)*1000
      
      df[[newcolname]][!is.na(base::match(df$Samp, sub$Samp))]<- sub[[newcolname]]
    }
    successTargs <- "X29.28"
    failTargs <- NA
  }else{
    successTargs <- NA
    failTargs <- "X29.28"
  }
  
  return(list(df, successTargs, failTargs))
}

successTargs <- c(NA)
failTargs <- c(NA)
#Runs the three functions above
for (i in 1:length(targCols)){
  allResults <- getRatioGeneral(avgdData, targCols[i], satCols[i])
  avgdData <- allResults[[1]]
  successTargs <- c(successTargs, allResults[[2]])
  failTargs <- c(failTargs, allResults[[3]])
}
allResults <- getRatioO2Ar(avgdData)
avgdData <- allResults[[1]]
successTargs <- c(successTargs, allResults[[2]])
failTargs <- c(failTargs, allResults[[3]])

allResults <- getRatio29.28(avgdData)
avgdData <- allResults[[1]]
successTargs <- c(successTargs, allResults[[2]])
failTargs <- c(failTargs, allResults[[3]])

#Reorders columns (Misc sample info, current values, saturation values, concentration values)
concentrations <- grep("Conc|del", colnames(avgdData))
saturations <- grep("Sat", colnames(avgdData))
others <- grep("Conc|Sat|del|Wat|Time|Samp", colnames(avgdData), invert = TRUE)
avgdData <- avgdData[, c(others, saturations, concentrations)]

#Saves data to a csv file
write.csv(avgdData, saveFile, quote = FALSE)

failTargs <- failTargs[!is.na(failTargs)]
successTargs <- successTargs[!is.na(successTargs)]

print(paste0("Program run successfully for ", 
             paste(unlist(successTargs), collapse = ", "), 
             "."), quote = FALSE)
print(paste0("Saved to ", saveFile, "."), quote = FALSE)
if(length(failTargs)>0){
  print(paste0("Program failed for ", failTargs, 
               ". Please check column names if you expected this to run."), quote = FALSE)
}