#Assumes inHg pressure measurements in excel sheet.
#If mmHg:
###Go into the `gather_data` function
###Change the line: press <- mean(sub$Pressure) * 25.4
###To: press <- mean(sub$Pressure)


#Runs assumming an excel sheet with the following columns, but will run even if some are missing: 
###N2.Ar, X40, X32, X28, O2.Ar, X29.28
###Column names are not case sensitive
library(readxl)

######EDIT THESE, and be sure to comment out the pressure line that is wrong for you (whether your pressure is in inHg or mmHg). It currently runs for inHg of pressure.
MIMSdata <- "2020_11_02_FMP.xlsx"
rawFile <- "2020_11_05a.csv"
saveFile <- "2020_11_02_FMP_final.csv"
pressure <- "inHg"
#pressure <- "mmHg"
source("mims_gas_functions.r")
######EDIT THESE
#
#
#

#Formats and reads in the raw files in the best manner for the gather_data function
read_raw_file <- function(filePath){
  nCols <- max(count.fields(filePath, sep = ','))
  firstRead <- read.csv(filePath, header = FALSE, col.names = paste0("V",seq_len(nCols)), 
                        fill = TRUE, stringsAsFactors = FALSE)
  trimmedRead <- firstRead[cumsum(complete.cases(firstRead)) != 0,]
  header <- as.character(trimmedRead[1,])
  colnames(trimmedRead) <- header
  trimmedRead <- trimmedRead[-1,]
  
  trimmedRead$Index <- as.numeric(as.character(trimmedRead$Index))
  
  range <- c(min(trimmedRead$Index), max(trimmedRead$Index))
  return(list(trimmedRead, range))
}

#Averages data from the raw document
gather_data <- function(excelfile, folder_with_raw_files){
  name <- read_excel(excelfile)
  rundate <- format(as.Date(name$rundate[1]), "%Y_%m_%d")
  name <- name[!is.na(name$SampleID),]
  name_range <- c(min(name$Index), max(name$Index))
  
  #Check for a raw file from the run date, then check contents to see if it includes 
  #all relevant indices
  pars <- read_raw_file(rawFile)
  rawname <- pars[[1]]
  raw_range <- pars[[2]]
  if (raw_range[1] > name_range[1] || raw_range[2] < name_range[2]){
    print(paste0(rawFile, " does not contain all of the indices of your excel sheet. Check to make sure this is the raw file you'd like to use and either replace \'rawFile\' with the correct file or run the other script to just use the excel sheet."))
  }
  datedf <- data.frame()
  for (j in unique(name$Samp)){
    sub <- name[(name$Samp == j),]
    indexes <- c(min(sub$Index), max(sub$Index))
    data <- rawname[(rawname$Index >= indexes[1]) & (rawname$Index <= indexes[2]),]
    data <- data[ , -which(names(data) %in% c("Index"))]
    data$Time <- as.numeric(as.POSIXct(data$Time, format = "%m/%d/%Y %H:%M:%S"))
    
    data[] <- lapply(data, as.numeric)
    
    #Sneak in the inHg to mmHg correction
    newdat <- data.frame(lapply(colMeans(data), type.convert), stringsAsFactors=FALSE)
    newdat$Time <- as.POSIXct(newdat$Time, origin = "1970-01-01")
    options(warn = -1)
    temp <- mean(sub$Temp)
    if (pressure == "inHg"){
      press <- mean(sub$Pressure) * 25.4
    } else if (pressure == "mmHg"){
      press <- mean(sub$Pressure)}
    else{print("Choose your pressure setting at the beginning of the code")}
    metadat <- data.frame("Samp" = sub[["Samp"]][1], "SampleID" = sub[["SampleID"]][1], 
                          "Pressure" = press, "Temp" = temp, 
                          "Calibnum" = sub[["Calibnum"]][1],
                          "Depth" = sub[["Depth"]][1], 
                          "WatDens" = watdens(temp), "O2Sat" = osat1(temp, press), 
                          "N2Sat" = nsat(temp, press), "ArSat" = arsat(temp, press), 
                          "O2.ArSat" = osat1(temp, press)/arsat(temp, press),
                          "N2.ArSat" = nsat(temp, press)/arsat(temp, press))
    
    options(warn = 0)
    
    metadat$Calibnum[is.na(metadat$Calibnum)] <- 
      as.numeric(as.character(sub[["Sampnum"]][1][is.na(sub[["Calibnum"]][1])]))
    
    tempdf <- merge(metadat, newdat)
    
    datedf <- rbind(datedf, tempdf)
  }
  
  return(datedf)
}

avgdData <- gather_data(MIMSdata, rawFile)

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
      
      calibs <- sub[grep("std", tolower(sub$SampleID)),]
      #Linear between low and high temps
      offsetfun <- lm(calibs[, which(!is.na(match(tolower(colnames(calibs)), 
                                                  tolower(targCol))))] ~ calibs[[satCol]])
      coeff1 <- coef(summary(offsetfun))[1]
      coeff2 <- coef(summary(offsetfun))[2]
      
      #Multiply currents
      sub[[newcolname]] <- (sub[, which(!is.na(match(tolower(colnames(sub)), 
                                                     tolower(targCol))))] - coeff1) / coeff2
      
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
      
      calibs <- sub[grep("std", tolower(sub$SampleID)),]
      #Linear between 0 and all temps
      xvals <- c(0, calibs[, which(!is.na(match(tolower(colnames(calibs)), 
                                                tolower("O2.ArSat"))))])
      yvals <- c(0, calibs[, which(!is.na(match(tolower(colnames(calibs)), 
                                                tolower("O2.Ar"))))])
      offsetfun <- lm(yvals ~ xvals)
      coeff1 <- coef(summary(offsetfun))[1]
      coeff2 <- coef(summary(offsetfun))[2]
      
      #Multiply currents
      sub[[newcolname]] <- (sub[, which(!is.na(match(tolower(colnames(sub)), 
                                                             tolower("O2.Ar"))))] - coeff1) / coeff2
      
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
      
      calibs <- sub[grep("std", tolower(sub$SampleID)),]
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
others <- grep("Conc|Sat|del|Wat|Time", colnames(avgdData), invert = TRUE)
avgdData <- avgdData[, c(others, saturations, concentrations)]

#Saves data to a csv file
write.csv(avgdData, saveFile, quote = FALSE)

failTargs <- failTargs[!is.na(failTargs)]
successTargs <- successTargs[!is.na(successTargs)]

print(paste0("Program ran successfully for ", 
             paste(unlist(successTargs), collapse = ", "), 
             "."), quote = FALSE)
print(paste0("Saved to ", saveFile, "."), quote = FALSE)
if(length(failTargs)>0){
  print(paste0("Program failed for ", failTargs, 
               ". Please check column names if you expected this to run."), quote = FALSE)
}
