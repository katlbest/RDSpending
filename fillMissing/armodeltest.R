#NOTES:======================================================================

#libraries and functions====================================================================
library(plyr)
library(ggplot2)
library(descr)
library(tseries)
library(gridExtra)
library(reshape2)
library(MARSS)
library(KFAS)
library(car)
library(mlogit)
library(nnet)
library(MASS)
#library(lme4)

#clear workspace ==============================================================
rm(list = ls())

#data i/o=======================================================================
  RDDATA = read.csv("RDDATA.csv") 
  #note: input file has been slightly modified by STATA and has date year and industry/year ID extracted
  CONCATID = read.csv("concat.csv")
  RDDATA$iyID = CONCATID$iyID
  RDDATA$datadate = as.Date(RDDATA$datadate, "%d%b%Y")
  RDDATA$datayear = as.numeric(format(RDDATA$datadate, format = "%Y"))
  RDDATA$npatappAdj = RDDATA$npatapp/RDDATA$sale

#create raw indexes=======================================================================
  #pull raw R&D index and patent index
    TEMP = RDDATA[,c("iyID", "xrdAdj")]
    TEMP = na.exclude(TEMP)
    TEMP = melt(TEMP, id = "iyID")
    INDTABLE = dcast(TEMP, iyID~variable, mean)
    INDTABLE2 = data.frame(freq(ordered(TEMP$iyID), plot=FALSE))
    INDTABLE2 = INDTABLE2[1:(nrow(INDTABLE2)-1),]
    INDTABLE2$iyID = rownames(INDTABLE2)
    INDTABLE2= INDTABLE2[,c(1,4)]
    INDTABLE = merge(x = INDTABLE, y = INDTABLE2, by = "iyID", all.x = TRUE)
    colnames(INDTABLE)= c("iyID", "rawRD", "RDCount")
    TEMP = RDDATA[,c("iyID", "npatappAdj")]
    TEMP = na.exclude(TEMP)
    TEMP = melt(TEMP, id = "iyID")
    TEMP2 = dcast(TEMP, iyID~variable, mean)
    TEMP3 = data.frame(freq(ordered(TEMP$iyID), plot=FALSE))
    TEMP3 = TEMP3[1:(nrow(TEMP3)-1),]
    TEMP3$iyID = rownames(TEMP3)
    TEMP3= TEMP3[,c(1,4)]
    INDTABLE = merge(x = INDTABLE, y = TEMP2, by = "iyID", all.x = TRUE)
    INDTABLE = merge(x = INDTABLE, y = TEMP3, by = "iyID", all.x = TRUE)
    colnames(INDTABLE)= c("iyID", "rawRD", "RDCount", "rawNumPat", "numPatCount")
    #write.csv(INDTABLE,"C:/Users/Katharina/Documents/Umich/rdspend/rawindex.csv")

#create model inputs=======================================================================
  #set up RD only data for model input
    RDONLY = data.frame(gvkey = RDDATA$gvkey, xrdAdj = RDDATA$xrdAdj, datadate = RDDATA$datadate,sic = RDDATA$sic, datayear = RDDATA$datayear)
    RDONLY = RDONLY[order(RDONLY$gvkey),]
    NOMISS = na.exclude(RDONLY)

  #create input lists by industry 
    indList = levels(factor(RDONLY$sic))
    dataList = list()
    nameVector = c()
  for (i in 1:length(indList)){
    curData = RDONLY[RDONLY$sic ==indList[i],]
    #if (nrow(curData) > 50){
    dataList[[length(dataList)+1]]= curData
    nameVector[[length(nameVector)+1]]= indList[i]
    #}
  }

  #clean inputs for each industry
    oneVarList = list()
    numCosList = list()
    startYearList = list()
    for (i in 1:length(nameVector)){
      curData = dataList[[i]]
      curDataOneVar = ddply(curData, ~datayear, 
        function(df) {
          res = data.frame(rbind(df$xrdAdj)) #take R&D spending as a percentage of sales
          names(res) = sprintf("%s",df$gvkey)
          res
        }
      )
      startYearList[[i]]= min(curDataOneVar$datayear)
      numCos = ncol(curDataOneVar)-1
      model.data = curDataOneVar[-c(1)] #delete time entry
      model.data = as.matrix(model.data)
      model.data = t(model.data)
      oneVarList[[i]]= model.data
      numCosList[[i]] = numCos
    }

#model setup================================================================================
  BAll = "identity"
  QAll= "diagonal and unequal" #note this could be changed if it causes problems since the unequal portion is irrelevant (it is a 1 by 1 matrix)
  AAll = "equal" #company inputs are allowed to have trends
  UAll = "equal" 
  RAll = "diagonal and equal"
  ZAll = matrix(1,1,1)
  pointList = data.frame(matrix(ncol = 5, nrow = 0))
  colnames(pointList) = c("industryName","companyName", "raw", "arEst", "ssEst")
  output.data = data.frame(matrix(ncol = 4, nrow = 0))
  colnames(output.data)= c("industryName", "companyName", "coeff1",  "intercept")
  output.data.ss = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(output.data.ss)= c("industryName", "companyName", "logLik", "numParams", "AICc", "States", "SEs", "converge")
  control.list = list(safe = TRUE, trace =1, allow.degen= TRUE, maxit = 1000,conv.test.slope.tol=0.1)

#run models with 10 points and project up to 15===============================================================
  for (i in 1:length(oneVarList)){ #industry loop 
    curData = oneVarList[[i]]
    industryName = nameVector[i] 
    companyNameVector = rownames(curData)
    for (j in 1:(nrow(curData))){#for each company, run model
      #set up inputdata
      companyName = companyNameVector[j]
      coData = curData[j,]
      if (length(coData[is.na(coData)])<length(coData)){ #we have some non-NA entries
        coData = na.trim(coData)
        numYears = length(coData)
        if (numYears > 14 & length(coData)== length(na.exclude(coData))){ #we must have 15 non-NA points
          coDataUse = coData[1:10] #use first 10 points for model
          curPointList = data.frame(matrix(ncol = 5, nrow = numYears))
          colnames(curPointList) = c("industryName","companyName", "raw", "arEst", "ssEst")
          curPointList$raw= coData
          curPointList$industryName = industryName
          curPointList$companyName = companyName
          #run model AR model
            curMod = ar.ols(coDataUse, AIC = FALSE, order.max = 1, intercept = TRUE, demean = FALSE, na.action = na.exclude)
            if (is.na(curMod$ar[1])){
              coeffs = c(NA, NA, NA)
            }
            else{
              coeffs = curMod$ar[[1]] 
              int = curMod$x.intercept
              coeffs = c(coeffs, int)
            }
            #do prediction here
            preds = predict(curMod, newdata = coData[1:10],n.ahead = (numYears-10))
            curPointList$arEst[11:numYears]= preds$pred
          #run state space model
            model.list = list(B=BAll, U=UAll, Q=QAll, A=AAll, R=RAll,  Z=ZAll)
            model.current = MARSS(coDataUse, model = model.list, miss.value =NA, control = control.list)
            #store output
            if (is.null(model.current$num.params)){
              print("failone")
              numParams = NA
              AICc = NA
              curStates = NA
              curSE = NA
              curConv = NA
              stateVect = rep(NA, numYears)
              seVect = rep(NA, numYears)
            } else{
              numParams = model.current$num.params
              AICc = model.current$AICc
              stateVect = model.current$states[1,]
              seVect = model.current$states.se[1,]
              curStates = toString(model.current$states)
              curSE = toString(model.current$states.se)
              curConv = model.current$converge
              #predict and store
                sim.data=MARSSsimulate(model.current, nsim=1, tSteps=(numYears-10))
                curPointList$ssEst[11:numYears] = sim.data$sim.data
            }
            if (is.null(model.current$logLik)){
              logLik = NA
            }else{
              logLik = model.current$logLik
            }
          #write outputs
            #ar
              cur.outdata =data.frame(industry= industryName,company= companyName, coeff1 = coeffs[1], int = coeffs[2], stringsAsFactors = FALSE)
              colnames(cur.outdata)= colnames(output.data)
              output.data= rbind(output.data, cur.outdata)
            #ss
              cur.outdata =data.frame(industry= industryName,company= companyName, logLik = logLik, numParams = numParams, AICc = AICc, states = curStates, ses = curSE, converge =curConv, stringsAsFactors = FALSE)
              colnames(cur.outdata)= colnames(output.data.ss)
              output.data.ss= rbind(output.data.ss, cur.outdata)
            #points
              colnames(curPointList)= colnames(pointList)
              pointList = rbind(pointList, curPointList)
        }
      }
    }
  }
  write.csv(output.data, "arout.csv")
  write.csv(pointList, "pointList.csv")
  write.csv(output.data.ss, "ssout.csv")


#company level models=================================================================
  #note run only on companies with 15 data points
  output.data = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(output.data)= c("industryName", "companyName", "logLik", "numParams", "AICc", "States", "SEs", "converge")
  output.outofsample = data.frame(matrix(ncol = 5, nrow = 0))
  colnames(output.outofsample) = c("industryName", "companyName", "datayear", "state", "se")
  #set model inputs
  BAll = "identity"
  QAll= "diagonal and unequal" #note this could be changed if it causes problems since the unequal portion is irrelevant (it is a 1 by 1 matrix)
  AAll = "equal" #company inputs are allowed to have trends
  UAll = "equal" 
  RAll = "diagonal and equal"
  ZAll = matrix(1,1,1)
  
  #set model controls, if necessary
  control.list = list(safe = TRUE, trace =1, allow.degen= TRUE, maxit = 1000,conv.test.slope.tol=0.1)
  #run models
  stringList = c()
  for (i in 1:length(oneVarList)){ #industry loop 
    curData = oneVarList[[i]]
    industryName = nameVector[i] 
    companyNameVector = rownames(curData)
    for (j in 1:(nrow(curData))){#for each company, run model
      #set up inputdata
      companyName = companyNameVector[j]
      #if (companyName %in% missedList){
      coData = curData[j,]
      if (length(coData[is.na(coData)])<length(coData)){
        nonNA1 = which(!is.na(coData)) #coData now a vector
        startIndex1 = min(nonNA1)
        endIndex1 = max(nonNA1)
        coData = coData[startIndex1:endIndex1]
        #if (!(is.null(numCos))){ #meaning we have more than 1 company
        numYears = length(coData)
        #TBD checks for missingness
        #run model
        model.list = list(B=BAll, U=UAll, Q=QAll, A=AAll, R=RAll,  Z=ZAll)
        model.current = MARSS(coData, model = model.list, miss.value =NA, control = control.list)
        #store output
        if (is.null(model.current$num.params)){
          print("failone")
          numParams = NA
          AICc = NA
          curStates = NA
          curSE = NA
          curConv = NA
          stateVect = rep(NA, numYears)
          seVect = rep(NA, numYears)
        } else{
          numParams = model.current$num.params
          AICc = model.current$AICc
          stateVect = model.current$states[1,]
          seVect = model.current$states.se[1,]
          curStates = toString(model.current$states)
          curSE = toString(model.current$states.se)
          curConv = model.current$converge
        }
        if (is.null(model.current$logLik)){
          logLik = NA
        }else{
          logLik = model.current$logLik
        }
      } else{
        print("failtwo")
        numParams = NA
        AICc = NA
        curStates = NA
        curSE = NA
        curConv = NA
        numYears = 1
        stateVect = rep(NA, numYears)
        seVect = rep(NA, numYears)
        logLik = NA
        model.current = NA
      }
      #note we took out an else here since we are not checking to make sure we have companies TBD   
      #store
      cur.outdata =data.frame(industry= industryName,company= companyName, logLik = logLik, numParams = numParams, AICc = AICc, states = curStates, ses = curSE, converge =curConv, stringsAsFactors = FALSE)
      colnames(cur.outdata)= colnames(output.data)
      output.data= rbind(output.data, cur.outdata)
      cur.outofsample = data.frame(matrix(ncol = 5, nrow = numYears))
      colnames(cur.outofsample) = colnames(output.outofsample)
      cur.outofsample$industryName = industryName
      cur.outofsample$companyName = companyName
      cur.outofsample$datayear = c(startYearList[[i]]:(startYearList[[i]]+numYears-1))
      cur.outofsample$state = stateVect
      cur.outofsample$se = seVect
      output.outofsample = rbind(output.outofsample, cur.outofsample)         
      modelString = paste("CoEdit", industryName,companyName, sep = ".")
      stringList = c(stringList, modelString)
      assign(paste("CoEdit", industryName, sep = "."), model.current)
      #}
    }
  }
  save.image(file = "CoEdit.RData")
  write.csv(output.data, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test2.csv")
  write.csv(output.outofsample, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test3.csv")
  coEdit.out = output.outofsample
  save(coEdit.out, file = "allCoOutMissed.RData") #i = 330, j = 96/141
  
  
#run AR company models