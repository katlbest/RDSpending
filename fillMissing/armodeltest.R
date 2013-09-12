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
  
  #set up RD and num patents data
    RDANDNUM = data.frame(gvkey = RDDATA$gvkey, npatappAdj = RDDATA$npatappAdj, xrdAdj = RDDATA$xrdAdj, datadate = RDDATA$datadate,sic = RDDATA$sic, datayear = RDDATA$datayear)
    RDANDNUM = RDANDNUM[order(RDANDNUM$gvkey),]
    RDANDNUM = RDANDNUM[!(is.na(RDANDNUM$xrdAdj)),]

  #create input lists by industry 
    indList = levels(factor(RDANDNUM$sic))
    dataList = list()
    nameVector = c()
    for (i in 1:length(indList)){
      #curData = RDONLY[RDONLY$sic ==indList[i],]
      curData = RDANDNUM[RDANDNUM$sic == indList[i],]
      dataList[[length(dataList)+1]]= curData
      nameVector[[length(nameVector)+1]]= indList[i]
      #}
    }

  #clean inputs for each industry
    oneVarList = list()
    twoVarList = list()
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
      curDataTwoVar = ddply(curData, ~datayear, 
                            function(df) {
                              res = data.frame(rbind(df$npatappAdj))
                              names(res) = sprintf("%s",df$gvkey)
                              res
                            }
      )
      numCos = ncol(curDataOneVar)-1
      model.data = curDataOneVar[-c(1)] #delete time entry
      model.data2 =curDataTwoVar[-c(1)]
      model.data = as.matrix(model.data)
      model.data2 = as.matrix(model.data2)
      model.data = t(model.data)
      model.data2 = t(model.data2)
      model.data2 = rbind(model.data, model.data2)
      oneVarList[[i]]= model.data
      twoVarList[[i]]= model.data2
      numCosList[[i]] = numCos
      startYearList[[i]]= min(curDataOneVar$datayear)
      oneVarList[[i]]= model.data
      numCosList[[i]] = numCos
      twoVarList[[i]]= model.data2
    }

#model setup================================================================================
  BAll = "identity"
  QAll= "diagonal and unequal" #note this could be changed if it causes problems since the unequal portion is irrelevant (it is a 1 by 1 matrix)
  AAll = "equal" #company inputs are allowed to have trends
  UAll = "equal" 
  RAll = "diagonal and equal"
  ZAll = matrix(1,1,1)
  pointList = data.frame(matrix(ncol = 11, nrow = 0))
  colnames(pointList) = c("industryName","companyName", "year", "raw", "numPats","arEst", "ssEst", "meanEst", "maEst", "isEst", "armaEst")
  output.data = data.frame(matrix(ncol = 4, nrow = 0))
  colnames(output.data)= c("industryName", "companyName", "coeff1",  "intercept")
  output.data.ss = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(output.data.ss)= c("industryName", "companyName", "logLik", "numParams", "AICc", "States", "SEs", "converge")
  control.list = list(safe = TRUE, trace =1, allow.degen= TRUE, maxit = 1000,conv.test.slope.tol=0.1)

#run models with 10 points and project up to 15===============================================================
  for (i in 1:length(twoVarList)){ #industry loop 
    curData = twoVarList[[i]]
    industryName = nameVector[i] 
    companyNameVector = rownames(curData)
    for (j in 1:(nrow(curData)/2)){#for each company, run model
      #set up inputdata
      companyName = companyNameVector[j]
      coData = curData[j,]
      coNum = curData[j+nrow(curData)/2,]
      if (length(coData[is.na(coData)])<length(coData)){ #we have some non-NA entries
        nonNA1 = which(!is.na(coData))
        startIndex1 = min(nonNA1)
        endIndex1 = max(nonNA1)
        startYear = startYearList[[i]][1]-startIndex1 + 1
        coNum = coNum[startIndex1:endIndex1]
        coData = na.trim(coData)
        numYears = length(coData)
        if (numYears > 14 & length(coData)== length(na.exclude(coData))){ #we must have 15 non-NA points
          coDataUse = coData[1:10] #use first 10 points for model
          curPointList = data.frame(matrix(ncol =11, nrow = numYears))
          colnames(curPointList) = c("industryName","companyName", "year", "raw","numPats", "arEst", "ssEst", "meanEst", "maEst", "isEst", "armaEst")
          curPointList$raw= coData
          curPointList$industryName = industryName
          curPointList$companyName = companyName
          curPointList$year = c(startYear:(startYear+numYears-1))
          curPointList$numPats = coNum
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
          #run mean model and predict
            curMod = mean(coDataUse)
            preds = rep(curMod, numYears-11)
            curPointList$meanEst[11:numYears] = curMod
          #run MA(2) model and predict
            # curMod = arma(coDataUse,order = c(0,2), include.intercept = TRUE)
              if(length(coDataUse)==length(coDataUse[coDataUse==0])){
                coeffes = c(NA,NA,NA)
              }
              else{
                curModMA= arima(coDataUse, order = c(0,0,2))  
                coeffs = curModMA$coef
              }
            #do prediction here
              predsMA = predict(curModMA,n.ahead = (numYears-10))
              curPointList$maEst[11:numYears]= predsMA$pred
          #do idiosyncratic error
            stdev= sd(coDataUse)
            predsIS = rnorm(n=numYears-10, m=curMod, sd=stdev) 
            curPointList$isEst[11:numYears]= predsIS
          #run arma model
            #if(length(coDataUse)==length(coDataUse[coDataUse==0])){
            #  coeffs = c(NA,NA,NA,NA)
            #}
            #else{
            #  curModARMA= arima(coDataUse, order = c(1,0,2))  
            #  coeffs = curModARMA$coef
          #}
          #do prediction here
            #predsARMA = predict(curModARMA,n.ahead = (numYears-10))
            #curPointList$armaEst[11:numYears]= predsARMA$pred
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
  write.csv(pointList, "pointList2.csv")
  write.csv(output.data.ss, "ssout.csv")

#get results=============================================================================
  #optional--read in pointlist
    pointList = read.csv("inputs/compiledPointList.csv")
  #overview of numbers of companies
    length(levels(as.factor(pointList$companyName)))
    successAR = pointList[!is.na(pointList$arEst),]
    length(levels(as.factor(successAR$companyName)))
    successSS = pointList[!is.na(pointList$ssEst),]
    length(levels(as.factor(successSS$companyName)))
    successMean = pointList[!is.na(pointList$meanEst),]
    successMA = pointList[!is.na(pointList$maEst),]
    successIS = pointList[!is.na(pointList$isEst),]
  #select only companies where state space model runs
    #successCos = levels(as.factor(successSS$companyName))
    #pointListUse = pointList[pointList$companyName %in% successCos,]
    #pointListUse = pointListUse[!(is.na(pointListUse$arEst)),]
  #determine error, overall
    MseAR = sum((successAR$arEst-successAR$raw)^2)
    MseSS = sum((successSS$ssEst-successSS$raw)^2)
    MseMean = sum((successMean$meanEst-successMean$raw)^2)
  #determine error, by company
    #AR model
      successAR$seAR = (successAR$arEst-successAR$raw)^2
      errors_AR = data.frame(companyName = aggregate(successAR$seAR, list(gp=successAR$companyName), sum)$gp,
        mseAR = aggregate(successAR$seAR, list(gp=successAR$companyName), mean)$x)
      mean(errors_AR$mseAR)
      sd(errors_AR$mseAR)
      errors_AR = errors_AR[!(errors_AR$companyName %in% c(1585,24922,11768,24473,25302,27747,29245,20245,27727,23605,12227,12274,13794)),]
      mean(errors_AR$mseAR)
      sd(errors_AR$mseAR)
    #state space model             
      successSS$seSS = (successSS$ssEst-successSS$raw)^2
      errors_SS = data.frame(companyName = aggregate(successSS$seSS, list(gp=successSS$companyName), sum)$gp,
        mseSS = aggregate(successSS$seSS, list(gp=successSS$companyName), mean)$x)
      mean(errors_SS$mseSS)
      sd(errors_SS$mseSS)
      errors_SS = errors_SS[!(errors_SS$companyName %in% c(1585,24922,11768,24473,25302,27747,29245,20245,27727,23605,12227,12274,13794)),]
      mean(errors_SS$mseSS)
      sd(errors_SS$mseSS)
    #mean model             
      successMean$seMean = (successMean$meanEst-successMean$raw)^2
      errors_Mean = data.frame(companyName = aggregate(successMean$seMean, list(gp=successMean$companyName), sum)$gp,
        mseMean = aggregate(successMean$seMean, list(gp=successMean$companyName), mean)$x)
      mean(errors_Mean$mseMean)
      sd(errors_Mean$mseMean)
      errors_Mean = errors_Mean[!(errors_Mean$companyName %in% c(1585,24922,11768,24473,25302,27747,29245,20245,27727,23605,12227,12274,13794)),]
      mean(errors_Mean$mseMean)
      sd(errors_Mean$mseMean)
    #MA model  
      successMA$seMA = (successMA$maEst-successMA$raw)^2
      errors_MA = data.frame(companyName = aggregate(successMA$seMA, list(gp=successMA$companyName), sum)$gp,
        mseMA = aggregate(successMA$seMA, list(gp=successMA$companyName), mean)$x)
      mean(errors_MA$mseMA)
      sd(errors_MA$mseMA)
      errors_MA = errors_MA[!(errors_MA$companyName %in% c(1585,24922,11768,24473,25302,27747,29245,20245,27727,23605,12227,12274,13794)),]
      mean(errors_MA$mseMA)
      sd(errors_MA$mseMA)
    #idiosyncratic model  
      successIS$seIS = (successIS$isEst-successIS$raw)^2
      errors_IS = data.frame(companyName = aggregate(successIS$seIS, list(gp=successIS$companyName), sum)$gp,
        mseIS = aggregate(successIS$seIS, list(gp=successIS$companyName), mean)$x)
      mean(errors_IS$mseIS)
      sd(errors_IS$mseIS)
      errors_IS = errors_IS[!(errors_IS$companyName %in% c(1585,24922,11768,24473,25302,27747,29245,20245,27727,23605,12227,12274,13794)),]
      mean(errors_IS$mseIS)
      sd(errors_IS$mseIS)
  #test differences between AR and state space results
    #t-test difference of means
      t.test(pointListUse$arEst, pointListUse$ssEst, paired = TRUE)
    #regress
      diffTest = lm (pointListUse$arEst~pointListUse$ssEst)
      diffTest = lm (pointListUse$ssEst~pointListUse$arEst)
    #plot
      plot(pointListUse$ssEst, pointListUse$arEst)
      noOuts= pointListUse[pointListUse$arEst < 10,]
      noOuts= noOuts[noOuts$ssEst < 10,]
      plot(noOuts$ssEst, noOuts$arEst)
      dim(noOuts[noOuts$arEst==0,])
  #check prediction of number of patents
    ar.mod = lm(numPatents~arEst, data = successAR)    
    ss.mod = lm(numPatents~ssEst, data = successSS)
    mean.mod = lm(numPatents~meanEst, data = successMean)
    ma.mod = lm(numPatents~maEst, data = successMA)
    is.mod = lm(numPatents~isEst, data = successIS)
    cor(successAR$arEst, successAR$numPatents)
    cor(successSS$ssEst, successSS$numPatents)
    cor(successMean$meanEst, successMean$numPatents)
    cor(successMA$maEst, successMA$numPatents)
    cor(successIS$isEst, successIS$numPatents)