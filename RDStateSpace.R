#NOTES:======================================================================

#libraries and functions====================================================================
  library(plyr)
  library(ggplot2)
  library(descr)
  library(tseries)
  library(gridExtra)
  library(reshape2)
  library(MARSS)

#clear workspace ==============================================================
  rm(list = ls())

#data i/o=======================================================================
  RDDATA = read.csv("C:/Users/Katharina/Documents/Umich/rdspend/RDDATA.csv") 
  #note: input file has been slightly modified by STATA and has date year and industry/year ID extracted
  CONCATID = read.csv("C:/Users/Katharina/Documents/Umich/rdspend/concat.csv")
  RDDATA$iyID = CONCATID$iyID
  RDDATA$datadate = as.Date(RDDATA$datadate, "%d%b%Y")
  RDDATA$datayear = as.numeric(format(RDDATA$datadate, format = "%Y"))

#pull industry averages and add to RDDATA, get industry average adjusted xrd metric
  #indCodes = levels(as.factor(RDDATA$sic))
  #FREQTABLE = freq(ordered(RDDATA$sic), plot=FALSE) #many industries have over 100 entries
  INDAVGTABLE = RDDATA[,c("iyID", "xrdAdj")]
  INDAVGTABLE = na.exclude(INDAVGTABLE)
  INDAVGTABLE = INDAVGTABLE[INDAVGTABLE$xrdAdj >0,]
  INDAVGTABLE = melt(INDAVGTABLE, id = "iyID")
  AvgRD = dcast(INDAVGTABLE, iyID~variable, mean)
  #AvgRD = AvgRD[,c("iyID", "X")]
  colnames(AvgRD)= c("iyID", "IndAvg")
  RDDATA2<- merge(x = RDDATA, y = AvgRD, by = "iyID", all.x = TRUE) #some will have NA for iyID?
  RDDATA2$xrdAdjbyInd = RDDATA2$xrdAdj-RDDATA2$IndAvg
  RDDATA2$npatappAdj = RDDATA2$npatapp/RDDATA2$sale
  #AVGTABLE = ddply(RDDATA2,~gvkey,summarise,meanNpa=mean(npatappAdj),sdNpa=sd(npatappAdj), meanXrdAdj = mean(xrdAdj), sdXrdAdj = sd(xrdAdj))
  #RDDATA2 = merge(x = RDDATA2, y = AVGTABLE, by = "gvkey", all.x = TRUE)
  #RDDATA2$npatappZ = (RDDATA2$npatappAdj-meanNpa)/sdNpa
  #RDDATA2$xrdZ= RDDATA2$xrdAdj- meanXrdAdj/sdXrdAdj

#clean data=======================================================================
  #get clean dataset--only data with at least 8 entries
    RDONLY = data.frame(gvkey = RDDATA2$gvkey, xrdAdj = RDDATA2$xrdAdj, datadate = RDDATA2$datadate, xrdAdjNormalized = RDDATA2$xrdAdjNormalized, xrd = RDDATA2$xrd, IndAvg = RDDATA2$IndAvg, xrdAdjbyInd = RDDATA2$xrdAdjbyInd, sic = RDDATA2$sic, npatgrant = RDDATA2$npatgrant, npatapp = RDDATA2$npatapp, datayear = RDDATA2$datayear, npatappAdj = RDDATA2$npatappAdj)
    RDONLY = RDONLY[order(RDONLY$gvkey),]
    NOMISS = na.exclude(RDONLY)
    FREQTABLE = freq(ordered(NOMISS$gvkey), plot=FALSE)
    FREQTABLE = data.frame(gvkey = as.factor(rownames(FREQTABLE)), freq = FREQTABLE[,1])
    RDONLYTEST = merge(x = RDONLY, y = FREQTABLE, by = "gvkey", all.x = TRUE)
    RDONLYTEST[is.na(RDONLYTEST$freq),]$freq = -3
    RDONLYTEST = RDONLYTEST[RDONLYTEST$freq >= 8,]
    write.csv(RDONLYTEST, "C:/Users/Katharina/Documents/Umich/rdspend/RDDATA-EIGHT.csv")
    write.csv(FREQTABLE, "C:/Users/Katharina/Documents/Umich/rdspend/freqtable.csv")
  
  #create output file by industry and save if industry has more than 50 entries
    indList = levels(factor(RDONLYTEST$sic))
    dataList = list()
    nameVector = c()
    for (i in 1:length(indList)){
      curData = RDONLYTEST[RDONLYTEST$sic ==indList[i],]
      if (nrow(curData) > 50){
        dataList[[length(dataList)+1]]= curData
        nameVector[[length(nameVector)+1]]= indList[i]
        outFile = paste("C:/Users/Katharina/Documents/Umich/rdspend/industryfiles/", indList[i], ".csv", sep = "")
        write.csv(curData, outFile)
      }
    }
    #create 3-company test file
      #industry is 100
      industryIndex = which(nameVector == 100)
      industryData = dataList[[industryIndex]]
      #industryData = industryData[industryData$freq==12,]
      
      #put this in MARSS form
        inputData = ddply(industryData, ~datayear, 
          function(df) {
            res = data.frame(rbind(df$xrdAdj)) #take R&D spending as a percentage of sales
            names(res) = sprintf("%s",df$gvkey)
            res
          }
        )
        inputData2 = ddply(industryData, ~datayear, 
                  function(df) {
                    res = data.frame(rbind(df$npatappAdj))
                    names(res) = sprintf("%s",df$gvkey)
                    res
                  }
        )

  #save workspace
    save.image(file = "cleandata.RData")
  
#state space========================================================================================
  #fix input data
    numCos = ncol(inputData)-1
    model.data = inputData[-c(1)] #delete time entry
    model.data2 =inputData2[-c(1)]
    model.data = as.matrix(model.data)
    model.data2 = as.matrix(model.data2)
    model.data = t(model.data)
    model.data2 = t(model.data2)
    model.data2 = rbind(model.data, model.data2)
  
  #run MARSS with reasonable specification and single input
    #define inputs
      #state equation
        #B is identity, since we assume autoregressive nature
          B1 = "identity"
        #U is zero, since we assume no trend
          U1 = "zero"
        #Q is constrained to allow a constant variance for each company in the same industry, and a set covariance between companies
          Q1 = "equalvarcov"
      #observation equation
        #Z adjusts for the bias in the signal, which is the same across industry
          Z1 = "diagonal and equal"
        #a is zero--signals are centered around the true value
          A1 = "zero"
        #R allows each company to have its own error in signals--each company has different bias in reporting
          R1 = "diagonal and equal"
      #initial values
        #initial values will be default, meaning that we assume that initial states are an estimated parameter with zero variance
    #model list
      model.list = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1)
      #control.list = list(allow.degen = TRUE, trace =1)
      control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)
    #run model  
      singleObs.model = MARSS(model.data, model = model.list, miss.value =NA, contol = control.list) #runs but does not converge
      singleObs.model=MARSSparamCIs(singleObs.model)
      plotData =data.frame(t(singleObs.model$states))
      seData = t(singleObs.model$states.se[,1])
      origData = data.frame(t(model.data))
      nameVect = c(1:ncol(origData))
      nameVect = paste("orig", nameVect, sep = "")
      colnames(origData) = nameVect
      plotData$time = c(1:nrow(plotData))
      plotData = cbind(plotData, origData)
      plotData$lb = plotData$state1 - seData[1] 
      plotData$ub = plotData$state1 + seData[1] 
      ggplot(data=plotData, aes(x=time, y=state1)) + geom_line() + geom_point(aes(x = time, y = orig1)) + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw()

    #run MARSS with reasonable specification and both inputs--nochange in R, identity Z1
      #define inputs that change
        #Z is the stacked identity structure to account for two inputs; we allow upward or downward shift through a so can have identity here
          source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZ.R")
          Z1 = getZ(numCos, "identity")
      #set up model
        model.list = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1)
        control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)
      #run model  
        twoObsIDZ.model = MARSS(model.data2, model = model.list, miss.value =NA, contol = control.list) #runs but does not converge

    #run MARSS with reasonable specification and both inputs--nochange in R, diagonatl Z1
      #define inputs that change
        #Z is the stacked identity structure to account for two inputs; we allow upward or downward shift through a so can have identity here
          source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZ.R")
          Z1 = getZ(numCos, "diagonal")
      #set up model
      model.list = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1)
      control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)
      #run model  
      twoObsDiagZ.model = MARSS(model.data2, model = model.list, miss.value =NA, contol = control.list) #runs but does not converge


#run MARSS with reasonable specification and both inputs--nochange in R, identity Z1
  #define inputs that change
    #Z is the stacked identity structure to account for two inputs; we allow upward or downward shift through a so can have identity here
    source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZ.R")
    Z1 = getZ(numCos, "identity")
    #change R so that you can have different errors for different types of signals
    source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
    R1 = getR(numCos)
  #set up model
    model.list = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1)
   control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)
  #run model  
    twoObsIDZNewR.model = MARSS(model.data2, model = model.list, miss.value =NA, contol = control.list) #runs but does not converge

#run MARSS with reasonable specification and both inputs--nochange in R, diagonatl Z1
  #define inputs that change
    #Z is the stacked identity structure to account for two inputs; we allow upward or downward shift through a so can have identity here
      source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZ.R")
      Z1 = getZ(numCos, "diagonal")
      source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
      R1 = getR(numCos)
  #set up model
    model.list = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1)
    control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)
  #run model  
    twoObsDiagZNewR.model = MARSS(model.data2, model = model.list, miss.value =NA, contol = control.list) #lowest AIC
    plotData =data.frame(t(twoObsDiagZNewR.model$states))
    seData = t(twoObsDiagZNewR.model$states.se[,1])
    origData = data.frame(t(model.data))
    nameVect = c(1:ncol(origData))
    nameVect = paste("orig", nameVect, sep = "")
    colnames(origData) = nameVect
    plotData$time = c(1:nrow(plotData))
    plotData = cbind(plotData, origData)
    plotData$lb = plotData$state1 - seData[1] 
    plotData$ub = plotData$state1 + seData[1] 
    ggplot(data=plotData, aes(x=time, y=state1)) + geom_line() + geom_point(aes(x = time, y = orig1)) + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw()

#stationarity testing===================================================================================
  #identify and plot companies with at least 8 non-missing data points
    RDONLY = data.frame(gvkey = RDDATA$gvkey, xrdAdj = RDDATA$xrdAdj, datadate = RDDATA$datadate, xrdAdjNormalized = RDDATA$xrdAdjNormalized, xrd = RDDATA$xrd, IndAvg = RDDATA$xrdIndAvg, xrdAdjbyInd = RDDATA$xrdAdjbyInd )
    RDONLY_NOMISS = na.exclude(RDONLY)
    RDONLY_NOMISS = RDONLY_NOMISS[RDONLY_NOMISS$xrdAdj>0,] #treat zero as missing
    FREQTABLE = freq(ordered(RDONLY_NOMISS$gvkey), plot=FALSE)
    FREQTABLE = data.frame(gvkey = as.factor(rownames(FREQTABLE)), freq = FREQTABLE[,1])
    RDONLY_NOMISS = merge(x = RDONLY_NOMISS, y = FREQTABLE, by = "gvkey", all.x = TRUE)
    RDONLY_NOMISS = RDONLY_NOMISS[RDONLY_NOMISS$freq >= 8,]
    RDONLY_NOMISS$gvkey = as.factor(RDONLY_NOMISS$gvkey)
    compList = levels(RDONLY_NOMISS$gvkey)
    #plot normalized rdspend for all cmpanies with at least 8 non-missing
      ggplot(RDONLY_NOMISS, aes(x=datadate, y=xrdAdjNormalized)) +geom_point(shape=1)+ labs(title = expression("Normalized adjusted spending over all companies"))
    #write to file
      write.csv(RDONLY_NOMISS, "C:/Users/Katharina/Documents/Umich/rdspend/RDONLY_NOMISS.csv")
  
  #plot stationarity for companies with most entries
    RDONLY_NOMISS<-RDONLY_NOMISS[order(RDONLY_NOMISS$freq),]
    MANY_ENTRIES = RDONLY_NOMISS[RDONLY_NOMISS$freq > 26,]
    numCos = length(unique(MANY_ENTRIES$gvkey)) #there are 284 companies with exactly 27 entries
    keyList = unique(MANY_ENTRIES$gvkey)[1:48]
    MANY_ENTRIES = MANY_ENTRIES[MANY_ENTRIES$gvkey %in% keyList,] #we select the first 48 (so they fit on my plots)
    plotList = list()
    outVector = data.frame(gvkey  = keyList, DFpValue = NA, KPSSpValue = NA)
    for (i in 1:length(keyList)){
      curData = RDONLY_NOMISS[RDONLY_NOMISS$gvkey == keyList[i],]
      plotList[[i]] = ggplot(curData, aes(x=datadate, y=xrdAdj)) +geom_point(shape=1)+ labs(title = paste("Adjusted spending, ",toString(keyList[i]), sep = "")) #+ theme(axis.title.x = element_text(size = 8), axis.text.x=element_text(size=8), axis.text.y=element_text(size=8))
      DFTest = adf.test(curData$xrdAdj, alternative = "stationary") #alt hypothesis is stationary, high p-value means fail to reject null->non-stationarity
      KPSSTest = kpss.test(curData$xrdAdj)
      outVector$DFpValue[i] = DFTest$p.value
      outVector$KPSSpValue[i]= KPSSTest$p.value #alt hypothesis is nonstationary, high p-value means fail to reject null -> stationarity
    }
    argsList <- c(plotList,3,2)
    namesList <- c(letters, paste("a", letters, sep = ""))
    names(argsList) <- c(namesList[1:48], "nrow", "ncol")
    pdf("C:/Users/Katharina/Documents/Umich/rdspend/plotByCo.pdf")
      do.call(marrangeGrob, argsList)
    dev.off()
    #ggsave("C:/Users/Katharina/Documents/Umich/rdspend/plotByCo.pdf", do.call(marrangeGrob, argsList)) #this makes ugly plots
    write.csv(outVector, "C:/Users/Katharina/Documents/Umich/rdspend/manyentriesPvals.csv")
  
  #run stationarity test for each of the companies
    #fill missing values with last reported, based on suggestion here: http://davegiles.blogspot.com/2012/04/unit-root-tests-with-missing.html
    outVector = data.frame(gvkey  = compList, DFpValue = NA, KPSSpValue = NA)
    for (i in 1:length(compList)){
      curData = RDONLY[RDONLY$gvkey == compList[i],]
      if (length(curData$xrdAdj)> length(na.exclude(curData$xrdAdj))){ #change missings to -3
        curData[is.na(curData$xrdAdj),]$xrdAdj = -3
      }
      firstIndex = 0
      changeIndex = 0 
      for (j in 1:nrow(curData)){
        if(firstIndex == 0 & curData$xrdAdj[j] >0){
          firstIndex = j
        }
      }
      if (firstIndex > 0){ #this means the company has at least some valid data
        for (j in firstIndex:nrow(curData)){
          if(curData$xrdAdj[j]<=0){ #currently treating zeros as missing, should ask jussi
            if (changeIndex == 0){
              changeIndex = j-1
            }
            else {
              if (changeIndex > 0){
                curData$xrdAdj[(changeIndex+1):(j-1)]= curData$xrdAdj[j]
                changeIndex = 0
              }
            }
          }
        }
      }
      curData<-curData[order(curData$datadate),]
      DFTest = adf.test(curData$xrdAdj, alternative = "stationary") #alt hypothesis is stationary, high p-value means fail to reject null->non-stationarity
      KPSSTest = kpss.test(curData$xrdAdj)
      outVector$DFpValue[i] = DFTest$p.value
      outVector$KPSSpValue[i]= KPSSTest$p.value #alt hypothesis is nonstationary, high p-value means fail to reject null -> stationarity
    }
    write.csv(outVector, "C:/Users/Katharina/Documents/Umich/rdspend/DFPvals.csv")
  
  #test stationarity and create plots for companies with most data with normalized industry 
    RDDATAIND = data.frame(gvkey = RDDATA$gvkey, xrdAdjbyInd = RDDATA$xrdAdjbyInd, datadate = RDDATA$datadate)
    RDDATAIND = na.exclude(RDDATAIND) #they all go away cause all have no years!
    RDDATAIND = RDDATAIND[RDDATAIND$xrdAdjbyInd >0,]
    FREQTABLE = freq(ordered(RDDATAIND$gvkey), plot=FALSE)
    FREQTABLE = data.frame(gvkey = as.factor(rownames(FREQTABLE)), freq = FREQTABLE[,1])
    RDDATAIND_NOMISS = merge(x = RDDATAIND, y = FREQTABLE, by = "gvkey", all.x = TRUE)
    MANY_ENTRIES_IND = RDDATAIND_NOMISS[RDDATAIND_NOMISS$freq > 20,]
    numCos = length(unique(MANY_ENTRIES_IND$gvkey)) #there are 284 companies with exactly 27 entries
    keyList = unique(MANY_ENTRIES_IND$gvkey)[1:48]
    MANY_ENTRIES_IND = MANY_ENTRIES_IND[MANY_ENTRIES_IND$gvkey %in% keyList,] #we select the first 48 (so they fit on my plots)
    plotList = list()
    outVector = data.frame(gvkey  = keyList, DFpValue = NA, KPSSpValue = NA)
    for (i in 1:length(keyList)){
      curData = RDDATAIND_NOMISS[RDDATAIND_NOMISS$gvkey == keyList[i],]
      plotList[[i]] = ggplot(curData, aes(x=datadate, y=xrdAdjbyInd)) +geom_point(shape=1)+ labs(title = paste("Adjusted spending, ",toString(keyList[i]), sep = "")) #+ theme(axis.title.x = element_text(size = 8), axis.text.x=element_text(size=8), axis.text.y=element_text(size=8))
      DFTest = adf.test(curData$xrdAdj, alternative = "stationary") #alt hypothesis is stationary, high p-value means fail to reject null->non-stationarity
      KPSSTest = kpss.test(curData$xrdAdj)
      outVector$DFpValue[i] = DFTest$p.value
      outVector$KPSSpValue[i]= KPSSTest$p.value #alt hypothesis is nonstationary, high p-value means fail to reject null -> stationarity
    }
    argsList <- c(plotList,3,2)
    namesList <- c(letters, paste("a", letters, sep = ""))
    names(argsList) <- c(namesList[1:48], "nrow", "ncol")
    pdf("C:/Users/Katharina/Documents/Umich/rdspend/plotByCoInd2.pdf")
    do.call(marrangeGrob, argsList)
    dev.off()
    #ggsave("C:/Users/Katharina/Documents/Umich/rdspend/plotByCo.pdf", do.call(marrangeGrob, argsList)) #this makes ugly plots
    write.csv(outVector, "C:/Users/Katharina/Documents/Umich/rdspend/manyentriesPvalsbyInd.csv")