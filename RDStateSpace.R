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
    RDONLY = data.frame(gvkey = RDDATA2$gvkey, xrdAdj = RDDATA2$xrdAdj, datadate = RDDATA2$datadate, xrdAdjNormalized = RDDATA2$xrdAdjNormalized, xrd = RDDATA2$xrd, IndAvg = RDDATA2$IndAvg, xrdAdjbyInd = RDDATA2$xrdAdjbyInd, sic = RDDATA2$sic, npatgrant = RDDATA2$npatgrant, npatapp = RDDATA2$npatapp, datayear = RDDATA2$datayear, npatappAdj = RDDATA2$npatappAdj, sale = RDDATA2$sale)
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

  #create test file for one industry
    #industry is 100, but can be changed
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

    #fix input data
        numCos = ncol(inputData)-1
        model.data = inputData[-c(1)] #delete time entry
        model.data2 =inputData2[-c(1)]
        model.data = as.matrix(model.data)
        model.data2 = as.matrix(model.data2)
        model.data = t(model.data)
        model.data2 = t(model.data2)
        model.data2 = rbind(model.data, model.data2)

    #save workspace
      save.image(file = "cleandata.RData")

  #create input file for each industry prgramatically
    oneVarList = list()
    twoVarList = list()
    numCosList = list()
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
    }
  #model.data is the single variable file
  #model.data2 is the multivariable file
  #covariates cannot have missing datas
    
#single-factor dfa models for industry index: r&d signal==========================================================

  #select industry
    industryIndex = 1
    oneVarInput = oneVarList[[industryIndex]]
    twoVarInput = twoVarList[[industryIndex]]
    numCos = numCosList[[industryIndex]]
    industryData = dataList[[industryIndex]]

  #set model inputs
    #set model inputs that are certain
      BAll = "identity"
      QAll= "diagonal and unequal" #note this could be changed if it causes problems since the unequal portion is irrelevant (it is a 1 by 1 matrix)
      source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZCol.R")
      ZAll = getZCol(numCos, "equal")
    #set model inputs, alternative 1
      U1 = "zero" #TBD
      A1 = "zero" #TBD
      R1 = "diagonal and equal"
      list.0.0.de = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1) #notation is model.list.u.a.r
    #set model inputs, alternative 2
      U2 = "unconstrained"
      A2 = "zero" #TBD
      R2 = "diagonal and equal"
      list.u.0.de = list(B=B1, U =U2, Q=Q1, Z=Z1, A=A2, R=R2) #notation is model.list.u.a.r
    #set model inputs, alternative 3
      U3 = "unconstrained"
      source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getACol.R")
      A3 = getACol(numCos)
      R3 = "diagonal and equal"
      list.u.a.de = list(B=B1, U =U3, Q=Q1, Z=Z1, A=A3, R=R3) #notation is model.list.u.a.r
      
  #set model controls, if necessary
    #control.list = list(safe = TRUE, trace =1, allow.degen= TRUE, maxit = 1000)

  #set up possible levels
    source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getACol.R")
    source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
    twoValR = getR(numCos)
    levels.R = list() #note: diagonal and unequal doesn't run
    levels.R[[1]]= "diagonal and equal"
    levels.R[[2]]= "unconstrained"
    levels.R[[3]]= "equalvarcov"
    levels.R[[4]]= twoValR
    twoValA = getACol(numCos)
    levels.A = list()
    levels.A[[1]]= "zero"
    levels.A[[2]] = twoValA
    levels.U = c("zero", "equal")
  
  #run models
    output.data = data.frame(matrix(ncol = 6, nrow = 0))
    colnames(output.data)= c("R", "A", "U", "logLik", "numParams", "AICc")
    for (i in 1:length(levels.R)){
      for (j in 1:length(levels.A)){
        for (U in levels.U){
          model.list = list(B=BAll, U=U, Q=QAll, Z=ZAll, A=levels.A[[j]], R=levels.R[[i]])
          model.current = MARSS(twoVarInput, model = model.list, miss.value =NA)
          if (is.null(model.current$num.params)){
            numParams = NA
            AICc = NA
          }
          else{
            numParams = model.current$num.params
            AICc = model.current$AICc
          }
          output.data= rbind(output.data, data.frame(R = levels.R[[i]][1], A = levels.A[[j]][1], U = U, logLik = model.current$logLik, numParams = numParams, AICc = AICc, stringsAsFactors = FALSE))
          assign(paste("model.", levels.R[[i]][1], levels.A[[j]][1], U, sep = "."), model.current)   
        }
      }
    }
    model.0.0.de = MARSS(twoVarInput, model = list.0.0.de, miss.value =NA)
    model.u.0.de = MARSS(twoVarInput, model = list.u.0.de, miss.value =NA)
    model.u.a.de = MARSS(twoVarInput, model = list.u.a.de, miss.value =NA)
  
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
R1 = getR(numCos)

  #get outputs 
    model.list = list(model.0.0.de, model.u.0.de, model.u.a.de, model.u.a.evc)
    stateList = list()
    SEList = list()
    AICList = data.frame(matrix(nrow = length(model.list), ncol = 1))
    CIList = list()
    colnames(AICList) = c("AIC")
    for (i in 1:length(model.list)){
      AICList[i,1] = model.list[[i]]$AIC
      #curAICc = model.list[[i]]$AICc
      stateList[[i]] = model.list[[i]]$states
      SEList[[i]] = model.list[[i]]$states.se #standard errors on the states
      CIList[[i]] = MARSSparamCIs(model.list[[i]]) #, method = "parametric") fails to run
      #curResiduals = residuals(model.list[[i]])
    }



  



#plot
  spp = rownames(twoVarInput)
  par(mfcol=c(3,3), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
  #for(i in spp){
  for (i in 10:nrow(twoVarInput)){
    plot(twoVarInput[i,],xlab="",ylab="Scaled signal")
    #, bty="L", xaxt="n", pch=16, col="blue", type="b")
    #axis(1,12*(0:dim(twoVarInput)[2])+1,1980+0:dim(twoVarInput)[2])
    title(rownames(twoVarInput)[i])
  }

  plotData =data.frame(t(newB.model$states))
  seData = t(newB.model$states.se[,1])
  origData = data.frame(t(twoVarInput))
  nameVect = c(1:ncol(origData))
  nameVect = paste("orig", nameVect, sep = "")
  colnames(origData) = nameVect
  plotData$time = c(1:nrow(plotData))
  plotData = cbind(plotData, origData)
  plotData$lb = plotData$state1 - seData[1] 
  plotData$ub = plotData$state1 + seData[1] 
  ggplot(data=plotData, aes(x=time, y=state1)) + geom_line() + geom_point(aes(x = time, y = orig1)) + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw() + geom_point(aes(x = time, y = orig2))+ geom_point(aes(x = time, y = orig3))+ geom_point(aes(x = time, y = orig4))+ geom_point(aes(x = time, y = orig5))+ geom_point(aes(x = time, y = orig6))+ geom_point(aes(x = time, y = orig7))+ geom_point(aes(x = time, y = orig8))+ geom_point(aes(x = time, y = orig9))




#plot R&D versus sales to check normalization====================================================================
  plotList= list()
  for (i in 1:length(dataList)){
    curData = dataList[[i]]
    curData$gvkey = as.factor(curData$gvkey)
    myPlot = ggplot(curData, aes(sale, xrd, group = gvkey, colour= gvkey)) + geom_point(shape = 2) + theme_bw() + labs(title=indList[i]) #+ coord_equal(ratio = 8) + scale_color_brewer(palette = "Set1") 
    plotList[[length(plotList)+1]]= myPlot
  }
  argsList <- c(plotList,3,2)
  namesList = c(1:length(plotList))
  namesList = as.character(namesList)
  names(argsList) <- c(namesList, "nrow", "ncol")
  pdf("C:/Users/Katharina/Documents/Umich/rdspend/linearityCheckPlot.pdf")
  do.call(marrangeGrob, argsList)
  dev.off()

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
    namesList = c(1:length(plotList))
    namesList = as.character(namesList)
    #namesList <- c(letters, paste("a", letters, sep = ""))
    names(argsList) <- c(namesList, "nrow", "ncol")
    pdf("C:/Users/Katharina/Documents/Umich/rdspend/plotBy.pdf")
    do.call(marrangeGrob, argsList)
    dev.off()
    #ggsave("C:/Users/Katharina/Documents/Umich/rdspend/plotByCo.pdf", do.call(marrangeGrob, argsList)) #this makes ugly plots
    write.csv(outVector, "C:/Users/Katharina/Documents/Umich/rdspend/manyentriesPvalsbyInd.csv")

#unstable state space trying to get all companies========================================================================================
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

#run MARSS with reasonable specification and both inputs--modified R with two variances, identity Z1
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

#run MARSS with reasonable specification and both inputs--modified R with two variances, diagonalZ Z1
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

#run MARSS with reasonable specification and both inputs--specialQ structure
#define inputs that change
#Z is the stacked identity structure to account for two inputs; we allow upward or downward shift through a so can have identity here
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZ.R")
Z1 = getZ(numCos, "diagonal")
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
R1 = getR(numCos)
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getQ.R")
Q1 = getQ(numCos) #this does not work because we can only have linear models
#set up model
model.list = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1)
control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)
#run model  
twoObsDiagZNewRnewQ.model = MARSS(model.data2, model = model.list, miss.value =NA, contol = control.list) #lowest AIC

#unstable state space with B that allows for interactions-de facto industry index=================================================================
indIndex = 1
numCos =numCosList[[indIndex]]
oneVarInput = oneVarList[[indIndex]]
oneVarInput = oneVarInput[,8:19]
twoVarInput = twoVarList[[indIndex]]
twoVarInput = twoVarInput[,8:19]
write.csv(twoVarInput, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test.csv")
fewMissInput = read.csv("C:/Users/Katharina/Documents/Umich/RDSpend/test2.csv")
fewMissInput = fewMissInput[,2:ncol(fewMissInput)]
numCos = nrow(fewMissInput)/2
fewMissInput = as.matrix(fewMissInput)
#define inputs
#state equation
#B allows all companies' true signals to affect all others'
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getB.R")
B1 = getB(numCos, "equal")
#U must be zero if we are to estimate B
U1 = "zero"
#U1 = "unconstrained"
#Q is diagonal, equal or unequal
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getQ2.R")
Q1 = getQ2(numCos, "unequal")
#observation equation
#Z is diagnoal, since each observation comes from on thing; allow Z to be diag and equal at top, then 1
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZ2.R")
Z1 = getZ2(numCos)
#a is zero--signals are centered around the true value, TBD
A1 = "zero"
#R allows each company to have its own error in signals--each company has different bias in reporting, no error in our covar signal, TBD maybe unequal
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR2.R")
R1 = getR2(numCos)
#source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
#R1 = getR(numCos)
#initial values
#initial values will be default, meaning that we assume that initial states are an estimated parameter with zero variance
#model list
#x0=fewMissInput[,1,drop=FALSE]
model.list = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1)
model.list$tinitx=1
#model.list$x0=x0
#control.list = list(allow.degen = TRUE, trace =1)
control.list = list(safe = TRUE, trace =1, allow.degen= TRUE) #, maxit = 1000
#run model
newB.model = MARSS(fewMissInput, model = model.list, miss.value =NA, contol = control.list) #runs but does not converge
#plot
plotData =data.frame(t(newB.model$states))
seData = t(newB.model$states.se[,1])
origData = data.frame(t(twoVarInput))
nameVect = c(1:ncol(origData))
nameVect = paste("orig", nameVect, sep = "")
colnames(origData) = nameVect
plotData$time = c(1:nrow(plotData))
plotData = cbind(plotData, origData)
plotData$lb = plotData$state1 - seData[1] 
plotData$ub = plotData$state1 + seData[1] 
ggplot(data=plotData, aes(x=time, y=state1)) + geom_line() + geom_point(aes(x = time, y = orig1)) + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw()

#unstable state space with industry index in addition to individual companies============================================================================
#define inputs
#state equation
#B is diagonal and 1 except for the industry index and covariates
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getB.R")
B1 = getB(numCos, "equal")
#U must be zero if we are to estimate B
U1 = "zero"
#U1 = "unconstrained"
#Q is diagonal, with zeros on the observation diagonals, equal or unequal
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getQ2.R")
Q1 = getQ2(numCos, "unequal")
#observation equation
#Z is diagnoal, since each observation comes from on thing; allow Z to be diag and equal at top, then 1
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZ2.R")
Z1 = getZ2(numCos)
#a is zero--signals are centered around the true value, TBD
A1 = "zero"
#R allows each company to have its own error in signals--each company has different bias in reporting, no error in our covar signal, TBD maybe unequal
source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR2.R")
R1 = getR2(numCos)
#source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
#R1 = getR(numCos)
#initial values
#initial values will be default, meaning that we assume that initial states are an estimated parameter with zero variance
#model list
x0=twoVarInput[,1,drop=FALSE]
model.list = list(B=B1, U =U1, Q=Q1, Z=Z1, A=A1, R=R1)
model.list$tinitx=1
model.list$x0=x0
#control.list = list(allow.degen = TRUE, trace =1)
control.list = list(safe = TRUE, trace =1, allow.degen= TRUE) #, maxit = 1000
#run model
newB.model = MARSS(twoVarInput, model = model.list, miss.value =NA, contol = control.list) #runs but does not converge
#plot
plotData =data.frame(t(newB.model$states))
seData = t(newB.model$states.se[,1])
origData = data.frame(t(twoVarInput))
nameVect = c(1:ncol(origData))
nameVect = paste("orig", nameVect, sep = "")
colnames(origData) = nameVect
plotData$time = c(1:nrow(plotData))
plotData = cbind(plotData, origData)
plotData$lb = plotData$state1 - seData[1] 
plotData$ub = plotData$state1 + seData[1] 
ggplot(data=plotData, aes(x=time, y=state1)) + geom_line() + geom_point(aes(x = time, y = orig1)) + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw()
