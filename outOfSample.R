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

#clear workspace ==============================================================
  rm(list = ls())

#data i/o=======================================================================
  RDDATA = read.csv("C:/Users/Katharina/Documents/Umich/rdspend/RDDATA.csv") 
  #note: input file has been slightly modified by STATA and has date year and industry/year ID extracted
  CONCATID = read.csv("C:/Users/Katharina/Documents/Umich/rdspend/concat.csv")
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
    write.csv(INDTABLE,"C:/Users/Katharina/Documents/Umich/rdspend/rawindex.csv")

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
  #reduce to usable variables
    redVarList = list()
    for (k in 1:length(oneVarList)){
      current=oneVarList[[k]]
      currenthold = current
      currenthold[currenthold == 0]= NA
      counters=apply(currenthold,1,function(x) sum(!is.na(x)))
      #redVarList[[k]]=current[counters >4,] #remove companies that don't have at least 4 non-zero, non-NA entries, CHANGE HERE
      redVarList[[k]]= oneVarList[[k]]
      #print(dim(redVarList[[k]])[1]-dim(current)[1])
    } 

#run model=======================================================================================
  output.data = data.frame(matrix(ncol = 7, nrow = 0))
  colnames(output.data)= c("industryName", "logLik", "numParams", "AICc", "States", "SEs", "converge")
  output.outofsample = data.frame(matrix(ncol = 4, nrow = 0))
  colnames(output.outofsample) = c("industryName", "datayear", "state", "se")
  for (k in 1:length(redVarList)){
    oneVarInput = redVarList[[k]]
    numCos = nrow(oneVarInput)
    numYears = 1
    industryName = nameVector[k] 
    if (!(is.null(numCos))){ #meaning we have more than 1 company
      numYears = ncol(oneVarInput)
      if (numCos >1){
        #set model inputs
          BAll = "identity"
          QAll= "diagonal and unequal" #note this could be changed if it causes problems since the unequal portion is irrelevant (it is a 1 by 1 matrix)
          ZIN= rep(1, numCos)
          ZIN = as.list(ZIN)
          ZAll = matrix(ZIN, numCos, 1)
          RAll = "diagonal and equal"
          AAll = "equal"
          UAll = "zero"
        #set model controls, if necessary
          control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)#, maxit = 1000)
        #run models
          stringList = c()
          model.list = list(B=BAll, U=UAll, Q=QAll, Z=ZAll, A=AAll, R=RAll)
          model.current = MARSS(oneVarInput, model = model.list, miss.value =NA, control = control.list)
          if (is.null(model.current$num.params)){
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
            #plot
              plotData = data.frame(t(model.current$states))
              seData = t(model.current$states.se[,1])
              origData = data.frame(t(oneVarInput))
              plotData$time = c(1:nrow(plotData))
              plotData$lb = plotData$state1 - 1.96 *seData[1] 
              plotData$ub = plotData$state1 + 1.96 *seData[1] 
              myPlot = ggplot(data=plotData, aes(x=time, y=state1)) + geom_line()  + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw() + labs(title=paste(industryName, sep = ","))
              j = 24
              for (i in 1:ncol(origData)){
                curInput = paste("curCo", i, sep = "")
                addString = paste("geom_point(aes(x = time, y = origData[,", i, "]), colour = ",j,")", sep = "")
                myPlot=  myPlot+ eval(parse(text = addString))
                j = j+5
              }
              ggsave(paste("C:/Users/Katharina/Documents/Umich/rdspend/firststage/firststageconfig24", industryName, ".pdf", sep = ""))
            }
          if (is.null(model.current$logLik)){
            logLik = NA
          }else{
            logLik = model.current$logLik
          }
        } else{
          numParams = NA
          AICc = NA
          curStates = NA
          curSE = NA
          logLik = NA
          model.current = NA
          curConv = NA
          stateVect = rep(NA,numYears)
          seVect = rep(NA, numYears)
        }
      } else{
        numParams = NA
        AICc = NA
        curStates = NA
        curSE = NA
        logLik = NA
        model.current = NA
        curConv = NA
        stateVect = rep(NA,numYears)
        seVect = rep(NA, numYears)
      }
      cur.outdata =data.frame(industry= industryName, logLik = logLik, numParams = numParams, AICc = AICc, states = curStates, ses = curSE, converge =curConv, stringsAsFactors = FALSE)
      colnames(cur.outdata)= colnames(output.data)
      output.data= rbind(output.data, cur.outdata)
      cur.outofsample = data.frame(matrix(ncol = 4, nrow = numYears))
      colnames(cur.outofsample) = colnames(output.outofsample)
      cur.outofsample$industryName = industryName
      cur.outofsample$datayear = c(startYearList[[i]]:(startYearList[[i]]+numYears-1))
      cur.outofsample$state = stateVect
      cur.outofsample$se = seVect
      output.outofsample = rbind(output.outofsample, cur.outofsample)         
      modelString = paste("model", industryName, sep = ".")
      stringList = c(stringList, modelString)
      assign(paste("modelTwostepSSconfig24", industryName, sep = "."), model.current)
    }
  save.image(file = "indIndexesconfig24.RData")
  write.csv(output.data, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test2.csv")
  write.csv(output.outofsample, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test3.csv")
  config.2.4.out = output.outofsample
  save(config.2.4.out, file = "config.2.4.out.RData")

#company level models=================================================================
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

  #set model controls, if necessary
    control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)#, maxit = 1000)
  #run models
  stringList = c()
    for (i in 1:length(oneVarList)){ #industry loop 
      curData = oneVarList[[i]]
      industryName = nameVector[i] 
      companyNameVector = rownames(curData)
      for (j in 1:(nrow(curData))){#for each company, run model
        #set up inputdata
          companyName = companyNameVector[j]
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
          modelString = paste("model", industryName,companyName, sep = ".")
          stringList = c(stringList, modelString)
          assign(paste("coModel", industryName, sep = "."), model.current)
      }
    }
  save.image(file = "coModels.RData")
  write.csv(output.data, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test2.csv")
  write.csv(output.outofsample, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test3.csv")
  all.out = output.outofsample
  save(all.out, file = "allCoOut.RData") #i = 330, j = 96/141

#company level diagnostics=================================================================
output.data = data.frame(matrix(ncol = 6, nrow = 0))
colnames(output.data)= c("industryName", "companyName", "numTot", "numZero", "numNA", "yearsRun")

for (i in 1:length(oneVarList)){ #industry loop 
  curData = oneVarList[[i]]
  industryName = nameVector[i] 
  companyNameVector = rownames(curData)
  for (j in 1:(nrow(curData))){#for each company, run model
    #set up inputdata
    companyName = companyNameVector[j]
    coData = curData[j,]
    numTot = length(coData)
    numNA = length(coData[is.na(coData)])
    noNA = na.exclude(coData)
    numZero = length(noNA[noNA==0])
    numYears = 0
    if (length(coData[is.na(coData)])<length(coData)){
      nonNA1 = which(!is.na(coData)) #coData now a vector
      startIndex1 = min(nonNA1)
      endIndex1 = max(nonNA1)
      coData = coData[startIndex1:endIndex1]
      numYears = length(coData)
    }
      #store
    cur.outdata =data.frame(industry= industryName,company= companyName, numTot = numTot, numZero = numZero, numNA = numNA, numYears = numYears, stringsAsFactors = FALSE)
    colnames(cur.outdata)= colnames(output.data)
    output.data= rbind(output.data, cur.outdata)
  }
}
write.csv(output.data, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test2.csv")