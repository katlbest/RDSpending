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
    #write.csv(RDDATA2,"C:/Users/Katharina/Documents/Umich/rdspend/test.csv") 

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
  
#do variance decomposition==================================================
  outputData = data.frame()
  output.data = data.frame(matrix(ncol =5, nrow = 0))
  colnames(output.data)= c("industry", "numCompanies", "numCompaniesRecip", "SE", "SD")
  outputLm = data.frame()
  outputLm = data.frame(matrix(ncol =6, nrow = 0))
  colnames(outputLm)= c("industry", "Coeff_NumCos", "Coeff_Constant", "p_NumCos", "p_Constant", "R2")
  industryAvgVector = list()
  for (k in 1:length(oneVarList)){
    curData = oneVarList[[k]]
    industryName = nameVector[k] 
    cum.output = data.frame(matrix(ncol =5, nrow = 0))
    colnames(cum.output)= c("industry", "numCompanies", "numCompaniesRecip", "SE", "SD")
    industryAvgVector[[k]]=rep(NA, ncol(curData))
    for (i in 1:ncol(curData)){#for each year
      curYear = c(na.exclude(curData[,i]))
      curNumCo = length(curYear)
      if (curNumCo>1){ #we have a data point
        sd(curYear)
        sd(curYear)/sqrt(curNumCo)
        cur.outdata =data.frame(industry = industryName, numCompanies= curNumCo, numCompaniesRecip = 1/curNumCo, SE = sd(curYear)/sqrt(curNumCo), SD = sd(curYear), stringsAsFactors = FALSE)
        colnames(cur.outdata)= colnames(output.data)
        output.data= rbind(output.data, cur.outdata)
        colnames(cur.outdata)= colnames(cum.output)
        cum.output= rbind(cum.output, cur.outdata)
        industryAvgVector[[k]][i]= mean(curYear)
      }
    }
    cum.output$Var = (cum.output$SD)^2
    cum.output$errorCoeff = 1
    curVarDecomp = lm(Var~0+ numCompaniesRecip+errorCoeff, data = cum.output)
    curNumCoeff =  summary(curVarDecomp)$coefficients[1,1]
    curNumP =  summary(curVarDecomp)$coefficients[1,4]
    if (dim(summary(curVarDecomp)$coefficients)[1]>1){
      curConstCoeff =  summary(curVarDecomp)$coefficients[2,1]
      curConstP =  summary(curVarDecomp)$coefficients[2,4]
    } else{
      curConstCoeff =  NA
      curConstP =  NA
    }
    curR2 = summary(curVarDecomp)$r.square
    cur.lmdata =data.frame(industry = industryName, coeff1 = curNumCoeff, coeff2 = curConstCoeff, p1 = curNumP, p2 = curConstP, r2 = curR2, stringsAsFactors = FALSE)
    colnames(cur.lmdata)= colnames(outputLm)
    outputLm= rbind(outputLm, cur.lmdata)
  }
output.data$Var = (output.data$SD)^2
output.data$errorCoeff = 1
varDecomp = lm(Var~0+ numCompaniesRecip+errorCoeff, data = output.data)
write.csv(outputLm, 'C:/Users/Katharina/Documents/Umich/RDSpend/test2.csv')
save.image(file = "varDecomp.RData")

#print missingness of the number of patents variable==========================================
output.data = data.frame(matrix(ncol = 8, nrow = 0))
colnames(output.data)= c("industry", "company", "tot", "zero", "na",  "tot2", "zero2", "na2")
for (i in 1:length(oneVarList)){ #industry loop
  curData = twoVarList[[i]]
  industryName = nameVector[i] 
  companyNameVector = rownames(curData)[1:(nrow(curData)/2)]
  curAvg = industryAvgVector[[i]]
  for (j in 1:(nrow(curData)/2)){#for each company, run model
    companyName = companyNameVector[j]
    coData = rbind(curData[j,], curData[j+length(companyNameVector),])
    #remove all parts of covariate where our co. is missing
    nonNA1 = which(!is.na(coData[1,])) #first row
    startIndex1 = min(nonNA1)
    endIndex1 = max(nonNA1)
    nonNA2 = which(!is.na(coData[2,])) #second row
    startIndex2 = min(nonNA2)
    endIndex2 = max(nonNA2)
    curAvgTruncated = curAvg[max(startIndex1, startIndex2):min(endIndex1, endIndex2)]#removed all parts of covariate where our co. is missing
    coData = coData[,max(startIndex1, startIndex2):min(endIndex1, endIndex2)]
    #remove any entries where index is missing
    nonNA = which(!is.na(curAvgTruncated))
    startIndex = min(nonNA)
    endIndex = max(nonNA)
    curAvgTruncated = curAvgTruncated[startIndex:endIndex]#removed all parts of covariate where our co. is missing
    coData = coData[,startIndex:endIndex]
    cCo= t(matrix(curAvgTruncated))
    rownames(cCo)= c("IndAvg")
    patData = coData[2,]
    totEntry = length(patData)
    naEntry = length(patData[is.na(patData)==TRUE])
    zeroEntry = length(na.exclude(patData)[na.exclude(patData) ==0])
    rdData = coData[1,]
    totEntry2 = length(rdData)
    naEntry2 = length(rdData[is.na(rdData)==TRUE])
    zeroEntry2 = length(na.exclude(rdData)[na.exclude(rdData) ==0])
    cur.outdata =data.frame(industry = industryName, company = companyName, tot = totEntry,zero = zeroEntry, na = naEntry ,tot2 = totEntry2,zero2 = zeroEntry2, na2 = naEntry2,stringsAsFactors = FALSE)
    colnames(cur.outdata)= colnames(output.data)
    output.data= rbind(output.data, cur.outdata)
  } 
}

#including industry average as a covariate=============================================
  output.data = data.frame(matrix(ncol = 9, nrow = 0))
  colnames(output.data)= c("industry", "company", "logLik", "numParams", "AICc", "modtype", "states", "se", "converge")
  source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZColSmall.R")
  source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
  #set constant parameters
  BAll = matrix(list(1))
  QAll= "diagonal and equal" #1 by 1
  AAll = "unconstrained"
  UAll = "zero"
  #UAll = "unconstrained"
  ZAll = matrix(list(1, "z2"))
  RAll = getR(1)
  covarList = list()
  inputList = list()
  modelList = list()
  n=1
  missingCovarCount= 0
  control.list = list(safe = TRUE, trace =1, allow.degen= TRUE, maxit = 1000)
  for (i in 1:length(oneVarList)){ #industry loop
    curData = twoVarList[[i]]
    industryName = nameVector[i] 
    companyNameVector = rownames(curData)[1:(nrow(curData)/2)]
    curAvg = industryAvgVector[[i]]
    for (j in 1:(nrow(curData)/2)){#for each company, run model
      companyName = companyNameVector[j]
      coData = rbind(curData[j,], curData[j+length(companyNameVector),])
      #remove all parts of covariate where our co. is missing
        nonNA1 = which(!is.na(coData[1,])) #first row
        startIndex1 = min(nonNA1)
        endIndex1 = max(nonNA1)
        nonNA2 = which(!is.na(coData[2,])) #second row
        startIndex2 = min(nonNA2)
        endIndex2 = max(nonNA2)
        curAvgTruncated = curAvg[max(startIndex1, startIndex2):min(endIndex1, endIndex2)]#removed all parts of covariate where our co. is missing
        coData = coData[,max(startIndex1, startIndex2):min(endIndex1, endIndex2)]
      #remove any entries where index is missing
        nonNA = which(!is.na(curAvgTruncated))
        startIndex = min(nonNA)
        endIndex = max(nonNA)
        curAvgTruncated = curAvgTruncated[startIndex:endIndex]#removed all parts of covariate where our co. is missing
        coData = coData[,startIndex:endIndex]
        coData[coData[2,]==0] = NA
        cCo= t(matrix(curAvgTruncated))
        naCCo = which(is.na(cCo))
        #find breaks in naCCo
        if (length(naCCo) >0){ #there are missing values
          missingCovarCount = missingCovarCount+1 #increase counter
          k=1
          while (k <= length(naCCo)){ #for each entry in my missing indicator vector
            curIndex = naCCo[k] #first missing index, original vector
            endIndex = NA #last missing index, original vector
            l = 0 #holds number of missing values beyond location k, plus 1
            while(is.na(endIndex) & curIndex+l <= length(cCo)){
              if (!is.na(cCo[curIndex+l])){
                endIndex = curIndex+l-1
              }
              l = l+1
            }
            outVect = seq(cCo[curIndex-1], cCo[endIndex+1], (cCo[endIndex+1]-cCo[curIndex-1])/((endIndex-curIndex)+2))
            cCo[curIndex:endIndex]= outVect[2:(length(outVect)-1)]
              #mean(cCo[endIndex+1], cCo[curIndex-1])
            k = k+l-1
          }
        }
        rownames(cCo)= c("IndAvg")
        covarList[[n]] = cCo
        inputList[[n]]= coData
        patData = coData[2,]
        patUsable = length(patData)-length(patData[is.na(patData)==TRUE])-length(na.exclude(patData)[na.exclude(patData) ==0])
        rdData = coData[1,]
        rdUsable = length(rdData)-length(rdData[is.na(rdData)==TRUE])-length(na.exclude(rdData)[na.exclude(rdData) ==0])
        if(rdUsable > 4 & patUsable > 4){
          modtype =1
          #run model
            model.list = list(B=BAll, U=UAll, Q=QAll, A=AAll, R=RAll,  Z=ZAll, c= cCo, C = "unconstrained")
            model.current = MARSS(coData, model = model.list, miss.value =NA, control = control.list)
          #store output
            if (is.null(model.current$num.params)){
              numParams = NA
              AICc = NA
              curStates =NA
              curSE = NA
              curConv = NA
            }else{
              numParams = model.current$num.params
              AICc = model.current$AICc
              curStates = toString(model.current$states)
              curSE = toString(model.current$states.se)
              curConv = model.current$converge
              #plot
                plotData = data.frame(t(model.current$states))
                seData = t(model.current$states.se[,1])
                origData = data.frame(t(coData))
                plotData$time = c(1:nrow(plotData))
                #plotData = cbind(plotData, origData)
                plotData$lb = plotData$state1 - 1.96 *seData[1] 
                plotData$ub = plotData$state1 + 1.96 *seData[1] 
                myPlot = ggplot(data=plotData, aes(x=time, y=state1)) + geom_line()  + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw() + geom_point(aes(x = time, y = origData[,1])) + geom_point(aes(x = time, y = origData[,2]) ,colour="#CC0000") + labs(title=paste(industryName, companyName, sep = ",")) + geom_point(aes(x = time, y = cCo[1,]) ,colour=28)
                ggsave(paste("C:/Users/Katharina/Documents/Umich/rdspend/zoneplots/simpleAvgPlot", industryName, companyName, ".pdf", sep = ""))
            }
            if (is.null(model.current$logLik)){
              logLik = NA
            }else{
              logLik = model.current$logLik
            }
        } else if(rdUsable > 6 & patUsable ==0){
          #run different model
            ZSingle = matrix(list("z1"))
            RAllRSingle = "diagonal and equal"
            model.list = list(B=BAll, U=UAll, Q=QAll, A=AAll, R=RAllRSingle,  Z=ZSingle, c= cCo, C = "unconstrained")
            model.current = MARSS(coData[1,], model = model.list, miss.value =NA, control = control.list)
            modtype = 2
            if (is.null(model.current$num.params)){
              numParams = NA
              AICc = NA
              curStates = NA
              curSE = NA
              curConv = NA
            }else{
              numParams = model.current$num.params
              AICc = model.current$AICc
              curStates = toString(model.current$states)
              curSE = toString(model.current$states.se)
              curConv = model.current$converge
              #plot
                plotData = data.frame(t(model.current$states))
                seData = t(model.current$states.se[,1])
                origData = data.frame(t(coData))
                plotData$time = c(1:nrow(plotData))
                #plotData = cbind(plotData, origData)
                plotData$lb = plotData$state1 - 1.96 *seData[1] 
                plotData$ub = plotData$state1 + 1.96 *seData[1] 
                myPlot = ggplot(data=plotData, aes(x=time, y=state1)) + geom_line()  + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw() + geom_point(aes(x = time, y = origData[,1])) + geom_point(aes(x = time, y = origData[,2]) ,colour="#CC0000") + labs(title=paste(industryName, companyName, sep = ",")) + geom_point(aes(x = time, y = cCo[1,]) ,colour=28)
                ggsave(paste("C:/Users/Katharina/Documents/Umich/rdspend/zoneplots/simpleAvgPlot", industryName, companyName, ".pdf", sep = ""))
            }
            if (is.null(model.current$logLik)){
              logLik = NA
            }else{
              logLik = model.current$logLik
            }
        } else{
          modtype = 3
          numParams = NA
          AICc=NA
          logLik = NA
          model.current = NA
          curStates = NA
          curSE = NA
          curConv = NA
        }
        #print("before")
        cur.outdata =data.frame(industry = industryName, company = companyName, logLik = logLik, numParams = numParams, AICc = AICc, modtype = modtype, states = curStates, se = curSE, converge= curConv,stringsAsFactors = FALSE)
        colnames(cur.outdata)= colnames(output.data)
        #print("after")
        output.data= rbind(output.data, cur.outdata)
        assign(paste("modeloneZ", industryName, companyName, sep = "."), model.current)   
        modelList[[n]]= model.current
        n = n+1
    } 
  }
  write.csv(as.matrix(output.data),file = "C:/Users/Katharina/Documents/Umich/RDSpend/test.csv")
  save.image(file = "zone.RData")

#get outputs from covariate model 1===========================================================
  stateList = list()
  SEList = list()
  AICList = data.frame(matrix(nrow = length(modelList), ncol = 1))
  CIList = list()
  colnames(AICList) = c("AIC")
  completedList = c(1,4,5,6,9,11,17,21,22,38,42,43,44,46,50,53,55,57,65,66,74,79,80,81,83,90,91,95,96,97,98,100,101,104,107,109,114,116,120,126,134,139,146,151,163,165,166,168,173,174,175,176,177,179,180,186,187,188,189,190,199,202,204,205,208,209,211,213,214,215,217,218,222,226,232,239,240,242,243,244,245,248,249,250,251,252,253,255,257,261,262,263,265,271,272,281,283,284,286,287,288,289,291,295,297,299,305,310,311,314,316,317,318,319,320,321,322,323,324,326,327,328,329,330,332,333,334,337,341,342,343,345,350,352,353,359,365,366,371,373,377,382,383,388,392,393,395,396,397,400,401,403,404,407,411,412,413,422,424,425,430,431,435,439,442,451,452,456,458,461,471,480,481,493,497,503,505,507,508,510,512,513,517,519,520,524,525,527,529,534,535,536,537,539,540,544,546,547,550,552,553,555,556,559,560,561,562,563,565,568,569,570,572,575,578,582,584,586,587,588,590,592,597,599,602,603,605,606,607,608,612,613,616,622,623,624,625,628,633,637,638,639,640,641,643,645,646,650,651,652,653,659,661,662,663,668,669,671,676,677,680,682,684,685,686,694,696,698,706,707,710,714,718,719,720,721,722,723,725,727,728,729,732,733,734,738,739,741,742,743,744,745,747,748,756,757,758,759,762,764,767,769,770,773,774,775,776,778,780,786,789,792,805,806,807,808,809,811,812,815,817,818,819,820,821,823,831,834,839,840,842,850,852,853,854,857,859,861,863,869,870,871,873,874,878,879,881,882,891,893,895,897,898,900,901,902,903,905,910,911,916,919,921,923,925,926,927,930,931,932,933,934,935,938,942,945,947,956,957,961,962,963,965,966,969,971,975,977,978,981,982,990,996,997,998,999,1000,1001,1002,1006,1007,1008,1010,1013,1016,1021,1022,1023,1024,1025,1028,1029,1032,1036,1037,1040,1041,1044,1045,1047,1048,1049,1051,1054,1055,1056,1060,1061,1062,1068,1070,1072,1077,1081,1084,1089,1091,1092,1093,1094,1095,1096,1097,1098,1099,1102,1106,1108,1109,1110,1112,1115,1121,1122,1123,1125,1129,1131,1132,1133,1134,1135,1137,1139,1140,1143,1147,1150,1155,1156,1157,1161,1162,1163,1164,1165,1170,1174,1175,1176,1177,1178,1179,1181,1182,1184,1185,1187,1188,1195,1197,1199,1200,1201,1202,1206,1211,1212,1213,1215,1218,1219,1222,1224,1225,1226,1227,1232,1233,1235,1236,1239,1242,1245,1256,1260,1263,1268,1270,1271,1272,1273,1274,1275,1276,1277,1280,1281,1282,1283,1289,1290,1301,1316,1319,1325,1334,1335,1340,1341,1343,1345,1351,1352,1354,1356,1358,1361,1364,1379,1382,1383,1390,1397,1398,1400,1401,1403,1416,1430,1433,1435,1437,1438,1439,1443,1446,1447,1448,1451,1452,1453,1454,1456,1458,1459,1464,1467,1471,1473,1474,1475,1476,1480,1481,1484,1485,1486,1487,1490,1491,1492,1493,1503,1506,1507,1513,1517,1522,1527,1528,1529,1530,1531,1532,1534,1535,1538,1542,1543,1545,1546,1550,1552,1555,1557,1561,1565,1567,1569,1570,1572,1577,1578,1580,1581,1582,1584,1585,1587,1588,1594,1595,1596,1597,1598,1599,1601,1604,1605,1609,1611,1612,1614,1615,1620,1621,1622,1632,1635,1636,1640,1643,1644,1646,1648,1650,1656,1659,1671,1672,1677,1678,1681,1688,1695,1698,1703,1706,1711,1712,1723,1725,1734,1736,1739,1742,1745,1749,1752,1754,1761,1767,1771,1776,1785,1790,1791,1792,1794,1795,1797,1798,1807,1809,1812,1813,1817,1818,1819,1823,1825,1837,1839,1842,1844,1845,1846,1847,1849,1850,1853,1854,1860,1871,1872,1874,1875,1879,1880,1884,1890,1896,1898,1902,1903,1905,1906,1908,1911,1912,1913,1916,1918,1921,1922,1923,1924,1928,1929,1930,1931,1932,1934,1935,1937,1941,1944,1945,1947,1949,1950,1951,1953,1955,1956,1957,1958,1959,1961,1964,1965,1966,1969,1971,1974,1977,1978,1980,1981,1982,1985,1986,1987,1988,1989,1990,1992,1993,1995,1996,1998,1999,2000,2001,2003,2004,2005,2006,2007,2008,2009,2010,2012,2013,2014,2018,2021,2022,2025,2026,2027,2029,2031,2033,2034,2036,2037,2040,2041,2047,2050,2051,2052,2054,2055,2056,2059,2061,2062,2063,2065,2066,2069,2070,2072,2075,2077,2079,2082,2085,2086,2092,2097,2099,2102,2103,2108,2109,2110,2114,2116,2117,2119,2121,2129,2133,2134,2135,2139,2140,2143,2151,2153,2155,2157,2158,2159,2160,2161,2162,2163,2164,2165,2167,2168,2169,2172,2176,2177,2178,2179,2180,2181,2182,2183,2184,2185,2188,2189,2191,2193,2194,2195,2197,2198,2199,2201,2206,2207,2208,2210,2216,2217,2219,2221,2222,2223,2225,2227,2228)
  completedList2  = c(2230,2231,2232,2233,2241,2245,2246,2248,2249,2250,2251,2253,2256,2258,2259,2260,2261,2265,2270,2271,2272,2279,2281,2284,2285,2289,2291,2293,2294,2301,2302,2304,2309,2312,2313,2314,2316,2317,2319,2327,2330,2332,2333,2334,2336,2337,2338,2339,2340,2342,2345,2347,2348,2352,2359,2364,2365,2368,2374,2377,2382,2383,2385,2387,2389,2390,2397,2398,2409,2410,2411,2414,2416,2419,2422,2424,2426,2427,2428,2430,2432,2434,2437,2438,2440,2443,2445,2446,2448,2449,2450,2451,2453,2455,2459,2460,2462,2463,2464,2468,2469,2471,2473,2475,2477,2478,2480,2482,2486,2487,2488,2490,2493,2498,2501,2502,2506,2509,2510,2511,2512,2513,2514,2515,2516,2517,2522,2523,2524,2525,2526,2527,2528,2533,2534,2536,2539,2541,2546,2548,2549,2551,2556,2557,2562,2563,2564,2566,2567,2569,2575,2576,2579,2580,2581,2584,2586,2588,2596,2597,2598,2604,2607,2611,2612,2616,2630,2633,2634,2635,2637,2639,2641,2642,2643,2646,2649,2650,2651,2653,2654)
  completedList3 = c(2656,2657,2661,2662,2663,2665,2667,2668,2669,2670,2676,2678,2679,2681,2682,2685,2688,2690,2691,2694,2696,2699,2702,2703,2704,2708,2709,2711,2712,2715,2717,2720,2722,2726,2727,2731,2733,2738,2744,2747,2749,2750,2751,2753,2755,2756,2757,2760,2761,2765,2768,2770,2771,2772,2776,2777,2778,2780,2781,2782,2783,2785,2787,2788,2790,2800,2801,2804,2807,2812,2820,2823,2824,2826,2831,2846,2847,2849,2859,2860,2867,2875,2883,2901,2903,2905,2947,2949,2963,2987,2990,2991,2999,3006,3111,3226,3233,3258,3276,3297,3309,3312,3313,3315,3319,3320,3370,3385,3389,3390,3391,3393,3418,3432,3435,3436,3441,3451,3456,3459,3464,3465,3471,3472,3473,3481,3486,3493,3523,3525,3529,3544,3547,3557,3564,3567,3570,3594,3598,3607,3609,3650,3686,3702,3733,3749,3750,3752,3755,3758,3766,3781,3799,3810,3817,3832,3841,3861,3881,3899,3901,3924,3969,3980,3994,3997,4011,4030,4032,4037,4045,4046,4061,4070,4086,4093,4104,4105,4107)
  completedList = c(completedList, completedList2)
  completedList = c(completedList, completedList3)
  for (i in 1:length(completedList)){
    n= completedList[[i]]
    AICList[i,1] = modelList[[n]]$AICc
    stateList[[i]] = modelList[[n]]$states
    SEList[[i]] = modelList[[n]]$states.se #standard errors on the states
    #CIList[[i]] = MARSSparamCIs(modelList[[n]]) #, method = "parametric") fails to run
    #curResiduals = residuals(model.list[[n]])
  }

lapply(stateList, write, "C:/Users/Katharina/Documents/Umich/RDSpend/test3.csv", append=TRUE, ncolumns=30)
lapply(SEList, write, "C:/Users/Katharina/Documents/Umich/RDSpend/test4.csv", append=TRUE, ncolumns=30)

#plot====================================================================================
 #plotList= list()
  completetedList2=c(1,4,5,9,11,21,22,38,42,43,44,46,50,53,55,57,66,74,79,80,81,83,90,91,96,97,98,100,101,104,107,109,114,116,120,126,134,139,151,163,165,168,173,174,175,177,179,180,186,187,188,189,190,199,202,204,205,208,209,213,214,215,217,218,226,232,239,240,242,243,244,245,248,249,250,251,252,253,255,257,261,262,263,265,271,272,281,283,284,286,287,288,289,291,297,305,310,311,314,316,317,318,319,320,321,322,323,324,326,327,328,329,330,332,333,334,337,341,343,345,352,353,365,366,371,373,377,382,383,388,392,393,395,396,397,403,404,407,411,412,413,424,425,431,435,439,442,452,456,458,461,471,481,497,505,507,508,510,513,520,524,525,527,529,536,539,540,544,546,547,550,553,555,556,559,560,561,562,563,565,569,570,572,578,582,584,586,587,588,590,592,597,599,602,603,605,607,608,612,613,616,622,623,625,633,637,638,639,640,641,643,645,646,651,652,653,661,662,663,671,676,677,680,682,684,685,686,694,696,698,706,707,714,719,720,721,722,723,725,727,728,729,732,733,738,739,741,742,743,744,745,747,748,756,757,758,759,762,764,769,773,774,775,776,778,786,792,805,806,808,809,812,815,818,819,820,834,839,840,842,850,852,853,854,857,859,861,863,869,870,871,873,878,881,882,891,893,895,897,900,901,903,905,910,916,919)
  completedList2 = c(completedList2,923,925,926,927,930,931,932,934,935,938,945,957,962,965,966,971,975,977,978,982,990,996,997,998,999,1000,1001,1002,1006,1007,1008,1010,1013,1016,1021,1022,1023,1024,1025,1028,1029,1032,1036,1037,1040,1041,1045,1048,1049,1054,1055,1056,1060,1061,1062,1068,1070,1072,1077,1081,1084,1089,1091,1092,1093,1094,1095,1096,1097,1098,1099,1106,1108,1110,1115,1121,1122,1123,1125,1129,1131,1132,1133,1134,1137,1139,1140,1143,1147,1150,1155,1156,1157,1161,1162,1163,1164,1165,1174,1175,1176,1177,1178,1179,1181,1182,1184,1185,1187,1188,1195,1197,1200,1201,1202,1206,1211,1212,1213,1215,1218,1219,1222,1224,1226,1232,1233,1239,1245,1256,1260,1263,1268,1270,1271,1273,1275,1276,1277,1280,1281,1282,1283,1289,1290,1319,1334,1335,1340,1341,1351,1352,1354,1356,1379,1383,1390,1397,1398,1400,1403,1416,1430,1433,1435,1437,1438,1443,1447,1448,1451,1452,1453,1454,1456,1458,1459,1464,1471,1473,1474,1476,1480,1481,1485,1486,1490,1493,1503,1506,1513,1517,1522,1527,1528,1530,1531,1534,1535,1538,1542,1543,1545,1546,1550,1552,1555,1557,1565,1567,1569,1570,1572,1577,1578,1581,1582,1584,1585,1587,1588,1594,1595,1596,1597,1598,1599,1601,1604,1605,1609,1611,1612,1614,1620,1621,1622,1632,1635,1636,1640,1643,1644,1646,1648,1650,1656,1659,1671,1672,1677,1678,1681,1688,1698,1703,1706,1723,1734,1736,1739,1742,1745,1749,1752,1754,1761,1785,1790,1791,1792,1795,1797,1798,1809,1817,1818,1823,1837,1839,1842,1844,1845,1846,1847,1849,1853,1871,1872,1874,1879,1880,1884,1890,1898,1903,1905,1906,1908,1911,1912,1913,1916,1918,1922,1923,1924,1928,1929,1930,1931,1932,1934,1935,1937,1941,1944,1945,1947,1949,1950,1953,1955,1957,1958,1959,1961,1964,1965,1966,1971,1974,1977,1978,1980,1981,1982,1985,1986,1987,1988,1989,1990,1992,1993,1995,1996,1998,1999,2000,2001,2003,2004,2005,2006,2007,2008,2009,2010,2012,2013,2014,2021,2022,2025,2026,2027,2029,2033,2036,2037,2040,2041,2047,2050,2051,2052,2054,2055,2056,2059,2061,2062,2063,2065,2066,2070,2072,2077,2079,2082,2085,2086,2092,2097,2102,2103,2108,2109,2114,2116,2117,2119,2121,2129,2133,2135,2140,2143,2151,2153,2155,2157,2158,2159,2160,2161,2163,2164,2167,2168,2169,2176,2177,2178,2179,2180,2181,2182,2183,2185,2188,2189,2191,2193,2194,2195,2197,2198,2199,2201,2206,2208,2210,2216,2217,2219,2221,2222,2223,2225,2227,2228,2230,2231,2232,2233,2241,2245,2246,2248,2249,2250,2251,2253,2258,2259,2260,2261,2265,2270,2271,2272,2279,2281,2284,2285,2289,2291,2293,2294,2301,2302,2304,2309,2312,2313,2314,2316,2317,2319,2327,2330,2332,2333,2334,2336,2337,2340,2342,2345,2347,2352,2359,2364,2365,2368,2374,2382,2383,2385,2387,2390,2398,2410,2411,2414,2416,2419,2422,2424,2426,2427,2428,2432,2434,2438,2443,2445,2446,2448,2451,2453,2455,2459,2463,2464,2468,2469,2471,2473,2475,2487,2488,2490,2493,2498,2501,2502,2506,2509,2511,2513,2514,2515,2516,2522,2523,2525,2526,2533,2534,2539,2541,2546,2548,2551,2556,2557,2562,2563,2564,2566,2575,2576,2579,2580,2581,2584,2586,2588,2597,2598,2604,2607,2611,2612,2633,2637,2639,2641,2642,2643,2649,2650,2651,2653,2654,2657,2662,2667,2668,2669,2670,2676,2678,2679,2682,2685,2688,2690,2691,2694,2699,2702,2703,2704,2708,2709,2711,2712,2715,2722,2726,2733,2738,2744,2750,2751,2753,2755,2757,2760,2761,2765,2768,2771,2772,2776,2777,2778,2783,2785,2787,2788,2790,2800,2801,2804,2807,2812,2820,2824,2831,2846,2849,2859,2860,2867,2875,2883,2901,2905,2963,2987,2990,2991,2999,3006,3226,3233,3258,3297,3309,3312,3320,3385,3389,3390,3391,3432,3435,3436,3441,3451,3459,3464,3465,3471,3472,3493,3529,3547,3557,3594,3650,3686,3702,3733,3749,3750,3752,3755,3758,3766,3781,3799,3817,3832,3861,3924,3969,3994,3997,4030,4037,4046,4061,4093,4104,4105,4107)
  n=1
  for (i in 1:length(oneVarList)){ #industry loop
    industryName = nameVector[i] 
    curData = twoVarList[[i]]
    companyNameVector = rownames(curData)[1:(nrow(curData)/2)]
    curAvg = industryAvgVector[[i]]
  for (j in 1:(nrow(curData)/2)){#for each company, run model
    if (n %in% completedList2){
      companyName = companyNameVector[j]
      coData = inputList[[n]]
      cCo = covarList[[n]]
      modelName = paste("model", industryName, companyName, sep = ".")
      model.current = eval(parse(text = modelName))
      plotData = data.frame(t(model.current$states))
      seData = t(model.current$states.se[,1])
      origData = data.frame(t(coData))
      plotData$time = c(1:nrow(plotData))
      #plotData = cbind(plotData, origData)
      plotData$lb = plotData$state1 - 1.96 *seData[1] 
      plotData$ub = plotData$state1 + 1.96 *seData[1] 
      myPlot = ggplot(data=plotData, aes(x=time, y=state1)) + geom_line()  + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw() + geom_point(aes(x = time, y = origData[,1])) + geom_point(aes(x = time, y = origData[,2]) ,colour="#CC0000") + labs(title=paste(industryName, companyName, sep = ",")) + geom_point(aes(x = time, y = cCo[1,]) ,colour=28)
      ggsave(paste("C:/Users/Katharina/Documents/Umich/rdspend/plotssimpavg/simpleAvgPlot2", industryName, companyName, ".pdf", sep = ""))
    }
    n = n+1
  }
  }
  argsList <- c(plotList,3,2)
  namesList = c(1:length(plotList))
  namesList = as.character(namesList)
  names(argsList) <- c(namesList, "nrow", "ncol")
  pdf("C:/Users/Katharina/Documents/Umich/rdspend/fits-simplemean.pdf")
  do.call(marrangeGrob, argsList)
  dev.off()
  
#INDMOD
#single-factor dfa models for industry index: r&d signal, all industries, best configuration==========================================================
  #reduce 1 var and 2 var lists
  redVarList = list()
  redVarList2 = list()
  for (k in 1:length(oneVarList)){
    current=oneVarList[[k]]
    current2 = twoVarList[[k]]
    currenthold = current
    currenthold[currenthold == 0]= NA
    currenthold2 = current2
    currenthold2[currenthold2 == 0]= NA
    secondhalf = current2[(nrow(current2)/2+1):nrow(current2),]
    secondhalf[secondhalf==0]=NA
    current2[(nrow(current2)/2+1):nrow(current2),]= secondhalf
    #current2[current2 == 0] = NA
    counters=apply(currenthold,1,function(x) sum(!is.na(x)))
    counters2=apply(currenthold2,1,function(x) sum(!is.na(x)))
    redVarList[[k]]=current[counters >4,]
    redVarList2[[k]]= current2[counters2>4,]
    print(dim(redVarList[[k]])[1]-dim(current)[1])
  } 

  #run model
    output.data = data.frame(matrix(ncol = 7, nrow = 0))
    colnames(output.data)= c("industryName", "logLik", "numParams", "AICc", "States", "SEs", "converge")
    for (k in 1:length(redVarList)){
      oneVarInput = redVarList[[k]]
      twoVarInput = redVarList2[[k]]
      numCos = nrow(oneVarInput)
      numExtra = nrow(twoVarInput)-nrow(oneVarInput)
      numTot = nrow(twoVarInput)
      #industryData = dataList[[k]]
      industryName = nameVector[k] 
      
      if (!(is.null(numCos))){ #meaning we have more than 1 company
        if (numCos >1){
          #set model inputs
          BAll = "identity"
          QAll= "diagonal and unequal" #note this could be changed if it causes problems since the unequal portion is irrelevant (it is a 1 by 1 matrix)
          #source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZCol.R")
          source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getR.R")
          ZIN= rep(1, numTot)
          #ZIN= c(rep(1, numCos), rep("z1", numExtra))
          ZIN = as.list(ZIN)
          #ZAll = matrix(ZIN, numCos,1)
          ZAll = matrix(ZIN, numTot, 1)
          #RAll = "equalvarcov" #TBD
          RAll = getR(numCos)
          AAll = "equal"
          UAll = "zero"
          
          #set model controls, if necessary
          control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)#, maxit = 1000)
          
          #run models
            stringList = c()
            model.list = list(B=BAll, U=UAll, Q=QAll, Z=ZAll, A=AAll, R=RAll)
            model.current = MARSS(twoVarInput, model = model.list, miss.value =NA, control = control.list)
            if (is.null(model.current$num.params)){
              numParams = NA
              AICc = NA
              curStates = NA
              curSE = NA
              curConv = NA
            } else{
              numParams = model.current$num.params
              AICc = model.current$AICc
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
                ggsave(paste("C:/Users/Katharina/Documents/Umich/rdspend/firststage/firststageconfig35", industryName, ".pdf", sep = ""))
            }
            if (is.null(model.current$logLik)){
              logLik = NA
            }else{
              logLik = model.current$logLik
            }
          } 
        } else{
          numParams = NA
          AICc = NA
          curStates = NA
          curSE = NA
          logLik = NA
          model.current = NA
          curConv = NA
        }
        cur.outdata =data.frame(industry= industryName, logLik = logLik, numParams = numParams, AICc = AICc, states = curStates, ses = curSE, converge =curConv, stringsAsFactors = FALSE)
        colnames(cur.outdata)= colnames(output.data)
        output.data= rbind(output.data, cur.outdata)
        modelString = paste("model", industryName, sep = ".")
        stringList = c(stringList, modelString)
        assign(paste("modelTwostepSSconfig35", industryName, sep = "."), model.current)
    }
    save.image(file = "indIndexesconfig35.RData")
    write.csv(output.data, file = "C:/Users/Katharina/Documents/Umich/RDSpend/test2.csv")

  #plot industry index files
  for (k in 1:length(redVarList)){
    oneVarInput = redVarList[[k]]
    numCos = nrow(oneVarInput)
    industryName = nameVector[k] 
    if (!(is.null(numCos))){ #meaning we have more than 1 company
      if (numCos >1){
        modelName = paste("modelTwostepSS", industryName, sep = ".")
        model.current = eval(parse(text = modelName))
        if (!(is.null(model.current$num.params))){  
          plotData = data.frame(t(model.current$states))
          seData = t(model.current$states.se[,1])
          origData = data.frame(t(oneVarInput))
          plotData$time = c(1:nrow(plotData))
          #plotData = cbind(plotData, origData)
          plotData$lb = plotData$state1 - 1.96 *seData[1] 
          plotData$ub = plotData$state1 + 1.96 *seData[1] 
          dev.off()
          myPlot = ggplot(data=plotData, aes(x=time, y=state1)) + geom_line()  + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw() + labs(title=paste(industryName, sep = ","))
          j = 24
          for (i in 1:ncol(origData)){
            curInput = paste("curCo", i, sep = "")
            #addList[[length(addList)+1]]=geom_point(aes(x = time, y = eval(parse(text =paste("curCo", i, sep = "")))), colour = j)
            addString = paste("geom_point(aes(x = time, y = origData[,", i, "]), colour = ",j,")", sep = "")
            myPlot=  myPlot+ eval(parse(text = addString))
            j = j+5
          }
          ggsave(paste("C:/Users/Katharina/Documents/Umich/rdspend/firststage/firststage", industryName, ".pdf", sep = ""))
        }
      }
    }
  }

#get individual company models=====================================================================
output.data = data.frame(matrix(ncol = 8, nrow = 0))
colnames(output.data)= c("industry", "company", "logLik", "numParams", "AICc", "states", "SEs", "modtype")
#set model inputs
  BAll = "identity"
  QAll= "diagonal and unequal" #note this could be changed if it causes problems since the unequal portion is irrelevant (it is a 1 by 1 matrix)
  source("C:/Users/Katharina/Documents/Umich/RDSpend/RCode/RDSpending/fun_getZCol.R")
  ZAll = matrix(list("z1","z2","z3"))
  AAll = "zero"
  UAll = "equal"
  covarList = list()
  inputList = list()
  modelList= list()

#set model controls, if necessary
  control.list = list(safe = TRUE, trace =1, allow.degen= TRUE)#, maxit = 1000)
  n = 1
  for (i in 1:length(oneVarList)){ #industry loop $must do rest of 189 separately
    curData = twoVarList[[i]]
    industryName = nameVector[i] 
    companyNameVector = rownames(curData)[1:(nrow(curData)/2)]
    curAvg = industryAvgVector[[i]]
    nameString = paste("modelTwostepSS", industryName, sep = ".")
    indMod = eval(parse(text =nameString))
    #check if indMod is NA, extract q and set up R
    if (!is.na(indMod)){
      if (indMod$converge==0 & indMod$par$Q[1] != 0){#model converged, and we cannot have a zero diagonal Q element
        q=indMod$par$Q[1]
        RCur = matrix(list("r1","r2","r2",q), 2, 2)
        RCurSingle = matrix(list("r1",0,"r2", 0,"r3",0,"r2", 0, q), 3, 3)
        for (j in 1:(nrow(curData)/2)){#for each company, run model
          companyName = companyNameVector[j]
          coData = rbind(curData[j,], curData[j+length(companyNameVector),])
          coData = rbind(coData, indMod$states)
          #remove all parts of covariate where our co. is missing
          nonNA1 = which(!is.na(coData[2,])) #first row
          startIndex1 = min(nonNA1)
          endIndex1 = max(nonNA1)
          nonNA2 = which(!is.na(coData[3,])) #second row
          startIndex2 = min(nonNA2)
          endIndex2 = max(nonNA2)
          curAvgTruncated = curAvg[max(startIndex1, startIndex2):min(endIndex1, endIndex2)]#removed all parts of covariate where our co. is missing
          coData = coData[,max(startIndex1, startIndex2):min(endIndex1, endIndex2)]
          #remove any entries where index is missing
          nonNA = which(!is.na(curAvgTruncated))
          startIndex = min(nonNA)
          endIndex = max(nonNA)
          curAvgTruncated = curAvgTruncated[startIndex:endIndex]#removed all parts of covariate where our co. is missing
          coData = coData[,startIndex:endIndex]
          coData[coData[2,]==0] = NA
          cCo= t(matrix(curAvgTruncated))
          naCCo = which(is.na(cCo))
          #find breaks in naCCo
          if (length(naCCo) >0){ #there are missing values
            k=1
            while (k <= length(naCCo)){ #for each entry in my missing indicator vector
              curIndex = naCCo[k] #first missing index, original vector
              endIndex = NA #last missing index, original vector
              l = 0 #holds number of missing values beyond location k, plus 1
              while(is.na(endIndex) & curIndex+l <= length(cCo)){
                if (!is.na(cCo[curIndex+l])){
                  endIndex = curIndex+l-1
                }
                l = l+1
              }
              outVect = seq(cCo[curIndex-1], cCo[endIndex+1], (cCo[endIndex+1]-cCo[curIndex-1])/((endIndex-curIndex)+2))
              cCo[curIndex:endIndex]= outVect[2:(length(outVect)-1)]
              #mean(cCo[endIndex+1], cCo[curIndex-1])
              k = k+l-1
            }
          }
          rownames(cCo)= c("IndAvg")
          covarList[[n]] = cCo
          inputList[[n]]= coData
          patData = coData[2,]
          patUsable = length(patData)-length(patData[is.na(patData)==TRUE])-length(na.exclude(patData)[na.exclude(patData) ==0])
          rdData = coData[1,]
          rdUsable = length(rdData)-length(rdData[is.na(rdData)==TRUE])-length(na.exclude(rdData)[na.exclude(rdData) ==0])
          if(rdUsable > 3 & patUsable > 3){
            modtype =1
            #run model
            model.list = list(B=BAll, U=UAll, Q=QAll, A=AAll, R=RCurSingle,  Z=ZAll, c= cCo, C = "unconstrained")
            model.current = MARSS(coData, model = model.list, miss.value =NA, control = control.list)
            #store output
            if (is.null(model.current$num.params)){
              numParams = NA
              AICc = NA
              curStates = NA
              curSE = NA
            }else{
              numParams = model.current$num.params
              AICc = model.current$AICc
              curStates = toString(model.current$states)
              curSE = toString(model.current$states.se)
              plotData = data.frame(t(model.current$states))
              seData = t(model.current$states.se[,1])
              origData = data.frame(t(coData))
              plotData$time = c(1:nrow(plotData))
              #plotData = cbind(plotData, origData)
              plotData$lb = plotData$state1 - 1.96 *seData[1] 
              plotData$ub = plotData$state1 + 1.96 *seData[1] 
              myPlot = ggplot(data=plotData, aes(x=time, y=state1)) + geom_line()  + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw() + geom_point(aes(x = time, y = origData[,1])) + geom_point(aes(x = time, y = origData[,2]) ,colour="#CC0000") + labs(title=paste(industryName, companyName, sep = ",")) + geom_point(aes(x = time, y = cCo[1,]) ,colour=28)
              ggsave(paste("C:/Users/Katharina/Documents/Umich/rdspend/plottwostage/secondstate", industryName, companyName, ".pdf", sep = ""))
            }
            if (is.null(model.current$logLik)){
              logLik = NA
            }else{
              logLik = model.current$logLik
            }
          } else if(rdUsable > 3 & patUsable ==0){
            #run different model
            ZSingle = matrix(list("z1", "z2"))
            model.list = list(B=BAll, U=UAll, Q=QAll, A=AAll, R=RCurSingle,  Z=ZSingle, c= cCo, C = "unconstrained")
            model.current = MARSS(coData[1,], model = model.list, miss.value =NA, control = control.list)
            modtype = 2
            if (is.null(model.current$num.params)){
              numParams = NA
              AICc = NA
              curStates = NA
              curSE = NA
            }else{
              numParams = model.current$num.params
              AICc = model.current$AICc
              curStates = toString(model.current$states)
              curSE = toString(model.current$states.se)
              plotData = data.frame(t(model.current$states))
              seData = t(model.current$states.se[,1])
              origData = data.frame(t(coData))
              plotData$time = c(1:nrow(plotData))
              #plotData = cbind(plotData, origData)
              plotData$lb = plotData$state1 - 1.96 *seData[1] 
              plotData$ub = plotData$state1 + 1.96 *seData[1] 
              myPlot = ggplot(data=plotData, aes(x=time, y=state1)) + geom_line()  + geom_line(aes(x = time, y = lb), plotData, lty = 'dashed')+ geom_line(aes(x = time, y = ub), plotData, lty = 'dashed')+theme_bw() + geom_point(aes(x = time, y = origData[,1])) + geom_point(aes(x = time, y = origData[,2]) ,colour="#CC0000") + labs(title=paste(industryName, companyName, sep = ",")) + geom_point(aes(x = time, y = cCo[1,]) ,colour=28)
              ggsave(paste("C:/Users/Katharina/Documents/Umich/rdspend/plottwostage/secondstate", industryName, companyName, ".pdf", sep = ""))
            }
            if (is.null(model.current$logLik)){
              logLik = NA
            }else{
              logLik = model.current$logLik
            }
          } else{
            modtype = 3
            numParams = NA
            AICc=NA
            logLik = NA
            model.current = NA
            curStates = NA
            curSE = NA
          }
        }
        cur.outdata =data.frame(industry = industryName, company = companyName, logLik = logLik, numParams = numParams, AICc = AICc, state = curStates, SEs = curSE, modtype = modtype, stringsAsFactors = FALSE)
        colnames(cur.outdata)= colnames(output.data)
        output.data= rbind(output.data, cur.outdata)
        assign(paste("model", industryName, companyName, sep = "."), model.current)   
        modelList[[n]]= model.current
        n = n+1
      }
    }
  #}
#note this does not include output for all companies, so we will have to change the plotting!
write.csv(as.matrix(output.data),file = "C:/Users/Katharina/Documents/Umich/RDSpend/test.csv")
save.image(file = "covariateMod2.RData")

#=======================================================================================================
#extracting convergence--sample code
#=======================================================================================================
#industry data
output.data = data.frame(matrix(ncol = 3, nrow = 0))
colnames(output.data)= c("industry", "company", "converge")
for (i in 1:length(oneVarList)){ 
  curData = twoVarList[[i]]
  industryName = nameVector[i] 
  companyNameVector = rownames(curData)[1:(nrow(curData)/2)]
  nameString = paste("modelTwostepSS", industryName, sep = ".")
  indMod = eval(parse(text =nameString))
  curConv = NA
  if(!is.na(indMod)){
    if(!is.null(indMod$converge)){
      curConv = indMod$converge
    } 
  } 
  cur.outdata =data.frame(industry = industryName, company = companyName,converge = curConv)
  colnames(cur.outdata)= colnames(output.data)
  output.data= rbind(output.data, cur.outdata)
}
                             
#company data
    output.data = data.frame(matrix(ncol = 3, nrow = 0))
    colnames(output.data)= c("industry", "company", "converge")
    for (i in 1:length(oneVarList)){ 
      curData = twoVarList[[i]]
      industryName = nameVector[i] 
      companyNameVector = rownames(curData)[1:(nrow(curData)/2)]
      for (j in 1:(nrow(curData)/2)){#for each company, run model
        companyName = companyNameVector[j]
        #nameString = paste("model", industryName, companyName, sep = ".")
        #nameString = paste("modelbig", industryName, companyName, sep = ".")
        nameString = paste("model", industryName, companyName, sep = ".")
        indMod = eval(parse(text =nameString))
        curConv = NA
        if(!is.na(indMod)){
          if(!is.null(indMod$converge)){
            curConv = indMod$converge
          } 
        } 
        cur.outdata =data.frame(industry = industryName, company = companyName,converge = curConv)
        colnames(cur.outdata)= colnames(output.data)
        output.data= rbind(output.data, cur.outdata)
      }
    }
    
    
    
#=======================================================================================================
#diagnostic testing
#=======================================================================================================

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