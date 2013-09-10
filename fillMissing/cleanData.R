#NOTES:======================================================================
  #this file compiles a cleaned input file for the regressions from all model runs

#libraries ====================================================================
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
  library(lme4)
  library(glm2)
  library(plm)

#clear workspace ===================================================================
  rm(list = ls())

#load RD input data==============================================================
  load(file = "inputs.RData")

#get completed runs into correct format ===================================================================
  costrict.dat = read.csv("companiesstrict.csv") 
  success.dat = read.csv("companiesstictsuccess.csv") #successfully run co's in strict case
  successList =levels(as.factor(success.dat$companyName))
  companies.dat = costrict.dat[costrict.dat$companyName %in% successList,]
  companies.dat$strictFlag = 1
  cononstrict.dat = read.csv("companiesnonstrict.csv")
  success.dat = read.csv("companiesnonstrictsuccess.csv")
  successList = levels(as.factor(success.dat$companyName))
  temp = cononstrict.dat[cononstrict.dat$companyName %in% successList,]
  temp$strictFlag = 0
  companies.dat = rbind(companies.dat, temp)

  indstrict.dat = read.csv("industriesstrict.csv")
  success.dat = read.csv("industriesstrictsuccess.csv")
  successList = levels(as.factor(success.dat$industryName))
  industries.dat = indstrict.dat[indstrict.dat$industryName %in% successList,] 
  industries.dat$strictFlag = 1
  indnonstrict.dat = read.csv("industriesnonstrict.csv")
  success.dat = read.csv("industriesnonstrictsuccess.csv")
  successList = levels(as.factor(success.dat$industryName))
  temp= indnonstrict.dat[indnonstrict.dat$industryName %in% successList,]
  temp$strictFlag = 0
  industries.dat = rbind(industries.dat, temp)

#simulate companies with number of patents data============================================
  coenough.dat = read.csv("companiesenoughdata.csv")
  simCos= levels(as.factor(coenough.dat$companyName))
  sim.dat = RDDATA[RDDATA$gvkey %in% simCos,]
  #create input lists by industry 
  indList = levels(factor(sim.dat$sic))
  dataList = list()
  nameVector = c()
  for (i in 1:length(indList)){
    curData = sim.dat[sim.dat$sic ==indList[i],]
    dataList[[length(dataList)+1]]= curData
    nameVector[[length(nameVector)+1]]= indList[i]
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
  #simulate company data using the number of patents information
    counter = 0
    for (i in length(twoVarList)){
      curIndData =twoVarList[[i]]
      curIndDataOne = oneVarList[[i]]
      for (j in 1:nrow(curIndDataOne)){
        #get input data
          curCo = data.frame(rd = curIndData[j,], numPats = curIndData[j+ nrow(curIndDataOne),])
        #try regression
          if (length(na.exclude(curCo$numPats))>2){ #if we have at least two data points in number of patents, try projecting
            curReg = lm(rd~numPats, data = curCo)
            if (summary(curReg)$r.squared > .1){
              original = curCo[,1]
              replace = predict(curReg, newdata = curCo[,2])
              for (k in 1:length(original)){
                if (is.na(original[k])){
                  original[k]= replace[k]
                }
              }
              counter = counter +1
              oneVarList[[i]][j,]= original
            }
          }
      }
    }
  
#simulate companies with industry data============================================
  counter = 0
  for (i in length(oneVarList)){
    curIndDataOne = oneVarList[[i]]
    for (j in 1:nrow(curIndDataOne)){
      #get input data
        curCo = curIndDataOne[j,]
        curInd = industries.dat[industries.dat$industryName == nameVector[i],]
        curInd = curInd[order(curInd$datayear),]
        startYear = min(curInd$datayear)
        startYear = startYear+1-1980
        indInput = rep(NA, 27)
        indInput[startYear:27]= curInd$state
      #try regression
        lm.dat = data.frame(coRD = curCo, indRD = indInput)
        curReg = lm(curCo~indInput)
        if (summary(curReg)$r.squared > .1){
          original = curCo
          predict.dat = data.frame(indRD = indInput)
          replace = predict(curReg, predict.dat)
          for (k in 1:length(original)){
            if (is.na(original[k])){
              original[k]= replace[k]
            }
          }
          counter = counter +1
          oneVarList[[i]][j,]= original
        }
      }
    }

#try runnign industry models with only the companies that have working company models===================================
  #get inputs of just working companies
    coWorkList = levels(as.factor(companies.dat$companyName))
    indWorkList = levels(as.factor(industries.dat$industryName))
    coworking.dat = RDDATA[RDDATA$gvkey %in% coWorkList,]
    #create input lists by industry 
    indsInSample = levels(factor(coworking.dat$sic))
    dataWorkList = list()
    nameVectorWork = c()
    for (i in 1:length(indsInSample)){
      curData = coworking.dat[coworking.dat$sic ==indsInSample[i],]
      dataWorkList[[length(dataWorkList)+1]]= curData
      nameVectorWork[[length(nameVectorWork)+1]]= indsInSample[i]
    }
    #clean inputs for each industry
    oneVarListWork = list()
    numCosListWork = list()
    startYearListWork = list()
    for (i in 1:length(nameVectorWork)){
      curData = dataWorkList[[i]]
      curDataOneVar = ddply(curData, ~datayear, 
                            function(df) {
                              res = data.frame(rbind(df$xrdAdj)) #take R&D spending as a percentage of sales
                              names(res) = sprintf("%s",df$gvkey)
                              res
                            }
      )
      numCos = ncol(curDataOneVar)-1
      model.data = curDataOneVar[-c(1)] #delete time entry
      model.data = as.matrix(model.data)
      model.data = t(model.data)
      oneVarListWork[[i]]= model.data
      numCosListWork[[i]] = numCos
      startYearListWork[[i]]= min(curDataOneVar$datayear)
    }

  #reduce to usable variables
  #  redVarListWork = list()
  #  for (k in 1:length(oneVarListWork)){
  #    current=oneVarListWork[[k]]
  #    currenthold = current
  #    currenthold[currenthold == 0]= NA
  #    counters=apply(currenthold,1,function(x) sum(!is.na(x)))
  #    redVarListWork[[k]]=current[counters >6,]
  #    redVarListWork[[k]]= oneVarListWork[[k]]
  #  }

  #run model=======================================================================================
    #note run this with at least 5 data points
    output.data = data.frame(matrix(ncol = 7, nrow = 0))
    colnames(output.data)= c("industryName", "logLik", "numParams", "AICc", "States", "SEs", "converge")
    output.outofsample = data.frame(matrix(ncol = 4, nrow = 0))
    colnames(output.outofsample) = c("industryName", "datayear", "state", "se")
    #missedList=c(3931,2590,3390,2520,2540,3221,3360,2531,3824,3570,3760,3942,3550,3567,3821,7384,2033,2111,2840,5160,3444,2833,2990,3562,9997,3561,3716,4822,5200,3452,3433,3442,3555,3844,7381,2013,2273,2851,2800,2820,3081,3678,2430,3630,3843,3270,3721,7320,7830,3523,3590,8734,5070,3290,5141,2000,3420,7380,3827,3851,5110,5051,5172,2060,2821,3569,3728,2040,5190,8090,2030,2400,2721,3711,5047,5080,8700,3580,3585,4700,2711,1382,7310,7371,2200,2870,3533,3944,8082,2860,3829,4412,1400,3861,2621,3949,3990,5065,7200,1389,3823,7359,3812,3640,8200,8742,3572,5122,3826,8071,2844,2750,5045,8731,8711,3825,3571,7374,2911,3312,7011,3089,3559,5411,3842,5961,4899,3841,4812,7990,7389,1040,9995,3845,5812,4813,7373,7370,7372,1044,1520,2084,2771,3281,3412,3911,4011,4013,4220,4222,4512,4513,5010,5050,5072,5082,5171,5180,5211,5271,5311,5331,5399,5531,5600,5621,5651,5944,5945,7340,7377,7997,8051,8111,8300,5661,5810,7350,7361,8062,8400,900,2790,4522,7000,7510,7996,8351,2732,3910,4100,5030,5731,8060,8900,2253,4213,4610,5000,5020,5712,5900,8050,2015,2044,4400,5064,5094,5130,5400,5735,8600,2085,2092,3211,5940,7822,5700,7819,5013,7829,8011,2421,5031,5099,5412,7841,8744,1540,3241,3451,5063,7331,8741,4210,5734,7311,8000,800,1531,2086,3873,2451,5090,7385,200,3430,2340,4581,4832,5093,5140,7948,1090,1731,7330,7500,2052,4731,7812,2300,2780,5912,1220,3720,1381,5150,7900,8093,2673,2741,5500,5960,2452,3960,3250,7600,3317,5990,8721,3751,2611,5084,2082,4833,1600,3822,5040,3950,3743,7363,2090,3790,3724,3480,4841,3730)
    for (k in 1:length(oneVarListWork)){
      oneVarInput = oneVarListWork[[k]]
      numCos = nrow(oneVarInput)
      numYears = 1
      industryName = nameVector[k] 
      if (industryName %in% indWorkList){ 
        print("already works")
      }
      else{
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
          UAll = "zero" #TBD was zero
          #set model controls, if necessary
          control.list = list(safe = TRUE, trace =1, allow.degen= TRUE, maxit = 1000)#,conv.test.slope.tol=0.1)
          #TBD set conv.test.slope.tol to .1 or less to test for convergence
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
            ggsave(paste("plots/notstrict", industryName, ".pdf", sep = ""))
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
      cur.outofsample$datayear = c(startYearList[[k]]:(startYearList[[k]]+numYears-1))
      cur.outofsample$state = stateVect
      cur.outofsample$se = seVect
      output.outofsample = rbind(output.outofsample, cur.outofsample)         
      modelString = paste("IndEdit", industryName, sep = ".")
      stringList = c(stringList, modelString)
      assign(paste("IndEdit", industryName, sep = "."), model.current)
      }
    }
    save.image(file = "workCosonlynotstrict.RData")
    write.csv(output.data, file = "test2.csv")
    write.csv(output.outofsample, file = "test3.csv")
    IndEdit.out = output.outofsample
    save(IndEdit.out, file = "workCosonlynotstrict.out.RData")
        

#add model runs to dataset on industries=================================================================
  #TBD note that we do not have raw R&D for industries or companies in this dataset, but we could get these if they become necessary
  industries.dat$onlyWorkingFlag = 0
  missindstrict.dat = read.csv("missingindustriesstrict.csv")
  keepInds = c(800,1400,1540,2090,2092,2300,2340,2451,2452,2611,2621,2840,2844,2870,2911,3089,3250,3270,3317,3360,3420,3430,3442,3444,3452,3550,3569,3571,3590,3630,3728,3812,3824,3844) #industries that run with strict restricition if only working companies are used
  missindstrict.dat = missindstrict.dat[missindstrict.dat$industryName %in% keepInds,]
  missindstrict.dat$strictFlag = 1
  missindstrict.dat$onlyWorkingFlag = 1
  industries.dat = rbind(industries.dat, missindstrict.dat)
  
  missindnonstrict.dat = read.csv("missingindustriesnonstrict.csv")
  keepInds = c(2430, 3433)
  missindnonstrict.dat = missindnonstrict.dat[missindnonstrict.dat$industryName %in% keepInds,]
  missindnonstrict.dat$strictFlag = 0
  missindnonstrict.dat$onlyWorkingFlag = 1
  industries.dat = rbind(industries.dat, missindnonstrict.dat)

#regression analysis=====================================================================
  #get lhs variable (number of patents)
    #lhs.dat = read.csv("lhsdat.csv") 
  #aggregate dataset  
    companies.dat$id = paste(companies.dat$industryName, companies.dat$datayear, sep = "")
    companies.dat$companyid = paste(companies.dat$companyName, companies.dat$datayear, sep = "")
    industries.dat$id = paste(industries.dat$industryName, industries.dat$datayear, sep = "")
    reg.dat = merge(x = companies.dat, y = industries.dat, by = "id", all.x = TRUE)
    colnames(reg.dat) = c("id", "industryName", "companyName", "datayear", "state_co", "stError_co", "strictFlag_co", "companyid", "drop", "drop2", "state_ind", "stError_ind", "strictFlag_ind", "onlyWorkingFlag")
    reg.dat= reg.dat[,c(1:8, 11:14)]
    #reg.dat = merge(x = reg.dat, y = lhs.dat, by = "companyName", all.x = TRUE)
    #add lhs variable
      RDDATA$companyid = paste(RDDATA$gvkey, RDDATA$datayear, sep = "")
      #keep only first entry with each company id
        RDDATA = RDDATA[!duplicated(RDDATA[,c('companyid')]),]
        #multiIDs= data.frame(freq(ordered(RDDATA$companyid), plot=FALSE))
        #multiIDs = multiIDs[multiIDs$Frequence >1,]
      reg.dat = merge(reg.dat, RDDATA, by = "companyid", all.x = TRUE) #problem arises here!
      reg.dat.nodeletes = reg.dat
    #delete any entries where company or industry model was not estimated for these years
      reg.dat = reg.dat[!is.na(reg.dat$state_co) & !is.na(reg.dat$state_ind),]
    #delete missing LHS--in this reduced sample there are no missing LHS's
      reg.dat = reg.dat[!is.na(reg.dat$npatappAdj),]
    #rename datayear
      colnames(reg.dat)[5] = "datayear"

  #model
    all.mod = lm(npatappAdj~state_co + stError_co + state_ind + stError_ind, data = reg.dat)
  #diagnostic plots
    #fitted versus residuals 
      plot(fitted(all.mod), residuals(all.mod), xlab="Fitted", ylab="Residuals")
    #fitted versus abs(residuals)
      plot(fitted(all.mod), abs(residuals(all.mod)), xlab="Fitted", ylab="|Residuals|")
    #QQ
      qqnorm(residuals(all.mod), ylab="Residuals") # Q-Q plot
      qqline(residuals(all.mod))
    #pairwise plot for linearlity
      pairs(reg.dat, panel=panel.smooth) #this takes forever
    #outliertest
      outlierTest(all.mod, order=FALSE) # Bonferonni p-value for most extreme obs
  #sampled regression
    sampleIndex = sample(nrow(reg.dat), size = nrow(reg.dat)/2, replace = FALSE)
    sample.dat = reg.dat[sampleIndex,]
    sample.mod = lm(npatappAdj~state_co + stError_co + state_ind + stError_ind, data = sample.dat)
  #out of sample
    outsample.dat = reg.dat[-sampleIndex,]
    yVect = outsample.dat$npatappAdj
    curMean = mean(yVect)
    curSSTot = sum((yVect-curMean)^2)
    curPred = predict(sample.mod, outsample.dat)
    curSSRes = sum((yVect-curPred)^2)
    curR2= 1-(curSSRes/curSSTot)
  #higher order terms
    ho.mod = lm(npatappAdj~state_co + I(state_co^2)+stError_co + state_ind +I(state_ind^2) + stError_ind, data = reg.dat)
  #industry dummies
    reg.dat$industryName = as.factor(reg.dat$industryName)
      allInd.mod = lm(npatappAdj~industryName + state_co + stError_co + state_ind + stError_ind, data = reg.dat)
  #random effects (different intercept for each industry (or company))
    #random intercepts
      allRE.mod = lmer(npatappAdj~state_co + stError_co +state_ind + stError_ind + (1|industryName), data = reg.dat)
    #random intercepts and slopes
      allRE2.mod = lmer(npatappAdj~state_co + stError_co +state_ind + stError_ind + (1+state_ind|industryName), data = reg.dat)
    #see here for move info: http://www.bodowinter.com/tutorial/bw_LME_tutorial2.pdf
  #logistic regression
    #all data model
      #set up categories
        reg.dat$catNumPat = NA
        reg.dat[reg.dat$npatappAdj==0,]$catNumPat= 0
        percentile = quantile(reg.dat[reg.dat$npatappAdj >0,]$npatappAdj, c(.5))
        reg.dat[reg.dat$npatappAdj>0 & reg.dat$npatappAdj < percentile,]$catNumPat= 1
        reg.dat[reg.dat$npatappAdj >= percentile,]$catNumPat= 2
      #using multinom
        test <- multinom(catNumPat ~ state_co + stError_co + state_ind + stError_ind, data = reg.dat)
        exp(coef(test))
        #pvalues
        summary.test = summary(test, Wald = TRUE)
        pchisq(summary.test$Wald.ratios^2, df = 1, low = F)
      #fit
        mod = test
        fitstat(mod)
      #function for getting fit output for multinom
        fitstat <- function(object) { 
          #thanks Ripley, B. D. for telling how to get the LogLik and when is invalid. 
        {if (!is.null(object$call$summ) && !identical(object$call$summ,0)) 
          stop("when 'summ' argument is not zero,can NOT get Loglik") } 
        object.base <- update(object,.~1,trace=FALSE) 
        dev.base <- deviance(object.base) ; L.base <- - dev.base/2 
        dev.full <- deviance(object) ; L.full <- - dev.full/2 
        G2 <- dev.base - dev.full 
        df <- object$edf - object.base$edf 
        LR.test.p <- pchisq(G2,df,lower=F) 
        
        aic <- object$AIC 
        
        n<-dim(object$residuals)[1] 
        
        #get the predict value to cal count R2 
        pre <- predict(object,type="class") 
        y <- eval.parent(object$call$data)[,as.character(object$call$formula[[2]])] 
        if (!identical(length(y),length(pre))) stop("Length not matched.") 
        tab <- table(y,pre) 
        if (!identical(dim(tab)[1],dim(tab)[2])) stop("pred and y have diff nlevels") 
        ad <- max(rowSums(tab))#max of row sum 
  
        #cal R2 
        ML.R2 <- 1-exp(-G2/n) 
        McFadden.R2 <- 1-(L.full/L.base) 
        McFadden.Adj.R2 <- 1-((L.full-mod$edf)/L.base) 
        Cragg.Uhler.R2 <- ML.R2/(1-exp(2*L.base/n)) 
        Count.R2 <- sum(diag(tab))/sum(tab) 
        Count.adj.R2 <- (sum(diag(tab))-ad)/(sum(tab)-ad) 
        
        #get the result 
        res<-list(LR=G2,df=df,LR.test.p =LR.test.p 
                  ,aic=aic,ML.R2=ML.R2,Cragg.Uhler.R2=Cragg.Uhler.R2,McFadden.R2 
                  =McFadden.R2 ,McFadden.Adj.R2=McFadden.Adj.R2,Count.R2=Count.R2,Count.adj.R2=Count.adj.R2) 
        
        #print the result 
        cat("\n", 
            paste(rep("-",21)), 
            "\n The Fitstats are : \n", 
            sprintf("G2(%d) = %f",df,G2), 
            " ,Prob ",format.pval(LR.test.p), 
            "\n",sprintf("AIC   = %f",aic), 
            sprintf(",ML.R2 = %f \n",ML.R2), 
            paste(rep("-",21)),"\n", 
            sprintf("Cragg.Uhler.R2  = %f \n",Cragg.Uhler.R2), 
            sprintf("McFadden.R2     = %f \n",McFadden.R2), 
            sprintf("McFadden.Adj.R2 = %f \n",McFadden.Adj.R2), 
            sprintf("Count.R2        = %f \n",Count.R2), 
            sprintf("Count.adj.R2    = %f \n",Count.adj.R2), 
            "\n Note:The maxinum of ML R2 is less than 1 \n", 
            paste(rep("-",21)),"\n") 
        invisible(res) 
        } 
496,480,662

#get raw R&D=========================================================================
  all.noest.mod = lm(npatappAdj~xrdAdj, data = reg.dat)
  rel.mod = lm(xrdAdj~state_co, data = reg.dat)
  #create aggregate plot
    testdat = reg.dat[,c("companyName", "xrdAdj")]
    testdat = na.exclude(testdat)
    avgSpend = aggregate(testdat$xrdAdj, by = list(companyName = testdat$companyName), FUN = mean)
    colnames(avgSpend)= c("companyName", "xrdAdj")
    testdat = reg.dat[,c("companyName", "state_co")]
    testdat = na.exclude(testdat)
    avgSpend2 = aggregate(testdat$state_co, by = list(companyName = testdat$companyName), FUN = mean)
    colnames(avgSpend2)= c("companyName", "state_co")
    plot.dat = merge(x = avgSpend, y = avgSpend2, by = "companyName", all.x = TRUE)
    plot(plot.dat$xrdAdj, plot.dat$state_co)
    plot.dat = plot.dat[496]

    testdat = reg.dat[,c("companyName", "npatappAdj")]
    testdat = na.exclude(testdat)
    avgSpend2 = aggregate(testdat$npatappAdj, by = list(companyName = testdat$companyName), FUN = mean)
    colnames(avgSpend2)= c("companyName", "npatappAdj")
    plot.dat2 = merge(x = plot.dat, y = avgSpend2, by = "companyName", all.x = TRUE)
    test = reg.dat[,c("companyName","industryName")]
    plot.dat2 = merge(x = plot.dat2, y = test, by = "companyName", all.x = TRUE)
    #remove duplicates
      plot.dat2 = plot.dat2[!duplicated(plot.dat2[,c('companyName')]),]
    test = lm(npatappAdj~xrdAdj, data = plot.dat2)
    test = lm(npatappAdj~xrdAdj +factor(industryName), data = plot.dat2)
    FREQTABLE = freq(ordered(RDDATA$sic), plot=FALSE) #many industries have over 100 entries
    FREQTABLE = data.frame(industryName = as.factor(rownames(FREQTABLE)), freq = FREQTABLE[,1])
    plot.dat3 = data.frame(merge(x = plot.dat2, y = FREQTABLE, by = "industryName", all.x = TRUE))
    plot.dat3 = na.exclude(plot.dat3)
    test = lm(npatappAdj~xrdAdj +factor(industryName), data = plot.dat3)

  #get companies with 15 entries only
    FREQTABLE = freq(ordered(companies.dat$companyName), plot=FALSE) #many industries have over 100 entries
    length(FREQTABLE[FREQTABLE[,1]>=10,][,1])
    length(FREQTABLE[FREQTABLE[,1]>=15,][,1])
    FREQTABLE = data.frame(companyName = as.factor(rownames(FREQTABLE)), freq = FREQTABLE[,1])
    moreFifteen.dat = data.frame(merge(x = companies.dat, y = FREQTABLE, by = "companyName", all.x = TRUE))
    moreFifteen.dat = moreFifteen.dat[moreFifteen.dat$freq >= 10,]
    regFifteen.dat = merge(x = moreFifteen.dat, y = industries.dat, by = "id", all.x = TRUE)
    colnames(regFifteen.dat) = c("id", "companyName", "industryName", "datayear", "state_co", "stError_co", "strictFlag_co", "companyid", "drop", "drop2","drop3", "state_ind", "stError_ind", "strictFlag_ind", "onlyWorkingFlag")
    regFifteen.dat= regFifteen.dat[,c(1:8, 12:15)]
    regFifteen.dat = merge(regFifteen.dat, RDDATA, by = "companyid", all.x = TRUE) #problem arises here!
    regFifteen.dat.nodeletes = regFifteen.dat

    modelList = levels(as.factor(regFifteen.dat$companyName))
    
  #run AR models

  #run state space models

  #check success
    

  #delete any entries where company or industry model was not estimated for these years
      regTen.dat = regTen.dat[!is.na(regTen.dat$state_co) & !is.na(regTen.dat$state_ind),]
    #delete missing LHS--in this reduced sample there are no missing LHS's
      regTen.dat = regTen.dat[!is.na(regTen.dat$npatappAdj),]
    #model
      all.mod = lm(npatappAdj~state_co + stError_co + state_ind + stError_ind, data = regTen.dat)

  #create panel data and test for lags
    panel.dat <- pdata.frame(reg.dat, index = c("companyName", "datayear"), drop.index = TRUE, row.names = TRUE)
    testdat = data.frame(rd = as.vector(panel.dat$xrdAdj), numPat = as.vector(panel.dat$npatappAdj))
    testdat = na.exclude(testdat)
    cor(x = testdat$rd, y = testdat$numPat, use = "complete.obs")
    numLags = 5
    laggedVars = lag(panel.dat$xrdAdj, 1:numLags)
    for (i in 1:numLags){
      testdat = data.frame(rd = as.vector(laggedVars[,i]), numPat = as.vector(panel.dat$npatappAdj))
      testdat = na.exclude(testdat)
      cor = cor(x = testdat$rd, y = testdat$numPat, use = "complete.obs")
      print(paste(i, cor, sep = ","))
    }

  #create AR models
  output.data = data.frame(matrix(ncol = 5, nrow = 0))
  colnames(output.data)= c("industryName", "companyName", "coeff1", "coeff2", "intercept")
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
          numYears = length(coData)
          #run model
            curMod = ar.ols(coData, AIC = TRUE, order.max = 2, intercept = TRUE, demean = FALSE, na.action = na.exclude)
            if (is.na(curMod$ar[1])){
              coeffs = c(NA, NA, NA)
            }
            else{
              coeffs = curMod$ar[[1]] 
              if(length(coeffs==1)){
                coeffs = c(coeffs, NA)
              }
              int = curMod$x.intercept
              coeffs = c(coeffs, int)
            }
        }
        else{
          coeffs = c(NA, NA, NA) 
        }
        cur.outdata =data.frame(industry= industryName,company= companyName, coeff1 = coeffs[1], coeff2 = coeffs[2], int = coeffs[3], stringsAsFactors = FALSE)
        colnames(cur.outdata)= colnames(output.data)
        output.data= rbind(output.data, cur.outdata)
      }
  }
  write.csv(output.data, "arout.csv")

  
