#'This package was built by Yunhan DU(1270950448@qq.com),from Southwestern University of Finance and Economics,China,Sichuan Province.
#'it can solve the problem that some economic data has NAs.
#'
#'Details:
#' linput is a generic function used to proceed with the NA values of panel data.
#' The function invokes particular methods (only interpolation or both extrapolation and interpolation) which depends on the value of the fifth argument m.
#'1.This package was built to solve the problem that some economic data has NAs by using linear input(method:interpolation) formula and average growth rate per annum(method:extrapolation) to input NAs.
#'2.For this package named linput, it can input time series variable of an individual with at least two values, if the variable has no values or only one value, Error information would be presented.
#'3.The time series variable of individuals with input would be saved in a temporary R dataframe named test_data2, The input variable would be named as testemploy of test_data2,and the input value also would be saved in a new R value named estemploy_i, i depends on the number of times you run this package linput. Every time you run this package to proceed your data, the input result would be saved in a new R value named testemployi(i=1,2,...) automaticly.
#'
#' @param d an dataframe object for which a panel data has some NA values.
#' @param p The variable which indicate the individual.
#' @param y The variable which indicate time series variable of an individual.
#' @param x The variable which indicate the variable has NA values and need to be input.
#' @param m m = 0/1,indicating only 'interpolation method'/both 'extrapolation and interpolation method' would be chosen.
#' @export
#' @examples
#' linput(user_data,user_data$province,user_data$year,user_data$income,m = 1)
#' linput(user_data,user_data$province,user_data$year,user_data$income,m = 0)
linput <- function(d,p,y,x,m){
  attr_temp <<- attributes(x)
  extrapolate <<- m
  if(extrapolate != 0 & extrapolate != 1){
    cat("Error:The method parameter inputed must be 0 or 1!!!")
  }
  if(extrapolate == 0 | extrapolate == 1){
    if (is.element('tidyverse', installed.packages()[,1]) == FALSE) {
      install.packages("tidyverse")
    } else if (is.element('tidyverse', installed.packages()[,1]) == TRUE){
      cat("Note:This package #Linear_inputed_Duyunhan# need the package #tidyverse#'s","\n",
          "supporting,now the package 'tidyverse' has already been installed.","\n")
    }
    library(tidyverse)
    library(plyr)
    test_data2 <<- d
    test_data2$individual <<- p
    test_data2$year <<- y
    test_data2$testemploy <<- x
    test_data2$individualID <<- c(as.factor(p))
    jmax <<- max(test_data2$individualID,na.rm = T)
    max_year <<- max(test_data2$year,na.rm = T)-min(test_data2$year,na.rm = T)+1
    insert_i <<- length(test_data2$individual)+1
    test_data2 <<- add_row(test_data2)
    step_long <<- max_year
    test_data2 <<- select(test_data2,-individualID)
    test_data2 <<- arrange(test_data2,individual,year)
    for (j in 1:jmax){
      for (q in 1:max_year){
        test_individual <<- test_data2$individual[1 + (j-1)*step_long]
        test_year <<- min(test_data2$year,na.rm = T)+q-1
        if(test_data2$individual[q + (j-1)*step_long] != test_individual | test_data2$year[q + (j-1)*step_long] != test_year){
          test_data2$individual[insert_i] <<- test_individual
          test_data2$year[insert_i] <<- test_year
          insert_i <<- insert_i +1
          test_data2 <<- add_row(test_data2)
          test_data2 <<- arrange(test_data2,individual,year)
        }
      }
    }
    test_data2 <<- filter(test_data2,!is.na(test_data2$individual))
    test_data2$individualID <<- c(as.factor(test_data2$individual))
    test_data2 <<- arrange(test_data2,individual,year,individualID)
    test_data2$individualID2 <<- 1:length(test_data2$individual)
    test_data2 <<- arrange(test_data2,individual,year,individualID,individualID2)
    step_long <<- which.max(test_data2$year)
    test_data2$individualID3 <<- test_data2$individualID2 - (test_data2$individualID-1)*step_long
    if(!exists("countttt")){
    inputed_data <<- select(test_data2,individual,year)
    }
    test_data2 <<- select(test_data2,individual,year,testemploy,individualID,individualID2,individualID3)
    maxi <<- max(test_data2$individualID3)
    maxk <<- max(test_data2$individualID)
    for(k in 1:maxk){
      step_long <<-  (k-1)*maxi
      for(i in 1:maxi)
      {
        if(i+2+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & !is.na(test_data2$testemploy[i+2+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] +
            (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+2+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+2+step_long]-test_data2$year[i+step_long])
        }
        if(i+3+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & !is.na(test_data2$testemploy[i+3+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] +
            (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+3+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+3+step_long]-test_data2$year[i+step_long])
        }
        if(i+4+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & is.na(test_data2$testemploy[i+3+step_long]) & !is.na(test_data2$testemploy[i+4+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] +
            (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+4+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+4+step_long]-test_data2$year[i+step_long])
        }
        if(i+5+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & is.na(test_data2$testemploy[i+3+step_long]) & is.na(test_data2$testemploy[i+4+step_long])
           && !is.na(test_data2$testemploy[i+5+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] + (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+5+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+5+step_long]-test_data2$year[i+step_long])
        }
        if(i+6+step_long < maxi+1+step_long && !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & is.na(test_data2$testemploy[i+3+step_long]) & is.na(test_data2$testemploy[i+4+step_long])
           & is.na(test_data2$testemploy[i+5+step_long]) & !is.na(test_data2$testemploy[i+6+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] + (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+6+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+6+step_long]-test_data2$year[i+step_long])
        }
        if(i+7+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & is.na(test_data2$testemploy[i+3+step_long]) & is.na(test_data2$testemploy[i+4+step_long])
           & is.na(test_data2$testemploy[i+5+step_long]) && is.na(test_data2$testemploy[i+6+step_long]) & !is.na(test_data2$testemploy[i+7+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] + (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+7+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+7+step_long]-test_data2$year[i+step_long])
        }
        if(i+8+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & is.na(test_data2$testemploy[i+3+step_long]) & is.na(test_data2$testemploy[i+4+step_long])
           & is.na(test_data2$testemploy[i+5+step_long]) & is.na(test_data2$testemploy[i+6+step_long]) & is.na(test_data2$testemploy[i+7+step_long]) & !is.na(test_data2$testemploy[i+8+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] + (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+8+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+8+step_long]-test_data2$year[i+step_long])
        }
        if(i+9+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & is.na(test_data2$testemploy[i+3+step_long]) & is.na(test_data2$testemploy[i+4+step_long])
           & is.na(test_data2$testemploy[i+5+step_long]) & is.na(test_data2$testemploy[i+6+step_long]) & is.na(test_data2$testemploy[i+7+step_long]) & is.na(test_data2$testemploy[i+8+step_long]) & !is.na(test_data2$testemploy[i+9+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] + (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+9+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+9+step_long]-test_data2$year[i+step_long])
        }
        if(i+10+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & is.na(test_data2$testemploy[i+3+step_long]) & is.na(test_data2$testemploy[i+4+step_long])
           & is.na(test_data2$testemploy[i+5+step_long]) & is.na(test_data2$testemploy[i+6+step_long]) & is.na(test_data2$testemploy[i+7+step_long]) & is.na(test_data2$testemploy[i+8+step_long]) & is.na(test_data2$testemploy[i+9+step_long]) && !is.na(test_data2$testemploy[i+10+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] + (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+10+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+10+step_long]-test_data2$year[i+step_long])
        }
        if(i+11+step_long < maxi+1+step_long & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long]) & is.na(test_data2$testemploy[i+2+step_long]) & is.na(test_data2$testemploy[i+3+step_long]) & is.na(test_data2$testemploy[i+4+step_long])
           & is.na(test_data2$testemploy[i+5+step_long]) & is.na(test_data2$testemploy[i+6+step_long]) & is.na(test_data2$testemploy[i+7+step_long]) & is.na(test_data2$testemploy[i+8+step_long]) & is.na(test_data2$testemploy[i+9+step_long]) & is.na(test_data2$testemploy[i+10+step_long]) & !is.na(test_data2$testemploy[i+11+step_long])){
          test_data2$testemploy[i+1+step_long] <<- test_data2$testemploy[i+step_long] + (test_data2$year[i+1+step_long]-test_data2$year[i+step_long])*(test_data2$testemploy[i+11+step_long]-test_data2$testemploy[i+step_long])/(test_data2$year[i+11+step_long]-test_data2$year[i+step_long])
        }
      }
    }
    for(k in 1:maxk){
      step_long <<- (k-1)*maxi
      fir_temp <<- NA
      fir_temp_year <<- NA
      end_temp <<- NA
      end_temp_year <<- NA
      if(!is.na(test_data2$testemploy[1+step_long])){
        fir_temp <<- test_data2$testemploy[1+step_long]
        fir_temp_year <<- test_data2$year[1+step_long]
      }
      if(!is.na(test_data2$testemploy[maxi+step_long])){
        end_temp <<- test_data2$testemploy[maxi+step_long]
        end_temp_year <<- test_data2$year[maxi+step_long]
      }
      for(i in 2:(maxi-1)){
        if(is.na(fir_temp) & is.na(test_data2$testemploy[i-1+step_long]) & !is.na(test_data2$testemploy[i+step_long])){
          fir_temp <<- test_data2$testemploy[i+step_long]
          fir_temp_year <<- test_data2$year[i+step_long]
        }
        if(is.na(end_temp) & !is.na(test_data2$testemploy[i+step_long]) & is.na(test_data2$testemploy[i+1+step_long])){
          end_temp <<- test_data2$testemploy[i+step_long]
          end_temp_year <<- test_data2$year[i+step_long]
        }
      }
      if((!is.na(end_temp_year) & !is.na(fir_temp_year)) & (fir_temp_year == end_temp_year) ){
        cat("\n","Warning:The",k)
        cat("th individual has only one value existed!!!","\n")
      }
      if(is.na(end_temp_year) & !is.na(fir_temp_year)){
        cat("\n","Warning:The",k)
        cat("th individual has only one value existed!!!","\n")
      }
      if(!is.na(end_temp_year) & is.na(fir_temp_year)){
        cat("\n","Warning:The",k)
        cat("th individual has only one value existed!!!","\n")
      }
      if(is.na(end_temp) & is.na(fir_temp)){
        cat("\n","Warning:The",k)
        cat("th individual has no value existed!!!","\n")
      }
      if(extrapolate == 1){
        if((!is.na(end_temp_year) & !is.na(fir_temp_year)) & (fir_temp_year != end_temp_year)){
          growth <<- (end_temp/fir_temp)^(1/(end_temp_year-fir_temp_year))-1
        }else if((!is.na(end_temp_year) & !is.na(fir_temp_year)) & (fir_temp_year == end_temp_year)){
          growth <<- 0
        }else if(is.na(end_temp_year) & is.na(fir_temp_year)){
          growth <<- 0
        }else if(!is.na(end_temp_year) & is.na(fir_temp_year)){
          growth <<- 0
        }else if(is.na(end_temp_year) & !is.na(fir_temp_year)){
          growth <<- 0
        }
        for(i in 1:maxi){
          if(growth!=0 && test_data2$year[i+step_long]<fir_temp_year & is.na(test_data2$testemploy[i+step_long])){
            test_data2$testemploy[i+step_long] <<- fir_temp/((1+growth)^(fir_temp_year-test_data2$year[i+step_long]))
          }
          if(growth!=0 && test_data2$year[i+step_long]>end_temp_year & is.na(test_data2$testemploy[i+step_long])){
            test_data2$testemploy[i+step_long] <<- end_temp*((1+growth)^(test_data2$year[i+step_long]-end_temp_year))
          }
        }
      }
    }

    test_data2 <<- select(test_data2,-individualID3,-individualID2)
    if(!exists("countttt")){
      countttt <<- 1
    }
  
    if(extrapolate == 1){
      cat("\n","inputed data method:interpolation and extrapolation ","\n")
    }else if(extrapolate !=1){
      cat("\n","inputed data method:interpolation without extrapolation ","\n")
    }
    cat("\n","Rsults Note:The inputed value has been stored in the R Data inputed_data and R value named testemploy'i',","\n")
    cat("You can use this package again and again without any conflict,","\n")
    cat("because its inputed value will be stored in the next R value named 'testemploy'i+1' automatically.","\n")
    cat("Here,'i' means the number of times you use the package 'Linear_inputed_Duyunhan'.","\n")
    cat("In the linpu 3.0 version,the variable of original data's attribution can be transferred to corresponding inputed_value,","\n")
    cat("So You'd better give the variable of original data a attribution,to identify different inputed_value.","\n")
  }

  rm(end_temp,envir=.GlobalEnv)
  rm(end_temp_year,envir=.GlobalEnv)
  rm(extrapolate,envir=.GlobalEnv)
  rm(fir_temp,envir=.GlobalEnv)
  rm(fir_temp_year,envir=.GlobalEnv)
  rm(growth,envir=.GlobalEnv)
  rm(insert_i,envir=.GlobalEnv)
  rm(jmax,envir=.GlobalEnv)
  rm(max_year,envir=.GlobalEnv)
  rm(maxi,envir=.GlobalEnv)
  rm(maxk,envir=.GlobalEnv)
  rm(step_long,envir=.GlobalEnv)
  rm(test_individual,envir=.GlobalEnv)
  rm(test_year,envir=.GlobalEnv)
  
  assign(paste("testemploy",countttt,sep=""),test_data2$testemploy,pos=.GlobalEnv)
  inputed_data[ncol(inputed_data)+1] <<- get(paste("testemploy",countttt,sep=""))
  names(inputed_data)[ncol(inputed_data)] <<- paste("inputed_value",countttt,sep="")
  attributes(inputed_data[[ncol(inputed_data)]]) <<- attr_temp
  countttt <<- countttt + 1
  rm(test_data2,envir=.GlobalEnv)
  rm(attr_temp,envir=.GlobalEnv)
  View(inputed_data)
  }





