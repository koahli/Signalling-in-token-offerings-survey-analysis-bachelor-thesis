#User defined functions functions for the analysis

#takes two data frames with the same columns, the data frames should be two different groups, the first dataset will be the yes group and the second the no group
#the data frames will be combined with column names from 1 to n, with 1 being the group column
#all -1 entires will be replaced with NA entries
#the return is a list with the combined data frame on the first position, ds1 (WITHHOUT yes in the first column) and with NAs instead of -1 and the same for ds2 on the third position
combine_ds_before_levene <- function(ds1, ds2){
  out <- list()
  
  ds1[ds1 == -1] <- NA #replaces every -1 with a NA
  ds2[ds2 == -1] <- NA #replaces every -1 with a NA
  out$ds1 <- ds1
  out$ds2 <- ds2
  
  yes <- rep('Yes', times = nrow(ds1))
  ds1 <- cbind(yes, ds1)
  
  no <- rep('No', times = nrow(ds2))
  ds2 <- cbind(no, ds2)
  
  colnames(ds1) <- 1:ncol(ds1)
  colnames(ds2) <- 1:ncol(ds2)
  
  result <- rbind(ds1, ds2)
  
  out$result <- result
  
  return(out)
}

#takes a data frame with a group in the first column and at least one other column, the groups will be tested for homogene variance
#returns heterogene variance when in at least one column the p-value is grater 0.05 which means the alternative hypothesis heterogene variance is true for this column 
leveneTestOnDS <- function(d){
  heterogene_counter <- 0
  for(i in 2:ncol(d)) {
    #browser() 
    lv <- leveneTest(d[,i] ~ d[,1], data = d)
    if(lv[1,3] > 0.05){
      heterogene_counter <- heterogene_counter + 1
    }
  }
  print(heterogene_counter)
}

#takes a vector with likert like data (e.g. from 1 to 5) and -1 for i don't know / not specified
#returns a vector with the average of the answers but without the "i don't know"-data used for the average on the first position and the percentage if the "i don't know"-answers on the second position
lik_avg_idk <- function(v) {
  idk <- 0
  sum <- 0
  for(i in 1:length(v)) {
    if(v[i] == -1){
      idk <- idk + 1
    }else {
      sum <- sum + v[i]
    }
  }
  avg <- sum / (length(v) - idk)
  idk_perc <- idk / length(v)
  return <- c(avg, idk_perc) #The return value of a function is the last expression in the function body to be evaluated.
}

#takes a data frame with likert like data (e.g. from 1 to 5) and -1 for i don't know / not specified (it should not contain factors)
#returns a data frame which has the return from a row for every column from the input data set with its average and percentage of idk (see lik_avg_idk)
lik_ds <- function(ds){
  data1 <- ds
  columns <- c("avg", "idk_perc")
  result <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(result) = columns # assign column names
  
  for(i in 1:ncol(data1)) {       # for-loop over columns
    result[i , ] <- lik_avg_idk(data1[ , i])
    #browser()
  }
  
  result
} 

#takes a data frame with likert like data (e.g. from 1 to 5) and -1 for i don't know / not specified (it should not contain factors)
#returns a data frame with the t-statistic, degrees of freedom, p-value, estimated mean and significance code (see below), it is calculated for the alternative hypothesis greater than 3
#***                 [0, 0.001]
#**              (0.001, 0.01]
#*               (0.01, 0.05]
#.                (0.05, 0.1]
#x                 (0.1, 1]
compare_with_3 <- function(ds){
  data1 <- ds
  data1[data1 == -1] <- NA #replaces every -1 with a NA
  
  columns <- c("t-statistic", "degrees_of_freedom", "p-value", "estimated_mean", "significance_code")
  result <- data.frame(matrix(nrow = 0, ncol = length(columns)))
  colnames(result) = columns # assign column names
  
  for(i in 1:ncol(data1)) {       # for-loop over columns
    t <- t.test(x = data1[ , i], mu =  3, alternative = "greater")
    result[i , 1] <- t$statistic
    result[i , 2] <- t$parameter
    result[i , 3] <- t$p.value
    result[i , 4] <- t$estimate
    
    if(t$p.value <= 0.001){
      result[i , 5] <- '***'
    } else if(t$p.value <= 0.01){
      result[i , 5] <- '**'
    } else if(t$p.value <= 0.05){
      result[i , 5] <- '*'
    } else if(t$p.value <= 0.1){
      result[i , 5] <- '.'
    } else{
      result[i , 5] <- 'x'
    }
    
    #browser()
  }
  
  result
} 