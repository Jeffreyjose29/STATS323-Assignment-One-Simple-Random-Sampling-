#set plot dimensions
par(mfrow = c(2,3))

#check working directory
getwd()
#set to location wanting to save files to
setwd("C:/Users/Jeffrey Jose/Desktop/STATS AssignmentOne/Populations")
##Load separate datasets
SampU.df = t(read.table("SampU.csv",header = FALSE,sep = ","))
SampU5.df = t(read.table("SampU5.csv",header = FALSE,sep = ","))
SampN.df = t(read.table("SampN.csv",header = FALSE,sep = ","))
SampN2.df = t(read.table("SampN2.csv",header = FALSE,sep = ","))
SampN5.df = t(read.table("SampN5.csv",header = FALSE,sep = ",")) 
SampL.df = t(read.table("SampL.csv",header = FALSE,sep = ",")) 
SampBern.df = t(read.table("SampBern.csv",header = FALSE,sep = ",")) 
SampChi21.df = t(read.table("SampChi21.csv",header = FALSE,sep = ","))
SampChi25.df = t(read.table("SampChi25.csv",header = FALSE,sep = ","))
SampChi210.df = t(read.table("SampChi210.csv",header = FALSE,sep = ",")) 
SampPoiss1.df = t(read.table("SampPoiss1.csv",header = FALSE,sep = ",")) 
SampPoiss5.df = t(read.table("SampPoiss5.csv",header = FALSE,sep = ","))
##put all datasets into one matrix (could have used all file here instead)
matrix = cbind(SampU.df,SampU5.df,SampN.df,SampN2.df,SampN5.df,SampL.df,SampBern.df, SampChi21.df,SampChi25.df,SampChi210.df,SampPoiss1.df,SampPoiss5.df)
colnames(matrix) = cbind("SampU","SampU5","SampN","SampN2","SampN5","SampL","SampBern", "SampChi21","SampChi25","SampChi210","SampPoiss1","SampPoiss5")

###QUESTION 1###
#declaring variables and setting to null (incase need to run again)
means = NULL
vars = NULL
totals = NULL

for(i in 1:12){
  #calc mean var and totals for each population and put into vector
  means = c(means,mean(matrix[,i]))
  vars = c(vars,var(matrix[,i]))
  totals = c(totals,sum(matrix[,i]))
  #plot histograms of populations
  hist(matrix[,i], main=substitute(paste('Histogram of ', matrix[a]), list(a=colnames(matrix)[i])))
}

#sanity check the outputs
means
vars
totals

#create matrix of all stats`                                                                `
Q1 = cbind(means,vars,totals)
#set row names to stats for that population
row.names(Q1) = rbind("SampU","SampU5","SampN","SampN2","SampN5","SampL","SampBern","SampChi21","SampChi25","SampChi210","SampPoiss1","SampPoiss5")
#write csv file
write.csv(Q1, "Question1.csv", row.names = TRUE)
#sanity check the outputs. Compare head to top of csv file
class(matrix)
dim(matrix)
head(matrix)

###QUESTION 2 AND 3###

#declaring variables and assigning values
d = .25
z = 1.645
N = 10000
#variables to be used within the loop
n0 = NULL
n = NULL
sampSize = NULL

#calculate req sample size for each population
for(i in 1:12){
  #n = N/(1+((((d)^2)*N)/(Q1[i,2]*z)))
  n0 = (Q1[i,2]*z^2)/(d^2)
  n = n0*(1/(1+(n0/N)))
  #created vector with required sample size
  sampSize = rbind(sampSize,ceiling(n))
}
row.names(sampSize) = row.names(Q1)
write.csv(sampSize, "Question2.csv", row.names = TRUE)

###QUESTION 3###
sampMeans = mat.or.vec(50, 12)
#sampVariance = mat.or.vec(50, 12)
#varianceDiff = mat.or.vec(50, 12)
diffs = mat.or.vec(50, 12)

#Estimate the sample means from sample of each of 12 populations, 50 times each
for(i in 1:12){
  for(j in 1:50){
    tempSample = sample(matrix[,i], sampSize[i], replace = FALSE, prob = NULL)
    sampMeans[j, i] = mean(tempSample)
    #get differences between pop and sample means
    diffs[j,i] = abs(sampMeans[j,i] - means[i])
  }
}

hist(sampMeans[,9])
hist(sampMeans[,10])

##Q-Q plot for the sample means of each population (done manually)
qqnorm(sampMeans[, 12], pch = 1, frame = FALSE)
qqline(sampMeans[, 12], col = "blue", lwd = 2)

skewness(sampMeans[, 12])
#Estimate the sample variance from sample of each of 12 populations, 50 times each
sampVariance = mat.or.vec(50, 12)
varianceDiff = mat.or.vec(50, 12)

for(i in 1:12){
  for(j in 1:50){
    tempSample = sample(matrix[,i], sampSize[i], replace = FALSE, prob = NULL)
    sampVariance[j, i] = var(tempSample)
    #get differences between pop and sample means
    varianceDiff[j,i] = abs(sampVariance[j,i] - vars[i])
  }
}

hist(sampVariance[, 7])
skewness(sampVariance[, 1])
skewness(sampVariance[, 2])
skewness(sampVariance[, 3])
skewness(sampVariance[, 4])
skewness(sampVariance[, 5])
skewness(sampVariance[, 6])
skewness(sampVariance[, 7])
skewness(sampVariance[, 8])
skewness(sampVariance[, 9])
skewness(sampVariance[, 10])
skewness(sampVariance[, 11])
skewness(sampVariance[, 12])

library(gplots)
plotmeans(sampMeans[,1], SampU.df, frame = FALSE, mean.labels = TRUE, connect = FALSE)

###QUESTION 4###

#declaring variables and assigning values
d = 0.005
z = 1.645
N = 10000
#variables to be used within the loop
n0 = NULL
n = NULL
sampSize = NULL
sampTotal = NULL

#calculate req sample size for each population
for(i in 1:12){
  #n = N/(1+((((d)^2)*N)/(Q1[i,3]*z)))
  n0 = (Q1[i,2]*z^2)/(d^2)
  # if(n0/N <= 0.1){
  #   n = n0
  # } else {
    n = n0*(1/(1+(n0/N))) 
  #}
  #created vector with required sample size
  sampSize = rbind(sampSize,ceiling(n))
}
row.names(sampSize) = row.names(Q1)
write.csv(sampSize, "Question4Part2.csv", row.names = TRUE)

sampTotal = mat.or.vec(50, 12)
#sampVariance = mat.or.vec(50, 12)
#varianceDiff = mat.or.vec(50, 12)
totalDiffs = mat.or.vec(50, 12)

#Estimate the sample means from sample of each of 12 populations, 50 times each
for(i in 1:12){
  for(j in 1:50){
    temp1Sample = sample(matrix[,i], sampSize[i], replace = FALSE, prob = NULL)
    sampTotal[j,i] = sum(temp1Sample)
    #get differences between pop and sample means
    totalDiffs[j,i] = abs(sampTotal[j,i] - totals[i])
  }
}

hist(sampTotal[, 12])

##Q-Q plot for the sample means of each population (done manually)
qqnorm(sampTotal[, 3], pch = 1, frame = FALSE)
qqline(sampTotal[, 3], col = "blue", lwd = 2)

library(e1071) 
skewness(sampMeans[,12])
