# Define a function to get the current directory of this file
library(tidyverse)
getCurrentFileLocation =  function()
{
  this_file = commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file = rstudioapi::getSourceEditorContext()$path
  }
  return(this_file)
}

# Name the current file's directory string BASE_DIR
BASE_DIR = dirname(getCurrentFileLocation())

# load the data frame
df = read.csv(paste(BASE_DIR, '/data/seizures_original.csv',sep=""))
times = sprintf('t%03d',seq(1,178))
colnames(df) = c('X',times,'y')

# First, we decide on a convention for dividing the data into training and test
# portions. Since the data are already randomized, we can simply take some proportion
# of the data for our training set. We use 85% of the data whenever we do exploratory 
# analysis to avoid over-fitting when making feature selections.
(n = length(df$X))
(m = round(0.85*n,0))

# Going forward, when doing feature selection, only the training set is considered
# in order to avoid over-optimizing selections

# Next, we compute the indexes of each response group
# This makes it easy to extract just those records from the data frame
seizure = c()
tumor = c()
offTumor = c()
eyesClosed = c()
eyesOpen = c()
for (i in seq(length(df$y))){
  if (df[i,180] == 1){
    seizure = c(seizure,i)
  }
  if (df[i,180] == 2){
    tumor = c(tumor,i)
  }
  if (df[i,180] == 3){
    offTumor = c(offTumor,i)
  }
  if (df[i,180] == 4){
    eyesClosed = c(eyesClosed,i)
  }
  if (df[i,180] == 5){
    eyesOpen = c(eyesOpen,i)
  }
}

###
# INITIAL DATA SUMMARIZATION
# Next, we use the indexes to compute basic summary statistics for each response group

# compute seizure summary
seizure_total_vector = c()
for (i in seizure){
  seizure_total_vector = c(seizure_total_vector,t(df[i,2:179]))
}
seizure_total_summary = summary(seizure_total_vector)

# compute tumor summary
tumor_total_vector = c()
for (i in tumor){
  tumor_total_vector = c(tumor_total_vector,t(df[i,2:179]))
}
tumor_total_summary = summary(tumor_total_vector)

# compute off tumor summary
offTumor_total_vector = c()
for (i in offTumor){
  offTumor_total_vector = c(offTumor_total_vector,t(df[i,2:179]))
}
offTumor_total_summary = summary(offTumor_total_vector)

# compute eyes closed summary
eyesClosed_total_vector = c()
for (i in eyesClosed){
  eyesClosed_total_vector = c(eyesClosed_total_vector,t(df[i,2:179]))
}
eyesClosed_total_summary = summary(eyesClosed_total_vector)

# compute eyes open summary
eyesOpen_total_vector = c()
for (i in eyesOpen){
  eyesOpen_total_vector = c(eyesOpen_total_vector,t(df[i,2:179]))
}
eyesOpen_total_summary = summary(eyesOpen_total_vector)

# finally, we bind all the summaries together and
# print a report table with all the values
group_summary = rbind(seizure_total_summary,tumor_total_summary,
                      offTumor_total_summary,eyesClosed_total_summary,
                      eyesOpen_total_summary)
rownames(group_summary) = c("1 seizure","2 tumor","3 off tumor",
                            "4 eyes closed", "5 eyes open")
Var. = c(var(seizure_total_vector),
         var(tumor_total_vector),
         var(offTumor_total_vector),
         var(eyesClosed_total_vector),
         var(eyesOpen_total_vector))
group_summary = cbind(group_summary,Var.)
round(group_summary,2)


###
# Next, we plot a sample of epochs from each group (for appendix A)

# We plot a 2x2 figure with 4 seizure epochs
par(mfrow=c(2,2))
plot(seq(0,177)/178,df[seizure[150],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[seizure[5],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[seizure[15],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[seizure[30],2:179],xlab="time",ylab="EEG reading",type="l")

# We repeat the plot for the tumor group
par(mfrow=c(2,2))
plot(seq(0,177)/178,df[tumor[150],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[tumor[5],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[tumor[15],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[tumor[30],2:179],xlab="time",ylab="EEG reading",type="l")

# We repeat the plot for the off tumor group
par(mfrow=c(2,2))
plot(seq(0,177)/178,df[offTumor[150],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[offTumor[5],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[offTumor[15],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[offTumor[30],2:179],xlab="time",ylab="EEG reading",type="l")

# We repeat the plot for the eyes closed group
par(mfrow=c(2,2))
plot(seq(0,177)/178,df[eyesClosed[150],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[eyesClosed[5],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[eyesClosed[15],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[eyesClosed[30],2:179],xlab="time",ylab="EEG reading",type="l")

# We repeat the plot for the eyes open group
par(mfrow=c(2,2))
plot(seq(0,177)/178,df[eyesOpen[150],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[eyesOpen[5],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[eyesOpen[15],2:179],xlab="time",ylab="EEG reading",type="l")
plot(seq(0,177)/178,df[eyesOpen[30],2:179],xlab="time",ylab="EEG reading",type="l")


###
# DATA TRANSFORMATION
# Next, for each epoch, we extract the features we will be using for the regression analysis,
# starting with the conventional statistical parameters (mean and variance)

# First, we extract the mean
mean = c()
for (i in seq(length(df$X))){
  mean = c(mean, mean(t(df[i,2:179])))
}

# Next, we extract the variance
variance = c()
for (i in seq(length(df$X))){
  variance = c(variance, var(t(df[i,2:179])))
}

# Next, we extract features related to autocorrelation
# These features measure how strongly neurons are locked in phase with
# each other.

# First, we define a function for computing autocorrelation for different
# displacement amounts.
autocor = function(n,df){
  autocor_n = c()
  for (i in seq(length(df$X))){
    ts = zoo::as.zoo(t(df[i,2:179]))
    ts_n <- lag(ts, k=-n, na.pad=T)
    autocor_n = c(autocor_n,cor(ts[!is.na(ts_n)], ts_n[!is.na(ts_n)]))
  }
  return(autocor_n)
}

# Next, we compute the autocorrelation at a sequence of levels (1-15) and
# check how these values correlate by computing a correlation matrix between them.
# We also look at the mean values at each level. We pick three autocorrelation
# values that are fairly un-correlated yet have mean values that aren't too small.

ac1 = autocor(1,df) # ***   Do not run  *** 
ac2 = autocor(2,df) # *** Use CSV below ***
ac3 = autocor(3,df)
ac4 = autocor(4,df)
ac5 = autocor(5,df)
ac6 = autocor(6,df)
ac7 = autocor(7,df)
ac8 = autocor(8,df)
ac9 = autocor(9,df)
ac10 = autocor(10,df)
ac11 = autocor(11,df)
ac12 = autocor(12,df)
ac13 = autocor(13,df)
ac14 = autocor(14,df)
ac15 = autocor(15,df)

# We bind the autocorrelation values for each epoch into a data frame
AC = data.frame(cbind(ac1,ac2,ac3,ac4,ac5,ac6,ac7,ac8,
                ac9,ac10,ac11,ac12,ac13,ac14,ac15))

# and write it to a CSV file so we don't have to re-compute these values
AC_write_path = paste(BASE_DIR, "/data/AC.csv")
write.csv(AC,file = AC_write_path)

# *** Run from here ***
# after this point, we use the read.csv commant to 

AC_path = paste(BASE_DIR, "/data/AC.csv")
AC = read.csv(AC_path,sep=",",header=TRUE)[2:16]
length(AC$ac1)

# In order to avoid bad statistics, I only did the exploratory analysis of autocorrelation
# on a training subset. 
AC_train = AC[1:m,]

# We compute the covariance matrix

AC_train__cor = cor(AC_train_)

# and the mean values

(AC_train__mean = colMeans(AC_train_))

# we compute AC_train_ means by group to see the overall profile
AC_train_mbg = rbind(AC_train__mean,
               colMeans(AC_train_[seizure,]),
               colMeans(AC_train_[tumor,]),
               colMeans(AC_train_[offTumor,]),
               colMeans(AC_train_[eyesClosed,]),
               colMeans(AC_train_[eyesOpen,]))
rownames(AC_train_mbg) = c("all","seizure","tumor","off.tumor","eyes.closed","eyes.open")

round(AC_train_mbg,2)

# Next, we compute the variances of eAC_train_h column of the AC_train_ group means in order
# to see which columns most differentiate the groups
AC_train_var = c()
for (j in seq(15)){
  AC_train_var = c(AC_train_var,var(AC_train_mbg[,j]))
}

# We compare the variances and the correlations in order to make a choice about
# which three fAC_train_tors to include.
round(AC_train_var,2)

round(AC_train__cor,2)

AC_train__index = c(3,8,15)

round(cor(AC_train_[,AC_train__index]),2)
AC_train_mbg[,AC_train__index]

# Even though ac3 and ac8 are highly correlated (0.7), I decided to use
# ac3, ac8 and ac15 based on the tradeoff between correlation and variance

ac3 = AC$ac3
ac8 = AC$ac8
ac15 = AC$ac14

# We also extract the Hurst exponent
# This estemates the rate at which autocorrelation decays with displacement
# It may turn out to be redundant, but I wanted to have it as an alternative
# option to the autocorrelation profile selected above
library(pracma)
H = c()
mean = c()
for (i in seq(length(df$X))){
  H = c(H, hurstexp(t(df[i,2:179]),display=FALSE)$Hrs)
}


###
# Next, we extract average frequency band intensities from each epoch
# These frequency band quantities may not be intuitive to the lay person, but
# they have strong interpretations in the literature on EEG. They are considered
# to be independent bandwidths on which neurons exchange long distance signals,
# kind of like how radio signals on different bandwidths share the same space.
Delta = c()
Theta = c()
Alpha = c()
Beta = c()
Gamma = c()
for (i in seq(length(df$X))){
  spec = spectrum(t(df[i,2:179]),plot=FALSE)$spec
  Delta = c(Delta,mean(spec[45:90])) # 0 to 4 Hz
  Theta = c(Theta,mean(spec[23:44])) # 4 to 8 Hz
  Alpha = c(Alpha,mean(spec[15:22])) # 8 to 12 Hz
  Beta = c(Beta,mean(spec[7:15]))    # 12 to 30 Hz
  Gamma = c(Gamma,mean(spec[4:6]))   # 30 to 45 Hz
}

###
# Lastly, we compute the fractal dimension using the box count method.
# this quantity might help capture the jaggedness in some of the EEG readings
# compared to others.
library(fractaldim)
fD = c()
mean = c()
for (i in seq(length(df$X))){
  fD = c(fD, fd.estimate(as.vector(t(df[i,2:179])),method='boxcount')$fd)
}


###
# Finally, we write our feature data to a CSV file to save for modeling analysis
fdata = cbind(mean,variance,fD,H,ac3,ac8,ac15,Delta,Theta,Alpha,Beta,Gamma)
fd_write_path = paste(BASE_DIR,  "/data/fdata.csv")
write.csv(fdata, file = fd_write_path)

###
# END
# The analysis is conducted in a separate script.
