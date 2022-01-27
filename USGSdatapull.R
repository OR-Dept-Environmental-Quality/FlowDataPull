#playing around with USGS data in R

library(dataRetrieval)

#step 1 - identify site of interest by its site ID

#north santiam river at Niagra (14181500)

siteNumbers<-c("14181500","11501000","11486990","11502500","14211542")

siteInfo<-readNWISsite(siteNumbers)

#figure out what data is available for the site

whatdat<-whatNWISdata(siteNumber=siteNumbers, service="dv")


#get the daily data for a site - must specify parameter, do last 10 years

#parameters of interest
#00010 Temperature, water (deg C)
#00060 Discharge, cfs -aka daily mean stream flow

#statistical codes of interest
#00001 = maximum
#00002 = minimum
#00003 = mean
#00008 = median

dailydis<-readNWISdv(siteNumber="14181400", parameterCd="00060", startDate = (Sys.Date()-3650), statCd=c('00001','00002','00003',"00008"))

dailytemp<-readNWISdv(siteNumber=siteNumbers, parameterCd="00010", startDate=(Sys.Date()-3650), statCd=c('00001','00002','00003',"00008"))

#let's look at what data we can get for velocity and stream depth - 
#lots of different parameter codes that might fit the bill, so let's look at data and see what we have
velocparam<-c("00055","70232", '72149', '72168',"72169","72190","72254","72255","72294","72321",
                            "72322","72323","81380","81904"
                            )
velocinfo<-readNWISpCode(velocparam)
  
depthparam<-c("00064","72178","72199","82903", "85310","85311")
depthinfo<-readNWISpCode(depthparam)

#see if we can get all the NWIS sites in Oregon - 
#when I refine search to just surface water (no ocean) I get about 6,910 sites
nwisor<-whatNWISsites(stateCd="OR",siteType=c("ES","LK","SP","ST"))

#what data is there for velocity - very little (only 50 stations in all)
orveloc<-whatNWISdata(stateCd="OR",parameterCd=velocparam)

#what data is there for depth - more data, but not great (490 stations in all - some of it rather old)
ordepth<-whatNWISdata(stateCd="OR",parameterCd=depthparam)

#compared with discharge? - have discharge data at 1,708 stations
ordis<-whatNWISdata(stateCd="OR",parameterCd="00060")


#get some data - can't through NWIS - must go through WQP...
dailyveloc<-readWQPqw(siteNumber="USGS-11502500", parameterCd=velocparam)
dailydepth<-readWQPqw(siteNumber='USGS-14206690', parameterCd=c("00064","72178","72199","82903", "85310","85311"))
###Lets do some calculations######

#copying in the information from Vanessa and Ryan Michie so that we can calculate important MZ flow statistics such as 7Q10, 1Q10 and 30Q5
#...will likely need to develop a different code to take care of the harmonic mean flow

library(RCurl)
library(lubridate)
library(zoo)
library(dplyr)
library(devtools)

devtools::install_github('DEQrmichie/dflowR', host = 'https://api.github.com', 
                         dependencies= TRUE, force = TRUE, upgrade='never')

library(dflowR)

#--- USGS web download example ------------------------------------------------------------------------------------

# download
q.df <- readNWISdv(siteNumbers = "14174000",
                   parameterCd = "00060",
                   startDate = "1970-10-01",
                   endDate = "2016-09-30",
                   statCd = "00003")

# Just get columns 3 and 4 (date and flow)
q <- q.df[,c(3,4)]
colnames(q) <-c("date", "flow")
q$date <- as.POSIXct(q$date, format="%Y-%m-%d")

dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="10-01", wyend="09-30")
thirty<-dflow(x=q, m=30, r=5, yearstart=NA, yearend=NA, wystart="10-01", wyend="09-30")


#-------try out harmonic mean
#rossman terminology NDAYS, NZEROES, DSUM, DR
# interpretation = 
#NDAYS is the total number of days of records, 
#NZEROES is the number of days with 0 flow
#DSUM is the sum of the reciprocal of each value (e.g 1/x(1)+1/x(2)+1/x(3)+1/x(t)....)
#DR= (NDAYS-NZEROES)/NDAYS

#HMEAN - (NDAYS-NZEROES)/DSUM*DR

#rossman notes that the final estimate is the weighted average of the harmonic mean of non-zero flows and zero (which is where the NZEROES and DR come in)

#psych package has harmonic mean calculator

library(psych)

#test the function with a simple example
x<-c(1,5,4,6)


harmonic.mean(x,zero=FALSE)
#2.474227 - correct

#test with a zero in the mix, if "zero=FALSE" in the function it should treutnr the harmonic mean of the non-zero elements

z<-c(1,5,4,6,0)

harmonic.mean(z,zero=FALSE)
#2.474227 - same answer as before, excellent

#so we can just take this function and multiply it by DR and we should get the exact same thing that DFLOW would return

dr<-(length(z)-length(which(z==0)))/length(z)


dharm<-harmonic.mean(z,zero=FALSE)*dr
#1.979

#this appears to be working. Now to make it into a function
dharmonic<-function(x) {
  
  return(harmonic.mean(x,zero=FALSE)*(length(x)-length(which(x==0)))/length(x))
  
}

dharmonic(z)

#great! This is working with our small examples, time to try actual flow data - take data from the original dflow example above
#need to remove date for this function to work

r<-q[,2]
  
dharmonic(r)

#function worked, though won't be able to verify against DFLOW until I speak more with Steve and Erich



