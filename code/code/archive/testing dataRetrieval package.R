install.packages("dataRetrieval")
library('dataRetrieval')

vignette("dataRetrieval", package="dataRetrieval")



startDate<-as.Date("2015-01-01")
var<-list()


WQPdata<-readWQPdata(statecode="Georgia", 
                     characteristicName='Escherichia coli',
                     startDateLo=startDate,
                     service="ProjectMonitoringLocationWeighting",
                     dataProfile="biological")

#just a test to see how to pull form WQP portal. Works decently. No long/lat data but it is there.