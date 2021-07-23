#############################
# Initial set-up
rm(list=ls())
options(stringsAsFactors=F)

#################################
# read in the ezR functions
source("https://raw.githubusercontent.com/stan-pounds/ezR4students/main/ezR4students-2021-07-20.R")

##########################
# Obtain the colon cancer data
use.packages("TH.data")
data(colon)

# Prepare recurrence data
rfs.clms=c("id","rx","sex","age",
           "obstruct","perfor",
           "adhere","nodes",
           "extent","surg","differ",
           "time","status")
rfs.data=colon[colon[,"etype"]==1,rfs.clms]
colnames(rfs.data)=gsub("time","recur.time",colnames(rfs.data))
colnames(rfs.data)=gsub("status","recur.status",colnames(rfs.data))


# Prepare overall survival data
os.clms=c("id","time","status")
os.data=colon[colon[,"etype"]==2,os.clms]
colnames(os.data)[2:3]=c("death.time","death.status")

# Merge the data sets
colon.data=merge(rfs.data,os.data,by="id")

# Check for any duplicated subject ID
any(duplicated(colon.data$id))

# define time to first event

colon.data$first.event.time=pmin(colon.data$death.time,
                                 colon.data$recur.time)

colon.data$first.event.status=(colon.data$recur.status)+2*(colon.data$recur.status==0)*(colon.data$death.status==1)

save(colon.data,
     file="C:/Users/spounds/Box/Biostat-Teaching/Intro-Biostats/DataSets/survival-package-colon-data.Rdata")

