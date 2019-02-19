
rm(list=ls())
library(data.table)



asofdates <- 2013:2004
hmda_header <- c("str","asofdate","respondentid","agencycode","typeofloan","purposeofloan","occupancy","amountofloan","actiontaken","msa","state","countycode",
                 "censustract","applicantsex","coapplicantsex","applicantincome","typeofpurchaser","denialreason1","denialreason2","denialreason3",
                 "editstatus","propertytype","preapprovals","applicantethnicity","coapplicantenthnicity","applicantrace1","applicantrace2","applicantrace3",
                 "applicantrace4","applicantrace5","coapplicantrace1","coapplicantrace2","coapplicantrace3","coapplicantrace4","coapplicantrace5",
                 "ratespread","hoepastatus","lienstatus","seqno")
split.points_hmda <- c(4,14,15,16,17,18,23,24,29,31,34,41,42,43,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,71,72,73,80)

hmda_files <- c("Lars.ultimate.2013.dat","Lars.ultimate.2012.dat","Lars.ultimate.2011.dat","Lars.ultimate.2010.dat","2009_Ultimate_PUBLIC_LAR.dat","lars.ultimate.2008.dat","lars.ultimate.2007.dat","LARS.ULTIMATE.2006.DAT","LARS.ULTIMATE.2005.DAT","u2004lar.public.dat")
 

path = "C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA/text files/"

for(j in 1:length(asofdates)) {
  print(asofdates[j])
  
  hmda_file_name = paste(path,hmda_files[j],sep="")
  
  hmda <- fread(hmda_file_name,header = FALSE,stringsAsFactors = FALSE,sep="\t",data.table = FALSE)
  hmda <- as.vector(hmda$V1)
  
  hmda_vector <- as.data.frame(matrix(ncol = 39,nrow=length(hmda)))
  
  hmda_vector[,1]<- hmda
  
  prev = 0
  i=2
  for(point in split.points_hmda) {
    print (paste(prev+1,point))
    hmda_vector[,i] <- sapply(hmda_vector$V1,function(x) substr(x,prev+1,point))
    i=i+1
    prev=point
  }
  
  names(hmda_vector) <- hmda_header
  
  hmda_vector <- data.table(hmda_vector)
  hmda_vector <- hmda_vector[hmda_vector$editstatus==" " & hmda_vector$propertytype=="1"]
  hmda_vector[,c("str","denialreason2","denialreason3","editstatus","applicantrace3","applicantrace4","applicantrace5","coapplicantrace3","coapplicantrace4","coapplicantrace5"):=list(NULL)]
  
  
  saveRDS(hmda_vector[hmda_vector$occupancy=="1" &hmda_vector$purposeofloan=="1"],file=paste(path,"OO_NP_",asofdates[j],".rds",sep=""))
  saveRDS(hmda_vector[hmda_vector$occupancy=="2" &hmda_vector$purposeofloan=="1"],file=paste(path,"NO_NP_",asofdates[j],".rds",sep=""))
  saveRDS(hmda_vector[hmda_vector$occupancy=="1" &hmda_vector$purposeofloan=="3"],file=paste(path,"OO_RF_",asofdates[j],".rds",sep=""))
  saveRDS(hmda_vector[hmda_vector$occupancy=="2" &hmda_vector$purposeofloan=="3"],file=paste(path,"NO_RF_",asofdates[j],".rds",sep=""))
  rm(hmda_vector)
  gc()
}




hmda_header <- c("asofdate","respondentid","agencycode","typeofloan","propertytype","purposeofloan","occupancy","amountofloan","preapprovals","actiontaken","msa","state","countycode",
                 "censustract","applicantethnicity","coapplicantenthnicity","applicantrace1","applicantrace2","applicantrace3",
                 "applicantrace4","applicantrace5","coapplicantrace1","coapplicantrace2","coapplicantrace3","coapplicantrace4","coapplicantrace5","applicantsex","coapplicantsex"
                 ,"applicantincome","typeofpurchaser","denialreason1","denialreason2","denialreason3","ratespread", "hoepastatus","lienstatus","editstatus","seqno")

hmda_files <- c("2016HMDALAR - National.csv","2015HMDALAR - National.csv","2014HMDALAR - National.csv")

asofdates <- 2016:2014
path = "C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA/text files/"


for(j in 1:length(asofdates)) {
  print(asofdates[j])
  
  hmda_file_name = paste(path,hmda_files[j],sep="")
  
  hmda <- fread(hmda_file_name,header = FALSE,stringsAsFactors = FALSE,sep=",",data.table = TRUE,colClasses = list(character=1:45))
  names(hmda) <- c(hmda_header,"V1","V2","V3","V4","V5","V6","V7")
  hmda <- hmda[,..hmda_header]
  
  hmda <- hmda[hmda$editstatus=="" & hmda$propertytype=="1"]
  
  hmda[,c("denialreason2","denialreason3","editstatus","applicantrace3","applicantrace4","applicantrace5","coapplicantrace3","coapplicantrace4","coapplicantrace5"):=list(NULL)]
  
  
  saveRDS(hmda[hmda$occupancy=="1" &hmda$purposeofloan=="1"],file=paste(path,"OO_NP_",asofdates[j],".rds",sep=""))
  saveRDS(hmda[hmda$occupancy=="2" &hmda$purposeofloan=="1"],file=paste(path,"NO_NP_",asofdates[j],".rds",sep=""))
  saveRDS(hmda[hmda$occupancy=="1" &hmda$purposeofloan=="3"],file=paste(path,"OO_RF_",asofdates[j],".rds",sep=""))
  saveRDS(hmda[hmda$occupancy=="2" &hmda$purposeofloan=="3"],file=paste(path,"NO_RF_",asofdates[j],".rds",sep=""))
  rm(hmda)
  gc()
}
