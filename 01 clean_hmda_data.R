# this file cleans hmda data (ultimate lar) and merge with lender data (final panel)
# Allstates_withlender_YYYY_subset.rds files are used in subsequent analysis

rm(list=ls())
library(data.table)

hmda_header <- c("str","asofdate","respondentid","agencycode","typeofloan","purposeofloan","occupancy","amountofloan","actiontaken","msa","state","countycode",
                      "censustract","applicantsex","coapplicantsex","applicantincome","typeofpurchaser","denialreason1","denialreason2","denialreason3",
                      "editstatus","propertytype","preapprovals","applicantethnicity","coapplicantenthnicity","applicantrace1","applicantrace2","applicantrace3",
                      "applicantrace4","applicantrace5","coapplicantrace1","coapplicantrace2","coapplicantrace3","coapplicantrace4","coapplicantrace5",
                      "ratespread","hoepastatus","lienstatus","seqno")
split.points_hmda <- c(4,14,15,16,17,18,23,24,29,31,34,41,42,43,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,71,72,73,80)

panel_header <- c("str","respondentid","msanumber","agencycode","agencygroupcode","respondentname","resp_home_city","resp_home_state","resp_FIPS_no","total_assets",
                  "otherlendercode","parentidentifier","parentname","parent_city","parent_state","asofyear","NICRSSD")
split.points_panel <- c(10,15,16,18,48,73,75,77,87,88,98,128,153,155,159,169)

# hmda_files <- c("Lars_ultimate_2014.txt","Lars.ultimate.2013.dat","Lars.ultimate.2012.dat","Lars.ultimate.2011.dat","Lars.ultimate.2010.dat","2009_Ultimate_PUBLIC_LAR.dat","lars.ultimate.2008.dat","lars.ultimate.2007.dat")
# panel_files <- c("Panel.final.2014.dat","Panel.final.2013.dat","Panel.final.2012.dat","Panel.final.2011.dat","Panel.final.2010.dat","2009_Final_PUBLIC_Panel.dat","panel.f.2008.dat","panel.f.2007.dat")
# asofdates <- 2014:2007

hmda_files <- c("LARS.ULTIMATE.2006.DAT","LARS.ULTIMATE.2005.DAT","u2004lar.public.dat")
panel_files <- c("PANEL.F.2006.DAT","PANEL.F.2005.DAT","f2004pan.public.dat")
asofdates <- 2014:2004

path = "C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA/"
path2 = "C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA Panel Final/"


for(j in 1:length(asofdates)) {
  print(asofdates[j])
  
  hmda_file_name = paste(path,hmda_files[j],sep="")
  panel_file_name = paste(path2,panel_files[j],sep="")
  with_lender_name = paste(path,"Allstates_withlender_",asofdates[j],".rds",sep="")
  with_lender_name_subset = paste(path,"Allstates_withlender_",asofdates[j],"_subset.rds",sep="")
  
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
  
  panel <- fread(panel_file_name,header = FALSE,stringsAsFactors = FALSE,sep="\t",data.table = FALSE)
  panel <- as.vector(panel$V1)
  
  panel_vector <- as.data.frame(matrix(ncol = 17,nrow=length(panel)))
  
  panel_vector[,1]<- panel
  
  prev = 0
  i=2
  for(point in split.points_panel) {
    print (paste(prev+1,point))
    panel_vector[,i] <- sapply(panel_vector$V1,function(x) substr(x,prev+1,point))
    i=i+1
    prev=point
  }
  
  names(panel_vector) <- panel_header
  panel_vector <- panel_vector[panel_vector$respondentid %in% unique(hmda_vector$respondentid),]
  panel_vector$str <- NULL
  panel_vector$msanumber <- NULL
  panel_vector <- panel_vector[!duplicated(panel_vector$respondentid),]
  
  hmda_vector$censustract <- paste(hmda_vector$state,hmda_vector$countycode,hmda_vector$censustract,sep="")
  
  hmda_vector <- data.table(hmda_vector)
  setkeyv(hmda_vector,c('respondentid'))
  hmda_vector[,str:=NULL]
  
  panel_vector <- data.table(panel_vector)
  setkeyv(panel_vector,c('respondentid'))
  
  hmda_vector <- merge(hmda_vector,panel_vector,by="respondentid")
  
  
  saveRDS(hmda_vector,file=with_lender_name)
  saveRDS(hmda_vector[hmda_vector$msa != "NA" & hmda_vector$occupancy=="1" & hmda_vector$propertytype=="1" & hmda_vector$purposeofloan=="1" & hmda_vector$actiontaken != 6 & hmda_vector$editstatus==" ",
                      c("actiontaken","applicantrace1","applicantincome","amountofloan","asofdate","respondentname","parentname","msa","censustract")],
                      file=with_lender_name_subset)
  
}


for(j in 1:length(asofdates)) {
  print(asofdates[j])
  
  with_lender_name = paste(path,"Allstates_withlender_",asofdates[j],".rds",sep="")
  with_lender_name_subset = paste(path2,"Allstates_withlender_refin_",asofdates[j],"_subset.rds",sep="")
  hmda <- readRDS(with_lender_name)
  saveRDS(hmda[hmda$msa != "NA" & hmda$occupancy=="1" & hmda$propertytype=="1" & hmda$purposeofloan=="3" & hmda$actiontaken != 6 & hmda$editstatus==" ",
                      c("actiontaken","applicantrace1","applicantincome","amountofloan","asofdate","respondentname","parentname","msa","censustract")],
          file=with_lender_name_subset)
}