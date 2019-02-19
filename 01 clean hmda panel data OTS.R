# this file cleans hmda lender data (final panel)
# panel_data_YYYY.rds files are used in subsequent analysis

rm(list=ls())
library(data.table)


asofdates <- 2006:2004


path = "C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA/"
path2 = "C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA Panel Final/"



for(j in 1:length(asofdates)) {
  print(asofdates[j])
  
  with_lender_name = paste(path,"Allstates_withlender_",asofdates[j],".rds",sep="")
  with_lender_name_subset = paste(path,"Allstates_withlender_",asofdates[j],"_agency.rds",sep="")
  
  hmda_vector <- readRDS(file=with_lender_name)
  
  saveRDS(hmda_vector[hmda_vector$msa != "NA" & hmda_vector$actiontaken != 6 & hmda_vector$editstatus==" ",
                      c("actiontaken","applicantrace1","applicantincome","amountofloan","asofdate","typeofloan","purposeofloan","occupancy","state","countycode","typeofpurchaser","propertytype","respondentname","parentname","msa","censustract","resp_home_city","resp_home_state","agencycode.y","agencygroupcode")],
          file=with_lender_name_subset)
}



# d1 <- readRDS("C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA Panel Final/panel_data_2010.rds")
# d1['id'] <- paste(d1$respondentname,d1$resp_home_city,d1$resp_home_state)
# d1 <- d1[,c("respondentid","id","agencycode")]
# d2 <- readRDS("C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA Panel Final/panel_data_2014.rds")
# d2['id'] <- paste(d2$respondentname,d2$resp_home_city,d2$resp_home_state)
# d2 <- d2[,c("respondentid","id","agencycode")]
# names(d2) <- c("respondentid14","id","agencycode14")
# 
# d3 <- merge(d1,d2,by="id")
# table(d3$agencycode,d3$agencycode14)
