rm(list=ls())
library(data.table)
library(stargazer)
library(lfe)
library(rgdal)
library(ggplot2)
library(reshape2)

files = list.files(path = 'C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA/rds/OO_NP', pattern = '.rds',full.names = TRUE)
hmda = lapply(files, function (x) readRDS(x))
hmda = rbindlist(hmda, fill = TRUE)

hmda <- hmda[hmda$actiontaken %in% c("1","3")]
hmda[,denied:=ifelse(hmda$actiontaken %in% c("3"),1,0)]


hmda_msa <- hmda[,.(denial_rate = mean(denied,na.rm=TRUE)), by = list(msa,asofdate)]
hmda_msa$msa <- as.integer(hmda_msa$msa)
hmda_msa[,year:=as.numeric(hmda_msa$asofdate)]
hmda_msa[,asofdate:=NULL]
hmda_msa[,year:=hmda_msa$year+1]

hmda_msa_np <- hmda_msa
names(hmda_msa_np) <- c("msa","denial_rate_np","year")

rm(hmda)
gc()


files = list.files(path = 'C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA/rds/OO_RF', pattern = '.rds',full.names = TRUE)
hmda = lapply(files, function (x) readRDS(x))
hmda = rbindlist(hmda, fill = TRUE)

hmda <- hmda[hmda$actiontaken %in% c("1","3")]
hmda[,denied:=ifelse(hmda$actiontaken %in% c("3"),1,0)]


hmda_msa <- hmda[,.(denial_rate = mean(denied,na.rm=TRUE)), by = list(msa,asofdate)]
hmda_msa$msa <- as.integer(hmda_msa$msa)
hmda_msa[,year:=as.numeric(hmda_msa$asofdate)]
hmda_msa[,asofdate:=NULL]
hmda_msa[,year:=hmda_msa$year+1]

hmda_msa_rf <- hmda_msa
names(hmda_msa_rf) <- c("msa","denial_rate_rf","year")

rm(hmda)
gc()


# these files were created by 03 create_loan_month_freddie.R
# file_list<- list.files(path="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/processed data/freddie per data",pattern="*.rds",full.names = TRUE)
# loan_month = lapply(file_list, function (x) data.table(readRDS(x)))
# loan_month = rbindlist(loan_month, fill = TRUE)
# loan_month[ ,zip3:= loan_month$POSTALCODE]
# loan_month[ ,month:= loan_month$CURRENTMONTH]
# loan_month[ ,year := as.numeric(substr(loan_month$month,1,4))]
# 
# loan_month[,default:=ifelse(loan_month$CURRENTLOANDELINQ>=1,1,0)]
# loan_month[,foreclosed:= ifelse(loan_month$ZEROBALANCECODE>=3 & !is.na(loan_month$ZEROBALANCECODE),1,0)]
# loan_month[,prepaid:=ifelse(loan_month$ZEROBALANCECODE==1 & !is.na(loan_month$ZEROBALANCECODE),1,0)]
# 
# saveRDS(loan_month,file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/processed data/freddie per data/freddie_data_loan_month.rds")
loan_month <- readRDS(file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/processed data/freddie per data/freddie_data_loan_month.rds")


loan_month <- merge(loan_month,hmda_msa_rf,by.x=c("MSA","year"),by.y = c("msa","year"))
loan_month <- merge(loan_month,hmda_msa_np,by.x=c("MSA","year"),by.y = c("msa","year"))

# ACS Data
acs_data <- fread("C:/Users/dnratnadiwakara/Documents/rent_strategic_default/acs_msa_2.csv",sep="|",stringsAsFactors = FALSE,header = FALSE)
names(acs_data) <- c("MSA","name","medianhhincome","medianage","totalpopulation","blackpopulation","whitepopulation","renters","laborforce","collegedegree","year")
acs_data$name <- NULL
acs_data[,white_frac:= acs_data$whitepopulation/acs_data$totalpopulation]
acs_data[,renter_frac:= acs_data$renters/acs_data$totalpopulation]
acs_data[,collegedegree_frac:= acs_data$collegedegree/acs_data$totalpopulation]
acs_data[,c("blackpopulation","whitepopulation","renters","laborforce","collegedegree"):=list(NULL)]

loan_month[,c("name","medianhhincome","medianage","totalpopulation","blackpopulation","whitepopulation","renters","laborforce","collegedegree"):=list(NULL)]

loan_month <- merge(loan_month,acs_data,by=c("MSA","year"),all.x=TRUE)


# BLS Data
bls_data <- fread(file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/bls_unemp_metro.txt")
bls_data[,MSA:=as.integer(substr(bls_data$series_id,8,12))]
bls_data[,month:=as.Date(paste(bls_data$year,substr(bls_data$period,2,3),"01",sep="-"))]
bls_data[,series:=substr(bls_data$series_id,19,20)]
bls_data <- bls_data[bls_data$series=="03"]
bls_data[,c("series_id","year","period","footnote_codes","series"):=list(NULL)]
names(bls_data) <- c("unemp_rate","MSA","month")
bls_data <- bls_data[!duplicated(bls_data[,c("MSA","month")])]
bls_data <- bls_data[bls_data$month >= "2008-01-01"]
setkey(bls_data,MSA,month)

setkey(loan_month,MSA,month)
loan_month <- merge(loan_month,bls_data,by=c("MSA","month"),all.x=TRUE)

loan_month <- loan_month[loan_month$year<=2014]
gc()

note =c("Fixed Effects: Loan, Year, MSA","Standard Errors clustered by Loan")


formulas <- list()
formulas[[1]] <- as.formula(paste("default~denial_rate_np+LOANAGE+I(LOANAGE^2)+I(LOANAGE^3)+current_equity*log(current_housevalue)+unemp_rate+log(medianhhincome)+log(totalpopulation)+white_frac+renter_frac+collegedegree_frac|factor(year)+factor(MSA)+factor(LOANNO)|0|LOANNO+MSA",sep=""))
formulas[[2]] <- as.formula(paste("default~denial_rate_rf+LOANAGE+I(LOANAGE^2)+I(LOANAGE^3)+current_equity*log(current_housevalue)+unemp_rate+log(medianhhincome)+log(totalpopulation)+white_frac+renter_frac+collegedegree_frac|factor(year)+factor(MSA)+factor(LOANNO)|0|LOANNO+MSA",sep=""))
gc()
rm(iv_reg)
iv_reg <- list()
iv_reg[[1]] <- felm(formulas[[1]], data=loan_month)
gc()
iv_reg[[1]] <- felm(formulas[[2]], data=loan_month)
gc()

stargazer(iv_reg,type="text",no.space = TRUE,omit.stat = c("f","adj.rsq","ser"))


loan_month_default <- loan_month[,.(default_rate=mean(default,na.rm=TRUE),
                                    denial_rate_np=mean(denial_rate_np,na.rm=TRUE),
                                    denial_rate_rf=mean(denial_rate_rf,na.rm=TRUE)),by=list(year)]

stargazer(loan_month_default,type="text",summary = FALSE)

loan_month_default <- melt(loan_month_default,id.vars = "month")


ggplot(data=loan_month_default, aes(x=month, y=value, color=variable)) + geom_line()
