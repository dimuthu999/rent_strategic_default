# this file uses cleaned hmda data using "01 clea_hmda_data.R" and 
# zillow "Quarterly Historic Metro ZRI" to construct the instrument


rm(list=ls())
library(data.table)
library(lfe)
library(stargazer)
library(plyr)
library(reshape2)
library(lubridate)




# Further clean hmda data --------------------------------------------------------------------

# these files were created by "01 clea_hmda_data.R"
files = list.files(path = 'C:/Users/dnratnadiwakara/Documents/interest rate and default/Data/Raw/HMDA/subsets', pattern = '.rds',full.names = TRUE)
hmda = lapply(files, function (x) as.data.frame(readRDS(x)))
hmda = rbindlist(hmda, fill = TRUE)
hmda <- as.data.frame(hmda)

hmda$actiontaken <- as.numeric(hmda$actiontaken)
hmda$applicantincome <- as.numeric(hmda$applicantincome)
hmda$amountofloan <- as.numeric(hmda$amountofloan)
hmda$asofdate <- as.numeric(hmda$asofdate)
hmda['loan_to_income'] <- hmda$amountofloan/hmda$applicantincome
hmda$respondentname <- tolower(hmda$respondentname)
hmda$parentname <- tolower(hmda$parentname)
hmda$msa <- as.numeric(hmda$msa)
hmda['msa_time'] <- paste(hmda$msa,hmda$asofdate)

hmda <- hmda[!is.na(hmda$msa),]

hmda <- hmda[hmda$actiontaken %in% c(1,3),]
hmda['denied']<-ifelse(hmda$actiontaken==3,1,0)

hmda['big4_1'] <- ifelse(grepl("wells fargo",hmda$respondentname)|grepl("bank of america",hmda$respondentname)|grepl("citig",hmda$respondentname)|grepl("jp",hmda$respondentname),1,0)

hmda['big4_2'] <- ifelse(grepl("wells fargo",hmda$parentname)|grepl("bank of america",hmda$parentname)|grepl("citig",hmda$parentname)|grepl("jp",hmda$parentname),1,0)

hmda['big4'] <- ifelse(hmda$big4_1==1 | hmda$big4_2==1,1,0)

hmda['msa_big4'] <- paste(hmda$msa,hmda$big4)

hmda['big4_year'] <- as.numeric(paste(hmda$asofdate,hmda$big4,sep=""))

hmda$censustract <- as.numeric(hmda$censustract)*100
# hmda <- hmda[!is.na(hmda$censustract),]




# # MSA/ZIP Big4 Fraction  Share(MSA,2008)---------------------------------------------------

temp <- hmda[hmda$asofdate==2008,]
msa_frac <- ddply(temp,.(msa),summarise,msa_big4_frac = mean(big4,na.rm=TRUE))
saveRDS(msa_frac,file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/msa_frac.rds")



# Zillow Rent -------------------------------------------------------------

# Affordability_ChainedZRI_2018Q3.csv is "Quarterly Historic Metro ZRI" from https://www.zillow.com/research/data/
zillow_rent <- read.csv(file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/Affordability_ChainedZRI_2018Q3.csv")
zillow_rent$RegionID <- NULL
zillow_rent$SizeRank <- NULL

zillow_rent <- melt(zillow_rent,id.vars = c("RegionName"))
zillow_rent['year'] <- substr(zillow_rent$variable,2,5)
zillow_rent['month'] <- substr(zillow_rent$variable,7,8)
zillow_rent <- zillow_rent[zillow_rent$month=="12",]
zillow_rent <- zillow_rent[zillow_rent$year >= "2008" & zillow_rent$year<="2015",]
zillow_rent$variable <- NULL
zillow_rent$month <- NULL
zillow_rent$RegionName <- as.character(zillow_rent$RegionName)
names(zillow_rent) <- c("msa","rent","year")
zillow_rent['city'] <- tolower(zillow_rent$msa)


library(zipcode)
data("zipcode")
zipcode$city <- tolower(paste(zipcode$city,", ",zipcode$state,sep=""))
zipcode[,c("state","latitude","longitude")] <- list(NULL)

zillow_rent <- merge(zillow_rent,zipcode,by="city",all.x = TRUE)
# unmatched <- as.data.frame(unique(zillow_rent[is.na(zillow_rent$zip),]$msa))
# write.csv(unmatched,file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/unmatched_msa.csv")

#unmatched names were manually checked and saved in this file
unmatched <- read.csv(file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/unmatched_msa_2.csv")
unmatched$msa <- as.character(unmatched$msa)
zillow_rent <- merge(zillow_rent,unmatched,by="msa",all.x = TRUE)
zillow_rent$zip <- ifelse(is.na(zillow_rent$zip),zillow_rent$zip2,zillow_rent$zip)
zillow_rent <- zillow_rent[!is.na(zillow_rent$zip),]
zillow_rent <- zillow_rent[!is.na(zillow_rent$rent),]
zillow_rent$zip2 <- NULL
zillow_rent$zip <- as.integer(zillow_rent$zip)

# zip cbsa code crosswalk file from https://www.huduser.gov/portal/datasets/usps_crosswalk.html
zip_cbsa <- read.csv(file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/ZIP_CBSA_092018.csv")
zillow_rent <- merge(zillow_rent,zip_cbsa,by="zip",all.x = TRUE)
not_na <- unique(zillow_rent[!is.na(zillow_rent$cbsa),]$msa)
yes_na <- unique(zillow_rent[is.na(zillow_rent$cbsa),]$msa)
unmatched <- not_na[!not_na %in% yes_na]
zillow_rent <- zillow_rent[!is.na(zillow_rent$cbsa),]
zillow_rent <- zillow_rent[!duplicated(zillow_rent[,c("cbsa","year")]),]

zillow_rent <- zillow_rent[!is.na(zillow_rent$rent),]
zillow_rent$msa <- NULL
zillow_rent <- merge(zillow_rent,msa_frac,by.x = "cbsa",by.y = "msa")
zillow_rent$year <- as.numeric(zillow_rent$year)




# Big4 Propensity to deny - Calculate L in equation 1--------------------------------------------------------------

reg <- felm(as.formula("denied~loan_to_income+log(applicantincome)+log(amountofloan)|
                            msa_big4+msa_time+applicantrace1+big4_year|0|msa_big4+msa_time"),data = hmda)
fes <- getfe(reg)
fes$fe <- as.character(fes$fe)
fes$idx <- as.character(fes$idx)
fes <- fes[fes$fe =="big4_year",]
fes <- as.data.frame(fes[,c("idx","effect")])
row.names(fes) <- NULL
fes$effect <- fes$effect-fes[fes$idx=="20070",]$effect
fes['year'] <- substr(fes$idx,1,4)
fes['big4'] <- substr(fes$idx,5,5)
fes$idx <- NULL
fes_big4 <- fes[fes$big4=="1",]
fes_big4$big4 <- NULL
fes_nbig4 <- fes[fes$big4=="0",]
fes_nbig4$big4 <- NULL
fes <- merge(fes_big4,fes_nbig4,by="year")
fes['big4_nbig4'] <- fes$effect.x - fes$effect.y
fes$effect.x <- NULL
fes$effect.y <- NULL




# Construct Instrument "credit_supply_shock" ----------------------------------------------------


zillow_rent <- merge(zillow_rent,fes,by='year')
zillow_rent['credit_supply_shock'] <- zillow_rent$big4_nbig4*zillow_rent$msa_big4_frac
zillow_rent$rent <- as.numeric(zillow_rent$rent)

zillow_rent_1 <- zillow_rent[,c("cbsa","year","rent")]
zillow_rent_1$year <- zillow_rent_1$year - 1
names(zillow_rent_1) <- c("cbsa","year","rent_1")
zillow_rent <- merge(zillow_rent,zillow_rent_1,by=c("cbsa","year"))
zillow_rent['log_delta_rent'] <- log(zillow_rent$rent_1/zillow_rent$rent)





# Saving Files ------------------------------------------------------------

saveRDS(fes,file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/big4_propensity_to_deny.rds")

saveRDS(zillow_rent,file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/creditsupplyshock.rds")

saveRDS(hmda[,c("denied","loan_to_income","applicantincome","amountofloan","msa_big4","msa_time","applicantrace1","big4_year","asofdate","big4"),
             ],file="C:/Users/dnratnadiwakara/Documents/rent_strategic_default/hmda_cleaned_merged.rds")
