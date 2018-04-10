# Primary care and health outcomes in the United States
# (c) 2018, Sanjay Basu, basus@stanford.edu

#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("labelled")
#install.packages("plm")
#install.packages("reshape")
#install.packages("stargazer")
#install.packages("httr")

rm(list=ls())

library(devtools)
library(tidyverse)
library(readxl)
library(labelled)
library(httr)

setwd("~/Data/ahrf")


# Download raw AHRF files ------------------------------------------------------
# See https://github.com/jjchern/ahrf/blob/master/data-raw/prep_county.R

url = "https://datawarehouse.hrsa.gov/DataDownload/AHRF/AHRF_2016-2017.ZIP"
fil_zip = tempfile(fileext = ".zip")

if(!file.exists("data-raw/county/ahrf2017.asc")) {
  download.file(url, fil_zip)
  dir.create("data-raw/county")
  unzip(fil_zip, exdir = "data-raw/county", junkpaths = TRUE)
}
list.files("data-raw/county")

raw_src = "data-raw/county/ahrf2017.asc" # Raw data
dic_src = "data-raw/county/ahrf2016-17.sas" # SAS dictionary file
doc_src = "data-raw/county/AHRF 2016-2017 Technitransl Documentation.xlsx"


# Find out the line for the first field: F00001 ---------------------------

read_excel(doc_src) %>%
  pull(X__1) %>%
  grepl("F00001", .) %>%
  which() -> bgn_line
bgn_line

# Prepare the layout file -------------------------------------------------

read_excel(doc_src,
           col_names = c("field", "col_col", "year_of_data", "var_label",
                         "characteristics", "source", "date_on"),
           skip = bgn_line) %>%
  filter(grepl("^F[0-9]", field)) %>%
  separate(col_col, c("col_start", "col_end")) %>%
  mutate_at(c("col_start", "col_end"), as.integer) -> ahrf_county_layout
ahrf_county_layout

# Prepare the county AHRF file --------------------------------------------

read_fwf(file = raw_src,
         col_positions = fwf_positions(start = ahrf_county_layout$col_start,
                                       end = ahrf_county_layout$col_end,
                                       col_names = ahrf_county_layout$field)) -> ahrf_county
ahrf_county

# Add variable labels -----------------------------------------------------

ahrf_county_layout %>%
  select(field, var_label) %>%
  deframe() %>%
  as.list() -> var_label(ahrf_county)
var_label(ahrf_county)

# Save AHRF  ----------------------------------------------------------------

save(ahrf_county,file="ahrf_county")

# Delete raw data as it’s too large ---------------------------------------

unlink(raw_src)

#  raw trans files ---------------------------------------
library(readr)
trans05 <- read_delim("data-raw/county/traffic2005.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
trans_us = cbind(trans05[,3],trans05[,7])
names(trans_us) <- c("fips","trans_2005")


trans_us = trans_us[1:1212,]
# Select columns with mort data and convert from text string with confidence intervals to numbers ---------------------------------------
counties_clean <- trans_us %>%
  select(2) %>%
  mutate_all(funs(as.numeric(substring(.,1,4))))

# Recombine AHRF with processed life expectancy data ---------------------------------------
trans_counties05 <- cbind(trans_us[,1],counties_clean) 
names(trans_counties05) <- c("fips","trans_2005")


trans10 <- read_delim("data-raw/county/traffic2010.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
trans_us = cbind(trans10[,3],trans10[,7])
names(trans_us) <- c("fips","trans_2010")


trans_us = trans_us[1:977,]
# Select columns with mort data and convert from text string with confidence intervals to numbers ---------------------------------------
counties_clean <- trans_us %>%
  select(2) %>%
  mutate_all(funs(as.numeric(substring(.,1,4))))

# Recombine AHRF with processed life expectancy data ---------------------------------------
trans_counties10 <- cbind(trans_us[,1],counties_clean) 
names(trans_counties10) <- c("fips","trans_2010")




trans15 <- read_delim("data-raw/county/traffic2015.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
trans_us = cbind(trans15[,3],trans15[,7])
names(trans_us) <- c("fips","trans_2015")


trans_us = trans_us[1:978,]
# Select columns with mort data and convert from text string with confidence intervals to numbers ---------------------------------------
counties_clean <- trans_us %>%
  select(2) %>%
  mutate_all(funs(as.numeric(substring(.,1,4))))

# Recombine AHRF with processed life expectancy data ---------------------------------------
trans_counties15 <- cbind(trans_us[,1],counties_clean) 
names(trans_counties15) <- c("fips","trans_2015")






transtot <- left_join(trans_counties10,trans_counties05, by=c("fips"="fips"))
transtot <- left_join(trans_counties15,transtot, by=c("fips"="fips"))


# Save LE  ----------------------------------------------------------------

save(transtot,file="trans_counties")



# Download raw CHRD files ---------------------------------------
chrd <- read_csv("http://www.countyhealthrankings.org/sites/default/files/CHR_TRENDS_CSV_2018.csv")
chrd <- chrd %>%
  separate(yearspan, into=c("time","yearend"), sep = "-") %>%
  unite(fips,"statecode","countycode",sep="") %>%
  select(c(1,3,4,9)) %>%
  mutate(time = as.integer(time)) %>%
  #  filter(time == 2000 | time==2005 | time==2010) %>%
  unite(var_year,"measurename","time") %>%
  spread(var_year,rawvalue)  %>%
  select(c("fips","Adult obesity_2005","Adult obesity_2010","Adult obesity_2013",
           "Air pollution - particulate matter_2005", "Air pollution - particulate matter_2010", "Air pollution - particulate matter_2012",
           "Alcohol-impaired driving deaths_2008","Alcohol-impaired driving deaths_2010","Alcohol-impaired driving deaths_2015",
           "Premature death_2005","Premature death_2010","Premature death_2014",
           "Preventable hospital stays_2006","Preventable hospital stays_2010","Preventable hospital stays_2015")) %>%
  mutate(fipscode = as.numeric(fips),
         obese_2005 = `Adult obesity_2005`*100,
         obese_2010 = as.numeric(`Adult obesity_2010`)*100,
         obese_2015 = obese_2010+5/3*(as.numeric(`Adult obesity_2013`)*100-as.numeric(`Adult obesity_2010`)*100),
         airpol_2005 = as.numeric(`Air pollution - particulate matter_2005`),
         airpol_2010 = as.numeric(`Air pollution - particulate matter_2010`),
         airpol_2015 = airpol_2010 + 5/2*(as.numeric(`Air pollution - particulate matter_2012`) - as.numeric(`Air pollution - particulate matter_2010`)),
         trans_2005 = as.numeric(`Alcohol-impaired driving deaths_2008`)-5/2*(as.numeric(`Alcohol-impaired driving deaths_2010`)-as.numeric(`Alcohol-impaired driving deaths_2008`)),
         trans_2010 = as.numeric(`Alcohol-impaired driving deaths_2010`),
         trans_2015 = as.numeric(`Alcohol-impaired driving deaths_2015`),
         premort_2005 = as.numeric(`Premature death_2005`),
         premort_2010 = as.numeric(`Premature death_2010`),
         premort_2015 = premort_2010 + 5/4*(as.numeric(`Premature death_2014`)-as.numeric(`Premature death_2010`)),
         prevhosp_2005 = as.numeric(`Preventable hospital stays_2006`)-1/4*(as.numeric(`Preventable hospital stays_2010`)-as.numeric(`Preventable hospital stays_2006`)),
         prevhosp_2010 = as.numeric(`Preventable hospital stays_2010`),
         prevhosp_2015 = as.numeric(`Preventable hospital stays_2015`)) %>%
  select(1,17:32)

# add in tobacco smoking data----

chrd_tob_15 <- read_csv("http://www.countyhealthrankings.org/sites/default/files/2017CHR_CSV_Analytic_Data.csv", skip = 1)
chrd_tob_15 <- chrd_tob_15 %>%
  rename(tob_2015 = "measure_9_value") %>%
  mutate(tob_2015 = as.numeric(tob_2015*100)) %>%
  select(c(fipscode,tob_2015))
chrd <- left_join(chrd,chrd_tob_15, by=c("fipscode"="fipscode"))



url <- "http://www.countyhealthrankings.org/sites/default/files/2012%20County%20Health%20Rankings%20National%20Data_v2.xls"
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
chrd_tob_10 <- read_excel(tf, "Ranked Measure Data",skip = 1)
chrd_tob_10 <- chrd_tob_10 %>%
  rename(tob_10 = "% Smokers") %>%
  mutate(tob_2010 = as.numeric(tob_10),
         tob_2005 = as.numeric(tob_10),
         fipscode = as.integer(FIPS))%>%
  select(c(fipscode,tob_2005,tob_2010)) 
chrd <- left_join(chrd,chrd_tob_10, by=c("fipscode"="fipscode"))
chrd[chrd<0]=0

# Save CHRD  ----------------------------------------------------------------

save(chrd,file="chrd")


# Download urban/rural data ----
library(readr)
# url = ("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/OAE/urbanrural/NCHSURCodes2013.txt")
# ctyurb <- read_table2(url, col.names = FALSE)
ctyurb <- read_table2("data-raw/county/NCHSURCodes2013.txt",col_names = FALSE)
ctyurb <- ctyurb %>%
  rename(fips = X1,
         urb_2015 = X9,
         urb_2005 = X10) %>%
  select(fips,urb_2005,urb_2015)
ctyurb$urb_2005[is.na(ctyurb$urb_2005)]=ctyurb$urb_2015[is.na(ctyurb$urb_2005)]
ctyurb$urb_2010 = round((as.integer(ctyurb$urb_2005)+as.integer(ctyurb$urb_2015))/2)

ctyurb$urb_2005[ctyurb$urb_2005>6]=""
ctyurb$urb_2010[ctyurb$urb_2010>6]=""
ctyurb$urb_2015[ctyurb$urb_2015>6]=""

ctyurb <- ctyurb %>%
  mutate(urb_2005 = as.numeric(urb_2005),
         urb_2015 = as.numeric(urb_2015),
         urb_2010 = as.numeric(urb_2010)) 
save(ctyurb,file="ctyurb")


# Organize into panel data  ---------------------------------------

rm(list=ls())
library(tidyverse)
setwd("~/Data/ahrf")


load("~/Data/ahrf/ahrf_county")
load("~/Data/ahrf/trans_counties")
load("~/Data/ahrf/chrd")
load("~/Data/ahrf/ctyurb")

# County-level non-federal [non-military] MD providers = total general practice (GP and fam med) + general internal medicine ---------------------------------------
# Other providers [DO, NP] available only from 2010

ahrf_county %>% 
  select(county = F04437, 
         fips = F00002, 
         gim_2005 = `F11209-05`, #    Gnrl Int Med, PC, Office Based 
         tgp_2005 = `F08860-05`, #    MD's, Tot Gen Pract, PC,Off Bsd 
         ped_2005 = `F11706-05`, #    Peds, Tot PC, Off Bsd
         pop_2005 = `F11984-05`, 
         gim_2010 = `F11209-10`,
         tgp_2010 = `F08860-10`,
         ped_2010 = `F11706-10`,
         pop_2010 = `F04530-10`, 
         gim_2015 = `F11209-15`,
         tgp_2015 = `F08860-15`,
         ped_2015 = `F11706-15`,
         pop_2015 = `F11984-15`,
         urb_2013 = `F00020-13`, #  Rural-Urban Continuum Code     , See https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/documentation/
         inc_2005 = `F13226-05`, #   Median Household Income             , See https://www.bea.gov/newsreleases/regional/lapi/lapi_newsrelease.htm
         inc_2010 = `F13226-10`,
         inc_2015 = `F13226-15`,
         ed_2006 = `F14480-06`, # % Persons 25+ Yrs w/<HS Diploma  , See https://www.census.gov/programs-surveys/acs/data.html
         ed_2011 = `F14480-11`,
         medct_2010 = `F15299-10`, #  Stan,Risk-Adj Per Cap Medcr Cst,  See https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Geographic-Variation/GV_PUF.html
         medct_2015 = `F15299-15`,
         eld_2005 = `F14083-05`, #  Population Estimate 65+        ,  Census County Char File  
         eld_2010 = `F14840-10`,
         eld_2015 = `F14083-15`,
         fem_2005 = `F13907-05`, # Pop tot fem, Census
         fem_2015 = `F13907-15`,
         blkm_2005 = `F13910-05`, # Pop Black male, Census 
         blkm_2015 = `F13910-15`,
         blkf_2005 = `F13911-05`, # Pop Black fem, Census
         blkf_2015 = `F13911-15`,
         hism_2005 = `F13920-05`, # Pop Hisp male, Census
         hism_2015 = `F13920-15`,
         hisf_2005 = `F13921-05`,
         hisf_2015 = `F13921-15`,
         unemp_2005 = `F06795-05`, #  Unemployment Rate, 16+         ,  Bureau of Labor Stats    
         unemp_2010 = `F06795-10`,
         unemp_2015 = `F06795-15`,
         poll_2006 = `F15338-06`, #  Days w/8-hr Avg Ozone ovr NAAQS,  CDC EPH Tracking Network 
         poll_2010 = `F15338-10`,
         poll_2012 = `F15338-12`,
         pov_2005 = `F13321-05`, #  Percent Persons in Poverty     ,  Census SAIPE             
         pov_2010 = `F13321-10`,
         pov_2015 = `F13321-15`,
         spec_2005 = `F11215-05`, #        M.D.'s, Total Ptn Care Non-Fed                              ,  AMA Phys Master File     
         spec_2010 = `F11215-10`,
         spec_2015 = `F11215-15`, 
         hobed_2005 = `F08921-05`, #  Hospital Beds                  ,  AHA Survey Database
         hobed_2010 = `F08921-10`, 
         hobed_2014 = `F08921-14`,
         unins_2010 = `F14751-10`, #  % <65 without Health Insurance ,  Census SAHIE             
         unins_2015 = `F15474-15`,
         mcare_2011 = `F15549-11`, #  Mdcre Enrllmnt, Agd & Dsbld Tot,  CMS Enroll Dashboard     
         mcare_2015 = `F15549-15`
  ) %>% 
  mutate(pop_2005 = as.integer(pop_2005),
         pop_2010 = as.integer(pop_2010),
         pop_2015 = as.integer(pop_2015),
         fp_2005 = as.integer(tgp_2005)/pop_2005*10000,
         fp_2010 = as.integer(tgp_2010)/pop_2005*10000,
         fp_2015 = as.integer(tgp_2015)/pop_2005*10000,
         gim_2005 = as.integer(gim_2005)/pop_2005*10000,
         gim_2010 = as.integer(gim_2010)/pop_2005*10000,
         gim_2015 = as.integer(gim_2015)/pop_2005*10000,
         pc_2005 = (as.integer(gim_2005)+ as.integer(tgp_2005)+ as.integer(ped_2005))/pop_2005*10000,  # PC providers per 10k pop
         pc_2010 = (as.integer(gim_2010)+as.integer(tgp_2010)+ as.integer(ped_2010))/pop_2010*10000,
         pc_2015 = (as.integer(gim_2015)+ as.integer(tgp_2015)+as.integer(ped_2015))/pop_2015*10000,
         inc_2005 = as.integer(inc_2005)*1.38, # adjust for inflation to 2015 USD, see https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1&year1=200001&year2=201501
         inc_2010 = as.integer(inc_2010)*1.23, # adjust for inflation to 2015 USD, see https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=200501&year2=201501
         inc_2015 = as.integer(inc_2015),
         ed_2005 = (as.integer(ed_2006)-0.2*(as.integer(ed_2011)-as.integer(ed_2006)))/10, # linear interp
         ed_2010 = (as.integer(ed_2011)-0.2*(as.integer(ed_2011)-as.integer(ed_2006)))/10,
         ed_2015 = (as.integer(ed_2011)+0.8*(as.integer(ed_2011)-as.integer(ed_2006)))/10,
         medct_2005 = (as.integer(medct_2010)-(as.integer(medct_2015)-as.integer(medct_2010)))/100, # linear interp
         medct_2010 = as.integer(medct_2010)/100,
         medct_2015 = as.integer(medct_2015)/100,
         eld_2005 = as.integer(eld_2005)/as.integer(pop_2005)*100, # as %age
         eld_2010 = as.integer(eld_2010)/as.integer(pop_2010)*100,
         eld_2015 = as.integer(eld_2015)/as.integer(pop_2015)*100,
         fem_2005 = as.integer(fem_2005)/as.integer(pop_2005)*100, # as %age
         fem_2010 = (as.integer(fem_2005)+as.integer(fem_2015))/2/as.integer(pop_2010)*100, # linear interp
         fem_2015 = as.integer(fem_2015)/as.integer(pop_2015)*100,
         blk_2005 = (as.integer(blkm_2005)+as.integer(blkf_2005))/as.integer(pop_2005)*100,
         blk_2015 = (as.integer(blkm_2015)+as.integer(blkf_2015))/as.integer(pop_2015)*100,
         blk_2010 = (blk_2005+blk_2015)/2,  # linear interp
         his_2005 = (as.integer(hism_2005)+as.integer(hisf_2005))/as.integer(pop_2005)*100,
         his_2015 = (as.integer(hism_2015)+as.integer(hisf_2015))/as.integer(pop_2015)*100,
         his_2010 = (his_2005+his_2015)/2, # linear interp
         unemp_2005 = as.integer(unemp_2005)/10,
         unemp_2010 = as.integer(unemp_2010)/10,
         unemp_2015 = as.integer(unemp_2015)/10,
         poll_2005 = as.integer(poll_2006)-1/4*(as.integer(poll_2010)-as.integer(poll_2006)),
         poll_2010 = as.integer(poll_2010),
         poll_2015 = as.integer(poll_2012)+3/2*(as.integer(poll_2012)-as.integer(poll_2010)),
         pov_2005 = as.integer(pov_2005)/10,
         pov_2010 = as.integer(pov_2010)/10,
         pov_2015 = as.integer(pov_2015)/10,
         spec_2005 = ((as.integer(spec_2005))/pop_2005*10000)-pc_2005,
         spec_2010 = ((as.integer(spec_2010))/pop_2010*10000)-pc_2010,
         spec_2015 = ((as.integer(spec_2015))/pop_2015*10000)-pc_2015,
         hobed_2005 = as.integer(hobed_2005)/pop_2005*10000,
         hobed_2010 = as.integer(hobed_2010)/pop_2010*10000,
         hobed_2015 = (as.integer(hobed_2014)+1/4*(as.integer(hobed_2014)-as.integer(hobed_2010)))/pop_2015*10000,
         unins_2005 = (as.integer(unins_2010)-(as.integer(unins_2015)-as.integer(unins_2010)))/10,
         unins_2010 = as.integer(unins_2010)/10,
         unins_2015 = as.integer(unins_2015)/10,
         mcare_2005 = (as.integer(mcare_2011)-6/4*(as.integer(mcare_2015)-as.integer(mcare_2011)))/pop_2005*100,
         mcare_2010 = (as.integer(mcare_2011)-1/4*(as.integer(mcare_2015)-as.integer(mcare_2011)))/pop_2010*100,
         mcare_2015 = as.integer(mcare_2015)/pop_2015*100
  ) -> ahrf_county
ahrf_county[ahrf_county<0]=0
#lapply(ahrf_county, summary)

ahrf_county <- ahrf_county %>%
  select(county,
         fips,
         fp_2005,
         fp_2010,
         fp_2015,
         gim_2005,
         gim_2010,
         gim_2015,
         pc_2005,
         pc_2010,
         pc_2015,
         inc_2005,
         inc_2010,
         inc_2015,
         ed_2005,
         ed_2010,
         ed_2015,
         medct_2005,
         medct_2010,
         medct_2015,
         eld_2005,
         eld_2010,
         eld_2015,
         fem_2005,
         fem_2010,
         fem_2015,
         blk_2005,
         blk_2010,
         blk_2015,
         his_2005,
         his_2010,
         his_2015,
         unemp_2005,
         unemp_2010,
         unemp_2015,
         poll_2005,
         poll_2010,
         poll_2015,
         pov_2005,
         pov_2010,
         pov_2015,
         spec_2005,
         spec_2010,
         spec_2015,
         hobed_2005,
         hobed_2010,
         hobed_2015,
         unins_2005,
         unins_2010,
         unins_2015,
         mcare_2005,
         mcare_2010,
         mcare_2015)


# Join data to the AHRF subset ----
counties_data <- left_join(ahrf_county,transtot, by=c("fips"="fips"))
counties_data

# Join data to the urban/rural subset ----
counties_data <- left_join(counties_data,ctyurb, by=c("fips"="fips"))
counties_data

# Join data to CHRD files ----
counties_data <- left_join(counties_data,chrd, by=c("fips"="fips"))
counties_data

#lapply(counties_data,summary)

paneldata = counties_data
paneldata = paneldata %>%
  select(-one_of("fips","fipscode"))

# Reshape wide to long; note that guam and puerto rico don't have LE available and make up most of the NA's, so need to exclude them when counting NA's for 50 states+DC ----
paneldata = data.frame(paneldata)
panel = reshape(paneldata, varying =dput(names(paneldata[,2:76])),
                direction="long",idvar="county",sep="_")




# center and scale ----
library(plm)
library(stargazer)

panel$pc = c(scale(panel$pc))
panel$ed = c(scale(panel$ed))
panel$medct = c(scale(panel$medct))
panel$eld = c(scale(panel$eld))
panel$fem = c(scale(panel$fem))
panel$blk = c(scale(panel$blk))
panel$his = c(scale(panel$his))
panel$unemp = c(scale(panel$unemp))
panel$poll = c(scale(panel$poll))
panel$pov = c(scale(panel$pov))
panel$hobed = c(scale(panel$hobed))
panel$unins = c(scale(panel$unins))
panel$mcare = c(scale(panel$mcare))
panel$obese = c(scale(panel$obese))
panel$tob = c(scale(panel$tob))
panel$spec = c(scale(panel$spec))
panel$inc = c(scale(panel$inc/1000))


panel$urb = panel$urb>=5
panel$trans = panel$trans*10



# Regressions  ----

library(lme4)
reg_meu = lmer(trans~pc+ (1+pc| county)+ (1|time) ,
               data = panel)
summary(reg_meu)
confint(reg_meu,method="Wald")

reg_mes = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
               data = panel)
summary(reg_mes)
confint(reg_mes,method="Wald")


paneltest = pdata.frame(panel,index = c("county","time"))
mylag <- function(x,lag) {
  c(rep(NA,lag),head(x,-lag))
}
dd=transform(paneltest,lagpc1=mylag(pc,1))
dd$pc[dd$time==2005]='NA'

reg_mel = lmer(trans~lagpc1+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+lagpc1| county)+ (1|time) ,
               data = dd)
summary(reg_mel)
confint(reg_mel,method="Wald") 




reg_mef = lmer(trans~fp+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
               data = panel)
summary(reg_mef)
confint(reg_mef,method="Wald")



reg_meg = lmer(trans~gim+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
               data = panel)
summary(reg_meg)
confint(reg_meg,method="Wald")




panelurb = panel[panel$urb==1,]
reg_meu = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
               data = panelurb)
summary(reg_meu)
confint(reg_meu,method="Wald")

panelrur = panel[panel$urb==0,]
reg_mer = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
               data = panelrur)
summary(reg_mer)
confint(reg_mer,method="Wald")



panellopov = panel[panel$pov<(-0.1584),]
reg_melp = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
                data = panellopov)
summary(reg_melp)
confint(reg_melp,method="Wald")

panelhipov = panel[panel$pov>(-0.1584),]
reg_mehp = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
                data = panelhipov)
summary(reg_mehp)
confint(reg_mehp,method="Wald")



panelloblk = panel[panel$blk<(-0.47508),]
reg_melb = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
                data = panelloblk)
summary(reg_melb)
confint(reg_melb,method="Wald")

panelhiblk = panel[panel$blk>(-0.47508),]
reg_mehb = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
                data = panelhiblk)
summary(reg_mehb)
confint(reg_mehb,method="Wald")



panellohis = panel[panel$his<(-0.38146),]
reg_melh = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1+pc| county)+ (1|time) ,
                data = panellohis)
summary(reg_melh)
confint(reg_melh,method="Wald")

panelhihis = panel[panel$his>(-0.38146),]
reg_mehh = lmer(trans~pc+urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec + (1|time) ,
                data = panelhihis)
summary(reg_mehh)
confint(reg_mehh,method="Wald")

