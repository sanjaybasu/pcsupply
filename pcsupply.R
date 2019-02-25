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
#install.packages("mctest)
#install.packages("olsrr")
#install.packages("lme4")
#install.packages("vioplot")

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
doc_src = "data-raw/county/AHRF 2016-2017 Technical Documentation.xlsx"


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


# Delete raw data as it’s too large ---------------------------------------

unlink(raw_src)

# Download raw IHME LE files ---------------------------------------
# See https://github.com/BuzzFeedNews/2017-05-us-health-care/blob/master/index.Rmd

url = "http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_XLSX.zip"
fil_zip = tempfile(fileext = ".zip")

{
  download.file(url, fil_zip)
  dir.create("data-raw/county")
  unzip(fil_zip, exdir = "data-raw/county", junkpaths = TRUE)
}
list.files("data-raw/county")

raw_src = "data-raw/county/IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_Y2017M05D08.xlsx" # Raw data

life_expect_us <- read_excel(raw_src, sheet = 1, skip = 3, col_names = FALSE) %>%
  select(1:10)
names(life_expect_us) <- c("place","fips","le_1980","le_1985","le_1990","le_1995","le_2000","le_2005","le_2010","le_2014")

# Clean the FIPS codes, adding zeros where necessary ---------------------------------------
life_expect_us <- life_expect_us %>%
  mutate(fips = ifelse(nchar(fips)==4|nchar(fips)==1,paste0("0",fips),fips))

# Extract data for counties only ---------------------------------------
life_expect_counties <- life_expect_us %>%
  filter(grepl(",",place)) %>%
  separate(place, into=c("place","state"), sep = ", ")

# Select columns with life expectancy data and convert from text string with confidence intervals to numbers ---------------------------------------
counties_clean <- life_expect_counties %>%
  select(4:11) %>%
  mutate_all(funs(as.numeric(substring(.,1,5))))

# Create data frame with names and abbreviations for states and District of Columbia  ---------------------------------------
states <- data_frame(state.name,state.abb)
names(states) <- c("state","abb")
dc <- data_frame("District of Columbia", "DC")
names(dc) <- c("state","abb")
states <- bind_rows(states,dc) 

# Join that to the counties' names, states, and FIPS codes ---------------------------------------
states_names <- life_expect_counties %>%
  select(1:3) %>%
  inner_join(states) %>%
  mutate(place = paste0(place,", ",abb))

# Recombine AHRF with processed life expectancy data ---------------------------------------
life_expect_counties <- bind_cols(states_names,counties_clean) 


# Linearly impute 2015 LE ----
life_expect_counties$le_2015 = life_expect_counties$le_2014 + 1/4*(life_expect_counties$le_2014-life_expect_counties$le_2010)

# keep relevant years ----
life_expect_counties <- life_expect_counties %>%
  select(fips, le_2005, le_2010, le_2015)

# Save LE  ----------------------------------------------------------------

save(life_expect_counties,file="life_expect_counties")



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
  select(c("fips","Adult obesity_2005","Adult obesity_2010","Adult obesity_2013")) %>%
  mutate(fipscode = as.character(fips),
         obese_2005 = `Adult obesity_2005`*100,
         obese_2010 = as.numeric(`Adult obesity_2010`)*100,
         obese_2015 = obese_2010+5/3*(as.numeric(`Adult obesity_2013`)*100-as.numeric(`Adult obesity_2010`)*100)
  ) %>%
  select(c("fipscode","obese_2005","obese_2010","obese_2015"))



# add in tobacco smoking data----

chrd_tob_15 <- read_csv("http://www.countyhealthrankings.org/sites/default/files/2017CHR_CSV_Analytic_Data.csv", skip = 1)
chrd_tob_15 <- chrd_tob_15 %>%
  rename(tob_2015 = "measure_9_value") %>%
  mutate(tob_2015 = as.numeric(tob_2015*100),
         fipscode = as.character(fipscode)) %>%
  select(c(fipscode,tob_2015)) 
chrd <- left_join(chrd,chrd_tob_15, by=c("fipscode"="fipscode"))



#url <- "http://www.countyhealthrankings.org/sites/default/files/2012%20County%20Health%20Rankings%20National%20Data_v2.xls"
#GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
#chrd_tob_10 <- read_excel(tf, "Ranked Measure Data",skip = 1)
chrd_tob_10 = read.csv("~/Downloads/2012 County Health Rankings National Data_v2.csv")
chrd_tob_10 <- chrd_tob_10 %>%
  rename(tob_10 = "X..Smokers") %>%
  mutate(tob_2010 = as.numeric(tob_10),
         tob_2005 = as.numeric(tob_10),
         fipscode = as.character(FIPS))%>%
  select(c(fipscode,tob_2005,tob_2010)) 
chrd <- left_join(chrd,chrd_tob_10, by=c("fipscode"="fipscode"))
chrd[chrd<0]=0

# Save CHRD  ----------------------------------------------------------------

save(chrd,file="chrd")


# Download urban/rural data ----
#https://www.cdc.gov/nchs/data/oae/NCHSUrbruralFileDocumentation.pdf
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

ctyurb <- ctyurb %>%
  mutate(urb_2005 = as.numeric(urb_2005),
         urb_2015 = as.numeric(urb_2015),
         urb_2010 = as.numeric(urb_2010)) 


ctyurb$urb_2005[ctyurb$urb_2005>6]=NA
ctyurb$urb_2010[ctyurb$urb_2010>6]=NA
ctyurb$urb_2015[ctyurb$urb_2015>6]=NA

ctyurb$urb_2005[ctyurb$urb_2005<5]=0
ctyurb$urb_2005[ctyurb$urb_2005>=5]=1
ctyurb$urb_2010[ctyurb$urb_2010<5]=0
ctyurb$urb_2010[ctyurb$urb_2010>=5]=1
ctyurb$urb_2015[ctyurb$urb_2015<5]=0
ctyurb$urb_2015[ctyurb$urb_2015>=5]=1

save(ctyurb,file="ctyurb")

# Organize into panel data  ---------------------------------------

rm(list=ls())
library(tidyverse)
setwd("~/Data/ahrf")


load("~/Data/ahrf/ahrf_county")
load("~/Data/ahrf/life_expect_counties")
load("~/Data/ahrf/chrd")
load("~/Data/ahrf/ctyurb")

# County-level non-federal [non-military] MD providers = total general practice (GP and fam med) + general internal medicine ---------------------------------------
# Other providers [DO, NP] available only from 2010

ahrf_county %>% 
  select(county = F04437, 
         fips = F00002, 
         gim_2005 = `F11209-05`, #    Gnrl Int Med, PC, Office Based 
         fp_2005 = `F08860-05`, #    MD's, Tot Gen Pract, PC,Off Bsd 
         ped_2005 = `F11706-05`, #    Peds, Tot PC, Off Bsd
         pop_2005 = `F11984-05`, 
         gim_2010 = `F11209-10`,
         fp_2010 = `F08860-10`,
         ped_2010 = `F11706-10`,
         pop_2010 = `F04530-10`, 
         gim_2015 = `F11209-15`,
         fp_2015 = `F08860-15`,
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
         tot_2005 = `F11215-05`, #        M.D.'s, Total Ptn Care Non-Fed                              ,  AMA Phys Master File     
         tot_2010 = `F11215-10`,
         tot_2015 = `F11215-15`, 
         hobed_2005 = `F08921-05`, #  Hospital Beds                  ,  AHA Survey Database
         hobed_2010 = `F08921-10`, 
         hobed_2014 = `F08921-14`,
         unins_2010 = `F14751-10`, #  % <65 without Health Insurance ,  Census SAHIE             
         unins_2015 = `F15474-15`,
         mcare_2011 = `F15549-11`, #  Mdcre Enrllmnt, Agd & Dsbld Tot,  CMS Enroll Dashboard     
         mcare_2015 = `F15549-15`,
         homeval_2006 = `F14613-06`, # median home value
         homeval_2011 = `F14613-11`
  ) %>% 
  mutate(pop_2005 = as.integer(pop_2005),
         pop_2010 = as.integer(pop_2010),
         pop_2015 = as.integer(pop_2015),
         fp_2005 = as.integer(fp_2005),
         fp_2010 = as.integer(fp_2010),
         fp_2015 = as.integer(fp_2015),
         gim_2005 = as.integer(gim_2005),
         gim_2010 = as.integer(gim_2010),
         gim_2015 = as.integer(gim_2015),
         ped_2005 = as.integer(ped_2005),
         ped_2010 = as.integer(ped_2010),
         ped_2015 = as.integer(ped_2015),
         pc_2005 = (as.integer(gim_2005)+as.integer(fp_2005)+as.integer(ped_2005))/pop_2005*100000,  # PC providers per 100k pop
         pc_2010 = (as.integer(gim_2010)+as.integer(fp_2010)+as.integer(ped_2010))/pop_2010*100000,
         pc_2015 = (as.integer(gim_2015)+as.integer(fp_2015)+as.integer(ped_2015))/pop_2015*100000,
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
         fem_2010 = (as.integer(fem_2005)+as.integer(fem_2015))/2/as.integer(pop_2010)*100, # linear interp
         fem_2005 = as.integer(fem_2005)/as.integer(pop_2005)*100, # as %age
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
         spec_2005 = ((as.integer(tot_2005))/pop_2005*100000)-pc_2005,
         spec_2010 = ((as.integer(tot_2010))/pop_2010*100000)-pc_2010,
         spec_2015 = ((as.integer(tot_2015))/pop_2015*100000)-pc_2015,
         hobed_2015 = (as.integer(hobed_2014)+1/4*(as.integer(hobed_2014)-as.integer(hobed_2010)))/pop_2015*100000,
         hobed_2005 = as.integer(hobed_2005)/pop_2005*100000,
         hobed_2010 = as.integer(hobed_2010)/pop_2010*100000,
         unins_2005 = (as.integer(unins_2010)-(as.integer(unins_2015)-as.integer(unins_2010)))/10,
         unins_2010 = as.integer(unins_2010)/10,
         unins_2015 = as.integer(unins_2015)/10,
         mcare_2005 = (as.integer(mcare_2011)-6/4*(as.integer(mcare_2015)-as.integer(mcare_2011)))/pop_2005*100,
         mcare_2010 = (as.integer(mcare_2011)-1/4*(as.integer(mcare_2015)-as.integer(mcare_2011)))/pop_2010*100,
         mcare_2015 = as.integer(mcare_2015)/pop_2015*100,
         homeval_2005 = as.integer(homeval_2006)*1.18-0.2*(as.integer(homeval_2011)*1.06-as.integer(homeval_2006)*1.18), # linear interp w/ CPI adjustment to 2015 $US
         homeval_2010 = as.integer(homeval_2011)*1.06-0.2*(as.integer(homeval_2011)*1.06-as.integer(homeval_2006)*1.18),
         homeval_2015 = as.integer(homeval_2011)*1.06+0.8*(as.integer(homeval_2011)*1.06-as.integer(homeval_2006)*1.18)
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
         ped_2005,
         ped_2010,
         ped_2015,
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
         mcare_2015,
         homeval_2005,
         homeval_2010,
         homeval_2015,
         tot_2005,
         tot_2010,
         tot_2015,
         pop_2005,
         pop_2010,
         pop_2015)


# Join data to the AHRF subset ----
counties_data <- left_join(ahrf_county,life_expect_counties, by=c("fips"="fips"))
counties_data

# Join data to the urban/rural subset ----
counties_data <- left_join(counties_data,ctyurb, by=c("fips"="fips"))
counties_data

# Join data to CHRD files ----
counties_data <- left_join(counties_data,chrd, by=c("fips"="fipscode"))
counties_data

#lapply(counties_data,summary)

paneldata = counties_data
paneldata = paneldata %>%
  select(-one_of("fips"))

# Reshape wide to long; note that guam and puerto rico don't have cvd available and make up most of the NA's, so need to exclude them when counting NA's for 50 states+DC ----
paneldata = data.frame(paneldata)
panel = reshape(paneldata, varying =dput(names(paneldata[,2:dim(paneldata)[2]])),
                direction="long",idvar="county",sep="_")


plot(panel$pc,panel$le)
plot(log(panel$pc),(panel$le), xlab = "Log (# of primary care physicians/10k pop)", ylab = "Age-adjusted life expectancy at birth (years)")


save(panel,file="pcpanel")
save(paneldata,file="pcpaneldata")




# descriptive stats ----
rm(list=ls())
library(tidyverse)
library(stargazer)
setwd("~/Data/ahrf")
load("~/Data/ahrf/pcpanel")
load("~/Data/ahrf/pcpaneldata")

panel$tot = panel$pc + panel$spec
panel05 = panel[,2:dim(panel)[2]]
panel05 = panel05[panel05$time==2005,]
panel10 = panel[,2:dim(panel)[2]]
panel10 = panel10[panel10$time==2010,]
panel15 = panel[,2:dim(panel)[2]]
panel15 = panel15[panel15$time==2015,]
paneldiff = panel15-panel05
tableone05 = matrix(0,nrow=dim(panel)[2]-1,ncol=3)
tableone10 = matrix(0,nrow=dim(panel)[2]-1,ncol=3)
tableone15 = matrix(0,nrow=dim(panel)[2]-1,ncol=3)
tableoned = matrix(0,nrow=dim(panel)[2]-1,ncol=3)

for (i in 1:(dim(panel)[2]-1)){
  tableone05[i,1:3] = c(mean(na.omit(panel05[,i])),quantile(panel05[,i],na.rm=T,c(.025,.975))[1],quantile(panel05[,i],na.rm=T,c(.025,.975))[2])
  tableone10[i,1:3] = c(mean(na.omit(panel10[,i])),quantile(panel10[,i],na.rm=T,c(.025,.975))[1],quantile(panel10[,i],na.rm=T,c(.025,.975))[2])
  tableone15[i,1:3] = c(mean(na.omit(panel15[,i])),quantile(panel15[,i],na.rm=T,c(.025,.975))[1],quantile(panel15[,i],na.rm=T,c(.025,.975))[2])
  tableoned[i,1:3] = c(mean(na.omit(paneldiff[,i])),quantile(paneldiff[,i],na.rm=T,c(.025,.975))[1],quantile(paneldiff[,i],na.rm=T,c(.025,.975))[2])
}
colnames(panel05)
table05=cbind(colnames(panel05),tableone05)
table10=cbind(colnames(panel10),tableone10)
table15=cbind(colnames(panel15),tableone15)
tabled = cbind(colnames(paneldiff),tableoned)

stargazer(table05,type="text")
stargazer(table10,type="text")
stargazer(table15,type="text")
stargazer(tabled,type="text")


sum(na.omit(panel$pc[panel$time==2005]==0))
sum(na.omit(panel$pc[panel$time==2015]==0))

sum(na.omit(panel$pc[panel$time==2005]>100))
sum(na.omit(panel$pc[panel$time==2015]>100))

pc_gainloss = (panel$pc[panel$time==2015]-panel$pc[panel$time==2005])
summary(pc_gainloss)
summary(pc_gainloss[panel$urb==0])
summary(pc_gainloss[panel$urb==1])
summary(pc_gainloss[panel$pov<16])
summary(pc_gainloss[panel$pov>=16])
summary(pc_gainloss[panel$blk<9])
summary(pc_gainloss[panel$blk>=9])
summary(pc_gainloss[panel$his<8])
summary(pc_gainloss[panel$his>=8])

quantile(pc_gainloss,c(.025,.975),na.rm=T)
quantile(pc_gainloss[panel$urb==0],c(.025,.975),na.rm=T)
quantile(pc_gainloss[panel$urb==1],c(.025,.975),na.rm=T)
quantile(pc_gainloss[panel$pov<16],c(.025,.975),na.rm=T)
quantile(pc_gainloss[panel$pov>=16],c(.025,.975),na.rm=T)
quantile(pc_gainloss[panel$blk<9],c(.025,.975),na.rm=T)
quantile(pc_gainloss[panel$blk>=9],c(.025,.975),na.rm=T)
quantile(pc_gainloss[panel$his<8],c(.025,.975),na.rm=T)
quantile(pc_gainloss[panel$his>=8],c(.025,.975),na.rm=T)



pcn_gainloss = (panel$fp[panel$time==2015]-panel$fp[panel$time==2005])+
  (panel$gim[panel$time==2015]-panel$gim[panel$time==2005])+
  (panel$ped[panel$time==2015]-panel$ped[panel$time==2005])
summary(pcn_gainloss)
summary(pcn_gainloss[panel$urb==0])
summary(pcn_gainloss[panel$urb==1])
summary(pcn_gainloss[panel$pov<16])
summary(pcn_gainloss[panel$pov>=16])
summary(pcn_gainloss[panel$blk<9])
summary(pcn_gainloss[panel$blk>=9])
summary(pcn_gainloss[panel$his<8])
summary(pcn_gainloss[panel$his>=8])

quantile(pcn_gainloss,c(.025,.975),na.rm=T)
quantile(pcn_gainloss[panel$urb==0],c(.025,.975),na.rm=T)
quantile(pcn_gainloss[panel$urb==1],c(.025,.975),na.rm=T)
quantile(pcn_gainloss[panel$pov<16],c(.025,.975),na.rm=T)
quantile(pcn_gainloss[panel$pov>=16],c(.025,.975),na.rm=T)
quantile(pcn_gainloss[panel$blk<9],c(.025,.975),na.rm=T)
quantile(pcn_gainloss[panel$blk>=9],c(.025,.975),na.rm=T)
quantile(pcn_gainloss[panel$his<8],c(.025,.975),na.rm=T)
quantile(pcn_gainloss[panel$his>=8],c(.025,.975),na.rm=T)


spec_gainloss = (panel$spec[panel$time==2015]-panel$spec[panel$time==2005])
summary(spec_gainloss)
summary(spec_gainloss[panel$urb==0])
summary(spec_gainloss[panel$urb==1])
summary(spec_gainloss[panel$pov<16])
summary(spec_gainloss[panel$pov>=16])
summary(spec_gainloss[panel$blk<9])
summary(spec_gainloss[panel$blk>=9])
summary(spec_gainloss[panel$his<8])
summary(spec_gainloss[panel$his>=8])

quantile(spec_gainloss,c(.025,.975),na.rm=T)
quantile(spec_gainloss[panel$urb==0],c(.025,.975),na.rm=T)
quantile(spec_gainloss[panel$urb==1],c(.025,.975),na.rm=T)
quantile(spec_gainloss[panel$pov<16],c(.025,.975),na.rm=T)
quantile(spec_gainloss[panel$pov>=16],c(.025,.975),na.rm=T)
quantile(spec_gainloss[panel$blk<9],c(.025,.975),na.rm=T)
quantile(spec_gainloss[panel$blk>=9],c(.025,.975),na.rm=T)
quantile(spec_gainloss[panel$his<8],c(.025,.975),na.rm=T)
quantile(spec_gainloss[panel$his>=8],c(.025,.975),na.rm=T)

specn_gainloss = ((panel$tot[panel$time==2015]-panel$pc[panel$time==2015])-
  (panel$tot[panel$time==2005]-panel$pc[panel$time==2005]))*(panel$pop[panel$time==2005])/100000
summary(specn_gainloss)
summary(specn_gainloss[panel$urb==0])
summary(specn_gainloss[panel$urb==1])
summary(specn_gainloss[panel$pov<16])
summary(specn_gainloss[panel$pov>=16])
summary(specn_gainloss[panel$blk<9])
summary(specn_gainloss[panel$blk>=9])
summary(specn_gainloss[panel$his<8])
summary(specn_gainloss[panel$his>=8])

quantile(specn_gainloss,c(.025,.975),na.rm=T)
quantile(specn_gainloss[panel$urb==0],c(.025,.975),na.rm=T)
quantile(specn_gainloss[panel$urb==1],c(.025,.975),na.rm=T)
quantile(specn_gainloss[panel$pov<16],c(.025,.975),na.rm=T)
quantile(specn_gainloss[panel$pov>=16],c(.025,.975),na.rm=T)
quantile(specn_gainloss[panel$blk<9],c(.025,.975),na.rm=T)
quantile(specn_gainloss[panel$blk>=9],c(.025,.975),na.rm=T)
quantile(specn_gainloss[panel$his<8],c(.025,.975),na.rm=T)
quantile(specn_gainloss[panel$his>=8],c(.025,.975),na.rm=T)


cor.test(pc_gainloss,spec_gainloss)
cor.test(pcn_gainloss,specn_gainloss)


# center and scale ----
rm(list=ls())
library(tidyverse)
library(plm)
library(lme4)
library(stargazer)
setwd("~/Data/ahrf")
load("~/Data/ahrf/pcpanel")
load("~/Data/ahrf/pcpaneldata")

panel$tot = panel$pc + panel$spec

zpanel = panel
zpanel$fp = log(zpanel$fp+1)
zpanel$gim = log(zpanel$gim+1)
zpanel$pc = log(zpanel$pc+1)
zpanel$inc = log(zpanel$inc)
zpanel$ed = log(zpanel$ed+1)
zpanel$medct = log(zpanel$medct)
zpanel$eld = log(zpanel$eld)
zpanel$fem = log(zpanel$fem)
zpanel$blk = log(zpanel$blk+1)
zpanel$his = log(zpanel$his+1)
zpanel$unemp = log(zpanel$unemp)
zpanel$poll = log(zpanel$poll+1)
zpanel$pov = log(zpanel$pov)
zpanel$spec = log(zpanel$spec+1)
zpanel$hobed = log(zpanel$hobed+1)
zpanel$unins = log(zpanel$unins)
zpanel$mcare = log(zpanel$mcare+1)
zpanel$obese = log(zpanel$obese)
zpanel$tot = log(zpanel$tot+1)
zpanel$tob = log(zpanel$tob)
zpanel$homeval = log(zpanel$homeval+1)

exp(mean(na.omit(log(panel$tot+1))))-exp(sd(na.omit(log(panel$tot+1))))
exp(mean(na.omit(log(panel$tot+1))))+exp(sd(na.omit(log(panel$tot+1))))
exp(mean(na.omit(log(panel$pc+1))))-exp(sd(na.omit(log(panel$pc+1))))
exp(mean(na.omit(log(panel$pc+1))))+exp(sd(na.omit(log(panel$pc+1))))
exp(mean(na.omit(log(panel$spec+1))))-exp(sd(na.omit(log(panel$spec+1))))
exp(mean(na.omit(log(panel$spec+1))))+exp(sd(na.omit(log(panel$spec+1))))
exp(mean(na.omit(log(panel$pov))))-exp(sd(na.omit(log(panel$pov))))
exp(mean(na.omit(log(panel$pov))))+exp(sd(na.omit(log(panel$pov))))
exp(mean(na.omit(log(panel$ed+1))))-exp(sd(na.omit(log(panel$ed+1))))
exp(mean(na.omit(log(panel$ed+1))))+exp(sd(na.omit(log(panel$ed+1))))
exp(mean(na.omit(log(panel$fem))))-exp(sd(na.omit(log(panel$fem))))
exp(mean(na.omit(log(panel$fem))))+exp(sd(na.omit(log(panel$fem))))
exp(mean(na.omit(log(panel$blk+1))))-exp(sd(na.omit(log(panel$blk+1))))
exp(mean(na.omit(log(panel$blk+1))))+exp(sd(na.omit(log(panel$blk+1))))
exp(mean(na.omit(log(panel$his+1))))-exp(sd(na.omit(log(panel$his+1))))
exp(mean(na.omit(log(panel$his+1))))+exp(sd(na.omit(log(panel$his+1))))
exp(mean(na.omit(log(panel$unemp))))-exp(sd(na.omit(log(panel$unemp))))
exp(mean(na.omit(log(panel$unemp))))+exp(sd(na.omit(log(panel$unemp))))
exp(mean(na.omit(log(panel$hobed+1))))-exp(sd(na.omit(log(panel$hobed+1))))
exp(mean(na.omit(log(panel$hobed+1))))+exp(sd(na.omit(log(panel$hobed+1))))
exp(mean(na.omit(log(panel$mcare+1))))-exp(sd(na.omit(log(panel$mcare+1))))
exp(mean(na.omit(log(panel$mcare+1))))+exp(sd(na.omit(log(panel$mcare+1))))
exp(mean(na.omit(log(panel$medct))))-exp(sd(na.omit(log(panel$medct))))
exp(mean(na.omit(log(panel$medct))))+exp(sd(na.omit(log(panel$medct))))
exp(mean(na.omit(log(panel$tob))))-exp(sd(na.omit(log(panel$tob))))
exp(mean(na.omit(log(panel$tob))))+exp(sd(na.omit(log(panel$tob))))
exp(mean(na.omit(log(panel$obese))))-exp(sd(na.omit(log(panel$obese))))
exp(mean(na.omit(log(panel$obese))))+exp(sd(na.omit(log(panel$obese))))
exp(mean(na.omit(log(panel$poll+1))))-exp(sd(na.omit(log(panel$poll+1))))
exp(mean(na.omit(log(panel$poll+1))))+exp(sd(na.omit(log(panel$poll+1))))
exp(mean(na.omit(log(panel$homeval+1))))-exp(sd(na.omit(log(panel$homeval+1))))
exp(mean(na.omit(log(panel$homeval+1))))+exp(sd(na.omit(log(panel$homeval+1))))


zpanel$fp = (zpanel$fp-mean(na.omit(zpanel$fp)))/(2*sd(na.omit(zpanel$fp)))
zpanel$gim = (zpanel$gim-mean(na.omit(zpanel$gim)))/(2*sd(na.omit(zpanel$gim)))
zpanel$pc = (zpanel$pc-mean(na.omit(zpanel$pc)))/(2*sd(na.omit(zpanel$pc)))
zpanel$spec = (zpanel$spec-mean(na.omit(zpanel$spec)))/(2*sd(na.omit(zpanel$spec)))
zpanel$tot = (zpanel$tot-mean(na.omit(zpanel$tot)))/(2*sd(na.omit(zpanel$tot)))
zpanel$ed = (zpanel$ed-mean(na.omit(zpanel$ed)))/(2*sd(na.omit(zpanel$ed)))
zpanel$medct = (zpanel$medct-mean(na.omit(zpanel$medct)))/(2*sd(na.omit(zpanel$medct)))
zpanel$eld = (zpanel$eld-mean(na.omit(zpanel$eld)))/(2*sd(na.omit(zpanel$eld)))
zpanel$fem = (zpanel$fem-mean(na.omit(zpanel$fem)))/(2*sd(na.omit(zpanel$fem)))
zpanel$blk = (zpanel$blk-mean(na.omit(zpanel$blk)))/(2*sd(na.omit(zpanel$blk)))
zpanel$his = (zpanel$his-mean(na.omit(zpanel$his)))/(2*sd(na.omit(zpanel$his)))
zpanel$unemp = (zpanel$unemp-mean(na.omit(zpanel$unemp)))/(2*sd(na.omit(zpanel$unemp)))
zpanel$poll = (zpanel$poll-mean(na.omit(zpanel$poll)))/(2*sd(na.omit(zpanel$poll)))
zpanel$pov = (zpanel$pov-mean(na.omit(zpanel$pov)))/(2*sd(na.omit(zpanel$pov)))
zpanel$hobed = (zpanel$hobed-mean(na.omit(zpanel$hobed)))/(2*sd(na.omit(zpanel$hobed)))
zpanel$unins = (zpanel$unins-mean(na.omit(zpanel$unins)))/(2*sd(na.omit(zpanel$unins)))
zpanel$mcare = (zpanel$mcare-mean(na.omit(zpanel$mcare)))/(2*sd(na.omit(zpanel$mcare)))
zpanel$obese = (zpanel$obese-mean(na.omit(zpanel$obese)))/(2*sd(na.omit(zpanel$obese)))
zpanel$tob = (zpanel$tob-mean(na.omit(zpanel$tob)))/(2*sd(na.omit(zpanel$tob)))
zpanel$inc = (zpanel$inc-mean(na.omit(zpanel$inc)))/(2*sd(na.omit(zpanel$inc)))
zpanel$homeval = (zpanel$homeval-mean(na.omit(zpanel$homeval)))/(2*sd(na.omit(zpanel$homeval)))

zpanel$le = zpanel$le*365.25


# address collinearity with VIFs----
# paneltest = (panel[complete.cases(panel),])
# paneltest = data.frame(paneltest[,3:25])
# x = as.matrix(paneltest[,1:15,17:23])
# y = as.matrix(paneltest[,16])
# library(mctest)
# omcdiag(x,y)
# imcdiag(x,y)
# 
# library(olsrr)
# testmodel = lm(le~.,
#                data = panel)
# ols_vif_tol(testmodel)
# 
# testmodel = lm(le~pc+
#                  urb+ed+medct+fem+blk+his+unemp+poll+pov+hobed+mcare+obese+tob+spec+homeval,
#                data = panel)
# ols_vif_tol(testmodel)

# Regressions  ----



reg_min = (lmer(le~pc+ (1+pc| county)+ (1|time) ,
                data = zpanel))
stargazer(reg_min,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)


reg_tot = (lmer(le~tot+urb+pov+ed+fem+blk+his+unemp+hobed+mcare+medct+tob+obese+poll+homeval+ (1+tot| county)+ (1|time) ,
                data = zpanel))
stargazer(reg_tot,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)
lotot=exp(mean(na.omit(log(panel$tot+1))))-exp(sd(na.omit(log(panel$tot+1))))
hitot=exp(mean(na.omit(log(panel$tot+1))))+exp(sd(na.omit(log(panel$tot+1))))
lotot
hitot
summary(reg_tot)$coefficients[2]/(hitot-lotot)



reg_pc = (lmer(le~pc+urb+pov+ed+fem+blk+his+unemp+hobed+mcare+medct+tob+obese+poll+homeval+ (1+pc| county)+ (1|time) ,
               data = zpanel))
stargazer(reg_pc,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)
lopc=exp(mean(na.omit(log(panel$pc+1))))-exp(sd(na.omit(log(panel$pc+1))))
hipc=exp(mean(na.omit(log(panel$pc+1))))+exp(sd(na.omit(log(panel$pc+1))))
lopc
hipc
summary(reg_pc)$coefficients[2]/(hipc-lopc)


reg_spec = (lmer(le~spec+urb+pov+ed+fem+blk+his+unemp+hobed+mcare+medct+tob+obese+poll+homeval+ (1+spec| county)+ (1|time) ,
                 data = zpanel))
stargazer(reg_spec,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)
lospec=exp(mean(na.omit(log(panel$spec+1))))-exp(sd(na.omit(log(panel$spec+1))))
hispec=exp(mean(na.omit(log(panel$spec+1))))+exp(sd(na.omit(log(panel$spec+1))))
lospec
hispec
summary(reg_spec)$coefficients[2]/(hispec-lospec)


reg_base = (lmer(le~pc+spec+urb+pov+ed+fem+blk+his+unemp+hobed+mcare+medct+tob+obese+poll+homeval+(1+pc| county)+ (1|time) ,
                 data = zpanel))
stargazer(reg_base,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)
summary(reg_base)$coefficients[2]/(hipc-lopc)
summary(reg_base)$coefficients[3]/(hispec-lospec)

confint(reg_base,method="Wald")[7]/(hipc-lopc)
confint(reg_base,method="Wald")[29]/(hipc-lopc)

confint(reg_base,method="Wald")[8]/(hispec-lospec)
confint(reg_base,method="Wald")[30]/(hispec-lospec)




# Sensitivity analyses ----------




reg_melu = lmer(le~pc+pc*urb+spec+urb+pov+ed+fem+blk+his+unemp+hobed+mcare+medct+tob+obese+poll+homeval + (1+pc| county)+ (1|time) ,
                data = zpanel)
stargazer(reg_melu,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)


reg_melp = lmer(le~pc+pc*pov+spec+urb+pov+ed+fem+blk+his+unemp+hobed+mcare+medct+tob+obese+poll+homeval + (1+pc| county)+ (1|time) ,
                data = zpanel)
stargazer(reg_melp,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)


reg_melb = lmer(le~pc+pc*blk+spec+urb+pov+ed+fem+blk+his+unemp+hobed+mcare+medct+tob+obese+poll+homeval + (1+pc| county)+ (1|time) ,
                data = zpanel)
stargazer(reg_melb,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)


reg_melh = lmer(le~pc+pc*his+spec+urb+pov+ed+fem+blk+his+unemp+hobed+mcare+medct+tob+obese+poll+homeval + (1+pc| county)+ (1|time) ,
                data = zpanel)
stargazer(reg_melh,type="text",style="asq",single.row=T,
          ci = T,digits=1,
          ci.level = 0.95)





