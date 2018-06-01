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
         fp_2005 = as.integer(tgp_2005)/pop_2005*100000,
         fp_2010 = as.integer(tgp_2010)/pop_2005*100000,
         fp_2015 = as.integer(tgp_2015)/pop_2005*100000,
         gim_2005 = as.integer(gim_2005)/pop_2005*100000,
         gim_2010 = as.integer(gim_2010)/pop_2005*100000,
         gim_2015 = as.integer(gim_2015)/pop_2005*100000,
         pc_2005 = (as.integer(gim_2005)+ as.integer(tgp_2005)+ as.integer(ped_2005))/pop_2005*100000,  # PC providers per 10k pop
         pc_2010 = (as.integer(gim_2010)+as.integer(tgp_2010)+ as.integer(ped_2010))/pop_2010*100000,
         pc_2015 = (as.integer(gim_2015)+ as.integer(tgp_2015)+as.integer(ped_2015))/pop_2015*100000,
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
         spec_2005 = ((as.integer(spec_2005))/pop_2005*100000)-pc_2005,
         spec_2010 = ((as.integer(spec_2010))/pop_2010*100000)-pc_2010,
         spec_2015 = ((as.integer(spec_2015))/pop_2015*100000)-pc_2015,
         hobed_2005 = as.integer(hobed_2005)/pop_2005*100000,
         hobed_2010 = as.integer(hobed_2010)/pop_2010*100000,
         hobed_2015 = (as.integer(hobed_2014)+1/4*(as.integer(hobed_2014)-as.integer(hobed_2010)))/pop_2015*100000,
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
counties_data <- left_join(ahrf_county,life_expect_counties, by=c("fips"="fips"))
counties_data

# Join data to the urban/rural subset ----
counties_data <- left_join(counties_data,ctyurb, by=c("fips"="fips"))
counties_data

# Join data to CHRD files ----
counties_data <- left_join(counties_data,chrd, by=c("fips"="fips"))
counties_data

require(maps)
require(ggmap)
map("county")

# data(unemp)
# data(county.fips)

# Plot unemployment by country
colors = c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", 
           "#4575b4")
# head(unemp)
# 
# head(county.fips)

# 
# unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 
#                                                     10, 100)))
# colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)]
# 
# map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
#     lty = 0, projection = "polyconic")

counties_data$pcchange = counties_data$pc_2015-counties_data$pc_2005

counties_data$colorBuckets <- as.numeric(cut(counties_data$pcchange, c(-10000,-10,-5,0,5,10,10000)))

colorsmatched <- counties_data$colorBuckets[match(county.fips$fips, counties_data$fipscode)]


#### FIGURE 1 -----


map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")


# Add border around each State
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("Change in primary care physician density, 2005-2015")

leg.txt <- c("<-10", "-10 to <-5", "-5 to <0", "0 to <+5", "+5 to >+10", ">+10")
legend("bottomright", leg.txt, horiz = F, pch="", cex=0.85, fill = colors, title = c(expression(paste(plain(Delta),"MDs/100k pop"))))








#### FIGURES -----


# APP FIG 1
library(metafor) 


# APP FIG 2
counties_data$specchange = counties_data$spec_2015-counties_data$spec_2005

par(font=1)
forest(c(mean(counties_data$specchange[counties_data$urb_2005<5],na.rm=T), 
         mean(counties_data$specchange[counties_data$urb_2005>=5],na.rm=T), 
         mean(counties_data$specchange[counties_data$pov_2005<16],na.rm=T),
         mean(counties_data$specchange[counties_data$pov_2005>=16],na.rm=T), 
         mean(counties_data$specchange[counties_data$blk_2005<9],na.rm=T),  
         mean(counties_data$specchange[counties_data$blk_2005>=9],na.rm=T), 
         mean(counties_data$specchange[counties_data$his_2005<8],na.rm=T), 
         mean(counties_data$specchange[counties_data$urb_2005>=8],na.rm=T)),
       ci.lb=c(quantile(counties_data$specchange[counties_data$urb_2005<5],c(.025),na.rm=T), 
               quantile(counties_data$specchange[counties_data$urb_2005>=5],c(.025),na.rm=T), 
               quantile(counties_data$specchange[counties_data$pov_2005<16],c(.025),na.rm=T),
               quantile(counties_data$specchange[counties_data$pov_2005>=16],c(.025),na.rm=T), 
               quantile(counties_data$specchange[counties_data$blk_2005<9],c(.025),na.rm=T),  
               quantile(counties_data$specchange[counties_data$blk_2005>=9],c(.025),na.rm=T), 
               quantile(counties_data$specchange[counties_data$his_2005<8],c(.025),na.rm=T), 
               quantile(counties_data$specchange[counties_data$urb_2005>=8],c(.025),na.rm=T)),
       ci.ub=c(quantile(counties_data$specchange[counties_data$urb_2005<5],c(.975),na.rm=T), 
               quantile(counties_data$specchange[counties_data$urb_2005>=5],c(.975),na.rm=T), 
               quantile(counties_data$specchange[counties_data$pov_2005<16],c(.975),na.rm=T),
               quantile(counties_data$specchange[counties_data$pov_2005>=16],c(.975),na.rm=T), 
               quantile(counties_data$specchange[counties_data$blk_2005<9],c(.975),na.rm=T),  
               quantile(counties_data$specchange[counties_data$blk_2005>=9],c(.975),na.rm=T), 
               quantile(counties_data$specchange[counties_data$his_2005<8],c(.975),na.rm=T), 
               quantile(counties_data$specchange[counties_data$urb_2005>=8],c(.975),na.rm=T)),
       xlim=c(-100,150),
       ylim=c(-1,17),
       rows=c(9,8,6,5,3,2,0,-1),
       slab=c(" Metro"," Non-metro/rural"," Low poverty"," High poverty"," Low Black %"," High Black %"," Low Hispanic %"," High Hispanic %"),
       xlab=c(expression(paste(plain(Delta),"MDs/100,000 pop"))),
       refline='NA')
par(font=2)
text(-100,c(10,7,4,1),pos=4,c("Urban/rural","Poverty","Black race","Hispanic ethnicity"))




# FIG 2
par(font=1)
forest(c(-7.9/(42.7-35.2), -3.0/(22.2-7.4), -14.1/(42.7-35.2), -26.3/(22.2-7.4), 0.3/(42.7-35.2), -2.7/(22.2-7.4), -1.9/(42.7-35.2), -1.3/(22.2-7.4), 1.1/(42.7-35.2), 5/(22.2-7.4)),
       ci.lb=c(-14.2/(42.7-35.2), -8.8/(22.2-7.4), -26.5/(42.7-35.2), -37.6/(22.2-7.4), -2.0/(42.7-35.2), -5.3/(22.2-7.4), -5.8/(42.7-35.2), -5.1/(22.2-7.4), -0.8/(42.7-35.2), 3.0/(22.2-7.4)),
       ci.ub=c(-1.6/(42.7-35.2), 2.7/(22.2-7.4), -1.7/(42.7-35.2), -15.1/(22.2-7.4), 2.7/(42.7-35.2), -0.1/(22.2-7.4), 1.9/(42.7-35.2), 2.4/(22.2-7.4), 3.1/(42.7-35.2), 6.9/(22.2-7.4)),
       xlim=c(-6,3),
       ylim=c(-1,20),
       rows=c(12,11,9,8,6,5,3,2,0,-1),
       slab=c(" Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"),
       xlab=c(expression(paste(plain(Delta),"Mortality per million"))),
       refline='NA')
par(font=2)
text(-6,c(13,10,7,4,1),pos=4,c("Cancer","Cardiovascular","Infectious","Respiratory","Substance/injury"))




# APP FIG 3

plot(counties_data$pcchange,counties_data$specchange)
cor(counties_data$pcchange,counties_data$specchange, use = "complete.obs", method = c("pearson"))










