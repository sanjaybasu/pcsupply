
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

counties_data$colorBuckets <- as.numeric(cut(counties_data$pcchange, c(-100,-1,-.5,0,.5,1,100)))

colorsmatched <- counties_data$colorBuckets[match(county.fips$fips, counties_data$fipscode)]


#### FIGURE 1 -----


map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")


# Add border around each State
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("Change in primary care physician density, 2005-2015")

leg.txt <- c("<-1", "-1 to <-0.5", "-0.5 to <0", "0 to <+0.5", "+0.5-<+1", ">+1")
legend("bottomright", leg.txt, horiz = F, fill = colors, title = c(expression(paste(plain(Delta),"MDs/10,000 pop"))))








#### FIGURES -----


# APP FIG 1
library(metafor) 
par(font=1)
forest(c(-0.2628, -0.697, -0.498, -0.629, -0.578,  -0.4700, -0.584, -0.4194),
       ci.lb=c(-2.629598, -5.092626, -4.684263, -4.358027, -4.786918, -3.217592, -4.575187, -3.974316),
       ci.ub=c(2.048900, 3.433928, 3.169193, 2.336349, 3.388390, 2.179708, 2.860016, 2.521582),
       xlim=c(-10,14),
       ylim=c(-1,17),
       rows=c(9,8,6,5,3,2,0,-1),
       slab=c(" Metro"," Non-metro/rural"," Low poverty"," High poverty"," Low Black %"," High Black %"," Low Hispanic %"," High Hispanic %"),
       xlab=c(expression(paste(plain(Delta),"MDs/10,000 pop"))),
       refline='NA')
par(font=2)
text(-10,c(10,7,4,1),pos=4,c("Urban/rural","Poverty","Black race","Hispanic ethnicity"))



# APP FIG 2
library(metafor) 
par(font=1)
forest(c(0.9126, -0.012, 0.546, -0.064, 0.226, 0.5306, 0.327, 0.2443),
       ci.lb=c(-2.812889, -4.540311, -3.974788, -4.012842, -4.276700, -3.573294, -4.268103, -3.340282),
       ci.ub=c(9.129309, 4.070574, 6.149926, 4.473024, 5.007611, 7.657232, 5.669563, 5.463903),
       xlim=c(-10,14),
       ylim=c(-1,17),
       rows=c(9,8,6,5,3,2,0,-1),
       slab=c(" Metro"," Non-metro/rural"," Low poverty"," High poverty"," Low Black %"," High Black %"," Low Hispanic %"," High Hispanic %"),
       xlab=c(expression(paste(plain(Delta),"MDs/10,000 pop"))),
       refline='NA')
par(font=2)
text(-10,c(10,7,4,1),pos=4,c("Urban/rural","Poverty","Black race","Hispanic ethnicity"))



# FIG 2
par(font=1)
forest(c(-12.4/3.2, -19.0/11.2,-18.8/3.2,-43/11.2, -0.7/3.2,-3.8/11.2, -4.8/3.2,-13.2/11.2, -0.3/3.2,0.3/11.2),
       ci.lb=c(-16.3/3.2, -24.5/11.2, -25.6/3.2, -54.1/11.2, -2.0/3.2, -6.2/11.2, -6.2/3.2, -16.6/11.2, -0.4/3.2, 0.2/11.2),
       ci.ub=c(-8.5/3.2, -13.6/11.2, -12.1/3.2, -31.9/11.2, 0.6/3.2, -1.4/11.2, -3.3/3.2, -9.8/11.2, -0.2/3.2, 0.4/11.2),
       xlim=c(-12,5),
       ylim=c(-1,20),
       rows=c(12,11,9,8,6,5,3,2,0,-1),
       slab=c(" Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"),
       xlab=c(expression(paste(plain(Delta),"Mortality per million"))),
       refline='NA')
par(font=2)
text(-12,c(13,10,7,4,1),pos=4,c("Cancer","Cardiovascular","Infectious","Respiratory","Substance/injury"))


# APP FIG 4
par(font=1)
forest(c(-3.0/3.2, -20.1/11.2,1.5/3.2,-47.8/11.2, 0.1/3.2,-4.1/11.2, -0.9/3.2,-14.5/11.2, -0.1/3.2,0.4/11.2),
       ci.lb=c(-5/3.2, -25.7/11.2, -3.3/3.2, -59.5/11.2, -0.8/3.2, -6.5/11.2, -2.1/3.2, -18/11.2, -.2/3.2, .3/11.2),
       ci.ub=c(-1.1/3.2, -14.6/11.2, 6.3/3.2, -36.2/11.2, 1/3.2, -1.7/11.2, 0.4/3.2, -10.9/11.2, -.01/3.2, .5/11.2),
       xlim=c(-8,5),
       ylim=c(-1,20),
       rows=c(12,11,9,8,6,5,3,2,0,-1),
       slab=c(" Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"," Primary care"," Non-primary care"),
       xlab=c(expression(paste(plain(Delta),"Mortality per million"))),
       refline='NA')
par(font=2)
text(-8,c(13,10,7,4,1),pos=4,c("Cancer","Cardiovascular","Infectious","Respiratory","Substance/injury"))
