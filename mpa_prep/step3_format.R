#Joshua G. Smith
#jossmith@mbayaq.org

#this script formats the cleaned foraging data for SOFA.
#user-defined filtering should be applied at this stage

rm(list=ls())

#load packages
librarian::shelf(tidyverse, readxl, here, DataExplorer)

#set directories 
datin <- "/Volumes/seaotterdb$/kelp_recovery/data/foraging_data/processed"
#datout <- "/Volumes/seaotterdb$/kelp_recovery/data/foraging_data/processed"

#get foraging data
for_dat <- read_csv(file.path(datin, "foraging_data_1997_2023.csv"))

################################################################################
#prep standard fields and data types ***DO NOT EDIT THIS CHUNK***

sofa_build1 <- for_dat %>%
                #add dummy fields to QAQC data
                mutate(region = NA,
                       period = NA,
                       season = ifelse(month %in% c(6,7,8,9,10),"summer","winter"),
                       #missing
                       prop_lost = NA,
                       how_lost = NA,
                       est_kg = NA,
                       est_cm = NA,
                       #fix pup
                       pup = case_when(
                         status %in% c('f', 'm', 't') ~ 'n',
                         status == 'NA' ~ 'u',
                         status == 's' ~ 's',
                         status == 'l' ~ 'l',
                         TRUE ~ NA_character_ # default case if none of the above conditions are met
                       )
                       )%>%
                dplyr::select(region, area, site = where, period,
                  date, season, sex, ageclass, pup, ottername= otterno, bout, subbout,
                  timestart, timeend, divenum, dt, st, success, prey, n_items= preynum,
                  size, qualifier, ht, prop_lost, how_lost, est_kg, est_cm) %>%
                #format data
                mutate(
                  #format date
                  date = format(as.Date(date), "%m-%d-%Y"),
                  #format time
                  timestart = format(strptime(timestart, format = "%H:%M:%S"), "%H:%M"),
                  timeend = format(strptime(timeend, format = "%H:%M:%S"), "%H:%M")
                       )

#check levels and format
str(sofa_build1)
unique(sofa_build1$success) #should be y, n, and c, but there are multiple
unique(sofa_build1$prey)
unique(sofa_build1$ageclass) #what is "o"?
unique(sofa_build1$sex)




