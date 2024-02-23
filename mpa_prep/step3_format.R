#Joshua G. Smith
#jossmith@mbayaq.org

#this script formats the cleaned foraging data for SOFA.
#user-defined filtering should be applied at this stage

rm(list=ls())

#load packages
librarian::shelf(tidyverse, readxl, here, DataExplorer)

#set directories 
datin <- "/Volumes/seaotterdb$/kelp_recovery/data/foraging_data/processed"
datout <- "/Volumes/seaotterdb$/kelp_recovery/data/sofa_data/intermediate"

#get foraging data
for_dat <- read_csv(file.path(datin, "foraging_data_1997_2023.csv"))

################################################################################
#prep standard fields and data types ***DO NOT EDIT THIS CHUNK***

sofa_build1 <- for_dat %>%
                #fill NAs
                mutate(stolenfr_number = ifelse(is.na(stolenfr_number),0,stolenfr_number),
                       pupshare_number = ifelse(is.na(pupshare_number),0,pupshare_number))%>%
                #add dummy fields to QAQC data
                mutate(region = NA,
                       period = NA,
                       season = ifelse(month %in% c(6,7,8,9,10),"summer","winter"),
                       prop_lost = (stolenfr_number + pupshare_number)/number,
                       #missing
                       how_lost = NA,
                       est_kg = NA,
                       est_cm = NA,
                       #fix pup
                       pup = case_when(
                         status %in% c('f', 'm', 't') ~ 'n',
                         is.na(status) ~ 'u',
                         status == 's' ~ 's',
                         status == 'l' ~ 'l',
                         status %in% c('p','r') ~ 'y',
                         TRUE ~ status
                       )
                       )%>%
                #pop_lost !> 1, so drop
                filter(prop_lost <= 1)%>%
                #recode ageclass
                mutate(ageclass = case_when(
                  ageclass == 's' ~ 'sa',
                  ageclass == 'o' ~ 'aa',
                  TRUE ~ ageclass
                ))%>%
                dplyr::select(region, area, site = where, period,
                  date, season, sex, ageclass, pup, ottername= otterno, bout, subbout,
                  timestart, timeend, divenum, dt, st, success, prey, n_items= number,
                  size, qualifier, ht, prop_lost, how_lost, est_kg, est_cm, lat, long) %>%
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
unique(sofa_build1$pup)


################################################################################
#filter data for SOFA ***EDIT THIS CHUNK***

sofa_build2 <- sofa_build1 %>%
                #filter focal spatial extent
                filter(lat > 36.488933 & lat < 36.665291) %>%
                #drop lat long
                select(-lat, -long)%>%
                #add SOFA run-specific fields
                mutate(region = "MPEN",
                       period = "2016-2023")
                

write.xlsx(sofa_build2, file.path(datout, "sofa_test_run.xlsx"),rowNames=FALSE)













