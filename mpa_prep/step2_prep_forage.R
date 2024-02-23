#Joshua G. Smith
#jossmith@mbayaq.org

#this script combines, cleans, and formats the data for SOFA. The output
#from this script is a data table that is ready for SOFA and can be filtered
#based on the focal spatial and temporal extent.  

rm(list=ls())

#load packages
librarian::shelf(tidyverse, readxl, here, DataExplorer)

#set directories 
datin <- "/Volumes/seaotterdb$/kelp_recovery/data/foraging_data/raw"
datout <- "/Volumes/seaotterdb$/kelp_recovery/data/foraging_data/processed"
figdir <- here::here("analyses","figures")

#get foraging data
for_dat_c <- read_csv(file.path(datin, "Forage_data_2016tocurrent.csv"))
for_dat_o <- read_csv(file.path(datin, "Forage_data_97to16.csv"))

dives_c <- read_csv(file.path(datin, "Forage_dives_2016tocurrent.csv"))
dives_o <- read_csv(file.path(datin, "Forage_dives_97to16.csv"))

index_c <- read_csv(file.path(datin, "Forage_index_2016tocurrent.csv"))
index_o <- read_csv(file.path(datin, "Forage_index_97to16.csv"))

################################################################################
#explore data

#2016 to current
str(for_dat_c)
str(dives_c)
str(index_c)

plot_intro(for_dat_c)
plot_intro(dives_c)
plot_intro(index_c)

#97 to 2016
str(for_dat_o)
str(dives_o)
str(index_o)

plot_intro(for_dat_o)
plot_intro(dives_o)
plot_intro(index_o)

################################################################################
#process foraging data

glimpse(for_dat_c)
glimpse(for_dat_o)

#step 1 ------------- merge old and current ------------------------------------

for_dat_m <- rbind(for_dat_o, for_dat_c)

#check rows
nrow(for_dat_o) + nrow(for_dat_c)
nrow(for_dat_m)

#step 2 ------------- clean ----------------------------------------------------

for_dat_build1 <- for_dat_m %>% janitor::clean_names() %>%
  #correct inconsistencies
  mutate(qualifier = factor(tolower(qualifier)),
         pup_behav = factor(tolower(pup_behav)),
         mom_resp = factor(tolower(mom_resp)),
         outcome = factor(tolower(outcome)),
         prey = factor(prey),
         tooltype = factor(tooltype),
         pupsh = factor(pupsh),
         pup_behav = factor(pup_behav),
         stolenfr = factor(stolenfr),
         steal = factor(steal),
         sizecm = factor(sizecm)
  )

str(for_dat_build1)
colnames(for_dat_build1)

################################################################################
#process dives

#step 1 ------------- merge old and current ------------------------------------

colnames(dives_o)
colnames(dives_c)

dives_m <- bind_rows(dives_o, dives_c)

#check rows
nrow(dives_o) + nrow(dives_c)
nrow(dives_m)

#step 2 ------------- clean ----------------------------------------------------

dive_build1 <- dives_m %>% janitor::clean_names() %>%
  #correct inconsistencies
  mutate(where = tolower(where),
         #fix incorrect sign
         long_obs_deg = ifelse(long_obs_deg == 121,-121,long_obs_deg),
         #fix kelp type
         kelptype = ifelse(kelptype == "xx","x",kelptype),
         kelptype = factor(tolower(kelptype)),
         success = factor(tolower(success)),
         sb_area = factor(toupper(sb_area))
         
  ) 

colnames(dive_build1)
str(dive_build1)


################################################################################
#process index

#step 1 ------------- merge old and current ------------------------------------

colnames(index_o)
colnames(index_c)

index_m <- bind_rows(index_o, index_c)


#check rows
nrow(index_o) + nrow(index_c)
nrow(index_m)

index_build1 <- index_m %>% janitor::clean_names() %>%
  mutate(visib = factor(tolower(visib)),
         sex = factor(sex),
         ageclass = factor(ageclass),
         status = factor(status),
         pupsz = factor(pupsz),
         consort = factor(consort),
         obsbeg = factor(obsbeg),
         obsend = factor(obsend),
         daynight = factor(daynight),
         area = factor(area),
         sky = factor(sky),
         winddir = factor(winddir),
         seaopen = factor(seaopen),
         seafix = factor(seafix),
         swell = factor(swell)
  )

str(index_build1)
colnames(index_build1)



################################################################################
#merge data

forage_join <- for_dat_build1 %>%
  #filter(steal == "No")%>% 
 #select variables of interest
  dplyr::select(foragdiv_id, foragdata_id, preynum, prey, number,
                size, qualifier, ht, stolenfr_number, pupshare_number)%>%
  mutate_if(is.character, str_trim)

dive_join <- dive_build1 %>%
  dplyr::select(foragdiv_id, bout, subbout, lat, long, 
                utm_ew, utm_ns, utm_zone, utm_datum,
                canopy,
                st,where,
                kelptype, divenum, dt, success) %>%
  mutate_if(is.character, str_trim) 

index_join <- index_build1 %>%
  dplyr::select(bout, date, otterno, sex, ageclass, status, area,
                timestart, timeend)%>%
  mutate_if(is.character, str_trim)


#merge forage data and bout info
data_build1 <- left_join(dive_join, forage_join, by="foragdiv_id")
anti_build <- anti_join(dive_join, forage_join, by="foragdiv_id")


#merge with index
data_build2 <- left_join(data_build1, index_join, by="bout")
anti_build2 <- anti_join(data_build1, index_join, by="bout")


################################################################################
#process data build

data_build3 <- data_build2 %>%
  mutate(date = dmy(date), 
         year = year(date),  
         month = month(date),  
         day = day(date))%>%
  dplyr::select(year, month, day, date, foragdiv_id, foragdata_id, bout, subbout,
                lat, long, otterno, everything()
  ) 

#check fields
str(data_build3)
colnames(data_build3)

unique(data_build3$year)
unique(data_build3$month)
unique(data_build3$day)
unique(data_build3$foragdiv_id)
unique(data_build3$foragdata_id)
unique(data_build3$bout)
unique(data_build3$subbout)
unique(data_build3$lat) # '1' 
unique(data_build3$long) # '1'
unique(data_build3$otterno)
unique(data_build3$canopy)
unique(data_build3$kelptype) # includes 0, 1, m00
unique(data_build3$divenum)
unique(data_build3$dt)
unique(data_build3$success)
unique(data_build3$preynum)
unique(data_build3$prey)
unique(data_build3$number)
unique(data_build3$size)
unique(data_build3$qualifier)
unique(data_build3$date)
unique(data_build3$sex)
unique(data_build3$ageclass)
unique(data_build3$status)

#fix incnsistencies
data_build4 <- data_build3 %>%
  mutate(kelptype = as.character(kelptype), # Convert to character first
         kelptype = case_when(
           kelptype %in% c("0", "1", "m00") ~ NA_character_, # Replace with NA
           TRUE ~ kelptype # Keep as is for other cases
         ),
         lat = ifelse(lat <30 , NA, lat),
         long = ifelse(long ==1, NA, long)
         ) %>%
  #convert back to factor
  mutate(kelptype = factor(kelptype))

unique(data_build4$kelptype)
unique(data_build4$lat)
unique(data_build4$long)


################################################################################
#export

write.csv(data_build4, file.path(datout, "foraging_data_1997_2023.csv"), row.names=FALSE)




