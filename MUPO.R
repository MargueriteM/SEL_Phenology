library(tidyverse)###contains dyplr
library(readxl)
library(lubridate)
library(data.table)

setwd("c:/UTEP/ThesisRelated/Data/Phenology/Data/MUPO")
getwd() 
mupo <- read_excel("MUPO_2010-20181102v2.xlsx")

setnames(mupo, old = c('GR01','GR02', 'GR03', 'GR04', 'GR05', 'GR06', 'GR07'),
         new = c('Initial growth','Leaves', 'All leaves withered', 
                 'Flower heads', 'Open flowers', 'Grains', 'Ripe grains'))

mupo_tidy <- mupo %>% 
  select(date_id, plant_id, `Initial growth`:`Ripe grains`) %>%   #pick variables by their name
  gather(phenophase, occurrence, -date_id, -plant_id)%>%
  mutate(year = year(date_id))



#separate plant ID into additional column (don't need to load any additional libraries)
mupo_tidy$transect <- sapply(strsplit(as.character(mupo_tidy$plant_id),"_"),"[",1)

mupo_tidy$plot <- paste(sapply(strsplit(as.character(mupo_tidy$plant_id),"_"),"[",1), 
                        sapply(strsplit(as.character(mupo_tidy$plant_id),"_"),"[",2), sep = "_")

mupo_tidy$species <- sapply(strsplit(as.character(mupo_tidy$plant_id),"_"),"[",3)

####################CALCULATE % Phenphase for the whole site ###########################################################################
# calculate total number of individuals observed per year and transect
mupo_observations <- mupo_tidy %>%
  distinct(year,phenophase,plant_id) %>%
  summarise(total_obs = length(plant_id))

# remove duplicates from data
mupo_tidy_2 <- unique(mupo_tidy)

# calculate number of individuals observed for each date and phenophase
mupo_count_total <- mupo_tidy_2 %>%
  group_by(date_id, year, phenophase, species) %>%  ### deleted plot column since the perenctage plot was over %100
  summarise(total_obs = length(occurrence))

# find dates when there are more observations than individuals
View(subset(mupo_count_total,total_obs>46))

#########TO SEARCH THE DATA#################################################################################3

View(subset(mupo_tidy_2, year=="2016"))

write.csv(temp2016, file = 'C:/UTEP/ThesisRelated/Data/Phenology/Data/LATR/2016.csv')
View(subset(mupo_tidy_2, date_id==as.Date("2016-10-13") & phenophase == "Flower heads"))
############################################################################################################


# calculate the sum for each date and phenophase, the sum will show the number of 1s counted 
#for that day for that phenophase
mupo_count_one <- mupo_tidy_2 %>%
  group_by(date_id, year, phenophase,species) %>%
  summarise(pheno_obs = sum(occurrence))

# combine the total counts and the 1 counts and keep all the rows that exist in total count data frame
mupo_count <- merge(mupo_count_total,mupo_count_one,by=c("date_id","year","phenophase","species"),all.x=TRUE)

# calculate the percent of individuals in a phenophase on each date
mupo_count <- mutate(mupo_count,pheno_perc = (pheno_obs/total_obs)*100) 


# give labels according to the phenophase type
mupo_count <- mupo_count %>%
  # divide phenophase into life strategy
  mutate(pheno_type = ifelse(phenophase %in% c("Initial growth","Leaves", "All leaves withered", "Flower heads", 
                                               "Open flowers", "Grains", "Ripe grains"), "primary production", "reproduction")) %>%
  # group into life stages
  #mutate(pheno_detail = factor(ifelse(phenophase %in% c("Breaking Leaf Bud","Young Unfolded Leaves"),"leaf out",
  #ifelse(phenophase %in% c(">25% Full Leaf Size", ">= 75% full leaf size"), "greening", "senescing")),
  #levels=c("leaf out","greening","senescing"))) %>%
  # create day of year column
  mutate(doy = yday(date_id))



ggplot(subset(mupo_count,pheno_type=="primary production"),
       aes(date_id,pheno_perc,colour=phenophase))+geom_line(size=1)+ggtitle("bush muhly(MUPO)")+facet_grid(phenophase~.)


ggplot(subset(prgl_count,pheno_type=="primary production"),
       aes(date_id,pheno_perc,colour=phenophase))+geom_line(size=0.8)+facet_grid(pheno_detail~.)


ggplot(subset(prgl_count,pheno_type=="primary production"),
       aes(doy,pheno_perc,colour=factor(year)))+geom_line()+facet_grid(phenophase~.)


