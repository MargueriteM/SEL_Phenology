
# This code will process phenology data from SEL UTEP lab. 


# load libraries
# if you don't have a library can install manually or with eg: install.package("readxl")
library(tidyverse)###contains dyplr
library(readxl)
library(lubridate)
library(data.table)

setwd("~/Desktop/OneDrive - University of Texas at El Paso/SEL_Phenology")

prgl <- read_excel("PRGL_2010_20191220.xlsx")

#deleting #3s if 4s, if #3s and #4s have a 1, put DS03's value as zero into new column DS03_1
prgl$DS03_1<-ifelse(prgl$DS04==1, 0, prgl$DS03)

# change the column names for PRGL
setnames(prgl, old = c('DS01','DS02', 'DS03_1', 'DS04', 'DS05', 'DS06', 'DS07', 'DS08', 'DS09', 'DS10', 'DS11','DS12'), #keeps this order
         new = c('Breaking Leaf Bud','Young Unfolded Leaves', '>25% Full Leaf Size', '>= 75% Full Leaf Size', '>50% Leaves Fallen',
                 'All Leaves Fallen', 'Flower Buds', 'Open Flowers', 'Full Flowering', 'Fruits', 'Ripe Fruits','Dropped Fruits'))

# FLCE
# flce <- read_excel("FLCE_2010-20181102v2.xlsx")
# setnames(flce, old = c('DS01','DS02', 'DS03', 'DS04', 'DS05', 'DS06', 'DS07', 'DS08', 'DS09', 'DS10', 'DS11', 'DS12'),
 #        new = c('Breaking Leaf Bud','Young Unfolded Leaves', '>25% Full Leaf Size', '>= 75% full leaf size', '>50% leaves fallen',
 #                'All leaves fallen', 'flower buds', 'Open flowers', 'Full Flowering', 'Fruits', 'Ripe fruits', 'Recent Fruit drops' ))

# LATR
# latr <- read_excel("LATR_20100309-20191220.xlsx")
# latr$date_id <- as.Date(latr$date_id)
# setnames(latr, old = c('BE01','BE02', 'BE03', 'BE04', 'BE05', 'BE06', 'BE07', 'BE08'),
#         new = c('Breaking Leaf Bud','Young Unfolded Leaves', 'Flower buds', 
#                 'Open Flowers', 'Full flowering', 'Fruits', 'Ripe fruits','Fruits from past growing season'))

# MUPO
# mupo <- read_excel("MUPO_2010-20181102v2.xlsx")
# setnames(mupo, old = c('GR01','GR02', 'GR03', 'GR04', 'GR05', 'GR06', 'GR07'),
#         new = c('Initial growth','Leaves', 'All leaves withered', 
#                 'Flower heads', 'Open flowers', 'Grains', 'Ripe grains'))


# Prepare Prgl for graphing. 
# change data from wide to long format
prgl_pheno <- prgl %>%
  select(date_id, plant_id,`Flower Buds`,`Open Flowers`,`Full Flowering`,`Fruits`,
         `Ripe Fruits`,`Dropped Fruits`) %>%   #pick variables by their name
  # select(-date_string) %>% # drop specifc columns
  gather(phenophase, occurrence, -date_id, -plant_id)%>%
  mutate(year = year(date_id))

# remove duplicate rows
prgl_pheno <- unique(prgl_pheno)

look <- prgl_pheno %>% 
  filter(date_id==as.Date("2010-03-09") & phenophase=="Flower Buds")


# Calculate total number of phenophase occurences on each date
prgl_count_total <- prgl_pheno %>%
  group_by(date_id, year, phenophase) %>%  ### deleted plot column since the perenctage plot was over %100
  summarise(total_obs = length(occurrence))

# make graph to see how total count varies over time
ggplot(prgl_count_total, aes(date_id, total_obs, colour=phenophase))+
  geom_point()


# sum the total number of observations. If all are 0 pheno_obs  will = 0
# and if there are 1s, the 1s will get summed to give total number of individual in phenophase
prgl_count_obs <- prgl_pheno %>%
  group_by(date_id, year, phenophase) %>%
  summarise(pheno_obs = sum(occurrence))


# make graph of total obervations by phenophase
ggplot(prgl_count_obs, aes(date_id, pheno_obs, colour=phenophase))+
  geom_line()

# graph with separated phenophases and separted years
ggplot(prgl_count_obs, aes(yday(date_id), pheno_obs, colour=phenophase))+
  geom_line()+
  facet_grid(phenophase~year)

############################prgl_count_obs/prgl_count_total############################################

# combine the totals and the summed observations into one data frame 
prgl_count <- merge(prgl_count_total,prgl_count_obs,by=c("date_id","year","phenophase"),all.x=TRUE)

# calculate the percent of individuals in a phenophase on each date
# and get the day of year for each date
prgl_count <- prgl_count %>% 
  mutate(pheno_perc = (pheno_obs/total_obs)*100,
         doy = yday(date_id))

# organise levels of phenophase to graph in sequential order of when phenophases occur
prgl_count <- prgl_count %>% 
  mutate(phenophase = factor(phenophase, 
                             levels=c("Flower Buds","Open Flowers","Full Flowering",
                                      "Fruits","Ripe Fruits", "Dropped Fruits")))

# plot to test levels
ggplot(prgl_count, aes(doy, pheno_obs, colour=phenophase))+
  geom_line()+
  facet_grid(phenophase~year) # rows~columns


#plots primary phenophases as all years in X variable and % phenophase as Y variable
ggplot(prgl_count,aes(date_id,pheno_perc,colour=phenophase))+
  geom_line(size=1.2)+
  labs(title="Honey mesquite Flowering and Fruit", x="date", y="Percent occurrence") +
  facet_grid(phenophase~.) +
  theme_bw()


# graph with subset of years
# (plyr package)
prgl_count %>%
  filter(year == 2013) %>% # can also do things like year>2013 or year >= 2013
  ggplot(.,aes(date_id,pheno_perc,colour=phenophase))+
  geom_line(size=1.2)+
  labs(title="Honey mesquite Flowering and Fruit", x="date", y="Percent occurrence") +
  facet_grid(phenophase~.) +
  theme_bw()

prgl_count %>%
  filter(year == 2013 & month(date_id)==8) %>% # can also do things like year>2013 or year >= 2013
  ggplot(.,aes(date_id,pheno_perc,colour=phenophase))+
  geom_line(size=1.2)+
  labs(title="Honey mesquite Flowering and Fruit", x="date", y="Percent occurrence") +
  facet_grid(phenophase~.) +
  theme_bw()
