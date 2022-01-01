# *** NOTE: CLEANING is everything before EDA Users ***

setwd("C:/Users/lujas/Desktop/MSSP Noise/")

# need to globalize this reading
noiseUsers <- read.csv("C:/Users/lujas/Desktop/MSSP Noise/Users.csv")
noise_meas <- read.csv("C:/Users/lujas/Desktop/MSSP Noise/Measurements.csv")
# Jenna install

# need to make sure these are installed
library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(tmap)
library(magrittr)
library(sf)

# Users: cleaning ####
# selecting useful variables 
noiseUsers <- subset(noiseUsers, select = c(UserID, Year.Born, Ethnicity, Sensitivity, Home.Noise, 
                                            Community.Noise, Work.Noise, Health.Status))
noiseUsers$Year.Born <- sub("-1", NA, noiseUsers$Year.Born)

# Measurements: cleaning ####
unique_info <- noise_meas %>% summarise_all(n_distinct)
remove_col <-  colnames(noise_meas)[which(unique_info[1,]==1)] 
## remove the 1-unique columns from the dataset
noise_meas %<>% select(-all_of(remove_col))
unique_info %<>% select(-all_of(remove_col)) 
noise_meas %<>% 
  separate(RawData, c("Min","Max","Avg","Median"), sep = ' ') %>%
  separate(Min, c("d_1","Min"), sep = ':') %>%
  separate(Max, c("d_2","Max"), sep = ':') %>%
  separate(Avg, c("d_3","Avg"), sep = ':') %>%
  separate(Median,c("d_4","Median"), sep = 6) %>%
  select(-d_1, -d_2, -d_3, -d_4)
noise_meas %<>% 
  separate(Location,c("Longitude","Latitude"), sep = ' Latitude') %<>%
  separate(Longitude,c("drop",'Longitude'),sep = ':') %<>%
  select(-drop)
# noise_meas %>% summarize_all(n_distinct)
# Feeling 
Negative <- noise_meas %>% filter(Feel %in% c("Irritated","Anxious","Frustrated","Angry"))
Neutral <- noise_meas %>% filter(Feel == 'Neutral')
Postive <- noise_meas %>% filter(Feel %in% c("Tranquil","Relaxed"))
Negative$s_Feel <- 'NEGATIVE'
Neutral$s_Feel <- 'Normal'
Postive$s_Feel <- 'POSTIVE'
noise_meas <- do.call("rbind", list(Postive,Neutral,Negative))

# Merging Users and Measurements #####
colnames(noise_meas)[1] <- 'UserID'
noise <- merge(noiseUsers,noise_meas,by='UserID',all=T)
# cleaning
noise_meas %>% summarize_all(n_distinct) -> aa ## look at number of unique values in each column
bb <- which(aa[1,]==1) ## make a list of the columns with only one unique value
cn <- colnames(noise_meas)[bb] ## list the 1-unique value column names
noise %<>% select(-all_of(bb)) ## remove the 1-unique columns from the dataset
aa %<>% select(-all_of(bb)) 

# Ordering variables, fixing categories (Health.Status, Year.Born, Ethnicity.grouped) #####

# Delete first few (blank) rows, Health.Status var
noise <- tail(noise,-18)
noise$Health.Status <- sub("Loud", NA, noise$Health.Status)
noise$Health.Status <- sub("Neutral", NA, noise$Health.Status)
noise$Health.Status <- sub("Quiet", NA, noise$Health.Status)
noise$Health.Status <- sub("Very quiet", NA, noise$Health.Status)

# Ordered factor for Vars
noise$Sensitivity <- factor(noise$Sensitivity, ordered = TRUE, levels = c("Not at all", "Very Little", "A Little","Moderately","Severely"))
noise$Home.Noise <- factor(noise$Home.Noise, ordered = TRUE, levels = c("Very Quiet", "Quiet", "Neutral","Loud","Very Loud"))
noise$Community.Noise <- factor(noise$Community.Noise, ordered = TRUE, levels = c("Very Quiet", "Quiet", "Neutral","Loud","Very Loud"))
noise$Work.Noise <- factor(noise$Work.Noise, ordered = TRUE, levels = c("Very Quiet", "Quiet", "Neutral","Loud","Very Loud"))
noise$Health.Status <- factor(noise$Health.Status, ordered = TRUE, levels = c("Very poor", "Poor", "Fair","Good","Excellent"))
noise$Sound <- factor(noise$Sound, ordered = TRUE, levels = c("Very quiet", "Quiet", "Moderately Loud","Loud","Very Loud"))
noise$Describe <- factor(noise$Describe, ordered = TRUE, levels = c("Unbearable", "Noisy", "Neutral","Pleasant","Very pleasant"))
noise$Feel <- factor(noise$Feel, ordered = TRUE, levels = c("Frustrated", "Angry", "Irritated","Anxious","Neutral","Relaxed","Tranquil"))
noise$s_Feel <- factor(noise$s_Feel, ordered = TRUE, levels = c("NEGATIVE","Normal","POSTIVE"))

# Ethnicity variable take first one only
## new variable "group_ethnicity"
for (i in 1:length(noise$Ethnicity)) {
  group_ethnicity <- unlist(strsplit(noise$Ethnicity[i],",")) # separates by the comma
  noise$Ethnicity.grouped[i] <- group_ethnicity[1]            # takes in the first ethnicity
}

# Year.Born into categories
## not optimized in all cases, strictly the number they put down assuming the year is 2020
## 0-12, 13-17,18-24,25-34,35-44,45-54,55-64,65+
## 2020-2008, 2007-2003, 2002-1996, 1995-1986, 1985-1976, 1975-1966, 1965-1956, 1955 and below
noise$Year.Born <- as.numeric(noise$Year.Born)
noise$Year.Born.category <- NA
for (i in 1:length(noise$Year.Born)) {
  if (noise$Year.Born[i] > 2007 & noise$Year.Born[i] < 2021 & (!is.na(noise$Year.Born[i])) ) {
    noise$Year.Born.category[i] <- '2008-2020'
  } else if(noise$Year.Born[i] > 2002 & noise$Year.Born[i] < 2008 & (!is.na(noise$Year.Born[i]))) {
    noise$Year.Born.category[i] <- '2003-2007'
  } else if(noise$Year.Born[i] > 1995 & noise$Year.Born[i] < 2003 & (!is.na(noise$Year.Born[i]))) {
    noise$Year.Born.category[i] <- '1996-2002'
  } else if(noise$Year.Born[i] > 1985 & noise$Year.Born[i] < 1996 & (!is.na(noise$Year.Born[i]))) {
    noise$Year.Born.category[i] <- '1986-1995'
  } else if(noise$Year.Born[i] > 1975 & noise$Year.Born[i] < 1986 & (!is.na(noise$Year.Born[i]))) {
    noise$Year.Born.category[i] <- '1976-1985'
  } else if(noise$Year.Born[i] > 1965 & noise$Year.Born[i] < 1976 & (!is.na(noise$Year.Born[i]))) {
    noise$Year.Born.category[i] <- '1966-1975'
  } else if(noise$Year.Born[i] > 1955 & noise$Year.Born[i] < 1966 & (!is.na(noise$Year.Born[i]))) {
    noise$Year.Born.category[i] <- '1956-1965'
  } else if(noise$Year.Born[i] < 1956 & (!is.na(noise$Year.Born[i]))) {
    noise$Year.Born.category[i] <- '1955 and below'
  } else {
    noise$Year.Born.category[i] <- NA
  }
}
noise$Year.Born.category <- factor(noise$Year.Born.category, ordered = TRUE, levels=c("1955 and below","1956-1965","1966-1975","1976-1985","1986-1995",
                                                                                      "1996-2002","2003-2007","2008-2020"))


# Vulnerability: cleaning Jenna
##################
### EDA Users ########
count_graphics <- function(column) {
  counts <- table(column)
  frame <- data.frame(counts)
  frame %>% 
    arrange(desc(Freq))
}
View(count_graphics(noise$Year.Born.category)) #plug in any column, doesn't track NA counts

#single variable
ggplot(noise, aes(x=Sensitivity)) +
  geom_bar()
ggplot(noise, aes(x=s_Feel)) +
  geom_bar()
lnames <- c("65+","55-64","45-54","35-44","25-34","18-24","U-12","N/A")
ggplot(noise, aes(x=Year.Born.category)) + 
  geom_bar() + scale_x_discrete(labels=lnames)

# mosaic, need to work on updating x-axis
ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Place), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')

ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Home.Noise), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')
# Less sensitive for louder noises

ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Work.Noise), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')

ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Community.Noise), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')
# higher sensitivity for louder noise? this one looks like it has something but maybe not?

ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Health.Status), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')
# Better health status leads to more moderate/lower levels of sensitivity

ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Sound), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')
ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Describe), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')

ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Feel), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')
# MAYBE more extreme feelings are less sensitive than neutral feelings

# AA, A, Black, Caucasian, Hisp, Other, PI, undefined is the order
ggplot(data = noise) +
  geom_mosaic(aes(x = product(Sensitivity, Ethnicity.grouped), fill=Sensitivity), na.rm=TRUE) + 
  labs(x = "test", title='x')

# goes from negative to normal to positive
ggplot(data = noise) +
  geom_mosaic(aes(x = product(Year.Born.category, s_Feel), fill=Year.Born.category), na.rm=TRUE) + 
  labs(x = "test", title='x')
# neutral and positive feelings more prevalent in younger people, more negative feelings in 1966-1975 range (45-54)


# VCD Package ####
install.packages("vcd")
library(vcd)

# Blue means more than predicted, red means less than predicted
#use gp=shading_Friendly for different shading
noise.table <- with(noise, table(Sensitivity, s_Feel))
mosaic(noise.table, shade=TRUE, legend=TRUE, main="Sensitivity vs. Feeling")

noise.table <- with(noise, table(Year.Born.category, s_Feel)) # maybe remove post-2000 year
vnames <- list(set_varnames=c(s_Feel="Feeling", Year.Born.category="Age"))
lnames <- list(Year.Born.category = c("65+","55-64","45-54","35-44","25-34","18-24","l","l"),
               s_Feel = c("Negative","Normal","Positive"))
mosaic(noise.table, shade=TRUE, legend=TRUE, labeling_args=vnames, set_labels=lnames, main="Age vs Feeling")

noise.table <- with(noise, table(Year.Born.category, Sensitivity)) # maybe remove post-2000 year
vnames <- list(set_varnames=c(Sensitivity="Sensitivity", Year.Born.category="Age"))
lnames <- list(Year.Born.category = c("65+","55-64","45-54","35-44","25-34","18-24","l","l"),
               Sensitivity = c("Not at all","Very Little","A Little","Moderately","Severely"))
mosaic(noise.table, shade=TRUE, legend=TRUE, labeling_args=vnames, set_labels=lnames, main="Sensitivity vs Age")

# AA, A, Black, Caucasian, Hisp, Other, PI, undefined is the order
noise.table <- with(noise, table(Sensitivity, Ethnicity.grouped))
lnames <- list(Ethnicity.grouped = c("AA","Afr.","Black","Caucasian","Hisp.","Other","PI","UnID"),
               Sensitivity = c("Not at all","Very Little","A Little","Moderately","Severely"))
mosaic(noise.table, shade=TRUE, legend=TRUE, set_labels=lnames, main="Sensitivity vs Ethnicity")

View(count_graphics(noise$Year.Born.category)) #plug in any column, doesn't track NA counts

# Chi square, H0 means no relationship, HA means there is dependence
## everything is super significant off everything b/c large n
chisq <- chisq.test(table(noise$Sensitivity,noise$Health.Status))
chisq

##################
### EDA Measurements Fan ########
tmap_mode('view')
epsg_wgs84 <- 4326 # GPS CRS (WGS 84)
noise %<>% 
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs(epsg_wgs84)
noise_plot<- noise_meas %>% select(userID, Avg, Sound, geometry)

raw_map <-
  tm_shape(noise) +
  tm_dots(col = 'red', size = .01, alpha=.5) +
  tm_layout(main.title='Noise Distribution',
            main.title.position="center",
            frame = FALSE)
raw_map

Sensitivity_map <-
  tm_shape(noise) +
  tm_dots('Sensitivity', size = .02, alpha=.5) +
  tm_layout(main.title='Sensitivity',
            main.title.position="center",
            frame = FALSE)
Sensitivity_map

Sound_map <-
  tm_shape(noise) +
  tm_dots(col = 'Sound', size = .01, alpha=.5,title='Sound Level') 
#+tm_facets(by = "Place", nrow = 1, free.coords = FALSE)
Sound_map

Describe_map <-
  tm_shape(noise) +
  tm_dots('Describe', size = .01, alpha=.5) +
  tm_layout(main.title='Noise Distribution',
            main.title.position="center",
            frame = FALSE)
Describe_map

Feeling_map <-
  tm_shape(noise) +
  tm_dots('Feel', size = .02, alpha=.5) +
  tm_layout(main.title='Noise Distribution',
            main.title.position="center",
            frame = FALSE)
Feeling_map

Indoors_map <- noise %>% filter( Place == 'Indoors')
Outdoors_map <- noise %>% filter( Place == 'Outdoors')
Atwork_map <- noise %>% filter( Place == 'At work')

tm_shape(Indoors_map) +
  tm_dots('Sound', size = .01, alpha=.5) +
  tm_layout(main.title='Noise Distribution',
            main.title.position="center",
            frame = FALSE)

tm_shape(Outdoors_map) +
  tm_dots('Sound', size = .01, alpha=.5) +
  tm_layout(main.title='Noise Distribution',
            main.title.position="center",
            frame = FALSE)

tm_shape(Atwork_map) +
  tm_dots('Sound', size = .01, alpha=.5) +
  tm_layout(main.title='Noise Distribution',
            main.title.position="center",
            frame = FALSE)



##################
### EDA Vulnerability Jenna
##################
# Extra: Users: Separating Weekday/Weekend Percentages ######


## functions to extract 
get_nums <- function(row_data_str) {                   #takes in percentages cell and returns just the numbers, used for get_matrix_of_nums
  list_of_pairs <- unlist(strsplit(row_data_str, " ")) #separates the space
  list_of_nums <- c()
  for (i in 1:length(list_of_pairs)) {
    list_of_nums[i] <- unlist(strsplit(toString(list_of_pairs[i]), split=":"))[2] #separates the semicolon and takes 2nd piece for each input
  }
  list_of_nums
}
# example: get_nums("a:10 b:20 c:20")

get_matrix_of_nums <- function(vec_of_row_data) {
  vec_of_nums <- list()
  for (i in 1:length(vec_of_row_data)) {
    vec_of_nums[[i]] <- get_nums(vec_of_row_data[i])
  }
  vec_of_nums
}

# aa <- get_matrix_of_nums(head(noiseUsers$Weekday.Percentages))
# aa[[5]][1] # calls the 5th observation's first cell (the Commuting #)

get_percentages_column <- function(vec_of_nums, placement) {    #maybe can merge with prev. function
  column <- c()
  for (i in 1:length(vec_of_nums)) {
    column[i] <- vec_of_nums[[i]][placement]
  }
  column
}
## applying to dataset 
# using functions to separate numbers
numbers_weekday <- get_nums(noiseUsers$Weekday.Percentages)
numbers_weekend <- get_nums(noiseUsers$Weekend.Percentages)

numbers_weekday_matrix <- get_matrix_of_nums(noiseUsers$Weekday.Percentages)
numbers_weekend_matrix <- get_matrix_of_nums(noiseUsers$Weekend.Percentages)

# creating columns
Commuting_weekday <- get_percentages_column(numbers_weekday_matrix,1)
Commuting_weekend <- get_percentages_column(numbers_weekend_matrix,1)
SchoolWork_weekday <- get_percentages_column(numbers_weekday_matrix,2)
SchoolWork_weekend <- get_percentages_column(numbers_weekend_matrix,2)
Home_weekday <- get_percentages_column(numbers_weekday_matrix,3)
Home_weekend <- get_percentages_column(numbers_weekend_matrix,3)
Sleeping_weekday <- get_percentages_column(numbers_weekday_matrix,4)
Sleeping_weekend <- get_percentages_column(numbers_weekend_matrix,4)
Physical_weekday <- get_percentages_column(numbers_weekday_matrix,5)
Physical_weekend <- get_percentages_column(numbers_weekend_matrix,5)
Errands_weekday <- get_percentages_column(numbers_weekday_matrix,6)
Errands_weekend <- get_percentages_column(numbers_weekend_matrix,6)
# format: "Commuting:10 School/Work:40 Home:20 Sleeping:5 Physical:undefined Errands:null"
#         "Commuting:10 School/Work:40 Home:20 Sleeping:20 Physical:5 Errands:null"

# add columns to dataset
noiseUsers <- cbind(noiseUsers, Commuting_weekday, Commuting_weekend, 
                    SchoolWork_weekday, SchoolWork_weekend,
                    Home_weekday, Home_weekend, 
                    Sleeping_weekday, Sleeping_weekend, 
                    Physical_weekday, Physical_weekend,
                    Errands_weekday, Errands_weekend)


