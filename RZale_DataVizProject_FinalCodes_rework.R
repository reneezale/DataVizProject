
# clean memory ------------------------------------------------------------
rm(list = ls())
library(ggplot2)
library(dplyr)


# read in data ------------------------------------------------------------
#set working directory

mydata = read.csv("Survey.csv")


# see data ----------------------------------------------------------


head(mydata)



# see data types ----------------------------------------------------------

str(mydata)
summary(mydata)



# deliverable 1 ----------------------------------------------------------
####################Vax Status Bar Chart########################

#summary table vax status and proportion table
absoluteVax <- table(mydata$NewVaxstatus,
                     exclude = 'nothing')
propVax=prop.table(absoluteVax)*100                     

absoluteVax 
propVax



#making data frame for graph
(VaxtableFreq=as.data.frame(absoluteVax))
names(VaxtableFreq)=c("VaxStatus","Count")
VaxtableFreq
VaxtableFreq$Percent=as.vector(propVax)
VaxtableFreq

#adding an order to the columns
VaxXAxisOrder <- c(6, 3, 2, 5, 1, 4)
VaxtableFreq$VaxXAxisOrder=as.vector(VaxXAxisOrder)

VaxtableFreq

#base GGPLOT2 starts with a "base", telling WHAT VARIABLES TO PLOT

#############FUNCTIONAL CONSOLIDATED CODE TO USE IN DASH#######################
vaxbase = ggplot(data = VaxtableFreq, aes(x = reorder(VaxStatus, VaxXAxisOrder), y = Percent)) +
  geom_bar(fill ="gray", stat = 'identity') + 
  geom_text(aes(label = paste(round(Percent), "%")), vjust = -0.5, size = 3) + 
  labs(x = "Vaccination Status", y = "Percent", caption = "Source: DACSS National Survey, Fall 2023") +
  ggtitle("Vaccination Status with new COVID-19 Vaccine", subtitle = "Fall 2023") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

vaxbase



# save vaxplot4 ----------------------------------------------------------
saveRDS(vaxbase, file = "VaxStatusBarChart.rds")

######################Age Histogram VAXXED RESPONDENTS#########################
barWIDTH=15
library(ggplot2)

VaxedData <- filter(mydata, NewVaxstatus == "Received")
y.AxisTextAge="Age"

VaxtitleText='Age of Vaccinated Respondents'
sub_titleText='Fall 2023'
sourceText='Source: DACSS National Survey, Fall 2023'

AgeHistbase= ggplot(VaxedData)  
h1= AgeHistbase + geom_histogram(aes(x = age),
                          binwidth = barWIDTH,
                          color = "#000000",
                            fill= "gray45") 
h1=h1 + labs(y="count")
h1

histy.AxisTextAge="Count of Vaccinated Respondents"
histx.AxisTextAge="Age"

VaxAgeHist = h1 + labs(title=VaxtitleText,
                                    x = histx.AxisTextAge, 
                       y = histy.AxisTextAge,
                                    caption = sourceText) 
VaxAgeHist

VaxAgeHist = VaxAgeHist + theme_minimal()

VaxAgeHist


#Adding in mean line and label
mn=mean(VaxedData$age,na.rm = T)
txtMean=paste0('Mean Age: ',round(mn))
txtMean

VaxAgeHist <- VaxAgeHist + geom_vline(xintercept = mn,color='red') + 
  annotate(geom = 'text',color='red',
           label=txtMean, # mean as text
           y = 19,
           x=mn+10,
           angle=0) 
VaxAgeHist



# save del2Draft ----------------------------------------------------------
saveRDS(VaxAgeHist, file = "VaxAgeHistFinal.rds")


#########################AGE HISTOGRAM NO PLANS RESPONDENTS################ 

barWIDTH=15
library(ggplot2)

VaxedData2 <- filter(mydata, NewVaxstatus == "No plans")
y.AxisTextAge="Age"

VaxtitleText='Age of Respondents with No Plans to Receive Updated Vaccine'
sub_titleText='Fall 2023'
sourceText='Source: DACSS National Survey, Fall 2023'

AgeHistbase= ggplot(VaxedData2)  
h1= AgeHistbase + geom_histogram(aes(x = age),
                                 binwidth = barWIDTH,
                                 color = "#000000",
                                 fill= "gray45") 
h1=h1 + labs(y="count")
h1

histy.AxisTextAge="Count of 'Vaccinate'No Plans' Respondents"
histx.AxisTextAge="Age"

VaxAgeHist2 = h1 + labs(title=VaxtitleText,
                       x = histx.AxisTextAge, 
                       y = histy.AxisTextAge,
                       caption = sourceText) 
VaxAgeHist2

VaxAgeHist2 = VaxAgeHist2 + theme_minimal()

VaxAgeHist2

#Adding in mean line and label
mn2 =mean(VaxedData2$age,na.rm = T)
txtMean2 =paste0('Mean Age: ',round(mn2))
txtMean2

VaxAgeHist2 <- VaxAgeHist2 + geom_vline(xintercept = mn2,color='red') + 
  annotate(geom = 'text',color='red',
           label=txtMean2, # mean as text
           y = 38,
           x=mn2+10,
           angle=0) 
VaxAgeHist2

# save del2Draft ----------------------------------------------------------
saveRDS(VaxAgeHist2, file = "VaxAgeHistFinalNoVax.rds")

# deliverable 3 Bivariate ----------------------------------------------------------
#########################Stacked Bar Chart#############################


###########Making data frame###############
(VaxedByPolitics = table(mydata$SimpleVaxStatus,mydata$SimplePolitical))
library(magrittr) # for %>%
(VaxedByPoliticsProp = (prop.table(VaxedByPolitics,
                            margin = 2)%>%round(.,3)))

VaxedByPoliticsDF = as.data.frame(VaxedByPolitics)

VaxedByPoliticsDF
names(VaxedByPoliticsDF)=c("VaxStatus","PoliticalAffiliation","counts")
VaxedByPoliticsDF$pctCol = as.data.frame(VaxedByPoliticsProp)[,3]
VaxedByPoliticsDF




#Reordering Political Affiliation Bar
VaxedByPoliticsDF$PoliticalAffiliationF <- factor(VaxedByPoliticsDF$PoliticalAffiliation, levels=c('Liberal', 'Moderate','Conservative','Not Sure'))
summary(VaxedByPoliticsDF$PoliticalAffiliationF)

summary(VaxedByPoliticsDF)

#####Making Chart############
library(scales)

stackedbase = ggplot(data=VaxedByPoliticsDF,aes(x= PoliticalAffiliationF, y=pctCol, fill = VaxStatus)) + 
  theme_minimal() +
  geom_bar(stat = "identity", position = 'stack') +
  geom_text(aes(label = paste(round((pctCol*100), 0), "%")), position = position_stack(vjust = 0.5)) + ####Labels showing incorrectly
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Political Affiliation", y = "", fill = "Vaccination Status", caption = "Source: DACSS National Survey, Fall 2023") +
  ggtitle("Politics and Vaccination Plans", subtitle = "New COVID-19 Vaccination Status by Political Affiliation") 

stackedbase

# save del3Draft ----------------------------------------------------------
saveRDS(stackedbase, file = "StackedBar2.rds")


############Region Attempt##################
library(sf)
region_map=sf::read_sf("cb_2022_us_region_500k.shp")
head(region_map)
head(mydata)

mydata4map <- mutate(mydata, SimpleVaxStatus_Num = recode(SimpleVaxStatus, 
                                                          `No plans` = "1", 
                                                          `Im not sure` = "2",
                                                          `Planning to` = "3",
                                                          `Received` = "4"))

mydata4map$SimpleVaxStatus_Num <- as.numeric(mydata4map$SimpleVaxStatus_Num)

head(mydata4map)

mydata4map %>%
  group_by(Region, State) %>%
  summarise_at(vars(SimpleVaxStatus_Num), list(counts=length, AvVax_perCap=mean)) -> mydata4map


mydata4map

library(dplyr)

# merge data into map ----------------------------------------------------------

myMapVaxregion=merge(region_map,mydata4map,by.x='NAME',"Region")

head(myMapVaxregion)
library(classInt)
# prepare plot

basemapregion = ggplot(myMapVaxregion) + 
  geom_sf(aes(fill=AvVax_perCap)) + 
  scale_fill_viridis_c(direction = -1, labels = c("No plans", "Not sure", "Plan to", "Received")) +
  labs (x="", y="", fill = "Average Vaccination Status", caption = "Source: DACSS National Survey, Fall 2023") + 
  ggtitle("Adoption of 2023-2024 COVID-19 Vaccine", subtitle = "Average Vaccination Level by Region")

basemapregion

# save del4Draft ----------------------------------------------------------
saveRDS(basemapregion, file = "RegionVaxMap.rds")

########################ATTEMPTING AND FAILING TO CROP OUT ISLANDS########################

#region_crop <- st_crop(region_map
                       #, xmin = -122.9, xmax = -122.5, ymin = 49.1, ymax = 49.3)

library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
usa <- world[world$name == "United States, "]

region_map2 = sf::read_sf("cb_2022_us_region_500k.shp")
head(region_map2)

st_crs(region_map2)
st_crs(usa)
#stetting data sets to same CRS 

target_crs <- st_crs(usa)
st_crs(region_map2) <- target_crs
st_crs(region_map2)

st_crs(usa)
st_crs(region_map2)

# Simplify geometries
region_map2 <- st_simplify(region_map2, preserveTopology = TRUE, dTolerance = 0.1)

# Enable spatial indexing
region_map2 <- st_set_crs(region_map2, st_crs(usa))

###Crop map, takes a long time
region_crop <- sf::st_intersection(region_map2, usa)
ggplot(region_crop)
head(mydata)

mydata4map <- mutate(mydata, SimpleVaxStatus_Num = recode(SimpleVaxStatus, 
                                                          `No plans` = "1", 
                                                          `Im not sure` = "2",
                                                          `Planning to` = "3",
                                                          `Received` = "4"))

mydata4map$SimpleVaxStatus_Num <- as.numeric(mydata4map$SimpleVaxStatus_Num)

head(mydata4map)

mydata4map %>%
  group_by(Region, State) %>%
  summarise_at(vars(SimpleVaxStatus_Num), list(counts=length, AvVax_perCap=mean)) -> mydata4map


mydata4map

library(dplyr)

# merge data into map ----------------------------------------------------------

myMapVaxregion2 = merge(region_crop,mydata4map,by.x='NAME',"Region")

head(myMapVaxregion2)
library(classInt)
# prepare plot

basemapregion2 = ggplot(myMapVaxregion2) + 
  geom_sf(aes(fill=AvVax_perCap)) + 
  scale_fill_viridis_c(direction = -1, labels = c("No plans", "Not sure", "Plan to", "Received")) +
  labs (x="", y="", fill = "Average Vaccination Status", caption = "Source: DACSS National Survey, Fall 2023") + 
  ggtitle("Adoption of 2023-2024 COVID-19 Vaccine", subtitle = "Average Vaccination Level by Region")

basemapregion2

#############DID NOT WORK######################


####################State Map NOT USING############################

library(sf)

state_map=sf::read_sf("cb_2022_us_state_500k.geojson")
head(state_map)
head(mydata)

str(mydata$SimpleVaxStatus)

mydata4map <- mutate(mydata, SimpleVaxStatus_Num = recode(SimpleVaxStatus, 
                                                          `No plans` = "1", 
                                                          `Im not sure` = "2",
                                                          `Planning to` = "3",
                                                          `Received` = "4"))

mydata4map$SimpleVaxStatus_Num <- as.numeric(mydata4map$SimpleVaxStatus_Num)

head(mydata4map)

mydata4map %>%
  group_by(Region, State) %>%
  summarise_at(vars(SimpleVaxStatus_Num), list(counts=length, AvVax_perCap=mean)) -> mydata4map


mydata4map

library(dplyr)

# merge data into map ----------------------------------------------------------
#mydataState=aggregate(data=mydata,Free.Lunch~County,FUN = mean)
myMapVax=merge(state_map,mydata4map,by.x='STUSPS',"State")

head(myMapVax)
library(classInt)
# prepare plot

basemap = ggplot(myMapVax) + 
  geom_sf(aes(fill=AvVax_perCap)) + 
  scale_fill_viridis_c(direction = -1, labels = c("No plans", "Not sure", "Plan to", "Received")) +
  labs (x="", y="", fill = "Average Vaccination Status", caption = "Source: DACSS National Survey, Fall 2023") + 
  ggtitle("Adoption of 2023-2024 COVID-19 Vaccine", subtitle = "Average Vaccination Level by Region")

basemap

# save del4Draft ----------------------------------------------------------
saveRDS(basemap, file = "VaxMap.rds")
