
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
vaxbase = ggplot(data = VaxtableFreq, 
             aes(x = reorder(VaxStatus, VaxXAxisOrder), # horizontal
                 y = Percent)) #vertical

vaxplot1 = vaxbase + geom_bar(fill ="gray",
                        stat = 'identity') # notice the "stat"
vaxplot1

titleText='Vaccination Status with new COVID-19 Vaccine'
sub_titleText='Fall 2023'
sourceText='Source: DACSS National Survey'

x.AxisText="Vaccination Status"
y.AxisText="Percent"

vaxplot2 = vaxplot1 + labs(title=titleText,
                     x =x.AxisText, 
                     y = y.AxisText,
                     caption = sourceText) 
vaxplot2

#adding %s on labels
#vaxplot3 = vaxplot2 + scale_y_continuous(breaks=c(0,10, 20, 30, 40),
                                         #limits = c(0, 45), 
                                         #labels = label_number(suffix = "%")) #####Not working
#vaxplot3

#adding in percentage labels 
####################Giving me error in dashboard############################
VAXLABELS = paste(round(VaxtableFreq$Percent,0),'%')


vaxplot3 = vaxplot2 + geom_text(vjust=0, #hjust if flipping
                          size = 5,
                          aes(y = Percent ,
                              label = VAXLABELS))

vaxplot3


#Tilting X axis names so that they fit 
vaxplot4 <- vaxplot3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
vaxplot4

#####Having trouble shifting labels######
#vaxplot5 <- vaxplot4 + geom_text(data = subset(vaxbase, VaxStatus == 4), aes(label = VAXLABELS),
          #position = position_dodge(width = 0.8), vjust = -0.6, size = 5)

# save vaxplot4 ----------------------------------------------------------
saveRDS(vaxplot4, file = "VaxStatusBarChart.rds")

######################Age Histogram#########################
barWIDTH=15
library(ggplot2)

VaxedData <- filter(mydata, NewVaxstatus == "Received")
y.AxisTextAge="Age"

VaxtitleText='Age of Vaccinated Respondents'
sub_titleText='Fall 2023'
sourceText='Source: DACSS National Survey, 2023'

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

# deliverable 3 Bivariate ----------------------------------------------------------
#########################Stacked Bar Chart#############################
(VaxedByPolitics = table(mydata$SimpleVaxStatus,mydata$SimplePolitical))
library(magrittr) # for %>%
(VaxedByPoliticsProp = prop.table(VaxedByPolitics,
                            margin = 2)%>%round(.,3))

VaxedByPoliticsDF = as.data.frame(VaxedByPolitics)

VaxedByPoliticsDF
names(VaxedByPoliticsDF)=c("VaxStatus","PoliticalAffiliation","counts")
VaxedByPoliticsDF$pctCol = as.data.frame(VaxedByPoliticsProp)[,3]
VaxedByPoliticsDF

#Reordering Political Affiliation Bar
VaxedByPoliticsDF$PoliticalAffiliationF <- factor(VaxedByPoliticsDF$PoliticalAffiliation, levels=c('Liberal', 'Moderate','Conservative','Not Sure'))
summary(VaxedByPoliticsDF$PoliticalAffiliationF)



summary(VaxedByPoliticsDF)


base2 = ggplot(data=VaxedByPoliticsDF,
             aes(x= PoliticalAffiliationF,
                 y=pctCol, # % not counts
                 fill=VaxStatus)) + theme_minimal()

barStacked2 = base2 + geom_bar(stat = "identity",
                               position = 'stack')#default
barStacked2

library(scales)
barStacked2= barStacked2 + geom_text(size = 5,# check below:
                                     position = position_stack(vjust = 0.5),# center
                                     aes(label=percent(pctCol,accuracy = 0.1)))# percent format

barStacked2 = barStacked2 + scale_y_continuous(labels = scales::percent)

barStacked2

barStacked2 = barStacked2 + theme_minimal() +
  labs(title = "Politics and Vaccination Plans",
       subtitle = "New COVID-19 Vaccination Status by Political Affiliation",
       x="",
       y="",
       fill='Vaccination Status',
       caption = "Source: DACSS National Survey, 2023") +
  theme(axis.text.y =element_blank())

barStacked2



# save del3Draft ----------------------------------------------------------
saveRDS(barStacked2, file = "StackedBar.rds")



# deliverable 4  ----------------------------------------------------------

library(sf)
county_map=sf::read_sf("WA_County_Boundaries.geojson")
head(county_map)
head(mydata)

# merge data into map ----------------------------------------------------------
mydataCounty=aggregate(data=mydata,Free.Lunch~County,FUN = mean)
myMapLunch=merge(county_map,mydataCounty,by.x='JURISDIC_2',"County")

# prepare plot

base=ggplot(myMapLunch)
del4Draft=base + geom_sf(aes(fill=Free.Lunch))
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")