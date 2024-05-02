############################################################
##Data Wrangling of Mortality Event Data
##written by Andrea Ayala last change 3/12/24
##Reference sites:
##https://www.statmethods.net/management/merging.html
##https://www.statmethods.net/management/subset.html
##https://statsandr.com/blog/descriptive-statistics-in-r/
##https://www.marsja.se/how-to-rename-column-or-columns-in-r-with-dplyr/
##https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
##http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
##https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression
##https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
##https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
##https://stackoverflow.com/questions/1296646/sort-order-data-frame-rows-by-multiple-columns
##http://homepage.stat.uiowa.edu/~luke/classes/STAT4580-2022/proportions.html
##https://statsandr.com/blog/chi-square-test-of-independence-in-r/
##https://bookdown.org/danieljcarter/r4steph/two-way-frequency-tables.html
##http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
##https://stats.stackexchange.com/questions/41270/nonparametric-equivalent-of-ancova-for-continuous-dependent-variables
##https://stackoverflow.com/questions/36459967/ggplot-glm-fitted-curve-without-interaction
##https://danstich.github.io/worst-r/13.1-intro13.html
##https://www.r-bloggers.com/2021/06/oddsplotty-has-landed-on-cran/
##https://stats.stackexchange.com/questions/168928/categorical-variables-in-lasso-regression
#####################################################################################

#Loading necessary packages
library(readr)
library(ggplot2)
library(googlesheets4)
library(httpuv)
library(googledrive)
library(gsheet)
library(dplyr)
library(pastecs)
library(plyr)
library(data.table)
library(tidyr)
library(ggpubr)
library(TTAinterfaceTrendAnalysis)
library(stargazer)
library(RColorBrewer)
library(viridis)
library(glue)
library(hrbrthemes)
library(vcd)
library(ggstatsplot)
library(epiDisplay)
library(tidyverse)
library(magrittr)
library(sm)
library(aod)
library(MASS)
library(leaps)
library(caret)
library(sandwich)
library(broom)
library(boot)
library(pscl)
library(lattice)
library(lmtest)
library(lme4)
library(dplyr)
library(epitools)
library(Epi)

rm(list=ls()) #this clears the workspace to make sure no leftover variables are left. Not strictly needed but often a good idea
#graphics.off(); #close all graphics windows, in case there are still some open from previous work performed

#Starting with descriptive statistics of bird cases

#Downloading the google sheet from the web - we have only one sheet, so we are going to end up with one dataframe

ms0<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1uO7Frb7MtkTsxXAJL0OxI4BKrXiuPTQH/edit#gid=1129042798')

#Let's convert the relevant variables into factors
ms0$Sample_ID<-as.factor(ms0$Sample_ID)
ms0$Species<-as.factor(ms0$Species)
ms0$State<-as.factor(ms0$State)
ms0$Clinical_Control<-as.factor(ms0$Clinical_Control)
ms0$Cases<-as.numeric(ms0$Cases)
ms0$Order<-as.factor(ms0$Order)
ms0$Family<-as.factor(ms0$Family)
ms0$Genus<-as.factor(ms0$Genus)
ms0$Seasonality<-as.factor(ms0$Seasonality)
ms0$Migration<-as.factor(ms0$Migration)
ms0$Feederwatch<-as.factor(ms0$Feederwatch)
ms0$Habitat<-as.factor(ms0$Habitat)
ms0$Primary_Food<-as.factor(ms0$Primary_Food)
ms0$Niche<-as.factor(ms0$Niche)
ms0$Small_Tube<-as.factor(ms0$Small_Tube)
ms0$Large_Hopper<-as.factor(ms0$Large_Hopper)
ms0$Small_Hopper<-as.factor(ms0$Small_Hopper)
ms0$Suet_Cage<-as.factor(ms0$Suet_Cage)
ms0$Large_Tube<-as.factor(ms0$Large_Tube)
ms0$Platform_Feeder<-as.factor(ms0$Platform_Feeder)
ms0$Ground_Feeder<-as.factor(ms0$Ground_Feeder)
ms0$SunflowerSeeds<-as.factor(ms0$SunflowerSeeds)
ms0$Hulled_Sunflower<-as.factor(ms0$Hulled_Sunflower)
ms0$Safflower<-as.factor(ms0$Safflower)
ms0$Suet<-as.factor(ms0$Suet_Cage)
ms0$Cracked_Corn<-as.factor(ms0$Cracked_Corn)
ms0$Peanuts<-as.factor(ms0$Peanuts)
ms0$Peanut_Hearts<-as.factor(ms0$Peanut_Hearts)
ms0$Fruit<-as.factor(ms0$Fruit)
ms0$Millet<-as.factor(ms0$Millet)
ms0$Oats<-as.factor(ms0$Oats)
ms0$Milo<-as.factor(ms0$Milo)
ms0$Mealworms<-as.factor(ms0$Mealworms)
ms0$Nyjer<-as.factor(ms0$Nyjer)
ms0$Birds<-as.factor(ms0$Birds)
ms0$Insects<-as.factor(ms0$Insects)
ms0$Non_feeder_seeds<-as.factor(ms0$Non_feeder_seeds)
ms0$Berries_Plants<-as.factor(ms0$Berries_Plants)
ms0$No_Food_Types<-as.numeric(ms0$No_Food_Types)
ms0$No_Feeder_types<-as.numeric(ms0$No_Feeder_types)
ms0$AlphaCode<-as.factor(ms0$AlphaCode)
ms0$Status<-as.factor(ms0$Status)

str(ms0)#tells us the structure of our variables
print(ms0)

#First, we are going to remove rows that represent "Controls" and 99

Clin<-ms0[!(ms0$Clinical_Control == "Control" | ms0$ID=="unknown"),]                                   # Subset rows with ==
print(Clin)
View(Clin)

Clin1<-Clin
View(Clin1)

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by Order

Clin2<-table(unlist(Clin1$Order))
Clin2

#Now let's convert our table to a dataframe so we can perform test of the distribution of the data (Cullen and Frey)
Clin3<- aggregate(Clin1$Cases, by=list(Order=Clin1$Order), FUN=sum)
Clin3

#shapiro.test(Clin3$`sum.Clin1$Cases`)

library(fitdistrplus)
descdist(Clin3$`sum.Clin1$Cases`)


#Shapiro-Wilk normality test

#data:  Clin3$`sum.Clin1$Cases`
#W = 0.6518, p-value = 0.002725 #data is non-normal

#Now let's do family

Clin4<-table(unlist(Clin1$Family))
Clin4

#Clin2<-Clin2[Clin2$ID!= "unknown", ]                          # Remove row based on condition
#Clin2

#Now let's convert our table to a dataframe so we can perform test of the normality of the data (Cullen and Frey)
Clin5<- aggregate(Clin1$Cases, by=list(Order=Clin1$Family), FUN=sum)
Clin5

#shapiro.test(Clin5$`sum.Clin1$Cases`)

library(fitdistrplus)
descdist(Clin5$`sum.Clin1$Cases`)

#Shapiro-Wilk normality test

#data:  Clin5$`sum.Clin1$Cases`
#W = 0.52532, p-value = 1.591e-05

#Now, we are going to aggregate and visualize the data representing the avian phylogeny of the clinical cases

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases 
#first by AVIAN ORDER

#Lets rename the second column 

colnames(Clin3)[2] ="cases"
Clin3

#Let's convert the cases variable to a factor so we can play with the levels
Clin3$cases<-as.factor(Clin3$cases)

levels(Clin3$cases) #it's in alphabetical order, but we want descending order

Clin3<-Clin3 %>% arrange(desc(cases)) #this puts our cases into descending order
View(Clin3)

#let's plot the total cases from the greatest to the least by running a factor() function. 
#This works only if your data is already in order from greatest to least of the total.

Clin3$cases<-factor(Clin3$cases, Clin3$cases)


####AVIAN FAMILIES

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases 
#by AVIAN FAMILY

Clin7<-ms0[!(ms0$Clinical_Control == "Control" | ms0$ID=="unknown"| ms0$Family=="99"),]                                   # Subset rows with ==
View(Clin7)                  # Remove row based on condition

Clin7<-droplevels(Clin7)
Clin7

ms_family_occurences<-table(unlist(Clin7$Family))
ms_family_occurences

#Now let's convert our table to a dataframe so we can plot the occurrences of this
ms_family_occurences1<- aggregate(Clin7$Cases, by=list(Family=Clin7$Family), FUN=sum)
ms_family_occurences1

#Now let's do a shapiro-wilk's test

shapiro.test(ms_family_occurences1$`sum.Clin7$Cases`)

#Shapiro-Wilk normality test

#data:  Clin7$`sum.Clin1$Cases`
#W = 0.52532, p-value = 1.591e-05 #data is non-normal


#Now, we are going to aggregate and visualize the data representing the avian phylogeny of the clinical cases

#Since our data is representative of individual cases, we will need to aggregate the data to plot cases 
#second by AVIAN FAMILY

#Lets rename the second column 

colnames(ms_family_occurences1)[2] ="cases"
ms_family_occurences1

#Let's convert the cases variable to a factor so we can play with the levels
ms_family_occurences1$cases<-as.factor(ms_family_occurences1$cases)

levels(ms_family_occurences1$cases) #it's in alphabetical order, but we want descending order

ms_family_occurences1<-ms_family_occurences1 %>% arrange(desc(cases)) #this puts our cases into descending order
View(ms_family_occurences1)

#let's plot the total cases from the greatest to the least by running a factor() function. 
#This works only if your data is already in order from greatest to least of the total.

ms_family_occurences1$Family <- factor(ms_family_occurences1$Family, ms_family_occurences1$Family)

########################################################################################################


#Let's plot Order & Family using a stacked bar chart
#We're going to start back with our original dataset 

#We are going to remove the Controls and the 99's/unknown from our dataset

ms1<-ms0[!(ms0$Clinical_Control=="Control" | ms0$ID=="unknown"),]
View(ms1)

#Changing the levels of the Orders so that they are in descending order

ms1$Order<-factor(ms1$Order, levels = c("Passeriformes", "Accipitriformes", 
                                        "Columbiformes", "Strigiformes"))

ggplot(ms1, aes(fill=Family, y=Cases, x=Order)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Avian Families Categorized by Order") +
  theme_ipsum() +
  xlab("")

##Now, let's plot Family cases using a stacked pie chart - we are going to use the dataframe
#ms_family_occurences1


ms_family_occurences1[,2] <- as.numeric(as.character(ms_family_occurences1[,2]))

#Now we are going to make a CSV of the family data
#write.csv(ms_family_occurences1,file='C:/Users/andre/OneDrive/Desktop/Avian_families.csv', row.names=TRUE)


# Make the plot
# First start with hole size
hsize <- 5

ms_family_occurences1 <- ms_family_occurences1 %>% 
  mutate(x = hsize)

p1<-ggplot(ms_family_occurences1, aes(x = hsize, y = cases, fill = Family)) +
  geom_col() +
  scale_fill_viridis_d(alpha = 0.5) +
  geom_text(aes(label = cases),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  xlab(" ") +
  ylab("Number of Cases ")
p1

#now we are going to plot species

#Clin1A #tells us how many cases per species we are looking at

#Changing the levels of the Species so that they are in descending order



library(dplyr)
ms1 <- ms1 %>%
  mutate(Species = recode(Species, BlueJay = 'Blue Jay', CommonGrackle ='Common Grackle', AmericanRobin = 'American Robin', 
                          EuropeanStarling = 'European Starling', NorthernCardinal = 'Northern Cardinal', HouseFinch ='House Finch',
                          CoopersHawk = 'Coopers Hawk', HouseSparrow = 'House Sparrow', MourningDove = 'Mourning Dove', NorthernMockingbird = 'Northern Mockingbird', AmericanCrow = 'American Crow', RustyBlackbird = 'Rusty Blackbird',
                          SharpShinnedHawk = 'Sharp-shinned Hawk', EasternPhoebe = 'Eastern Phoebe', RockPigeon = 'Rock Pigeon', TuftedTitmouse = 'Tufted Titmouse', EasternScreechOwl = 'Eastern Screech Owl',
                          RosebreastedGrosbeak = 'Rose-breasted Grosbeak'))
ms1

ms1$Species<-factor(ms1$Species, levels = c("Blue Jay", 
                                            "Common Grackle",
                                            "American Robin",
                                            "European Starling", 
                                            "Northern Cardinal",
                                            "House Finch",
                                            "Coopers Hawk",
                                            "House Sparrow",
                                            "Mourning Dove",
                                            "Northern Mockingbird",
                                            "American Crow",
                                            "Rusty Blackbird",
                                            "Sharp-shinned Hawk",
                                            "Eastern Phoebe",
                                            "Rock Pigeon",
                                            "Tufted Titmouse",
                                            "Eastern Screech Owl",
                                            "Rose-breasted Grosbeak"))

ms1nona <- ms1 %>% drop_na(Species)
#write.csv(ms1nona,file='C:/Users/andre/OneDrive/Desktop/Avian_Species.csv', row.names=TRUE)



p3<-ggplot(ms1nona, aes(fill=Species, y=Cases, x=Species)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Avian Species Cases") +
  theme_ipsum() +
  xlab("")
p3 


p3 + theme(axis.text.x = element_text(angle = 90))

#Aggregating the data to add text to the Species plot, renaming it p4

dfl <- ddply(ms1nona, .(Species), summarize, y=length(Species))
str(dfl)

p4<-ggplot(dfl, aes(Species, y=y, fill=Species)) + geom_bar(stat="identity") +
  geom_text(aes(label=y), vjust=-0.3) +
  scale_fill_viridis(discrete = T) +
  ggtitle("Avian Species Cases") +
  theme_ipsum() +
  xlab("") +
  ylab("Cases")
p4 


p4 + theme(axis.text.x = element_text(angle = 90))


#Now let's look at our data in terms of cases according to Order
#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by Order

##Let's examine Order statistically

Order = subset(ms1, select = c("Order"))
print(Order)
str(Order)

table(Order)

Order <- droplevels(Order) #Gets rid of the level "99"
Order

table(Order)

#Doing proportions

tablea <- table(Order)
prop.table(tablea)

#Now lets do a chi-square test
#there are unknowns, so best to stick with 177, 6, 3, 1

Order.Chi<-c(177, 6, 3, 1)

chisq.test(Order.Chi, p = c(.25, .25, .25, .25))

####doing follow-up chi-square tests to determine where the significance is
Order.Chi_1<-c(177, 6) #Passer vs. Accipt
Order.Chi_2<-c(177, 3) #Passer vs. Columb
Order.Chi_3<-c(177, 1) #Passer vs. Strig
Order.Chi_4<-c(6, 3)   #Accipt vs. Columb
Order.Chi_5<-c(6, 1)   #Accipt vs. Strig
Order.Chi_6<-c(3, 1)   #Columb vs. Strig

chisq.test(Order.Chi_1) ##Passer vs. Accipt
chisq.test(Order.Chi_2) #Passer vs. Columb
chisq.test(Order.Chi_3) #Passer vs. Strig
chisq.test(Order.Chi_4) ##Accipt vs. Columb
chisq.test(Order.Chi_5) #Accipt vs. Strig
chisq.test(Order.Chi_6) #Columb vs. Strig

#Following a Chi-Square test that includes an explanatory variable with 3 or more groups, 
#we need to subset to each possible paired comparison. When interpreting these paired comparisons, 
#rather than setting the  ??
#-level (p-value) at 0.05, we divide 0.05 by the number of paired comparisons that we will be making. 
#The result is our new  ??
#-level (p-value)

#When we evaluate the p-value for each of these post hoc chi-square tests, 
#we will use 0.05/6 = 0.0083 as our alpha. 

###############################Ecological Variables####################################################################
#Now let's look at our data according to Seasonality
#Since our data is representative of individual cases, we will need to aggregate the data to plot cases by Seasonality
#However, we still have one individual that has a 99 for many of its variables, so we will need to remove it
View(ms1)

ms2<-ms1[ms1$Sample_ID != "21-7963", ]      
View(ms2)

#Let's extract Seasonality from the dataset

seasonal = subset(ms2, select = c("Seasonality"))
print(seasonal)
str(seasonal)

table(seasonal)

seasonal <- droplevels(seasonal) #Gets rid of the level "99"
seasonal

table(seasonal)

#Doing proportions

table2 <- table(seasonal)
prop.table(table2)

#Now lets do a chi-square test

seasonal.Chi<-c(2, 1, 183, 1)

chisq.test(seasonal.Chi, p = c(.25, .25, .25, .25))

####doing follow-up chi-square tests to determine where the significance is
seasonal.Chi_1<-c(2, 1) #Breeding vs. Non
seasonal.Chi_2<-c(2, 183) #Breeding vs. Yearround
seasonal.Chi_3<-c(2, 1) #Breeding vs. Yearround.Breeding 
seasonal.Chi_4<-c(1, 183) #Non vs. Yearround
seasonal.Chi_5<-c(1, 1) #Non vs. YB
seasonal.Chi_6<-c(183, 1) #Yearround vs. YB

chisq.test(seasonal.Chi_1)
chisq.test(seasonal.Chi_2)
chisq.test(seasonal.Chi_3)
chisq.test(seasonal.Chi_4)
chisq.test(seasonal.Chi_5)
chisq.test(seasonal.Chi_6)

#Following a Chi-Square test that includes an explanatory variable with 3 or more groups, 
#we need to subset to each possible paired comparison. When interpreting these paired comparisons, 
#rather than setting the  ??
#-level (p-value) at 0.05, we divide 0.05 by the number of paired comparisons that we will be making. 
#The result is our new  ??
#-level (p-value)

#When we evaluate the p-value for each of these post hoc chi-square tests, 
#we will use 0.05/6 = 0.0083 as our alpha. 

#Let's do a Poisson Chi-Square GOF Test for seasonal data - changed by Sabrina's suggestion

ms2<-droplevels(ms2)
ms2

#Now let's convert our table to a dataframe so we can perform test of the normality of the data (shapiro-wilks)
ms_2 <- aggregate(list(x = ms2$Cases), list(Seasonality = ms2$Seasonality), FUN = sum)
ms_2

#Analyzing the distribution of the variable using the Cullen and Frey plot. 

library(fitdistrplus)
descdist(ms_2$x)

#Beta distribution

#estimated skewness:  1.999839 
#estimated kurtosis:  6.999459 


#shapiro.test(ms_2$x)

#Shapiro-Wilk normality test

#data:  ms_2$`sum.ms2$Cases`
#W = 0.63375, p-value = 0.00144

#####################Let's do Feederwatch

Feederw = subset(ms2, select = c("Feederwatch"))
print(Feederw)
str(Feederw)

table(Feederw)

Feederw <- droplevels(Feederw) #Gets rid of the level "99"
Feederw

table(Feederw)

#Doing proportions

table4 <- table(Feederw)
prop.table(table4)

#Now lets do a chi-square test

Feederw.Chi<-c(10, 177)

chisq.test(Feederw.Chi, p = c(.5, .5))

####doing follow-up chi-square tests to determine where the significance is
Feederw.Chi_1<-c(10, 177) #10 = No, 177 = yes

chisq.test(Feederw.Chi_1)

#Cannot do a shapiro-wilk's for binary data

####################Let's do Primary Food

Food = subset(ms2, select = c("Primary_Food"))
print(Food)
str(Food)

table(Food)

Food <- droplevels(Food) #Gets rid of the level "99"
Food

table(Food)

#Doing proportions

table5 <- table(Food)
prop.table(table5)

#Now lets do a chi-square test

Food.Chi<-c(6, 32, 131, 17, 1)

chisq.test(Food.Chi, p = c(.2, .2, .2, .2, .2))

####doing follow-up chi-square tests to determine where the significance is
Food.Chi_1<-c(6, 32) #Birds vs. Insects
Food.Chi_2<-c(6, 131) #Birds vs. Omnivores
Food.Chi_3<-c(6, 17) #Birds vs. Seeds
Food.Chi_4<-c(6, 1) #Birds vs. Small Animals
Food.Chi_5<-c(32, 131) #Insects vs. Omnivores
Food.Chi_6<-c(32, 17) #Insects vs. Seeds
Food.Chi_7<-c(32, 1) # Insects vs. Small Animals
Food.Chi_8<-c(131, 17) #Omni vs. Seeds
Food.Chi_9<-c(131, 1) #Omni vs. Small Animals
Food.Chi_10<-c(17, 1) #Seeds vs. SA


chisq.test(Food.Chi_1)
chisq.test(Food.Chi_2)
chisq.test(Food.Chi_3)
chisq.test(Food.Chi_4)
chisq.test(Food.Chi_5)
chisq.test(Food.Chi_6)
chisq.test(Food.Chi_7)
chisq.test(Food.Chi_8)
chisq.test(Food.Chi_9)
chisq.test(Food.Chi_10)

#When we evaluate the p-value for each of these post hoc chi-square tests, 
#we will use 0.05/10 = 0.005 as our alpha. 

#Cullen and Frey Distribution Plot

#Now let's convert our table to a dataframe so we can perform test of the normality of the data - Cullen and Frey
ms_3 <- aggregate(list(x = ms2$Cases), list(Primary_Food = ms2$Primary_Food), FUN = sum)
ms_3

library(fitdistrplus) #Omnivores
descdist(ms_3$x)

#Cullen and Frey Results
#estimated skewness:  1.974617 
#estimated kurtosis:  7.010337 


#data:  ms_3$`sum.ms2$Cases`
#W = 0.74578, p-value = 0.02719

####################Let's do Niche

Niche1 = subset(ms2, select = c("Niche"))
print(Niche1)
str(Niche1)

table(Niche1)

Niche1 <- droplevels(Niche1) #Gets rid of the level "99"
Niche1

table(Niche1)

#Doing proportions

table6 <- table(Niche1)
prop.table(table6)

#Now doing Chi-square goodness of fit tests

niche.Chi<-c(7, 1, 2, 177)

chisq.test(niche.Chi, p = c(.25, .25, .25, .25))

####doing follow-up chi-square tests to determine where the significance is
niche.Chi_1<-c(7, 1) #Aerial vs. Flycatching
niche.Chi_2<-c(7, 2) #Aerial vs. Foliage
niche.Chi_3<-c(7, 177) #Aerial vs. Ground
niche.Chi_4<-c(1, 2) #Flycatch vs. Foliage
niche.Chi_5<-c(1, 177) #Flycatch vs. Ground
niche.Chi_6<-c(2, 177) #Foliage vs. Ground

chisq.test(niche.Chi_1)
chisq.test(niche.Chi_2)
chisq.test(niche.Chi_3)
chisq.test(niche.Chi_4)
chisq.test(niche.Chi_5)
chisq.test(niche.Chi_6)

#When we evaluate the p-value for each of these post hoc chi-square tests, 
#we will use 0.05/6 = 0.0083 as our alpha. 

#Cullen and Frey Plot

#Now let's convert our table to a dataframe so we can perform test of the normality of the data (shapiro-wilks)
ms_4 <- aggregate(list(x = ms2$Cases), list(Niche = ms2$Niche), FUN = sum)
ms_4

library(fitdistrplus) #Niche - Aerial
descdist(ms_4$x)

#Shapiro-Wilk normality test

#data:  ms_4$`sum.ms2$Cases`
#W = 0.65514, p-value = 0.003047

####################Let's do Habitat

Habitat1 = subset(ms2, select = c("Habitat"))
print(Habitat1)
str(Habitat1)

table(Habitat1)

Habitat1 <- droplevels(Habitat1) #Gets rid of the level "99"
Habitat1

table(Habitat1)

#Doing proportions

table6 <- table(Habitat1)
prop.table(table6)

#Now doing Chi-square goodness of fit tests

Habitat.Chi<-c(110, 53, 24)

chisq.test(Habitat.Chi, p = c(1/3, 1/3, 1/3))

####doing follow-up chi-square tests to determine where the significance is
Habitat.Chi_1<-c(110, 53) #Forest vs OW
Habitat.Chi_2<-c(110, 24) #Forest vs. Towns
Habitat.Chi_3<-c(53, 24) #Open Woodlands vs. Towns


chisq.test(Habitat.Chi_1)
chisq.test(Habitat.Chi_2)
chisq.test(Habitat.Chi_3)


#When we evaluate the p-value for each of these post hoc chi-square tests, 
#we will use 0.05/3 = 0.016 as our alpha. 

#Cullen and Frey plot

#Now let's convert our table to a dataframe so we can perform test of the normality of the data (shapiro-wilks)
ms_5 <- aggregate(list(x = ms2$Cases), list(Habitat = ms2$Habitat), FUN = sum)
ms_5

library(fitdistrplus) #Habitat - town
descdist(ms_5$x) #No results, need at least four categories.

#Shapiro-Wilk normality test

#data:  ms_5$`sum.ms2$Cases`
#W = 0.96587, p-value = 0.6451

####################Let's do Natural Foods: Birds

Birds1 = subset(ms2, select = c("Birds"))
print(Birds1)
str(Birds1)

table(Birds1)

Birds1 <- droplevels(Birds1) #Gets rid of the level "99"
Birds1

table(Birds1)

#Doing proportions

table7 <- table(Birds1)
prop.table(table7)

#Now doing Chi-square goodness of fit tests

Birds.Chi<-c(55, 132) #No, 55, Yes 132

chisq.test(Birds.Chi, p = c(1/2, 1/2))

####################Let's do Natural Foods: Insects

Insects1 = subset(ms2, select = c("Insects"))
print(Insects1)
str(Insects1)

table(Insects1)

Insects1 <- droplevels(Insects1) #Gets rid of the level "99"
Insects1

table(Insects1) #No 13, Yes 174

#Doing proportions

table8 <- table(Insects1)
prop.table(table8)

#Now doing Chi-square goodness of fit tests

Insects.Chi<-c(13, 174) 

chisq.test(Insects.Chi, p = c(1/2, 1/2))

####################Let's do Natural Foods: Non Feeder Seeds

NFS1 = subset(ms2, select = c("Non_feeder_seeds"))
print(NFS1)
str(NFS1)

table(NFS1)

NFS1 <- droplevels(NFS1) #Gets rid of the level "99"
NFS1

table(NFS1) #No 26, Yes 161

#Doing proportions

table8 <- table(NFS1)
prop.table(table8)

#Now doing Chi-square goodness of fit tests

NFS.Chi<-c(26, 161) #

chisq.test(NFS.Chi, p = c(1/2, 1/2))

####################Let's do Natural Foods: Berries Plants

BP1 = subset(ms2, select = c("Berries_Plants"))
print(BP1)
str(BP1)

table(BP1)

BP1 <- droplevels(BP1) #Gets rid of the level "99"
BP1

table(BP1) #No 121, Yes 66

#Doing proportions

table9 <- table(BP1)
prop.table(table9)

#Now doing Chi-square goodness of fit tests

BP.Chi<-c(121, 66) #

chisq.test(BP.Chi, p = c(1/2, 1/2))

####################################################################################################################
#######Now doing Feeder Foods#######################################################################################
#####################################################################################################################
#Sunflower Seeds

Sun1 = subset(ms2, select = c("SunflowerSeeds"))
table(Sun1)

#Doing proportions

table14 <- table(Sun1)
prop.table(table14)

##Now doing Chi-square goodness of fit test

Sun1.Chi<-c(29, 158)

chisq.test(Sun1.Chi, p = c(.5, .5))

#Hulled Sunflower

Hulled1 = subset(ms2, select = c("Hulled_Sunflower"))
table(Hulled1)

#Doing proportions

table15 <- table(Hulled1)
prop.table(table15)

##Now doing Chi-square goodness of fit test

Hulled1.Chi<-c(10, 177)

chisq.test(Hulled1.Chi, p = c(.5, .5))

#Safflower

Saff1 = subset(ms2, select = c("Safflower"))
table(Saff1)

#Doing proportions

table16 <- table(Saff1)
prop.table(table16)

##Now doing Chi-square goodness of fit test

Saff1.Chi<-c(45, 142)

chisq.test(Saff1.Chi, p = c(.5, .5))

#Suet

Suet2 = subset(ms2, select = c("Suet"))
table(Suet2)

#Doing proportions

table17 <- table(Suet2)
prop.table(table17)

##Now doing Chi-square goodness of fit test

Suet2.Chi<-c(75, 112)

chisq.test(Suet2.Chi, p = c(.5, .5))

#Cracked Corn

Corn1 = subset(ms2, select = c("Cracked_Corn"))
table(Corn1)

#Doing proportions

table18 <- table(Corn1)
prop.table(table18)

##Now doing Chi-square goodness of fit test

Corn1.Chi<-c(36, 151)

chisq.test(Corn1.Chi, p = c(.5, .5))

##Peanuts

Peanuts1 = subset(ms2, select = c("Peanuts"))
table(Peanuts1)

#Doing proportions

table19 <- table(Peanuts1)
prop.table(table19)

##Now doing Chi-square goodness of fit test

Peanuts1.Chi<-c(50, 137)

chisq.test(Peanuts1.Chi, p = c(.5, .5))

##Peanut Hearts

Hearts1 = subset(ms2, select = c("Peanut_Hearts"))
table(Hearts1)

#Doing proportions

table20 <- table(Hearts1)
prop.table(table20)

##Now doing Chi-square goodness of fit test

Hearts1.Chi<-c(16, 171)

chisq.test(Hearts1.Chi, p = c(.5, .5))

##Fruit

Fruit1 = subset(ms2, select = c("Fruit"))
table(Fruit1)

#Doing proportions

table21 <- table(Fruit1)
prop.table(table21)

##Now doing Chi-square goodness of fit test

Fruit1.Chi<-c(32, 155)

chisq.test(Fruit1.Chi, p = c(.5, .5))

##Millet

Millet1 = subset(ms2, select = c("Millet"))
table(Millet1)

#Doing proportions

table22 <- table(Millet1)
prop.table(table22)

##Now doing Chi-square goodness of fit test

Millet1.Chi<-c(36, 151)

chisq.test(Millet1.Chi, p = c(.5, .5))

##Oats

Oats1 = subset(ms2, select = c("Oats"))
table(Oats1)

#Doing proportions

table23 <- table(Oats1)
prop.table(table23)

##Now doing Chi-square goodness of fit test

Oats1.Chi<-c(151, 36)

chisq.test(Oats1.Chi, p = c(.5, .5))

##Milo

Milo1 = subset(ms2, select = c("Milo"))
table(Milo1)

#Doing proportions

table24 <- table(Milo1)
prop.table(table24)

##Now doing Chi-square goodness of fit test

Milo1.Chi<-c(61, 126)

chisq.test(Milo1.Chi, p = c(.5, .5))

##Mealworms

Mealworms1 = subset(ms2, select = c("Mealworms"))
table(Mealworms1)

#Doing proportions

table25 <- table(Mealworms1)
prop.table(table25)

##Now doing Chi-square goodness of fit test

Mealworms1.Chi<-c(65, 120)

chisq.test(Mealworms1.Chi, p = c(.5, .5))

##Nyjer

Nyjer1 = subset(ms2, select = c("Nyjer"))
table(Nyjer1)

#Doing proportions

table26 <- table(Nyjer1)
prop.table(table26)

##Now doing Chi-square goodness of fit test

Nyjer1.Chi<-c(178, 9)

chisq.test(Nyjer1.Chi, p = c(.5, .5))


####Bivariate Chi-square tests of association between ecological variables
View(ms2)

ms3 <- droplevels(ms2)

ggbarstats(
  data = ms3,
  x = Seasonality,
  y = Feederwatch
) +
  labs(caption = NULL) # remove caption


ggbarstats(
  data = ms3,
  x = Seasonality,
  y = Habitat
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Seasonality,
  y = Primary_Food
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Seasonality,
  y = Niche
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Seasonality,
  y = Birds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Seasonality,
  y = Insects
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Seasonality,
  y = Non_feeder_seeds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Seasonality,
  y = Berries_Plants
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Feederwatch,
  y = Habitat
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Feederwatch,
  y = Primary_Food
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Feederwatch,
  y = Niche
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Feederwatch,
  y = Birds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Feederwatch,
  y = Insects
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Feederwatch,
  y = Non_feeder_seeds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Feederwatch,
  y = Berries_Plants
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Habitat,
  y = Primary_Food
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Habitat,
  y = Niche
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Habitat,
  y = Birds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Habitat,
  y = Insects
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Habitat,
  y = Non_feeder_seeds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Habitat,
  y = Berries_Plants
) +
  labs(caption = NULL) # remove caption


ggbarstats(
  data = ms3,
  x = Primary_Food,
  y = Niche
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Primary_Food,
  y = Birds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Primary_Food,
  y = Insects
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Primary_Food,
  y = Non_feeder_seeds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Primary_Food,
  y = Berries_Plants
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Birds,
  y = Insects
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Birds,
  y = Non_feeder_seeds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Birds,
  y = Berries_Plants
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Insects,
  y = Non_feeder_seeds
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Insects,
  y = Berries_Plants
) +
  labs(caption = NULL) # remove caption

ggbarstats(
  data = ms3,
  x = Non_feeder_seeds,
  y = Berries_Plants
) +
  labs(caption = NULL) # remove caption

#################################################################################################################
################################## Logistic Regression One -- Feederwatch Birds ##########################################################
#################################################################################################################
#https://quantifyinghealth.com/logistic-regression-in-r-with-categorical-variables/

##Hypothesis 1

View(ms3)

#Selecting the columns to be used

behave <- subset(ms3, select=c("Order", "Seasonality", "Feederwatch", "Habitat", "Primary_Food", "Niche", "Birds", "Insects", "Non_feeder_seeds", "Berries_Plants"))
str(behave)

#write.csv(behave,file='C:/Users/andre/OneDrive/Desktop/behave.csv', row.names=TRUE)

library(lmtest)
library(rms)
library(aod)
library(mlogit)
library(dfidx)
library(performance)
library(DescTools)

##Feederwatch is the dependent variable - testing the behavioral variables

model_1 = glm( Feederwatch ~ Order + Seasonality + Primary_Food + Habitat, family = 'binomial', data = behave)
summary(model_1)
r2(model_1)
model_performance(model_1)
PseudoR2(model_1, c("McFadden", "Nagel"))
sum(residuals(model_1, type = "deviance")^2)
r2_somers(model_1)
lrtest(model_1)

#Calculate p-value
fmod1 <- glm(Feederwatch ~ Order + Seasonality + Primary_Food + Habitat,family = "binomial", data = behave) ##"full" mod
nmod1 <- glm(Feederwatch ~ 1, family = 'binomial', data = behave) ##"null" mod
anova(nmod1, fmod1, test = 'Chisq')

model_2 = glm( Feederwatch ~ Order + Seasonality + Primary_Food, family = 'binomial', data = behave)
#glm.fit: algorithm did not converge 
summary(model_2)
r2(model_2)
model_performance(model_2)
PseudoR2(model_2, c("McFadden", "Nagel"))
sum(residuals(model_2, type = "deviance")^2)
r2_somers(model_2)
lrtest(model_2)

fmod2 <- glm(Feederwatch ~ Order + Seasonality + Primary_Food,family = "binomial", data = behave) ##"full" mod
nmod2 <- glm(Feederwatch ~ 1, family = 'binomial', data = behave) ##"null" mod
anova(nmod2, fmod2, test = 'Chisq')


model_3 = glm( Feederwatch ~ Order + Seasonality, family = 'binomial', data = behave)
summary(model_3)
r2(model_3)
model_performance(model_3)
PseudoR2(model_3, c("McFadden", "Nagel"))
sum(residuals(model_3, type = "deviance")^2)
lrtest(model_3)

fmod3 <- glm(Feederwatch ~ Order + Seasonality,family = "binomial", data = behave) ##"full" mod
nmod3 <- glm(Feederwatch ~ 1, family = 'binomial', data = behave) ##"null" mod
anova(nmod3, fmod3, test = 'Chisq')


model_4 = glm( Feederwatch ~ Seasonality, family = 'binomial', data = behave)
summary(model_4)
r2(model_4)
model_performance(model_4)
PseudoR2(model_4, c("McFadden", "Nagel"))
sum(residuals(model_4, type = "deviance")^2)
r2_somers(model_4)
lrtest(model_4)

fmod4 <- glm(Feederwatch ~ Seasonality,family = "binomial", data = behave) ##"full" mod
nmod4 <- glm(Feederwatch ~ 1, family = 'binomial', data = behave) ##"null" mod
anova(nmod4, fmod4, test = 'Chisq')


model_5 = glm( Feederwatch ~ Niche, family = 'binomial', data = behave)
summary(model_5)
r2(model_5)
model_performance(model_5)
PseudoR2(model_5, c("McFadden", "Nagel"))
sum(residuals(model_5, type = "deviance")^2)
r2_somers(model_5)
lrtest(model_5)

fmod5 <- glm(Feederwatch ~ Niche,family = "binomial", data = behave) ##"full" mod
nmod5 <- glm(Feederwatch ~ 1, family = 'binomial', data = behave) ##"null" mod
anova(nmod5, fmod5, test = 'Chisq')

model_6 = glm(Feederwatch ~ Niche + Seasonality, family = 'binomial', data = behave)
summary(model_6)
r2(model_6)
model_performance(model_6)
PseudoR2(model_6, c("McFadden", "Nagel"))
sum(residuals(model_6, type = "deviance")^2)
r2_somers(model_6)
lrtest(model_6)

fmod6 <- glm(Feederwatch ~ Niche + Seasonality,family = "binomial", data = behave) ##"full" mod
nmod6 <- glm(Feederwatch ~ 1, family = 'binomial', data = behave) ##"null" mod
anova(nmod6, fmod6, test = 'Chisq')


model_7 = glm(Feederwatch ~ Niche + Seasonality + Order, family = 'binomial', data = behave)
summary(model_7)
r2(model_7)
model_performance(model_7)
PseudoR2(model_7, c("McFadden", "Nagel"))
sum(residuals(model_7, type = "deviance")^2)
r2_somers(model_7)
lrtest(model_7)

fmod7 <- glm(Feederwatch ~ Niche + Seasonality + Order,family = "binomial", data = behave) ##"full" mod
nmod7 <- glm(Feederwatch ~ 1, family = 'binomial', data = behave) ##"null" mod
anova(nmod7, fmod7, test = 'Chisq')

compare_performance(model_1, model_2, model_3, model_4, model_5, model_6, model_7, verbose = FALSE)


##################################################################################################################
########################################## Logistic Regression 2 - Ground foraging ################################
##################################################################################################################

#Adding a column to behave so that ground foraging is represented as Yes, all others No

library(dplyr)
behave1 <- mutate(behave, GroundF = ifelse(Niche=="Ground", 'Yes', 'No'))

library(lmtest)
library(rms)
library(aod)
library(mlogit)
library(dfidx)
library(performance)
library(DescTools)

behave1$GroundF <- as.factor(behave1$GroundF)

##Ground foraging is the dependent variable - testing the behavioral variables

moddel_1 = glm(GroundF ~ Order + Seasonality + Primary_Food + Habitat, family = 'binomial', data = behave1)
summary(moddel_1)
r2(moddel_1)
model_performance(moddel_1)
PseudoR2(moddel_1, c("McFadden", "Nagel"))
sum(residuals(moddel_1, type = "deviance")^2)
r2_somers(moddel_1)
lrtest(moddel_1)

#Calculate p-value
fmodd1 <- glm(GroundF ~ Order + Seasonality + Primary_Food + Habitat,family = "binomial", data = behave1) ##"full" mod
nmodd1 <- glm(GroundF ~ 1, family = 'binomial', data = behave1) ##"null" mod
anova(nmodd1, fmodd1, test = 'Chisq')


moddel_2 = glm(GroundF ~ Order + Seasonality + Primary_Food, family = 'binomial', data = behave1)
#glm.fit: algorithm did not converge 
summary(moddel_2)
r2(moddel_2)
model_performance(moddel_2)
PseudoR2(moddel_2, c("McFadden", "Nagel"))
sum(residuals(moddel_2, type = "deviance")^2)
r2_somers(moddel_2)
lrtest(moddel_2)

fmodd2 <- glm(GroundF ~ Order + Seasonality + Primary_Food,family = "binomial", data = behave1) ##"full" mod
nmodd2 <- glm(GroundF ~ 1, family = 'binomial', data = behave1) ##"null" mod
anova(nmodd2, fmodd2, test = 'Chisq')

#Model 3

moddel_3 = glm(GroundF ~ Order + Seasonality, family = 'binomial', data = behave1)
summary(moddel_3)
r2(moddel_3)
r2_somers(moddel_3)
model_performance(moddel_3)
PseudoR2(moddel_3, c("McFadden", "Nagel"))
sum(residuals(moddel_3, type = "deviance")^2)
lrtest(moddel_3)

fmodd3 <- glm(GroundF ~ Order + Seasonality,family = "binomial", data = behave1) ##"full" mod
nmodd3 <- glm(GroundF ~ 1, family = 'binomial', data = behave1) ##"null" mod
anova(nmodd3, fmodd3, test = 'Chisq')

#Model 4


moddel_4 = glm(GroundF ~ Seasonality, family = 'binomial', data = behave1)
summary(moddel_4)
r2(moddel_4)
model_performance(moddel_4)
PseudoR2(moddel_4, c("McFadden", "Nagel"))
sum(residuals(moddel_4, type = "deviance")^2)
r2_somers(moddel_4)
lrtest(moddel_4)

fmodd4 <- glm(GroundF ~ Seasonality,family = "binomial", data = behave1) ##"full" mod
nmodd4 <- glm(GroundF ~ 1, family = 'binomial', data = behave1) ##"null" mod
anova(nmodd4, fmodd4, test = 'Chisq')

########################################################################################################################
##################################### Creating a map of the outbreak ###################################################
########################################################################################################################

#Creating a dataframe because the other is too cumbersome

place <- c(
        "connecticut", 
        "district of columbia", 
        "florida", 
        "maryland", 
        "nebraska", 
        "new hampshire", 
        "new jersey",
        "ohio",
        "pennsylvania",
        "virginia",
        "wisconsin")
mortality <- c(29, 29, 1, 92, 1, 6, 23, 6, 3, 2, 1) 

# Join the variables to create a data frame
st_mortality <- data.frame(place,mortality)
st_mortality

#https://stackoverflow.com/questions/24441775/how-do-you-create-a-us-states-heatmap-based-on-some-values

st_mortality$region <- tolower(st_mortality$place)
library(ggplot2)
library(maps)
states <- map_data("state")
map.df <- merge(states,st_mortality, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=mortality))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()


