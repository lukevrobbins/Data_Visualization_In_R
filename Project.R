library(dplyr)
library(ggplot2)
library(stringr)
library(maps)
library(sf)
library(usmap)
library(maptools)
library(rgdal)

p_data <- read.csv("police_fatalities.csv")

table(p_data$Subject.s.age)
table(p_data$Cause.of.death)

str(p_data)

############### Data cleaning ######
#replace subject.age missing values with NA, change ages less than 15 to NA
#create gender variable and assign subject.race with imputation values and change it as factor
#filter data to cause of death by: Gunshot, Stabbed, chemical agent/pepper spay, Tasered, Asphyxiated/Restrained,
#Beaten/Bludgeoned with instrument, vehicle


#Removing unused columns
drop <- c("URL.of.image.of.deceased", 
          "A.brief.description.of.the.circumstances.surrounding.the.death", 
          "Dispositions.Exclusions.INTERNAL.USE..NOT.FOR.ANALYSIS",
          "Supporting.document.link",
          "Link.to.news.article.or.photo.of.official.document",
          "Unique.ID.formula",
          "Unique.identifier..redundant.",
          "Video",
          "Date.Description")

p_data <- p_data[, !names(p_data) %in% drop]



cause_list <- c("Gunshot","Pursuit","Asphyxiated/Restrained","Beaten/Bludgeoned with instrument","Vehicle",
                "Chemical agent/Pepper spray","Gunshot","Stabbed")

myData <- filter(p_data, p_data$Cause.of.death %in% cause_list)

#Changing empty cells to NA
na_if(myData, " ")



#change values
myData <- myData %>% mutate(Cause.of.death = replace(Cause.of.death, which(Cause.of.death == "Beaten/Bludgeoned with instrument"), "Beaten")) 
myData <- myData %>% mutate(Cause.of.death = replace(Cause.of.death, which(Cause.of.death == "Vehicle"), "Pursuit")) 
myData <- myData %>% mutate(Cause.of.death = replace(Cause.of.death, which(Cause.of.death == "Asphyxiated/Restrained"), "Asphyxiated")) 
                  

#rename columns
myData = dplyr::rename(myData, death_cause = Cause.of.death)
myData = dplyr::rename(myData, age = Subject.s.age)
myData = dplyr::rename(myData, race = Subject.s.race.with.imputations)
myData = dplyr::rename(myData, date_of_death = Date.of.injury.resulting.in.death..month.day.year.)
myData = dplyr::rename(myData, city = Location.of.death..city.)
myData = dplyr::rename(myData, county = Location.of.death..county.)
myData = dplyr::rename(myData, state = Location.of.death..state.)
myData = dplyr::rename(myData, zip = Location.of.death..zip.code.)
myData = dplyr::rename(myData, street = Location.of.injury..address.)
myData = dplyr::rename(myData, gender = Subject.s.gender)
myData = dplyr::rename(myData, name = Subject.s.name)
myData = dplyr::rename(myData, year = Date..Year.)
myData = dplyr::rename(myData, agency = Agency.responsible.for.death)
myData = dplyr::rename(myData, lat = Latitude)
myData = dplyr::rename(myData, long = Longitude)

# cleaning values for race

myData <- myData %>% mutate(race = replace(race, which(race %in% c("HIspanic/Latino","Hispanic/Latino") ), "Hispanic"))
myData <- myData %>% mutate(race = replace(race, which(race %in% c("Race unspecified",NA) ), "Unknown"))
myData <- myData %>% mutate(race = replace(race, which(race %in% c("Native American/Alaskan","European-American/White") ), "White"))
myData <- myData %>% mutate(race = replace(race, which(race %in% c("Other Race","Middle Eastern") ), "Other"))
myData <- myData %>% mutate(race = replace(race, which(race == "Asian/Pacific Islander" ), "Asian"))
myData <- myData %>% mutate(race = replace(race, which(race == "African-American/Black" ), "Black"))

#change race as factor
myData$race = factor(myData$race, levels = c("White","Black","Hispanic","Asian","Other","Unknown"))

#cleaning gender values
myData <- myData %>% mutate(gender = replace(gender, which(gender == "" ), "Unknown"))
myData$gender = factor(myData$gender, levels = c("Male","Female","Transgender","Unknown"))

#categorize agency 
myData <- myData %>% mutate(agency = replace(agency, str_detect(agency, "Sheriff"), "Sheriff Office")) %>% 
    mutate(agency = replace(agency, str_detect(agency, "Police"), "Police Department")) %>% 
    mutate(agency = replace(agency, str_detect(agency, "Federal Bureau"), "FBI")) 

main_agency <- c("Sheriff Office","Police Department","FBI")
#Create a not in function
`%!in%` = Negate(`%in%`)
# change remaining agencies to Others
myData <- myData %>% mutate(agency = replace(agency, which(agency %!in% main_agency), "Others")) 

#change agency as factor
myData$agency = factor(myData$agency, levels = c("Sheriff Office","Police Department","FBI","Others"))

#cleaning age

myData$age = substr(myData$age,1,2)
myData$age = as.numeric(myData$age)
myData <- myData %>% mutate(age = replace(age, which(age < 10 ), NA))

#Bucket ages in groups of 10s
myData$age_grps <- cut(myData$age, breaks=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), right = FALSE)

#Create variable for year of death
myData$year_of_death = as.numeric(substr(myData$date_of_death,7,length(myData$date_of_death)))



#Final data set
df <- select(myData,name, age,age_grps,race,gender,street,city,state,zip,county,date_of_death,death_cause,year_of_death,agency)
str(myData)
str(df)



############### Visualization code ######
# Has Police brutality been increasing over the years.(Used in presentation)
ggplot(data = subset(myData, !is.na(year)), aes(x= as.factor(year)))+
  geom_bar(aes(fill = race)) +
  scale_fill_manual(values = c("#FFDB6D", "#C4961A", "#293352", 
                               "#D16103", "#C3D7A4", "#52854C", "#4E84C4"))+
  geom_text(stat = 'count',aes(label =..count.., hjust = 1.2), color = "#1A6D0F")+
  labs(title = "Brutality Distribution From 2000 to 2020",
       y="Number of Deaths", x = "") + coord_flip()

# Age Distribution bar chart
ggplot(data = subset(df, !is.na(age_grps)))+
  geom_histogram(aes(age_grps), stat="count", fill = "#1883C8") +
  labs(title = "Number of Deaths by Age Group", x = "Age Groups by 10 Years", y = "")

# Age box plot per year
ggplot(data = subset(myData, !is.na(year))) +
  geom_boxplot(aes(x = as.factor(year), y = age), fill = "#1883C8") +
  labs(title = "Age Concentration by Year", x = "Year", y = "Age")

# State fatality bar graph descending
F <- data.frame(table(df$state))
ggplot(F, aes(x= reorder(Var1, Freq),Freq , fill=Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "States by Death Count", x="") + 
  coord_flip() +
  guides(fill = guide_legend(title = "States"))

# Top 30 state bar graph descending
x = top_n(data.frame(table(df$city)),30)
ggplot(x , aes(x= reorder(Var1, Freq), y=Freq, fill=Var1))+
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() +
  labs(title = "Top 30 Cities by Death Count", x = "", y = "") +
  guides(fill = guide_legend(title="Cities"))

# Deaths by agencies
x = top_n(data.frame(table(df$agency)),20)
ggplot(x , aes(x= reorder(Var1, Freq), y=Freq, fill=Var1)) +
  geom_bar(stat = "identity", position = 'dodge') + 
  coord_flip() + 
  guides(fill = guide_legend(title = "Agency"))

 
# Number of death by race
df_death <- df
df_death <- na.omit(na_if(df_death, "Unknown"))
ggplot(df_death, aes(x = race)) + 
  geom_bar(aes(fill = gender)) +
  labs(x = "Race", y = "Number of Deaths", 
       title = "Number of Deaths by Race and Gender", 
       caption = "Race only used if imputation confidence was over 80%") +
  geom_text(stat = 'count',aes(label =..count.., vjust = -.6), color = "#1A6D0F") +
  guides(fill = guide_legend(title = "Gender"))

# White deaths as percent of population is .00005146
# Black deaths as percent of population is .0001589
# Hispanic deaths as percent of population is .00007017
# Asian deaths as percent of population is .00002628


# Creating table of state freq for death
state_count <- as.data.frame(table(myData$state))
state_count <- dplyr::rename(state_count, state = Var1)

# Plotting with state_count df
plot_usmap(data = state_count, values = "Freq", labels = TRUE, label_color = "black", color = "black") + 
  scale_fill_continuous(low = "white", high = "red", name = "Number of Deaths", label = scales::comma) + 
  theme(legend.position = "left") +
  labs(title = "Number of Deaths from Police by State 2000-2020", subtitle = "Total number of deaths per state, not adjusted")

# Creating bar chart number of deaths in top 10 cities
city_count <- as.data.frame(table(myData$city))
city_count <- dplyr::rename(city_count, city = Var1)
city_count <- arrange(city_count, -Freq)
city_count <- city_count[1:10, ]

ggplot(data = city_count) +
  geom_bar(aes(x = Freq, y = reorder(city, Freq)), stat = "identity", fill = "#F15533", color = "black") +
  labs(title = "Top 10 Cities With Police Brutality Death", 
       subtitle = "From 2000 to 2020", x = "Number of Deaths", y = "Cities") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
# Creating table of state freq for death
state_count <- as.data.frame(table(myData$state))
state_count <- dplyr::rename(state_count, state = Var1)

#death map 
us_map <- map_data("state")
region_key <- data.frame(state.abb, state.name)

#Rename columns
region_key = dplyr::rename(region_key, state = state.abb)
region_key = dplyr::rename(region_key, region = state.name)
region_key$region <- tolower(region_key$region)



# Age by gendar and race boxplot
ggplot(df, aes(x=race, y=age, col= race))+
  geom_boxplot()  + 
  facet_wrap(~gender, ncol = 2 )+
  labs(title = "Age Distribution by race & gender")

# Density plot for gender of victims by year
ggplot(df, aes(x=year_of_death, color=race)) + 
  geom_density(alpha=0.5)+ 
  labs(title = "Density Plot of Gender victims by Year")+
  facet_wrap(~gender, ncol = 2 )

#Age Distribution by Year and Cause of Death
ggplot(df, aes(x= year_of_death, y=age, col= death_cause)) + 
  geom_point() + facet_wrap(~gender, ncol = 2) + 
  labs(x = "Year", y = "Age", 
       col = "Cause of Death", 
       title = "Age Distribution by Year and Cause of Death")

#Correlation of Agency and Cause of Death regarding race
ggplot(df, aes(x= agency, fill=death_cause)) + 
  geom_bar(position = 'dodge') + 
  labs(title = "Agency and Cause of Death Barplot", 
       x = "Agency", 
       y = "Number of Deaths", 
       fill = "Cause of Death") + 
  facet_wrap(~race, ncol = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Correlation of Agency and Gender regarding race
ggplot(df, aes(x = agency, fill = gender)) + 
  geom_bar(position = 'dodge') + 
  labs(title = "Agency and Gender Barplot", 
       x = "Agency", y = "Number of Deaths", fill = "Gender") + 
  facet_wrap(~race, ncol = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Correlation of Agency and Cage regarding race
ggplot(df, aes(x = agency, fill = age_grps)) + 
  geom_bar(position = 'dodge') + 
  labs(title = "Agency and Age Barplot", 
       x = "Agency", y = "Number of Deaths", 
       fill = "Age") + 
  facet_wrap(~race, ncol = 2) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



