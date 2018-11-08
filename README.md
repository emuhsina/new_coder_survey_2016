
# EDA of "New Coder Survey 2016"
[Free code camp](https://www.freecodecamp.org/) conducted a survey in 2016 to understand new coders. Over 15000 new coders respondended to 48 questions.

The dataset consists of 113 variables and over 15000 observations. 


```R
# Loading packages
library(tidyverse)
library(ggthemes);
```


```R
#Reading data
survey <- read_csv("2016-FCC-New-Coders-Survey-Data.csv");
```

    Parsed with column specification:
    cols(
      .default = col_integer(),
      BootcampName = col_character(),
      CityPopulation = col_character(),
      CodeEventBootcamp = col_character(),
      CodeEventDjangoGirls = col_character(),
      CodeEventGameJam = col_character(),
      CodeEventMeetup = col_character(),
      CodeEventOther = col_character(),
      CodeEventRailsGirls = col_character(),
      CodeEventWorkshop = col_character(),
      CountryCitizen = col_character(),
      CountryLive = col_character(),
      EmploymentField = col_character(),
      EmploymentFieldOther = col_character(),
      EmploymentStatus = col_character(),
      EmploymentStatusOther = col_character(),
      Gender = col_character(),
      ID.x = col_character(),
      ID.y = col_character(),
      JobApplyWhen = col_character(),
      JobPref = col_character()
      # ... with 17 more columns
    )
    See spec(...) for full column specifications.


## Gender and Age Distribution
Looking at the dataset, which sex is mostly participated in the online survey?


```R
# Setting the size of plots in this notebook
options(repr.plot.width=6, repr.plot.height=3)

#Gender Distribution
survey %>%
  filter(!is.na(Gender)) %>%
  group_by(Gender) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  ggplot(aes(x= reorder(Gender, count), y = count, fill = Gender)) + 
  geom_bar(stat = "identity") + ggtitle("Participation in the Survey") + geom_text(aes(label = round(pct, digits= 2)))+ xlab("")  + coord_flip()
```




![png](output_4_1.png)



```R
#Age distribution
survey %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x= Age)) + geom_histogram(binwidth = 2)
```




![png](output_5_1.png)


     Age       
 Min.   :10.00  
 1st Qu.:23.00  
 Median :27.00  
 Mean   :29.18  
 3rd Qu.:33.00  
 Max.   :86.00  

78% of the respondents are men and 21% are women. The median age is 27. The youngest new coder is 10 years old and the oldest one is 86 years.

## Which role are you most interested in?


```R
#Job roles interested
survey %>%
  filter(!is.na(JobRoleInterest)) %>%
  group_by(JobRoleInterest) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  ggplot(aes(x= reorder(JobRoleInterest, count), y = count)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + xlab("") + coord_flip()
```




![png](output_9_1.png)


40% of the new coders answered "*Full-stack web developer" to the question.

# Data Scientist/Data Engineer
As I am interested in **Data Science** let's dive deeper into Data Scientist/Data Engineer Subset. 10% of the total respondents (ie, 646 respondents) interested in *Data Science/Data Engineering*. 

[1] 646 113


```R
#Data Science - Gender distribution 
survey %>%
  filter(!is.na(Gender),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  group_by(Gender) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  ggplot(aes(x= reorder(Gender, count), y = count, fill = Gender)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + xlab("") + coord_flip()
```




![png](output_13_1.png)



```R
#Age distribution of respondents interested in Data Science/ Data Engineer
survey %>%
  filter(!is.na(Age),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  ggplot(aes(x= Age)) + geom_histogram(binwidth = 1)
```




![png](output_14_1.png)


      Age       
 Min.   :14.00  
 1st Qu.:22.00  
 Median :26.00  
 Mean   :27.72  
 3rd Qu.:31.25  
 Max.   :65.00 

Of the 646 Data scientists/ Data Engineers who responded to the survey **25% are women**. 21% of new coders in general are women. Their median age is 26. The youngest and oldest among them are 14 and 65 years respectively. It is clear from the plot that the age distribution is positively skewed. 

## How many months have you been programming for?


```R
#How many years have you been programming for?
survey %>%
  filter(!is.na(MonthsProgramming),
         MonthsProgramming < 300, 
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  ggplot(aes(x= MonthsProgramming)) + geom_histogram(binwidth = 10)
```




![png](output_18_1.png)



```R
survey %>%
  filter(!is.na(MonthsProgramming),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  select(MonthsProgramming) %>%
  summary()
```


     MonthsProgramming
     Min.   :  0.00   
     1st Qu.:  3.00   
     Median :  8.00   
     Mean   : 16.17   
     3rd Qu.: 20.00   
     Max.   :360.00   


 The median programming experience is 8 months and they started programming an average of 16 months ago.


```R
#Hours Spend learning each week
survey %>%
  filter(!is.na(HoursLearning),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  ggplot(aes(x= HoursLearning)) + geom_histogram(binwidth = 5)
```




![png](output_21_1.png)


 HoursLearning  
 Min.   : 0.00  
 1st Qu.: 5.00  
 Median :10.00  
 Mean   :14.41  
 3rd Qu.:20.00  
 Max.   :80.00  

They spend an average of 14 hours per week for learning. 


```R
#Job Preference
survey %>%
  filter(!is.na(JobPref),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  group_by(JobPref) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  ggplot(aes(x= reorder(JobPref, count), y = count)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + coord_flip()

```




![png](output_24_1.png)


Almost half of the developing Data scientists or engineers who responded to the survey prefer to work for a medium sized company. None of them are interested to start their on company or prefer freelance work.


```R
#Job where pref
survey %>%
  filter(!is.na(JobWherePref),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  group_by(JobWherePref) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  ggplot(aes(x= reorder(JobWherePref, count), y = count)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + coord_flip() + scale_fill_manual(values = brewer.pal(3, "Blues"))
```




![png](output_26_1.png)


Most of them wants to work in an office instead of working remotely.


```R
#Relocate
survey %>%
  filter(!is.na(JobRelocateYesNo),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  group_by(JobRelocateYesNo) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  ggplot(aes(x= reorder(JobRelocateYesNo, count), y = count)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + coord_flip() + scale_fill_brewer()
```




![png](output_28_1.png)


### Majority are willing to relocate for job. 


```R
#Which country are you a citizen of?
survey %>%
  filter(!is.na(CountryCitizen),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  group_by(CountryCitizen) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ggplot(aes(x= reorder(CountryCitizen, count), y = count)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + coord_flip()
```




![png](output_30_1.png)


Most of the respondents are from USA. 


```R
survey %>%
  filter(!is.na(LanguageAtHome),
          JobRoleInterest == "Data Scientist / Data Engineer") %>%
  group_by(LanguageAtHome) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ggplot(aes(x= reorder(LanguageAtHome, count), y = count)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + coord_flip()
```




![png](output_32_1.png)


Half of them are native English speakers.


```R
survey %>%
  filter(!is.na(SchoolDegree),
          JobRoleInterest == "Data Scientist / Data Engineer") %>%
  group_by(SchoolDegree) %>%
  summarize(count = n()) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ggplot(aes(x= reorder(SchoolDegree, count), y = count)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + coord_flip()
```




![png](output_34_1.png)


More than half of them have atleast bachelor's degree.


```R
otherFieldDs <- data.frame("Other", 681)
names(otherFieldDs) <- c("EmploymentStatus", "count")
survey %>%
  filter(!is.na(EmploymentStatus),
        JobRoleInterest == "Data Scientist / Data Engineer") %>%
  group_by(EmploymentStatus) %>%
  summarize(count = n()) %>%
  rbind(otherFieldDs) %>%
  mutate(pct = count/ sum(count) * 100 ) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ggplot(aes(x= reorder(EmploymentStatus, count), y = count)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(pct, digits= 2))) + coord_flip()
```




![png](output_36_1.png)


More than half of them are currently working.
