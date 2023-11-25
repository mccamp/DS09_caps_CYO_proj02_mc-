#DS09_project2_v.2 - Copy_14.11.23 - Copy....R

#project contents:

#Index-Introduction:

#Index:
  #1. The subject of study;
  #2. Measuring effects.
  #3. Data available.
  #4. Initial solution.
  #5. Solution.
  #6. Conclusions - impact.

#Introduction:
#1. The subject of study.
#In this project we´ll study evolution of salaries in the USA through the last 
#40 years.
#In particular we'll be interested in analyzing and describing relationships 
#between education and demographic variables and hourly pay rates.

#2. Measuring effects. How do these variables impact pay rates, how do the 
#potential effects of these variables
#evolve through time? Are there significant differences based on such variables?

#3. DATA. We'll use the data.set  "wages-by-education-in-the-usa-1973-2022" 
#from Kaggle. Using Data Wrangling and visualization, we will explore the data
#assess our first perceptions and define our working hypothesis. 

#4. In order to understand the test our working hypothesis, we'll utilize 
#least square estimates/linear regression techniques. 

#5. Further to this, we'll implement a model which will take into account the
#accumulative effects of variables on outcomes, 


#6. Finally we'll draw some conclusions, impact of the findings (effect of 
#variables) on the subject of study (pay- outcome).



#Parts 1-2 define the focus of the study, as described above.
#Part 3 - DATA.

#Loading packages:
library(tidyverse)

library(dslabs)
library(dplyr)
library(ggplot2)
library(caret)
library(lubridate)
library(readr)


## step 1_ Read the input dataset
#read from web source:

#wages_edu <-read.csv("/kaggle/input/wages-by-education-in-the-usa-1973-2022/wages_by_education.csv")
#wages_edu <-read.csv ("https://www.kaggle.com/datasets/asaniczka/wages-by-education-in-the-usa-1973-2022/wages_by_education.csv")

#kaggle datasets download -d asaniczka/wages-by-education-in-the-usa-1973-2022

#Note "kaggle" might ask for signing-in. Once signed in, click on the download 
#button to recover the data set. The data set can then be stored in the computer, 
#and be read into R from a local folder:

options(timeout = 120)
wages_edu <- read.csv("C:\\Users\\marc.camprodon\\Documents\\DS_09_Capstone\\Datasets for project 2\\PROJECT2_v2_DATA_MODEL\\wages_by_education.csv", header=TRUE, stringsAsFactors=FALSE)
wages_edu


#Reviewing DATA

#List of unique columns
col_names_edu <- colnames(wages_edu)
col_names_edu
    
    #>     col_names_edu
    #[1] "year"                            "less_than_hs"                    "high_school"                     "some_college"                   
    #[5] "bachelors_degree"                "advanced_degree"                 "men_less_than_hs"                "men_high_school"                
    #[9] "men_some_college"                "men_bachelors_degree"            "men_advanced_degree"             "women_less_than_hs"             
    #[13] "women_high_school"               "women_some_college"              "women_bachelors_degree"          "women_advanced_degree"          
    #[17] "white_less_than_hs"              "white_high_school"               "white_some_college"              "white_bachelors_degree"         
    #[21] "white_advanced_degree"           "black_less_than_hs"              "black_high_school"               "black_some_college"             
    #[25] "black_bachelors_degree"          "black_advanced_degree"           "hispanic_less_than_hs"           "hispanic_high_school"           
    #[29] "hispanic_some_college"           "hispanic_bachelors_degree"       "hispanic_advanced_degree"        "white_men_less_than_hs"         
    #[33] "white_men_high_school"           "white_men_some_college"          "white_men_bachelors_degree"      "white_men_advanced_degree"      
    #[37] "black_men_less_than_hs"          "black_men_high_school"           "black_men_some_college"          "black_men_bachelors_degree"     
    #[41] "black_men_advanced_degree"       "hispanic_men_less_than_hs"       "hispanic_men_high_school"        "hispanic_men_some_college"      
    #[45] "hispanic_men_bachelors_degree"   "hispanic_men_advanced_degree"    "white_women_less_than_hs"        "white_women_high_school"        
    #[49] "white_women_some_college"        "white_women_bachelors_degree"    "white_women_advanced_degree"     "black_women_less_than_hs"       
    #[53] "black_women_high_school"         "black_women_some_college"        "black_women_bachelors_degree"    "black_women_advanced_degree"    
    #[57] "hispanic_women_less_than_hs"     "hispanic_women_high_school"      "hispanic_women_some_college"     "hispanic_women_bachelors_degree"
    #[61] "hispanic_women_advanced_degree" 
   
#Viewing the first part of the data.frame:
head(wages_edu)

#Displaying the internal strcuture of the data set:
str(wages_edu)
    #[1] 'data.frame':	50 obs. of  61 variables
    #We'd like to have a more manageable data frame.We want to transform it 
    #to Tidy Data. 
    #Tidy format in which each row represents one observation and columns 
    #represent (combinations of) the different variables available for each of 
    #these observations. 
    #We could consolidate the information reshaping the data: we could organize 
    #data in 5 columns, 4 selection variables, Education level, Race, Gender, 
    #Year, and the result of the observation 
    #or fifth variable Pay (value USD per h). 
#We will iniciate the process of DATA WRANGLING, converting the data set to 
#tidy form. 
#We can observe that data can be organized in 12 groups: 
#-All, All-Men, All-Women, All-Black,All-White,All-Hispanic;
#-Men-Black, Women-Black, Men-White, Women-White, Men-Hispanic, Women-Hispanic.
#All of these groups have 5 possible values for the education level. 
#We'll generate following these 12 groups as data.frames from "wages_edu", all 
#with a Tidy format:  
    
#6B.1_wages_edu_all => year=all ; gen=all ; race=all ; level_edu=all five ; 
#pay=values

    wages_edu_all <- wages_edu %>% select(year,less_than_hs,high_school,some_college,bachelors_degree,advanced_degree)
    wages_edu_all
    
    tidy_wages_edu_all <- gather(wages_edu_all, edu_level, pay, `less_than_hs`:`advanced_degree`)
    tidy_wages_edu_all
    tidy_wages_edu_all <-tidy_wages_edu_all %>% mutate(gender="All", race="All") %>% mutate(edu_5=c(1:250))
    #we'll prepare these 12 data.frames from the main source, and work on transform them to tidy data.
    #In the process, in order to improve and complete tidiness of data, we'll focus on the edu_level variable.
    #this has 55 different values, while there are actually only 5 different levels 
    #of education being considered. "edu_level" includes gender and race variables within in the original data frame structure.
    #we'll reduce that complexity to 5 actual eductaion level values (hereby refered as "a","b","c","d","e"), 
    #from the maximum education level (advanced degree), now level "a", to the minimum education level, 
    #"less than hs", hereby level "e":
    tidy_wages_edu_all$edu_5[1:50]="e"
    tidy_wages_edu_all$edu_5[51:100]="d"
    tidy_wages_edu_all$edu_5[101:150]="c"
    tidy_wages_edu_all$edu_5[151:200]="b"
    tidy_wages_edu_all$edu_5[201:250]="a"
    
    tidy_wages_edu_all
    str(tidy_wages_edu_all)
    df6B1<-tidy_wages_edu_all
    df6B1
    str(df6B1)
    qplot(year,pay,data=df6B1)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, races, genres")
  
    #6B.2_wages_edu_Men => year=all ; gen=Men ; race=all ; level_edu=all five ; pay=values
    wages_edu_Men <- wages_edu %>% select(year,men_less_than_hs,men_high_school,men_some_college,men_bachelors_degree,men_advanced_degree)
    wages_edu_Men
    
    tidy_wages_edu_Men <- gather(wages_edu_Men, edu_level, pay, `men_less_than_hs`:`men_advanced_degree`)
    tidy_wages_edu_Men
    tidy_wages_edu_Men <-tidy_wages_edu_Men %>% mutate(gender="Men", race="All")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_Men
    str(tidy_wages_edu_Men)
    
    tidy_wages_edu_Men$edu_5[1:50]="e"
    tidy_wages_edu_Men$edu_5[51:100]="d"
    tidy_wages_edu_Men$edu_5[101:150]="c"
    tidy_wages_edu_Men$edu_5[151:200]="b"
    tidy_wages_edu_Men$edu_5[201:250]="a"
    
    df6B2<-tidy_wages_edu_Men
    df6B2
    
    qplot(year,pay,data=df6B2)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, races, Men")
    
    #6B.3_wages_edu_Women => year=all ; gen=Women ; race=all ; level_edu=all five ; pay=values
    wages_edu_Women <- wages_edu %>% select(year,women_less_than_hs,women_high_school,women_some_college,women_bachelors_degree,women_advanced_degree)
    wages_edu_Women
    
    tidy_wages_edu_Women <- gather(wages_edu_Women, edu_level, pay, `women_less_than_hs`:`women_advanced_degree`)
    tidy_wages_edu_Women
    tidy_wages_edu_Women <-tidy_wages_edu_Women %>% mutate(gender="Women", race="All")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_Women
    str(tidy_wages_edu_Women)
    
    tidy_wages_edu_Women$edu_5[1:50]="e"
    tidy_wages_edu_Women$edu_5[51:100]="d"
    tidy_wages_edu_Women$edu_5[101:150]="c"
    tidy_wages_edu_Women$edu_5[151:200]="b"
    tidy_wages_edu_Women$edu_5[201:250]="a"
    
    tidy_wages_edu_Women
    df6B3<-tidy_wages_edu_Women
    
    qplot(year,pay,data=df6B3)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, races, Women")
    
    #6B.4_wages_edu_B => year=all ; gen=all ; race=Black ; level_edu=all five ; pay=values
    wages_edu_B <- wages_edu %>% select(year,black_less_than_hs,black_high_school,black_some_college,black_bachelors_degree,black_advanced_degree)
    wages_edu_B
    
    tidy_wages_edu_B <- gather(wages_edu_B, edu_level, pay, `black_less_than_hs`:`black_advanced_degree`)
    tidy_wages_edu_B
    tidy_wages_edu_B <-tidy_wages_edu_B %>% mutate(gender="All", race="Black")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_B
    str(tidy_wages_edu_B)
    
    tidy_wages_edu_B$edu_5[1:50]="e"
    tidy_wages_edu_B$edu_5[51:100]="d"
    tidy_wages_edu_B$edu_5[101:150]="c"
    tidy_wages_edu_B$edu_5[151:200]="b"
    tidy_wages_edu_B$edu_5[201:250]="a"
    
    tidy_wages_edu_B
    df6B4<-tidy_wages_edu_B
    df6B4
    
    qplot(year,pay,data=df6B4)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, genres, Black")
    
    #6B.5_wages_edu_W => year=all ; gen=all ; race=White ; level_edu=all five ; pay=values
    wages_edu_W <- wages_edu %>% select(year,white_less_than_hs,white_high_school,white_some_college,white_bachelors_degree,white_advanced_degree)
    wages_edu_W
    
    tidy_wages_edu_W <- gather(wages_edu_W, edu_level, pay, `white_less_than_hs`:`white_advanced_degree`)
    tidy_wages_edu_W
    tidy_wages_edu_W <-tidy_wages_edu_W %>% mutate(gender="All", race="White")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_W
    str(tidy_wages_edu_W)
    
    tidy_wages_edu_W$edu_5[1:50]="e"
    tidy_wages_edu_W$edu_5[51:100]="d"
    tidy_wages_edu_W$edu_5[101:150]="c"
    tidy_wages_edu_W$edu_5[151:200]="b"
    tidy_wages_edu_W$edu_5[201:250]="a"
    
    tidy_wages_edu_W
    df6B5<-tidy_wages_edu_W
    
    qplot(year,pay,data=df6B5)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, genres, White")
    
    #6B.6_wages_edu_H => year=all ; gen=all ; race=Hispanic ; level_edu=all five ; pay=values
    wages_edu_H <- wages_edu %>% select(year,hispanic_less_than_hs,hispanic_high_school,hispanic_some_college,hispanic_bachelors_degree,hispanic_advanced_degree)
    wages_edu_H
    
    tidy_wages_edu_H <- gather(wages_edu_H, edu_level, pay, `hispanic_less_than_hs`:`hispanic_advanced_degree`)
    tidy_wages_edu_H
    tidy_wages_edu_H <-tidy_wages_edu_H %>% mutate(gender="All", race="Hispanic")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_H
    str(tidy_wages_edu_H)
    
    tidy_wages_edu_H$edu_5[1:50]="e"
    tidy_wages_edu_H$edu_5[51:100]="d"
    tidy_wages_edu_H$edu_5[101:150]="c"
    tidy_wages_edu_H$edu_5[151:200]="b"
    tidy_wages_edu_H$edu_5[201:250]="a"
    
    tidy_wages_edu_H
  
    df6B6<-tidy_wages_edu_H
    
    qplot(year,pay,data=df6B6)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, genres, Hispanic")
    
    
    #6A.1_wages_edu_M_B => year=all ; gen=Men ; race=Black ; level_edu=all five ; pay=values
    wages_edu_M_B <- wages_edu %>% select(year,black_men_less_than_hs,black_men_high_school,black_men_some_college,black_men_bachelors_degree,black_men_advanced_degree)
    wages_edu_M_B
    
    tidy_wages_edu_M_B <- gather(wages_edu_M_B, edu_level, pay, `black_men_less_than_hs`:`black_men_advanced_degree`)
    tidy_wages_edu_M_B
    tidy_wages_edu_M_B <-tidy_wages_edu_M_B %>% mutate(gender="Men", race="Black")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_M_B
    str(tidy_wages_edu_M_B)
    
    tidy_wages_edu_M_B$edu_5[1:50]="e"
    tidy_wages_edu_M_B$edu_5[51:100]="d"
    tidy_wages_edu_M_B$edu_5[101:150]="c"
    tidy_wages_edu_M_B$edu_5[151:200]="b"
    tidy_wages_edu_M_B$edu_5[201:250]="a"
    
    tidy_wages_edu_M_B
  
    df6A1<-tidy_wages_edu_M_B
    
    qplot(year,pay,data=df6A1)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, Men, Black")
  
    #6A.2_wages_edu_M_W => year=all ; gen=Men ; race=White ; level_edu=all five ; pay=values
    wages_edu_M_W <- wages_edu %>% select(year,white_men_less_than_hs,white_men_high_school,white_men_some_college,white_men_bachelors_degree,white_men_advanced_degree)
    wages_edu_M_W
    tidy_wages_edu_M_W <- gather(wages_edu_M_W, edu_level, pay, `white_men_less_than_hs`:`white_men_advanced_degree`)
    tidy_wages_edu_M_W
    tidy_wages_edu_M_W <-tidy_wages_edu_M_W %>% mutate(gender="Men", race="White")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_M_W
    class(tidy_wages_edu_M_W)
    str(tidy_wages_edu_M_W)
    
    tidy_wages_edu_M_W$edu_5[1:50]="e"
    tidy_wages_edu_M_W$edu_5[51:100]="d"
    tidy_wages_edu_M_W$edu_5[101:150]="c"
    tidy_wages_edu_M_W$edu_5[151:200]="b"
    tidy_wages_edu_M_W$edu_5[201:250]="a"
    
    tidy_wages_edu_M_W
   
    df6A2<-tidy_wages_edu_M_W
    
    qplot(year,pay,data=df6A2)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, Men, White")
    
    #6A.3_wages_edu_M_H => year=all ; gen=Men ; race=Hispanic ; level_edu=all five ; pay=values
    wages_edu_M_H <- wages_edu %>% select(year,hispanic_men_less_than_hs,hispanic_men_high_school,hispanic_men_some_college,hispanic_men_bachelors_degree,hispanic_men_advanced_degree)
    wages_edu_M_H
    tidy_wages_edu_M_H <- gather(wages_edu_M_H, edu_level, pay, `hispanic_men_less_than_hs`:`hispanic_men_advanced_degree`)
    tidy_wages_edu_M_H
    tidy_wages_edu_M_H <-tidy_wages_edu_M_H %>% mutate(gender="Men", race="Hispanic")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_M_H
    str(tidy_wages_edu_M_H)
    
    tidy_wages_edu_M_H$edu_5[1:50]="e"
    tidy_wages_edu_M_H$edu_5[51:100]="d"
    tidy_wages_edu_M_H$edu_5[101:150]="c"
    tidy_wages_edu_M_H$edu_5[151:200]="b"
    tidy_wages_edu_M_H$edu_5[201:250]="a"
    
    tidy_wages_edu_M_H
   
    df6A3<-tidy_wages_edu_M_H
    
    qplot(year,pay,data=df6A3)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, Men, Hispanic")
    
    #6A.4_wages_edu_W_B => year=all ; gen=Women ; race=Black ; level_edu=all five ; pay=values
    wages_edu_W_B <- wages_edu %>% select(year,black_women_less_than_hs,black_women_high_school,black_women_some_college,black_women_bachelors_degree,black_women_advanced_degree)
    wages_edu_W_B
    
    tidy_wages_edu_W_B <- gather(wages_edu_W_B, edu_level, pay, `black_women_less_than_hs`:`black_women_advanced_degree`)
    tidy_wages_edu_W_B
    tidy_wages_edu_W_B <-tidy_wages_edu_M_B %>% mutate(gender="Women", race="Black")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_W_B
    str(tidy_wages_edu_W_B)
    
    tidy_wages_edu_W_B$edu_5[1:50]="e"
    tidy_wages_edu_W_B$edu_5[51:100]="d"
    tidy_wages_edu_W_B$edu_5[101:150]="c"
    tidy_wages_edu_W_B$edu_5[151:200]="b"
    tidy_wages_edu_W_B$edu_5[201:250]="a"
    
    tidy_wages_edu_W_B
    
    df6A4<-tidy_wages_edu_W_B
    
    qplot(year,pay,data=df6A4)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, Women, Black")
    
    #6A.5_wages_edu_W_W => year=all ; gen=Women ; race=White ; level_edu=all five ; pay=values
    wages_edu_W_W <- wages_edu %>% select(year,white_women_less_than_hs,white_women_high_school,white_women_some_college,white_women_bachelors_degree,white_women_advanced_degree)
    wages_edu_W_W
    tidy_wages_edu_W_W <- gather(wages_edu_W_W, edu_level, pay, `white_women_less_than_hs`:`white_women_advanced_degree`)
    tidy_wages_edu_W_W
    tidy_wages_edu_W_W <-tidy_wages_edu_W_W %>% mutate(gender="Women", race="White")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_W_W
    str(tidy_wages_edu_W_W)
    
    tidy_wages_edu_W_W$edu_5[1:50]="e"
    tidy_wages_edu_W_W$edu_5[51:100]="d"
    tidy_wages_edu_W_W$edu_5[101:150]="c"
    tidy_wages_edu_W_W$edu_5[151:200]="b"
    tidy_wages_edu_W_W$edu_5[201:250]="a"
    
    tidy_wages_edu_W_W
    
    df6A5<-tidy_wages_edu_W_W
    class(tidy_wages_edu_W_W)
    
    qplot(year,pay,data=df6A5)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, Women, White")
    
    #6A.6_wages_edu_W_H => year=all ; gen=Women ; race=Hispanic ; level_edu=all five ; pay=values
    wages_edu_W_H <- wages_edu %>% select(year,hispanic_women_less_than_hs,hispanic_women_high_school,hispanic_women_some_college,hispanic_women_bachelors_degree,hispanic_women_advanced_degree)
    wages_edu_W_H
    tidy_wages_edu_W_H <- gather(wages_edu_W_H, edu_level, pay, `hispanic_women_less_than_hs`:`hispanic_women_advanced_degree`)
    tidy_wages_edu_W_H
    tidy_wages_edu_W_H <-tidy_wages_edu_W_H %>% mutate(gender="Women", race="Hispanic")%>% mutate(edu_5=c(1:250))
    tidy_wages_edu_W_H
    str(tidy_wages_edu_W_H)
    
    tidy_wages_edu_W_H$edu_5[1:50]="e"
    tidy_wages_edu_W_H$edu_5[51:100]="d"
    tidy_wages_edu_W_H$edu_5[101:150]="c"
    tidy_wages_edu_W_H$edu_5[151:200]="b"
    tidy_wages_edu_W_H$edu_5[201:250]="a"
    
    tidy_wages_edu_W_H
    
    df6A6<- tidy_wages_edu_W_H
    
    class(tidy_wages_edu_M_W)
    
    qplot(year,pay,data=df6A6)+geom_smooth()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary all education, Women, Hispanic")
    
#Now we've made data tidy, and have created 12 data frames: 
#6A.1,6A.2,6A.3,6A.4,6A.5,6A.6;
#6B.1,6B.2,6B.3,6B.4,6B.5,6B.6.
    
#Let's combine them all together, so we can start data exploration-analysis.
#We need to add rows. We'll use the bind_rows function from tidyverse:
    
    df6A <- bind_rows(list(df6A1,df6A2,df6A3,df6A4,df6A5,df6A6))
    df6B <- bind_rows(list(df6B1,df6B2,df6B3,df6B4,df6B5,df6B6))
    df12 <- bind_rows(list(df6A,df6B))
    
    head(df12)
   
#Let's display the internal strcuture of the new formed data frame:
    str(df12)
    #'data.frame':	3000 obs. of  6 variables:
    #$ year     : int  2022 2021 2020 2019 2018 2017 2016 2015 2014 2013 ...
    #$ edu_level: chr  "black_men_less_than_hs" "black_men_less_than_hs" "black_men_less_than_hs" "black_men_less_than_hs" ...
    #$ pay      : num  16.4 15.4 15.5 15.1 14.3 ...
    #$ gender   : chr  "Men" "Men" "Men" "Men" ...
    #$ race     : chr  "Black" "Black" "Black" "Black" ...
    #$ edu_5    : chr  "e" "e" "e" "e" ...
    
    #Level of education is now contained in two variables, "edu_level" and "edu_5". We'll create a data frame showing
    #only one variable for education level, selecting "edu_5" (since edu_5 does not include other additional (social) 
    #information:
    df12_t <- df12 %>% select(year,gender,race,edu_5,pay)
    str(df12_t)
    #[1] 'data.frame':	3000 obs. of  5 variables:
    #$ year     : int  2022 2021 2020 2019 2018 2017 2016 2015 2014 2013 ...
    #$ edu_level: chr  "black_men_less_than_hs" "black_men_less_than_hs" "black_men_less_than_hs" "black_men_less_than_hs" ...
    #$ pay      : num  16.4 15.4 15.5 15.1 14.3 ...
    #$ gender   : chr  "Women" "Women" "Women" "Women" ...
    #$ race     : chr  "Black" "Black" "Black" "Black" ...
    #$ edu_5    : chr  "e" "e" "e" "e" ...
    
    #We can compare "df12" and "df12_t", see how df12_t (Tidy) is the Tidiest version:
    
    head(df12)
    head(df12_t)
    
    unique_columns_count <-  df12 %>%
      summarise(n_year = n_distinct(year),
                n_edu_level = n_distinct(edu_level),
                n_gender = n_distinct(gender),
                n_race = n_distinct(race),
                n_edu_5=n_distinct(edu_5),
                n_pay = n_distinct(pay))
    print(unique_columns_count)
    #n_year   n_edu_level n_gender n_race n_edu_5  n_pay
    #     50          55        3      4       5    1857
    
    unique_columns_count <-  df12_t %>%
      summarise(n_year = n_distinct(year),
                n_gender = n_distinct(gender),
                n_race = n_distinct(race),
                n_edu_5=n_distinct(edu_5),
                n_pay = n_distinct(pay))
    print(unique_columns_count)
    #n_year n_gender n_race n_edu_5 n_pay
    #   50        3      4       5   1857
    
    #Summary (df12_t -vs- df12): edu_level variable had 55 different values, while there are actually only 5 different levels 
    #of education being considered. "edu_level" included gender and race variables within.
    #we have now simplified these 55 values, to the 5 actual education level values, without loosing the additional
    #information contained in "edu_level".

#Data exploration: once we've got data re-organized, let's start with representing data 
#through data visualization tools: 
    
#Starting with "generic data" (data including "All" races, "All" genders, "All" races and genders)
#compare avg values versus segmented data (by race, by gender). See how education level - year affect
#pay values:
    #we can select a race and gender, see how the level of education 
    #plays a role in the pay level through time:
   
    df12_t %>%
      filter(race == "All",gender == "All") %>%
      ggplot(aes(year, pay, color = edu_5)) +
      geom_line()+
      xlab("YEAR") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary by education")
    
    #This plot provides already a visual overview of the evolution of salary levels, 
    #based on the 5 different education levels considered, through time. 
    #By education level, through time, we observe:
    #Level "a", advanced degree, has evolved from around 37$/hour in 1973 to around 54$/h in 2022.
    #Level "b", bachelor degree, has evolved from around 33$/hour in 1973 to around 42$/h in 2022.
    #Level "c", some college, advanced degree, has changed little, from around 24$/hour in 1973 to around 25$/h in 2022.
    #Level "d", high-school, has stagnated around 22$/hour, from 1973 until 2022.Dropping 2-3$/h through the mid 90s, 
    #recovering in the mid 2000s,just to drop adn recover again around 2020.
    #Level "e", less than high-school, has dropped from around 18$/hour in 1973 to around 16$/h in 2022.
    #Between education levels: we observe large dispersion on pay values, around 38$/h pay difference 
    #between levels "e" and "a"(16-54) in 2022 (salary "a" being 2,3 times that of salary "e"). 
    #In 1973 dispersion was at 19$/h (18-37) (salary "a" being 2 times that of salary "e").
    #An interesting finding is, salaries for education levels (a,b) have grown through time, while salaries
    #in the c-d categories have stagnated (which means, purchasing power should be much lower in 2022
    #with that level of salary versus the one enjoyed in 1973 with the same pay). Category "e" has decreased.

    
#We'll now use a "facet wrap" plot to compare the pay evolution through time 
#(for each of the 5 education levels), and for the 6 race-gender combinations:
#Black-Women, Black-Men; Hispanic-Women, Hispanic-Men; White-Women, White-Men: 
#this might help us visualize significant differences among graphics:
   
    df12_t %>%
      filter(race %in% c("Black", "White", "Hispanic") & gender %in% c("Women","Men")) %>%
      ggplot(aes(year, pay, color = edu_5)) +
      geom_line() +
      facet_wrap(~race ~gender)+
      xlab("YEAR [1973-2022]") +
      ylab("SALARY [$/h]") +
      ggtitle("Salary by education, 
      Race-Gender MATRIX")
    
#Observations:
#A. On race and gender,
#A.1.Race-Gender gap: 
    #-White_Men are the best paid at equal level of education, for all 5 education levels;
    #the best paid of any race-gender combination. Specially for higher education levels.
    #-Hispanic_Women are the worst paid group at the highest education level.
    #We can print a few values confirming these observations from the plot:  
    
    which.max(df12_t$pay)
    df12_t$pay[452]
    df12_t$race[452]
    df12_t$gender[452]
    #[1] 64.04 [1] "White" [1] "Men"
    which.min(df12_t$pay)
    df12_t$pay[1278]
    df12_t$race[1278] 
    df12_t$gender[1278]
    #[1] 11.35 [1]"Hispanic" [1] "Women"
    
    #These are coherent with our perception:
    #Best (pay) value of all across our data belongs to the White_Men group;
    #Worst (pay) value of all across our data, belongs to the Hispanic_Women group.
    
#A.2. Gender gap: 
    #-The lowest gender gap within a race seems to be within the Black group. 
    #Both gender distributions through time are similar, at similar values.
    #-The White and Hispanic groups showing a larger gap between
    #gender, 
    #-White group, gap between Women and Mean is around a 40% plus pay for Men, for all education levels.
    #In absolute values,the more education level, the larger the absolute value differential. 
    #-Hispanic group: Men receive between 30 to 40% more pay than Women, for the same education level.
    
    #we can produce a plot considering only GENDER (race within), for a more generic view of
    #the effect of gender on pay:
    #overview of pay evolution through time for the two considered genders(plus All):
    df12_t %>% filter(edu_5 == "a" & race =="White") %>%
      ggplot(aes(year, pay, color=gender)) +
      geom_line()
    #Overall (not splitting gender by race), Men pay rates are well above those of Women's. 
    #all the way from 1973 until 2022. Furthermore, the gap (1973: 32-40 to 2022: 44-63) seems
    #to have grown.
    
#A.3. Race gap: 
    #-White_Men are the best payed group, while White_Women receive a worse pay then Hispanic_Men and Black_Men and 
    #Women, at high education levels.
    #-Hispanic Men are better payed than Black Men and Women, however Hispanic Women are worse payed 
    #than Black Mean and Women.
    
    #it seems that race and gender are key-factors which combined result in the observed effects. 
    #in order to view a more generic picture of the race gap, we could plot the Race-All portion of data,
    #which combines genders within each race:
    
    df12_t %>% filter(race %in% c("Black", "White", "Hispanic") & gender == "All") %>%
      ggplot(aes(year, pay, color = edu_5)) +
      geom_line() +
      facet_wrap(~race)+
      ggtitle("Salary by education and race")
      
    #this "Salary by education and race" helps compare salaries among races.
    #when not considering gender (race values combining here both genders) separately,
    #then we can see that:
    #-The best pay for education levels a,b,c,d has been through time for the White group.
    #-Hispanic where paid less than Black and White in 1973, however Hispanic are
    #paid more than Black for the top education levels a,b, still worse than the White group.
    #-The White group at the lowest education level, where paid more than Hispanic, these more
    #than Black group members, in 1973. However in 2022 the White group is the second best paid
    #at edu_5 level "e", behind Hispanic, remaining the Black group as the worst paid one
    #at level "e".
    
    #We can produce a plot considering only RACE (gender within), for a more generic view of
    #the effect of race on pay:
    #overview of pay evolution through time for the three considered races (plus All):
    df12_t %>% filter(edu_5 == "a" & gender=="Men") %>%
      ggplot(aes(year, pay, color=race)) +
      geom_line()
    #this approach reveals that, after the mid 80s:
    #-White perceive the highest salaries;
    #-Hispanic perceive the second highest salaries (except a short period in the early 2000s);
    #-Black perceive the lowest salaries.
    
#B. on time and education: 
#B.1. Time effect: consistently, 
    #-for "a" and "b" education levels, although oscillations (positive, negative slopes) 
    #are observed, a positive trend is observed through time. 
    #-For levels "c" and "d", salaries are overall stagnant, through time;
    #-For level "e", pay level has decreased over time.
    
#B.2. Education level effect: we've covered this variable before, nevertheless, as a generic consistent
    #finding, for all other variable effect combinations, education remains a key variable, its effect  
    #coherent across all visual representations of data: 
    #"across time, the higher the education level, the higher the pay".

#Finally, we can display the starting and end points of the time scale being considered,
    #observe how pay-race fare (comparison between starting and end points):  
    
    #Faceting:
    filter(df12_t, year%in%c(1973, 2022) & gender=="All") %>%
      ggplot(aes(race, pay, col = edu_5)) +
      geom_point() +
      facet_grid(. ~ year)

#Further visualization of the education level effects.
    #We can add boxplot representation for a visual representation of pay versus education, 
    #where time, race, gender explain the position of the plot 5-values.
    
    b_p <- df12_t %>%
      filter(year == 2022 & !is.na(pay)) %>%
      ggplot(aes(edu_5, pay)) +
      geom_boxplot() +
      #scale_y_continuous(trans = "log2") +
      xlab("") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    b_p
    b_p + geom_point(alpha = 0.5)
    
#In addition to that, we can represent the generic effect of time on pay.
#Facet_grid density plots can help us with visualizing trends: 
 
   filter(df12_t, year %in% c(1973,1983, 1993,2003,2013, 2022) & gender=="All") %>%
      ggplot(aes(pay)) +
      geom_density(fill="blue")+
      facet_grid(. ~ year)
    #this graphic shows a growth -through time- of density of salaries
    #towards higher pay ones, consistent with an increasing gap between (edu) pay levels.
    #growth of "a"-"b" education levels through time, and stagnation (with even decrese in level of pay,
    #in some decades) for "e"-"d".. levels of education.
    
#We can summarize our findings after data visualization work.
#GENERAL CONCLUSIONS after data wranging and visualization:
    #1.level of education has the highest effect on pay;
    #2.time has an effect on pay:
      #-2.1."a","b" levels growing;
      #-2.2."c","d" stagnating;
      #-2.3."e" levels decreasing.
    #2.race consistently (through time) affects the level of pay for the same education level,
    #being White the highest paid, hispanic following, and black last;
    #3. gender has been and continues to be a factor affecting pay level. Men are paid more than Women 
    #across education level and race;
    #4.through time pay rates are increasing for higher levels of education (a, b), and showing 
    #stagnation for the lower levels of education (c,d),and decreasing for the lowest (e), across races and genders. 
    

#Part 4. Initial solution.   
    #Our working hypothesis will be based on the work we've done so far with Data Visualization.
    #we'll fit a model which considers education, gender and race effects, separately, then a
    #global model which considers effects from education, gender and also race.
    #we'll start considering race, gender, the race+gender, then a stronger variable edu_5, and 
    #finally all of them combined, as following:
    #fit1 [pay ~ race]
    #fit2 [pay ~ gender]
    #fit3 [pay ~ gender + race]
    #fit4 [pay ~ edu_5 ]
    #fit5 [pay ~ edu_5 + gender + race]
    #fit6 [pay ~ edu_5 + year + gender + race]
    
    #least squares:
    
    #We will try first  the race effect on pay (pay ~ race):
    #race effect _ pay - fitting a linear model:
    fit1 <- lm(pay ~ as.factor(race), data = df12_t)
    fit1
    summary(fit1)
    
    #Call:
    # lm(formula = pay ~ as.factor(race), data = df12_t)
    
    #Residuals:
    # Min      1Q  Median      3Q     Max 
    #-16.331  -7.822  -3.429   7.846  36.061 
    
    #Coefficients:
    #Estimate Std. Error t value Pr(>|t|)    
    #(Intercept)              27.4589     0.3811  72.046  < 2e-16 ***
    # as.factor(race)Black     -1.8798     0.5390  -3.488 0.000495 ***
    # as.factor(race)Hispanic  -2.4815     0.5390  -4.604 4.32e-06 ***
    # as.factor(race)White      0.6016     0.5390   1.116 0.264472    
    
    # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    #Residual standard error: 10.44 on 2996 degrees of freedom
    #Multiple R-squared:  0.01476,	Adjusted R-squared:  0.01377 
    #F-statistic: 14.96 on 3 and 2996 DF,  p-value: 1.15e-09
    
    #Coefficients:
    #(Intercept)     as.factor(race)Black  as.factor(race)Hispanic     as.factor(race)White  
    #27.4589                  -1.8798                  -2.4815                   0.6016 
    
    #fitting a linear model with pay as dependent and race as independent variables, data from df12_t, 
    #we obtain an intercept of 27.4589 and slopes for Black (-1.8798), Hispanic(-2.4815) and White (0.6016).
    #According to this, we observe a positive effect on pay for the White group, and negative effects 
    #for then Black and Hispanic groups. 
    #p-values are very low for Black and Hispanic, but over 0.05 for White.
    #however residuals arelarge. 
    #multiple R squared is far from 1, the model can explain some 1,4% of the variability.
    
    #Then we'll study the gender effect on pay (gender ~ race):
    fit2 <- lm(pay ~ as.factor(gender), data=df12_t)
    summary(fit2)
    
    ##result of summary(fit2)
    #Call:
    # lm(formula = pay ~ as.factor(gender), data = df12_t)
    
    #Residuals:
    #  Min      1Q  Median      3Q     Max 
    #-15.349  -7.590  -3.539   7.470  34.571 
    
    #Coefficients:
    #Estimate Std. Error t value Pr(>|t|)    
    #(Intercept)             26.3648     0.3241  81.355  < 2e-16 ***
    
    # as.factor(gender)Men     3.1038     0.4583   6.772 1.52e-11 ***
    # as.factor(gender)Women  -2.6412     0.4583  -5.763 9.10e-09 ***
    
      # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
      
      #Residual standard error: 10.25 on 2997 degrees of freedom
      #Multiple R-squared:  0.04992,	Adjusted R-squared:  0.04929 
      #F-statistic: 78.74 on 2 and 2997 DF,  p-value: < 2.2e-16
      
      #Coefficients:
      #(Intercept)    as.factor(gender)Men  as.factor(gender)Women  
      #26.365                   3.104                  -2.641  
      
    #fitting a linear model with pay as dependent and gender as independent variables, data from df12_t, 
    #we obtain an intercept of 26.365 and slopes for Men (3.104) and Women (-2.641).
    #gender=Men adds to the pay value while gender=Women reduces pay.
    #p-values are very low for gender, both for Men and Women, thus the predictors are good.
    #however residuals are very high. 
    #multiple R squared is far from 1, the model can explain some 5% of the variability.
    
    #Now we'll combine gender and race, see how its combined effects improve the model:
    
    fit3 <- lm(pay ~ as.factor(gender) + as.factor(race), data=df12_t)
    summary(fit3)
    plot(fit3$residuals, pch = 16, col = "red")
    
   # Residuals:
   #  Min      1Q  Median      3Q     Max 
   #-15.019  -7.344  -3.813   7.402  33.111 
    
   # Coefficients:
   # Estimate Std. Error t value Pr(>|t|)    
   # (Intercept)              27.3047     0.4550  60.016  < 2e-16 ***
   # as.factor(gender)Men      3.1038     0.4550   6.822 1.08e-11 ***
   # as.factor(gender)Women   -2.6412     0.4550  -5.805 7.09e-09 ***
   # as.factor(race)Black     -1.8798     0.5253  -3.578 0.000351 ***
   # as.factor(race)Hispanic  -2.4815     0.5253  -4.724 2.42e-06 ***
   # as.factor(race)White      0.6016     0.5253   1.145 0.252256    
   
   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
   # Residual standard error: 10.17 on 2994 degrees of freedom
   # Multiple R-squared:  0.06468,	Adjusted R-squared:  0.06312 
   # F-statistic: 41.41 on 5 and 2994 DF,  p-value: < 2.2e-16
    
    #fitting a linear model with pay as dependent and gender and race as independent variables, 
    #data from df12_t, #we obtain:
    #-an intercept of 26.365 and slopes for Men (3.104) and Women (-2.641),
    #-slopes for Black (-1.8798), Hispanic (-2.4815), and White (0.6016)
    #gender=Men and race=White adds to the pay value while gender=Women and race=Hispanic or Black 
    #reduces pay.
    #p-values are very low for Men, Women, Black, Hispanic, making these good predictors, however White
    #shows a 0.252 value (>0.05), not so good as a predictor.
    #Residuals remain large.
    #multiple R squared is still far from 1. The model can explain some 6,3% of the variability.
    
    #Following we study the effect of edu_5 on pay. Considering education level seems to be
    #have the strongest effects on pay (hypothesis after perception from data visualization):
    fit4 <- lm(pay ~ edu_5, data=df12_t)
    summary(fit4)
    plot(fit4$residuals, pch = 16, col = "red")
    
    #Residuals:
    #  Min       1Q   Median       3Q      Max 
    #-20.8410  -2.4267  -0.3572   1.9541  22.2690 
    
    #Coefficients:
    #  Estimate Std. Error t value Pr(>|t|)    
    #(Intercept)  41.7710     0.1818  229.71   <2e-16 ***
    #  edu_5b       -8.9748     0.2572  -34.90   <2e-16 ***
    #  edu_5c      -19.2598     0.2572  -74.89   <2e-16 ***
    #  edu_5d      -21.6126     0.2572  -84.04   <2e-16 ***
    #  edu_5e      -26.4129     0.2572 -102.71   <2e-16 ***
    
    #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    #Residual standard error: 4.454 on 2995 degrees of freedom
    #Multiple R-squared:  0.8206,	Adjusted R-squared:  0.8204 
    #F-statistic:  3426 on 4 and 2995 DF,  p-value: < 2.2e-16
    
    #fitting a linear model with pay as dependent and edu_5 as independent variables, 
    #data from df12_t, we obtain:
    #-an intercept of 41.7710 and negative slopes for edu levels b,c,d and e.
    #p-values are low across the education levels, thus the predictor(s) are good.
    #however residuals keep being large. 
    #multiple R squared is now close to 1, the model can explain some 82% of the variability.
    #this is a KEY FINDING, this confirms the perception that EDUCATION LEVEL has 
    #the MOST EFFECT on PAY.
    
    #Finally, we'll fit a model with "edu_5", "gender", and "race" as independent variables
    #(pay ~ edu_5 + gender + race):
    fit5 <- lm(pay ~ as.factor(edu_5) + as.factor(gender) + as.factor(race), data=df12_t)
    summary(fit5)
    plot(fit5$residuals, pch = 16, col = "red")
    
    #Residuals:
    # Min       1Q   Median       3Q      Max 
    #-16.6591  -1.6084  -0.1449   1.3394  17.8594 
    
    #Coefficients:
    #  Estimate Std. Error  t value Pr(>|t|)    
    #(Intercept)              42.5568     0.2058  206.783   <2e-16 ***
    #  as.factor(edu_5)b        -8.9748     0.2058  -43.608   <2e-16 ***
    #  as.factor(edu_5)c       -19.2598     0.2058  -93.583   <2e-16 ***
    #  as.factor(edu_5)d       -21.6126     0.2058 -105.016   <2e-16 ***
    #  as.factor(edu_5)e       -26.4129     0.2058 -128.340   <2e-16 ***
    #  as.factor(gender)Men      3.1038     0.1594   19.470   <2e-16 ***
    #  as.factor(gender)Women   -2.6412     0.1594  -16.568   <2e-16 ***
    #  as.factor(race)Black     -1.8798     0.1841  -10.212   <2e-16 ***
    #  as.factor(race)Hispanic  -2.4815     0.1841  -13.481   <2e-16 ***
    #  as.factor(race)White      0.6016     0.1841    3.268   0.0011 ** 
    
    #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    
    #Residual standard error: 3.565 on 2990 degrees of freedom
    #Multiple R-squared:  0.8853,	Adjusted R-squared:  0.885 
    #F-statistic:  2565 on 9 and 2990 DF,  p-value: < 2.2e-16
    
    #fitting a linear model with pay as dependent and edu_5, gender, race as independent variables, 
    #data from df12_t, we obtain:
    #-an intercept of 42.5568 and positive slopes for Men and White, and negative slopes for edu levels b,c,d and e, Women, Black, and Hispanic.
    #p-values are low across the education levels, thus the predictor(s) are good.
    #residuals have decreased (-17 to +18).
    #multiple R squared is now close to 1, the model can explain some 88.5% of the variability.
  
  #This is yet the best model version.
    #as a last potential improvement, let's add to it YEAR as independent variable:
    
    #Finally, we'll fit a model with "edu_5", "gender", and "race" as independent variables
    #(pay ~ edu_5 + gender + race):
    fit6 <- lm(pay ~ as.factor(edu_5) + as.factor(year)+ as.factor(gender) + as.factor(race), data=df12_t)
    summary(fit6)
    plot(fit6$residuals, pch = 16, col = "red")
    
    #Residuals:
    #  Min       1Q   Median       3Q      Max 
    #-15.1794  -1.7902  -0.2027   1.7869  14.0083 
    
    #Coefficients:
    #  Estimate Std. Error  t value Pr(>|t|)    
    #(Intercept)              41.79359    0.43883   95.239  < 2e-16 ***
    #Residual standard error: 3.129 on 2941 degrees of freedom
    #Multiple R-squared:  0.9131,	Adjusted R-squared:  0.9114 
    #F-statistic: 532.6 on 58 and 2941 DF,  p-value: < 2.2e-16
    
    #fitting a linear model with pay as dependent and edu_5, year, gender, race as independent variables, 
    #data from df12_t, we obtain:
    #-an intercept of 41.79359 and positive slopes for Men and White,and years from 1998 to 2022, 
    #-and negative slopes for edu levels b,c,d and e, Women, Black, and Hispanic, and years 1973-1997.
    #p-values are low across all variables-values, expect for years 1973-1997.
    #residuals have decreased (-15 to +14).
    #multiple R squared is now close to 1, the model can explain some 91.1% of the variability.
    
    
#MACHINE LEARNING MODEL:
#Part 5. Solution.  Building a "Recommendation System" type tool.
#Now we'll be interested on projecting expected pay rates through the period, 1973-2022.
#We can start with defining the overall pay average, through time, and across all relevant
#variables, education level, race, and gender. 
    
#we'll work with a "recommendation system" -type tool.
# We will create a simple model -algorithm- which we will improve through a sequence of steps 
#(successive models). 
# Using a training set (train_set) and a test set (test_set) to assess the accuracy of the models.
# The RMSE function (residual mean square error) will help us evaluate model accuracy.
# By calculating the error made between prediction of ratings, and true ratings.  
    
#Creating train and test sets:
    library(caret)
    set.seed(755)
    test_index <- createDataPartition(y = df12_t$pay, times = 1, p = 0.2,
                                      list = FALSE)
    #test_index
    train_set <- df12_t[-test_index,]
    test_set <- df12_t[test_index,]
    train_set
    test_set
    
    #To make sure we don’t include RACE and GENDER in the test set that do not appear in the
    #training set, we remove these entries using the semi_join function:
    test_set <- test_set %>%
      semi_join(train_set, by = "race") %>%
      semi_join(train_set, by = "gender")
    test_set
    
    #The RMSE function
    RMSE <- function(true_pay, predicted_pay){
      sqrt(mean((true_pay - predicted_pay)^2))
    }
    #A FIRST MODEL:
    mu_hat <- mean(df12_t$pay)
    mu_hat
    #> #[1]  26.51901
    #If we predict all unknown ratings with ˆμ we obtain the following RMSE:
    naive_rmse <- RMSE(test_set$pay, mu_hat)
    naive_rmse
    #> #[1] 10.70215
    
        #Regarding significance of the obtained values, considering the nature of our data.
        #the obtained mu_hat is the average pay, through the considered period, and all variables.
        
        #> we've got sets of data referring to ALL genders and ALL races. 
        #> #and also sets of data, specific for a matrix of gender-race values.
        #> let's see if these two (generic-ALL ; matrix) types of data, are to be considered in this study,
        #> or a filter is to be applied:
        
        #> #Let's study if values for ALL genders-ALL races, correspond to the combination of the
        #> #3 races and 2 genders being considered:
        
        #df12_t.All <- df12_t%>% filter( race=="All" & gender=="All")
        #str(df12_t.All)
        #mu_hat.All <- mean(df12_t.All$pay)
        #mu_hat.All
        #[1] 27.6932
        
        #we've got 6 groups of 250 observations, each based on generic information (including 
        #more than variable variation within, for example all White observations, which include both genders within)
        #These are "All"; "All MEN"; "All WOMEN";"All White"; "All Black"; "All Hispanic".
        
        #if we only include observations with independent variables defining 
        #the observation value (matrix of genders x races), we might get more accurate variable -effect 
        #approximations. 
        
        #df12_t.matrix <- df12_t%>% filter( race %in% c("Black","White","Hispanic") & gender %in% c("Women","Men"))
        #str(df12_t.matrix)
        #mu_hat.matrix <- mean(df12_t.matrix$pay)
        #mu_hat.matrix
        #[1] 26.34754
        
        #naive_rmse
        #> #[1] 10.34644 for df12_t.matrix, a little lower than the former 10.706  (for df12_t)
        
        #We might want to select df12_t.matrix from here. However the observed difference between using one (all data) 
        #or the other ("matrix" data only) approach is small (delta-mu = 0.17   ; delta-rmse= 0.35 )
          
#Our first model will return an average expected pay for any combination of education level, race, 
    #gender and year:        
   
    mu_hat <- mean(df12_t$pay)
    mu_hat
    #[1] 26.51901
          
    naive_rmse <- RMSE(test_set$pay, mu_hat)
    naive_rmse
    #[1] 10.70215
          
    rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
    rmse_results 
    
    # A tibble: 1 × 2
    #method            RMSE
    #<chr>            <dbl>
    #  1 Just the average  10.7
        
    
# Modeling edu effects            
    mu_hat_2 <- mean(df12_t$pay) 
    mu_hat_2
    #[1] 26.51901
    edu_avgs <- train_set %>% 
      group_by(edu_5) %>% 
      summarize(b_i = mean(pay - mu_hat_2))
    edu_avgs
                
    edu_avgs %>% qplot(b_i, geom ="histogram", bins = 3, data = ., color = I("black"))
                
    predicted_pay <- mu_hat_2 + test_set %>% 
      left_join(edu_avgs, by='edu_5') %>%
      pull(b_i)
    predicted_pay
      
      model_1_rmse <- RMSE(predicted_pay, test_set$pay)
      model_1_rmse
      #[1] 4.496685
                
      rmse_results <- bind_rows(rmse_results,
            data_frame(method="Educ Effect Model",
               RMSE = model_1_rmse ))
      rmse_results
      rmse_results %>% knitr::kable()       
      #|method             |      RMSE|
      #|:------------------|---------:|
      #|Just the average   | 10.702153|
      #|edu Effect Model   |  4.496684| 
      #|     

#Modeling race effect on top of edu effect:
               
      train_set %>% 
          group_by(race) %>% 
          summarize(b_u = mean(pay)) %>% 
          ggplot(aes(b_u)) + 
          geom_histogram(bins = 5, color = "black")
          #graphic shows distribution around b_u of ca.26,5        
                
      race_avgs <-train_set %>% 
           left_join(edu_avgs, by='edu_5') %>%
           group_by(race) %>%
           summarize(b_u = mean(pay - mu_hat - b_i))
      race_avgs  
      # A tibble: 4 × 2
      #race        b_u
      #<chr>     <dbl>
      #1 All       0.941
      #2 Black    -0.876
      #3 Hispanic -1.59 
      #4 White     1.54 
                
      predicted_pay_2 <- test_set %>% 
          left_join(edu_avgs, by='edu_5') %>%
          left_join(race_avgs, by='race') %>%
          mutate(pred = mu_hat + b_i + b_u) %>%
          pull(pred)
      predicted_pay_2      
                
       model_2_rmse <- RMSE(predicted_pay_2, test_set$pay)
       model_2_rmse
       #[1] 4.318814        
             
        rmse_results <- bind_rows(rmse_results,
              data_frame(method="Educ + Race Effects Model",  
              RMSE = model_2_rmse ))
        rmse_results %>% knitr::kable()        
        # |method                    |      RMSE|
        # |:-------------------------|---------:|
        # |Just the average          | 10.702153|
        # |edu Effect Model          |  4.496684|
        # |Educ + Race Effects Model |  4.318814| 
        
#Adding gender effect on top of race effect on top of edu effect:
        
        train_set %>% 
          group_by(gender) %>% 
          summarize(b_w = mean(pay)) %>% 
          ggplot(aes(b_w)) + 
          geom_histogram(bins = 3, color = "black")
   
        gender_avgs <-train_set %>% 
          left_join(edu_avgs, by='edu_5') %>%
          left_join(race_avgs, by='race') %>%
          group_by(gender) %>%
          summarize(b_w = mean(pay - mu_hat - b_i - b_u))
        gender_avgs  
  
        predicted_pay_03 <- test_set %>% 
          left_join(edu_avgs, by='edu_5') %>%
          left_join(race_avgs, by='race') %>%
          left_join(gender_avgs, by='gender')%>%
          mutate(pred = mu_hat + b_i + b_u +b_w) %>%
          pull(pred)
        predicted_pay_03       
    
        model_03_rmse <- RMSE(predicted_pay_03, test_set$pay)
        model_03_rmse
        #[1] 3.644879
        
        rmse_results <- bind_rows(rmse_results,
                                  data_frame(method="Educ + Race + Gender Effects Model",  
                                             RMSE = model_03_rmse ))
        rmse_results %>% knitr::kable()
        
      
          #|method                             |      RMSE|
          #|:----------------------------------|---------:|
          #|Just the average                   | 10.702153|
          #|edu Effect Model                   |  4.496684|
          #|Educ + Race Effects Model          |  4.318814|
          #|Educ + Race + Gender Effects Model |  3.644879|
        
#Effect of education in the model is important. Also taking into account race and 
#Gender effects help improve the model's results. 
        
#Adding year effects:
        
        train_set %>% 
          group_by(year) %>% 
          summarize(b_z = mean(pay)) %>% 
          ggplot(aes(b_z)) + 
          geom_histogram(bins = 3, color = "black")
        
        year_avgs <-train_set %>% 
          left_join(edu_avgs, by='edu_5') %>%
          left_join(race_avgs, by='race') %>%
          left_join(gender_avgs, by='gender')%>%
          group_by(year) %>%
          summarize(b_z = mean(pay - mu_hat - b_i - b_u - b_w))
        year_avgs  
        
        predicted_pay_04 <- test_set %>% 
          left_join(edu_avgs, by='edu_5') %>%
          left_join(race_avgs, by='race') %>%
          left_join(gender_avgs, by='gender')%>%
          left_join(year_avgs, by='year')%>%
          mutate(pred = mu_hat + b_i + b_u +b_w +b_z) %>%
          pull(pred)
        predicted_pay_04       
        
        model_04_rmse <- RMSE(predicted_pay_04, test_set$pay)
        model_04_rmse
        #[1] 3.262347
        
        rmse_results <- bind_rows(rmse_results,
                                  data_frame(method="Educ + Race + Gender + Year Effects Model",  
                                             RMSE = model_04_rmse ))
        rmse_results %>% knitr::kable()
        
        #|method                                    |      RMSE|
        #|:-----------------------------------------|---------:|
        #|Just the average                          | 10.702153|
        #|edu Effect Model                          |  4.496684|
        #|Educ + Race Effects Model                 |  4.318814|
        #|Educ + Race + Gender Effects Model        |  3.644879|
        #|Educ + Race + Gender + Year Effects Model |  3.262347|
       
#Regularization:   
            
#-By considering the education, race, gender and year effect in our model, we have improved model accuracy, 
#now well below the initial 10,7 value.
#-We could further improve model accuracy, by using regularization, 
#removing effects of noisy estimates (penalizing large estimates coming from small sample sizes).
#Anomalies in pay curves through time.
                
    
#Penalized least squares
#Choosing lambda <- 3
                
          lambda <- 3
          mu <- mean(train_set$pay)
          edu_train_avgs <- train_set %>% 
             group_by(edu_5) %>%
             summarize(b_i=sum(pay-mu)/(n()+lambda),n_i=n())
          edu_train_avgs    
                
       #results
       predicted_pay_05 <- test_set %>%
         left_join(edu_train_avgs, by = "edu_5") %>%
         mutate(pred=mu + b_i) %>%
         pull(pred)
       predicted_pay_05
                
       sum(is.na(predicted_pay_05)) #[1] 0
       model_5_rmse <- RMSE(predicted_pay_05, test_set$pay)
       model_5_rmse
                
       predicted_pay_05na <-replace_na(predicted_pay_05, mu_hat)
       predicted_pay_05na
                
       #RMSE(predicted_ratings,temp$rating)
        model_5_rmse.na <- RMSE(predicted_pay_05na, test_set$pay)
        model_5_rmse.na
        #[1] 4.501724
                
        rmse_results <- bind_rows(rmse_results,
                 data_frame(method="Regularized Educ Effect Model",  
                 RMSE = model_5_rmse))
        rmse_results %>% knitr::kable()        
                
        #|method                                    |      RMSE|
        #|:-----------------------------------------|---------:|
        #|Just the average                          | 10.702153|
        #|edu Effect Model                          |  4.496684|
        #|Educ + Race Effects Model                 |  4.318814|
        #|Educ + Race + Gender Effects Model        |  3.644879|
        #|Educ + Race + Gender + Year Effects Model |  3.262347|
        #|Regularized Edu Effect Model              |  4.501724|
             
        
        
#Cross-validation - we'll use cross-validation for choosing a lambda:
      lambdas <- seq(0, 10, 0.25)
                
       ##sum(is.na(edx$rating)) #[1] 0
       ##sum(is.na(temp$rating)) #[1] 0
       #edx$rating
        train_set$pay    
        rmses <- sapply(lambdas, function(l){
          mu <- mean(train_set$pay)
          b_i <- train_set %>%
            group_by(edu_5) %>%
            summarize(b_i = sum(pay - mu)/(n()+l))
          b_u <- train_set %>%
            left_join(b_i, by="edu_5") %>%
            group_by(race) %>%
            summarize(b_u = sum(pay - b_i - mu)/(n()+l))
          b_w <- train_set %>%
            left_join(b_i, by="edu_5") %>%
            left_join(b_u, by="race") %>%
            group_by(gender) %>%
            summarize(b_w = sum(pay - b_i -b_u - mu)/(n()+l))
          predicted_pay_06 <-
            test_set %>%
            left_join(b_i, by = "edu_5") %>%
            left_join(b_u, by = "race") %>%
            left_join(b_w, by = "gender") %>%
            mutate(pred = mu + b_i + b_u + b_w) %>%
            pull(pred)
          return(RMSE(predicted_pay_06, test_set$pay))
        })
        
        qplot(lambdas, rmses)
        lambda <- lambdas[which.min(rmses)]
        lambda
        #[1] 0
        lambda_REE <-0
        lambda_REE
        
        #rmse for lambda is 3.645
        
        rmse_lambda_REE <- sapply(lambda_REE, function(l){
          mu <- mean(train_set$pay)
          b_i <- train_set %>%
            group_by(edu_5) %>%
            summarize(b_i = sum(pay - mu)/(n()+l))
          b_u <- train_set %>%
            left_join(b_i, by="edu_5") %>%
            group_by(race) %>%
            summarize(b_u = sum(pay - b_i - mu)/(n()+l))
          b_w <- train_set %>%
            left_join(b_i, by="edu_5") %>%
            left_join(b_u, by="race") %>%
            group_by(gender) %>%
            summarize(b_w = sum(pay - b_i -b_u - mu)/(n()+l))
          predicted_pay_06 <-
            test_set %>%
            left_join(b_i, by = "edu_5") %>%
            left_join(b_u, by = "race") %>%
            left_join(b_w, by = "gender") %>%
            mutate(pred = mu + b_i + b_u + b_w) %>%
            pull(pred)
          return(RMSE(predicted_pay_06, test_set$pay))
        })
        
        rmse_lambda_REE
        #[1] 3.644879
                
     rmse_results <- bind_rows(rmse_results,
              data_frame(method="Regularized Educ+Race+Gender Effect Model",  
                                                     RMSE = rmse_lambda_REE ))
     rmse_results %>% knitr::kable()         
    
     
     #|method                                    |      RMSE|
     #|:-----------------------------------------|---------:|
     #|Just the average                          | 10.702153|
     #|edu Effect Model                          |  4.496684|
     #|Educ + Race Effects Model                 |  4.318814|
     #|Educ + Race + Gender Effects Model        |  3.644879|
     #|Educ + Race + Gender + Year Effects Model |  3.262347|
     #|Regularized Edu Effect Model              |  4.501724|
     #|Regularized Edu+Race+Gender Effect Model  |  3.644879|
    
    
    #Regularization is not doing much here, we could conclude there isn't much "noise" to be neutralized.
    #Our model including "year" produces the best RMSE.
 
##Part 6 - CONCLUSIONS.
     
     #GENERAL CONCLUSIONS after data wranging and visualization, and modeling:
     
     #1.All variables (4 independent), education level (edu_5), time (year), gender (gender), race (race) have effects on 
     #wages (pay).
     #2.Level of EDUCATION has the highest effect on pay. The higher the education level, the higher wages,
     #through time and for any race and gender;
     #3.TIME has an effect on pay:
     #-3.1."a" (advanced_degree) and "b" (bachelors_degree) levels growing through time;
     #-3.2."c" (some_college) and "d" (high_school) stagnating;
     #-3.3."e" (lower_than_high-school)level decreasing thorugh time.
     #Through time pay rates are increasing for higher levels of education (a, b), and showing 
     #stagnation for the lower levels of education (c,d),and decreasing for the lowest (e), across 
     #races and genders.
     #2.RACE consistently (through time) affects the level of pay for the same education level,
     #being through time (in general, except a few anomalies) White the highest paid, Hispanic following, 
     #and Black last;
     #4.GENDER has been and continues to be a factor affecting pay level. Men are paid more than Women 
     #across education level and race;
     #5.Regarding combinations of RACE-GENDER: 
     #-White_Men are the best paid at equal level of education, for all 5 education levels;
     #the best paid of any race-gender combination. Specially for higher education levels.
     #-Hispanic_Women are the worst paid group at the highest education level.
     # Effects of the 4 independent variables are shown on the various modeling approaches we've 
     # utilized in this study, coherent with these observations.
     
     #Note this study is based on the available data (selected data frame), and is therefore limited by
     #the extension and depth of the data. As an example ,three races are considered only, while population 
     #in the USA includes a quite larger number of ethnicities. 
     #Nevertheless, this the study illustrates, within its limitations, the impact- effect, of the various 
     #considered sociodemographic variables.The study could be further enriched by adding additional relevant complementary data to it.
     
     
 
##########################################
 
    
    