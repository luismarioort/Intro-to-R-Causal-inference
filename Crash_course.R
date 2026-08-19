# R for causal inference
# Material for Political Economy I at ITAM- Professor Horacio Larreguy
# Author: Luis Mario Ortiz Gutierrez

#Setting for our session -------------------------------------------------------
##How can we manage packages? : ------------------------------------------------
##Classic option

#We install the package:
install.packages("dplyr")

#Then, we call it
library(dplyr)

#Pacman solution:
if(!require("pacman")){ #This way, we can run the code and it will not install it if we already have it
  install.packages("pacman")
}

#Then, we update and call all the packages needed
pacman::p_load(dplyr, #For data manipulation
               ggplot2,#For visualization
               this.path, #This allows us to get the path of the folder where this file is
               fixest #This is for regressions
               )

#We set our working directory using the previous package
setwd( #This sets our working directory (to call data (input) and save graphs or tables (output)
  dirname( #This calls for the folder containing the folder where this file is 
    this.dir() #The path of this file
    ))

# Clear workspace
rm(list = ls())

#Part 1: Data   ----------------------------------------------------------------

##Types of data   --------------------------------------------------------------
#Objects in the enviroment (note, they cannot start with numbers, or have many special characters)
double <- 1.15
integer <- 42L #That's how we force a number to be an integer 
my_logical <- TRUE
my_character <- "Econometrics"

typeof(double) #This way, we can know what something is
typeof(15/2)
typeof(15)
typeof(15L)
typeof("15")

##Vectors ---------------------------------------------------------------------- 
numeric <- c(1,2,3,4,5)
typeof(numeric) #Type of does not give you vector, 

vector_1=c("a",1,4L) #A vector can only have one type of data. it coerces the components into the most flexible 
typeof(vector_1)

vector_2=c(1,4L)
typeof(vector_2)

vector_3=c(TRUE,4L, FALSE)
typeof(vector_3) #TRUE gets coerced as 1. FALSE as 0.
vector_3

vector_4=c("TRUE",TRUE,"FALSE",FALSE,1,1L)
typeof(vector_4)
vector_4

#You can get statistics for numeric (double or integer) vectors
mean(numeric)
median(numeric)
sd(numeric)

#Distribution for other vectors
table(vector_4)

##Data frames ------------------------------------------------------------------
#They are a way of grouping vectors from the same size
#In practice, super useful way to save data. Columns are variables, rows are observations
#You can store multiple dimensions of data for your unit of analysis (observation)

#Example:

name <- c("Luis", "Horacio", "Alexa", "Adrián", "Maria José")

major <- c("Economics", "Economics", "Political science", "Political science", "International Relationships")

grade <- c(8.5,9.5,9.5,8.6,9.8)

school = data.frame(names,major,grade) #They have to be the same size!


#How do we use what is in a data frame?

#As vectors:
school$grade

mean(school$grade)

#Broad analysis
mean(school$grade)

table(school$major)

table(school$major,school$grade)

str(school)

#By observations:

school[1,]
school[c(1,2,3),]

#By points:
school[1,1]


##Panel data -------------------------------------------------------------------
#Same unit of analysis for many periods
#We can simulate data to check a lot of things

set.seed(1) #We set seed to replicate random stuff

#Suppose we are interested on tracking progress over the semesters

name <- c("Luis", "Horacio", "Alexa", "Adrián", "Maria José")
semester <-c(1:8)

#We expand to see each observation across time
school_panel <- expand.grid(
  name=as.character(name),
  semester=semester
)

#Let's begin with dplyr. It is great for manipulation of databases 

school_panel <- school_panel |> #What |< (or %>%) does is that it asks to modify the data next to it
  mutate(
    grades=rnorm(n=40,mean=8.5,sd=1), #We simulate a normal distribution for the grades
    grades_rounded=round(grades),
    year=2022+semester%/%2, #(%/% is integer division)
         ) 

school_panel = school_panel %>% 
  group_by(name) %>% #You group by a variable and take statistics within that variable
  mutate(average_grade=mean(grades),
         lowest_grade=min(grades)) %>%
  ungroup()

graduates <- school_panel %>%
  filter(average_grade>=8)#This is used to subset (or filter) the data

graduates_summary <- school_panel %>% 
  group_by(name) %>% #You group by a variable and take statistics within that variable
  summarise(average_grade=mean(grades), #You change the structure of the database. One observation per grouped variable.
         lowest_grade=min(grades)) %>%
  ungroup()

only_grades<-school_panel %>%
  select(name,semester,grades) %>% #When you only need certain variables
  rename(student=name) #self explanatory

#What if you have many datasets and you need info from them?
names_2

admin_data<-data.frame(
  name=c("Horacio","Celeste", "Alexa", "Adrián", "Maria José", "Pablo", "Daniela"),
  year_entered=round(runif(n=7,min=2000,max=2003))
)

#Left join respects structure of left database
left=left_join(school_panel,admin_data,by="name")

#Right join respects structure of left database
right<-right_join(school_panel,admin_data,by="name")

#Inner join takes the intersection
inner<-inner_join(school_panel,admin_data,by="name")

#Full takes everything on both 
full<-full_join(school_panel,admin_data,by="name")

#Clean enviroment
rm(list = ls())

#Part 2: visualization before regression ---------------------------------------

#Let's simulate data with way more observations
school_panel <- expand.grid(
  id_student=paste0("a",c(1:50)),semester=c(1:9)
)

student=data.frame(
  id_student=paste0("a",c(1:50)),
  major=rdunif(50, b=4, a = 1))  %>%
  mutate(
    ability=rnorm(50,8,0.5),
    gpa_hs=ability+rnorm(50,0,1),
    gpa_hs=case_when(
      gpa_hs>10~10,
      gpa_hs<6~6,
      TRUE~gpa_hs)
  )

major_name=data.frame(major=c(1,2,3,4),
                      major_name=c("Economics", "Political science", "Law", "International Relationships"))
student=left_join(student,major_name,by="major")


school_panel=left_join(school_panel, student, by="id_student") 
school_panel=school_panel %>%
  mutate(
    difficulty=(major+semester-0.5*major*semester)/2,
    attendance=runif(450,0,1),
    gpa=0.9*ability+attendance-difficulty,
    gpa=case_when(
      gpa>10~10,
      gpa<5~5,
      TRUE~gpa)
  )

#Plots -----------------------------------------------------------------------
summary(school_panel$gpa)

#Density chart  
ggplot(data=school_panel, aes(x=gpa)) +
  geom_density()+
  theme_bw()

#How can we split it? several options:
ggplot(data=school_panel, aes(x=gpa,color=major_name)) +
  geom_density()+
  theme_bw()

ggplot(data=school_panel, aes(x=gpa)) +
  geom_density()+
  facet_wrap(~ major_name) +
  theme_bw()


ggplot(data=school_panel, aes(x=gpa)) +
  geom_density()+
  facet_grid(~ major_name) +
  theme_bw()

#How related is gpa with attendance?

ggplot(data=school_panel, aes(x=attendance,y=gpa)) +
  geom_point() +
  theme_bw()

#We can add a regression line:

ggplot(data=school_panel, aes(x=attendance,y=gpa)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

#Or, we can run a regression:

#We will be using feols or felm in this course:
?feols
model1=feols(
  fml=gpa~attendance, #Our formula GPA=\beta_0+\beta_1 Attendance_i +u_
  vcov=~id_student, #Our variance covariance matrix. We should include it at the level on which the treatment is assigned.
  data=school_panel
  )

etable(model1) #Our model is not well specified: the coefficients are biased

#What if we use the correct specification?

model2=feols(
  fml=gpa~attendance+ability+difficulty,
  vcov=~id_student, #If we use a variable in the data, it clusters. Else, you should at least use heteroskedasticity robust errors: vcov="hetero" 
  data=school_panel
)

etable(model1,model2)

