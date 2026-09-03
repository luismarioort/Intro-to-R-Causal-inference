# R for causal inference: Experiments
# Material for Political Economy I at ITAM- Professor Horacio Larreguy
# Author: Luis Mario Ortiz Gutierrez

pacman::p_load(dplyr, #For data manipulation
               ggplot2,#For visualization
               this.path, #This allows us to get the path of the folder where this file is
               fixest, #This is for regressions
               randomizr, #For randomization
               RCT
)


#We set our working directory using the previous package
setwd(dirname(this.dir()))

#Fundamental problem of causal inference ---------------------------------------

#When we try to show that the A has an effect on B, we implicitly are talking of counterfactuals
#If A rises B, then B would have been lower if A had not happened
#In the real world we can only observe whether it happens or if it did not

#Luckily, when we simulate, we can also simulate the counterfactuals.


#Suppose, we want to see the effect of a mentoring program in grades.

##Let's simulate the data: -----------------------------------------------------
set.seed(1)

student=data.frame(
  id_student=paste0("a",c(1:100)),
  ability=rnorm(100,7,0.5)
  ) %>%
  mutate(
    gpa_hs=ability+rnorm(100,0,1),
    gpa_hs=case_when(
      gpa_hs>10~10,
      gpa_hs<6~6,
      TRUE~gpa_hs)
  )

#Assume that, if your high school gpa is <7.5, you have to take a nivelation course

#Also assume that, on average, if attending the course make your first semester GPA increase on 1 point 

effect=1.0

#What would have been the gpa if the program did not exist, assume something similar to high school

student= student %>%
  mutate(
    potential_gpa=ability+rnorm(100,0,0.2),
    potential_gpa=case_when(
      potential_gpa>10~10,
      potential_gpa<6~6,
      TRUE~potential_gpa),
    attended=ifelse(potential_gpa<7,1,0),
    actual_gpa=ifelse(potential_gpa<7,potential_gpa+effect,potential_gpa
                      )
    )

## Selection bias --------------------------------------------------------------

#What happens if we try to estimate the effect?

simple_ols=feols(actual_gpa~attended,data=student)
cofounded_control=feols(actual_gpa~attended+gpa_hs,data=student)


etable(simple_ols,cofounded_control)

obs_diff=mean(student$actual_gpa[student$attended==1])-mean(student$actual_gpa[student$attended==0])



#The effect is negative. Why, because of the selection bias:

#E[Y|D=1]-E[Y|D=0] = E[Y(1)|D=1]-E[Y(0)|D=0] + E[Y(0)|D=1] - E[Y(0)|D=1]
#---------------------------------------------this is 0 since X-X=0

#We regroup the terms:
#E[Y|D=1]-E[Y|D=0] = E[Y(1)|D=1] - E[Y(0)|D=1] + E[Y(0)|D=1] - E[Y(0)|D=0] 
#-------------------(-----------ATT----------) + (----Selection bias-----)

ATT=mean(student$actual_gpa[student$attended==1])-mean(student$potential_gpa[student$attended==1])

bias=mean(student$potential_gpa[student$attended==1])-mean(student$potential_gpa[student$attended==0])

obs_diff
ATT
bias

#Then, what can we do?

##Randomize!!!! -----------------------------------------------------------------
#If assignment and treatment is random, selection bias disappear:
# E[Y(0)|D=1] - E[Y(0)|D=0]= E[Y(0)] - E[Y(0)] = 0

treatment <- c("treated", "control")

student= student %>%
  mutate(
    group=sample(treatment, size = 100,replace=T),
    randomized_treatment=ifelse(group=="treated",1,0),
    randomized_gpa=ifelse(group=="treated",potential_gpa+rnorm(100,1,0.2),potential_gpa)
  )

#Now, we can estimate the effect:
experiment=feols(randomized_gpa~randomized_treatment,data=student)

etable(simple_ols,experiment)

#We recovered (almost) our treatment effect. How can we improve?

#Sample selection --------------------------------------------------------------

#Why did we not estimate our effect?

#Probably from noise. We should increase our sample size:

#https://egap.shinyapps.io/power-app/
treatment <- c("treated", "control")

experiment_data=data.frame(
  id_student=paste0("a",c(1:300)),
  potential_gpa=rnorm(300,7,1)) %>%
  mutate(
    potential_gpa=case_when(
      potential_gpa>10~10,
      potential_gpa<6~6,
      TRUE~potential_gpa),
    group=sample(treatment, size = 300,replace=T),
    randomized_treatment=ifelse(group=="treated",1,0),
    randomized_gpa=ifelse(group=="treated",potential_gpa+rnorm(300,1,0.2),potential_gpa)
  )

large_experiment=feols(randomized_gpa~randomized_treatment,data=experiment_data)

etable(simple_ols,experiment,large_experiment)

#Simple randomization with balance testing -------------------------------------
rm(list = ls())

#Let's simulate a population
ages=c(18:30)
woman=c(1,0)
income_decile=c(1:10)
public_high_school=c(1,1,1,0)
group=c("Treated","Control")
state=c(1:32)


data_experiment=data.frame(
  id=paste0("id_",1:5000),
  woman=sample(woman,size=5000,replace=T),
  income_decile=sample(income_decile,size=5000,replace=T),
  public_high_school=sample(public_high_school,size=5000,replace=T),
  group=sample(group,size=5000,replace=T)
  ) %>%
  mutate(
    treatment=ifelse(group=="Treated",1,0),
    potential_gpa=rnorm(5000,7,1),
    potential_gpa=case_when(
      potential_gpa>10~10,
      potential_gpa<6~6,
      TRUE~potential_gpa)
    )

#First thing: size. 
table(data_experiment$group)

data_experiment= data_experiment %>%
  mutate(
    randomizr_treatment=randomizr::complete_ra(5000),
    balanced_gpa=ifelse(randomizr_treatment==1,potential_gpa+rnorm(5000,1,0.2),potential_gpa),
    balanced_gpa=case_when(
      balanced_gpa>10~10,
      balanced_gpa<6~6,
      TRUE~balanced_gpa)
  )
table(data_experiment$randomizr_treatment)



#Then, we can use a regression to check whether the covariates are explained by the treatment

woman=feols(woman~randomizr_treatment,data=data_experiment)
income_decile=feols(income_decile~randomizr_treatment,data=data_experiment)
public_high_school=feols(public_high_school~randomizr_treatment,data=data_experiment)

etable(woman,income_decile,public_high_school)

#Or a t-test, either simple
t.test(woman ~ randomizr_treatment, data = data_experiment)
t.test(income_decile ~ randomizr_treatment, data = data_experiment)
t.test(public_high_school ~ randomizr_treatment, data = data_experiment)


#SUTVA !!! (Stable Unit of Treatment Value Assumption) -------------------------

#There are 3 sub-assumputions inside SUTVA:

##1. No spillovers--------------------------------------------------------------
#This means that the control group is not affected by 
friend_groups=paste0("g_",1:200)

balanced=feols(balanced_gpa~randomizr_treatment,data=data_experiment)

data_experiment = data_experiment %>%
  mutate(friend_group=sample(friend_groups,size=5000,replace=T)) %>%
  group_by(friend_group) %>%
  mutate(group_treated=sum(randomizr_treatment)) %>% 
  ungroup() %>%
  mutate(
    friend_gpa=balanced_gpa+0.5*group_treated,
    friend_gpa=case_when(
      friend_gpa>10~10,
      friend_gpa<6~6,
      TRUE~friend_gpa)
  )

no_spillovers=feols(balanced_gpa~randomizr_treatment,data=data_experiment)
spillovers=feols(friend_gpa~randomizr_treatment,data=data_experiment)
etable(no_spillovers,spillovers)


#How can we deal with this? assign treatement on a higher level (i.e. at the classroom or even school level)


##2. Single treatment dosage----------------------------------------------------
#Not varying quality, dosage, and delivery
#What if there are two classrooms: one gets two hours per week and the full treatment effect
#But the other gets only one and half treatment effect

hours_per_week=c(1,0.5)

data_experiment = data_experiment %>%
  mutate(
    hours=sample(hours_per_week,size=5000,replace=T),
    dosage_gpa=ifelse(
      randomizr_treatment==1,
      potential_gpa+hours_per_week*rnorm(5000,1,0.2),
      potential_gpa))
    

weird_dosage=feols(dosage_gpa~randomizr_treatment,data=data_experiment)
etable(weird_dosage,spillovers,no_spillovers)

