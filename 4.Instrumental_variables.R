# R for causal inference: Instrumental variables
# Material for Political Economy I at ITAM- Professor Horacio Larreguy
# Author: Luis Mario Ortiz Gutierrez

if (!require('pacman')) {install.packages('pacman')}

pacman::p_load(dplyr, #For data manipulation
               ggplot2,#For visualization
               this.path, #This allows us to get the path of the folder where this file is
               fixest, #This is for regressions (and 2SLS)
               randomizr, #For randomization
               purrr, #For simulations
               AER,
               ivDiag
               )

#We set our working directory using the previous package
setwd(dirname(this.dir()))

#Back to the mentoring program -------------------------------------------------

#The university now mandates a mentoring program for a random half of the freshmen.
#The assignment is random, but the university cannot force students to show up:
#  - Some assigned students never attend (never-takers)
#  - Some non-assigned students find a mentor anyway (always-takers)
#  - The rest attend only if they are assigned (compliers)

#Then, assignment (Z) is random, but attendance (D) is not.
#We will see that comparing attended vs. not attended fails for several reasons:
#  1. Omitted variable bias
#  2. Simultaneity
#  3. Measurement error
#And that the random assignment can be used as an instrument for all of them.

#Note: in this class we do not cap the GPA at 6 and 10 so that the true
#parameters are exactly recoverable.

##Let's simulate the data: -----------------------------------------------------
set.seed(1)

n=5000
effect=1.0

student=data.frame(
  id_student=paste0("a",c(1:n)),
  ability=rnorm(n,7,0.5)
  ) %>%
  mutate(
    gpa_hs=ability+rnorm(n,0,1), #A noisy measure of ability
    assigned=randomizr::complete_ra(n), #The random assignment (Z)
    #Compliance type depends on ability: weak students look for help anyway,
    #strong students do not bother showing up
    need=-(ability-7)+rnorm(n,0,0.5),
    type=case_when(
      need>quantile(need,0.85)~"always-taker",
      need<quantile(need,0.25)~"never-taker",
      TRUE~"complier"),
    attended=case_when(
      type=="always-taker"~1,
      type=="never-taker"~0,
      type=="complier"~assigned), #(D)
    potential_gpa=ability+rnorm(n,0,0.3), #Y(0)
    actual_gpa=potential_gpa+effect*attended #Y
  )

table(student$type)
table(student$assigned,student$attended)

#Omitted variable bias ---------------------------------------------------------

#What happens if we compare those who attended vs. those who did not?

simple_ols=feols(actual_gpa~attended,data=student)
etable(simple_ols)

#The effect is way below 1. Why? Attending depends on ability and ability affects GPA.
#Ability is in the error term:

#True model:  Y = a + b*D + g*Ability + e
#Short model: Y = a + b_short*D + u,     where u = g*Ability + e

#In a simulation we observe ability, so we can check the formula:
long_ols=feols(actual_gpa~attended+ability,data=student)

etable(simple_ols,long_ols)


##Can we control for a proxy? --------------------------------------------------
#In real life we do not observe ability, but we observe high-school GPA
proxy_ols=feols(actual_gpa~attended+gpa_hs,data=student)
etable(simple_ols,proxy_ols,long_ols)

#The bias shrinks but does not disappear: gpa_hs is ability measured with noise
#(we will come back to this in the measurement error section)

##The IV solution --------------------------------------------------------------
#The assignment is random, so it is unrelated to ability.
#It only affects GPA through attendance.

first_stage=feols(attended~assigned,data=student)

reduced_form=feols(actual_gpa~assigned,data=student)

#2SLS in fixest: outcome ~ exogenous controls | fixed effects | endogenous ~ instrument
iv=feols(actual_gpa~1|1|attended~assigned,data=student)

etable(first_stage,reduced_form,iv,fitstat="ivf")

etable(simple_ols,proxy_ols,iv,
       fitstat="ivf")


#Simultaneity ------------------------------------------------------------------

#Now think about hours of mentoring instead of attending or not.
#Assigned students have mandated sessions, but mentors also respond to grades:
#if the student is doing badly (low GPA), the mentor schedules more hours.

#Then there are two equations that hold at the same time:
#  GPA   = ability + tau*hours + e              (mentoring -> grades)
#  hours = 2 + 8*assigned - 1.5*(GPA-7) + u     (grades -> mentoring)

tau=0.15

#We solve the system to get GPA and hours as functions of exogenous stuff only
#(the reduced form):
#  GPA*(1+1.5*tau) = ability + tau*(2+8*assigned+1.5*7+u) + e

student=student %>%
  mutate(
    e=rnorm(n,0,0.5), #Shock to grades (a bad semester, being sick)
    u=rnorm(n,0,2), #Shock to mentoring (the mentor's availability)
    gpa_sim=(ability+tau*(2+8*assigned+1.5*7+u)+e)/(1+1.5*tau),
    hours=2+8*assigned-1.5*(gpa_sim-7)+u
  )

#Even if we control for ability (so there is no OVB), OLS is biased:
#a bad shock e lowers GPA -> the mentor gives more hours -> hours correlated with e
sim_ols=feols(gpa_sim~hours,data=student)
sim_ols_ability=feols(gpa_sim~hours+ability,data=student)
etable(sim_ols,sim_ols_ability)

#The assignment shifts hours but it is not affected by the grade shock
sim_iv=feols(gpa_sim~ability|hours~assigned,data=student)

etable(sim_ols,sim_ols_ability,sim_iv)
tau

#Let's see it:
ggplot(student,aes(x=hours,y=gpa_sim,color=factor(assigned)))+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=F)+
  labs(color="Assigned",y="GPA",x="Mentoring hours")+
  theme_minimal()

#Within each group the slope is negative (the mentor reacts to grades),
#but the jump between groups (driven by the random assignment) is positive.
#IV only uses the jump.

#Measurement error -------------------------------------------------------------

##1. Classical error in the treatment (hours) ----------------------------------
#Now, no simultaneity: hours depend only on the assignment and a random shock.
#But the hours come from a survey and students do not remember well.

student=student %>%
  mutate(
    true_hours=2+8*assigned+rnorm(n,0,2),
    gpa_me=ability+tau*true_hours+rnorm(n,0,0.5),
    reported_hours=true_hours+rnorm(n,0,4) #Classical error: mean 0, unrelated to everything
  )

me_true=feols(gpa_me~true_hours+ability,data=student)
me_reported=feols(gpa_me~reported_hours+ability,data=student)

etable(me_true,me_reported)


#IV: the assignment is correlated with the true hours but not with the recall error
me_iv=feols(gpa_me~ability|reported_hours~assigned,data=student)

etable(me_true,me_reported,me_iv)


#IV with non-compliance: the LATE ---------------------------------------------

#Now let the effect be different for each student:
#weaker students benefit more from the mentoring

student=student %>%
  mutate(
    effect_i=1+0.8*(7-ability)+rnorm(n,0,0.2),
    gpa_het=potential_gpa+effect_i*attended
  )

#The true parameters (only observable in a simulation):
ATE=mean(student$effect_i)
ATT=mean(student$effect_i[student$attended==1])
LATE=mean(student$effect_i[student$type=="complier"])

student %>%
  group_by(type) %>%
  summarise(effect=mean(effect_i),ability=mean(ability),share=n()/n)

##Step by step -----------------------------------------------------------------

#1. Intention to treat (reduced form): the effect of being ASSIGNED
itt=feols(gpa_het~assigned,data=student)

#2. First stage: the effect of being assigned on attending
first_stage=feols(attended~assigned,data=student)

etable(itt,first_stage)

#The ITT is diluted: many assigned students did not attend
#and some non-assigned attended.

#3. Wald estimator: ITT / first stage
wald=coef(itt)["assigned"]/coef(first_stage)["assigned"]
wald
LATE

##2SLS -------------------------------------------------------------------------
#With a binary instrument and no controls, 2SLS = Wald

iv_het=feols(gpa_het~1|1|attended~assigned,data=student)


#Why not doing the two stages by hand? The coefficient is the same, but the SE are wrong
student=student %>%
  mutate(attended_hat=fitted(first_stage))

manual_2sls=feols(gpa_het~attended_hat,data=student)
etable(iv_het,manual_2sls)

##Which effect is this? --------------------------------------------------------
data.frame(
  estimand=c("OLS","ATE","ATT","LATE","2SLS"),
  value=c(coef(feols(gpa_het~attended,data=student))["attended"],
          ATE,ATT,LATE,coef(iv_het)["fit_attended"])
)

#2SLS recovers the effect for COMPLIERS, not the ATE.
#Always-takers (weak students) have bigger effects, and never-takers smaller ones:
#we learn nothing about them from the instrument.


#Assumption testing -----------------------------------------------------------

##1. Independence (as-good-as-random assignment) ------------------------------
#We can test it the same way as in the experiments class: balance
balance_hs=feols(gpa_hs~assigned,data=student)
etable(balance_hs)

##2. Relevance (first stage) ---------------------------------------------------
#Rule of thumb: first stage F above 10 (Stock and Yogo), or 14 (Horacio), or 100 (un random) 
fitstat(iv_het,~ivf+ivwald)

##3. Exclusion restriction -----------------------------------------------------
#The assignment must affect GPA ONLY through attending.
#What if the letter announcing the mandate scares students and they study more,
#even if they never meet their mentor?

student=student %>%
  mutate(gpa_excl=gpa_het+0.3*assigned)

iv_excl=feols(gpa_excl~1|attended~assigned,data=student)
etable(iv_het,iv_excl)

#The bias is the direct effect divided by the first stage: small first stages amplify it
0.3/coef(first_stage)["assigned"]

#This assumption CANNOT be tested with the data. We need to argue it.

##4. Monotonicity (no defiers) -------------------------------------------------
#Some students may rebel: they would attend if NOT assigned, but refuse if assigned
#Suppose the strongest 20% of the compliers are actually defiers ("I don't need help")

student=student %>%
  group_by(type) %>%
  mutate(top_ability=ability>quantile(ability,0.8)) %>%
  ungroup() %>%
  mutate(
    type_mono=ifelse(type=="complier"&top_ability,"defier",type),
    attended_mono=case_when(
      type_mono=="always-taker"~1,
      type_mono=="never-taker"~0,
      type_mono=="complier"~assigned,
      type_mono=="defier"~1-assigned),
    gpa_mono=potential_gpa+effect_i*attended_mono
  )

iv_mono=feols(gpa_mono~1|attended_mono~assigned,data=student)
etable(iv_het,iv_mono)

