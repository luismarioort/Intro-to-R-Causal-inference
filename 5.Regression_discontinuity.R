# R for causal inference: Regression discontinuity
# Material for Political Economy I at ITAM- Professor Horacio Larreguy
# Author: Luis Mario Ortiz Gutierrez

if (!require('pacman')) {install.packages('pacman')}

pacman::p_load(dplyr, #For data manipulation
               ggplot2,#For visualization
               this.path, #This allows us to get the path of the folder where this file is
               fixest, #This is for regressions (and 2SLS)
               purrr, #For simulations and loops
               rdrobust, #Calonico, Cattaneo and Titiunik: estimation, optimal bandwidths and plots
               rddensity, #Cattaneo, Jansson and Ma: density (McCrary-type) test
               ggfixest)

#We set our working directory using the previous package
setwd(dirname(this.dir()))

#Back to the mentoring program (one last time) ---------------------------------

#This time the university does not randomize. It uses a rule:
#every freshman takes a diagnostic exam and those who score below 45
#are mandated to attend the mentoring program.

#Comparing mandated vs. non-mandated students is hopeless: students below 45 are weaker.
#But a student who scored 49.8 and one who scored 50.2 are basically the same student
#with a different draw on exam day. Near the cutoff, the rule is "as good as random".

#Jargon:
#  - Running variable (X): the exam score
#  - Cutoff (c): 50
#  - Sharp design: treatment is a deterministic function of X, D = 1(exam < 50)

#Note: as in the IV class, we do not cap the GPA (or the exam) so that
#the true parameters are exactly recoverable.

##Let's simulate the data: -----------------------------------------------------
set.seed(1)

n=10000
cutoff=45

student=data.frame(
  id_student=paste0("a",c(1:n)),
  ability=rnorm(n,7,0.5),
  public_hs=rbinom(n,1,0.5), #Went to a public high school
  female=rbinom(n,1,0.5)
  ) %>%
  mutate(
    gpa_hs=ability+rnorm(n,0,1), #Predetermined: it happened before the exam
    exam=50+20*(ability-7)+rnorm(n,0,8), #Ability plus luck on exam day
    #We center the running variable: x = points BELOW the cutoff.
    #This way treated students are on the right (x>=0), which is what rdrobust
    #assumes when it reports (right limit) - (left limit)
    x=cutoff-exam,
    mandated=as.numeric(x>=0), #(D)
    #Heterogeneous effects: bigger for students far below the cutoff
    #and for students from public high schools
    effect=0.8+0.4*public_hs+0.03*x,
    #Y(0) is a (mildly) nonlinear function of ability
    potential_gpa=ability+0.3*(ability-7)^2+rnorm(n,0,0.3), #Y(0)
    actual_gpa=potential_gpa+effect*mandated #Y
  )

mean(student$mandated)



#The naive comparison
naive=feols(actual_gpa~mandated,data=student,vcov="hetero")
etable(naive)

#Much smaller than the true effects (see below): mandated students are weaker,
#so the selection bias pulls the comparison down.

#Plots ------------------------------------------------------------------------

#RDD is a very visual method: if you cannot see the jump, you probably should not
#believe the regression.

##Raw data --------------------------------------------------------------------
ggplot(student,aes(x=x,y=actual_gpa,color=factor(mandated)))+
  geom_point(alpha=0.15,size=0.6)+
  geom_vline(xintercept=0,linetype="dashed")+
  labs(color="Mandated",x="Points below the cutoff",y="GPA")+
  theme_minimal()

#Too noisy. The standard is to plot means within bins of the running variable.

##Binned scatter by hand -------------------------------------------------------
#Bins of width 2. Bins should never mix observations from both sides of the cutoff:
#since the cutoff is at 0, floor() respects it
binned=student %>%
  mutate(bin=2*floor(x/2)+1) %>%
  group_by(bin,mandated) %>%
  summarise(gpa=mean(actual_gpa),obs=n(),.groups="drop")

ggplot()+
  geom_point(data=binned,aes(x=bin,y=gpa),color="darkblue")+
  geom_smooth(data=student,aes(x=x,y=actual_gpa,group=mandated),
              method="lm",formula=y~x,color="darkred",se=F)+
  geom_vline(xintercept=0,linetype="dashed")+
  labs(x="Points below the cutoff",y="GPA")+
  theme_minimal()

##rdplot -----------------------------------------------------------------------
#rdplot chooses the number of bins for us.
#  - "esmv" (default): evenly spaced bins that mimic the variance of the raw data
#  - "es": evenly spaced bins that minimize the IMSE (fewer bins, smoother picture)
#By default, the lines are 4th order global polynomials: fine for a picture,
#not for estimation (see the polynomial section)

?rdplot
rdplot(y=actual_gpa,x=x,c=0, data=student,
       x.label="Points below the cutoff",y.label="GPA",title="")

rdplot(y=actual_gpa,x=x,c=0, data=student,binselect="es",
       x.label="Points below the cutoff",y.label="GPA",title="")

#We can also restrict the plot to a window and use a linear fit
rdplot(y=student$actual_gpa[abs(student$x)<=15],x=student$x[abs(student$x)<=15],
       c=0,p=1,x.label="Points below the cutoff",y.label="GPA",title="")

#Estimation -------------------------------------------------------------------

#We never observe mandated and non-mandated students with the same score.
#We must EXTRAPOLATE each side to the cutoff and compare:
#  tau = lim(x->0+) E[Y|x] - lim(x->0-) E[Y|x] = E[Y(1)-Y(0)|x=0]
#This works if E[Y(0)|x] and E[Y(1)|x] are continuous at the cutoff.

##1. Global parametric ---------------------------------------------------------
#Y = a + tau*D + b1*x + b2*D*x + e
#Centering x at the cutoff makes tau the jump AT the cutoff
#The interaction allows different slopes on each side

global_lin=feols(actual_gpa~mandated*x,data=student,vcov="hetero")
global_quad=feols(actual_gpa~mandated*x+mandated*(x^2),data=student,vcov="hetero")

etable(naive,global_lin,global_quad,keep="^mandated$")

##2. Narrowing the window ------------------------------------------------------
#Closer to the cutoff, functional form matters less... but we have fewer observations

windows=c(30,20,10,5,2,1)

model_30=feols(actual_gpa~mandated*x,data=student %>% filter(abs(x)<=30),vcov="hetero")
model_20=feols(actual_gpa~mandated*x,data=student %>% filter(abs(x)<=20),vcov="hetero")
model_10=feols(actual_gpa~mandated*x,data=student %>% filter(abs(x)<=10),vcov="hetero")
model_5=feols(actual_gpa~mandated*x,data=student %>% filter(abs(x)<=5),vcov="hetero")
model_2=feols(actual_gpa~mandated*x,data=student %>% filter(abs(x)<=2),vcov="hetero")
model_1=feols(actual_gpa~mandated*x,data=student %>% filter(abs(x)<=1),vcov="hetero")

etable(model_30,model_20,model_10,model_5,model_2,model_1)

ggcoefplot(list(model_30,model_20,model_10,model_5,model_2,model_1),keep="mandated",drop="mandated:x")

#The bias-variance trade-off in one picture.

####3. Local linear regression --------------------------------------------------
#Instead of a hard window, weight the observations by their distance to the cutoff.
#Triangular kernel: weight = 1 - |x|/h inside the bandwidth, 0 outside

h=10

local_by_hand=feols(actual_gpa~mandated*x,
                    data=student %>% filter(abs(x)<=h) %>% mutate(w=1-abs(x)/h),
                    weights=~w,vcov="hetero")
etable(local_by_hand)

#rdrobust does the same (with h fixed by us)
local_rd=rdrobust(y=student$actual_gpa,x=student$x,c=0,h=h,kernel="triangular")

local_rd$coef[1]

#Same point estimate. The standard errors differ a bit: rdrobust uses
#a nearest-neighbor variance estimator by default.

##4. rdrobust with the optimal bandwidth ---------------------------------------
#Nobody should pick h=10 because it looks nice. Calonico, Cattaneo and Titiunik (2014):
#choose the h that minimizes the MSE of the estimator (bias^2 + variance)
rd=rdrobust(y=student$actual_gpa,x=student$x,c=0)
summary(rd)

#Three rows:
#  - Conventional: the local linear estimate with the usual CI.
#    The optimal h leaves some bias on purpose, so this CI is too narrow.
#  - Bias-corrected: subtracts an estimate of the bias (from a quadratic fit)
#  - Robust: bias-corrected AND accounts for the noise of estimating the bias. Report this CI.

rd$bws #h: bandwidth for the estimate; b: bandwidth used to estimate the bias
rd$N_h #Effective observations on each side

#LATE (again) -----------------------------------------------------------------

#What did we just estimate? The effect for students AT the cutoff.
#In a simulation we can compare it with everything else:

truth=student %>%
  summarise(LATE=mean(effect[abs(x)<1]),
            ATT=mean(effect[mandated==1]),
            ATE=mean(effect))
truth
rd$coef[1]

#The RDD recovers the effect at the cutoff, not the ATT nor the ATE.
#Students far below the cutoff benefit more, but we have no counterfactual for them
#without assuming a functional form (extrapolation).
#Like the LATE in IV: very credible, for a very specific population.

#Optimal bandwidth ------------------------------------------------------------

#rdbwselect shows the options:
#  - mserd: one MSE-optimal h for both sides (default)
#  - msetwo: a different h on each side
#  - cerrd: optimal for the coverage of the CI (smaller h)
summary(rdbwselect(y=student$actual_gpa,x=student$x,c=0,bwselect="mserd"))
summary(rdbwselect(y=student$actual_gpa,x=student$x,c=0,bwselect="msetwo"))
summary(rdbwselect(y=student$actual_gpa,x=student$x,c=0,bwselect="cerrd"))

##Sensitivity to the bandwidth ------------------------------------------------
#Always show that the result does not depend on the exact h: 0.5h, 0.75h, h, 1.5h, 2h
h_opt=rd$bws[1,1]

sensitivity=map_dfr(c(0.5,0.75,1,1.5,2),function(m){
  model=rdrobust(y=student$actual_gpa,x=student$x,c=0,h=m*h_opt)
  data.frame(multiplier=m,
             h=m*h_opt,
             estimate=model$coef[1],
             ci_low=model$ci[3,1], #Robust CI
             ci_high=model$ci[3,2],
             obs=sum(model$N_h))
})

sensitivity

ggplot(sensitivity,aes(x=h,y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=ci_low,ymax=ci_high),width=0.2)+
  geom_hline(yintercept=truth$at_cutoff,linetype="dashed",color="darkred")+
  geom_vline(xintercept=h_opt,linetype="dotted")+
  labs(x="Bandwidth",y="Estimate (robust 95% CI)")+
  theme_minimal()

##Sensitivity to the polynomial order ------------------------------------------
#Linear or quadratic, never more (Gelman and Imbens, 2019)
rd_p2=rdrobust(y=student$actual_gpa,x=student$x,c=0,p=2)
summary(rd_p2)

#Note that the optimal bandwidth grows with p: a more flexible fit can use data further away


#Assumption testing ----------------------------------------------------------

#Continuity of potential outcomes cannot be tested (we never see Y(0) for the mandated).
#But it has testable implications:
#  1. No bunching of the running variable at the cutoff (density test)
#  2. Predetermined covariates do not jump at the cutoff (balance)
#  3. No jumps where there is no cutoff (placebo cutoffs)

##1. Density test (McCrary; Cattaneo, Jansson and Ma) --------------------------
ggplot(student,aes(x=x,fill=factor(mandated)))+
  geom_histogram(binwidth=1,boundary=0,color="white")+
  geom_vline(xintercept=0,linetype="dashed")+
  labs(fill="Mandated",x="Points below the cutoff",y="Students")+
  theme_minimal()

density_test=rddensity(X=student$x,c=0)
summary(density_test)

rdplotdensity(density_test,student$x)

#The p-value is large: no evidence of sorting. Nobody manipulated the exam...yet.

##2. Covariate balance --------------------------------------------------------
#Run the RDD using each predetermined covariate as the outcome.
#We standardize them so the estimates are comparable.

covariates=c("gpa_hs","public_hs","female")

summary(rdrobust(y=gpa_hs,x=student$x,c=0,data=student))

summary(rdrobust(y=public_hs,x=student$x,c=0,data=student))

summary(rdrobust(y=female,x=student$x,c=0,data=student))


#Note: high school GPA is strongly correlated with the running variable
#(both depend on ability) and still does not jump. That is the whole point.
#Warning: with many covariates, some will "fail" by chance (5% of them at the 5% level).
#Try other seeds at the top of the script and you will see it.

##3. Placebo cutoffs (sample split) -------------------------------------------
#Pretend the cutoff is somewhere else. To avoid picking up the real jump,
#use only one side of the true cutoff:
#  - placebos to the left (x<0): only non-mandated students
#  - placebos to the right (x>=0): only mandated students

placebo_under=student %>% filter(x<0)
placebo_over=student %>% filter(x>=0)

summary(rdrobust(y=actual_gpa,x=x,c=-15,data=placebo_under))
summary(rdrobust(y=actual_gpa,x=x,c=-10,data=placebo_under))
summary(rdrobust(y=actual_gpa,x=x,c=-5,data=placebo_under))
summary(rdrobust(y=actual_gpa,x=x,c=5,data=placebo_over))
summary(rdrobust(y=actual_gpa,x=x,c=10,data=placebo_over))
summary(rdrobust(y=actual_gpa,x=x,c=15,data=placebo_over))


##What does manipulation look like? --------------------------------------------
#Next year, students learn the rule. Most of the strong students who fell just below
#the cutoff (up to 5 points) complain and the professor regrades their exam "generously":
#they end up just above 45 and avoid the mentoring.

#When can this happen? The rule is known, students want to avoid it, and they can
#(partially) control their score. Compare: nobody can manipulate the rainfall in FONDEN.

set.seed(2)

manipulated=student %>%
  mutate(
    regraded=x>=0 & x<5 & ability>6.9 & runif(n)<0.9,
    x=ifelse(regraded,-runif(n,0,1),x), #They end up just above the cutoff
    mandated=as.numeric(x>=0),
    actual_gpa=potential_gpa+effect*mandated
  )

ggplot(manipulated,aes(x=x,fill=factor(mandated)))+
  geom_histogram(binwidth=1,boundary=0,color="white")+
  geom_vline(xintercept=0,linetype="dashed")+
  labs(fill="Mandated",x="Points below the cutoff",y="Students")+
  theme_minimal()

summary(rddensity(X=manipulated$x,c=0))

#Balance fails too: the students who moved are not a random sample
summary(rdrobust(y=manipulated$gpa_hs,x=manipulated$x,c=0))

#And the estimate is biased
rd_manipulated=rdrobust(y=manipulated$actual_gpa,x=manipulated$x,c=0)
rd_manipulated$coef[1]
truth$LATE

#A common patch is the "donut" RDD: drop the observations closest to the cutoff.
#A small donut only removes the heap...
donut_1=manipulated %>% filter(abs(x)>1)
summary(rdrobust(y=donut_1$actual_gpa,x=donut_1$x,c=0))

#...but the strong students also disappeared from the mandated side (0<=x<5).
#The donut must cover the whole region where sorting happened:
donut_5=manipulated %>% filter(abs(x)>5)
summary(rdrobust(y=donut_5$actual_gpa,x=donut_5$x,c=0))

#It works here because we KNOW who moved. In real data we do not, and a bigger donut
#means extrapolating from further away (more functional form, a different population).

#Heterogeneous effects (sample split) ------------------------------------------

#Does the program work better for students from public high schools?
#Estimate the RDD separately in each subsample
rd_public=rdrobust(y=student$actual_gpa[student$public_hs==1],x=student$x[student$public_hs==1],c=0)
rd_private=rdrobust(y=student$actual_gpa[student$public_hs==0],x=student$x[student$public_hs==0],c=0)

summary(rd_public)
summary(rd_private)

binned=student %>%
  mutate(bin=2*floor(x/2)+1) %>%
  group_by(bin,mandated,public_hs) %>%
  summarise(gpa=mean(actual_gpa),obs=n(),.groups="drop") 

student=student %>%
  mutate(group_mandated=paste0(mandated,public_hs))

ggplot()+
  geom_point(data=binned,aes(x=bin,y=gpa,color=public_hs))+
  geom_smooth(data=student,aes(x=x,y=actual_gpa,group=group_mandated),
              method="lm",formula=y~x,color="darkred",se=F)+
  geom_vline(xintercept=0,linetype="dashed")+
  labs(x="Points below the cutoff",y="GPA")+
  theme_minimal()

#Two warnings:
#  1. Each subsample gets its own optimal bandwidth, so we compare slightly different
#     populations. You can fix h (e.g., the pooled h_opt) in both.
#  2. Only split by pre treatment variables (and check balance within each subsample)

#Fuzzy RDD -------------------------------------------------------------------

#As in the IV class, the university cannot force students to show up:
#  - Some mandated students never attend (never-takers, more likely strong students)
#  - Some non-mandated students ask for a mentor anyway (always-takers, weak students)
#Now the cutoff changes the PROBABILITY of attending, but not from 0 to 1.

set.seed(1)

student=student %>%
  mutate(
    need=-(ability-7)+rnorm(n,0,0.5),
    type=case_when(
      need>quantile(need,0.85)~"always-taker",
      need<quantile(need,0.25)~"never-taker",
      TRUE~"complier"),
    attended=case_when(
      type=="always-taker"~1,
      type=="never-taker"~0,
      type=="complier"~mandated),
    gpa_fuzzy=potential_gpa+effect*attended
  )

#The sharp RDD on attendance now fails: attendance is a choice
table(student$mandated,student$attended)

##First stage: the jump in the probability of attending -----------------------
rdplot(y=student$attended,x=student$x,c=0,
       x.label="Points below the cutoff",y.label="Attended mentoring",title="")

##The fuzzy RDD is an IV --------------------------------------------------------
#Instrument: Z = 1(x>=0). Treatment: attended.
#  tau_FRD = (jump in Y at the cutoff) / (jump in D at the cutoff)
#Estimates the LATE for compliers AT the cutoff

rd_fuzzy=rdrobust(y=student$gpa_fuzzy,x=student$x,c=0,fuzzy=student$attended)
summary(rd_fuzzy)


#The same as 2SLS inside the bandwidth, with the triangular weights.
#The running variable (and its interaction with Z) are exogenous controls
fuzzy_2sls=feols(gpa_fuzzy~x+x:mandated|attended~mandated,
                 data=student %>% filter(abs(x)<=h_fuzzy) %>% mutate(w=1-abs(x)/h_fuzzy),
                 weights=~w,vcov="hetero")

etable(fuzzy_2sls,fitstat="ivf")

#Compared with the truth:
student %>%
  filter(abs(x)<2) %>%
  group_by(type) %>%
  summarise(effect=mean(effect),students=n())

#All the IV assumptions apply locally: relevance (the first stage jumps),
#exclusion (crossing 50 only matters through attending), and monotonicity
#(nobody attends BECAUSE they were not mandated).

