#Class 3 -------------------------------

# R for causal inference: Difference-in-differences
# Material for Political Economy I at ITAM- Professor Horacio Larreguy
# Author: Luis Mario Ortiz Gutierrez

if (!require('pacman')) {install.packages('pacman')}
if (!require('did_multiplegt_dyn')) {install.packages("DIDmultiplegtDYN")}

pacman::p_load(dplyr, #For data manipulation
               ggplot2,#For visualization
               this.path, #This allows us to get the path of the folder where this file is
               fixest, #This is for regressions
               purrr,
               panelView,
               didimputation, #Borusyak et al.
               did, #Callaway and Sant'Anna
               DIDmultiplegtDYN,
               polars
               )

#What happens when experiments are not viable
#Are we doomed? Can we still estimate causal treatment effects?
#Yes! under certain assumptions

#Let's go back to schooling simulations
set.seed(1)

id_student=paste0("a",1:100)
semester=1:8

school_panel <- expand.grid(
  id_student=as.character(id_student),
  semester=semester
)

student=data.frame(
  id_student=paste0("a",c(1:100)),
  major=rdunif(100, b=4, a = 1))  %>%
  mutate(
    ability=rnorm(100,8,0.5),
    gpa_hs=ability+rnorm(100,0,1),
    gpa_hs=case_when(
      gpa_hs>10~10,
      gpa_hs<6~6,
      TRUE~gpa_hs)
  )


school_panel=left_join(school_panel, student, by="id_student") 

school_panel=school_panel %>%
  mutate(
    difficulty=(semester)/2,
    potential_gpa=0.9*ability-0.3*difficulty+rnorm(800,0,0.1),
    potential_gpa=case_when(
      potential_gpa>10~10,
      potential_gpa<5~5,
      TRUE~potential_gpa)
  )

#Now, lets suppose that, 4th semester, if they have gpa<7, they take the course 

effect=1.0

school_panel=school_panel %>%
  group_by(id_student) %>%
  mutate(
    attended=ifelse(potential_gpa[semester==4]<7,1,0),
  ) %>%
  ungroup()
  
school_panel=school_panel %>%  
  mutate(
    actual_gpa=ifelse(attended==1&semester>=5,potential_gpa+effect,potential_gpa),
    post=ifelse(semester>=5,1,0),
    time_to_treatment=semester-5,
    attended_post=attended*post
  )

#Aggainst, what happens if we run a regression?

simple_ols=feols(
  actual_gpa~attended_post,
  cluster="id_student",
  data=school_panel
)

etable(simple_ols)


#Why?

figure_data=school_panel %>%
  group_by(attended,semester) %>% 
  summarise(gpa=mean(actual_gpa),
            potential_pa=mean(potential_gpa)) %>%
  ungroup()

ggplot(data=figure_data,aes(x=semester, y=gpa,color=factor(attended),group=factor(attended))) +
  geom_line() +
  geom_line(aes(x=semester, y=potential_pa,color=factor(attended),group=factor(attended)),linetype="dashed") +
  theme_minimal()

#Here, we can see the logic of diff-in-diffs

#Difference 1

diff1=mean(school_panel$actual_gpa[school_panel$attended==1&school_panel$post==1])-
  mean(school_panel$actual_gpa[school_panel$attended==1&school_panel$post==0])
diff1

#Difference 2
diff2=mean(school_panel$actual_gpa[school_panel$attended==0&school_panel$post==1])-
  mean(school_panel$actual_gpa[school_panel$attended==0&school_panel$post==0])
diff2

#Difference-in-differences
diff1-diff2

#how do we formally test this?

#Event study plots

#Y_{i,t}=\alpha_i+\delta_t+\sum_{t!=-1}(\beta_t (Treated_i*year_t))

event_study=feols(
  actual_gpa~i(time_to_treatment,attended,ref=-1)|id_student+semester,
  cluster="id_student",
  data=school_panel
)

iplot(event_study)


#We show there are no anticipation effects and we assume there would have been parallell trends

#then, we can estimate the att:

#Either with fixed effects
att=feols(
  actual_gpa~attended*post|id_student+semester,
  cluster="id_student",
  data=school_panel
)

#Or just the dummies
att_dummies=feols(
  actual_gpa~attended*post,
  cluster="id_student",
  data=school_panel
)

etable(att_dummies)


#If you had taken this 10 years ago, the course would have ended here

#Now, let's suppose we have staggered adoption

school_panel=school_panel %>%
  group_by(id_student) %>%
  mutate(
  semester_treatment=ifelse(
    sum(attended)==0,0,
    rdunif(100,2,8))
  ) %>%
  ungroup()


school_panel=school_panel %>%
  mutate(
    post_staggered=ifelse(
      attended==1&semester>=semester_treatment,1,0
    ),
    time_to_treatment=ifelse(attended==1,semester-semester_treatment,0),
    actual_gpa_stagg=ifelse(post_staggered==1,potential_gpa+effect,potential_gpa)
  )

#We can visualize the treatment:
?panelview


panelview(
  data=school_panel,
  Y="actual_gpa_stagg",
  D="post_staggered",
  index=c("id_student","semester")
)

figure_data=school_panel %>%
  group_by(semester_treatment,semester) %>% 
  summarise(gpa=mean(actual_gpa_stagg),
            potential_pa=mean(potential_gpa)) %>%
  ungroup()

ggplot(data=figure_data,aes(x=semester, y=gpa,color=factor(semester_treatment),group=factor(semester_treatment))) +
  geom_line() +
  geom_line(aes(x=semester, y=potential_pa,color=factor(semester_treatment),group=factor(semester_treatment)),linetype="dashed") +
  theme_minimal()



#Then, we run the event study

event_study_staggered=feols(
  actual_gpa_stagg~i(time_to_treatment,attended,ref=-1)|id_student+semester,
  cluster="id_student",
  data=school_panel
)

iplot(event_study_staggered)

#And the ATT?

att_staggered=feols(
  actual_gpa_stagg~attended*post_staggered|id_student+semester,
  cluster="id_student",
  data=school_panel
)

etable(att_staggered)


#Everything is ok, because TWFE estimates a ponderate sum of effects, with weights that sum 1

#What happens when effects become heterogeneous

#Say, the classes become more efficient


school_panel=school_panel %>%
  mutate(
    effect_het=ifelse(time_to_treatment>=0&attended==1,0.05+time_to_treatment^2/5,0),
    actual_gpa_stag_het=potential_gpa+effect_het,
    actual_gpa_stag_het=case_when(
      actual_gpa_stag_het>10~10,
      actual_gpa_stag_het<5~5,
      TRUE~actual_gpa_stag_het)
  )

#Now this is our average treatment effect:

#ATE
mean(school_panel$effect_het)

#ATT
true_effect_staggered=mean(school_panel$effect_het[school_panel$post_staggered==1])


figure_data=school_panel %>%
  group_by(semester_treatment,semester) %>% 
  summarise(gpa=mean(actual_gpa_stag_het),
            potential_pa=mean(potential_gpa)) %>%
  ungroup()

ggplot(data=figure_data,aes(x=semester, y=gpa,color=factor(semester_treatment),group=factor(semester_treatment))) +
  geom_line() +
  geom_line(aes(x=semester, y=potential_pa,color=factor(semester_treatment),group=factor(semester_treatment)),linetype="dashed") +
  theme_minimal()

event_study_stag_het=feols(
  actual_gpa_stag_het~i(time_to_treatment,attended,ref=-1)|id_student+semester,
  cluster="id_student",
  data=school_panel
)

iplot(event_study_stag_het)
  
att_stagg_het=feols(
  actual_gpa_stag_het~attended*post_staggered|id_student+semester,
  cluster="id_student",
  data=school_panel
)

etable(att_stagg_het)

#Now att is biased


#Another possible issue: if there are not never treated units

school_panel_filtered=school_panel %>%
  filter(attended==1)

figure_data=school_panel_filtered %>%
  group_by(semester_treatment,semester) %>% 
  summarise(gpa=mean(actual_gpa_stag_het),
            potential_pa=mean(potential_gpa)) %>%
  ungroup()

ggplot(data=figure_data,aes(x=semester, y=gpa,color=factor(semester_treatment),group=factor(semester_treatment))) +
  geom_line() +
  geom_line(aes(x=semester, y=potential_pa,color=factor(semester_treatment),group=factor(semester_treatment)),linetype="dashed") +
  theme_minimal()

event_study_stag_het_filt=feols(
  actual_gpa_stag_het~i(time_to_treatment,attended,ref=-1)|id_student+semester,
  cluster="id_student",
  data=school_panel_filtered
)

iplot(event_study_stag_het)

att_stagg_het_filt=feols(
  actual_gpa_stag_het~attended*post_staggered|id_student+semester,
  cluster="id_student",
  data=school_panel_filtered
)

#All of our models
etable(simple_ols,att,att_stagg_het_filt,att_staggered,att_stagg_het)


true_effect_staggered=mean(school_panel_filtered$actual_gpa_stag_het-school_panel_filtered$potential_gpa)

true_effect_staggered

true_effects=school_panel_filtered %>%
  group_by(time_to_treatment) %>%
  summarise(att_t=mean(actual_gpa_stag_het)-mean(potential_gpa))

#As you saw in class, there are multiple ways to solve this

#Borusyak, Jaravel, Spiess (2024). 

?did_imputation

#ATT
att_borusyak=did_imputation(
  data=school_panel,
  yname="actual_gpa_stag_het",
  gname="semester_treatment",
  idname="id_student",
  tname="semester",
  cluster_var = "id_student"
)

att_borusyak

#Event study
es_borusyak=did_imputation(
  data=school_panel,
  yname="actual_gpa_stag_het",
  gname="semester_treatment",
  idname="id_student",
  tname="semester",
  cluster_var = "id_student",
  horizon=TRUE,
  pretrends = TRUE
)

ggplot(data=es_borusyak) +
  geom_point(aes(x=as.numeric(term),y=estimate)) +
  geom_errorbar(aes(x=as.numeric(term),ymin = conf.low  , ymax = conf.high),width=0.1) +
  geom_vline(aes(xintercept=-1),linetype = "dashed") +
  geom_hline(aes(yintercept=0)) +
  theme_bw()

att_borusyak_filt=did_imputation(
  data=school_panel_filtered,
  yname="actual_gpa_stag_het",
  gname="semester_treatment",
  idname="id_student",
  tname="semester",
  cluster_var = "id_student"
)
att_borusyak_filt

es_borusyak_filt=did_imputation(
  data=school_panel_filtered,
  yname="actual_gpa_stag_het",
  gname="semester_treatment",
  idname="id_student",
  tname="semester",
  cluster_var = "id_student",
  horizon=TRUE,
  pretrends = TRUE
)

ggplot(data=es_borusyak_filt) +
  geom_point(aes(x=as.numeric(term),y=estimate)) +
  geom_errorbar(aes(x=as.numeric(term),ymin = conf.low  , ymax = conf.high),width=0.1) +
  geom_vline(aes(xintercept=-1),linetype = "dashed") +
  geom_hline(aes(yintercept=0)) +
  theme_bw()


#Callaway & Sant'anna
?att_gt

school_panel=school_panel %>%
  mutate(
    id_callaway=as.numeric(substring(id_student,2))
    )

cs <- att_gt(
  yname = "actual_gpa_stag_het",
  tname = "semester",
  idname = "id_callaway",
  gname = "semester_treatment",
  data = school_panel
)
aggte(cs, type = "simple")

es_cs=aggte(cs, type = "dynamic")
ggdid(es_cs)

school_panel_filtered=school_panel_filtered %>%
  mutate(
    id_callaway=as.numeric(substring(id_student,2))
  )

cs_filtered <- att_gt(
  yname = "actual_gpa_stag_het",
  tname = "semester",
  idname = "id_callaway",
  gname = "semester_treatment",
  data = school_panel_filtered
)
aggte(cs_filtered, type = "simple")


es_cs_filt=aggte(cs_filtered, type = "dynamic")
ggdid(es_cs_filt)


#Chaisemartin and d'Haultfoeuille
?did_multiplegt_dyn

dcdh=did_multiplegt_dyn(
  df=school_panel,
  outcome="actual_gpa_stag_het",
  time="semester",
  group="id_callaway",
  treatment="post_staggered",
  effects=10,
  placebo=10
)

summary(dcdh)
