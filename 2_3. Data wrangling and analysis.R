#############General infos################
##Author: Clément ANNE
##Author e-mail: clement.anne90@gmail.com
##Date: March 08,2022
##########################################

################Outline###################
##
##2. Data wrangling
##  2.0. Preliminary steps
##  2.1. Fixing NAs
##    2.1.1. Rider infos
##    2.1.2. Race infos
##    2.1.3. Computing recent stats
##  2.2. Data aggregation and wrangling
##    2.2.1. Variable names
##    2.2.2. wrangling function
##
##3. Analysis
##  3.1. Data analysis
##  3.2. Loop over windows
##  3.3. Final models
##  
##########################################


#######################################################
#                                                     #
#         Cycling predictions using ML                #
#                       -                             #
#               2. Data wrangling                     #
#                                                     #
#######################################################


##################################################
#                                                #
#            2.0. Preliminary steps              #
#                                                #
##################################################

#Clear current environment
rm(list=ls())

###Libraries
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")


library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
library(caret)
library(knitr)
library(tinytex)


###Working directory
wd <- getwd()

###Load datasets
load(file.path("rda","race_infos.rda"))
load(file.path("rda","point_scales.rda"))
load(file.path("rda","results.rda"))
load(file.path("rda","rider_infos.rda"))
load(file.path("rda","rider_past_results.rda"))

###Install tinytex if needed
if(is_tinytex()==FALSE) tinytex::install_tinytex()

##################################################
#                                                #
#               2.1. Fixing NAs                  #
#                                                #
##################################################

#############################################
#          2.1.1. Rider infos               #
#############################################

summary(dtb_rider_infos_clean)

dtb_rider_infos_clean_woNAs <- dtb_rider_infos_clean%>%
  mutate_at(c("Height","Weight"),as.numeric)%>%
  mutate(mean_height=mean(Height,na.rm=TRUE))%>%
  mutate(mean_weight=mean(Weight,na.rm=TRUE))%>%
  mutate(Height=ifelse(is.na(Height),mean_height,Height))%>%
  mutate(Weight=ifelse(is.na(Weight),mean_weight,Weight))%>%
  select(-mean_weight,-mean_height)

rm(dtb_rider_infos_clean)

#############################################
#          2.1.2. Race infos                #
#############################################


race_infos_2014_to_2021_clean%>%
  mutate_all(is.na)%>%
  summarize_all(sum)%>%
  t()

####Replace race infos
#1. Replace by race_id level means
#-ProfileScore: 29 NAs
#-Vert meters: 33 NAs
#-race profile: 22 NAs

###Average features by race ID
mean_race_features <- race_infos_2014_to_2021_clean%>%
  select(ProfileScore,Vert_meters,race_profile,race_id,year)%>%
  group_by(race_id)%>%
  mutate_at(c("ProfileScore","Vert_meters"),mean,na.rm=TRUE)%>%
  ungroup()%>%
  select(race_id,ProfileScore,Vert_meters)%>%
  filter(!is.na(ProfileScore) & !is.na(Vert_meters))%>%
  rename(ProfileScore_common=ProfileScore)%>%
  rename(Vert_meters_common=Vert_meters)%>%
  distinct()

###Most common race profile by race ID
most_common_race_profile <- race_infos_2014_to_2021_clean%>%
  filter(!is.na(race_profile))%>%
  group_by(race_id,race_profile)%>%
  mutate(count_race_profile=n())%>%
  ungroup()%>%
  group_by(race_id)%>%
  mutate(most_common_race_profile_N=max(count_race_profile))%>%
  filter(most_common_race_profile_N==count_race_profile)%>%
  ungroup()%>%
  select(race_id,race_profile)%>%
  rename(race_profile_common=race_profile)%>%
  distinct()

#Temp dtb
t <- race_infos_2014_to_2021_clean%>%
  left_join(mean_race_features,by="race_id")%>%
  left_join(most_common_race_profile,by="race_id")%>%
  mutate(race_profile=ifelse(is.na(race_profile),race_profile_common,race_profile))%>%
  mutate(Vert_meters=ifelse(is.na(Vert_meters),Vert_meters_common,Vert_meters))%>%
  mutate(ProfileScore=ifelse(is.na(ProfileScore),ProfileScore_common,ProfileScore))

###Special fix for the London Classic
#-No info on Vert meters
#-Info only once for the Profile Score (25)
#-Consistent with the race profile (flat)
#-We will estimate the average Vert Meter through loess
###

#Average profile score for the London classic
mean_ProfileScore_London <- t%>%
  filter(race_id=="ride-london-classic")%>%
  summarize(mean(ProfileScore,na.rm = TRUE))%>%
  pull()

#Graph  
t%>%
  filter(race_id!="ride-london-classic")%>%
  ggplot(aes(ProfileScore,Vert_meters))+
  geom_point()+
  geom_smooth(method=loess)

#Loess estimate
fit_Vert_meters <- with(t,loess(Vert_meters~ProfileScore))
mean_Vert_meters_London <- predict(fit_Vert_meters,mean_ProfileScore_London)

#Database without NAs
race_infos_2014_to_2021_clean_woNAs <- t%>%
  mutate(Vert_meters=ifelse(race_id=="ride-london-classic",as.numeric(mean_Vert_meters_London),Vert_meters))%>%
  mutate(ProfileScore=ifelse(race_id=="ride-london-classic",as.numeric(mean_ProfileScore_London),ProfileScore))%>%
  select(-ProfileScore_common,-race_profile_common,-Vert_meters_common)

#Remove old or temporay files
rm(t,most_common_race_profile,mean_race_features,fit_Vert_meters,
   mean_ProfileScore_London,mean_Vert_meters_London)
rm(race_infos_2014_to_2021_clean)

#############################################
#      2.1.3. Computing recent stats        #
#############################################

###In the long term this section will be back in the first script file

###
# -A former version used mean stats
#   -Those variables are removed for the time being
###

###Proccess stats from past races for a given rider
my_wrangle_rider_past_results <- function(race,y,r){
  
  race_date <- race_infos_2014_to_2021_clean_woNAs%>%
    filter(race_id==race & year==as.character(y))%>%
    pull(Date)
  
  past_results <- dtb_past_results%>%
    filter(rider_id==r & Date<race_date)
  
  before_race_date <- race_date-1
  
  form_window <- seq(2,8,1) #Form window (tunable)
  status_window <- seq(1,5,1) #Status window (tunable)
  
  ####Recent form (2-8w)
  ##Race days
  rider_form1 <-map_dfc(1:length(form_window),function(i){
    past_results%>%
      filter(Date>(before_race_date-dweeks(form_window[i])))%>%
      filter(!is.na(Distance))%>% #Remove annex/general classification rows
      summarize(N_form_days=n())%>%
      rename_all(paste0,"_fw","_",as.character(form_window[i]),"w")  
  })

  ##PCS points earned
  rider_form2 <-map_dfc(1:length(form_window),function(i){
    past_results%>%
      filter(Date>(before_race_date-dweeks(form_window[i])))%>%
      summarize(sum_form_PCS=sum(PointsPCS))%>%
      rename_all(paste0,"_fw","_",as.character(form_window[i]),"w")  
  })
  
  
  ####Overall status(1-35y)
  ##Race days
  rider_status1 <- map_dfc(1:length(status_window),function(i){
    past_results%>%
      filter(Date>(before_race_date-dweeks(52*status_window[i]+2)))%>%
      filter(!is.na(Distance))%>% #Remove annex/general classification rows
      summarize(N_status_days=n())%>%
      rename_all(paste0,"_sw","_",as.character(status_window[i]),"y") 
  })
  ##PCS points earned
  rider_status2 <- map_dfc(1:length(status_window),function(i){
    past_results%>%
      filter(Date>(before_race_date-dweeks(52*status_window[i]+2)))%>%
      summarize(sum_status_PCS=sum(PointsPCS))%>%
      rename_all(paste0,"_sw","_",as.character(status_window[i]),"y") 
  })
  
  
  ####1-day WT status(1-5y)
  rider_1dayWT_status <- map_dfc(1:length(status_window),function(i){
    past_results%>%
      filter(race_id%in%race_infos_2014_to_2021_clean_woNAs$race_id)%>%
      filter(Date>(before_race_date-dweeks(52*status_window[i]+2)))%>%
      summarize(N_status_1dayWT=n(),
                sum_status_1dayWT_PCS=sum(PointsPCS))%>%
      rename_all(paste0,"_sw","_",as.character(status_window[i]),"y") 
  })
  
  ####1-day-WT career
  rider_1dayWT_career <- past_results%>%
    filter(race_id%in%race_infos_2014_to_2021_clean_woNAs$race_id)%>%
    summarize(N_career_1dayWT=n(),
              sum_career_1dayWT_PCS=sum(PointsPCS))  
  
  ####Career exp
  rider_career1 <- past_results%>%
    filter(!is.na(Distance))%>% #Remove annex/general classification rows
    summarize(N_career_days=n())
  rider_career2<- past_results%>%
    summarize(sum_career_PCS=sum(PointsPCS))
  
  ####Race career exp
  race_career <- past_results%>%
    filter(race_id==race)%>%
    summarize(N_race=n(),
              sum_race_PCS=sum(PointsPCS))
  
  ####Race status exp
  race_status <- map_dfc(1:length(status_window),function(i){
    past_results%>%
      filter(race_id==race)%>%
      filter(Date>(before_race_date-dweeks(52*status_window[i]+2)))%>%
      summarize(N_race_status=n(),
                sum_race_status_PCS=sum(PointsPCS))%>%
      rename_all(paste0,"_sw","_",as.character(status_window[i]),"y") 
    
  })
  
  
  bind_cols(rider_form1,rider_form2,
            rider_status1,rider_status2,
            rider_1dayWT_status,rider_1dayWT_career,
            rider_career1,rider_career2,
            race_career,race_status)
}

#Examples:
my_wrangle_rider_past_results("paris-roubaix",2019,"peter-sagan")%>%
  t()
my_wrangle_rider_past_results("paris-roubaix",2014,"peter-sagan")%>%
  t()
my_wrangle_rider_past_results("e3-harelbeke",2016,"michael-schar")%>%
  t()

#Warning: Above function only works for the current 2014-2021 studied time span

######Database aggregating all stats for riders before each race
###-Beware it takes 2-3h to run

dtb_recent_stats2 <- map_dfr(1:nrow(dtb_results_2014_to_2021),function(i){
  i_pct <- i/nrow(dtb_results_2014_to_2021)*100
  print(paste("Computation over the",i,"th observation (",round(i_pct,1),"% completion)"))
  rider <- dtb_results_2014_to_2021$rider_id[i]
  race <- dtb_results_2014_to_2021$race_id[i]
  year <- dtb_results_2014_to_2021$year[i]
  my_wrangle_rider_past_results(race,year,rider)
})

save(dtb_recent_stats2,file=file.path("rda","rider_ability_stats2.rda"))

summary(dtb_recent_stats2)
    
##################################################
#                                                #
#    2.2. Data aggregation and wrangling         #
#                                                #
##################################################

###Load in case we do not want to run the previous part
load(file.path("rda","rider_ability_stats2.rda"))

####
#-The wrangling function will prepare a unique database
#  depending on the status and form windows
#-It is sub-optimal but reduces computation time
# instead of computing stats again for any window
####

#############################################
#         2.2.1. Variable names             #
#############################################


var_names <- variable.names(dtb_recent_stats2)

###Career strength vars (9 vars)
career_strength_vars <- var_names[str_detect(var_names,"\\_1?[a-zA-Z]{2,}$")==TRUE]

###Form strength vars (3 vars)
form_strength_vars_2w<- var_names[str_detect(var_names,"\\_2w$")==TRUE]
form_strength_vars_3w<- var_names[str_detect(var_names,"\\_3w$")==TRUE]
form_strength_vars_4w<- var_names[str_detect(var_names,"\\_4w$")==TRUE]
form_strength_vars_5w<- var_names[str_detect(var_names,"\\_5w$")==TRUE]
form_strength_vars_6w<- var_names[str_detect(var_names,"\\_6w$")==TRUE]
form_strength_vars_7w<- var_names[str_detect(var_names,"\\_7w$")==TRUE]
form_strength_vars_8w<- var_names[str_detect(var_names,"\\_8w$")==TRUE]

###Status strength vars (9 vars)
status_strength_vars_1y<- var_names[str_detect(var_names,"\\_1y$")==TRUE]
status_strength_vars_2y<- var_names[str_detect(var_names,"\\_2y$")==TRUE]
status_strength_vars_3y<- var_names[str_detect(var_names,"\\_3y$")==TRUE]
status_strength_vars_4y<- var_names[str_detect(var_names,"\\_4y$")==TRUE]
status_strength_vars_5y<- var_names[str_detect(var_names,"\\_5y$")==TRUE]

#############################################
#         2.2.2. wrangling function         #
#############################################

###Function available only for form window [2-8w]
# and status window [1-5y]
my_wrangle_dataset <- function(f,s){
  
  form_window <- f
  status_window <- s
  
  form_strength_vars <- case_when(form_window==2 ~ form_strength_vars_2w,
                                  form_window==3 ~ form_strength_vars_3w,
                                  form_window==4 ~ form_strength_vars_4w,
                                  form_window==5 ~ form_strength_vars_5w,
                                  form_window==6 ~ form_strength_vars_6w,
                                  form_window==7 ~ form_strength_vars_7w,
                                  form_window==8 ~ form_strength_vars_8w,)
  
  status_strength_vars <- case_when(status_window==1 ~ status_strength_vars_1y,
                                    status_window==2 ~ status_strength_vars_2y,
                                    status_window==3 ~ status_strength_vars_3y,
                                    status_window==4 ~ status_strength_vars_4y,
                                    status_window==5 ~ status_strength_vars_5y)
  
  
  ###########
  #   Step 1: Data aggregation
  ###########
  
  
  ###Here we make a temporary choice 4w form window and 3y status window
  ### Will be relaxed and tuned later on
  
  ####Merging datasets
  Merged_dtb <- dtb_results_2014_to_2021%>%
    bind_cols(dtb_recent_stats2%>%select(all_of(career_strength_vars)))%>%
    bind_cols(dtb_recent_stats2%>%select(all_of(form_strength_vars)))%>%
    bind_cols(dtb_recent_stats2%>%select(all_of(status_strength_vars)))%>%
    left_join(dtb_rider_infos_clean_woNAs,by="rider_id")%>% #Rider infos dataset
    left_join(race_infos_2014_to_2021_clean_woNAs,by=c("race_id","year")) #Race infos dataset
  
  
  ###########
  #   Step 2: Wrangling
  ###########
  
  ####Remaining non-numerical ranks
  Merged_dtb%>%
    filter(fight_finish==FALSE)%>%
    pull(Rnk)%>%table() #OK (DNF,DSQ,OTL)
  
  #Main Y factor
  table(Merged_dtb$Main_result) 

  ###Sorted factor result
  est_results <- factor(Merged_dtb$Main_result,
                        levels = c("Fought for the win",
                                   "Fought for the podium",
                                   "Fought for the top 10",
                                   "Active in the final",
                                   "Finished the race",
                                   "Did not finish"))
  
  ###Identify missing values in team ID
  sum(is.na(Merged_dtb$team_id)) #5 missing team IDs
  #Manual fixes using PCS website  
  Merged_dtb_clean <- Merged_dtb%>%
    mutate(team_id=ifelse(rider_id=="jean-pierre-drucker" &
                            year==2014 &
                            is.na(team_id),
                          "wanty-groupe-gobert-2014",team_id),
           team_id=ifelse(rider_id=="michael-schwarzmann" &
                            year==2016 &
                            is.na(team_id),
                          "bora-argon-18-2016",team_id),
           team_id=ifelse(rider_id=="sjoerd-van-ginneken" &
                            year==2018 &
                            is.na(team_id),
                          "roompot-nederlandse-loterij-2018",team_id),
           team_id=ifelse(rider_id=="matteo-bono" &
                            year==2018 &
                            is.na(team_id),
                          "uae-team-emirates-2018",team_id),
           team_id=ifelse(rider_id=="max-walscheid" &
                            year==2021 &
                            is.na(team_id),
                          "team-qhubeka-assos-2021",team_id))
  #Check
  sum(is.na(Merged_dtb_clean$team_id)) #0 (Fixed)
  
  
  #####Adding the number of teammates on the race
  Merged_dtb_clean <- Merged_dtb_clean%>%
    mutate(race_year_team_id=paste0(race_id,"/",year,"/",team_id))%>%
    group_by(race_year_team_id)%>%
    mutate(N_riders_race_team=n())%>%
    ungroup()
  
  #####Adding the number of riders on the race
  Merged_dtb_clean <- Merged_dtb_clean%>%
    group_by(race_year_id)%>%
    mutate(N_riders_race=n())%>%
    ungroup()
  
  ##Dummy race nationality is the race rider
  Merged_dtb_clean <-Merged_dtb_clean%>%
    mutate(race_rider_nationality=ifelse(rider_nation==nationality,TRUE,FALSE))
  prop.table(table(Merged_dtb_clean$race_rider_nationality)) #19%
  
  ###Last BIB number is 1 "Unofficial leader"
  Merged_dtb_clean <-Merged_dtb_clean%>%
    mutate(BIB_finishby1=ifelse(str_ends(as.character(BIB),"1"),TRUE,FALSE))
  prop.table(table(Merged_dtb_clean$BIB_finishby1)) #13%
  
  ###Professional experience
  Merged_dtb_clean <-Merged_dtb_clean%>%
    mutate(professional_exp=as.numeric(Date-professional_since)/365.25)
  
  ###Accurate age
  Merged_dtb_clean <-Merged_dtb_clean%>%
    mutate(age_acc=as.numeric(Date-Date_of_birth)/365.25)
  
  ###Add race-year-team ID (for stats at the team level at the start of eachs race)
  Merged_dtb_clean <-Merged_dtb_clean%>%
    mutate(race_year_team_id=paste0(race_id,"/",year,"/",team_id))
  
  ###N_riders_race: Number of riders at the start
  Merged_dtb_clean <-Merged_dtb_clean%>%
    group_by(race_year_id)%>%
    mutate(N_riders_race=n())%>%
    ungroup()
  
  ###Variables in numeric form
  Merged_dtb_clean <- Merged_dtb_clean%>%
    mutate_at(c("Distance","Weight","Height"),as.numeric)
  
  ###########
  #   Step 3: Removing useless variables
  ###########
  
  Merged_dtb_clean <- Merged_dtb_clean%>%
    select(-BIB,-Rider,-Age,-Team,-UCI,-Minute_gap,-Second_gap,-rider_nation,
           -Date_of_birth,-Nationality,-Place_of_birth,-professional_since,
           -Passed_away_on,-max_date,-Date,-nationality)
  
  ###########
  #   Step 4: Stength variables
  ###########

  strength_vars <- c(career_strength_vars,form_strength_vars,status_strength_vars)

  ####Team strength
  replace_suffix <- function(string){str_replace(string,"_\\d{1}[yw]","")}    
  form_suffix <- paste0(as.character(f),"w")
  status_suffix <-   paste0(as.character(s),"y")
  
  ####Compute team strength
  Team_strength <- Merged_dtb_clean%>%
    group_by(race_year_team_id)%>%
    transmute_at(vars(all_of(strength_vars)),sum)%>%
    rename_at(vars(contains("status")),replace_suffix)%>%
    rename_at(vars(contains("form")),replace_suffix)%>%
    mutate(N_riders=n())%>%
    ungroup()%>%
    select(-race_year_team_id)%>%
    rename_all(paste0,"_team")
  
  ####Compute rank within team ("status in the team")
  Team_status <- Merged_dtb_clean%>%
    group_by(race_year_team_id)%>%
    transmute_at(vars(all_of(strength_vars)),scale)%>% ####Scale strength vars at the race-year-team level!
    ungroup()%>%
    select(-race_year_team_id)%>%
    rename_all(paste0,"_teamstatus")

  ###Merging dataset
  Merged_dtb_clean2 <- Merged_dtb_clean%>%
    bind_cols(Team_strength,Team_status)
  
  
  ###########
  #   Step 5: Variable selection
  ###########

  ####################Y variables
  
  variables_y <- c("Main_result",
                   "fight_win",
                   "fight_podium",
                   "fight_top10",
                   "fight_active",
                   "fight_finish")

  ####################X variables
  
  variables_x_rider <- c("age_acc", #Scale at the race level
                         "professional_exp", #Scale at the race level
                         "Weight", #Scale at the race level
                         "Height") #Scale at the race level

  variables_x_rider_teamstatus_t <- variable.names(Team_status) #Already scaled at the race-team level
  variables_x_rider_strength_t <- strength_vars #Scale at the race level
  variables_x_rider_t <- c(variables_x_rider_teamstatus_t,variables_x_rider_strength_t)
  
  variables_x_team_t <- variable.names(Team_strength) #Scale at the race level
  
  variables_x_race_t <- c("Distance", #Scale at the dtb level
                          "Points_scale",  #Not scaled
                          "ProfileScore",  #Scale at the dtb level
                          "Vert_meters", #Scale at the dtb level
                          "race_profile", #Not scaled
                          "N_riders_race", #Scaled at the dtb level
                          "day_id", #Scale at the dtb level
                          "race_rider_nationality")  #Not scaled

  variables_x <- c(variables_x_rider,
                   variables_x_rider_t,
                   variables_x_team_t,
                   variables_x_race_t)
  
  variables_id <- c("rider_id","race_year_id","race_year_team_id")
  
  ###########
  #   Step 6: Scaling variables
  ###########
  
  Merged_dtb_clean2%>%
    #Variables scaled at the dtb level
    mutate_at(c("Distance","ProfileScore","Vert_meters","N_riders_race","day_id"),scale)%>%
    group_by(race_year_id)%>%
    mutate_at(all_of(variables_x_rider),scale)%>%
    mutate_at(all_of(variables_x_rider_strength_t),scale)%>%
    mutate_at(all_of(variables_x_team_t),scale)%>%
    ungroup()%>%
    select(all_of(variables_y),all_of(variables_x),all_of(variables_id))%>%
    relocate(all_of(variables_y),all_of(variables_x),all_of(variables_id))
}

###Function available only for form window [2-8w] and status window [1-5y]
my_wrangle_dataset(4,3)
my_wrangle_dataset(6,4)

#######################################################
#                                                     #
#         Cycling predictions using ML                #
#                       -                             #
#                 3. Analysis                         #
#                                                     #
#######################################################

#####Because of the data structure, 
# we split the dataset depending on the race-year IDs

###Seed set 2022-3-3=2016
set.seed(2016,sample.kind="Rounding")
test_set_races <-  race_infos_2014_to_2021_clean_woNAs$race_year_id[createDataPartition(1:130,times=1,p=0.2,list=FALSE)]
train_set_races <- race_infos_2014_to_2021_clean_woNAs$race_year_id[!race_infos_2014_to_2021_clean_woNAs$race_year_id%in%test_set_races]


###Folds for Cross Validation 5-fold CV
set.seed(2016,sample.kind="Rounding")
CV_races_id <- createFolds(train_set_races,k=5,list=TRUE)


##################################################
#                                                #
#            3.1. Data analysis                  #
#                                                #
##################################################

###Cleaned results from Lombardia 2016
example_lombardia2016 <- dtb_results_2014_to_2021%>%
  filter(race_id=="il-lombardia" & year==2016)%>%
  select(Rnk,Rider,Time_seconds,Time_seconds_fixed,fight_win,fight_podium,fight_top10,fight_active)%>%
  .[1:35,]%>%
  rename(Gap_secs=Time_seconds,
         Gap_secs_fixed=Time_seconds_fixed)
kable(example_lombardia2016)

Working_dataset <- my_wrangle_dataset(6,3)%>%
  filter(race_year_id%in%train_set_races) #Equivalent to train set

###Identify NAs
#-Team status variables need to be reworked
#
sum(is.na(Working_dataset))
Working_dataset%>%
  mutate_all(is.na)%>%
  summarize_all(sum)%>%
  t()

###Probas outcomes
Working_dataset%>%
  summarize_at(c("fight_win","fight_podium",
                 "fight_top10","fight_active",
                 "fight_finish"),mean)%>%
  t()
#Fight win: 10%
#Fight podium: 11%
#Fight top 10: 15%
#Fight active: 26%
#Fight finish: 66%

prop.table(table(Working_dataset$Main_result))
#10% fight for the win
#1% Fight for podium
#3% Fight for top 10
#9% Active final
#42% Fight to finish
#34% Did not finish


###Graph point scale top 50                 
t <- scales_list_2014_to_2021_clean%>%
  filter(year==2021)%>%
  filter(Result<=50)

t%>%
  ggplot(aes(factor(Result),Points,color=Points_scale))+
  geom_point()+
  scale_y_sqrt()+
  theme(axis.text.x = element_text(angle=90,hjust=1))+
  xlab("Result")
rm(t)

###Proportion outcomes within each race
mean_results <- Working_dataset%>%
  group_by(race_year_id)%>%
  transmute_at(c("fight_win","fight_podium",
                 "fight_top10","fight_active",
                 "fight_finish"),mean)%>%
  distinct()%>%
  ungroup()

mean_results%>%
  summarize_at(-1,median)%>%t()
mean_results%>%
  pivot_longer(-1,names_to = "Prop_dummy_name",
               values_to = "Prop_dummy")%>%
  mutate(Prop_dummy_name=reorder(Prop_dummy_name,Prop_dummy,FUN=median))%>%
  ggplot(aes(Prop_dummy_name,Prop_dummy))+
  geom_boxplot()+
  coord_flip()+
  xlab("")+
  ylab("% riders")+
  ggtitle("Race outcomes")

###Mean outcome by race id
mean_results%>%
  mutate(race_id=str_split(race_year_id,"/",simplify = TRUE)[,1]%>%factor())%>%
  pivot_longer(2:6,names_to = "Prop_dummy_name",
               values_to = "Prop_dummy")%>%
  mutate(Prop_dummy_name=Prop_dummy_name%>%
           factor(levels=c("fight_win","fight_podium","fight_top10",
                           "fight_active","fight_finish")))%>%
  mutate(race_id=reorder(race_id,Prop_dummy,median))%>%
  ggplot(aes(race_id,Prop_dummy))+
  facet_wrap(~Prop_dummy_name)+
  geom_boxplot()+
  coord_flip()+
  xlab("")+
  ggtitle("Outcomes by race")


###Time gap top 50  
dtb_results_2014_to_2021%>%
  filter(Rnk!="DSQ" & Rnk!="DNF" & Rnk!="OTL")%>%
  mutate_at("Rnk",as.numeric)%>%
  select(Rnk,Time_seconds)%>%
  filter(Rnk<=50)%>%
  ggplot(aes(factor(Rnk),Time_seconds))+
  geom_boxplot()+
  scale_y_sqrt()+
  xlab("Rank")+
  ylab("Seconds gap from the winner")


##################################################
#                                                #
#            3.2. Loop over windows              #
#                                                #
##################################################

###########Selection of the tuning parameters related to the status and form windows

variables_id <- c("rider_id","race_year_id","race_year_team_id")


ks <- seq(3,30,2)

my_running_workfile <- function(f,s){
  
  Working_dataset <- my_wrangle_dataset(f,s)
  
  #####
  #                                                
  #  Step 1: Data partition                
  #                                                
  #####


  Working_dataset2 <- Working_dataset%>%
    select(-N_riders_team)%>%
    select(-ends_with("teamstatus"))%>% #Remove team status vars
    select(-Main_result,-fight_podium,-fight_win,
           -fight_active,-fight_finish)%>% #Remove other outputs
    mutate(fight_top10=factor(fight_top10,levels=c(TRUE,FALSE)))%>%
    select(-Points_scale,-race_rider_nationality) #Remove factors
  
  sum(is.na(Working_dataset2))
  Working_dataset2%>%
    mutate_all(is.na)%>%
    summarize_all(sum)%>%
    t()


  train_set <- Working_dataset2%>%
    filter(race_year_id%in%train_set_races)
  test_set <- Working_dataset2%>%
    filter(race_year_id%in%test_set_races)  
  
  
  #####
  #                                                
  #  Step 2: knn model w/o pca              
  #                                                
  #####
  

  ###Create data partition inside the training set
  
  print("Model 1: KNN without PCA")
  
  knn_wo_pca <- map_dfr(ks,function(k_knn){
    print(paste0("k=",as.character(k_knn)))
    ###5-fold Cross-Validation
    CV <- map_dfr(1:5,function(i){
      ind <- CV_races_id[[i]] #Race indices
      test_set_races_CV <- train_set_races[ind] #Races in the test set for CV
      train_set_races_CV <- train_set_races[-ind] #Races in the train set for CV
      #Training set for CV
      train_CV <- train_set%>%
        filter(race_year_id%in%test_set_races_CV)%>%
        select(-all_of(variables_id))
      #Test set for CV
      test_CV<- train_set%>%
        filter(!race_year_id%in%test_set_races_CV)%>%
        select(-all_of(variables_id))
      
      #KNN estimate
      knn_fit <- knn3(fight_top10~.,k=k_knn,data=train_CV)
      #Prediction
      y_hat <- predict(knn_fit,test_CV,type="class")
      
      confusionMatrix(y_hat,test_CV$fight_top10)
      #CV stats
      tibble(CV_round=i,
             F1_score=F_meas(y_hat,test_CV$fight_top10),
             accuracy=confusionMatrix(y_hat,test_CV$fight_top10)$overall["Accuracy"],
             sensitivity=confusionMatrix(y_hat,test_CV$fight_top10)$byClass["Sensitivity"],
             specificity=confusionMatrix(y_hat,test_CV$fight_top10)$byClass["Specificity"])
    })
    #Selection of k depending on the F1 score
    tibble(k=k_knn,F1_score=mean(CV$F1_score),
           accuracy=mean(CV$accuracy),
           sensitivity=mean(CV$sensitivity),
           specificity=mean(CV$specificity))
})
  
  
  #####
  #                                                
  #  Step 3: knn model w/ pca              
  #                                                
  #####
  
  pca_train <- prcomp(train_set[,-1]%>%
                        select(-all_of(variables_id)))
  
  summary(pca_train)$importance%>%
    as_tibble()%>%
    .[3,]%>%
    pivot_longer(everything(),values_to = "Prop",names_to="PCA_name")%>%
    mutate(PCA_name=str_replace(PCA_name,"PC","")%>%as.numeric())%>%
    ggplot(aes(PCA_name,Prop))+
    geom_point()+
    geom_line()
  
  ###Min number of PCAs to represent at least 95% variation
  PCA_number <- summary(pca_train)$importance%>%
    as_tibble()%>%
    .[3,]%>%
    pivot_longer(everything(),values_to = "Prop",names_to="PCA_name")%>%
    mutate(PCA_name=str_replace(PCA_name,"PC","")%>%as.numeric())%>%
    filter(Prop>=0.95)%>%
    summarize(min=min(PCA_name))%>%
    pull(min) #22
  
  ###Threshold
  summary(pca_train)$importance[,PCA_number] #95.6%
  
  train_set_id <- train_set%>%
    select(all_of(variables_id))
  ###Train set with PCA vectors
  train_set_pca <- bind_cols(fight_top10=train_set$fight_top10,
                             pca_train$x[,1:PCA_number],
                             train_set_id)
  
  #Temporary X variables in test set
  x_test_set_temp <- test_set[,-1]%>%
    select(-all_of(variables_id)) 
  #Column means of X
  col_means_x_test <- colMeans(x_test_set_temp)
  #Apply PCA rotation
  x_test_set <- as.matrix(sweep(x_test_set_temp, 2, col_means_x_test)) %*% pca_train$rotation
  
  test_set_id <- test_set%>%
    select(all_of(variables_id))
  test_set_pca <- bind_cols(fight_top10=test_set$fight_top10,
                            x_test_set[,1:PCA_number],
                            test_set_id)
  

  print("Model 2: KNN PCA")
  
  knn_with_pca <- map_dfr(ks,function(k_knn){
    print(paste0("k=",as.character(k_knn)))
    ###5-fold Cross-Validation
    CV <- map_dfr(1:5,function(i){
      ind <- CV_races_id[[i]] #Race indices
      test_set_races_CV <- train_set_races[ind] #Races in the test set for CV
      train_set_races_CV <- train_set_races[-ind] #Races in the train set for CV
      #Training set for CV
      train_CV <- train_set_pca%>%
        filter(race_year_id%in%test_set_races_CV)%>%
        select(-all_of(variables_id))
      #Test set for CV
      test_CV<- test_set_pca%>%
        filter(!race_year_id%in%test_set_races_CV)%>%
        select(-all_of(variables_id))
      
      #KNN estimate
      knn_fit <- knn3(fight_top10~.,k=k_knn,data=train_CV)
      #Prediction
      y_hat <- predict(knn_fit,test_CV,type="class")
      
      confusionMatrix(y_hat,test_CV$fight_top10)
      #CV stats
      tibble(CV_round=i,
             F1_score=F_meas(y_hat,test_CV$fight_top10),
             accuracy=confusionMatrix(y_hat,test_CV$fight_top10)$overall["Accuracy"],
             sensitivity=confusionMatrix(y_hat,test_CV$fight_top10)$byClass["Sensitivity"],
             specificity=confusionMatrix(y_hat,test_CV$fight_top10)$byClass["Specificity"])
    })
    #Selection of k depending on the F1 score
    tibble(k=k_knn,F1_score=mean(CV$F1_score),
           accuracy=mean(CV$accuracy),
           sensitivity=mean(CV$sensitivity),
           specificity=mean(CV$specificity))
  })

  knn_with_pca <- knn_with_pca%>%
    mutate(Model="Knn with PCA")
  knn_wo_pca <- knn_wo_pca%>%
    mutate(Model="KNN without PCA")
  
  #Output
  knn_with_pca%>%
    bind_rows(knn_wo_pca)%>%
    mutate(Status_window=s,
           Form_window=f)
  
}

#####################

results <- map_dfr(seq(2,6,2),function(form_w){
  print(paste0("Form window: ",form_w," weeks"))
  map_dfr(3:5,function(status_w){
    print(paste0("Status window: ",status_w," years"))
        my_running_workfile(form_w,status_w)
    })
})


save(results,file=file.path("rda","final_results.rda"))

##################################################
#                                                #
#            3.3. Final models                   #
#                                                #
##################################################

load(file.path("rda","final_results.rda"))

###Model 1
results%>%
  filter(Model=="KNN without PCA")%>%
  mutate(sw_fw=paste0("sw",as.character(Status_window),"_","fw",as.character(Form_window)))%>%
  ggplot(aes(k,F1_score,color=sw_fw))+
  geom_point()+
  geom_line()

###Graph according to accuracy
results%>%
  filter(Model=="KNN without PCA")%>%
  mutate(sw_fw=paste0("sw",as.character(Status_window),"_","fw",as.character(Form_window)))%>%
  ggplot(aes(k,accuracy,color=sw_fw))+
  geom_point()+
  geom_line()  
  
###Model 2
results%>%
  filter(Model=="Knn with PCA")%>%
  mutate(sw_fw=paste0("sw",as.character(Status_window),"_","fw",as.character(Form_window)))%>%
  ggplot(aes(k,F1_score,color=sw_fw))+
  geom_point()+
  geom_line()  

optimal_model <- results%>%
  filter(F1_score==max(F1_score))

results%>%
  filter(Model=="KNN without PCA")%>%
  filter(F1_score==max(F1_score))%>%
  t()

optimal_model <- results%>%
  filter(F1_score==max(F1_score))
optimal_model%>%kable()

###Graph according to accuracy
results%>%
  filter(Model=="Knn with PCA")%>%
  mutate(sw_fw=paste0("sw",as.character(Status_window),"_","fw",as.character(Form_window)))%>%
  ggplot(aes(k,accuracy,color=sw_fw))+
  geom_point()+
  geom_line()  

#########################

Working_dataset <- my_wrangle_dataset(optimal_model$Form_window,
                                      optimal_model$Status_window)

#####
#                                                
#  Step 1: Data partition                
#                                                
#####


Working_dataset2 <- Working_dataset%>%
  select(-N_riders_team)%>%
  select(-ends_with("teamstatus"))%>% #Remove team status vars
  select(-Main_result,-fight_podium,-fight_win,
         -fight_active,-fight_finish)%>% #Remove other outputs
  mutate(fight_top10=factor(fight_top10,levels=c(TRUE,FALSE)))%>%
  select(-Points_scale,-race_rider_nationality) #Remove factors

sum(is.na(Working_dataset2))
Working_dataset2%>%
  mutate_all(is.na)%>%
  summarize_all(sum)%>%
  t()

train_set_id <- Working_dataset2%>%
  filter(race_year_id%in%train_set_races)%>%
  select(all_of(variables_id))

test_set_id <- Working_dataset2%>%
  filter(race_year_id%in%test_set_races)%>%
  select(all_of(variables_id))

train_set <- Working_dataset2%>%
  filter(race_year_id%in%train_set_races)%>%
  select(-all_of(variables_id))
test_set <- Working_dataset2%>%
  filter(race_year_id%in%test_set_races)%>%
  select(-all_of(variables_id))


#####
#                                                
#  Step 2: knn model w/o pca              
#                                                
#####

#KNN estimate
knn_fit_opt1 <- knn3(fight_top10~.,k=optimal_model$k,data=train_set)
#Prediction
y_hat_opt1 <- predict(knn_fit_opt1,test_set,type="class")

cm_opt1 <- confusionMatrix(y_hat_opt1,test_set$fight_top10)
F1score_opt1 <- F_meas(y_hat_opt1,test_set$fight_top10)

results_opt1 <- tibble(Model="Knn without PCA",
                       k=optimal_model$k,
                       Form_window=paste0(as.character(optimal_model$Form_window)," weeks"),
                       Status_window=paste0(as.character(optimal_model$Status_window)," years"),
                       F1_score=F1score_opt1,
                       accuracy=cm_opt1$overall["Accuracy"],
                       sensitivity=cm_opt1$byClass["Sensitivity"],
                       specificity=cm_opt1$byClass["Specificity"])



#####
#                                                
#  Step 3: knn model w/ pca              
#                                                
#####

pca_train_opt <- prcomp(train_set[,-1])

summary(pca_train_opt)$importance%>%
  as_tibble()%>%
  .[3,]%>%
  pivot_longer(everything(),values_to = "Prop",names_to="PCA_name")%>%
  mutate(PCA_name=str_replace(PCA_name,"PC","")%>%as.numeric())%>%
  ggplot(aes(PCA_name,Prop))+
  geom_point()+
  geom_line()

###Min number of PCAs to represent at least 95% variation
PCA_number <- summary(pca_train_opt)$importance%>%
  as_tibble()%>%
  .[3,]%>%
  pivot_longer(everything(),values_to = "Prop",names_to="PCA_name")%>%
  mutate(PCA_name=str_replace(PCA_name,"PC","")%>%as.numeric())%>%
  filter(Prop>=0.95)%>%
  summarize(min=min(PCA_name))%>%
  pull(min) #22

###Threshold
summary(pca_train_opt)$importance[,PCA_number] #95.6%

###Train set with PCA vectors
train_set_pca_opt <- bind_cols(fight_top10=train_set$fight_top10,
                               pca_train_opt$x[,1:PCA_number])

#Temporary X variables in test set
x_test_set_temp_opt <- test_set[,-1]
#Column means of X
col_means_x_test_opt <- colMeans(x_test_set_temp_opt)
#Apply PCA rotation
x_test_set_opt <- as.matrix(sweep(x_test_set_temp_opt, 2, col_means_x_test_opt)) %*% pca_train_opt$rotation

test_set_pca_opt <- bind_cols(fight_top10=test_set$fight_top10,
                          x_test_set_opt[,1:PCA_number])



#####KNN estimate
knn_fit_pca_opt <- knn3(fight_top10~.,k=optimal_model$k,data=train_set_pca_opt)
#Prediction
y_hat_pca_opt <- predict(knn_fit_pca_opt,test_set_pca_opt,type="class")

cm_opt2 <- confusionMatrix(y_hat_pca_opt,test_set_pca_opt$fight_top10)
F1score_opt2 <- F_meas(y_hat_pca_opt,test_set_pca_opt$fight_top10)


results_opt2 <- tibble(Model="Knn with PCA",
                       k=optimal_model$k,
                       Form_window=paste0(as.character(optimal_model$Form_window)," weeks"),
                       Status_window=paste0(as.character(optimal_model$Status_window)," years"),
                       F1_score=F1score_opt2,
                       accuracy=cm_opt2$overall["Accuracy"],
                       sensitivity=cm_opt2$byClass["Sensitivity"],
                       specificity=cm_opt2$byClass["Specificity"])

options(digits=3)
results_opt1%>%
  bind_rows(results_opt2)%>%
  kable()
