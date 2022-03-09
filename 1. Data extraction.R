#######################################################
#                                                     #
#         Cycling predictions using ML                #
#                       -                             #
#               1. Data extraction                    #
#                                                     #
#######################################################

#############General infos################
##Author: Clément ANNE
##Author e-mail: clement.anne90@gmail.com
##Date: March 08,2022
##########################################


################Outline###################
##
##  1.0. Preliminary steps
##  1.1. Race list and infos
##  1.2. Point scales
##  1.3. Race results
##    1.3.1. Extraction
##    1.3.2. Grouping riders in race results
##  1.4. Rider infos
##  1.5. Rider results per year
##    1.5.1. Extraction
##    1.5.2. Wrangling
##    1.5.3. Past results dataset
##
##########################################

##################################################
#                                                #
#            1.0. Preliminary steps              #
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

###Install tinytex if needed
if(is_tinytex()==FALSE) tinytex::install_tinytex()

###Working directory
getwd()

start_Rfile_running <- now()

##################################################
#                                                #
#         1.1. Race list and infos               #
#                                                #
##################################################

###Function extract 1-day WT races per year
my_extract_races <- function(year){
  url <- paste0("https://www.procyclingstats.com/races.php?year=",as.character(year),"&circuit=1&class=1.UWT&filter=Filter&s=year-calendar")
  h <- read_html(url)
  
  h%>%
    html_elements(".basic")%>%
    html_elements("a")%>%
    html_attr("href")%>%
    as_tibble()%>%
    setNames("race_id")%>%
    mutate(type=str_extract(race_id,"^[A-Za-z]{4}"))%>%
    filter(type=="race")%>%
    filter(str_detect(race_id,"preview")==FALSE)
}
#Example
my_extract_races(2014)

###Full list of races
race_list_2014_to_2021 <- map_dfr(2014:2021,function(y){
  str_split(my_extract_races(y)$race_id,"/",simplify=TRUE)%>%
    as_tibble()%>%
    select(-1)%>%
    setNames(c("race_id","year"))%>%
    mutate(race_year_id=paste(race_id,as.character(year),sep="/"))
})
nrow(race_list_2014_to_2021)
#Number of obs per race
race_list_2014_to_2021%>%
  group_by(race_id)%>%
  summarize(n=n())%>%
  arrange(desc(n))%>%
  mutate(race_id=reorder(race_id,n))

###Function extract race infos
my_extract_race_infos<- function(race,year){
  url_ry <- paste0("https://www.procyclingstats.com/race/",
                   race,
                   "/",
                   as.character(year),
                   "/result"
  )
  
  h <- read_html(url_ry)
  
  intro_node <- h%>%html_elements(".infolist")
  
  infos <- html_text(intro_node[[1]])
  infos_str <- str_split(infos,"\n")%>%
    .[[1]]%>%
    str_trim()%>%
    str_match("([A-Za-z]+\\W*[A-Za-z]*\\W*[A-Za-z]*)\\:\\s*(\\w+\\W*\\w*\\W*\\w*\\W*\\w*\\W*\\w*\\W*\\w*\\W*\\w*\\W*\\w*\\W*\\w*\\W*)")
  
  #Extract race profile
  race_profile <- h%>%html_elements(".infolist")%>%
    html_elements("span")%>%
    html_attr("class")%>%
    word(-1)
  race_profile[race_profile=="p0"] <- NA
  
  nodes <- h%>%
    html_elements(".page-title")%>%
    html_elements("span")%>%
    html_attr("class")
  
  nationality <- nodes[str_detect(nodes,"flag")==TRUE][1]%>%
    word(2)
  
  
  colnames <- t(infos_str[,-1])[1,]%>%
    str_trim()%>%
    str_replace_all("\\.","")%>%
    str_replace_all("\\s+","_")
  
  t(infos_str[,-1])%>%
    as_tibble()%>%
    setNames(colnames)%>%
    .[-1,]%>%
    select(colnames[!is.na(colnames)])%>%
    mutate(race_id=race,
           year=year,
           nationality=nationality,
           race_profile=race_profile,
           race_year_id=paste(race_id,as.character(year),sep="/"))
}
#Example
my_extract_race_infos("paris-roubaix",2017)%>%
  select(Date,Distance,ProfileScore,Vert_meters,race_id,year,nationality,race_profile)

###Full database of race infos
race_infos_2014_to_2021 <- map2_dfr(race_list_2014_to_2021$race_id,race_list_2014_to_2021$year,function(x,y){
  my_extract_race_infos(x,y)
})

###Remove race list (available in the race info dtb)
rm(race_list_2014_to_2021)

###Wrangling race infos
race_infos_2014_to_2021_clean <- race_infos_2014_to_2021%>%
  select(Date,Distance,Points_scale,
         ProfileScore,Vert_meters,
         race_id,year,nationality,
         race_profile,race_year_id)%>%
  mutate(Date=dmy(Date),
         day_id=yday(Date),
         Distance=str_extract(Distance,"^\\d+"),
         Points_scale=factor(Points_scale,levels=c("1.WT.A",
                                                   "1.WT.B",
                                                   "1.WT.C")),
         ProfileScore=as.numeric(ProfileScore),
         Vert_meters=as.numeric(Vert_meters),
         year=as.numeric(year),
         race_profile=ifelse(race_profile=="p0",NA,race_profile),
         race_profile=factor(race_profile,levels=c("p1",
                                                   "p2",
                                                   "p3",
                                                   "p4",
                                                   "p5")),
         race_profile=recode(race_profile,
                             `p1`="flat",
                             `p2`="hills, flat finish",
                             `p3`="hills, uphill finish",
                             `p4`="mountains, flat finish",
                             `p5`="mountains, uphill finish"))

rm(race_infos_2014_to_2021)  

###Saving the race infos dataset
save(race_infos_2014_to_2021_clean,file=file.path("rda","race_infos.rda"))

##################################################
#                                                #
#           1.2. Point scales                    #
#                                                #
##################################################

my_extract_point_scale <- function(year){
  
  #Identify the point scales list table for a given year
  url <- paste0("https://www.procyclingstats.com/info.php?s=point-scales&season=",
                as.character(year))
  h <- read_html(url)
  nodes <- h%>%
    html_elements("table")
  scales <- html_table(nodes[[1]]) #Different point scales
  
  #Extract vector of hypertext references
  href_scales <- h%>%
    html_elements("table")%>%
    .[[1]]%>%
    html_elements("a")%>%
    html_attr("href")
  
  #Keeping only World Tour scales (1-day classics start with 1.WT)
  WT_scales <- scales%>%
    setNames(c("id","point_scale","points_winner","N_races"))%>%
    cbind(href_scales)%>%
    filter(str_detect(point_scale,"^1.WT")==TRUE)
  
  #Dataset with World Tour scales for a given year
  map_dfr(1:nrow(WT_scales),function(i){
    url <- paste0("https://www.procyclingstats.com/info.php",
                  WT_scales$href_scales[i])
    h <- read_html(url)
    
    nodes <- h%>%
      html_elements("table")
    scale_tab <- html_table(nodes[[2]]) ###Table with point scale
    
    scale_tab%>%
      mutate(point_scale=WT_scales$point_scale[i],
             year=year)
  })
}
#Example
my_extract_point_scale(2019)

###Dataset with point scales depending on WT 1-day race scale by year
scales_list_2014_to_2021 <- map_dfr(2014:2021,my_extract_point_scale)

####Wrangling
scales_list_2014_to_2021_clean <- scales_list_2014_to_2021%>%
  mutate(Points_scale=factor(point_scale,levels=c("1.WT.A",
                                          "1.WT.B",
                                          "1.WT.C")))%>%
  select(-point_scale)

rm(scales_list_2014_to_2021)

save(scales_list_2014_to_2021_clean,file=file.path("rda","point_scales.rda"))


##################################################
#                                                #
#             1.3. Race results                  #
#                                                #
##################################################

#############################################
#            1.3.1. Extraction              #
#############################################

###Extract startlist from race results attributes
my_extract_race_startlist <- function(race,year){
  url_ry <- paste0("https://www.procyclingstats.com/race/",
                   race,
                   "/",
                   as.character(year),
                   "/result"
  )
  
  h <- read_html(url_ry)
  
  ###Extract hrefs attributes from the table
  
  #Divide the table into lines
  nodes <- h%>%html_elements("table")%>% 
    html_elements("tr")%>%
    .[-1] #Removing the first line (column title line)
  
  #Extract ids from hrefs attributes for each line
  ids <- map_dfr(1:length(nodes),function(i){
    id_vector <- nodes[[i]]%>%
      html_elements("a")%>%
      html_attr("href")
    
  tibble(rider_id=id_vector[1],
         team_id=id_vector[2])%>%
      setNames(c("rider_id","team_id"))%>%
    mutate_all(str_replace,"team/|rider/","")
  })


  ###Extract nationality through flag attribute
  rider_nation <- h%>%html_elements("table")%>%
    html_elements("tr")%>%
    html_attr("data-nation")%>%
    .[-1] #Removing the first line (column title line)
           
  tibble(rider_id=ids$rider_id,
         team_id=ids$team_id,
         rider_nation=rider_nation,
         year=year,
         race_id=race)
}
#Example
my_extract_race_startlist("ronde-van-vlaanderen",2014)

###Extract race results table
my_extract_race_results <- function(race,year){
  url_ry <- paste0("https://www.procyclingstats.com/race/",
                race,
                "/",
                as.character(year),
                "/result"
                )
  
  h <- read_html(url_ry)
  
  nodes <- html_nodes(h,"table")
  
  #Results table
  html_table(nodes[[1]])%>%
    select(-H2H) #Remove optional columns
  
}
#Example
my_extract_race_results("paris-roubaix",2018)


#############################################
#   1.3.2. Grouping riders in race results  #
#############################################

###Wrangling race results
my_wrangle_race_results <- function(race,year){
  tab <- my_extract_race_results(race,year)%>%
    cbind(my_extract_race_startlist(race,year))%>%
    #Remove riders which did not start
    filter(Rnk!="DNS")%>%

    mutate(Team=if_else(nchar(Team)==0,"-",Team))%>% #Trick to prevent error with missing team
    mutate(Rider=str_replace(Rider,Team,""))%>% #Team names stuck at the end of rider names
    #Bib number in character
    mutate(BIB=as.character(BIB))%>%
    #Replace Time gap is NA when OTL (Outside Time Limit), DNF (Did Not Finished), or DSQ (Disqualified)
    mutate(Time=if_else(str_detect(Rnk,"OTL|DNF|DSQ"),"-",Time))%>%
    ###Preparing the time gap variable
    mutate(Time=str_trim(Time))%>%
    mutate(Time=str_replace(Time,",,",""))%>% #Remove commas in the time gap column
    mutate(Time=str_replace(Time,"-",""))%>% #Remove commas in the time gap column
        mutate(Time=if_else(Rnk=="1","0:00",Time))%>% #Replace the race duration for the leader by 0:00
    #Extract time patterns
    mutate(Time=str_replace(Time,"^(\\d{1,2})\\:(\\d{2})\\d*\\:?\\d*$","\\1:\\2"))%>%
    extract(Time,c("Minute_gap","Second_gap"),regex="(\\d{1,2})\\:(\\d{2})",remove=FALSE)%>%
    #Convert variables as numeric
    mutate_at(c("Minute_gap","Second_gap"),as.numeric)%>%
    #Convert time gap to a period vector (minute:second)
    mutate(Time_seconds=ms(Time,quiet=TRUE)%>%period_to_seconds())%>% #Warning message because of NAs (DNF/DNS...)
    mutate(Time_seconds_fixed=Time_seconds) #Column for Time fixes
   
  min_gap_remaining <- min(diff(as.numeric(levels(factor(tab$Time_seconds_fixed))))) #Minimum gap within groups in the top 30 for the while loop

  if (min_gap_remaining<=10){
    while (min_gap_remaining<=10){
      
      ##Identify levels in time gap for the top 30
      group_levels <- tab%>%
        summarize(levels=levels(factor(Time_seconds_fixed)))%>%
        pull(levels)
      
      ##Identify groups which finished within 10 seconds or less
      levels <- as.numeric(group_levels)
      levels_gap <- c(0,diff(as.numeric(group_levels))) #Gap between levels
      a <- which(levels_gap<=10) #Group numbers (a and a+1) identified 
      a <- a[a!=1] ###Remove 1st occurence (Rider who finished 1st) and keep only the 1st pair of close groups
      
      print(paste("Number of group pairs remaining to fix=",length(a)))
      
      a <- a[1]#Keep only the 1st pair of group to fix
      
      ##Temp table to identify the main group between both
      tab_temp <- tab%>%
        group_by(Time_seconds_fixed)%>%
        summarize(n=n())
      ##Variable identifying whether the 1st group was the main group
      main_before <- ifelse(which.max(tab_temp$n[(a-1):a])==1,"TRUE","FALSE") ##Will pick the one in front in case both groups had the same size
      
      ##Groups after correction
      levels_alt <- levels
      
      ###Case 1: Main group in front
      levels_alt[a] <- ifelse(main_before==TRUE,levels_alt[a-1],levels_alt[a])
      
      ##Case 2: Main group behind
      levels_alt[a-1] <- ifelse(main_before==FALSE,levels_alt[a],levels_alt[a-1])
      
      ###Correction in the table
      tab <- tab%>%
        mutate(Time_seconds_fixed=ifelse(Time_seconds_fixed==levels[a-1],levels_alt[a-1],Time_seconds_fixed))%>%
        mutate(Time_seconds_fixed=ifelse(Time_seconds_fixed==levels[a],levels_alt[a],Time_seconds_fixed))
      
      #Check if further split needed
      min_gap_remaining <- min(diff(as.numeric(levels(factor(tab$Time_seconds_fixed)))))
      
    }
  }  
  tab%>%
    mutate(Time_gap_factors=factor(tab$Time_seconds_fixed))%>%
    group_by(Time_gap_factors)%>%
    mutate(Group_min=ifelse(!is.na(Time_seconds_fixed),
                            min(as.numeric(Rnk)),
                            NA))%>%
    mutate(Main_result=case_when(Group_min==1~"Fought for the win",
                            Group_min<=3~"Fought for the podium",
                            Group_min<=10~"Fought for the top 10",
                            Group_min<=30~"Active in the final",
                            Group_min>30~"Finished the race",
                            TRUE~"Did not finish"),
           fight_win=if_else(!is.na(Group_min),Group_min==1,FALSE),
           fight_podium=if_else(!is.na(Group_min),Group_min<=3,FALSE),
           fight_top10=if_else(!is.na(Group_min),Group_min<=10,FALSE),
           fight_active=if_else(!is.na(Group_min),Group_min<=30,FALSE),
           fight_finish=(!is.na(Group_min)))%>%
    ungroup()%>%
    select(-Time_gap_factors)
  
}
#Example
my_wrangle_race_results("il-lombardia",2019)
my_wrangle_race_results("e3-harelbeke",2016)


###Data frame with all results
dtb_results_2014_to_2021 <- map2_dfr(race_infos_2014_to_2021_clean$race_id,
                                     race_infos_2014_to_2021_clean$year,
                                     function(race,year){
  print(paste("Race=",race))
  print(paste("Year=",as.character(year)))
  my_wrangle_race_results(race,year)}
)

#Riders with the most WT 1-day races over 2014-2021
dtb_results_2014_to_2021%>%
  group_by(rider_id)%>%
  summarize(n=n())%>%
  arrange(desc(n))
#Histogram races per rider
dtb_results_2014_to_2021%>%
  group_by(rider_id)%>%
  summarize(n=n())%>%
  ggplot(aes(n))+
  geom_histogram(binwidth=1,col="black")+
  ylab("N riders")+
  xlab("N races")


save(dtb_results_2014_to_2021,file=file.path("rda","results.rda"))

##################################################
#                                                #
#               1.4. Rider infos                 #
#                                                #
##################################################

###Last rider race in the dataset
last_rider_race <- dtb_results_2014_to_2021%>%
  left_join(race_infos_2014_to_2021_clean,by=c("race_id","year"))%>%
  group_by(rider_id)%>%
  summarize(n=n(),
            max_date=max(Date))%>%
  arrange(desc(n))%>%
  mutate(i=1:length(rider_id))

####Rider list
rider_list <- unique(dtb_results_2014_to_2021$rider_id)


####Function extract rider infos
my_extract_rider_infos <- function(rider){
  
  url <- paste0("https://www.procyclingstats.com/rider/",
                rider)
  h <- read_html(url)
  
  ##Professional seasons
  seasons <- h%>%
    html_elements("div")%>%
    html_elements(".season")%>%
    html_text()%>%
    as.numeric()
  first_season <-   min(seasons[!is.na(seasons)])
  
  ##Rider info nodes
  nodes <- h%>%
    html_elements(".rdr-info-cont")%>%
    html_elements("b")
  #Extract rider info features as a string vector
  rider_infos_features <-map_chr(1:length(nodes),
                                 function(i)html_text(nodes[[i]]))
  ##Rider info content node
  node <- h%>%
    html_elements(".rdr-info-cont")%>%
    .[[1]]
  
  ##Info content split between lines  
  rider_infos <- html_text(node)%>%
    str_split("\n")%>% 
    .[[1]]
  rider_infos <-rider_infos[1] #Info in the 1st line only (Will appear as a string character)
  char_length <- nchar(rider_infos) #Length of the string character
  
  #Locate the beginning and end of content within the string
  Infos_features_chars <- map_dfr(rider_infos_features,function(f){
    rider_infos%>%
      str_locate(f)%>%
      as_tibble()
  })
  #Extract rider info content
  rider_info_content <- Infos_features_chars%>%
    cbind(n=1:nrow(Infos_features_chars))%>%
    mutate(start_content=end+1,
           end_content=ifelse(n!=nrow(Infos_features_chars),start[n+1]-1,char_length))%>%
    mutate(rider_info_content=str_sub(rider_infos,start_content,end_content))%>%
    pull(rider_info_content)
  
    #Rider info extracted as a tibble
    tibble(rider_infos_features=rider_infos_features,
         rider_info_content=rider_info_content)%>%
    mutate(rider_infos_features=str_replace_all(rider_infos_features,"\\s+","_"))%>%
    mutate(rider_infos_features=str_replace_all(rider_infos_features,":",""))%>%
    pivot_wider(names_from = "rider_infos_features",values_from = "rider_info_content")%>%
    mutate_all(str_trim)%>%
    cbind(professional_since=make_date(year=first_season,month=1,day=1))%>%
      mutate(rider_id=rider)
  
}
#Example

###Dataset of rider infos
rider_infos_dtb <- map_dfr(rider_list,function(rider){
  print(paste("Rider:",rider))
  my_extract_rider_infos(rider)
})%>%
  mutate(Date_of_birth=str_replace(Date_of_birth,"\\s*\\(\\d{2}\\)\\s*",""))%>%
  mutate(Date_of_birth=dmy(Date_of_birth))%>%
  mutate(Weight=str_extract(Weight,"^\\d+"),
         Height=100*as.numeric(str_extract(Height,"^\\d{1}\\.?\\d*")))

dtb_rider_infos_clean <- rider_infos_dtb%>%
  left_join(last_rider_race,by="rider_id")%>%
  select(-i,-n)

###Remove rider list
rm(rider_list, rider_infos_dtb, last_rider_race)

save(dtb_rider_infos_clean,file=file.path("rda","rider_infos.rda"))

##################################################
#                                                #
#           1.5. Rider results per year          #
#                                                #
##################################################

#############################################
#            1.5.1. Extraction              #
#############################################

###Extracting rider results for a given year
my_extract_rider_year <- function(rider,year){
  
  url <- paste0("https://www.procyclingstats.com/rider/",
                rider,
                "/",
                as.character(year))
  h <- read_html(url)
  
  nodes <- h%>%
    html_elements("table")
  
  results_tab <- html_table(nodes[[1]])%>%
    select(Date,Result,Race,Distance,PointsPCS,PointsUCI)%>%
    mutate(year=year,
           rider_id=rider,
           Result=as.character(Result))%>%
    as_tibble()
  
  href_list <- h%>%
    html_elements("table")%>%
    html_elements("tr")%>%
    html_elements("a")%>%
    html_attr("href")
  
  href_list_clean <- href_list[str_detect(href_list,"race/")==TRUE]
  
  if (nrow(results_tab)>0){
  href_tab <- str_split(href_list_clean,"/",simplify=TRUE)%>%
    as_tibble()%>%
    select(-1)%>%
    setNames(c("race_id","year","race_part"))%>%
    select(-year) #year already exist in t
  
  results_tab%>%
    bind_cols(href_tab)
  }
}
#Example
my_extract_rider_year("peter-sagan",2013)
my_extract_rider_year("maciej-bodnar",2004) #Gap year for him


#############################################
#            1.5.2. Wrangling               #
#############################################

###Wrangling rider results for a given year
my_wrangle_rider_year <- function(rider,year){
  
  
  t <- my_extract_rider_year(rider,year)
  
  if (!is.null(t)){
  t%>%
    mutate(Date=make_date(year=year,day=str_extract(Date,"^\\d+"),month=str_extract(Date,"\\d+$")))%>%
    filter(Result!="")%>% #Remove "title" rows for multi-stage races since we have the race name in race_id
    mutate(PointsPCS=ifelse(is.na(PointsPCS),0,PointsPCS))%>%
    mutate(PointsUCI=ifelse(is.na(PointsUCI),0,PointsUCI))%>%
    mutate_at(c("Distance"),as.numeric)
  }
}
#Example
my_wrangle_rider_year("peter-sagan",2013)
my_wrangle_rider_year("maciej-bodnar",2004) #Gap year for him


#############################################
#       1.5.3. Past results dataset         #
#############################################


###Extracting races prior to a race date for a given rider
my_merge_rider_past_races <- function(race_date,rider){
  
  before_race_date <- race_date-1
  rider_since_year <- dtb_rider_infos_clean$professional_since[which(dtb_rider_infos_clean$rider_id==rider)]%>%year()
  race_year <- year(race_date)
  
  map_dfr(rider_since_year:race_year,function(y){
    print(paste("Year:",y))
    tab <- my_wrangle_rider_year(rider,y)
    if (!is.null(tab)){
      tab%>%
        mutate(race_year_id=paste(race_id,as.character(year),sep="/"))
    }
  })%>%
    fill(Date,.direction = "up")%>% #Fill Date so that annex/general classification row gets the date of the last stage
    filter(Date<=before_race_date) #Keep results before the race date
}  

#Example: Sagan before Paris-Roubaix 2019
race_date <- race_infos_2014_to_2021_clean%>%
  filter(race_id=="paris-roubaix" & year=="2019")%>%
  pull(Date)
my_merge_rider_past_races(race_date,"peter-sagan")%>%View()

race_date <- ymd("2021-10-03")
rider <- "maciej-bodnar"
my_merge_rider_past_races(ymd("2021-10-03"),"maciej-bodnar")

rm(race_date,rider)



###Dataset all relevant prior results
dtb_past_results <- map_dfr(dtb_rider_infos_clean$rider_id,function(rider){
  print(paste("Rider:",rider))
  max_race_date <- dtb_rider_infos_clean$max_date[which(dtb_rider_infos_clean$rider_id==rider)]
  my_merge_rider_past_races(max_race_date,rider)
})



save(dtb_past_results,file=file.path("rda","rider_past_results.rda"))


##############################################################################
##############################################################################

###Assessing R code runtime 
end_Rfile_running <- now()
c(start_Rfile_running,end_Rfile_running)