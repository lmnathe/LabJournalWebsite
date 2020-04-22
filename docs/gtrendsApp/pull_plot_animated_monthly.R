
packages<- c('gtrendsR', #googletrends
             'dplyr', 
             'rgdal', #for reading .json
             'ggplot2', #plotting 
             'cdlTools', #fips function
             'RCurl', #pulling covid data from github
             'stringr') #converting to plotly
suppressPackageStartupMessages(
  invisible(lapply(packages,library,character.only=TRUE)))

#PULL COVID CASES DATA
url <- getURL('https://raw.githubusercontent.com/datasets/covid-19/master/data/us_confirmed.csv')
cases<- read.csv(text = x)
cases<-cases %>% group_by(Province.State,Date) %>% 
  summarise(Case= sum(Case,na.rm = T)) %>% 
  ungroup() %>%
  mutate(FIPS = fips(Province.State,to = "FIPS")) %>%
  mutate(mdate = format(as.Date(x = Date,format = "%Y-%m-%d"),"%Y-%m")) %>%
  group_by(mdate,FIPS) %>%
  summarise(Case = max(Case,na.rm = T))
# expand back to november filling cases with 0

cases<- cases %>% as.data.frame() %>%
  ungroup() %>%
  complete(mdate = format(seq.Date(
    as.Date("2019-11-01",format = "%Y-%m-%d"),
    #as.Date("2020-05-01",format = "%Y-%m-%d"),
    max(as.Date(paste0(mdate,"-01"),format = "%Y-%m-%d")),
    by="month"),"%Y-%m"),FIPS) %>%
  mutate(Case = ifelse(is.na(Case),0,Case))


data<-data.frame()
#trend-by-dma
gtrend_keywords<- c("small business loan","furlough","overdraft",
                    "stimulus check","divorce","legal zoom")
time1<- seq.Date(from = as.Date("2019-11-01",format="%Y-%m-%d"),
                 to = Sys.Date(),by ="month")
time2<- c(seq(as.Date("2019-12-01"),length=5,by="months")-1,Sys.Date()-1)
#adding yesterdays date to incomplete month
i<-1
x<-1
for(i in 1:length(gtrend_keywords)){
  for(x in 1:length(time2)){
    last_90_c19<-gtrends(c(gtrend_keywords[i]), geo = c("US"), 
                         #time = "now 7-d",
                         #time = paste((Sys.Date() -1 - weeks(1)), Sys.Date()-1),
                         time = paste(time1[x], time2[x]),
                         gprop="web", hl="en-US",
                         low_search_volume = TRUE)
    tmp<-data.frame(last_90_c19$interest_by_dma) %>%
      filter(!location %in% c("Anchorage AK","Honolulu HI","Fairbanks AK","Juneau AK")) %>%
      mutate(location = ifelse(location == "Florence-Myrtle Beach SC","myrtle beach-florence, SC",location),
             date = paste(time1[x],time2[x])) %>%
      arrange(location)
    tmp<-cbind(tmp,niel_codes)
    
    data<-bind_rows(data,tmp)
    
    x<-x+1
  }
  #Sys.sleep(10)
  i<-i+1
}
data<- data %>% rowwise() %>% mutate(mdate = format(as.Date(strsplit(date, " ")[[1]][1],format = "%Y-%m-%d"),"%Y-%m"),
                                     FIPS = fips(ifelse(
                                       str_sub(location,-2,-1)=="D)","DC",
                                       str_sub(location,-2,-1)),to="FIPS")
                                     )%>% ungroup()
data<- data %>% arrange(date)
data<- data %>% left_join(cases,by = c("mdate","FIPS"))




saveRDS(data,'shapefiles/animated_monthly.rds')









