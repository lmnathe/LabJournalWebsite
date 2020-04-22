
packages<- c('gtrendsR', #googletrends
             'dplyr', 
             'rgdal', #for reading .json
             'ggplot2', #plotting 
             'cdlTools', #fips function
             'RCurl', #pulling covid data from github
             'stringr',
             'lubridate', #converting to plotly
             'rgeos') #gBuffer
suppressPackageStartupMessages(invisible(lapply(packages,library,character.only=TRUE)))

#PULL COVID CASES DATA
url <- 'https://raw.githubusercontent.com/datasets/covid-19/master/data/us_confirmed.csv'
cases<- download.file(url = url, destfile = basename(url))
cases<- readr::read_csv("us_confirmed.csv")
cases<-cases %>% group_by(`Province/State`,Date) %>% 
  summarise(Case= sum(Case,na.rm = T)) %>% 
  ungroup() %>%
  mutate(FIPS = fips(`Province/State`,to = "FIPS")) %>%
  mutate(wdate = floor_date(as.Date(x = Date,format = "%Y-%m-%d"),'week')) %>%
  group_by(wdate,FIPS) %>%
  summarise(Case = max(Case,na.rm = T))
# expand back to november filling cases with 0

cases<- cases %>% ungroup()%>%
  as.data.frame() %>%
  tidyr::complete(wdate = seq.Date(
    as.Date("2019-11-03",format = "%Y-%m-%d"),
    #as.Date("2020-05-01",format = "%Y-%m-%d"),
    max(cases$wdate),
    by="week"),FIPS) %>%
  mutate(Case = ifelse(is.na(Case),0,Case))


#### READ IN NEILSEN MAP DATA ####
neil <- readOGR("shapefiles/nielsentopo.json", "nielsen_dma", stringsAsFactors=FALSE, 
                verbose=FALSE)
neil <- SpatialPolygonsDataFrame(gBuffer(neil, byid=TRUE, width=0),
                                 data=neil@data)
neil_map <- fortify(neil, region="id")
niel_codes<-data.frame("dma"=tolower(as.character(neil@data[["dma1"]])),"id"=neil@data[["dma"]],
                       "lat"=neil@data[["latitude"]],"long"=neil@data[["longitude"]]) %>%
  mutate(dma=as.character(dma)) %>% arrange(dma)



data<-data.frame()
#trend-by-dma
gtrend_keywords<- c("small business loan","furlough","overdraft",
                    "stimulus check")
time1<- seq.Date(from = as.Date("2019-11-03",format="%Y-%m-%d"),
                 to = Sys.Date(),by ="week")
time2<- c(seq(as.Date("2019-11-10"),length=length(time1)-1,by="week"),Sys.Date()-1)
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
             date = paste(time1[x],time2[x]),
             hits = as.numeric(hits)) %>%
      arrange(location)
    tmp<-cbind(tmp,niel_codes)
    
    data<-bind_rows(data,tmp)
    
    x<-x+1
  }
  #Sys.sleep(10)
  i<-i+1
}
backup<-data
data<-backup
data<- data %>% 
  rowwise() %>%
  mutate(wdate = 
           as.Date(strsplit(date, " ")[[1]][1],format = "%Y-%m-%d"),
                                     FIPS = fips(ifelse(
                                       str_sub(location,-2,-1)=="D)","DC",
                                       str_sub(location,-2,-1)),to="FIPS")
                                     )%>% ungroup()
data<- data %>% arrange(date)
data<- data %>% left_join(cases,by = c("wdate","FIPS"))
saveRDS(data,'shapefiles/animated_weekly.rds')









