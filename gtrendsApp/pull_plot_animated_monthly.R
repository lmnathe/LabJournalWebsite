# ?	Billing deferral 
# ?	Frozen account (this is going to be murky bc it may include many gym accounts)
# ?	Overdraft forgiveness 
# ?	Special loan program
# ?	Interest-free loan

library(tools) #titlecasing
source('/hpc/creditmarket/Lucas/relationship_distance/code/distance/theme_frb.R')

packages <- c("dplyr",'foreign','ggplot2','sp','haven',
              'rgdal','maptools','rgeos','ggplot2','jsonlite',
              'purrr','viridis','scales','stringr','ggthemes',
              'RColorBrewer','ggpubr','lubridate','gtrendsR','pracma','tools')
library(gganimate)
library(animation)
library(plotly)
### helper functions ----
my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "white")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}
my_theme2 = function() {
  my_theme() +
    theme(axis.title = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank())
}
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
#Set proxy to be able to start download
setHandleParameters(user= NULL, password= NULL, domain= NULL, proxyhost = 'wwwproxy.frb.gov', proxyport = 8080)

#### READ IN NEILSEN MAP DATA ####
neil <- readOGR("/href/research3/m1lmn03/ln/jdp/code/nielsentopo.json", "nielsen_dma", stringsAsFactors=FALSE, 
                verbose=FALSE)
neil <- SpatialPolygonsDataFrame(gBuffer(neil, byid=TRUE, width=0),
                                 data=neil@data)
neil_map <- fortify(neil, region="id")
niel_codes<-data.frame("dma"=tolower(as.character(neil@data[["dma1"]])),"id"=neil@data[["dma"]],
                       "lat"=neil@data[["latitude"]],"long"=neil@data[["longitude"]]) %>%
  mutate(dma=as.character(dma)) %>% arrange(dma)



data<-data.frame()
#trend-by-state
#gtrend_keywords<- c("personal loan","student loan","payment delay","payment deferral","payday loan","forebearance")
gtrend_keywords<- c("hardship program","forebearance","payment delay","payday loan","pawnshop","personal loan","cash loan")
time1<- seq.Date(from = as.Date("2019-11-01",format="%Y-%m-%d"),
                 to = Sys.Date(),by ="month")
time2<- c(seq(as.Date("2019-12-01"),length=5,by="months")-1,Sys.Date()-1)
#adding yesterdays date to incomplete month
#date.end.month <- c(seq(as.Date("2020-01-01"),length=4,by="months")-1,Sys.Date()-1)
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
data<- data %>% rowwise() %>% mutate(dates = format(as.Date(strsplit(date, " ")[[1]][1],format = "%Y-%m-%d"),"%Y-%m")) %>% ungroup()
data<- data %>% arrange(date)
saveRDS(data,'/href/scratch3/m1lmn03/corona/gtrends/data/animated_monthly.rds')
#data<-readRDS('/href/scratch3/m1lmn03/corona/gtrends/data/animated_monthly.rds')









