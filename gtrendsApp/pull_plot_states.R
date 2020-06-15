### A: Lucas Nathe 
### D: 3/19/2020
### P: 


library(gtrendsR) #pull google trends
library(lubridate) 
library(dplyr)
library(ggplot2) #plotting
library(tools) #titlecasing
#source('/Users/prnathe/Documents/LucasNathe/gtrendsApp/theme_custom.R')
#source('theme_custom.R')
#Set proxy to be able to start download

data<-data.frame()
#trend-by-state
gtrend_keywords<- c("small + business + loan","furlough","overdraft",
                    "stimulus + check",'divorce','legal + zoom')
gtrend_geos<-c("US","US-WA","US-NY","US-CA")
i<-1
x<-1
for(i in 1:length(gtrend_keywords)){
  x<-1
  for(x in 1:length(gtrend_geos)){
    last_90_c19<-gtrends(c(gtrend_keywords[i]), geo = c(gtrend_geos[x]), time = paste("2019-12-01",Sys.Date()-1), gprop="web", hl="en-US",
                         low_search_volume = FALSE)
    if(is.null(last_90_c19$interest_over_time)){
      tmp<-data.frame(date = NA,
                      hits = NA,
                      geo = gtrend_geos[x],
                      keyword = gtrend_keywords[i])
    }
    else{
      tmp<- data.frame(
        date = as.character(last_90_c19$interest_over_time$date),
        hits = as.numeric(ifelse(last_90_c19$interest_over_time$hits=="<1",0,
                                 last_90_c19$interest_over_time$hits)),
        geo = last_90_c19$interest_over_time$geo,
        keyword = gtrend_keywords[i]
      )
    }

    data<-bind_rows(data,tmp)
    
    x<-x+1
  }
  i<-i+1
}


data<- data %>% 
  mutate(date2 = as.Date(date),
         dot_date = 
           ifelse(geo=="US-WA",
                  "2020-01-20",
                  ifelse(geo == "US-NY",
                         "2020-03-02",
                         ifelse(geo=="US-CA",
                                "2020-01-25",NA))))
data<- data %>%
  group_by(geo) %>% 
  mutate(dot_val = ifelse(date2==dot_date,hits,NA))
#saveRDS(data,'/Users/prnathe/Documents/LucasNathe/gtrendsApp/shapefiles/states.rds')
saveRDS(data,'shapefiles/states.rds')
### STATES AND US ----
# p<-1
# for(p in 1:length(gtrend_keywords)){
#   tmp<-ggplot() + 
#     geom_line(data = data %>% filter(keyword == gtrend_keywords[p] & !is.na(hits) & geo!="US"), 
#               mapping = aes(as.Date(date), hits,color = geo))+
#     geom_line(data = data %>% filter(keyword == gtrend_keywords[p] & !is.na(hits) & geo=="US"), 
#               mapping = aes(as.Date(date), hits,color = geo),size =1.25)+
#     geom_point(data = data %>% filter(keyword == gtrend_keywords[p] & !is.na(hits) & geo!="US"),
#                mapping=aes(x = as.Date(dot_date),y=dot_val,color = geo),size=3)+
#     #geom_line(aes(size=data$line_size)) +
#     theme_frb()+
#     xlab("") + 
#     scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") + 
#     ylab("Google search index") + ylim(0,100) + ggtitle(paste0("Search term:",toTitleCase(gtrend_keywords[p]))) +
#     theme(plot.title = element_text(size = 12, face="bold", margin = margin(10,0,10,0)),
#           legend.text = element_text(size = 7.5))+
#     #guides(color = guide_legend(ncol=2))+
#     geom_vline(aes(xintercept = as.numeric(as.Date("2020-01-20"))),linetype="dotted",color="black")+
#     geom_text(aes(x = as.Date("2020-01-15"),label = "First US case of\nCOVID-19",y=95),color="black",size=2.25,nudge_x = 12.5,family="Times")
#   assign(gsub(x = gtrend_keywords[p],pattern = " ",replacement = "_"),
#          tmp)
#   p<-p+1
# }
