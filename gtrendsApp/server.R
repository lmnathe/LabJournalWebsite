### A: Lucas Nathe
### D: 4/22/2020
### U: 
### P: i) Shiny Server for google trends/COVID-19 cases

packages<- c('dplyr',
             'shiny', #shiny
             'RColorBrewer', #color pallete
             'shinydashboard', #shinty
             'plotly', #for reading .json
             'ggplot2', #plotting 
             'rgdal', #fips function
             'R.utils', #pulling covid data from github
             'ggplot2', #plotting
             'rgeos', # gBuffer
             'mapdata', #us map
             'stringi',#string work
             'stringr', #string work
             'stargazer' #exporting the coef with sig stars
)
suppressPackageStartupMessages(
  invisible(lapply(packages,library,character.only=TRUE)))
server <- function(input, output) {
  #get colors
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  
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
  us <- map_data("state")
  # INTEREST RATE DATA
  #data<- readRDS('shapefiles/animated_monthly.rds')
  dataw<- readRDS('/Users/prnathe/Documents/LucasNathe/gtrendsApp/shapefiles/animated_weekly.rds')

  gtrend_keywordsw<- c("small business loan","furlough","overdraft",
                      "stimulus check","divorce","legal zoom")
  output$plot1<-  renderPlotly({
    ggplotly(ggplot() + 
               geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
               geom_point(data=filter(dataw,keyword == gtrend_keywordsw[1]),
                          aes(x=long, y=lat, size = hits,color=log(Case),label1 = Case, label2 = location,frame = wdate))+
               scale_size_continuous(name = "Hits", range = c(0.5,7))+ # Don't know why this scale is not showing up
               #scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
               scale_colour_gradientn( name = "Log(Cases)",colours = myPalette(100))+
               my_theme2()) %>% 
      layout(title = list(text = paste0(stri_trans_general(unique(gtrend_keywordsw[1]),id = "Title"),
                                        '<br>',
                                        '</sup>')),
             annotations = 
               list(x = 1, y = -0.1, text = "Source: Search Intensity, Google Trends;Case count, Johns Hopkins University", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=15, color="black"))
      ) %>%
      layout(annotations = 
               list(x = 0.35, y = 0.1, text = paste0("Hits ~ Log(Cases):",
                                                     str_trim(strsplit(stargazer(lm(formula = hits ~ log(Case+1) + as.factor(FIPS) + as.factor(wdate),
                                                                                    data = filter(dataw,keyword == gtrend_keywordsw[[1]])),type = 'text')[7],split = ")")[[1]][2]),
                                                     "\nControlling for state and week fixed effects."
               ), 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=15, color="black"))
      )
  })
   output$plot2 <-renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(dataw,keyword == gtrend_keywordsw[2]),
                           aes(x=long, y=lat, size = hits,color=log(Case),label1 = Case, label2 = location,frame = wdate))+
                scale_size_continuous(name = "Hits", range = c(0.5,7))+ # Don't know why this scale is not showing up
                #scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                scale_colour_gradientn( name = "Log(Cases)",colours = myPalette(100))+
                my_theme2()) %>% 
       layout(title = list(text = paste0(stri_trans_general(unique(gtrend_keywordsw[2]),id = "Title"),
                                         '<br>',
                                         '</sup>')),
              annotations = 
                list(x = 1, y = -0.1, text = "Source: Search Intensity, Google Trends;Case count, Johns Hopkins University", 
                     showarrow = F, xref='paper', yref='paper', 
                     xanchor='right', yanchor='auto', xshift=0, yshift=0,
                     font=list(size=15, color="black"))
       ) %>%
       layout(annotations = 
                list(x = 0.35, y = 0.1, text = paste0("Hits ~ Log(Cases):",
                                                      str_trim(strsplit(stargazer(lm(formula = hits ~ log(Case+1) + as.factor(FIPS) + as.factor(wdate),
                                                      data = filter(dataw,keyword == gtrend_keywordsw[[2]])),type = 'text')[7],split = ")")[[1]][2]),
                                                      "\nControlling for state and week fixed effects."
                ), 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="black"))
       )
})
   output$plot3<- renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(dataw,keyword == gtrend_keywordsw[3]),
                           aes(x=long, y=lat, size = hits,color=log(Case),label1 = Case, label2 = location,frame = wdate))+
                scale_size_continuous(name = "Hits", range = c(0.5,7))+ # Don't know why this scale is not showing up
                #scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                scale_colour_gradientn( name = "Log(Cases)",colours = myPalette(100))+
                my_theme2()) %>% 
       layout(title = list(text = paste0(stri_trans_general(unique(gtrend_keywordsw[3]),id = "Title"),
                                         '<br>',
                                         '</sup>')),
              annotations = 
                list(x = 1, y = -0.1, text = "Source: Search Intensity, Google Trends;Case count, Johns Hopkins University", 
                     showarrow = F, xref='paper', yref='paper', 
                     xanchor='right', yanchor='auto', xshift=0, yshift=0,
                     font=list(size=15, color="black"))
       ) %>%
       layout(annotations = 
                list(x = 0.35, y = 0.1, text = paste0("Hits ~ Log(Cases):",
                                                      str_trim(strsplit(stargazer(lm(formula = hits ~ log(Case+1) + as.factor(FIPS) + as.factor(wdate),
                                                                                     data = filter(dataw,keyword == gtrend_keywordsw[[3]])),type = 'text')[7],split = ")")[[1]][2]),
                                                      "\nControlling for state and week fixed effects."
                ), 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="black"))
       )
})
   output$plot4<-renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(dataw,keyword == gtrend_keywordsw[4]),
                           aes(x=long, y=lat, size = hits,color=log(Case),label1 = Case, label2 = location,frame = wdate))+
                scale_size_continuous(name = "Hits", range = c(0.5,7))+ # Don't know why this scale is not showing up
                #scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                scale_colour_gradientn( name = "Log(Cases)",colours = myPalette(100))+
                my_theme2()) %>% 
       layout(title = list(text = paste0(stri_trans_general(unique(gtrend_keywordsw[4]),id = "Title"),
                                         '<br>',
                                         '</sup>')),
              annotations = 
                list(x = 1, y = -0.1, text = "Source: Search Intensity, Google Trends;Case count, Johns Hopkins University", 
                     showarrow = F, xref='paper', yref='paper', 
                     xanchor='right', yanchor='auto', xshift=0, yshift=0,
                     font=list(size=15, color="black"))
       ) %>%
       layout(annotations = 
                list(x = 0.35, y = 0.1, text = paste0("Hits ~ Log(Cases):",
                                                      str_trim(strsplit(stargazer(lm(formula = hits ~ log(Case+1) + as.factor(FIPS) + as.factor(wdate),
                                                                                     data = filter(dataw,keyword == gtrend_keywordsw[[4]])),
                                                                                  type = 'text')[7],split = ")")[[1]][2]),
                                                      "\nControlling for state and week fixed effects."
                ), 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="black"))
       )
})
   
   output$plot5<- renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(dataw,keyword == gtrend_keywordsw[5]),
                           aes(x=long, y=lat, size = hits,color=log(Case),label1 = Case, label2 = location,frame = wdate))+
                scale_size_continuous(name = "Hits", range = c(0.5,7))+ # Don't know why this scale is not showing up
                #scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                scale_colour_gradientn( name = "Log(Cases)",colours = myPalette(100))+
                my_theme2()) %>% 
       layout(title = list(text = paste0(stri_trans_general(unique(gtrend_keywordsw[5]),id = "Title"),
                                         '<br>',
                                         '</sup>')),
              annotations = 
                list(x = 1, y = -0.1, text = "Source: Search Intensity, Google Trends;Case count, Johns Hopkins University", 
                     showarrow = F, xref='paper', yref='paper', 
                     xanchor='right', yanchor='auto', xshift=0, yshift=0,
                     font=list(size=15, color="black"))
       ) %>%
       layout(annotations = 
                list(x = 0.35, y = 0.1, text = paste0("Hits ~ Log(Cases):",
                                                      str_trim(strsplit(stargazer(lm(formula = hits ~ log(Case+1) + as.factor(FIPS) + as.factor(wdate),
                                                                                     data = filter(dataw,keyword == gtrend_keywordsw[[5]])),
                                                                                  type = 'text')[7],split = ")")[[1]][2]),
                                                      "\nControlling for state and week fixed effects."
                ), 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="black"))
       )
   })
   output$plot6<-  renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(dataw,keyword == gtrend_keywordsw[6]),
                           aes(x=long, y=lat, size = hits,color=log(Case),label1 = Case, label2 = location,frame = wdate))+
                scale_size_continuous(name = "Hits", range = c(0.5,7))+ # Don't know why this scale is not showing up
                #scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                scale_colour_gradientn( name = "Log(Cases)",colours = myPalette(100))+
                my_theme2()) %>% 
       layout(title = list(text = paste0(stri_trans_general(unique(gtrend_keywordsw[6]),id = "Title"),
                                         '<br>',
                                         '</sup>')),
              annotations = 
                list(x = 1, y = -0.1, text = "Source: Search Intensity, Google Trends;Case count, Johns Hopkins University", 
                     showarrow = F, xref='paper', yref='paper', 
                     xanchor='right', yanchor='auto', xshift=0, yshift=0,
                     font=list(size=15, color="black"))
       ) %>%
       layout(annotations = 
                list(x = 0.35, y = 0.1, text = paste0("Hits ~ Log(Cases):",
                                                      str_trim(strsplit(stargazer(lm(formula = hits ~ log(Case+1) + as.factor(FIPS) + as.factor(wdate),
                                                     data = filter(dataw,keyword == gtrend_keywordsw[[6]])),type = 'text')[7],split = ")")[[1]][2]),
                                                      "\nControlling for state and week fixed effects."
                ), 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="black"))
       )
   })
  
}