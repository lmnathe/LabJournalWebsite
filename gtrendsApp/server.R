### A: Lucas Nathe
### D: 3/17/2020
### U: 
### P: i) Shiny Server for JDP ISR content
library(shiny)
library(dplyr)
library(shinydashboard)
#library(policyPlot)
library(plotly)
library(rgdal)
library(R.utils)
library(ggplot2)
library(rgeos)
library(mapdata)
# Define server logic to plot various JDP ISR data ----
server <- function(input, output) {
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
  source("theme_frb.R") #pp theme
  # INTEREST RATE DATA
  data<- readRDS('animated_monthly.rds')
  neil <- readOGR("nielsentopo.json", "nielsen_dma", stringsAsFactors=FALSE, 
                  verbose=FALSE)
  neil <- SpatialPolygonsDataFrame(gBuffer(neil, byid=TRUE, width=0),
                                   data=neil@data)
  neil_map <- fortify(neil, region="id")

  gtrend_keywords<- c("hardship program","forebearance","payment delay","payday loan","pawnshop","personal loan","cash loan")
  
  output$plot1 <- renderPlotly({
   # ggplotly(ggplot() + geom_polygon(data = neil_map,aes(x = long,y=lat,group=group))   +
   #                  geom_point(data=filter(data,keyword == gtrend_keywords[1]),aes(x=long, y=lat, size = hits,frame = dates),color = 'purple')+
   #                  scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
   #                  labs(size = 'Hits',title = capitalize(unique(gtrend_keywords[1])))+
   #                     theme(legend.position = 'top')+
   #                  theme(legend.position = c(0, 1),legend.justification = c(0, 1)))
    ggplotly(ggplot() + 
               geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                     geom_point(data=filter(data,keyword == gtrend_keywords[1]),
                                aes(x=long, y=lat, size = hits,frame = dates,name=location),color = 'purple')+
                     scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                     labs(size = 'Hits',title = capitalize(unique(gtrend_keywords[1])))+
                     my_theme2()+
                        theme(legend.position = 'top')+
                     theme(legend.position = c(0, 1),legend.justification = c(0, 1)))
  })
   output$plot2 <-renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(data,keyword == gtrend_keywords[2]),
                           aes(x=long, y=lat, size = hits,frame = dates,name=location),color = 'purple')+
                scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                labs(size = 'Hits',title = capitalize(unique(gtrend_keywords[2])))+
                my_theme2()+
                theme(legend.position = 'top')+
                theme(legend.position = c(0, 1),legend.justification = c(0, 1)))
})
   output$plot3<- renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(data,keyword == gtrend_keywords[4]),
                           aes(x=long, y=lat, size = hits,frame = dates,name=location),color = 'purple')+
                scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                labs(size = 'Hits',title = capitalize(unique(gtrend_keywords[4])))+
                my_theme2()+
                theme(legend.position = 'top')+
                theme(legend.position = c(0, 1),legend.justification = c(0, 1)))
})
   output$plot4<-renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(data,keyword == gtrend_keywords[5]),
                           aes(x=long, y=lat, size = hits,frame = dates,name=location),color = 'purple')+
                scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                labs(size = 'Hits',title = capitalize(unique(gtrend_keywords[5])))+
                my_theme2()+
                theme(legend.position = 'top')+
                theme(legend.position = c(0, 1),legend.justification = c(0, 1)))
})
   
   output$plot5<- renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(data,keyword == gtrend_keywords[6]),
                           aes(x=long, y=lat, size = hits,frame = dates,name=location),color = 'purple')+
                scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                labs(size = 'Hits',title = capitalize(unique(gtrend_keywords[6])))+
                my_theme2()+
                theme(legend.position = 'top')+
                theme(legend.position = c(0, 1),legend.justification = c(0, 1)))
   })
   output$plot6<-  renderPlotly({
     ggplotly(ggplot() + 
                geom_polygon(data=us,aes(x=long,y=lat, group=group),color = "#2b2b2b",fill="white")+
                geom_point(data=filter(data,keyword == gtrend_keywords[7]),
                           aes(x=long, y=lat, size = hits,frame = dates,name=location),color = 'purple')+
                scale_size_continuous(range = c(1,8),breaks = c(25,50,75,100))+
                labs(size = 'Hits',title = capitalize(unique(gtrend_keywords[7])))+
                my_theme2()+
                theme(legend.position = 'top')+
                theme(legend.position = c(0, 1),legend.justification = c(0, 1)))
   })
  # 
  # output$plot4<- renderPlotly({
  #   if(is.null(input$inCheckboxGroup_finance)){
  #     ggplotly(ggplot(data=term,
  #                     aes(x=date,y=mavg_ft))+
  #                geom_line()+
  #                theme_frb()+
  #                labs(x="Date",y="Average Term")
  #              
  #     )
  #   }
  #   else{
  #     ggplotly(ggplot(data=filter(term,Manufacturer %in% input$inCheckboxGroup_finance),
  #                     aes(x=date,y=avg_ft,color=Manufacturer))+
  #                geom_line()+
  #                theme_frb()+
  #                labs(x="Date",y="Average Term") 
  #     )
  #   }
  # })
  # 
  # output$plot5<- renderPlotly({
  #   if(is.null(input$inCheckboxGroup_lease)){
  #     ggplotly(ggplot(data=term,
  #                     aes(x=date,y=mavg_lt))+
  #                geom_line()+
  #                theme_frb()+
  #                labs(x="Date",y="Average Lease Term")
  #     )
  #   }
  #   else{
  #     ggplotly(ggplot(data=filter(term,Manufacturer %in% input$inCheckboxGroup_lease),
  #                     aes(x=date,y=avg_lt,color=Manufacturer))+
  #                geom_line()+
  #                theme_frb()+
  #                labs(x="Date",y="Average Lease Term") 
  #     )
  #   }
  # })
  # 
  # output$plot6<- renderPlotly({
  #   ggplotly(ggplot(data=dp_m, aes(x=reorder(Manufacturer,-m_dpf),y=m_dpf,fill=Manufacturer)) +
  #              geom_bar(stat = "identity")+
  #              theme_frb()+
  #              labs(x="",y="Average Down Payment")+
  #              theme(axis.text.x = element_text(size=10, angle=45, hjust=1, vjust=1))
  #   )
  # })
  # 
  # output$plot7<- renderPlotly({
  #   ggplotly(ggplot(data=dp_m, aes(x=reorder(Manufacturer,-m_dpl),y=m_dpl,fill=Manufacturer)) +
  #              geom_bar(stat = "identity")+
  #              theme_frb()+
  #              labs(x="",y="Average Lease Down Payment")+
  #              theme(axis.text.x = element_text(size=10, angle=45, hjust=1, vjust=1))+
  #              guides(fill=guide_legend(ncol=2))
  #   )
  # })
  # 
  
}