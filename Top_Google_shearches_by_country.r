
library(rvest)
library(dplyr)
library(calibrate)
library(stringi)
library(ggplot2)
library(maps)
library(ggmap)
library(stringr)
library(gtrendsR)
library(dplyr)

wiki= read_html("https://en.wikipedia.org/wiki/Member_states_of_the_United_Nations")

countries=wiki %>%
  html_nodes("table") %>%
    .[[2]]%>%
  html_table(fill=T)

countries[,1]=stri_sub(countries[,1],3)
countries[,1]= gsub("\\(.*)","",countries[,1])
countries[,1]= gsub("\\[.*]","",countries[,1])
countries= countries[,1]

length(countries)

countries[1:10]

gconnect(username, password)

data=gtrends('india')
z=as.data.frame(data$searches)
names(z)=c('searches','hits')
z=filter(z,hits>10)

 z$searches <- factor(z$searches, levels = z$searches[order(z$hits,decreasing =F)])  # re-order the terms

ggplot(z, aes(searches,hits))+ 
  geom_bar(stat='identity',fill="skyblue",color='black')+ylab('Hits')+
  theme(axis.title.y=element_blank(), axis.text.y = element_text(colour="black",size=10),
        axis.title.x = element_text(colour="blue",size=14),
        axis.text.x = element_text(colour="black",size=14,angle=90,hjust=.5,
                                   vjust=.5))+coord_flip()+
  ggtitle('Related top searches for India')+theme(plot.title = element_text(size = 14,colour="blue"))


regions = as.data.frame(data$regions)

names(regions)=c('region','hits')

regions$region[regions$region=="United States"] = "USA"

world_map = map_data("world")

world_map =merge(world_map, regions, by="region",all.x = TRUE)

world_map = world_map[order(world_map$group, world_map$order),]

g=ggplot(world_map, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=hits), color="gray70") 

g+theme(axis.text.y   = element_blank(),
        axis.text.x   = element_blank(),
        axis.title.y  = element_blank(),
        axis.title.x  = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
scale_fill_gradient(low = "skyblue", high = "blue", guide = "colorbar",na.value="white")+
 theme(legend.key.size = unit(1, "cm"),
         legend.title = element_text(size = 12, colour = "blue"),
         legend.title.align=0.3,legend.text = element_text(size = 10))+
         theme(panel.border = element_rect(colour = "gray70", fill=NA, size=0.5))

searches=c()

for(i in 1:dim(countries)[1]){
    data=gtrends(as.character(countries[i,1]))
    z=data$searches
    top_5=paste(as.data.frame(z)[,1][1],collapse=", ")
    searches=c(searches,top_5)
    
}

Data=as.data.frame(list(Country=countries, Top_searches=searches))

Data[sapply(searches, function(x) "war"%in%unlist(str_split(x,' '))),]

Data[sapply(searches, function(x) "air"%in%unlist(str_split(x,' '))| 
    "airways"%in%unlist(str_split(x,' '))),]

Data[sapply(searches, function(x) "hotel"%in%unlist(str_split(x,' '))),]

Data[sapply(searches, function(x) "ancient"%in%unlist(str_split(x,' '))),]

Data[sapply(searches, function(x) "weather"%in%unlist(str_split(x,' '))),]

Data[sapply(searches, function(x) "map"%in%unlist(str_split(x,' '))),]

Data[sapply(searches, function(x) "africa"%in%unlist(str_split(x,' '))),]
