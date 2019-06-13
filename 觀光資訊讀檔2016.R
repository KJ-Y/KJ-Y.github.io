library(dplyr)
library(tidyr)

#資料清理

after1<- read.csv("D:/大三下/使用R語言進行資料分析/final/after_TW_itny.csv")

colnames(after1)[7] <- "South Korea"
#View(after1)
after_TW_itny <- gather(after1, last_stop, num_traveler, HongKong, Japan, "South Korea", China, Philippines, Thailand)
#View(after_TW_itny)

#修改欄位部分
v <-c("Taiwan",mode="character",length = 768)
v[1:768] <- "Taiwan"
#View(v)
after_TW_itny2 <- cbind(after_TW_itny,v)
#View(after_TW_itny2)
temp <- after_TW_itny2$num_traveler
after_TW_itny2$num_traveler <- after_TW_itny2$v
after_TW_itny2$v <- temp
colnames(after_TW_itny2) <- c("Year","居住地","細分","Residence","last_stop","Taiwan","num_traveler")
#View(after_TW_itny2)

#先從2017年入手
after_TW_itny3 <- after_TW_itny2 %>%
  #filter(Year=="2017",居住地 == "亞洲地區")
  filter(Year=="2016")
after_TW_itny3$Residence <- as.character(after_TW_itny3$Residence)

n <- length(after_TW_itny3$Residence)
for (i in (1:n)){
  #print(after_TW_itny3$Residence[i])
  if (after_TW_itny3$Residence[i] == "U.K."){
    after_TW_itny3$Residence[i] <- "United Kingdom"
    #print(after_TW_itny3$Residence[i])
  }else if (after_TW_itny3$Residence[i] == "U.S.A."){
    after_TW_itny3$Residence[i] <- "United States of America"
    #print(after_TW_itny3$Residence[i])
  }
}

View(after_TW_itny3)
#View(before)
#開始畫圖
library(devtools)
library(rnaturalearth) 
library(rgdal)
library(sp)
#plot(ne_countries())

countries <- ne_countries()
#countries
#View(countries)
#states <- ne_states(iso_a2 = 'US')
#View(states)

#獲得世界城市所有的坐標
countries$longitude <- coordinates(countries)[,1]
countries$longitude

countries$latitude <- coordinates(countries)[,2]
countries$latitude

countries_xyresi <- countries@data %>%
  select(admin, longitude, latitude)
countries_xylast<- countries@data %>%
  select(admin, longitude, latitude)

#View(countries_xyresi)
#開始匹配
af3_match <- after_TW_itny3 %>%
  left_join(countries_xyresi, by = c('Residence' = 'admin')) %>%
  left_join(countries_xylast, by = c('last_stop' = 'admin')) %>%
  left_join(countries_xylast, by = c('Taiwan' = 'admin')) 
 
View(af3_match)

#NA部分

#HongKong
hk_x <- 114.1784
hk_y <- 22.3165
#af3_match$longitude.x[c(1,13,25,33,37,49,61,65,97,129,161)] <- hk_x
af3_match$longitude.y[c(1:32)] <- hk_x
#af3_match$latitude.x[c(1,13,25,33,37,49,61,65,97,129,161)] <- hk_y
af3_match$latitude.y[c(1:32)] <- hk_y
View(af3_match)

# #Middle East
# me_x <- 43.7134
# me_y <- 23.9812
# af3_match$longitude.x[c(6,18,30,38,42,54,66,70,102,134,166)] <- me_x
# af3_match$latitude.x[c(6,18,30,38,42,54,66,70,102,134,166)] <- me_y
# 
# #Singapore
# s_x <- 103.8678
# s_y <- 1.3555
# af3_match$longitude.x[c(8,20,32,40,44,56,68,72,104,136,168)] <- s_x
# af3_match$latitude.x[c(8,20,32,40,44,56,68,72,104,136,168)] <- s_y
# #af3_match$longitude.x[c(8,20,32,44,56,68)] <- s_x
# #af3_match$latitude.x[c(8,20,32,44,56,68)] <- s_y
# 
# View(af3_match)

library(geosphere)

flows <- gcIntermediate(af3_match[,12:13], af3_match[,10:11], sp = TRUE, addStartEnd = TRUE)

flows$counts <- af3_match$num_traveler /50000

flows$origins <- af3_match$Taiwan

flows$destinations <- af3_match$last_stop

flows$resident <- af3_match$Residence
#View(flows)

library(leaflet)
library(RColorBrewer)
library(devtools)
library(rnaturalearth) 
library(rgdal)
library(sp)

hover <- paste0("From ",flows$resident, ", ",
                flows$origins, " to ", 
                flows$destinations, ': ', 
                as.character(flows$counts))

pal <- colorFactor(brewer.pal(4, 'Set2'), flows$resident)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows, weight = ~counts, label = hover, 
               group = ~resident, color = ~pal(resident)) %>%
  addLayersControl(overlayGroups = unique(flows$resident), 
                   options = layersControlOptions(collapsed = FALSE))


# #=========嘗試break 超過180度的線=================
# library(tidyverse)
# library(maps)
# library(geosphere)
# 
# 
# 
#   inter <- gcIntermediate(c(af3_match$longitude.x, af3_match$latitude.x), c(af3_match$longitude.y, af3_match$latitude.x), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
#   inter=data.frame(inter)
#   diff_of_lon=abs(af3_match$longitude.x) + abs(af3_match$longitude.y)
#   if(diff_of_lon > 180){
#     lines(subset(inter, lon>=0), col="slateblue", lwd=2)
#     lines(subset(inter, lon<0), col="slateblue", lwd=2)
#   }else{
#     lines(inter, col="slateblue", lwd=2)
#   }

