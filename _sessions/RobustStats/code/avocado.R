library(geonames)
library(nasapower)
library(tidyverse)
options(geonamesUsername="schultem")
options(geonamesHost="api.geonames.org")


# read data
avocado = read_csv('_sessions/RobustStats/1_Data/avocado.csv')

handle_nam = function(nam){
  tmp = str_replace_all(nam,"([A-Z])", " \\1")
  tmp = str_replace(tmp,"^[:space:]*",'')
  tmp = str_replace(tmp,"[:space:]+",' ')
  tmp}

handle_nam2 = function(nam){
  tmp = str_replace_all(nam,"([A-Z])", " \\1")
  tmp = str_replace(tmp,"^[:space:]*",'')
  tmp = str_split(tmp,"[:space:]+")[[1]]
  if(length(tmp)>1){
    tmp2 =   paste(tmp[1:(length(tmp)-1)],tmp[2:length(tmp)])
    tmp = c(tmp2, tmp)
    }
  tmp}

# GET WHEATHER -----------------------------------

get_wheather = function(nam){
  
  orig_nam = nam
  
  # get candidates
  res = geonames::GNsearch(name = handle_nam(nam), 
                           country="US", maxRows = 1000)
  res = res %>% filter(fclName %in% c("city, village,..."))
  
  # check
  if(nrow(res) == 0){
    nams = handle_nam2(nam)
    for(i in 1:length(nams)){
      nam = nams[i]
      res = geonames::GNsearch(name = nam, 
                               country="US", maxRows = 1000)
      res = res %>% filter(fclName %in% c("city, village,..."))
      if(nrow(res)>0) break 
    }
  }
  
  # check
  if(nrow(res) == 0){
    nams = handle_nam2(nam)
    for(i in 1:length(nams)){
      nam = nams[i]
      res = geonames::GNsearch(name = nam, 
                               country="US", maxRows = 1000)
      res = res %>% filter(fclName %in% c("country, state, region,...",
                                          "spot, building, farm"))
      if(nrow(res)>0) break 
    }
  }
  
  # get location
  ord = order(stringdist::stringdist(res$name, nam))
  inf_nam = res$name[ord][1] 
  inf_loc = as.numeric(unlist(res[ord,c('lng','lat')][1,]))

  cat(orig_nam,' ', inf_nam,'\n', sep='')
    
  # get wheather
  # RH2M - Relative Humidity at 2 Meters
  # T2M - Temperature at 2 Meters
  # PRECTOT - Precipitation
  wheather = get_power(community = "AG",
                       lonlat = inf_loc,
                       pars = c("RH2M", "T2M", "PRECTOT"),
                       dates = c("2015-01-04", "2018-03-25"),
                       temporal_average = "DAILY")
  
  tibble(location = orig_nam,
         inferred_location = inf_nam,
         longitude = inf_loc[1],
         latitude = inf_loc[2],
         date = wheather$YYYYMMDD,
         humidity = wheather$RH2M,
         temperature = wheather$T2M,
         precipation = wheather$PRECTOT)
  }

get_wheather_total = function(wheather){

  w = wheather[names(wheather) != "TotalUS"]
  wn = do.call(rbind, w)
  wn = wn %>% group_by(date) %>%
    summarize(
      humidity = mean(humidity),
      temperature = mean(temperature),
      precipation = mean(precipation)
    )
  
  tibble(location = "TotalUS",
         inferred_location = "US_total",
         longitude = NA,
         latitude = NA,
         date = wn$date,
         humidity = wn$humidity,
         temperature = wn$temperature,
         precipation = wn$precipation)
  }




locations = unique(avocado$region) ; length(locations)
wheather = lapply(locations, get_wheather)
names(wheather) = locations

wheather[["NorthernNewEngland"]] = get_wheather("NewEngland")
wheather[["TotalUS"]] = get_wheather_total(wheather)

all_wheather = do.call(rbind, wheather[names(wheather) != "TotalUS"])

all_wheather$humidity_usa = wheather[["TotalUS"]]$humidity
all_wheather$temperature_usa = wheather[["TotalUS"]]$temperature
all_wheather$precipation_usa = wheather[["TotalUS"]]$precipation

a = wheather[["TotalUS"]] 

avocado_w = avocado %>%
  left_join(all_wheather, by = c("region" = "location", "Date" = "date")) %>%
  filter(region != "NorthernNewEngland",
         region != "TotalUS") %>%
  mutate(month = month(Date),
         season = case_when(
         month %in% c(12,1,2) ~ "winter",
         month %in% c(3,4,5) ~ "spring",
         month %in% c(6,7,8) ~ "summer",
         month %in% c(9,10,11) ~ "autumn"
         ),
         volume_index = (`Total Volume`)**.1,
         volume_index = (volume_index-1.5) * 2.7) %>%
  select(`Total Volume`,volume_index, AveragePrice, type, temperature, temperature_usa,
         humidity, humidity_usa, precipation, precipation_usa, year, season, Date, region, longitude, latitude) %>%
  rename(date = Date,
         volume = `Total Volume`,
         price_per_avocado = AveragePrice,
         precipitation = precipation,
         precipitation_usa = precipation_usa)


avocado_w %>%
  filter(region == "California") %>%
  group_by(region, year) %>%
  dplyr::summarize(m = mean(volume)) %>%
  arrange(desc(m))


# cali
avocado_cali = avocado_w %>%
  filter(region == 'California')

# # new york
# avocado_ny = avocado_w %>%
#   filter(region == 'NewYork')

avocado_ny$volume = avocado_ny$volume * 3.9
avocado_ny$volume_index = avocado_ny$volume ** .1
avocado_ny$volume_index = (avocado_ny$volume_index-1.5) * 2.7

write_csv(avocado_w, '_sessions/RobustStats/1_Data/avocado.csv')
write_csv(avocado_cali, '_sessions/RobustStats/1_Data/avocado_cali.csv')
write_csv(avocado_ny, '_sessions/RobustStats/1_Data/avocado_ny.csv')
dim(avocado_w)
dim(avocado_cali)
dim(avocado_ny)

require(tidyverse)
avo = read_csv('_sessions/RobustStats/1_Data/avocado_en.csv')
avo_c = read_csv('_sessions/RobustStats/1_Data/avocado_en_cali.csv')

avo = avo %>% rename(
  Verkaufsvolumen = volume,
  Verkaufsvolumen_index = volume_index,
  Preis = price_per_avocado,
  Typ = type,
  Temparatur = temperature,
  Temperatur_USA = temperature_usa,
  Luftfeuchtigkeit = humidity ,
  Luftfeuchtigkeit_USA = humidity_usa,
  Niederschlag = precipitation,
  Niederschlag_USA = precipitation_usa,
  Jahr = year,
  Jahreszeit = season,
  Datum = date,
  Region = region,
  Längengrad = longitude,
  Breitengrad = latitude
  )

avo_c = avo_c %>% rename(
  Verkaufsvolumen = volume,
  Verkaufsvolumen_index = volume_index,
  Preis = price_per_avocado,
  Typ = type,
  Temparatur = temperature,
  Temperatur_USA = temperature_usa,
  Luftfeuchtigkeit = humidity ,
  Luftfeuchtigkeit_USA = humidity_usa,
  Niederschlag = precipitation,
  Niederschlag_USA = precipitation_usa,
  Jahr = year,
  Jahreszeit = season,
  Datum = date,
  Region = region,
  Längengrad = longitude,
  Breitengrad = latitude
  )

write_csv(avo, '_sessions/RobustStats/1_Data/avocado.csv')
write_csv(avo_c, '_sessions/RobustStats/1_Data/avocado_cali.csv')
write_csv(avocado_ny, '_sessions/RobustStats/1_Data/avocado_ny.csv')

cat(paste0(names(avo),' = ,'),sep='\n')










plot(avocado_cali$temperature, avocado_cali$volume)


m1 = glm(volume ~ price_per_avocado, data = avocado_cali)
summary(m1)
plot(predict(m1),residuals(m1))

plot(avocado_cali$temperature, avocado_cali$volume)

m1 = lm(volume_index ~ price_per_avocado, data = avocado_cali)
summary(m1)
plot(predict(m1),residuals(m1))

m1 = lm(volume_index ~ temperature + type, data = avocado_cali)
summary(m1)
plot(predict(m1),residuals(m1))

m1 = lm(volume_index ~ season + temperature + type, data = avocado_cali)
summary(m1)
plot(predict(m1),residuals(m1))



NewYork

unique(avocado_w$region)

range(avocado_w$volume_level)

cor(avocado_w)

require(lme4)

m1 = lmer(volume_level ~ type + (1 + type | region) , data=avocado_w)  ; plot(predict(m1),residuals(m1))
m2 = lmer(volume_level ~ type + price_per_avocado + (1 + type + price_per_avocado | region) , data=avocado_w) ; plot(predict(m2),residuals(m2))
m3 = lmer(volume_level ~ type + price_per_avocado + year + season + (1 + type + price_per_avocado | region) , data=avocado_w) ; plot(predict(m3),residuals(m3))



summary(m2)


m1 = lm(volume_level ~   price_per_avocado + temperature, data=avocado_w)


m2 = lmer(volume_level ~ type + price_per_avocado + I(scale(price_per_avocado)**2) + temperature + (1 + type | region) , data=avocado_w) 
plot(predict(m2),residuals(m2),col=ifelse(avocado_w$type=="organic",'red','blue'))


m = lm(volume_level ~   price_per_avocado, data=avocado_w)
plot(predict(m),m$residuals)


m3 = lmer(volume_level ~ price_per_avocado + temperature + (1 | region), data=avocado_w)

plot(predict(m1),avocado_w$volume_level)

plot(predict(m1),m1$residuals)
plot(predict(m2),residuals(m2),col=ifelse(avocado_w$type=="organic",'red','blue'))
plot(predict(m2),avocado_w$volume_level)

summary(m2)

nams = names(avocado_w)
nams = nams[!nams %in% c("volume","volume_level","type","region")]
clas = sapply(avocado_w[,nams], class)

regs = list()
for(i in 1:length(nams)) {
  if(clas[i] == "numeric"){
  m = lmer(volume_level ~ I(avocado_w[[nams[i]]]) +  I(scale(avocado_w[[nams[i]]])**2) + (1 + type | region), data=avocado_w)
  } else {
  m = lmer(volume_level ~ I(avocado_w[[nams[i]]]) + (1 | region) + (1 | type), data=avocado_w)
  }
  regs[[i]] = summary(m)$coef[,3]
  }
names(regs) = nams

                                   

plot(avocado_w$price_per_avocado,avocado_w$volume_level)

table(avocado_w$region[avocado_w$volume_level>4.2],avocado_w$season[avocado_w$volume_level>4.2])
table(avocado_w$region[avocado_w$volume_level<4.5])

plot(avocado_w$date[avocado_w$region == "California"],avocado_w$volume_level[avocado_w$region == "California"])

avocado %>% filter(region == "California") %>% print(n = 1000)


plot(predict(m2),residuals(m2))
plot(predict(m3),residuals(m3))


plot(avocado_w$price_per_avocado,predict(m3))
plot(avocado_w$price_per_avocado, avocado_w$volume)

plot(avocado_w$temperature,predict(m3))
plot(avocado_w$temperature, avocado_w$volume)


summary(m1)
summary(m2)

tapply(avocado_w$temperature, avocado_w$region, function(x) sum(is.na(x)))

spl_avo = split(avocado_w, avocado_w$region)

regs = list()
for(i in 1:length(spl_avo)) regs[[i]] = lm(volume ~ price_per_avocado + temperature, data=spl_avo[[i]])$coeff


summary(m1)
summary(m2)

anova(m1, m2)


cor(avocado_w$volume,avocado_w$price_per_avocado)
cor(avocado_w$temperature,avocado_w$price_per_avocado,use="complete.obs")




cor(avocado_w$`Total Volume`, avocado_w$temperature, use="complete.obs")

a = avocado_w %>%
  filter(region == "California")

cor(a$`AveragePrice`, a$temperature)
  

plot(avocado_w$Date, avocado_w$`Total Volume`)

plot(avocado_w$Date, avocado_w$AveragePrice)

m = lmer(data = avocado_w)



w = do.call(rbind, wheather)

w %>% 
  filter(location != inferred_location) %>% 
  select(location, inferred_location) %>%
  unique() %>% print(n = 20)


# GET WHEATHER -----------------------------------


with(a, plot(age, G1))

require(tidyverse)

a = read_csv('_sessions/RobustStats/1_Data/avocado.csv')
a = subset(a, year == 2017)

r = lm(AveragePrice ~ Date,data=a)
a$resid = r$residuals
a$predi = predict(r)

set.seed(27)
ggplot(a[sample(1:nrow(a),200),], mapping = aes(Date, AveragePrice)) + geom_point() + theme_bw() +
  labs(y = 'Average price') + 
  geom_segment(aes(x = Date, xend=Date,y = AveragePrice, yend = predi),col='red')+
  geom_smooth(method='lm', col = 'steelblue', lwd=2, se=FALSE)


cor.test(formula = ChickWeight, data = ChickWeight)
