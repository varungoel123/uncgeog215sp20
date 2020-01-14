
wdi_full <- WDIbulk()

wdi_dat <- wdi_full$Data
data(gapminder)


#wdi_dat <- wdi_dat %>% filter(Country.Name %in% unique(gapminder$country))

wdi_2018 <- wdi_dat %>% filter(year == 2001)

#wdi_indicators <- data.frame(indicator = unique(wdi_2018$Indicator.Name), flag=0)
#write_csv(wdi_indicators,"wdi_indicators.csv")

#wdi_countries <- data.frame(country= unique(wdi_2018$Country.Name), flag=0)
#write_csv(wdi_countries,"wdi_countries.csv")

#wdi_indicators_lsit <- unique(wdi_2018$Indicator.Name)

wdi_ind_chosen <- read_csv("wdi_indicators_filled.csv")
wdi_country_chosen <- read_csv("wdi_countries_filled.csv")

wdi_2018 <- wdi_2018 %>% filter(Indicator.Name %in% wdi_ind_chosen$indicator[wdi_ind_chosen$flag ==1]) %>%
  filter(Country.Name %in% wdi_country_chosen$country[wdi_country_chosen$flag ==0])
wdi_2018 <- wdi_2018[,-c(2,4,5)]

wdi_2018 <-left_join(wdi_2018,wdi_ind_chosen[,1:3],by=c("Indicator.Name"="indicator"))
wdi_2018 <- wdi_2018[,c(1,5,3)]

wdi_2018 <- spread(wdi_2018,ind_short,value)
wdi_2018 <- wdi_2018[,-c(5,11,19,27)]
wdi_2008 <-wdi_2018
########################################



wdi_2018 <- wdi_dat %>% filter(year == 2011)

#wdi_indicators <- data.frame(indicator = unique(wdi_2018$Indicator.Name), flag=0)
#write_csv(wdi_indicators,"wdi_indicators.csv")

#wdi_countries <- data.frame(country= unique(wdi_2018$Country.Name), flag=0)
#write_csv(wdi_countries,"wdi_countries.csv")

#wdi_indicators_lsit <- unique(wdi_2018$Indicator.Name)

wdi_ind_chosen <- read_csv("wdi_indicators_filled.csv")
wdi_country_chosen <- read_csv("wdi_countries_filled.csv")

wdi_2018 <- wdi_2018 %>% filter(Indicator.Name %in% wdi_ind_chosen$indicator[wdi_ind_chosen$flag ==1]) %>%
  filter(Country.Name %in% wdi_country_chosen$country[wdi_country_chosen$flag ==0])
wdi_2018 <- wdi_2018[,-c(2,4,5)]

wdi_2018 <-left_join(wdi_2018,wdi_ind_chosen[,1:3],by=c("Indicator.Name"="indicator"))
wdi_2018 <- wdi_2018[,c(1,5,3)]

wdi_2018 <- spread(wdi_2018,ind_short,value)

wdi_2018 <- wdi_2018[,-c(5,11,19,27)]
###############
# add continents
country_deets <- wdi_full$Country

#write_files
wdi_2008 <- left_join(wdi_2008,country_deets[,c(3,8,9)],by=c("Country.Name"="Table.Name"))
wdi_2018 <- left_join(wdi_2018,country_deets[,c(3,8,9)],by=c("Country.Name"="Table.Name"))

write_csv(wdi_2008,"wdi_2001.csv")
write_csv(wdi_2018,"wdi_2011.csv")

write_csv(wdi_ind_chosen[wdi_ind_chosen$flag ==1,-2],"wdi_codebook.csv")
