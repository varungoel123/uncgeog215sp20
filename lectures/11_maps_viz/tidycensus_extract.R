library(tidycensus)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(janitor)

###### Download full cafo file
if(!file.exists("nc_cafo19.xls")){
  download.file(url = "https://files.nc.gov/ncdeq/List_Of%20Permitted_Animal_Facilities2019-11-06.xls",
                destfile = "nc_cafo19.xls")
}
nc_cafo <-read_excel("nc_cafo19.xls", sheet = 1)
nc_cafo <-read_excel("nc_cafo19.xls", sheet = 1,skip = 2)


download.file(url = "https://www.countyhealthrankings.org/sites/default/files/media/document/state/downloads/2018%20County%20Health%20Rankings%20North%20Carolina%20Data%20-%20v3.xls",
                destfile = "nc_health.xls")

nc_cafo <-read_excel("nc_health.xls", sheet = 3,skip = 1)
nc_cafo <-read_excel("nc_cafo19.xls", sheet = 1,skip = 2)
<>

nc_cafo <- nc_cafo %>% clean_names(case = "snake")

nc_cafo %>% group_by(regulated_operation) %>% count()

cafo_hog <- nc_cafo %>% filter(regulated_operation == "Swine")



census_api_key("13291b78ef1a3db6eb0f30ddf99d75d8d76abdd0", install = T)

v17 <- load_variables(2018, "acs5", cache = TRUE)

inc_cols <- v17 %>% slice(label %>% str_detect("Median household income") %>%
                            which())
## choose 1,3,4,5,6,9,10
inc_cols_var <- v17 %>% slice(1,3,4,5,6,9,10) %>% .$name

##
pop_cols <- v17 %>% slice(name %>% str_detect("B01001") %>%
                                        which()) %>%
  filter(name %in% c("B01001_001",paste0("B01001",c("B","C","D","E","H","I"),
                                        "_","001")))
pop_cols_var <- pop_cols %>% .$name
##

codebook <- v17 %>% filter(name %in% c(inc_cols_var,pop_cols_var))



nc <- get_acs(geography = "county", 
              variables = c(inc_cols_var,pop_cols_var), 
              state = "NC", 
              year = 2018)

### get data
nc_shp <- st_read("./data/NC_Counties/NC_Counties.shp",stringsAsFactors =F)
nc_shp <- nc_shp %>% clean_names(case = "snake")
nc <- nc %>% mutate(co_name = NAME %>% 
                      str_replace(" County, North Carolina","") %>%
  str_to_upper())

nc_cafo <- read_excel("./data/NC_CAFO_SWINE.xlsx",sheet = 1)
nc_cafo <- nc_cafo %>% clean_names(case = "snake")
nc_cafo <- nc_cafo %>% mutate(co_name = county_name %>% str_to_upper())
cafo_cnty_count <- nc_cafo %>% group_by(co_name) %>% count

cafo_shp <- nc_cafo %>% st_as_sf(coords = c("location_long_num",
                                            "location_lat_num"),
                                 crs = st_crs(nc_shp)$epsg)

nc_shp <- left_join(nc_shp,cafo_cnty_count,by="co_name")
nc_shp <- nc_shp %>% mutate(n = n %>% replace_na(0))

fill_breaks <- c(0,1,5,20,50,100,1000)

nc_shp <- nc_shp %>% 
  mutate(n_breaks = cut(n,breaks = fill_breaks, include.lowest = T,
                        right = F))


ggplot() + geom_sf(dat = nc_shp) +
  geom_sf(data = cafo_shp,aes(size = allowable_count),
          col="red",alpha=0.4) + 
  theme_minimal() + scale_size_area()

ggplot() + geom_sf(dat = nc_shp, aes(fill = n)) + 
  scale_color_brewer() +
  theme_minimal()

nc_shp %>%ggplot(aes(x=n)) + geom_histogram(binwidth = 2,col="white") + 
  theme_minimal()




ggplot() + geom_sf(dat = nc_shp,aes(fill=n_breaks)) +
  scale_fill_brewer(type = "seq",palette = "YlOrRd")

nc_shp %>% filter(n > 0) %>% ggplot() + geom_bar(aes(y=n, x = reorder(factor(co_name),n)),
                    stat = "identity") + coord_flip() +
  theme_minimal()

##### mutate minority variables




