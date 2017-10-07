library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(jpndistrict)
library(sf)
library(sp)
library(ggplot2)
library(mapview)
library(openxlsx)
options(stringsAsFactors = F)

d <- read.xlsx("jidousha_hoyuu.xlsx",startRow = 3)

d_edit <- 
  d %>%  mutate(都道府県 = 都道府県 %>% str_replace_all(pattern = "  | ",replacement = "")) %>% 
  mutate_at(vars(contains("車")), function(x) str_replace_all(x, pattern=",", replacement="")) %>% 
  mutate(合計=合計 %>% str_replace_all(pattern=",", replacement=""), 局=NULL) %>% 
  mutate_at(vars(contains("車"), "合計"), as.numeric) %>% 
  filter(都道府県!="計") %>% 
  arrange(desc(合計))


pref <- read_csv("pref.csv", col_types = cols())
pref <- 
  pref %>% 
  mutate(pref_name2 = str_sub(pref_name, end=-2L)) %>% 
  mutate(pref_name2 = str_replace_all(pref_name2, pattern="北海", replacement="北海道"))

jpn <- raster::getData(country="JPN", level=1)
jpn <- jpn %>% st_as_sf()

jinkou <- read_csv("jinkou.csv", locale = locale(encoding="CP932")) %>% 
  filter(X3 %in% pref$pref_name)


d_edit %>% 
  left_join(pref, by=c("都道府県"="pref_name2")) %>% 
  right_join(as.data.frame(jpn), by=c("pref_name"="NL_NAME_1")) %>% 
  left_join(jinkou, by=c("pref_name"="X3")) %>% 
  mutate(乗用車PerPOP=乗用車/`人口　平成27年`) %>% 
  st_as_sf(crs=4612) %>% 
  ggplot()+
  geom_sf(aes(fill=乗用車PerPOP), color=NA) +
  scale_fill_viridis_c()
  

d_edit %>% 
  left_join(pref, by=c("都道府県"="pref_name2")) %>% 
  right_join(as.data.frame(jpn), by=c("pref_name"="NL_NAME_1")) %>% 
  left_join(jinkou, by=c("pref_name"="X3")) %>% 
  mutate(乗用車PerPOP=乗用車/`人口　平成27年`) %>% 
  st_as_sf(crs=4612) %>%
  ggplot() +
  geom_sf(aes(fill=`平成22年～27年の人口増減率（％）`), color=NA) +
  labs(title="H22~27年の人口増減率(percent)") +
  scale_fill_viridis_c() +
  lims(x=c(130,145), y=c(30,45))

