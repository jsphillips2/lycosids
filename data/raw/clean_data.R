#==========
#========== Preliminaries
#==========

# load packages
library(tidyverse)
library(lubridate)

# set import path
pitfall_raw <- read_csv("data/raw/pitfall.csv")

# examine sheets
sheets <- excel_sheets(import)

# set theme
theme_set(theme_bw() %+replace% 
            theme(panel.grid = element_blank(),
                  strip.background = element_blank(),
                  legend.margin = margin(0,0,0,0),
                  strip.text = element_text(size=10),
                  legend.text = element_text(size=10),
                  axis.text=element_text(size=10, color="black"),
                  axis.title.y=element_text(angle = 90 ,margin=margin(0,15,0,0)),
                  axis.title.x=element_text(margin=margin(15,0,0,0))))




#==========
#========== Lycosid data
#==========

# read data
pitfall <- pitfall_raw %>%
  mutate(setdate = ymd(setdate),
         coldate = ymd(coldate),
         year = year(coldate),
         yday = yday(coldate)) %>%
  filter(year >= 2013)

# extract lycosid columns
# calculate trap days manually
adults <- pitfall %>%
  select(trans, dist, coldate, daysout, year, yday,
         lyco_pal_f, lyco_pal_m, 
         lyco_sph_f, lyco_sph_m, 
         lyco_hyp_f, lyco_hyp_m,
         lyco_unid_f, lyco_unid_m) %>%
  gather(group, count, lyco_pal_f:lyco_unid_m) %>%
  mutate(species = strsplit(group, "_") %>% map_chr(~.x[2]),
         sex = strsplit(group, "_") %>% map_chr(~.x[3])) 
  



adults %>%
  filter(species != "unid") %>%
  ggplot(aes(yday, count, color = factor(year)))+
  facet_grid(species~sex, scales = "free_y")+
  geom_point()


pitfall %>%
  filter(lyco_juv < 20) %>%
  ggplot(aes(yday, lyco_juv, color = factor(year)))+
  geom_point()



adulst2 <- adults %>% 
  filter(species != "unid") %>%
  mutate(sex_spp = factor(paste0(sex, "_", species)),
         species = factor(species))
                            
