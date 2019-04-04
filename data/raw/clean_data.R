#==========
#========== Preliminaries
#==========

# load packages
library(tidyverse)
library(readxl)
library(lubridate)

# set import path
import <- "data/raw/MASTER_LTREB_2013-2018_13Sep18.xlsx"

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
pitfall <- read_excel(import, sheet = "Pitfall", na=c("","NA")) %>%
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


library(mgcv)

adulst2 <- adults %>% 
  filter(species != "unid") %>%
  mutate(sex_spp = factor(paste0(sex, "_", species)),
         species = factor(species))
                            
m <- gam(count ~ 
           s(yday, by = species, k = 6) + s(yday, by = sex_spp, k = 6) + 
           s(year, by = species, k = 4) + s(year, by = sex_spp, k =4) + 
           species + sex + daysout,
          data = adulst2, 
         family = "poisson")
anova(m)
nd_y <- adulst2 %>%
  expand(nesting(species,sex,sex_spp), year, yday = mean(yday), daysout = mean(daysout))
nd_y$count <- predict(m, newdata = nd_y, type = "response")

nd_y %>%
  ggplot(aes(year, count, color = sex))+
  facet_wrap(~species)+
  scale_y_continuous(trans = "log1p")+
  geom_jitter(data = adulst2, width = 0.1, alpha = 0.5, size = 1)+
  geom_line(size = 0.8)


nd_d <- adulst2 %>%
  expand(nesting(species,sex,sex_spp), year = 2015, yday = seq(min(yday), max(yday), 5), 
         daysout = mean(daysout))
nd_d$count <- predict(m, newdata = nd_d, type = "response")

nd_d %>%
  ggplot(aes(yday, count, color = sex))+
  facet_wrap(~species)+
  scale_y_continuous(trans = "log1p")+
  # geom_point(data = adulst2, alpha = 0.5, size = 1)+
  geom_line(size = 0.8)


