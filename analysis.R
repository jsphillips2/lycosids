#=========================================================================================
#========== Preliminaries
#=========================================================================================

# load packages
library(tidyverse)
library(lubridate)
library(brms)
library(cowplot)
library(bayesplot)

# parallel processing
options(mc.cores = parallel::detectCores()-2)

# set import path
pitfall_raw <- read_csv("data/raw/pitfall.csv")

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

#=========================================================================================





#=========================================================================================
#========== Lycosid data
#=========================================================================================

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
         sex = strsplit(group, "_") %>% map_chr(~.x[3]))  %>%
  mutate(sex_spp = factor(paste0(sex, "_", species)),
         species = factor(species))

adults_clean <- adults %>% 
  filter(species != "unid")%>%
  select(count, species, daysout, trans, dist, coldate, year, yday, dist, sex) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(catch_rate = log(count + 1) / daysout,
         plot = factor(paste(trans,dist)),
         time = as.numeric(coldate) - min(as.numeric(coldate)),
         y = (catch_rate - mean(catch_rate)) / sd(catch_rate),
         yday = (yday - mean(yday)) / sd(yday),
         year = (year - mean(year)) / sd(year),
         dist = (dist - mean(dist)) / sd(dist)
  ) %>%
  na.omit()

# wide data
adults_wide <- adults_clean %>%
  select(trans, dist, year, yday, plot, time, species, sex, y) %>%
  pivot_wider(names_from = species, values_from = y) %>%
  mutate(ar_gr = factor(paste(trans,dist,sex, year)))

#=========================================================================================





#=========================================================================================
#========== Fit model 
#=========================================================================================

model <- brm(mvbind(pal, sph, hyp) ~ 
               sex + year + yday + dist + 
               (1 | p | trans) + (1 | q | plot) +
             ar(time = time, gr = ar_gr, p = 1, cov = FALSE),
             family = "gaussian",
             data = adults_wide,
             iter = 1000,
             chains = 4,
             cores = 4)
# write_rds(model,"model_fit.rds")
# model <- read_rds("model_fit.rds")

# examine fit
summary(model, prob = 0.68)

#=========================================================================================





#=========================================================================================
#========== Plot
#=========================================================================================

trans_data <- adults_clean %>%
  mutate(distance = dist * sd(adults$dist) + mean(adults$dist)) %>%
  group_by(trans, plot, dist,distance, species) %>%
  summarize(y = mean(y)) %>%
  pivot_wider(names_from = species, values_from = y) %>%
  ungroup()

ggplot(data = trans_data,
       aes(x = hyp, 
           y = sph, 
           color = trans))+
  geom_point(aes(size = distance))+
  scale_color_manual(guide = "none",
                     values = c("firebrick","dodgerblue","gray50",
                                "magenta2","darkorange","darkgreen","goldenrod4"))

ggplot(data = trans_data,
       aes(x = hyp, 
           y = pal, 
           color = trans, 
           size = distance))+
  geom_point()+
  scale_color_manual(guide = "none",
                     values = c("firebrick","dodgerblue","gray50",
                                "magenta2","darkorange","darkgreen","goldenrod4"))
  

ggplot(data = trans_data,
       aes(x = sph, 
           y = pal, 
           color = trans, 
           size = distance))+
  geom_point()


year_data <- adults_clean %>%
  mutate(distance = dist * sd(adults$dist) + mean(adults$dist)) %>%
  group_by(year, plot, trans, distance, species) %>%
  summarize(y = mean(y))


ggplot(data = year_data,
       aes(x = year,
           y = y))+
  facet_wrap(~species, nrow = 3)+
  geom_line(aes(group = plot),
            alpha = 0.5,
            size = 0.3)



season_data <- adults_clean %>%
  mutate(distance = dist * sd(adults$dist) + mean(adults$dist)) %>%
  group_by(yday, plot, trans, distance, species) %>%
  summarize(y = mean(y))



ggplot(data = season_data,
       aes(x = yday,
           y = y))+
  facet_wrap(~species, nrow = 3)+
  geom_line(aes(group = plot),
            alpha = 0.5,
            size = 0.3)



season_data <- adults_clean %>%
  mutate(distance = dist * sd(adults$dist) + mean(adults$dist)) %>%
  group_by(plot, trans, distance, species) %>%
  summarize(y = mean(y))



ggplot(data = season_data,
       aes(x = distance,
           y = y))+
  facet_wrap(~species, nrow = 3)+
  geom_point()


#=========================================================================================





#=========================================================================================
#========== Direct correlations
#=========================================================================================

adults_clean %>%
  group_by(trans, species) %>%
  summarize(y = mean(y)) %>%
  pivot_wider(names_from = species, values_from = y) %>%
  ungroup() %>%
  select(-trans) %>%
  cor()

adults_clean %>%
  group_by(plot, species) %>%
  summarize(y = mean(y)) %>%
  pivot_wider(names_from = species, values_from = y) %>%
  ungroup() %>%
  select(-plot) %>%
  cor()

adults_clean %>%
  group_by(year, species) %>%
  summarize(y = mean(y)) %>%
  pivot_wider(names_from = species, values_from = y) %>%
  ungroup() %>%
  select(-year) %>%
  cor()

adults_clean %>%
  group_by(yday, species) %>%
  summarize(y = mean(y)) %>%
  pivot_wider(names_from = species, values_from = y) %>%
  ungroup() %>%
  select(-yday) %>%
  cor()

