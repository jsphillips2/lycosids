
library(lme4)
library(AICcmodavg)

adulst3 <- adulst2 %>%
  select(count, species, daysout, trans, dist, coldate, year, yday, dist) %>%
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





mm <- lmer(y ~ yday + year + dist + (yday + year + dist | species),
          data = adulst3) 
summary(mm)


mm1 <- lmer(y ~ yday + year + dist + (year + dist | species),
           data = adulst3) 
summary(mm)

mm2 <- lmer(y ~ yday + year + dist + (yday + dist | species),
           data = adulst3) 
summary(mm)


mm3 <- lmer(y ~ yday + year + dist + (yday + year| species),
           data = adulst3) 
summary(mm)

AIC(mm, mm1, mm2, mm3)
anova(mm, mm3)

coef(mm)




nd_y <- adulst3 %>%
  tidyr::expand(species, year = seq(min(year), max(year), 0.1), 
         yday = mean(yday), dist = mean(dist))
pred_y <- predict(mm, newdata = nd_y, type = "response")
nd_y$y <- pred_y

nd_y %>%
  ggplot(aes(year, y, color = species))+
  facet_wrap(~species)+
  geom_jitter(data = adulst3 %>%
                group_by(species, year, trans, dist) %>%
                summarize(y = mean(y, na.rm = T)), 
              width = 0.1, alpha = 0.5, size = 1)+
  geom_line(size = 0.8)+
  scale_color_manual(values = c("dodgerblue","firebrick","gray50"))


nd_d <- adulst3 %>%
  tidyr::expand(species, year = 0, yday = seq(min(yday), max(yday), 1), 
         dist = mean(dist))
pred_d <- predict(mm, newdata = nd_d, type = "response")
nd_d$y <- pred_d


nd_d %>%
  ggplot(aes(yday, y, color = species))+
  facet_wrap(~species)+
  scale_y_continuous()+
  geom_point(data = adulst3 %>%
               group_by(species, yday, trans, dist) %>%
               summarize(y = mean(y, na.rm = T)), alpha = 0.5, size = 1)+
  geom_line(size = 0.8)+
  scale_color_manual(values = c("dodgerblue","firebrick","gray50"))


nd_d <- adulst3 %>%
  tidyr::expand(species, year = 0, dist = seq(min(dist), max(dist), 1), 
         yday = mean(yday))
pred_d <- predict(mm, newdata = nd_d, type = "response")
nd_d$y <- pred_d


nd_d %>%
  ggplot(aes(dist, y, color = species))+
  facet_wrap(~species)+
  scale_y_continuous()+
  geom_point(data = adulst3 %>%
               group_by(species, year, trans, dist) %>%
               summarize(y = mean(y, na.rm = T)), 
                         alpha = 0.5, size = 1)+
  geom_line(size = 0.8)+
  scale_color_manual(values = c("dodgerblue","firebrick","gray50"))
