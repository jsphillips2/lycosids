
library(brms)
rstan::rstan_options(auto_write = TRUE)


adulst3 <- adulst2 %>%
  select(count, species, daysout, trans, dist, coldate, year, yday, dist, sex) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(catch_rate = log(count + 1) / daysout,
         ar_gr = factor(paste(trans,dist,species, sex, year)),
         time = as.numeric(coldate) - min(as.numeric(coldate)),
         y = (catch_rate - mean(catch_rate)) / sd(catch_rate),
         yday = (yday - mean(yday)) / sd(yday),
         year = (year - mean(year)) / sd(year),
         dist = (dist - mean(dist)) / sd(dist)
         ) %>%
  na.omit()




start_time <- Sys.time()
mm <- brm(y ~ yday + year + dist + 
            (0 + yday + year + dist | species) + 
            (1 | trans * species) 
          # + 
          #   ar(time = time, gr = ar_gr, p = 1, cov = FALSE)
          ,
          data = adulst3,
          warmup = 50, 
          iter   = 100, 
          chains = 4, 
          cores = 4,
          inits  = "random",
          control = list(adapt_delta = 0.95)) 
end_time <- Sys.time()
end_time - start_time


summary(mm)
ranef(mm)$species
