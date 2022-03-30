if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("flavjack/inti")

library(inti)
library(purrr)
library(dplyr)
fb <- inti::met
str(fb)
envs <- levels(fb$env)

model <- 1:length(envs) %>% map(function(x) {
  
  model <- fb %>% filter(env %in% envs[x]) %>% 
    
    H2cal(trait = "yield"
          , gen.name = "cultivar"
          , rep.n = 4
          , fixed.model = "0 + (1|rep) + (1|rep:alpha) + cultivar"
          , random.model = "1 + (1|rep) + (1|rep:alpha) + (1|cultivar)"
          # , plot_diag = T
          , emmeans = F
    )
  
  blues <- model$blues %>% mutate(trial = levels(fb$env)[x])
  
})

blues.h2cal <- bind_rows(model) %>% 
  separate(trial, c("zone", "location", "year")) %>% 
  mutate(across(c(yield, smith.w), as.numeric)) %>% 
  mutate(across(!c(yield, smith.w), as.factor))


met <- blues.h2cal %>%
  mutate(across(!yield, as.factor)) %>%
  H2cal(trait = "yield"
        , gen.name = "cultivar"
        , rep.n = 4
        , env.n = 18
        , env.name = "location"
        , fixed.model = "0 + zone + (1|zone:location) + (1|zone:cultivar) + cultivar"
        , random.model = "1 + zone + (1|zone:location) + (1|zone:cultivar) + (1|cultivar)"
        # , plot_diag = T
        , emmeans = T
        # , weights = blues.h2cal$smith.w
  )

blups.h2cal <- met$blups

str(blues.h2cal)
str(blups.h2cal)
