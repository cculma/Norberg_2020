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


library(caret)
library(tidyverse)
G1 <- read.csv("~/Documents/Cesar/git/big_files/Norberg_1.txt", check.names = F)
G1[1:4,1:6]
G1 <- G1 %>% unite(col = "Marker1", 1:2, sep = "_", remove = T) %>% column_to_rownames(var = "Marker1") 
G1 <- t(G1)
dim(G1)
nzv <- nearZeroVar(G1)
G2 <- G1[, -nzv]
dim(G2)
G2 <- t(G2)
G2[1:4,1:6]
class(G2)
G2 <- as.data.frame(G2)
G2 <- G2 %>% rownames_to_column(var = "Marker1") %>% separate(1, c("Chrom", "Position"), sep = "_", remove = T, convert = FALSE, extra = "merge")
write.csv(G2, "~/Documents/Cesar/git/big_files/Norberg_2.txt", quote = F, row.names = F)



# comboInfo <- findLinearCombos(G2)
# G3 <- G2[, -comboInfo$remove]
# dim(G3)
