
all.data <- data.frame(elev = elev, site = site, woody = rowSums(mts.woody), ferns = rowSums(mts.ferns))
all.data.long <- all.data %>%
  pivot_longer(cols = 3:4, values_to = "richness", names_to = "taxa")

all.data.nest <- all.data.long %>% 
    group_by(site, taxa) %>%
    nest() 

 all.data.mod <- all.data.nest %>% 
     mutate(
      model.p = map(data, function(df) glm(richness ~ elev, family = poisson(), data = df)),
      tidied = map(model.p, tidy),
      ) %>%
    unnest(tidied) %>%
    rowwise() %>%
    mutate(AIC.p = MuMIn::AICc(model.p))


predict.h <- predict(all.data.mod$model.p[[1]], data.frame(elev = elev[1:10]), type = "response", se.fit = TRUE)
predict.r <- predict(all.data.mod$model.p[[5]], data.frame(elev = elev[11:16]), type = "response", se.fit = TRUE)
predict.t <- predict(all.data.mod$model.p[[9]], data.frame(elev = elev[17:24]), type = "response", se.fit = TRUE)

predict.f.h <- predict(all.data.mod$model.p[[3]], data.frame(elev = elev[1:10]), type = "response", se.fit = TRUE)
predict.f.r <- predict(all.data.mod$model.p[[7]], data.frame(elev = elev[11:16]), type = "response", se.fit = TRUE)
predict.f.t <- predict(all.data.mod$model.p[[11]], data.frame(elev = elev[17:24]), type = "response", se.fit = TRUE)


woody.plot <- filter(all.data.long, taxa == "woody") %>% 
mutate(est = c(predict.h$fit, predict.r$fit, predict.t$fit),
       se = c(predict.h$se.fit, predict.r$se.fit, predict.t$se.fit))


ferns.plot <- filter(all.data.long, taxa == "ferns") %>% 
mutate(est = c(predict.f.h$fit, predict.f.r$fit, predict.f.t$fit),
       se = c(predict.f.h$se.fit, predict.f.r$se.fit, predict.f.t$se.fit))



