# Section 11.4.2: Knorr-Held models with space-time interactions
# Data: monthly TNC usage
# Inputs: "tnc_monthly_data_for_spatiotemporal.csv"
# Source codes from Section 11.4.1
source("Chapter-11/Section-11.4.1.R")
library(kableExtra)
library(gridExtra)
# Model KH1. Knorr-Held model with interaction between unstructured spatial effect and structural temporal effect
formula.kh1 <-
  tnc.month ~ 1 + f(id.zone, model = "bym", graph = nyc.adj) +
  f(id.month, model = "rw1") +
  f(id.monthu, model = "iid") +
  f(id.zoneu.monthu, model = "iid")
tnc.kh1 <- inla(
  formula.kh1,
  data = data.kh,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  family = "poisson"
)
summary(tnc.kh1)
# Model KH2. Knorr-Held model with interaction between structured spatial effect and unstructured temporal effect
formula.kh2 <- tnc.month ~ 1 +
  f(id.zone, model = "bym", graph = nyc.adj) +
  f(id.month, model = "rw1") +
  f(id.monthu, model = "iid") +
  f(
    id.month.int,
    model = "iid",
    group = id.zone.int,
    control.group = list(model = "besag", graph = nyc.adj)
  )
tnc.kh2 <- inla(
  formula.kh2,
  data = data.kh,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  family = "poisson"
)
summary(tnc.kh2)
# Model KH3. Knorr-Held model with interaction unstructured spatial effect and the structured temporal effect
formula.kh3 <- tnc.month ~ 1 +
  f(id.zone, model = "bym", graph = nyc.adj) +
  f(id.month, model = "rw1") +
  f(id.monthu, model = "iid") +
  f(
    id.zone.int,
    model = "iid",
    group = id.month.int,
    control.group = list(model = "rw1")
  )
tnc.kh3 <- inla(
  formula.kh3,
  data = data.kh,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  family = "poisson"
)
summary(tnc.kh3)
# Model KH4. Knorr-Held model with interaction between the structured spatial effect and the structured temporal effect
formula.kh4 <- tnc.month ~ 1 +
  f(id.zone, model = "bym", graph = nyc.adj) +
  f(id.month, model = "rw1") +
  f(id.monthu, model = "iid") +
  f(
    id.zone.int,
    model = "besag",
    graph = nyc.adj,
    group = id.month.int,
    control.group = list(model = "rw1")
  )
tnc.kh4 <- inla(
  formula.kh4,
  data = data.kh,
  control.predictor = list(compute = TRUE, link = 1),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  family = "poisson"
)
summary(tnc.kh4)
# Code for Table 11.1
## mae
mae.typeA <-
  cbind.data.frame(
    obs = data.kh$tnc.month,
    fit = round(exp(
      summary.tnc.typeA$linear.predictor$mean
    )),
    zoneid = data.kh$zoneid
  ) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    mae(x$obs, x$fit)) %>%
  lapply(function(x)
    str_remove_all(x, "MAE is ")) %>%
  sapply(function(x)
    as.numeric(x)) %>%
  mean
mae.typeB <-
  cbind.data.frame(
    obs = data.kh$tnc.month,
    fit = round(exp(
      summary.tnc.typeB$linear.predictor$mean
    )),
    zoneid = data.kh$zoneid
  ) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    mae(x$obs, x$fit)) %>%
  lapply(function(x)
    str_remove_all(x, "MAE is ")) %>%
  sapply(function(x)
    as.numeric(x)) %>%
  mean
mae.type1 <-
  cbind.data.frame(
    obs = data.kh$tnc.month,
    fit = round(exp(
      summary.tnc.type1$linear.predictor$mean
    )),
    zoneid = data.kh$zoneid
  ) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    mae(x$obs, x$fit)) %>%
  lapply(function(x)
    str_remove_all(x, "MAE is ")) %>%
  sapply(function(x)
    as.numeric(x)) %>%
  mean
mae.type2 <-
  cbind.data.frame(
    obs = data.kh$tnc.month,
    fit = round(exp(
      summary.tnc.type2$linear.predictor$mean
    )),
    zoneid = data.kh$zoneid
  ) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    mae(x$obs, x$fit)) %>%
  lapply(function(x)
    str_remove_all(x, "MAE is ")) %>%
  sapply(function(x)
    as.numeric(x)) %>%
  mean
mae.type3 <-
  cbind.data.frame(
    obs = data.kh$tnc.month,
    fit = round(exp(
      summary.tnc.type3$linear.predictor$mean
    )),
    zoneid = data.kh$zoneid
  ) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    mae(x$obs, x$fit)) %>%
  lapply(function(x)
    str_remove_all(x, "MAE is ")) %>%
  sapply(function(x)
    as.numeric(x)) %>%
  mean
mae.type4 <-
  cbind.data.frame(
    obs = data.kh$tnc.month,
    fit = round(exp(
      summary.tnc.type4$linear.predictor$mean
    )),
    zoneid = data.kh$zoneid
  ) %>%
  group_split(zoneid) %>%
  lapply(function(x)
    mae(x$obs, x$fit)) %>%
  lapply(function(x)
    str_remove_all(x, "MAE is ")) %>%
  sapply(function(x)
    as.numeric(x)) %>%
  mean

df.comp <-
  as_tibble(cbind.data.frame(
    Model = c("KH additive", "KH1", "KH2", "KH3", "KH4", "Temporal model"),
    DIC = c(
      summary.tnc.typeA$dic$dic,
      summary.tnc.type1$dic$dic,
      summary.tnc.type2$dic$dic,
      summary.tnc.type3$dic$dic,
      "",
      summary.tnc.typeB$dic$dic
    ),
    WAIC = c(
      summary.tnc.typeA$waic$waic,
      summary.tnc.type1$waic$waic,
      summary.tnc.type2$waic$waic,
      summary.tnc.type3$waic$waic,
      summary.tnc.type4$waic$waic,
      summary.tnc.typeB$waic$waic
    ),
    MAE = c(
      mae.typeA,
      mae.type1,
      mae.type2,
      mae.type3,
      mae.type4,
      mae.typeB
    )
  ))
kable(
  df.comp,
  booktabs = TRUE,
  align = "c",
  caption = 'In-sample model comparisons.',
  digits = 3,
  col.names = c("Model", "DIC", "WAIC", "Average MAE"),
  escape = F
) %>%
  kable_styling(
    bootstrap_options = "striped",
    position = "left",
    fixed_thead = TRUE,
    full_width = FALSE
  )
# Code for Figure 11.1
# Type A
df.typeA <-
  cbind.data.frame(fit = round(exp(
    summary.tnc.typeA$linear.predictor$mean
  )), zoneid = data.kh$zoneid)
mean.fit.typeA <- df.typeA %>%
  group_by(zoneid) %>%
  summarise(fit = mean(fit))
fit.typeA <- left_join(zoneshape, mean.fit.typeA, by = c("zoneid"))
plot.fit.typeA <-
  tm_shape(fit.typeA) + tm_polygons(style = "quantile", col = "fit") +
  tm_layout(title = 'Additive')

# Type B
df.typeB <-
  cbind.data.frame(fit = round(exp(
    summary.tnc.typeB$linear.predictor$mean
  )), zoneid = data.kh$zoneid)
mean.fit.typeB <- df.typeB %>%
  group_by(zoneid) %>%
  summarise(fit = mean(fit))
fit.typeB <- left_join(zoneshape, mean.fit.typeB, by = c("zoneid"))
plot.fit.typeB <-
  tm_shape(fit.typeB) + tm_polygons(style = "quantile", col = "fit") +
  tm_layout(title = 'Additive - No spatial')

# Type 1
df.type1 <-
  cbind.data.frame(fit = round(exp(
    summary.tnc.type1$linear.predictor$mean
  )), zoneid = data.kh$zoneid)
# Type 2
df.type2 <-
  cbind.data.frame(fit = round(exp(
    summary.tnc.type2$linear.predictor$mean
  )), zoneid = data.kh$zoneid)

# Type 3
df.type3 <-
  cbind.data.frame(fit = round(exp(
    summary.tnc.type3$linear.predictor$mean
  )), zoneid = data.kh$zoneid)
# Type 4
df.type4 <-
  cbind.data.frame(fit = round(exp(
    summary.tnc.type4$linear.predictor$mean
  )), zoneid = data.kh$zoneid)
#Type A: KH Additive model
res.typeA <- df.typeA %>%
  mutate(obs = data.kh$tnc.month) %>%
  mutate(res = (obs - fit)) %>%
  select(zoneid, res) %>%
  group_by(zoneid) %>%
  summarise(res = mean(res))
res.typeA <- left_join(zoneshape, res.typeA, by = "zoneid")
plot.res.typeA <-
  tm_shape(res.typeA) + tm_polygons(style = "quantile", col = "res") +
  tm_layout(title = 'Additive')
#Type B: temporal only model
res.typeB <- df.typeB %>%
  mutate(obs = data.kh$tnc.month) %>%
  mutate(res = (obs - fit)) %>%
  select(zoneid, res) %>%
  group_by(zoneid) %>%
  summarise(res = mean(res))
res.typeB <- left_join(zoneshape, res.typeB, by = "zoneid")
plot.res.typeB <-
  tm_shape(res.typeB) + tm_polygons(style = "quantile", col = "res") +
  tm_layout(title = 'Additive- no spatial')
#KH Type 1interaction
res.type1 <- df.type1 %>%
  mutate(obs = data.kh$tnc.month) %>%
  mutate(res = (obs - fit)) %>%
  select(zoneid, res) %>%
  group_by(zoneid) %>%
  summarise(res = mean(res))
res.type1 <- left_join(zoneshape, res.type1, by = "zoneid")
plot.res.type1 <-
  tm_shape(res.type1) + tm_polygons(style = "quantile", col = "res") +
  tm_layout(title = 'Type1')
#KH Type 2interaction
res.type2 <- df.type2 %>%
  mutate(obs = data.kh$tnc.month) %>%
  mutate(res = (obs - fit)) %>%
  select(zoneid, res) %>%
  group_by(zoneid) %>%
  summarise(res = mean(res))
res.type2 <- left_join(zoneshape, res.type2, by = "zoneid")
plot.res.type2 <-
  tm_shape(res.type2) + tm_polygons(style = "quantile", col = "res") +
  tm_layout(title = 'Type2')
#KH Type 3interaction
res.type3 <- df.type3 %>%
  mutate(obs = data.kh$tnc.month) %>%
  mutate(res = (obs - fit)) %>%
  select(zoneid, res) %>%
  group_by(zoneid) %>%
  summarise(res = mean(res))
res.type3 <- left_join(zoneshape, res.type3, by = "zoneid")
plot.res.type3 <-
  tm_shape(res.type3) + tm_polygons(style = "quantile", col = "res") +
  tm_layout(title = 'Type3')
#KH Type 4interaction
res.type4 <- df.type4 %>%
  mutate(obs = data.kh$tnc.month) %>%
  mutate(res = (obs - fit)) %>%
  select(zoneid, res) %>%
  group_by(zoneid) %>%
  summarise(res = mean(res))
res.type4 <- left_join(zoneshape, res.type4, by = "zoneid")
plot.res.type4 <-
  tm_shape(res.type4) + tm_polygons(style = "quantile", col = "res") +
  tm_layout(title = 'Type4')
# tmap_arrange(plot.res.typeA, plot.res.type1, plot.res.type2,
#              plot.res.type3, plot.res.type4, ncol=2, nrow = 3)

## alt plot
res.typeA$model <- "Additive"
res.type1$model <- "Type1"
res.type2$model <- "Type2"
res.type3$model <- "Type3"
res.type4$model <- "Type4"

res.all <- rbind(res.typeA, res.type1, res.type2, res.type3,
                 res.type4)
res.all$model <- factor(res.all$model)
tm_shape(res.all) + tm_polygons(style = "quantile", col = "res") +
  tm_facets("model", ncol = 3)
# Posterior distributions under Model KH3
post.sampletype3 <-
  inla.posterior.sample(n = 500, tnc.type3, seed = 1234)
temp.nu <- temp.delta <- list()
for (k in 1:length(post.sampletype3)) {
  x <- post.sampletype3[[k]]
  temp.nu[[k]] <-
    cbind.data.frame(nu = tail(x$latent[grep("id.zone:",
                                             rownames(x$latent))], g),
                     zone = unique(data.kh$zoneid))
  temp.delta[[k]] <-
    cbind.data.frame(delta = x$latent[grep("id.zone.int:",
                                           rownames(x$latent))],
                     zone = data.kh$zoneid) %>%
    group_by(zone) %>%
    summarize(across(delta,  ~ mean(.x)))
}
# Code for Figure 11.2
temp.nu <- temp.delta <- list()
for (k in 1:length(post.sampletype3)) {
  x <- post.sampletype3[[k]]
  temp.nu[[k]] <-
    cbind.data.frame(nu = tail(x$latent[grep("id.zone:", rownames(x$latent))], g),
                     zone = unique(data.kh$zoneid))
  temp.delta[[k]] <-
    cbind.data.frame(delta = x$latent[grep("id.zone.int:", rownames(x$latent))], zone = data.kh$zoneid) %>%
    group_by(zone) %>%
    summarize(across(delta,  ~ mean(.x)))
}
nu.all <- bind_rows(temp.nu)
nu.all$zone <- factor(nu.all$zone, levels = unique(data.kh$zoneid))
p1 <- ggplot(nu.all, aes(x = zone, y = nu)) +
  geom_boxplot(outlier.shape = NA) +
  ylab(expression(nu)) +
  xlab("zone") +
  theme(axis.text.x = element_blank())
## boxplot delta
delta.all <- bind_rows(temp.delta)
delta.all$zone <-
  factor(delta.all$zone, levels = unique(data.kh$zoneid))
p2 <- ggplot(delta.all, aes(x = zone, y = delta)) +
  geom_boxplot(outlier.shape = NA) +
  ylab(expression(delta)) +
  xlab("zone") +
  theme(axis.text.x = element_blank())
grid.arrange(p1, p2)
# Code for Figure 11.3
df <-
  cbind.data.frame(obs = data.kh$tnc.month, zoneid = data.kh$zoneid)
mean.obs <- df %>%
  group_by(zoneid) %>%
  summarise(obs.fit = mean(obs))
obs.df <- left_join(zoneshape, mean.obs, by = c("zoneid"))
obs.df$key <- "Observed"
# Type 3
df.type3 <-
  cbind.data.frame(fit = round(exp(
    summary.tnc.type3$linear.predictor$mean
  )), zoneid = data.kh$zoneid)
mean.fit.type3 <- df.type3 %>%
  group_by(zoneid) %>%
  summarise(obs.fit = mean(fit))
fit.type3 <- left_join(zoneshape, mean.fit.type3, by = c("zoneid"))
fit.type3$key <- "Fit(Type3)"
#fit.type4$model <- "Type4"

fit.all <- rbind(obs.df, fit.type3) %>%
  rename(count = obs.fit)
fit.all$key <- factor(fit.all$key, levels = unique(fit.all$key))
tm_shape(fit.all) + tm_polygons(style = "quantile", col = "count") +
  tm_facets("key", ncol = 2)
