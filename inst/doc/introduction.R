## ----setup, include = FALSE---------------------------------------------------
# # jss style
# knitr::opts_chunk$set(prompt=TRUE, echo = TRUE, highlight = FALSE, continue = " + ", comment = "")
# options(replace.assign=TRUE, width=90, prompt="R> ")

# rmd style
knitr::opts_chunk$set(collapse = FALSE, comment = "#>", warning = FALSE, message = FALSE)

# load packages
library(ggplot2)
library(SSN2)

## ---- eval = FALSE------------------------------------------------------------
#  library(SSN2)

## -----------------------------------------------------------------------------
citation(package = "SSN2")

## -----------------------------------------------------------------------------
copy_lsn_to_temp()

## -----------------------------------------------------------------------------
temp_path <- paste0(tempdir(), "/MiddleFork04.ssn")
mf04p <- ssn_import(
  path = temp_path,
  predpts = c("pred1km", "CapeHorn", "Knapp"),
  overwrite = TRUE
)

## -----------------------------------------------------------------------------
summary(mf04p)

## ---- eval = FALSE------------------------------------------------------------
#  library(ggplot2)

## -----------------------------------------------------------------------------
names(mf04p$preds)

## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = mf04p$edges) +
  geom_sf(data = mf04p$preds$pred1km) +
  geom_sf(data = mf04p$obs, color = "brown", size = 1.5) +
  theme_bw()

## -----------------------------------------------------------------------------
ssn_create_distmat(
  ssn.object = mf04p,
  predpts = c("pred1km", "CapeHorn", "Knapp"),
  among_predpts = TRUE,
  overwrite = TRUE
)

## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = mf04p$edges) +
  geom_sf(data = mf04p$obs, aes(color = Summer_mn), size = 1.5) +
  scale_color_viridis_c(limits = c(-1.5, 17), option = "H") +
  theme_bw()

## -----------------------------------------------------------------------------
tg <- Torgegram(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04p,
  type = c("flowcon", "flowuncon", "euclid")
)

## -----------------------------------------------------------------------------
plot(tg)

## -----------------------------------------------------------------------------
ssn_mod <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04p,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_type = "gaussian",
  additive = "afvArea"
)

## -----------------------------------------------------------------------------
summary(ssn_mod)

## -----------------------------------------------------------------------------
varcomp(ssn_mod)

## -----------------------------------------------------------------------------
tidy(ssn_mod, conf.int = TRUE)

## -----------------------------------------------------------------------------
glance(ssn_mod)

## -----------------------------------------------------------------------------
ssn_mod2 <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04p,
  taildown_type = "spherical"
)
glances(ssn_mod, ssn_mod2)

## -----------------------------------------------------------------------------
ml_mod <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04p,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_type = "gaussian",
  additive = "afvArea",
  estmethod = "ml"
)
ml_mod2 <- ssn_lm(
  formula = Summer_mn ~ AREAWTMAP,
  ssn.object = mf04p,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_type = "gaussian",
  additive = "afvArea",
  estmethod = "ml"
)
glances(ml_mod, ml_mod2)

## -----------------------------------------------------------------------------
loocv_mod <- loocv(ssn_mod)
loocv_mod$RMSPE
loocv_mod2 <- loocv(ssn_mod2)
loocv_mod2$RMSPE

## -----------------------------------------------------------------------------
aug_ssn_mod <- augment(ssn_mod)
aug_ssn_mod

## ---- results = "hide"--------------------------------------------------------
library(sf)
st_write(aug_ssn_mod, paste0(tempdir(), "/aug_ssn_mod.shp"))

## -----------------------------------------------------------------------------
plot(ssn_mod, which = 1)

## ---- results = "hide"--------------------------------------------------------
predict(ssn_mod, newdata = "pred1km")

## -----------------------------------------------------------------------------
aug_preds <- augment(ssn_mod, newdata = "pred1km")
aug_preds[, ".fitted"]

## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = mf04p$edges) +
  geom_sf(data = aug_preds, aes(color = .fitted), size = 1.5) +
  scale_color_viridis_c(limits = c(-1.5, 17), option = "H") +
  theme_bw()

## ---- results = "hide"--------------------------------------------------------
st_write(aug_preds, paste0(tempdir(), "/aug_preds.gpkg"))

## ---- results = "hide"--------------------------------------------------------
predict(ssn_mod)
predict(ssn_mod, newdata = "all")

## -----------------------------------------------------------------------------
predict(ssn_mod, newdata = "pred1km", block = TRUE, interval = "prediction")

## -----------------------------------------------------------------------------
euclid_init <- euclid_initial("gaussian", de = 1, known = "de")
euclid_init

## -----------------------------------------------------------------------------
ssn_init <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04p,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_initial = euclid_init,
  additive = "afvArea"
)
ssn_init

## -----------------------------------------------------------------------------
ssn_rand <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04p,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_type = "gaussian",
  additive = "afvArea",
  random = ~ as.factor(netID)
)
ssn_rand

## -----------------------------------------------------------------------------
ssn_part <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04p,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_type = "gaussian",
  additive = "afvArea",
  partition_factor = ~ as.factor(netID)
)
ssn_part

## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = mf04p$edges) +
  geom_sf(data = mf04p$obs, aes(color = C16), size = 1.5) +
  scale_color_viridis_c(option = "H") +
  theme_bw()

## -----------------------------------------------------------------------------
ssn_pois <- ssn_glm(
  formula = C16 ~ ELEV_DEM + AREAWTMAP,
  family = "poisson",
  ssn.object = mf04p,
  tailup_type = "epa",
  taildown_type = "mariah",
  additive = "afvArea"
)

## -----------------------------------------------------------------------------
summary(ssn_pois)

## -----------------------------------------------------------------------------
ssn_nb <- ssn_glm(
  formula = C16 ~ ELEV_DEM + AREAWTMAP,
  family = "nbinomial",
  ssn.object = mf04p,
  tailup_type = "epa",
  taildown_type = "mariah",
  additive = "afvArea"
)

## -----------------------------------------------------------------------------
loocv_pois <- loocv(ssn_pois)
loocv_pois$RMSPE
loocv_nb <- loocv(ssn_nb)
loocv_nb$RMSPE

## -----------------------------------------------------------------------------
tu_params <- tailup_params("exponential", de = 0.4, range = 1e5)
td_params <- taildown_params("spherical", de = 0.1, range = 1e6)
euc_params <- euclid_params("gaussian", de = 0.2, range = 1e3)
nug_params <- nugget_params("nugget", nugget = 0.1)
set.seed(2)

## -----------------------------------------------------------------------------
sims <- ssn_simulate(
  family = "gaussian",
  ssn.object = mf04p,
  network = "obs",
  additive = "afvArea",
  tailup_params = tu_params,
  taildown_params = td_params,
  euclid_params = euc_params,
  nugget_params = nug_params,
  mean = 0,
  samples = 1
)
head(sims)

## -----------------------------------------------------------------------------
sims <- ssn_simulate(
  family = "binomial",
  ssn.object = mf04p,
  network = "obs",
  additive = "afvArea",
  tailup_params = tu_params,
  taildown_params = td_params,
  euclid_params = euc_params,
  nugget_params = nug_params,
  mean = 0,
  samples = 2
)
head(sims)

## ----get-labels, echo = FALSE-------------------------------------------------
labs <- knitr::all_labels()
labs <- setdiff(labs, c("setup", "get-labels"))

## ----all-code, ref.label=labs, eval = FALSE-----------------------------------
#  library(SSN2)
#  citation(package = "SSN2")
#  copy_lsn_to_temp()
#  temp_path <- paste0(tempdir(), "/MiddleFork04.ssn")
#  mf04p <- ssn_import(
#    path = temp_path,
#    predpts = c("pred1km", "CapeHorn", "Knapp"),
#    overwrite = TRUE
#  )
#  summary(mf04p)
#  library(ggplot2)
#  names(mf04p$preds)
#  ggplot() +
#    geom_sf(data = mf04p$edges) +
#    geom_sf(data = mf04p$preds$pred1km) +
#    geom_sf(data = mf04p$obs, color = "brown", size = 1.5) +
#    theme_bw()
#  ssn_create_distmat(
#    ssn.object = mf04p,
#    predpts = c("pred1km", "CapeHorn", "Knapp"),
#    among_predpts = TRUE,
#    overwrite = TRUE
#  )
#  ggplot() +
#    geom_sf(data = mf04p$edges) +
#    geom_sf(data = mf04p$obs, aes(color = Summer_mn), size = 1.5) +
#    scale_color_viridis_c(limits = c(-1.5, 17), option = "H") +
#    theme_bw()
#  tg <- Torgegram(
#    formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
#    ssn.object = mf04p,
#    type = c("flowcon", "flowuncon", "euclid")
#  )
#  plot(tg)
#  ssn_mod <- ssn_lm(
#    formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
#    ssn.object = mf04p,
#    tailup_type = "exponential",
#    taildown_type = "spherical",
#    euclid_type = "gaussian",
#    additive = "afvArea"
#  )
#  summary(ssn_mod)
#  varcomp(ssn_mod)
#  tidy(ssn_mod, conf.int = TRUE)
#  glance(ssn_mod)
#  ssn_mod2 <- ssn_lm(
#    formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
#    ssn.object = mf04p,
#    taildown_type = "spherical"
#  )
#  glances(ssn_mod, ssn_mod2)
#  ml_mod <- ssn_lm(
#    formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
#    ssn.object = mf04p,
#    tailup_type = "exponential",
#    taildown_type = "spherical",
#    euclid_type = "gaussian",
#    additive = "afvArea",
#    estmethod = "ml"
#  )
#  ml_mod2 <- ssn_lm(
#    formula = Summer_mn ~ AREAWTMAP,
#    ssn.object = mf04p,
#    tailup_type = "exponential",
#    taildown_type = "spherical",
#    euclid_type = "gaussian",
#    additive = "afvArea",
#    estmethod = "ml"
#  )
#  glances(ml_mod, ml_mod2)
#  loocv_mod <- loocv(ssn_mod)
#  loocv_mod$RMSPE
#  loocv_mod2 <- loocv(ssn_mod2)
#  loocv_mod2$RMSPE
#  aug_ssn_mod <- augment(ssn_mod)
#  aug_ssn_mod
#  library(sf)
#  st_write(aug_ssn_mod, paste0(tempdir(), "/aug_ssn_mod.shp"))
#  plot(ssn_mod, which = 1)
#  predict(ssn_mod, newdata = "pred1km")
#  aug_preds <- augment(ssn_mod, newdata = "pred1km")
#  aug_preds[, ".fitted"]
#  ggplot() +
#    geom_sf(data = mf04p$edges) +
#    geom_sf(data = aug_preds, aes(color = .fitted), size = 1.5) +
#    scale_color_viridis_c(limits = c(-1.5, 17), option = "H") +
#    theme_bw()
#  st_write(aug_preds, paste0(tempdir(), "/aug_preds.gpkg"))
#  predict(ssn_mod)
#  predict(ssn_mod, newdata = "all")
#  predict(ssn_mod, newdata = "pred1km", block = TRUE, interval = "prediction")
#  euclid_init <- euclid_initial("gaussian", de = 1, known = "de")
#  euclid_init
#  ssn_init <- ssn_lm(
#    formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
#    ssn.object = mf04p,
#    tailup_type = "exponential",
#    taildown_type = "spherical",
#    euclid_initial = euclid_init,
#    additive = "afvArea"
#  )
#  ssn_init
#  ssn_rand <- ssn_lm(
#    formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
#    ssn.object = mf04p,
#    tailup_type = "exponential",
#    taildown_type = "spherical",
#    euclid_type = "gaussian",
#    additive = "afvArea",
#    random = ~ as.factor(netID)
#  )
#  ssn_rand
#  ssn_part <- ssn_lm(
#    formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
#    ssn.object = mf04p,
#    tailup_type = "exponential",
#    taildown_type = "spherical",
#    euclid_type = "gaussian",
#    additive = "afvArea",
#    partition_factor = ~ as.factor(netID)
#  )
#  ssn_part
#  ggplot() +
#    geom_sf(data = mf04p$edges) +
#    geom_sf(data = mf04p$obs, aes(color = C16), size = 1.5) +
#    scale_color_viridis_c(option = "H") +
#    theme_bw()
#  ssn_pois <- ssn_glm(
#    formula = C16 ~ ELEV_DEM + AREAWTMAP,
#    family = "poisson",
#    ssn.object = mf04p,
#    tailup_type = "epa",
#    taildown_type = "mariah",
#    additive = "afvArea"
#  )
#  summary(ssn_pois)
#  ssn_nb <- ssn_glm(
#    formula = C16 ~ ELEV_DEM + AREAWTMAP,
#    family = "nbinomial",
#    ssn.object = mf04p,
#    tailup_type = "epa",
#    taildown_type = "mariah",
#    additive = "afvArea"
#  )
#  loocv_pois <- loocv(ssn_pois)
#  loocv_pois$RMSPE
#  loocv_nb <- loocv(ssn_nb)
#  loocv_nb$RMSPE
#  tu_params <- tailup_params("exponential", de = 0.4, range = 1e5)
#  td_params <- taildown_params("spherical", de = 0.1, range = 1e6)
#  euc_params <- euclid_params("gaussian", de = 0.2, range = 1e3)
#  nug_params <- nugget_params("nugget", nugget = 0.1)
#  set.seed(2)
#  sims <- ssn_simulate(
#    family = "gaussian",
#    ssn.object = mf04p,
#    network = "obs",
#    additive = "afvArea",
#    tailup_params = tu_params,
#    taildown_params = td_params,
#    euclid_params = euc_params,
#    nugget_params = nug_params,
#    mean = 0,
#    samples = 1
#  )
#  head(sims)
#  sims <- ssn_simulate(
#    family = "binomial",
#    ssn.object = mf04p,
#    network = "obs",
#    additive = "afvArea",
#    tailup_params = tu_params,
#    taildown_params = td_params,
#    euclid_params = euc_params,
#    nugget_params = nug_params,
#    mean = 0,
#    samples = 2
#  )
#  head(sims)

