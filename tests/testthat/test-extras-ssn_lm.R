test_that("blank test", {
  expect_null(NULL)
})

test_local <- FALSE # FALSE for CRAN

if (test_local) {
  # Copy the mf04p .ssn data to a local directory and read it into R
  # When modeling with your .ssn object, you will load it using the relevant
  # path to the .ssn data on your machine
  copy_lsn_to_temp()
  temp_path <- paste0(tempdir(), "/MiddleFork04.ssn")
  mf04p <- ssn_import(
    temp_path,
    predpts = c("pred1km", "CapeHorn", "Knapp"),
    overwrite = TRUE
  )

  ssn_create_distmat(
    ssn.object = mf04p,
    predpts = c("pred1km", "CapeHorn", "Knapp"),
    overwrite = TRUE
  )

  test_that("random effects work", {
    ssn_mod <- ssn_lm(Summer_mn ~ ELEV_DEM, mf04p,
      tailup_type = "exponential",
      taildown_type = "exponential", euclid_type = "exponential",
      nugget_type = "nugget", additive = "afvArea",
      random = ~ as.factor(netID)
    )
    expect_s3_class(ssn_mod, "ssn_lm")
    expect_vector(predict(ssn_mod, "pred1km"))
  })

  test_that("partition factors work", {
    ssn_mod <- ssn_lm(Summer_mn ~ ELEV_DEM, mf04p,
      tailup_type = "exponential",
      nugget_type = "nugget", additive = "afvArea",
      partition_factor = ~ as.factor(netID)
    )
    expect_s3_class(ssn_mod, "ssn_lm")
    expect_vector(predict(ssn_mod, "pred1km"))
  })

  test_that("anisotropy works", {
    ssn_mod <- ssn_lm(Summer_mn ~ ELEV_DEM, mf04p,
      taildown_type = "exponential",
      nugget_type = "nugget", estmethod = "ml",
      anisotropy = TRUE
    )
    expect_s3_class(ssn_mod, "ssn_lm")
    expect_vector(predict(ssn_mod, "pred1km"))
  })

  test_that("fixing parameters works", {
    tu <- tailup_initial("exponential", de = 1, known = "de")
    ssn_mod <- ssn_lm(Summer_mn ~ ELEV_DEM, mf04p,
      tailup_initial = tu,
      taildown_type = "exponential", euclid_type = "exponential",
      nugget_type = "none", additive = "afvArea"
    )
    expect_s3_class(ssn_mod, "ssn_lm")
    expect_vector(predict(ssn_mod, "pred1km"))
    expect_equal(coef(ssn_mod, type = "tailup")[["de"]], 1)
  })

  test_that("missing data works", {
    mf04p$obs$Summer_mn[1] <- NA
    ssn_mod <- ssn_lm(Summer_mn ~ ELEV_DEM, mf04p,
      tailup_type = "exponential",
      nugget_type = "nugget", additive = "afvArea"
    )
    expect_s3_class(ssn_mod, "ssn_lm")
    expect_vector(predict(ssn_mod, "pred1km"))
    expect_vector(predict(ssn_mod, ".missing"))
  })
}
