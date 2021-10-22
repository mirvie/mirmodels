# Generate documentation for roxygen to avoid repetition.

roxy_cohort_data_title <- function(cohort) {
  checkmate::assert_string(cohort, min.chars = 1)
  glue::glue("Get the {cohort} data.")
}

roxy_cohort_data_subtitle <- function(cohort) {
  checkmate::assert_string(cohort, min.chars = 1)
  glue::glue("Get the {cohort} gene count data and associated metadata.")
}

roxy_cohort_data_details <- function() {
  c(
    "There is an ID column called `mirvie_id`, a column called `cohort` which",
    "is just the first two characters in `mirvie_id`,",
    "and the rest of the data is gene counts."
  )
}

roxy_tidymodel_return <- function() {
  c(
    "A [parsnip::model_fit] object. To use this fitted model `mod` to make ",
    "predictions on some new data `df_new`, use ",
    "`predict(mod, new_data = df_new)`."
  )
}

roxy_mod_heading <- function(model_name) {
  checkmate::assert_string(model_name)
  glue::glue("Train a {model_name} model.")
}

roxy_mod_desc <- function(model_name, hyperparams) {
  checkmate::assert_string(model_name)
  checkmate::assert_character(hyperparams, min.len = 1, min.chars = 1)
  hyperparams <- stringr::str_c("`", hyperparams, "`") %>%
    glue::glue_collapse(sep = ", ", last = " and ")
  c(
    stringr::str_c(stringr::str_sub(roxy_mod_heading(model_name), 1, -2), ","),
    glue::glue("selecting hyperparameters {hyperparams} by cross-validation.")
  )
}

roxy_metric_char_vec <- function() {
  paste(
    "If you specify this as a multi-element character vector, the first",
    "element will be used to select the best model; subsequent metrics will",
    "also be reported for that model in the `cv_performance` attribute of the",
    "the returned object."
  )
}

roxy_de_return <- function() {
  c(
    "A tibble with 8 columns:",
    "* `gene`: The gene name.",
    "* `log2fc`: The log2 fold-change between case and control.",
    "* `pvalue`: The p-value for that gene being differentially expressed.",
    "* `padj`: Adjusted p-values.",
    "* `base_med`: The median raw value for the baseline samples.",
    "* `case_med`: The median raw value for the case samples.",
    "* `base_mean`: The mean raw value for the baseline samples.",
    "* `case_mean`: The mean raw value for the case samples."
  )
}

roxy_meta_pca_outlier <- function() {
  "* `meta_pca_outlier`: Is the sample a PCA outlier (found via robust PCA)?"
}

roxy_meta_major_race <- function() {
  "* `meta_major_race`: The race of the mother."
}
roxy_meta_ethnicity <- function() {
  "* `meta_ethnicity`: The mother's ethnicity."
}
roxy_meta_mom_age <- function() {
  "* `meta_mom_age`: The mother's age."
}
roxy_meta_deliveryga <- function() {
  paste(
    "* `meta_deliveryga`: The gestational age of the baby at delivery",
    "(according to ultrasound)."
  )
}
roxy_meta_parity <- function() {
  paste(
    "* `meta_parity`: The number of previous pregnancies that lasted at",
    "least 24 months."
  )
}
roxy_meta_gravida <- function() {
  paste("* `meta_gravida`: The number of previous pregnancies.")
}
roxy_meta_prom_or_pprom <- function() {
  "* `meta_prom_or_pprom`: Was there a (preterm) premature rupture of membrane?"
}
roxy_meta_prom <- function() {
  "* `meta_prom`: Was there a premature rupture of membrane?"
}
roxy_meta_iugr <- function() {
  "* `meta_iugr`: Was there intrauterine growth restriction?"
}
roxy_meta_smoker <- function() {
  "* `meta_smoker`: Is the mother a smoker?"
}
roxy_meta_labor_spontaneous <- function() {
  "* `meta_labor_spontaneous`: Was the labor spontaneous?"
}
roxy_meta_labor_augmented <- function() {
  "* `meta_labor_augmented`: Was the labor augmented?"
}
roxy_meta_labor_induced_complication <- function() {
  paste(
    "* `meta_labor_induced_complication`: Was the labor induced due to a",
    "complication?"
  )
}
roxy_meta_labor_induced_elective <- function() {
  paste(
    "* `meta_labor_induced_elective`:",
    "Was the labor electively induced?"
  )
}
roxy_meta_ivf <- function() {
  "* `meta_ivf`: Was it an IVF conception?"
}
roxy_meta_conception <- function() {
  "* `meta_conception`: The conception method."
}
roxy_meta_chronic_hypertension <- function() {
  "* `meta_chronic_hypertension`: Did the mother develop chronic hypertension?"
}
roxy_meta_collectionga <- function() {
  paste(
    "* `meta_collectionga`: The gestational age of the baby at the time",
    "of the blood draw (determined by ultrasound)."
  )
}
roxy_meta_min_before_spin <- function() {
  "* `meta_minutes_before_spin`: Minutes between blood draw and spin."
}
roxy_meta_n_prior_term_deliveries <- function() {
  paste(
    "* `meta_n_prior_term_deliveries`: The number of prior term deliveries",
    "that the mother has had."
  )
}
roxy_meta_n_prior_preterm_deliveries <- function() {
  paste(
    "* `meta_n_prior_preterm_deliveries`: The number of prior preterm",
    "deliveries that the mother has had."
  )
}
roxy_meta_baby_sex <- function() {
  "* `meta_baby_sex`: The sex of the baby."
}
roxy_meta_baby_weight_g <- function() {
  paste(
    "* `meta_baby_weight_g`: The weight of the newborn in grams.",
    "For twins this is the mean of the two."
  )
}
roxy_meta_weeks_to_delivery <- function() {
  "* `meta_weeks_to_delivery`: The time to delivery in weeks."
}
roxy_meta_mom_id <- function() {
  "* `meta_mom_id`: Unique identifier for each mother."
}
roxy_meta_draw <- function() {
  "* `meta_draw`: The draw number."
}
roxy_meta_mom_weight_kg <- function() {
  "* `meta_mom_weight_kg`: The mother's pre-pregnancy weight in kilograms."
}
roxy_meta_mom_height_cm <- function() {
  "* `meta_mom_height_cm`: The mother's pre-pregnancy height in centimetres."
}
roxy_meta_mom_bmi <- function() {
  "* `meta_mom_bmi`: The mother's pre-pregnancy BMI."
}
roxy_meta_former_smoker <- function() {
  "* `meta_former_smoker`: Is the mother a former smoker?"
}
roxy_meta_baby_head_circumference_cm <- function() {
  paste(
    "* `meta_baby_head_circumference_cm`:",
    "The circumference of the baby's head in centimetres.",
    "For twins this is the mean of the two."
  )
}
roxy_meta_baby_length_cm <- function() {
  paste(
    "* `meta_baby_length_cm`:",
    "The length of the baby in centimetres.",
    "For twins this is the mean of the two."
  )
}
roxy_meta_sample_type <- function() {
  "* `meta_sample_type`: Case or control."
}
roxy_meta_case_control_match_id <- function() {
  paste(
    "* `meta_case_control_match_id`:",
    "Cases and controls are pairwise-matched on this ID."
  )
}
roxy_meta_prev_pprom <- function() {
  paste(
    "* `meta_prev_pprom`:",
    "Has the mother had a previous preterm premature rupture",
    "of membranes?"
  )
}
roxy_meta_miscarriage <- function() {
  paste(
    "* `meta_prev_late_miscarriage`:",
    "Did the pregnancy end in miscarriage?"
  )
}
roxy_meta_prev_late_miscarriage <- function() {
  paste(
    "* `meta_prev_late_miscarriage`:",
    "Has the mother had a previous late miscarriage?"
  )
}
roxy_meta_prev_cervical_surgery <- function() {
  paste(
    "* `meta_prev_cervical_surgery`:",
    "Has the mother had a previous cervical surgery?"
  )
}
roxy_meta_low_risk_at_enrollment <- function() {
  paste(
    "* `meta_low_risk_at_enrollment`:",
    "Was the pregnancy classed as low-risk at enrollment?"
  )
}
roxy_meta_type_2_diabetes <- function() {
  "* `meta_type_2_diabetes`: Does the mother have type 2 diabetes?"
}
roxy_meta_autoimmune_disease <- function() {
  "* `meta_autoimmune_disease`: Does the mother have an auto-immune disease?"
}
roxy_meta_chronic_viral_infection <- function() {
  paste(
    "* `meta_chronic_viral_infection`:",
    "Does the mother have a chronic viral infection?"
  )
}
roxy_meta_antihypertensives <- function() {
  "* `meta_antihypertensives`: Is the mother taking antihypertensives?"
}
roxy_meta_immunosuppressants <- function() {
  "* `meta_immunosuppressants`: Is the mother taking immunosuppressants?"
}
roxy_meta_bv_history <- function() {
  "* `meta_bv_history`: Does the mother have a history of bacterial vaginosis?"
}
roxy_meta_first_pregnancy <- function() {
  "* `meta_first_pregnancy`: Is this the mother's first pregnancy?"
}
roxy_meta_n_prior_preg_0_13 <- function() {
  "* `meta_n_prior_preg_0_13`: Number of prior pregnancies lasting 0-13 weeks."
}
roxy_meta_n_prior_preg_14_23 <- function() {
  paste(
    "* `meta_n_prior_preg_14_23`:",
    "Number of prior pregnancies lasting 14-23 weeks."
  )
}
roxy_meta_n_prior_preg_24_plus <- function() {
  paste(
    "* `meta_n_prior_preg_24_plus`:",
    "Number of prior pregnancies lasting at least 24 weeks."
  )
}
roxy_meta_n_prior_preg_37_plus <- function() {
  paste(
    "* `meta_n_prior_preg_37_plus`:",
    "Number of prior pregnancies lasting at least 37 weeks."
  )
}
roxy_meta_tocolysis <- function() {
  "* `meta_tocolysis`: Is the mother on medication to suppress early labor?"
}
roxy_meta_steroids_fetal_lung <- function() {
  paste(
    "* `meta_steroids_fetal_lung`:",
    "Is the mother taking steroids for fetal lung development?"
  )
}
roxy_meta_antibiotics <- function() {
  "* `meta_antibiotics`: Is the mother on antibiotics?"
}
roxy_meta_progesterone <- function() {
  "* `meta_progesterone`: Is the mother on progesterone supplements?"
}
roxy_meta_emergengy_cerclage <- function() {
  paste(
    "* `meta_emergengy_cerclage`:",
    "Did the mother receive emergency cerclage",
    "(an effort to prolong pregnancy)."
  )
}
roxy_meta_mag_sulph <- function() {
  paste(
    "* `meta_mag_sulph`:",
    "Did the mother take magnesium sulphate",
    "(it's prescribed during pregnancy to prevent seizures",
    "in women with pre-eclampsia)."
  )
}
roxy_meta_maternal_pyrexia <- function() {
  "* `meta_maternal_pyrexia`: Did the mother develop a fever during pregnancy?"
}
roxy_meta_crp_highest_value <- function() {
  "* `meta_crp_highest_value`: CRP is a marker for inflammation."
}
roxy_meta_wcc_highest_value <- function() {
  "* `meta_wcc_highest_value`: White cell count in g/dL."
}
roxy_meta_blood_loss_ml <- function() {
  "* `meta_blood_loss_ml`: Blood loss during labor."
}
roxy_meta_threatened_preterm_labor <- function() {
  "* `meta_threatened_preterm_labor`: Did labor threaten prematurely?"
}
roxy_meta_pprom <- function() {
  "* `meta_pprom`: Was there preterm premature rupture of membranes?"
}
roxy_meta_obstetric_cholestasis <- function() {
  paste(
    "* `meta_obstetric_cholestasis`: Obstetric cholestasis",
    "is a disorder affecting the liver during pregnancy."
  )
}
roxy_meta_antepartum_haemorrhage <- function() {
  "* `meta_antepartum_haemorrhage`: Was there antepartum haemorrhaging?"
}
roxy_meta_vital_status <- function() {
  paste(
    "* `meta_vital_status`: The birth status",
    "(\"Live birth\", \"Stillbirth\" or \"Late miscarriage\")."
  )
}
roxy_meta_apgar <- function() {
  paste(
    "* `meta_apgar`: Health rating of the baby at birth.",
    "Scale: 1-10. Higher is better."
  )
}
roxy_meta_sbcu_or_nicu <- function() {
  "* `meta_sbcu_or_nicu`: Did the baby go into some sort of ICU?"
}
roxy_meta_neonatal_inpatient_nights <- function() {
  "* `meta_neonatal_inpatient_nights`: Number of neonatal inpatient nights."
}
roxy_meta_hrs_rom_to_delivery <- function() {
  paste(
    "* `meta_hrs_rom_to_delivery`:",
    "Hours between rupture of membrane and delivery."
  )
}
roxy_meta_study_purpose <- function() {
  "* `meta_study_purpose`: The purpose of the study."
}
roxy_meta_mom_gdm <- function() {
  "* `meta_mom_gdm`: Did the mother develop gestational diabetes?"
}
roxy_meta_pre_eclampsia <- function() {
  "* `meta_pre_eclampsia`: Did the mother develop pre-eclampsia?"
}
roxy_meta_mom_gbs <- function() {
  "* `meta_mom_gbs`: Was the mother infected with group B strep?"
}
roxy_meta_non_gdm_diabetic <- function() {
  "* `meta_non_gdm_diabetic`: Is the mother a non-GDM diabetic?"
}
roxy_meta_normal_delivery <- function() {
  "* `meta_normal_delivery`: Was it a normal delivery?"
}
roxy_meta_collection_num <- function() {
  "* `meta_collection_num`: The collection number."
}
roxy_meta_sga <- function() {
  "* `meta_sga`: Was the baby small for its gestational age at birth?"
}
roxy_meta_lupus <- function() {
  "* `meta_lupus`: Did the mother develop lupus during the pregnancy?"
}
roxy_meta_caesarean <- function() {
  "* `meta_caesarean`: Was the birth by C-section?"
}
roxy_meta_stillbirth <- function() {
  "* `meta_stillbirth`: Was it a stillbirth?"
}
roxy_meta_education <- function() {
  "* `meta_education`: The education level of the mother."
}
roxy_meta_multiples <- function() {
  "* `meta_multiples`: Twins or more?"
}
roxy_meta_batch_num <- function() {
  "* `meta_batch_num`: The batch number."
}
roxy_meta_sample_volume_ul <- function() {
  "* `meta_sample_volume_ul`: The sample volume in microlitres."
}
roxy_meta_q_pcr_actb_ct <- function() {
  "* `meta_q_pcr_actb_ct`: The qPCR cycling threshold for ACTB."
}
roxy_meta_q_pcr_ercc_ct <- function() {
  "* `meta_q_pcr_ercc_ct`: The qPCR cycling threshold for ERCCs."
}
roxy_meta_conception_spontaneous <- function() {
  "* `meta_conception_spontaneous`: Was the conception spontaneous?"
}
roxy_meta_conception_assisted <- function() {
  "* `meta_conception_spontaneous`: Was the conception medically assisted?"
}
roxy_lm_rlm_list <- function() {
  paste(
    "A list of fitted linear models",
    "(the results of calls to [stats::lm()] or [MASS::rlm()])."
  )
}
roxy_meta_pe_htn <- function() {
  paste(
    "* `meta_pe_htn`: Information about pre-eclampsia and hypertension",
    "on a sliding scale. Levels are `0_none`, `1_chronic_htn`,",
    "`2_severe_chronic_htn`, `3_pi_htn`, `4_severe_pi_htn`,",
    "`5_pe`, `6_severe_pe`, `7_pe_and_chronic_htn`,",
    "`8_pe_and_severe_chronic_htn`."
  )
}
roxy_meta_prenatal_screening_ga <- function() {
  paste(
    "* `meta_prenatal_screening_ga`: The gestational age at the time of",
    "prenatal screening."
  )
}
roxy_meta_ga_method <- function() {
  paste("* `meta_ga_method`: The method of determining the gestational age.")
}
roxy_meta_lab_qc_passed <- function() {
  "* `meta_lab_qc_passed`: Did the sample pass lab QC?"
}
roxy_meta_q_pcr_ver <- function() {
  "* `meta_q_pcr_ver`: The qPCR protocol version."
}