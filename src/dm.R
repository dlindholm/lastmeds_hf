################################################################################
# Heart failure treatment in the last years of life ============================
################################################################################
# Data management
# D. Lindholm

# Setup ==============================================================
  # Load required packages
  packages <- c("data.table", "Epi", "fst", "parallel", "stringr")
  invisible(lapply(packages, library, character.only = TRUE))

  # Define project home directory and data location
  proj_home <- "/lastmeds/" # Directory in Singularity container
  data_home <- "/data/"     # Directory in Singularity container

  # Load helper functions
  source(paste0(proj_home, "src/dm_utils.R"))

  # Settings for parallel processing
  setDTthreads(0L) # Use all threads with data.table
  cores <- detectCores() - 1L # Use all but one thread with e.g. mclapply


# Load data ==========================================================
  load_dataset <- function(x){
    read_fst(paste0(data_home, x), as.data.table = TRUE)
  }

  # Patient register
  patreg_long <- load_dataset("celosia_patreg_hf_pop.fst")
  patreg_agesex <- load_dataset("celosia_patreg_hf_pop_agesex_cols.fst")

  # Death register
  death_long <- load_dataset("celosia_death_hf_pop.fst")

  # Dispensed drugs register
  # ATC:C* and ATC:A10BK*
  lmed <- load_dataset("celosia_lmed_hf_pop_atc_c_a10bk.fst")

  # Defined daily doses
  ddd <- fread(paste0(proj_home, "data/ddd.csv"))


# Define study population ============================================
  # 1. Heart failure diagnosis at some point
  #       Based on Schaufelberger et al (ESC Heart Failure 2020; 7: 37–46),
  #       who reported high diagnostic validity if using only I50 as 
  #       diagnosis in either primary or secondary position.
  # 2. Dead
  #  
  # dhf_id contains patient IDs of all patients who fulfill above criteria

  patreg_long[, I50 := grepl("^I50", diag_icd10)]
  dhf_id <- death_long[
    id_pat %in% patreg_long[I50 == TRUE, unique(id_pat)],
      unique(id_pat)]

# ===> dhf_id


# Process death registry =============================================
    # Get death dates and convert to right format
    death_hf <- unique(death_long[id_pat %in% dhf_id, .(id_pat, DODSDAT)])
    
    # Convert date to YYYY-MM-DD
    death_hf[, dodth := as.Date(DODSDAT, format = "%Y%m%d")]
    
    # This fails in 2697 observations (0.49%); the exact death day is not known.
    #
    # For 251 individuals (0.046 %) only year of death is known.
    # (Unknown values are represented with 00 in both month and day positions.)
    # The death day will be imputed to July 1st in such cases
    # (approximate mid point of each year)
    death_hf[is.na(dodth), DODSDAT := gsub("0000$", "0701", DODSDAT)]

    # For 2446 individuals (0.45 %) only year and month of death are known. 
    # The death day will be imputed to the 15th day of each month in such cases.
    # (approximate mid point of each month)
    death_hf[is.na(dodth), DODSDAT := gsub("00$", "15", DODSDAT)]

    # Convert imputed dates
    death_hf[is.na(dodth), dodth := as.Date(DODSDAT, format = "%Y%m%d")]

    # Convert dates to decimal form
    death_hf[, dodth := cal.yr(dodth)][, DODSDAT := NULL]

    # Extract primary cause of death
    death_icd <- death_long[id_pat %in% dhf_id & diag_pos == "ulorsak"][
      , .(id_pat, code = icd10_code)]

# ===> death_hf
#   id_pat : patient ID
#   dodth  : death date
    death_meta <- list(dodth = "Date of death")

# ===> death_icd
#   id_pat : patient ID
#   code   : ICD10 code (underlying cause of death)


# Process prescribed drugs registry ==================================
  # Define list of ATC-codes of interest (heart failure meds):
    atc <- list(ace   = c("^C09A", "^C09B"),
                arb   = c("^C09C", "^C09DA", "^C09DB", "C09DX01"),
                arni  = c("C09DX04"),
                sglt2 = c("^A10BK"),
                bb    = c("^C07"),
                mra   = c("^C03DA"),
                loop  = c("^C03C"))

  # Select lmed to only include patients who died with HF and who had 
  # any prescription of the medications of interest:
    sp <- lmed[id_pat %in% dhf_id &
            grepl(paste_or(unlist(atc)), ATC)]

    rm(lmed); gc()

  # The process to calculate the treatment end date for each prescription
  # will be based by:

  # 1. If ApoDos prescription, treatment end date = EDATUM + 14d
  #    (as the vast majority of ApoDos prescriptions are for 14 days)

  # 2. Extract prescribed daily dose from free text field (DOSER), and 
  #    calculate from the number of packages (ANTAL) and package sizes (antnum)
  #
  # 3. Calculate from DDD

  # To account for 80% adherence, the treatment durations will be 1.25x that
  # of the calculated values.

  # Extract treatment end date from prescription text -------------
    # Get unique dosings (there are multiple with the same dosing text),
    # and order on frequency: 
    unique_dosings <- sp[, .N, by = "DOSER"]
    setorder(unique_dosings, -N)
    unique_dosings[, N := NULL]

    # Create index for joining with 'sp' later
    unique_dosings[, index := 1:.N]
    sp[unique_dosings, on = "DOSER", index := i.index]

    # Clean free text (to lowercase, remove excess whitespace, etc)
    unique_dosings[, DOSER := clean_dosing(DOSER)]

    # Extract doses from free text
    unique_dosings[, tablets := unlist(mclapply(DOSER, get_dose, mc.cores = cores))]

    # Create flag to identify 'as needed' prescriptions
    unique_dosings[, as_needed := grepl("VID BEHOV|VB|EFTER BEHOV|V B",
                                        DOSER, ignore.case = TRUE)]

    # Join with 'sp' dataset
    sp[unique_dosings, on = 'index', `:=` (tablets = i.tablets, as_needed = i.as_needed)]
  
    # Calculate 'dotxend' = date of treatment end (for each prescription)
    sp[as_needed == FALSE & tablets > 0, dotxend := EDATUM + 1.25 * (ANTAL * antnum / tablets)]
    
  # Extract ApoDos prescriptions ----------------------------------
    # Create ApoDos flag. A peculiarity in the dataset is that ApoDos have
    # decimal values for the ANTAL variable; i.e. flag all such all observations
    sp[, apodos := (ANTAL - floor(ANTAL)) > 0]
    
    # As ApoDos are dispensed every 14 days in the vast majority of cases, 
    # the calculated date of treatment end (for each prescription) will be
    # calculated as fulfillment date (EDATUM) + 14 days (see point 1. above;
    # this takes priority over the free text extraction)
    sp[apodos == TRUE, dotxend := EDATUM + (1.25 * 14)]
    rm(unique_dosings); gc()

  # Calculate DDD for remaining prescriptions (where dosing is missing) ----
    # Few (about 0.65%) of (non-'as needed') prescriptions lack 'dotxend'.
    # For these prescriptions, DDD will be used to calculate treatment period
    # Select those prescriptions,excluding i.v. treatments (which have a dosing
    # of mg/ml) and combination pills (which lack DDD). Both have a '/' 
    # in the 'styrkalf' variable

    d_ddd <- sp[is.na(dotxend) &
                as_needed == FALSE &
                !grepl("/", styrkalf) &
                styrkalf != ""]

    # All of the remaining doses are given in 'mg'. Convert from g -> mg in the
    # DDD table and combine with data
    ddd[, ddd := as.numeric(ddd)]
    ddd[unit == "g", `:=` (ddd = 1000 * ddd, unit = "mg")]
    d_ddd[ddd, on = .(ATC = atc), ddd := i.ddd]

    # The highly specific case of a Losartan packaging, which includes a few 
    # tablets of lower dose followed by the target dose of 50 mg, will be set
    # to that target dose. (<150 observations)
    d_ddd[styrkalf == "12,5 mg + 50 mg" & ATC == "C09CA01", styrkalf := "50 mg"]

    # Convert 'styrkalf' from string to numeric
    d_ddd[, styrkalf := str_replace_all(styrkalf, ",", ".")]
    d_ddd[, strength := as.numeric(str_extract(styrkalf, "(0|[1-9]\\d*)(\\.\\d+)?"))]

    # Calculate 'dotxend' = date of treatment end (for each prescription)
    d_ddd[, tablets := ddd / strength]
    d_ddd[, dotxend := EDATUM + 1.25 * (ANTAL * antnum / tablets)]

    # Combine with 'sp' dataset
    sp[d_ddd, on = c("id_pat", "ATC", "EDATUM"), dotxend := i.dotxend]

    # In the end, 0.09% of non-'as needed' prescriptions lack end date

  # Create variable for treatment class -------------------------
    # Create lookup data.table based on the ATC codes defined above
    tmp <- lapply(names(atc),
      function(x){
        data.table(txclass = x,
                   ATC     = grep(paste_or(atc[[x]]),
                                  unique(sp$ATC),
                                  value = TRUE))
      }) |> rbindlist()

    # Join with 'sp' dataset
    sp[tmp, on = 'ATC', txclass := i.txclass]
    rm(tmp)

  # Convert dates to decimal form -------------------------------
    sp[, (c("EDATUM", "dotxend")) := lapply(.SD, cal.yr), 
         .SDcols = c("EDATUM", "dotxend")]

  # Select variables to keep ------------------------------------
    sp <- sp[, .(id_pat,
                 txclass,
                 as_needed,
                 dotxstart = EDATUM,
                 dotxend,
                 atc = ATC)]
    
    sp <- unique(sp)

# ===> sp
#   id_pat    : patient ID
#   txclass   : treatment class
#   as_needed : is this an 'as needed' prescription?
#   dotxstart : date of treatment start
#   dotxend   : date of treatment end
#   atc       : ATC code

    sp_meta <- list(id_pat    = "Patient ID",
                    txclass   = "Treatment class",
                    as_needed = "Is this an 'as needed' prescription?", 
                    dotxstart = "Date of treatment start",
                    dotxend   = "Date of treatment end", 
                    atc       = "ATC code")

# Process patient registry ===========================================
    # Extract dob and sex --------------------------------------------
    # Exact dob is not in dataset, but estimated by subtracting age
    # from admission date from the first non-missing hospitalization
    setorder(patreg_agesex, id_pat, indate)
    agesex <- patreg_agesex[!is.na(indate), .SD[1L], by = id_pat]
    agesex[, dob := cal.yr(indate - (ALDER * 365.25))]
    agesex[, sex := fcase(KON == 1L, "M",
                          KON == 2L, "F")]
    agesex <- agesex[id_pat %in% dhf_id, .(id_pat, dob, sex)]

    # Extract index HF hospitalization -------------------------------
    hf_index <- patreg_long[I50 == TRUE & id_pat %in% dhf_id]
    setorder(hf_index, id_pat, indate)
    hf_index <- hf_index[, .SD[1L], by = id_pat]
    hf_index <- hf_index[, .(id_pat, indate)]
    hf_index[, dohf := cal.yr(indate)]
    hf_index[, indate := NULL]

# ===> agesex
#   id_pat     : patient ID
#   dob        : date of birth
#   sex        : sex, M or F

# ===> hf_index
#   id_pat     : patient ID
#   dohf       : date of index HF event (hospitalization or spec. visit)

  par_meta <- list(dob  = "Date of birth",
                   sex  = "Sex",
                   dohf = "Date of index HF event (HF diagnosis)")

# Comorbidities (from both death and patient registries) =============
  # Define comorbid conditions:
    comorbidities <- list(
      diabetes      = c("^E10", "^E11", "^E12", "^E13", "^E14"),
      hypertension  = c("^I10", "^I11", "^I12", "^I13", "^I15"),
      ihd           = c("^I20", "^I21", "^I22", "^I23", "^I24", "^I25"),
      pad           = c("^I70","^I71","^I72", "^I731", "^I739", "^I74", "^I773",
                        "^I776", "^I778", "^I79"),
      valvular      = c("^I05", "^I06", "^I07", "^I08", "^I34", "^I35", "^I36",
                        "^I37", "^Z952", "^Z953", "^Z954"),
      cmp           = "^I42",
      myocarditis   = c("^I012", "^I090", "^I40", "^I41"),
      congenital    = c("^Q20", "^Q21", "^Q22", "^Q23", "^Q24"),
      amyloidosis   = "^E85",
      thyroid       = c("^E00","^E01","^E02","^E03","^E04","^E05","^E06","^E07"),
      ckd           = "^N18", 
      afib          = "^I48",
      stroke        = c("^I60", "^I61", "^I62", "^I63", "^I64", "^I65", "^I66"),
      copd          = "^J44",
      pah           = "^I270",
      cancer        = "^C"
      )  

  # Combine death reg and patient reg into one large data.table
    comorb <- rbindlist(
      list(
        death_long[id_pat  %in% dhf_id, .(id_pat, icd10_code)],
        patreg_long[id_pat %in% dhf_id, .(id_pat, icd10_code = diag_icd10)]
      )
    )

  # Extract comorbidities from large table
    comorb[, names(comorbidities) := lapply(comorbidities,
                                      function(x) grepl(paste_or(x), icd10_code))]
    comorb <- comorb[, icd10_code := NULL][
                     , lapply(.SD, function(x) sum(x) > 0L), by = id_pat]
    setorder(comorb, id_pat)

  # Variable labels
    comorb_meta <- list(
      diabetes      = "Diabetes mellitus",
      hypertension  = "Hypertension",
      ihd           = "Ischemic heart disease",
      pad           = "Peripheral arterial disease",
      valvular      = "Valvular heart disease",
      cmp           = "Cardiomyopathy",
      myocarditis   = "Myocarditis",
      congenital    = "Congenital heart disease",
      amyloidosis   = "Amyloidosis",
      thyroid       = "Thyroid disease",
      ckd           = "Chronic kidney disease",
      afib          = "Atrial fibrillation/flutter",
      stroke        = "Stroke",
      copd          = "Chronic obstructive pulmonary disease",
      pah           = "Pulmonary arterial hypertension",
      cancer        = "Cancer"
                      )

# Combine and create final analysis datasets =========================
    # Combine death_hf, agesex, hf_index -------------
    dah <- merge(death_hf, agesex, by = "id_pat")
    dah <- merge(dah, hf_index,    by = "id_pat")
    setcolorder(dah, c("id_pat", "sex", "dob", "dohf", "dodth"))
    
    # Medication dataset -----------------------------
    d_meds <- merge(sp, dah, by = "id_pat", all = TRUE)
    setcolorder(d_meds, c("id_pat", "sex", "txclass", "atc", "as_needed", "dob",
      "dohf", "dotxstart", "dotxend", "dodth"))
    
    write_fst(d_meds, paste0(proj_home, "data/meds.fst"), compress = 100)
    
    # Comorbidities dataset --------------------------
    comorb <- merge(comorb, dah, by = "id_pat", all = TRUE)
    setcolorder(comorb, c("id_pat", "sex", "dob", "dohf", "dodth", 
      "hypertension", "ihd", "afib", "cancer", "diabetes", "ckd", "copd",
      "thyroid", "stroke", "valvular", "cmp", "amyloidosis", "congenital"))

    write_fst(comorb, paste0(proj_home, "data/comorb.fst"), compress = 100)

    # Causes of death dataset ------------------------
    write_fst(death_icd, paste0(proj_home, "data/death_icd.fst"), compress = 100)
    
    # Metadata ---------------------------------------
    vars <- c(sp_meta, par_meta, death_meta, comorb_meta)

    # Save ATC codes, comorbidities ICD codes and metadata -----
    saveRDS(atc, paste0(proj_home, "data/atc.rds"))
    saveRDS(comorbidities, paste0(proj_home, "data/comorb_icd.rds"))
    saveRDS(vars, paste0(proj_home, "data/vars.rds"))
# END