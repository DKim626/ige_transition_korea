library(xml2)
library(here)
library(purrr)
library(stringr)


#### 1. Load codebook in .xml format ####
doc <- read_xml(here::here('data/PSID/raw/J353793_codebook.xml'))

parse_value_range <- function(x) {
  
  if (is.na(x) || !nzchar(x)) {
    return(tibble(val_min = NA_real_, val_max = NA_real_))
  }
  
  x2 <- str_replace_all(x, "–", "-")     # en-dash → hyphen
  
  m  <- str_match(x2,
                  "^\\s*([+-]?\\d[\\d,]*)\\s*(?:-\\s*([+-]?\\d[\\d,]*))?\\s*$")
  
  if (is.na(m[1,1])) return(tibble(val_min = NA_real_, val_max = NA_real_))
  
  a <- str_replace_all(m[1,2], ",", "")
  b <- if (!is.na(m[1,3])) str_replace_all(m[1,3], ",", "") else NA_character_
  
  if (is.na(b)) {
    tibble(val_min = suppressWarnings(as.numeric(a)),
           val_max = suppressWarnings(as.numeric(a)))
  } else {
    tibble(val_min = suppressWarnings(as.numeric(a)),
           val_max = suppressWarnings(as.numeric(b)))
  }
}



jobs <- xml_find_all(doc, ".//JOBID")

dict <- map_dfr(jobs, function(j){
  fileid <- xml_text(xml_find_first(j, "./FILEID"))
  vars   <- xml_find_all(j, "./LIST_VARIABLE/VARIABLE")
  map_dfr(vars, function(v){
    tibble(
      fileid  = fileid,
      year    = xml_text(xml_find_first(v, "./YEAR")),
      type_id = xml_text(xml_find_first(v, "./TYPE_ID")),
      varname = xml_text(xml_find_first(v, "./NAME")),
      label   = str_squish(xml_text(xml_find_first(v, "./LABEL"))),
      qtext   = str_squish(xml_text(xml_find_first(v, "./QTEXT"))),
      etext   = str_squish(xml_text(xml_find_first(v, "./ETEXT")))
    )
  })
}) %>%
  mutate(year = suppressWarnings(as.integer(year)),
         type_id = suppressWarnings(as.integer(type_id))) %>%
  arrange(fileid, year, varname)


codes_long <- map_dfr(jobs, function(j){
  fileid <- xml_text(xml_find_first(j, "./FILEID"))
  vars   <- xml_find_all(j, "./LIST_VARIABLE/VARIABLE")
  
  map_dfr(vars, function(v){
    varname <- xml_text(xml_find_first(v, "./NAME"))
    year    <- xml_text(xml_find_first(v, "./YEAR"))
    codes   <- xml_find_all(v, "./LIST_CODE/CODE")
    
    if (length(codes) == 0) return(tibble())
    
    map_dfr(codes, function(cn){
      val_txt <- str_squish(xml_text(xml_find_first(cn, "./VALUE")))
      lab_txt <- str_squish(xml_text(xml_find_first(cn, "./TEXT")))
      rng <- parse_value_range(val_txt)
      tibble(
        fileid      = fileid,
        year        = suppressWarnings(as.integer(year)),
        varname     = varname,
        value_raw   = val_txt,
        value_label = lab_txt,
        val_min     = rng$val_min,
        val_max     = rng$val_max
      )
    })
    
  })
}) %>%
  arrange(fileid, year, varname, val_min, val_max)

saveRDS(dict, file = here::here('data/PSID/clean/codebook.rds'))
saveRDS(codes_long, file = here::here('data/PSID/clean/codebook_value.rds'))




temp[stri_detect_fixed(temp, "HD")]

key_vars <- dict %>%
  filter(
    str_detect(varname, "^ER30001$|^ER30002$") |
      str_detect(label,
                 regex("RELATION.*HEAD|REL TO HEAD|RELATIONSHIP",
                       ignore_case = TRUE)) |
      str_detect(label,
                 regex(
                   "PERSON # OF MOTHER|PERSON # OF FATHER|PARENT.*POINTER|PNTR",
                   ignore_case = TRUE)
                 ) |
      str_detect(label, 
                 regex("SPLIT|FROM HHID|MERGE|SPLITOFF|ORIG HEAD|ORIG SPOUSE",
                       ignore_case = TRUE)
                 ) |
      str_detect(label, 
                 regex("COHORT|SEX OF INDIVIDUAL|AGE|BIRTH", 
                       ignore_case = TRUE)
                 )
  ) %>%
  arrange(year, varname)

write_csv(key_vars, "psid_key_vars_for_linking.csv")
