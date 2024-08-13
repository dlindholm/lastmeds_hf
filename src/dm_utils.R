# Helper functions ========================================================

# Define the different ways of referring to tablets (including typos! :) ) 
tabl <- "(tabletter|tabl|tablett|kapsel|kapslar|kaplsar|depotkapslar|depokapsel|depåkapsel|depotablett|depottablett|depottabl|filmdragerad tablett|st filmdragerad tablett|filmdrag tablett|filmdrag tabl|st tabletter|st depottablett|tab|tb|takl|st tablett|st kapsel|st kapslar|kaps|depotkaps|st depotkapsel|hård depotkapsel|st.|tbl|dosering|talett|urindrivande tablett|vattendrivande kapsel)"

# FUNCTION: paste_or ------------------------------------------------------
# Convenience function to collapse a vector into one string,
# combined using | .
# e.g. paste_or(atc$arb)
paste_or <- function(x) paste(x, collapse = "|")

# FUNCTION: clean_dosing --------------------------------------------------
# Will clean the `DOSER` variable
clean_dosing <- function(x){
  tolower(x)                                              |>
    str_replace_all("\\s+", " ")                          |>
    str_replace_all("\\(=furix\\)|=furix", "")            |>
    str_replace_all("1tabl", "1 tabl")                    |>
    str_replace_all("1kaps", "1 kaps")                    |>
    str_replace_all("1 en kapsel", "1 kapsel")            |>  
    str_replace_all("1st", "1 st")                        |>
    str_replace_all("1 depottablett", "1 tablett")        |>
    str_replace_all("1/2 depottablett", "0.5 tablett")    |>
    str_replace_all("1 blodtryckstablett", "1 tablett")   |>
    str_replace_all("en halv tabl", "0.5 tabl")           |>
    str_replace_all("1/2 (tabl|tbl)", "0.5 tabl")         |>
    str_replace_all("1/4", "0.25")                        |>
    str_replace_all("0.5 \\(en halv\\) tabl", "0.5 tabl") |>
    str_replace_all("en tab", "1 tab")                    |>
    str_replace_all("en kapsel", "1 kapsel")              |>
    str_replace_all("1 - tabl", "1 tabl")                 |>
    str_replace_all("1 - kapsel", "1 kapsel")             |>
    str_replace_all("1 - depotkapsel. hård", "1 kapsel")  |>
    str_replace_all("1 - depottablett", "1 tabl")         |>
    str_replace_all("2 - tabl", "2 tabl")                 |>
    str_replace_all("1-2 tabl", "1.5 tabl")               |>
    str_replace_all("1-2 kapslar", "1.5 kapslar")         |>
    str_replace_all("1/2-1 tabl", "0.75 tabl")            |>
    str_replace_all("½-1 tabl", "0.75 tabl")              |>
    str_replace_all("2-3 tabl", "2.5 tabl")               |>
    str_replace_all(",", ".")                             |>    
    str_replace_all("1,5", "1.5")                         |>
    str_replace_all("0,5", "0.5")                         |>
    str_replace_all("2,5", "2.5")                         |>
    str_replace_all("1½", "1.5")                          |>
    str_replace_all("2½", "2.5")                          |>
    str_replace_all("½", "0.5")                           |>
    str_replace_all("0.5 - tabl", "0.5 tabl")             |>
    str_replace_all("1.5 - tabletter", "1.5 tabl")        |>
    str_replace_all("en och 0.5 tabl", "1.5 tabl")        |>
    str_replace_all("1.5tabl", "1.5 tabl")                |>
    str_replace_all("en och halv", "1.5")                 |>
    str_replace_all("två", "2")                           |>
    str_replace_all("tre", "3")                           |>
    str_replace_all("fyra", "4")                          |>
    str_replace_all("1/2", "0.5")                         |>
    str_replace_all("klockan 08:00.", "klockan 08:00. ")           
}

# FUNCTION: get_dose ------------------------------------------------------
# Will read free text prescriptions from the prescribed drugs register data
# ('DOSER' variable) and convert to numeric values
get_dose <- function(x){

  # 1+0+1 or 1 + 0 or 1 + 0 + 1 + 1
  if(str_detect(x, "(0|[1-9]\\d*)(\\.\\d+)?\\s?\\+\\s?(0|[1-9]\\d*)(\\.\\d+)?\\s?(?:\\+\\s?(0|[1-9]\\d*)(\\.\\d+)?\\s?)")){
    tablets <- str_extract(x, "(0|[1-9]\\d*)(\\.\\d+)?\\s?\\+\\s?(0|[1-9]\\d*)(\\.\\d+)?\\s?(?:\\+\\s?(0|[1-9]\\d*)(\\.\\d+)?\\s?)") |>
      str_extract_all("(0|[1-9]\\d*)(\\.\\d+)?") |>
        unlist() |> 
          as.numeric() |>
            sum()
    return(tablets)
   }

  # 1 tabl [...]
  if(str_detect(x, paste0("[[:digit:]] ", tabl))){
    # Extract number of tablets
    tablets <- str_extract_all(x, paste0("(0|[1-9]\\d*)(\\.\\d+)? ", tabl)) |>
      unlist() |>
        str_extract_all("(0|[1-9]\\d*)(\\.\\d+)?") |>
          unlist() |>
            as.numeric() |>
              sum()

    if(str_detect(x, "[[:digit:]] gång")){
    # Extract times per day
      times <- str_extract(x, "[[:digit:]] gång") |>
        str_extract("[[:digit:]]") |>
          as.numeric()
    } else times <- 1L

    if(str_detect(x, "varannan dag|var tredje dag")){
    # Extract every other or every third day
      tmp <- str_extract(x, "varannan dag|var tredje dag")
      denom <- ifelse(tmp == "varannan dag", 2, 3)
    } else denom <- 1L

    # Morgon och kväll (and variants)
    if(str_detect(x, "(morgon och kväll|morgonen och kvällen|morgon och (lunch|lunchtid)|vid frukost och lunch|på morgonen och till lunch)")){
      tablets <- tablets * 2
    }   
  
    return(tablets * times / denom)
    }
  
  # 1X1 or 1*1 or 1 x 1
  if(str_detect(x, "(0|[1-9]\\d*)(\\.\\d+)?\\s?(x|X|\\*)\\s?\\d")){
    tablets <- str_extract(x, "(0|[1-9]\\d*)(\\.\\d+)?\\s?(x|X|\\*)\\s?\\d") |>
      str_extract_all("(0|[1-9]\\d*)(\\.\\d+)?") |>
        unlist() |> 
          as.numeric() |>
            prod()
    return(tablets)
    }

  # 1 dagl or 1 dgl or 1 st dagligen or 1 t dagligen
  if(str_detect(x, "(0|[1-9]\\d*)(\\.\\d+)?\\s? (dagl|dgl|st dagl|t dagl)")){
    tablets <- str_extract(x, "(0|[1-9]\\d*)(\\.\\d+)?\\s?(dagl|dgl|st dagl|t dagl)") |>
      str_extract("(0|[1-9]\\d*)(\\.\\d+)?") |>
        as.numeric()
    return(tablets)
  }

  # 2 på morgonen (och) 1 lunchtid or 2 på morgonen (och) 1 på kvällen
  if(str_detect(x, "(0|[1-9]\\d*)(\\.\\d+)?\\s?på morgonen( och)?\\s?(0|[1-9]\\d*)(\\.\\d+)?\\s?(på kvällen|lunchtid)" )){
    tablets <- str_extract(x, "(0|[1-9]\\d*)(\\.\\d+)?\\s?på morgonen( och)?\\s?(0|[1-9]\\d*)(\\.\\d+)?\\s?(på kvällen|lunchtid)" ) |>
      str_extract_all("(0|[1-9]\\d*)(\\.\\d+)?") |>
        unlist() |> 
          as.numeric() |>
            sum()
    return(tablets)
  }

  # 1 (ta) varannan dag
  if(str_detect(x, "(0|[1-9]\\d*)(\\.\\d+)?( ta)? varannan dag")){
    tablets <- str_extract(x, "(0|[1-9]\\d*)(\\.\\d+)?( ta)? varannan dag") |>
      str_extract("(0|[1-9]\\d*)(\\.\\d+)?") |>
        as.numeric()
    return(tablets / 2)
  }

  # 1 på morgonen|på kvällen|var morgon|till natten (contingent on no match above)
  if(str_detect(x, "(0|[1-9]\\d*)(\\.\\d+)? (på morgonen|på kvällen|var morgon|till natten|om dagen)")){
    tablets <- str_extract(x, "(0|[1-9]\\d*)(\\.\\d+)? (på morgonen|på kvällen|var morgon|till natten|om dagen)") |>
      str_extract("(0|[1-9]\\d*)(\\.\\d+)?") |>
        as.numeric()
    return(tablets)
  }

  # 1 st 2 gånger
  if(str_detect(x, "(0|[1-9]\\d*)(\\.\\d+)? st ([0-9.]+) gånger")){
    tablets <- str_extract_all(x, "(0|[1-9]\\d*)(\\.\\d+)?") |>
          unlist() |>
            as.numeric() |>
              prod()
    return(tablets)
  }
  
  # Return NA if unsuccessful: 
  NA
}

#END