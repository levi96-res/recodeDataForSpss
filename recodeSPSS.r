library('haven')
library('stringr')
recodeSPSS <- function(df) {
  if (is.data.frame(df)) {
    varnames = names(df) # list of vars of data frame
    
    for (i in varnames) {
      # checking if variable fit
      if ((stringr::str_detect(df[[i]][3], '[:digit:]\\.') && class(df[[i]]) =='character')) {
        message(paste('Variable ', i, sep = ''))
        print(names(table(df[[i]]))) # print the recode note
        
        # recoding
        value <- names(table(df[[i]]))
        for (j in seq_len(length(value))) {
          df[[i]][df[[i]] == value[j]] <- j
        }
        
        # labelling
        labels <- seq_len(length(value))
        class(labels) <- "character"
        names(labels) <- value
        df[[i]] <- haven::labelled(df[[i]], labels = labels)
      }
    }
    
    # returning data frame
    return(df)
  } else {
    message('Input must be a well structured data frame!')
  }
}

# write data
# haven::write_sav(ss, "a.sav")