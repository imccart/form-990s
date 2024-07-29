
# Import data -------------------------------------------------------------

## NOTE: For 1985-1999, EZ forms are provided separately from full form 990s. For 2000-2007, EZ forms are included as part of the full data.
## Beginning in 2008, EZ forms were again separated from the full 990 data. 


## 2008 data
record.spec <- read_excel("data/input/microdata/irs-direct/eo2008_derl.xls", 
    range='A4:E190',
    sheet="EZ & EZ COMP",
    col_names=c("element_number","varname","start","length","type")) %>%
    mutate(start=as.numeric(start), length=as.numeric(length), type="c")
    
tax.dat08ez <- read_fwf("data/input/microdata/irs-direct/08eoezflat.txt",
    fwf_widths(record.spec$length, record.spec$element_number),
    col_types=paste(record.spec$type, collapse = "")) %>%
    rename_with(~ str_replace_all(., "-", "_")) %>%
    mutate(year=2008)



## 2009 data
record.spec <- read_excel("data/input/microdata/irs-direct/eo2009_derl.xls", 
    range='A4:E192',
    sheet="EZ990_09",
    col_names=c("element_number","varname","start","length","type")) %>%
    mutate(start=as.numeric(start), length=as.numeric(length), type="c")
    
tax.dat09ez <- read_fwf("data/input/microdata/irs-direct/09eoezflat.txt",
    fwf_widths(record.spec$length, record.spec$element_number),
    col_types=paste(record.spec$type, collapse = "")) %>%
    rename_with(~ str_replace_all(., "-", "_")) %>%
    mutate(year=2009)


## 2010 data
record.spec <- read_excel("data/input/microdata/irs-direct/2010/10eoderl.xls",
    range='A4:E193',
    sheet="EZ990_10",
    col_names=c("element_number","varname","start","length","type")) %>%
    mutate(start=as.numeric(start), length=as.numeric(length), type="c")
    
tax.dat10ez <- read_fwf("data/input/microdata/irs-direct/2010/10eoez.txt",
    fwf_widths(record.spec$length, record.spec$element_number),
    col_types=paste(record.spec$type, collapse = "")) %>%
    rename_with(~ str_replace_all(., "-", "_")) %>%
    mutate(year=2010)
