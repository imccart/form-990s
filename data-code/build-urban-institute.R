
# Import data -------------------------------------------------------------

## Testing individual years
tax.dat1 <- read_csv(paste0("data/input/microdata/urban-institute/CORE-",i,"-501CE-NONPROFIT-PZ.csv"))
form990 <- nccsdata::get_data(dsname="core", time="1989", 
                    scope.orgtype="NONPROFIT",scope.formtype="PZ",
                    ntee=c("Health"))
form990.1989 <- as_tibble(form990) %>%
    select(FY=FISYR, EIN=EIN, NAME=NAME, 
            sales_receipts=P1SALES,
            total_expenses=P1TOTEXP,
            gross_income=GROSSINC,
            gross_receipts=GROSSREC,
            total_assets=P4E_ASST,
            total_liabilities=P4E_LIAB,
            retained_earnings=P4E_RETN) %>%
    mutate(year=1989)
           


form990 <- nccsdata::get_data(dsname="core", time="1990", 
                    scope.orgtype="NONPROFIT",scope.formtype="PZ",
                    ntee=c("Health"))
form990.1990 <- as_tibble(form990) %>%
    select(FY=FISYR, EIN=EIN, NAME=NAME, 
            sales_receipts=P1SALES,
            total_expenses=P1TOTEXP,
            gross_income=GROSSINC,
            gross_receipts=GROSSREC,
            total_assets=P4E_ASST,
            total_liabilities=P4E_LIAB,
            retained_earnings=P4E_RETN) %>%
    mutate(year=1990)



form990 <- nccsdata::get_data(dsname="core", time="1991", 
                    scope.orgtype="NONPROFIT",scope.formtype="PZ",
                    ntee=c("Health"))
form990.1991 <- as_tibble(form990) %>%
    select(FY=FISYR, EIN=EIN, NAME=NAME, 
            sales_receipts=P1SALES,
            total_expenses=P1TOTEXP,
            gross_income=GROSSINC,
            gross_receipts=GROSSREC,
            total_assets=P4E_ASST,
            total_liabilities=P4E_LIAB,
            retained_earnings=P4E_RETN) %>%
    mutate(year=1991)



## Full data in loop
tax.dat <- tibble()
for (i in 89:91) {

    form990 <- nccsdata::get_data(dsname="core", time=i, 
                        scope.orgtype="NONPROFIT",scope.formtype="PZ",
                        ntee=c("Health"))
    form990.1989 <- as_tibble(form990) %>%
        select(FY=FISYR, EIN=EIN, NAME=NAME, 
                sales_receipts=P1SALES,
                total_expenses=P1TOTEXP,
                gross_income=GROSSINC,
                gross_receipts=GROSSREC,
                total_assets=P4E_ASST,
                total_liabilities=P4E_LIAB,
                retained_earnings=P4E_RETN) %>%
        mutate(year=i)

    tax.dat <- bind_rows(tax.dat1, tax.dat)
}


