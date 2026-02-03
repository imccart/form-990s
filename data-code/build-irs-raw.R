
# Import data -------------------------------------------------------------

## From A Comparative Study of Financial Data Sources for Critical Access Hospitals..." by Ozmeral et al, 
##      Journal of Rural Health, May 2012.
## Guidestar info: https://www.guidestar.org/popup_digitizing_irs_form990_premium_pro.html

## Measures of profitability:
##  - Total Margin: Net Income/(Net Patient Service Revenue + Other Income)
##  - Operating Margin: Operating Income/(Net Patient Service Revenue + Other Income) ?
##  - Return on Assets: Net Income/Total Assets

## Liquidity indicators:
##  - Current Ratio: Current Assets/Current Liabilities
##  - Days cash on hand: (Cash + Marketable Securities + Investments)/ ((Total Expenses - Depreciation)/365)
##  - Net days in accounts receivable: (Net Accounts Receivable/ (Net Patient Service Revenue/365)

## Cost indicator:
##  - Salaries to total expense: (Salaries + Employee Benefits)/Total Expenses

## Capital structure:
##  - Equity financing ratio: Fund Balance/Total Assets
##  - Debt service coverage: (Net Income + Depreciation + Interest)/ (Interest + Short Term Notes Payable)
##  - Loong term debt to capitalization: (Short Term Notes Payable + Long Term Liabilities)/(Short Term Notes Payable + Long Term Liabilities + Fund Balance)

## NOTE: For 1985-1999, EZ forms are provided separately from full form 990s. For 2000-2007, EZ forms are included as part of the full data.
## Beginning in 2008, EZ forms were again separated from the full 990 data. 

## 1985 through 1997
record.range <- c('A7:E207','A8:E206','A8:E206','A8:E207','A7:E204',
                  'A7:E314','A7:E315','A7:E334','A7:E334','A7:E335',
                  'A5:E333','A7:E340','A5:E372')
tax.dat85 <- tibble()
for (i in 85:97) {
    y=1900+i
    record.spec <- read_excel(paste0("data/input/microdata/irs-direct/eo19",i,"_derl.xls"), 
        range=record.range[i-84],
        col_names=c("element_number","varname","start","length","type")) %>%
        mutate(start=as.numeric(start), length=as.numeric(length), type="c")
    
    tax.dat0 <- read_fwf(paste0("data/input/microdata/irs-direct/eo_c3_",i,".txt"),
        fwf_widths(record.spec$length, record.spec$element_number),
        col_types=paste(record.spec$type, collapse = "")) %>%
        rename_with(~ str_replace_all(., "-", "_")) %>%
        mutate(
            year=y,
            current_assets=rowSums(cbind(
                as.numeric(E161),
                as.numeric(E162),
                as.numeric(E163),
                as.numeric(E164),
                if (i>=89) as.numeric(E165) else 0,
                if (i %in% 89:93) 0 else as.numeric(E166),
                as.numeric(E167),
                as.numeric(E169),
                as.numeric(E170)
            ), na.rm=TRUE),
            current_liabilities=rowSums(cbind(
                as.numeric(E179),
                as.numeric(E180),
                if (i>=96) as.numeric(E181) else 0,
                as.numeric(E182),
                as.numeric(E184)
            ), na.rm=TRUE)
        ) %>%
        select(year, name=E002, ein=E003, state=E009, zip=E010, exemption=E011,
               total_revenue=E047, total_expenses=E052, net_assets=E056,
               comp_executive=E060, comp_other=E064, comp_pension=E068, comp_benefits=E072,
               depreciation=E125,
               total_assets=E178, cash=E161, investments_securities=E172, investments_lbe=E173, 
               fixed_assets=E175,
               investments_other=E174,
               total_liabilities=E186,
               current_assets, current_liabilities) %>%
        mutate(across(c(total_revenue:current_liabilities), as.numeric))

## beginning in 1990, other info is available including subsidiary info:
##                subsidiary1=P801, subsidiary2=P805, subsidiary3=P809, subsidiary4=P813,
##                subsidiary1_share=P802, subsidiary2_share=P806, subsidiary3_share=P810, subsidiary4_share=P814,
##                subsidiary1_income=P803, subsidiary2_income=P807, subsidiary3_income=P811, subsidiary4_income=P815

    tax.dat85 <- bind_rows(tax.dat0, tax.dat85)
}


## 1998 and 1999
record.range <- c('A11:E378','A9:E375')
tax.dat98 <- tibble()
for (i in 98:99) {
    y=1900+i
    record.spec <- read_xls(paste0("data/input/microdata/irs-direct/eo",i,"derl.xls"), 
        range=record.range[i-97],
        col_names=c("element_number","varname","start","length","type")) %>%
        mutate(start=as.numeric(start), length=as.numeric(length), type="c")
    
    tax.dat0 <- read_fwf(paste0("data/input/microdata/irs-direct/eo",i,"c3.txt"),
        fwf_widths(record.spec$length, record.spec$element_number),
        col_types=paste(record.spec$type, collapse = "")) %>%
        rename_with(~ str_replace_all(., "-", "_")) %>%
        mutate(
            year=y,
            current_assets=rowSums(cbind(
                as.numeric(E161),
                as.numeric(E162),
                as.numeric(E163),
                as.numeric(E164),
                as.numeric(E165),
                as.numeric(E166),
                as.numeric(E167),
                as.numeric(E169),
                as.numeric(E170)
            ), na.rm=TRUE),
            current_liabilities=rowSums(cbind(
                as.numeric(E179),
                as.numeric(E180),
                as.numeric(E181),
                as.numeric(E182),
                as.numeric(E184)
            ), na.rm=TRUE)
        ) %>%
        select(year, name=E002, ein=E003, state=E009, zip=E010, exemption=E011,
               total_revenue=E047, total_expenses=E052, net_assets=E056,
               comp_executive=E060, comp_other=E064, comp_pension=E068, comp_benefits=E072,
               depreciation=E125,
               total_assets=E178, cash=E161, investments_securities=E172, investments_lbe=E173, 
               fixed_assets=E175,
               investments_other=E174,
               total_liabilities=E186,
               current_assets, current_liabilities) %>%
        mutate(across(c(total_revenue:current_liabilities), as.numeric))
  
    tax.dat98 <- bind_rows(tax.dat0, tax.dat98)
}


## 2000 through 2007
record.range <- c('A6:E385','A6:E385','A6:E407','A6:E407','A6:E407','A5:E415','A5:E444','A5:E444')
tax.dat00 <- tibble()
for (i in 2000:2007) {
    if (i==2000) {
        flat.path=paste0("data/input/microdata/irs-direct/eo",i,".flat")
    } else {
        flat.path=paste0("data/input/microdata/irs-direct/eo",i,"flat.txt")
    }

    if (i==2001) {
        derl.path=paste0("data/input/microdata/irs-direct/eo",i,"_derl.xls")
    } else {
        derl.path=paste0("data/input/microdata/irs-direct/eo",i,"derl.xls")
    }
    record.spec <- read_excel(derl.path, 
        range=record.range[i-1999],
        col_names=c("element_number","varname","start","length","type")) %>%
        mutate(start=as.numeric(start), length=as.numeric(length), type="c")

    if (i<2006) {
        tax.dat0 <- read_fwf(flat.path,
            fwf_widths(record.spec$length, record.spec$element_number),
            col_types=paste(record.spec$type, collapse = "")) %>%
            rename_with(~ str_replace_all(., "-", "_")) %>%
            mutate(
                year=i,
                current_assets=if (i<2002) {
                    rowSums(cbind(
                        as.numeric(A040),
                        as.numeric(A050),
                        as.numeric(A060),
                        as.numeric(A070),
                        as.numeric(A080),
                        as.numeric(A090),
                        as.numeric(A100),
                        as.numeric(A110),
                        as.numeric(A120)
                    ), na.rm=TRUE)
                } else {
                    rowSums(cbind(
                        as.numeric(A040),
                        as.numeric(A050),
                        as.numeric(A003),
                        as.numeric(A004),
                        as.numeric(A005),
                        as.numeric(A006),
                        as.numeric(A007),
                        as.numeric(A110),
                        as.numeric(A011)
                    ), na.rm=TRUE)
                },
                current_liabilities=if (i<2002) {
                    rowSums(cbind(
                        as.numeric(L020),
                        as.numeric(L030),
                        as.numeric(L040),
                        as.numeric(L050),
                        as.numeric(L080)
                    ), na.rm=TRUE)
                } else {
                    rowSums(cbind(
                        as.numeric(L001),
                        as.numeric(L002),
                        as.numeric(L003),
                        as.numeric(L004),
                        as.numeric(L007)
                    ), na.rm=TRUE)
                }
            ) %>%
            filter(EZ_IND=="N") %>%
            select(year, name=E020, ein=EIN, state=E030, zip=E040, exemption=E050,
                total_revenue=R270, total_expenses=X050, net_assets=N040,
                comp_executive=F825, comp_other=F830, comp_pension=F835, comp_benefits=F840,
                depreciation=F910,
                total_assets=A180, cash=A040, investments_securities=A130, investments_lbe=A140, 
                investments_other=A150,
                fixed_assets=A160,                
                total_liabilities=L090,
                current_assets, current_liabilities) %>%
            mutate(across(c(total_revenue:current_liabilities), as.numeric))            
    } else {
        tax.dat0 <- read_fwf(flat.path,
            fwf_widths(record.spec$length, record.spec$element_number),
            col_types=paste(record.spec$type, collapse = "")) %>%
            rename_with(~ str_replace_all(., "-", "_")) %>%
            mutate(
                year=i,
                current_assets=rowSums(cbind(
                    as.numeric(A040),
                    as.numeric(A050),
                    as.numeric(A003),
                    as.numeric(A004),
                    as.numeric(A005),
                    as.numeric(A006),
                    as.numeric(A007),
                    as.numeric(A110),
                    as.numeric(A011)
                ), na.rm=TRUE),
                current_liabilities=rowSums(cbind(
                    as.numeric(L001),
                    as.numeric(L002),
                    as.numeric(L003),
                    as.numeric(L004),
                    as.numeric(L007)
                ), na.rm=TRUE)
            ) %>%
            filter(EZ_IND=="N") %>%
            select(year, name=E020, ein=EIN, state=E030, zip=E040, exemption=E050,
                total_revenue=R270, total_expenses=X050, net_assets=N040,
                comp_executive=F825, comp_other=F830, comp_pension=F835, comp_benefits=F840,
                depreciation=F910,
                total_assets=A180, cash=A040, investments_securities_pub=A125, 
                investments_securities_oth=A130, investments_lbe=A140, 
                investments_other=A150,
                fixed_assets=A160,           
                total_liabilities=L090,
                current_assets, current_liabilities) %>%
            mutate(across(c(total_revenue:current_liabilities), as.numeric)) %>%
            mutate(investments_securities=investments_securities_pub+investments_securities_oth)
    }
  
    tax.dat00 <- bind_rows(tax.dat0, tax.dat00)
}

## 2008 data
record.spec <- read_excel("data/input/microdata/irs-direct/eo2008_derl.xls", 
    range='A4:E695',
    col_names=c("element_number","varname","start","length","type")) %>%
    mutate(start=as.numeric(start), length=as.numeric(length), type="c")
    
tax.dat08 <- read_fwf("data/input/microdata/irs-direct/eo2008.flat",
    fwf_widths(record.spec$length, record.spec$element_number),
    col_types=paste(record.spec$type, collapse = "")) %>%
    rename_with(~ str_replace_all(., "-", "_")) %>%
    mutate(
        year=2008,
        current_assets=rowSums(cbind(
            as.numeric(csh_nnint_bearng_eoy),
            as.numeric(savngs_temp_csh_invst_eoy),
            as.numeric(accts_rcvbl_eoy),
            as.numeric(pledge_grnts_rcvbl_eoy),
            as.numeric(rcvbl_from_ofcrs_etc_eoy),
            as.numeric(rcvbl_from_dsqlfy_prsns_eoy),
            as.numeric(oth_nts_lns_rcvbl_net_eoy),
            as.numeric(invntry_for_sl_or_use_eoy),
            as.numeric(prepaid_exp_defrd_chrgs_eoy)
        ), na.rm=TRUE),
        current_liabilities=rowSums(cbind(
            as.numeric(accts_pybl_accr_exp_eoy),
            as.numeric(grnts_pybl_eoy),
            as.numeric(defrd_rev_eoy),
            as.numeric(lns_from_ofcr_dir_eoy),
            as.numeric(oth_liab_eoy)
        ), na.rm=TRUE)
    ) %>%
    select(year, name, ein, state, zip, exemption=subcd,
           total_revenue=tot_rev_cy, total_expenses=tot_expns_cy, net_assets=net_asts_or_fund_bals_eoy,
           comp_executive=comp_curr_ofcr_tot, comp_other=oth_sal_wg_tot, comp_pension=pnsn_plan_contris_tot, comp_benefits=oth_empl_bnfts_tot,
           depreciation=deprec_dpltn_tot,
           total_assets=tot_asts_eoy, cash=csh_nnint_bearng_eoy, investments_securities_pub=invst_pub_trd_sec_eoy, 
           investments_securities_oth=invst_oth_sec_eoy, fixed_assets=land_bldgs_equip_bss_net_eoy, 
           investments_lbe=d_tot_bk_vl_land_bldg,
           investments_other=invst_prg_rltd_eoy,
           total_liabilities=tot_liab_eoy,
           current_assets, current_liabilities) %>%
    mutate(across(c(total_revenue:current_liabilities), as.numeric)) %>%
    mutate(investments_securities=investments_securities_pub+investments_securities_oth)


## 2009 data
record.spec <- read_excel("data/input/microdata/irs-direct/eo2009_derl.xls", 
    range='A5:E852',
    col_names=c("element_number","varname","start","length","type")) %>%
    mutate(start=as.numeric(start), length=as.numeric(length), type="c")
    
tax.dat09 <- read_fwf("data/input/microdata/irs-direct/09eoflat.txt",
    fwf_widths(record.spec$length, record.spec$element_number),
    col_types=paste(record.spec$type, collapse = "")) %>%
    rename_with(~ str_replace_all(., "-", "_")) %>%
    mutate(
        year=2009,
        current_assets=rowSums(cbind(
            as.numeric(csh_nnint_bearng_eoy),
            as.numeric(savngs_temp_csh_invst_eoy),
            as.numeric(accts_rcvbl_eoy),
            as.numeric(pledge_grnts_rcvbl_eoy),
            as.numeric(rcvbl_from_ofcrs_etc_eoy),
            as.numeric(rcvbl_from_dsqlfy_prsns_eoy),
            as.numeric(oth_nts_lns_rcvbl_net_eoy),
            as.numeric(invntry_for_sl_or_use_eoy),
            as.numeric(prepaid_exp_defrd_chrgs_eoy)
        ), na.rm=TRUE),
        current_liabilities=rowSums(cbind(
            as.numeric(accts_pybl_accr_exp_eoy),
            as.numeric(grnts_pybl_eoy),
            as.numeric(defrd_rev_eoy),
            as.numeric(lns_from_ofcr_dir_eoy),
            as.numeric(oth_liab_eoy)
        ), na.rm=TRUE)
    ) %>%
    select(year, name, ein, state, zip, exemption=subcd,
           total_revenue=tot_rev_cy, total_expenses=tot_expns_cy, net_assets=net_asts_or_fund_bals_eoy,
           comp_executive=comp_curr_ofcr_tot, comp_other=oth_sal_wg_tot, comp_pension=pnsn_plan_contris_tot, comp_benefits=oth_empl_bnfts_tot,
           depreciation=deprec_dpltn_tot,
           total_assets=tot_asts_eoy, cash=csh_nnint_bearng_eoy, investments_securities_pub=invst_pub_trd_sec_eoy, 
           investments_securities_oth=invst_oth_sec_eoy, fixed_assets=land_bldgs_equip_bss_net_eoy, 
           investments_lbe=d_tot_bk_vl_land_bldg,
           investments_other=invst_prg_rltd_eoy,
           total_liabilities=tot_liab_eoy,
           current_assets, current_liabilities) %>%
    mutate(across(c(total_revenue:current_liabilities), as.numeric)) %>%
    mutate(investments_securities=investments_securities_pub+investments_securities_oth)



## 2010 data
record.spec <- read_excel("data/input/microdata/irs-direct/2010/10eoderl.xls",
    range='A5:E830',
    col_names=c("element_number","varname","start","length","type")) %>%
    mutate(start=as.numeric(start), length=as.numeric(length), type="c")
    
tax.dat10 <- read_fwf("data/input/microdata/irs-direct/2010/10eo.txt",
    fwf_widths(record.spec$length, record.spec$element_number),
    col_types=paste(record.spec$type, collapse = "")) %>%
    rename_with(~ str_replace_all(., "-", "_")) %>%
    mutate(
        year=2010,
        current_assets=rowSums(cbind(
            as.numeric(csh_nnint_bearng_eoy),
            as.numeric(savngs_temp_csh_invst_eoy),
            as.numeric(accts_rcvbl_eoy),
            as.numeric(pledge_grnts_rcvbl_eoy),
            as.numeric(rcvbl_from_ofcrs_etc_eoy),
            as.numeric(rcvbl_from_dsqlfy_prsns_eoy),
            as.numeric(oth_nts_lns_rcvbl_net_eoy),
            as.numeric(invntry_for_sl_or_use_eoy),
            as.numeric(prepaid_exp_defrd_chrgs_eoy)
        ), na.rm=TRUE),
        current_liabilities=rowSums(cbind(
            as.numeric(accts_pybl_accr_exp_eoy),
            as.numeric(grnts_pybl_eoy),
            as.numeric(defrd_rev_eoy),
            as.numeric(lns_from_ofcr_dir_eoy),
            as.numeric(oth_liab_eoy)
        ), na.rm=TRUE)
    ) %>%
    select(year, name, ein, state, zip, exemption=subcd,
           total_revenue=tot_rev_cy, total_expenses=tot_expns_cy, net_assets=net_asts_or_fund_bals_eoy,
           comp_executive=comp_curr_ofcr_tot, comp_other=oth_sal_wg_tot, comp_pension=pnsn_plan_contris_tot, comp_benefits=oth_empl_bnfts_tot,
           depreciation=deprec_dpltn_tot,
           total_assets=tot_asts_eoy, cash=csh_nnint_bearng_eoy, investments_securities_pub=invst_pub_trd_sec_eoy, 
           investments_securities_oth=invst_oth_sec_eoy, fixed_assets=land_bldgs_equip_bss_net_eoy, 
           investments_lbe=d_tot_bk_vl_land_bldg,
           investments_other=invst_prg_rltd_eoy,
           total_liabilities=tot_liab_eoy,
           current_assets, current_liabilities) %>%
    mutate(across(c(total_revenue:current_liabilities), as.numeric)) %>%
    mutate(investments_securities=investments_securities_pub+investments_securities_oth)


# Save final data ---------------------------------------------------------
final.tax.dat <- bind_rows(tax.dat85, tax.dat98, tax.dat00, tax.dat08, tax.dat09, tax.dat10)
