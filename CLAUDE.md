# Hospital Form 990s

## Project Overview

This repository builds a panel dataset of hospital financial data from IRS Form 990 filings (1985-2010), linked to AHA (American Hospital Association) hospital identifiers. The output is a single tab-delimited file (`data/output/form990_ahaid.txt`).

## Repository Structure

```
data-code/
  _build_data.R          Main entry point; sources build-irs-raw.R, merges EIN-AHA crosswalks, exports final data
  build-irs-raw.R        Reads and standardizes IRS Form 990 fixed-width microdata (1985-2010)
  build-propublica.R     Standalone script; pulls Form 990 data + PDFs from ProPublica API (2010+)
  build-irs-raw-ez.R     Incomplete/exploratory script for Form 990-EZ data (2008-2010); not currently used
  build-urban-institute.R  Incomplete/exploratory script using Urban Institute nccsdata package; not currently used

data/input/microdata/    Symlinked to external storage; contains raw IRS files and EIN-AHA crosswalks
data/input/propublica/   ProPublica downloads: per-EIN subdirs with CSVs and PDFs
data/output/             Contains the final merged dataset
```

## Data Pipeline

1. `build-irs-raw.R` reads IRS SOI Tax Stats fixed-width files using companion Excel data dictionaries (derl files). File formats and variable names change across year ranges (1985-1997, 1998-1999, 2000-2005, 2006-2007, 2008, 2009, 2010), so each era has its own parsing logic. EZ filers are excluded for 2000-2007 (via `EZ_IND=="N"` filter); other year ranges have EZ data in separate files. Output is `final.tax.dat`.

2. `_build_data.R` loads two EIN-to-AHA crosswalk files produced by a former graduate student (Hanna Glenn):
   - `AHA_ein_matches.rds` — EIN-to-AHA matches by hospital/system name
   - `manual_matched_eins.rds` — additional hand-matched EINs where name matching failed
   The manual matches are coalesced into the primary crosswalk, then pivoted wide so each EIN maps to one or more AHA hospital IDs and system IDs. The crosswalk is joined onto the Form 990 data, and the result is exported.

## Key Financial Variables

All sourced from Form 990 filings:
- **Identity:** name, EIN, state, zip, exemption code
- **Income statement:** total_revenue, total_expenses, depreciation
- **Compensation:** comp_executive, comp_other, comp_pension, comp_benefits
- **Balance sheet:** total_assets, total_liabilities, net_assets, cash, investments (securities, land/building/equipment, other), fixed_assets, current_assets, current_liabilities

These support standard hospital financial ratios (total margin, operating margin, current ratio, days cash on hand, equity financing ratio, etc.) as described in the comments of `build-irs-raw.R`.

## ProPublica Pipeline (`build-propublica.R`)

Runs independently of `_build_data.R`. For each unique hospital EIN (NTEE codes E20–E24) in `form990_ahaid.txt`, it queries the ProPublica Nonprofit Explorer API, saves structured filing data (generally 2011+) and downloads all available PDFs. Resume-safe: per-EIN CSVs in `data/input/propublica/{ein}/` serve as checkpoints. Combined output goes to `data/output/form990_propublica.csv`. Configurable `max_eins` parameter at top of script for testing.

**Current status:** Running for ~4,994 hospital EINs. Has retry logic with exponential backoff for 429 rate-limit errors on PDF downloads.

**TODO:** Once the full run completes, add a "retry missing PDFs" pass. Some PDFs fail due to 429 errors even with retries, and the current resume logic skips EINs that already have a CSV (so missed PDFs aren't retried). Need to add logic that checks which PDFs *should* exist vs which *do* exist, and retries the missing ones.

## Code Conventions

- R only (no Python, no Makefile)
- Uses `pacman::p_load()` for package management
- Raw data files are gitignored (`data/*`); only code is tracked
- Input data lives on external/synced storage via symlink at `data/input/microdata`
- Fixed-width parsing relies on Excel "derl" (data element record layout) files shipped with the IRS microdata
