# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2/4/2026
## Description:   Pull Form 990 data and PDFs from ProPublica Nonprofit Explorer API
##                for EINs in the existing IRS microdata panel.
##                Runs independently of _build_data.R.


# Settings ----------------------------------------------------------------

api_pause     <- 1    # seconds between organization API calls
pdf_pause     <- 2    # seconds between PDF downloads
max_eins      <- 10  # set to a finite number to test on a subset


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr2, jsonlite)

propublica_dir <- "data/input/propublica"
output_file    <- "data/output/form990_propublica.csv"


# Read unique EINs from existing data -------------------------------------

irs_dat <- read_tsv("data/output/form990_ahaid.txt", col_types = cols(.default = "c"))
eins    <- unique(na.omit(irs_dat$ein))
eins    <- eins[seq_len(min(length(eins), max_eins))]

cat(sprintf("Found %d unique EINs to query.\n", length(eins)))


# Create output directories -----------------------------------------------

dir.create(propublica_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)


# Column renaming map ------------------------------------------------------

rename_map <- c(
  tax_prd_yr         = "year",
  ein                = "ein",
  totrevenue         = "total_revenue",
  totfuncexpns       = "total_expenses",
  totassetsend       = "total_assets",
  totliabend         = "total_liabilities",
  totnetassetend     = "net_assets",
  compnsatncurrofcr  = "comp_executive",
  othrsalwages       = "comp_other",
  payrolltx          = "payroll_tax"
)


# Helper: download a single PDF -------------------------------------------

download_pdf <- function(url, dest) {
  if (is.null(url) || is.na(url) || url == "") return(invisible(NULL))
  if (file.exists(dest)) return(invisible(NULL))

  tryCatch({
    resp <- request(url) |>
      req_headers(`User-Agent` = "R hospital-form-990s research project") |>
      req_timeout(60) |>
      req_perform()
    writeBin(resp_body_raw(resp), dest)
    Sys.sleep(pdf_pause)
  }, error = function(e) {
    message(sprintf("  PDF download failed (%s): %s", basename(dest), conditionMessage(e)))
  })
}


# Helper: parse one organization response ----------------------------------

parse_filings <- function(org_ein, filings) {
  if (length(filings) == 0) return(tibble())

  rows <- map(filings, function(f) {
    as_tibble(lapply(f, function(x) if (is.null(x)) NA else x))
  })
  dat <- bind_rows(rows)

  # Rename columns that have a mapping
  for (old_name in names(rename_map)) {
    new_name <- rename_map[[old_name]]
    if (old_name %in% names(dat) && old_name != new_name) {
      dat <- rename(dat, !!new_name := !!sym(old_name))
    }
  }

  # Ensure ein column exists
  if (!"ein" %in% names(dat)) {
    dat$ein <- org_ein
  }

  dat
}


# Main loop ----------------------------------------------------------------

errors <- tibble(ein = character(), status = character(), message = character())

for (i in seq_along(eins)) {
  current_ein <- eins[i]
  ein_dir     <- file.path(propublica_dir, current_ein)
  data_file   <- file.path(ein_dir, paste0(current_ein, "_data.csv"))

  # Resume: skip if per-EIN CSV already exists
  if (file.exists(data_file)) {
    cat(sprintf("EIN %s (%d/%d): already done, skipping.\n", current_ein, i, length(eins)))
    next
  }

  # Query API
  api_url <- sprintf(
    "https://projects.propublica.org/nonprofits/api/v2/organizations/%s.json",
    current_ein
  )

  resp <- tryCatch({
    request(api_url) |>
      req_headers(`User-Agent` = "R hospital-form-990s research project") |>
      req_error(is_error = function(resp) FALSE) |>
      req_timeout(30) |>
      req_perform()
  }, error = function(e) {
    list(error = TRUE, message = conditionMessage(e))
  })

  # Handle connection/timeout errors
  if (is.list(resp) && !inherits(resp, "httr2_response") && isTRUE(resp$error)) {
    errors <- bind_rows(errors, tibble(ein = current_ein, status = "request_error", message = resp$message))
    cat(sprintf("EIN %s (%d/%d): request error — %s\n", current_ein, i, length(eins), resp$message))
    Sys.sleep(api_pause)
    next
  }

  # Handle HTTP errors
  status <- resp_status(resp)
  if (status == 404) {
    dir.create(ein_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv(tibble(), data_file)
    cat(sprintf("EIN %s (%d/%d): not found (404).\n", current_ein, i, length(eins)))
    Sys.sleep(api_pause)
    next
  }
  if (status != 200) {
    errors <- bind_rows(errors, tibble(ein = current_ein, status = as.character(status), message = "non-200 response"))
    cat(sprintf("EIN %s (%d/%d): HTTP %d, skipping.\n", current_ein, i, length(eins), status))
    Sys.sleep(api_pause)
    next
  }

  body <- resp_body_json(resp)

  # Parse structured filing data
  filing_dat <- parse_filings(current_ein, body$filings_with_data)

  # Collect all filings (with and without data) for PDF download
  all_filings <- c(body$filings_with_data, body$filings_without_data)

  # Download PDFs
  n_pdfs <- 0
  if (length(all_filings) > 0) {
    dir.create(ein_dir, recursive = TRUE, showWarnings = FALSE)
    for (f in all_filings) {
      pdf_url <- f$pdf_url
      if (is.null(pdf_url) || is.na(pdf_url) || pdf_url == "") next
      yr <- f$tax_prd_yr
      if (is.null(yr)) yr <- "unknown"
      pdf_dest <- file.path(ein_dir, paste0(current_ein, "_", yr, ".pdf"))
      if (!file.exists(pdf_dest)) {
        download_pdf(pdf_url, pdf_dest)
        n_pdfs <- n_pdfs + 1
      }
    }
  }

  # Save per-EIN CSV
  dir.create(ein_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(filing_dat, data_file)

  cat(sprintf("EIN %s (%d/%d): %d filings, %d PDFs downloaded.\n",
              current_ein, i, length(eins), nrow(filing_dat), n_pdfs))

  Sys.sleep(api_pause)
}


# Log errors ---------------------------------------------------------------

if (nrow(errors) > 0) {
  error_file <- file.path(propublica_dir, "errors.csv")
  write_csv(errors, error_file)
  cat(sprintf("\n%d EINs had errors. See %s\n", nrow(errors), error_file))
}


# Combine all per-EIN CSVs ------------------------------------------------

csv_files <- list.files(propublica_dir, pattern = "_data\\.csv$",
                        recursive = TRUE, full.names = TRUE)

combined <- map(csv_files, function(f) {
  tryCatch(read_csv(f, col_types = cols(.default = "c")), error = function(e) tibble())
}) |> bind_rows()

if (nrow(combined) > 0) {
  # Coerce financial columns to numeric where they match known names
  numeric_cols <- c("total_revenue", "total_expenses", "total_assets",
                    "total_liabilities", "net_assets", "comp_executive",
                    "comp_other", "payroll_tax", "year")
  for (col in intersect(numeric_cols, names(combined))) {
    combined[[col]] <- as.numeric(combined[[col]])
  }
  write_csv(combined, output_file)
  cat(sprintf("\nCombined dataset: %d rows, %d columns → %s\n",
              nrow(combined), ncol(combined), output_file))
} else {
  cat("\nNo filing data found across all EINs.\n")
}
