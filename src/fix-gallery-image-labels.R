# load packages and functions ---------------------------------------------
library(fs)
library(readr)
library(stringr)
library(lubridate)
library(purrr)

# find and read all gallery qmd files -------------------------------------
gallery_qmd <- dir_ls(
  "./galleries",
  type = "file",
  recurse = TRUE,
  regexp = "index.qmd"
)

# function to extract dates from filenames and format as labels -----------
qmd_modify <- function(x) {
  qmd_lines <- read_lines(x)

  date_pattern <- stamp("Thu, May 1, 2025")
  qmd_new <- qmd_lines[str_which(qmd_lines, "^\\!\\[\\]")] %>%
    str_replace(
      pattern = "^\\!\\[\\]",
      replacement = str_c(
        "![",
        str_match(., "\\d{4}-\\d{2}-\\d{2}") |> ymd() |> date_pattern(),
        "]"
      )
    )

  if (length(qmd_new) > 0) {
    qmd_lines[str_which(qmd_lines, "^\\!\\[\\]")] <- qmd_new
    write_lines(qmd_lines, file = x)
  }
}


# rewrite all qmd files ---------------------------------------------------
walk(gallery_qmd, \(x) qmd_modify(x))
