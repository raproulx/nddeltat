# load packages and functions ---------------------------------------------
library(readr)
library(lubridate)
library(purrr)
library(stringr)
library(fs)

# find monthly gallery directories without index.qmd ----------------------
gallery_dirs <- dir_ls(
  "./galleries",
  type = "directory"
)

gallery_qmd <- gallery_dirs |>
  dir_ls(type = "file", recurse = TRUE, regexp = "index.qmd")

gallery_update <- gallery_dirs[!gallery_dirs %in% path_dir(gallery_qmd)]

# create blank index.qmd file for new months  ----------------------------
write_qmd <- function(x) {
  txt <- c(
    "---",
    str_c(
      "title: ",
      my(str_c(path_file(x), " 2025")) |> format(format = "%B")
    ),
    str_c(
      "date: \"",
      mdy(str_c(path_file(x), " 1 ", year(Sys.Date()))),
      "\""
    ),
    "description: Historical maximum Delta T derived from North Dakota Agricultural Weather Network hourly data, which is available at [NDAWN.org](https://ndawn.ndsu.nodak.edu/current.html)",
    "editor: source",
    "---"
  )

  write_lines(txt, path(x, "index", ext = "qmd"))
}

walk(gallery_update, \(x) write_qmd(x))
