render_ext <- function(time_point) {
  # assuming the output format of input.Rmd is PDF
  rmarkdown::render(
    paste0(rmd_p, "dt_review_ahha.RMD"),
    output_file = paste0("ahha_review_", time_point),
    params = list(time_point = time_point),
    envir = parent.frame()
  )
}

rmarkdown::render(
  here("Rmd", "dt_review_ahha_ss.RMD"),
  output_file = here(
    "results",
     glue('fss_t1_ahha_review_{Sys.Date()}.html')
  )
)
