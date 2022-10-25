# Read qualtRics ####

surveys <- all_surveys()
surveys <- surveys %>%
  filter(name %>% str_detect("fss.*ahha"))

df <- surveys %>%
  mutate(
    data = map(
      id,
      ~ fetch_survey_chr(
        .x,
        convert = FALSE #  since some same cols have different levels
        # , # which cannot bind_rows when unnesting
        # force = TRUE
        ) %>%
        label_to_colnames %>%
        clean_names
      )
  )

df <- df %>%
  mutate(
    data = map(
      data,
      . %>%
        rename_with(
          ~ str_replace(.x, "(caregiver_number_\\d|p\\d)", "parent")
        )
    )
  )

df <- df %>%
  unnest(data) %>%
  rename(survey_id = id) # id is survey-specific; from qualtRics

df <- df %>%
  separate(
    name,
    c("study_phase", "data_type", "time_point", "parent_sv"),
    "_"
    ) %>%
  mutate(
    parent_sv = str_remove(parent_sv, "parent-"),
    progress = as.numeric(progress)
  )

df <- df %>%
  unite(
    col = recipient_name,
    recipient_first_name,
    recipient_last_name,
    sep = " ",
    na.rm = TRUE,
    remove = FALSE
  ) %>% # pastes first and last names with na.rm
  add_count(recipient_name, parent_sv)

df <- df %>%
  group_by(recipient_name, parent_sv) %>%
  filter(
    progress == max(progress)
    & start_date == max(start_date)
  ) %>%
  ungroup()

dupes <- df %>%
  get_dupes(recipient_name, parent_sv) %>%
  select(
    dupe_count, recipient_name, parent_sv,
    finished, progress, start_date
    ) %>%
  arrange(recipient_name)

stopifnot(nrow(dupes) == 0)

# df <- df %>%
#   rename_with(
#     ~ str_replace(.x, "^(child|parent)_", "\\1__")
#   )

# ## Checking whether the names from pivot longer will be duplicates
# ## of any current names
# current_names <- df %>% names
# names_to_add <- df %>% select(matches("__")) %>% names %>% str_remove(".*__")
# names_to_add %>% .[. %in% current_names]

df <- df %>%
  pivot_longer(
    matches("^(parent|child)"),
    names_pattern = "^(parent|child)_(.*)",
    names_to = c("role", ".value"),
    values_drop_na = TRUE
  )

# df <- df %>%
#   pivot_longer(
#     matches("^(parent|child)"),
#     names_to = c("role", ".value"),
#     names_sep = "__",
#     values_drop_na = TRUE
#   )

df <- df %>%
  rename(
    date = recorded_date,
    pregnant = pregnancy,
    bf = breastfeeding,
    comments = additional_comments,
    ht_choice = height_selected_choice,
    bm_choice = weight_selected_choice,
    ht_bm_notes = height_and_weight_notes,
    wc_choice = waist_circumference_selected_choice,
    wc_cm = waist_circumference_waist_circumference_to_nearest_0_1_cm_e_g_23_5_text,
    wc_notes = waist_circumference_notes
  ) %>%
  mutate(
    height_cm_converted = (30.48*as.numeric(height_imperial_feet)
    + 2.54*as.numeric(height_imperial_inches)) %>% as.character
  )

df <- df %>%
  mutate(
    ht_cm = coalesce(
      !!! select(.,
                 height_metric_cm,
                 height_cm_converted,
                 height_height_to_nearest_0_1_cm_e_g_54_2_text
      )
    ),
    bm_kg = coalesce(
      !!! select(., starts_with("weight_weight"))
    ),
    email = case_when(str_detect(id, "P") ~ recipient_email, TRUE ~ email),
    across(c(starts_with("recipient"), email), str_to_title),
    fid = external_data_reference
  ) %>%
  select(
    - recipient_email,
    - starts_with("weight_weight"),
    - starts_with("height"),
  )

writeClip <- function(object) {
  OS <- Sys.info()["sysname"]
  if(!(OS %in% c("Darwin", "Windows", "Linux"))) {
    stop("Copying to clipboard not yet supported on your OS")
  }
  switch(
    OS,
    Darwin = {
      con <- pipe("pbcopy", "w")
      writeLines(object, con=con)
      close(con)
    },
    Windows = writeClipboard(object, format = 1),
    Linux = {
      if (Sys.which("xclip") == "") {
        if (Sys.which("xclip") == "") {
          mess <- c("Clipboard on Linux requires 'xclip'. Try using:",
                    "sudo apt-get install xclip")
          message(paste(mess, collapse = "\n"))
        }
      }
      con <- pipe("xclip -selection clipboard -i", open = "w")
      writeLines(object, con=con)
      close(con)
    })
}

# df %>% filter(id == "HA46") %>% select(-matches("name|email|location|address")) %>%  write_csv(here("data", "fss_ahha_ha46.csv"))

vroom_write(
  df,
  paste0(cle_p, "ahha_ss.csv")
)


