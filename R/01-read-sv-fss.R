library(qualtRics)
library(sjlabelled)

surveys <- all_surveys()

surveys <- surveys %>%
  filter(name %>% str_detect("fss.*survey"))

sv <- surveys %>%
  mutate(
    data = map(
      id,
      ~ fetch_survey_chr(.x, convert = FALSE) %>%
        label_to_colnames
      )
  )


sv <- sv %>% unnest(data) %>% clean_names %>%
  rename(survey_id = id, adult_id = caregiver_number_1_id) # id is survey-specific; from qualtRics

sv <- sv %>%
  separate(
    name,
    c("study_phase", "data_type", "time_point", "parent_sv"),
    "_"
  )


### Duplicates ####
## sv %>% get_dupes(owner_id)

# Thu Mar  3 18:18:11 2022 ------------------------------
## Shannon Pare through email
## Please use the first record (ID R_3oY2JExE8T2zjdF) in any case - they seem to be the same.
sv <- sv %>%
  filter(
    !(time_point == "t1" & adult_id == "HP204"
    & response_id != "R_3oY2JExE8T2zjdF")
  )

sv %>% select(starts_with("adult")) %>% names

### PIVOT ####

## cols starting with child_ren are not specific to child

sx <- sv %>%
  select(
    child_id,
    child_sex_selected_choice,
    adult_id,
    adult_gender_selected_choice
    )

sx <- sx %>%
  pivot_longer(
    everything(),
    names_to = c("role", ".value"),
    names_pattern = "^(adult|child)_(.*)"
    )

write_csv(sx %>% select(-role), paste0(cle_p, "ss_id_sex.csv"))
