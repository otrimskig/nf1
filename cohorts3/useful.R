mutate(
  consistent_date = case_when(
    str_detect(injection_date, "/") ~ mdy(injection_date),
    str_detect(injection_date, "-") ~ ymd(injection_date),
    TRUE ~ NA_Date_
  )
)