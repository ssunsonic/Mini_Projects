source("~/Documents/UCD Classes/22-23/Spring Q23 Classes/STA 141B/HW1/141B_HW1_Funtions.R")

wea_tables = lapply(
  c(
    "USA_CA_Fairfield/fairfield.wea",
    "USA_CA_Napa/napa.wea",
    "USA_CA_UC_Davis/davis.wea",
    "USA_CA_Point/point_reyes.wea",
    "USA_CA_Marin/marin.wea"
  ),
  open_wea
)

pvsyst_tables = lapply(
  c(
    "USA_CA_Fairfield/fairfield.pvsyst",
    "USA_CA_Napa/napa.pvsyst",
    "USA_CA_UC_Davis/davis.pvsyst",
    "USA_CA_Point/point_reyes.pvsyst",
    "USA_CA_Marin/marin.pvsyst"
  ),
  open_pvsyst
)

davis_monthly = open_monthly(
  "USA_CA_UC_Davis/davis.stat",
  c(
    "Monthly Statistics for Dry Bulb temperatures",
    "Monthly Statistics for Dew Point temperatures",
    "Monthly Wind Direction",
    "Monthly Statistics for Wind Speed"
  )
)

davis_hourly = open_hourly(
  "~/Documents/UCD Classes/22-23/Spring Q23 Classes/STA 141B/HW1/USA_CA_UC_Davis/davis.stat",
  c(
    "Average Hourly Statistics for Dry Bulb temperatures",
    "Average Hourly Statistics for Dew Point temperatures",
    "Average Hourly Relative Humidity",
    "Average Hourly Statistics for Direct Normal Solar Radiation",
    "Average Hourly Statistics for Wind Speed"
  )
)

davis_hourly_data = merge_hourly_data(
  "USA_CA_UC_Davis/davis.stat",
  c(
    "Average Hourly Statistics for Dry Bulb temperatures",
    "Average Hourly Statistics for Dew Point temperatures",
    "Average Hourly Relative Humidity",
    "Average Hourly Statistics for Direct Normal Solar Radiation",
    "Average Hourly Statistics for Wind Speed"
  )
)

fairfield_hourly_plots = plot_hourly(
  "USA_CA_Fairfield/fairfield.stat",
  c(
    "Average Hourly Statistics for Dry Bulb temperatures",
    "Average Hourly Statistics for Dew Point temperatures",
    "Average Hourly Relative Humidity",
    "Average Hourly Statistics for Direct Normal Solar Radiation",
    "Average Hourly Statistics for Wind Speed"
  )
)

napa_hourly_plots = plot_hourly(
  "USA_CA_Napa/napa.stat",
  c(
    "Average Hourly Statistics for Dry Bulb temperatures",
    "Average Hourly Statistics for Dew Point temperatures",
    "Average Hourly Relative Humidity",
    "Average Hourly Statistics for Direct Normal Solar Radiation",
    "Average Hourly Statistics for Wind Speed"
  )
)

davis_hourly_plots = plot_hourly(
  "USA_CA_UC_Davis/davis.stat",
  c(
    "Average Hourly Statistics for Dry Bulb temperatures",
    "Average Hourly Statistics for Dew Point temperatures",
    "Average Hourly Relative Humidity",
    "Average Hourly Statistics for Direct Normal Solar Radiation",
    "Average Hourly Statistics for Wind Speed"
  )
)

point_reyes_hourly_plots = plot_hourly(
  "USA_CA_Point/point_reyes.stat",
  c(
    "Average Hourly Statistics for Dry Bulb temperatures",
    "Average Hourly Statistics for Dew Point temperatures",
    "Average Hourly Relative Humidity",
    "Average Hourly Statistics for Direct Normal Solar Radiation",
    "Average Hourly Statistics for Wind Speed"
  )
)

marin_hourly_plots = plot_hourly(
  "USA_CA_Marin/marin.stat",
  c(
    "Average Hourly Statistics for Dry Bulb temperatures",
    "Average Hourly Statistics for Dew Point temperatures",
    "Average Hourly Relative Humidity",
    "Average Hourly Statistics for Direct Normal Solar Radiation",
    "Average Hourly Statistics for Wind Speed"
  )
)