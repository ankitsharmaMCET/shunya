library(tidyverse)
library(data.table)
library(glue)
library(IDEEA.dev)
f <- "data-raw/Data Generation_and_Storage_of_ElectricityExcel.xlsx"
sheets <- readxl::excel_sheets(f)
# s <- sheets[2]

read_tech_data <- function(f, s, fill_na = T, range = "B4:F45",
                           storage = FALSE ) {
  # browser()
  x <- readxl::read_excel(f, s, range) %>% rename(parname = 1) %>%
    filter(!is.na(`2020`) | !is.na(`2030`) | !is.na(`2040`) | !is.na(`2050`)) %>%
    filter(!grepl("Gross Heat", parname, ignore.case = T)) %>%
    filter(!grepl("Startup cost", parname, ignore.case = T)) %>%
    mutate(param = as.character(NA), .after = 1)
  # as.data.table()

  "Electricity efficiency, condensation mode, gross (%), name plate"
  if (storage) {
    ii <- grepl("Charging efficiency", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) == 1) x$param[ii] <- "inpeff"
    jj <- ii
    ii <- grepl("Electricity efficiency, gross", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) == 1) x$param[ii] <- "eff_roundtrip"
  } else {
    ii <- grepl("Electricity efficiency", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) == 1) x$param[ii] <- "cinp2use"
  }

  "Technical lifetime (years)"
  if (!storage) {
    ii <- grepl("Technical lifetime", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) == 1) x$param[ii] <- "olife"
  } else {
    ii <- grepl("Technical inverter lifetime", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) == 1) x$param[ii] <- "olife_inverter"
    ii <- grepl("battery cycles", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) == 1) x$param[ii] <- "battery_cycles_max"
  }

  "Capital cost (cr. ₹/MW)"
  if (!storage) {
    ii <- grepl("Capital.cost", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) >= 1) x$param[ii][1] <- "invcost"
  } else {
    ii <- grepl("per MWh basis", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) >= 1) x$param[ii][1] <- "invcost_MWh"
    ii <- grepl("per MW basis", x$parname, ignore.case = T); summary(ii)
    if (sum(ii) >= 1) x$param[ii][1] <- "invcost_MW"
  }

  "Fixed O&M ( cr. ₹/MW/year)"
  ii <- grepl("Fixed.O.M", x$parname, ignore.case = T); summary(ii)
  if (sum(ii) >= 1) x$param[ii][1] <- "fixom"

  "Variable O&M (₹/MWh)"
  ii <- grepl("Variable.O.M", x$parname, ignore.case = T); summary(ii)
  if (sum(ii) >= 1) x$param[ii][1] <- "varom"

  "Minimum Up time (hours)"
  ii <- grepl("Minimum Up time", x$parname, ignore.case = T); summary(ii)
  if (sum(ii) == 1) x$param[ii] <- "rampup"

  "Minimum Down time (hours)"
  ii <- grepl("um Down time", x$parname, ignore.case = T); summary(ii)
  if (sum(ii) == 1) x$param[ii] <- "rampdown"

  "SO2 (mg/Nm^3 fuel)"
  ii <- grepl("SO2", x$parname, ignore.case = T); summary(ii)
  if (sum(ii) == 1) x$param[ii] <- "cinp2aout_SO2"

  "NOX (mg/Nm^3 fuel)"
  ii <- grepl("NOX", x$parname, ignore.case = T); summary(ii)
  if (sum(ii) == 1) x$param[ii] <- "cinp2aout_NOx"

  "Standard Particulate Matter (mg/Nm^3 fuel)"
  ii <- grepl("Particulate Matter", x$parname, ignore.case = T); summary(ii)
  if (sum(ii) == 1) x$param[ii] <- "cinp2aout_PM"

  # browser()
  dat <- x %>%
    select(-1) %>%
    pivot_longer(cols = `2020`:`2050`, names_to = "year",
                 values_transform = as.numeric,
                 names_transform = as.character) %>%
    filter(!is.na(param)) %>%
    # filter(!is.na(value)) %>%
    mutate(value = signif(as.numeric(value), 3),
           year = as.integer(year))

  if (fill_na) dat <- fill(dat, value, .direction = "down")

  list(
    sheet = s,
    desc = colnames(readxl::read_excel(f, s, "C3")),
    data = dat
  )
}

# read_tech_data <- function(f, s, fill_na = T) {
#   # browser()
#   x <- readxl::read_excel(f, s, "B4:F45") %>% rename(parname = 1) %>%
#     filter(!is.na(`2020`) | !is.na(`2030`) | !is.na(`2040`) | !is.na(`2050`)) %>%
#     filter(!grepl("Gross Heat", parname, ignore.case = T)) %>%
#     filter(!grepl("Startup cost", parname, ignore.case = T)) %>%
#     mutate(param = as.character(NA), .after = 1)
#   # as.data.table()
#
#   "Electricity efficiency, condensation mode, gross (%), name plate"
#   ii <- grepl("Electricity efficiency", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) == 1) x$param[ii] <- "cinp2use"
#
#   "Technical lifetime (years)"
#   ii <- grepl("Technical lifetime", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) == 1) x$param[ii] <- "olife"
#
#   "Capital cost (cr. ₹/MW)"
#   ii <- grepl("Capital.cost", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) >= 1) x$param[ii][1] <- "invcost"
#
#   "Fixed O&M ( cr. ₹/MW/year)"
#   ii <- grepl("Fixed.O.M", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) >= 1) x$param[ii][1] <- "fixom"
#
#   "Variable O&M (₹/MWh)"
#   ii <- grepl("Variable.O.M", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) >= 1) x$param[ii][1] <- "varom"
#
#   "Minimum Up time (hours)"
#   ii <- grepl("Minimum Up time", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) == 1) x$param[ii] <- "rampup"
#
#   "Minimum Down time (hours)"
#   ii <- grepl("um Down time", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) == 1) x$param[ii] <- "rampdown"
#
#   "SO2 (mg/Nm^3 fuel)"
#   ii <- grepl("SO2", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) == 1) x$param[ii] <- "cinp2aout_SO2"
#
#   "NOX (mg/Nm^3 fuel)"
#   ii <- grepl("NOX", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) == 1) x$param[ii] <- "cinp2aout_NOx"
#
#   "Standard Particulate Matter (mg/Nm^3 fuel)"
#   ii <- grepl("Particulate Matter", x$parname, ignore.case = T); summary(ii)
#   if (sum(ii) == 1) x$param[ii] <- "cinp2aout_PM"
#
#   # browser()
#   dat <- x %>%
#     select(-1) %>%
#     pivot_longer(cols = `2020`:`2050`, names_to = "year",
#                  values_transform = as.numeric,
#                  names_transform = as.character) %>%
#     filter(!is.na(param)) %>%
#     # filter(!is.na(value)) %>%
#     mutate(value = signif(as.numeric(value), 3),
#            year = as.integer(year))
#
#   if (fill_na) dat <- fill(dat, value, .direction = "down")
#
#   list(
#     sheet = s,
#     desc = colnames(readxl::read_excel(f, s, "C3")),
#     data = dat
#   )
# }

if (F) { # test
  s <- 11
  tch <- read_tech_data(f, s)
  tch$data %>% as.data.frame()
}

# function to create a technology using the imported data
fTech <- function(techdata, name, y, cinp, afs = NULL,
                  ystart = y, yend = y + 9) {
  # create technology object from x-data.frame
  if (F) { # test
    s <- sheets[2]; s
    y <- 2020; ystart = y; yend = y + 9
    name <- "ECOASUP"
    cinp <- "COA"
    afs <- 0.8
    techdata <- read_tech_data(f, s)
    techdata$data %>% print(n = 50)
  }
  x <- filter(techdata$data, year == y) %>% pivot_wider(names_from = param)
  # x %>% t()
  tech <- newTechnology(
    name = paste(name, y, sep = "_"),
    desc = glue("{techdata$desc}, {y}"),
    input = data.frame(
      comm = cinp,
      unit = "GWh"
    ),
    output = data.frame(
      comm = "ELC",
      unit = "GWh"
    ),
    cap2act = 8760,
    invcost = data.frame(
      invcost = convert("cr.INR/MW", "cr.INR/GW", x$invcost)
    ),
    fixom = list(
      fixom = convert("cr.INR/MW", "cr.INR/GW", x$fixom)
    ),
    # start = list(start = ystart),
    # end = list(end = yend),
    olife = list(olife = x$olife)
  )

  if (!is.null(ystart)) {
    tech <- tech %>%
      update(start = list(start = ystart))
  }

  if (!is.null(yend)) {
    tech <- tech %>%
      update(end = list(end = yend))
  }

  if (!is.null(x[["cinp2use"]])) {
    tech <- tech %>%
      update(
        ceff = data.frame(
          comm = cinp,
          cinp2use = x$cinp2use/100)
        )
  }

  if (!is.null(x[["cinp2aout_SO2"]])) {
    tech <- tech %>%
      update(
        aux = data.frame(
          acomm = c("NOX", "SOX", "PM"),
          unit = c("kt", "kt", "kt")
        ),
        aeff = data.frame(
          acomm = c("SOX", "NOX", "PM"),
          comm = cinp,
          cinp2aout = c( # check units & update
            x$cinp2aout_SO2/1e3,
            x$cinp2aout_NOx/1e3,
            x$cinp2aout_PM/1e3
          ))
      )
  }

  if (!is.null(x[["cinp2use"]])) {
    tech <- tech %>%
      update(
        ceff = data.frame(
          comm = cinp,
          cinp2use = x$cinp2use/100)
      )
  }

  if (!is.null(x[["rampup"]]) || !is.null(x[["rampdown"]])) {
    # x[["rampup"]]
    # tech@af
    tech <- tech %>%
      update(af = list(
        slice = "ANNUAL",
        rampup = if_else(is.null(x[["rampup"]]), NA, x[["rampup"]]),
        rampdown = if_else(is.null(x[["rampdown"]]), NA, x[["rampdown"]])
        )
      )
  }

  if (!is.null(afs)) {
    tech <- tech %>%
      update(afs = list(slice = "ANNUAL", afs.up = afs))
  }

  return(tech)
}

if (F) {
  fTech(tch, "ECOA", 2030, "COA") %>% draw()
  fTech(tch, "ECOA", 2050, "COA") %>% draw()
}

# function to create a technology for every year in the data
fTech_yy <- function(techdata, name, cinp, afs = NULL) {
  ll <- list()
  for (y in unique(techdata$data$year)) {
    ll[[paste(name, y, sep = "_")]] <- fTech(techdata, name, y, cinp, afs)
  }
  ll
}

if (F) {
  a <- fTech_yy(tch, "ECOA", "COA", .85)
  names(a)
  a$ECOA_2020 %>% draw()
}

# function to create a storage group of technologies using the data
# comm_elc - electricity commodity
# comm_stg - storage commodity
# if comm_stg != comm_els, charger and discsharger will be created as well
fStorage <- function(techdata, name = "STG_BTR", y,
                     comm_elc = "ELC", comm_stg = comm_elc,
                     # afs = NULL,
                     ystart = y, yend = y + 9) {
  # browser()
  # create technology object from x-data.frame
  if (F) { # test
    y <- 2020; ystart = y; yend = y + 9
    name <- "STG_BTR"
    comm_elc <- "ELC"
    comm_stg <- comm_elc
    # afs <- 0.8
    s <- "09 Battery storage, Li-ion"
    techdata <- read_tech_data(f, s, range = "B4:F27", storage = T)
    techdata$data %>% print(n = 50)
  }
  x <- filter(techdata$data, year == y) %>% pivot_wider(names_from = param)
  stg <- newStorage(
    name = paste(name, y, sep = "_"),
    desc = glue("{techdata$desc}, {y}"),
    commodity = comm_stg,
    cap2stg = 1,
    seff = list(inpeff = x$eff_roundtrip/100),
    invcost = data.frame(
      invcost = convert("cr.INR/MW", "cr.INR/GW", x$invcost_MWh)
    ),
    fixom = list(
      fixom = convert("cr.INR/MW", "cr.INR/GW", x$fixom)
    ),
    # varom = list(
    #   varom = convert("INR/MWh", "cr.INR/GWh", x$varom)
    # ),
    # start = list(start = ystart),
    # end = list(end = yend),
    olife = list(olife = x$olife_inverter)
  )

  if (!is.null(ystart)) {
    stg <- stg %>%
      update(start = list(start = ystart))
  }

  if (!is.null(yend)) {
    stg <- stg %>%
      update(end = list(end = yend))
  }

  if (comm_stg != comm_elc) {
    # create charging and discharging techs
    charger <- newTechnology(
      name = paste(stg@name, comm_stg, sep = '_'),
      desc = glue("{techdata$desc}, charger, {y}"),
      input = data.frame(
        comm = comm_elc,
        unit = "GWh"
      ),
      output = data.frame(
        comm = comm_stg,
        unit = "GWh"
      ),
      cap2act = 8760,
      invcost = data.frame(
        invcost = convert("cr.INR/MW", "cr.INR/GW", x$invcost_MW/2)
      ),
      fixom = list(
        fixom = convert("cr.INR/MW", "cr.INR/GW", x$fixom/2)
      ),
      # varom = list(
      #   varom = convert("INR/MWh", "cr.INR/GWh", x$varom)
      # ),
      start = stg@start,
      end = stg@end,
      olife = list(olife = x$olife_inverter)
    )
    # create charging and discharging techs
    discharger <- newTechnology(
      name = paste(stg@name, comm_stg, sep = '_'),
      desc = glue("{techdata$desc}, discharger, {y}"),
      input = data.frame(
        comm = comm_stg,
        unit = "GWh"
      ),
      output = data.frame(
        comm = comm_elc,
        unit = "GWh"
      ),
      cap2act = 8760,
      invcost = data.frame(
        invcost = convert("cr.INR/MW", "cr.INR/GW", x$invcost_MW/2)
      ),
      fixom = list(
        fixom = convert("cr.INR/MW", "cr.INR/GW", x$fixom/2)
      ),
      # varom = list(
      #   varom = convert("INR/MWh", "cr.INR/GWh", x$varom)
      # ),
      start = stg@start,
      end = stg@end,
      olife = list(olife = x$olife_inverter)
    )
    return(list(stg, charger, discharger))
  }
  return(stg)
}

# function to create a technology for every year in the data
fStorage_yy <- function(techdata, name, comm_elc = "ELC",
                        comm_stg = comm_elc) {
  # browser()
  ll <- list()
  for (y in unique(techdata$data$year)) {
    ll[[paste(name, y, sep = "_")]] <- fStorage(techdata, name, y,
                                                comm_elc = comm_elc,
                                                comm_stg = comm_stg)
  }
  ll
}


# Creating tech/stg objects for each type and year of investment,
# storing multiple objects in repository by fuel/type group

### ECOASUB - 01a Coal, sub-critical ####
s <- sheets[2]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, "ECOASUB", "COA", afs = .85)
a$ECOASUB_2030 %>% draw()

ECOASUB <- newRepository(
  name = "ECOASUB",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
names(ECOASUB@data)

### ECOASUP - 01b Coal, supercritical ####
s <- sheets[3]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, "ECOASUP", "COA", afs = .85)
names(a)
ECOASUP <- newRepository(
  name = "ECOASUB",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)

### ECOAULT - 01c Coal, ultra-supercritical ####
s <- sheets[4]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, "ECOAULT", "COA", afs = .85)
names(a)
ECOAULT <- newRepository(
  name = "ECOAULT",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(ECOAULT@data[[2]])

### ENGOC - 02a Gas turb. open cycle ####
s <- sheets[5]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "ENGOC", cinp = "GAS", afs = .90)
names(a)
a[[1]] %>% draw()
ENGOC <- newRepository(
  name = "ENGOC",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(ENGOC@data[[2]])

### ENGCC - 02b Gas turb. combined cycle ####
s <- sheets[6]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "ENGCC", cinp = "GAS", afs = .90)
names(a)
a[[1]] %>% draw()
ENGCC <- newRepository(
  name = "ENGCC",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(ENGCC@data[[2]])

### ENGEN - 02c Gas engine ####
s <- sheets[7]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "ENGEN", cinp = "GAS", afs = .90)
names(a)
a[[1]] %>% draw()
ENGEN <- newRepository(
  name = "ENGEN",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(ENGEN@data[[2]])

### EBIO - 03 Biomass extract. plant ####
s <- sheets[8]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "EBIO", cinp = "BIO", afs = .85)
names(a)
a[[1]] %>% draw()
EBIO <- newRepository(
  name = "EBIO",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(EBIO@data[[2]])

### EWIN - 04 Onshore turbines ####
s <- sheets[9]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "EWIN", cinp = "REN")
names(a)
a[[1]] %>% draw()
EWIN <- newRepository(
  name = "EWIN",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(EWIN@data[[2]])

### EWIF - 05 Offshore turbines ####
s <- sheets[10]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "EWIF", cinp = "REN")
names(a)
a[[1]] %>% draw()
EWIF <- newRepository(
  name = "EWIF",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(EWIF@data[[2]])

### ESOL - 06a Photovoltaics Large ####
s <- sheets[11]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "ESOL", cinp = "REN")
names(a)
a[[1]] %>% draw()
ESOL <- newRepository(
  name = "ESOL",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(ESOL@data[[2]])

### EHYD - 07d Large hydro, res ####
s <- sheets[16]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "EHYD", cinp = "REN", afs = .50) # assumption
names(a)
a[[1]] %>% draw()
EHYD <- newRepository(
  name = "EHYD",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(EHYD@data[[2]])

### ENUC - 08b Nuclear, LWR ####
s <- sheets[20]
s
tch <- read_tech_data(f, s)
tch$data %>% as.data.frame()

a <- fTech_yy(tch, name = "ENUC", cinp = "NUC", afs = .90)
names(a)
a[[1]] %>% draw()
ENUC <- newRepository(
  name = "ENUC",
  desc = tch$desc,
  misc = list(soruce = basename(f), sheet = tch$sheet, data = tch$data)
) %>% add(a)
draw(ENUC@data[[2]])

### STG_BTR - 09 Battery storage, Li-ion ####
s <- sheets[21]
s
stg <- read_tech_data(f, s, range = "B4:F27", storage = T)
stg$data %>% print(n = 50)
# g <-  fStorage(stg, y = 2020)

a <- fStorage_yy(stg, name = "STG_BTR", comm_elc = "ELC", comm_stg = "ELC")
names(a)
a[[1]] %>% class()
a[[1]] %>% length()
STG_BTR <- newRepository(
  name = "STG_BTR",
  desc = stg$desc,
  misc = list(soruce = basename(f), sheet = stg$sheet, data = stg$data)
) %>% add(a)
# draw(STG_BTR@data[[2]])


## Save data ####
ideea_techs <- list(
  ECOASUB = ECOASUB,
  ECOASUP = ECOASUP,
  ECOAULT = ECOAULT,
  ENGOC = ENGOC,
  ENGCC = ENGCC,
  ENGEN = ENGEN,
  EBIO = EBIO,
  EWIN = EWIN,
  EWIF = EWIF,
  ESOL = ESOL,
  EHYD = EHYD,
  ENUC = ENUC,
  STG_BTR = STG_BTR
)
save(ideea_techs, file = "data-raw/ideea_techs.RData")
