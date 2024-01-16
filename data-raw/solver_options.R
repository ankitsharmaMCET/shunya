# ideea_data <- "D:/Dropbox/India/IDEEA/ideea_data"
# devtools::install_github("GAMS-dev/gdxrrw/gdxrrw")
gams_cmd_line <- "C:/GAMS/win64/32.2/gams.exe energyRt.gms"
gdx_library <- "C:/GAMS/35"
try({
  library(gdxrrw)
  igdx(gdx_library)
  library(gdxtools)
})



# memory managements in GAMS: https://support.gams.com/solver:error_1001_out_of_memory
{gams_options_parallel <- "
*$exit
energyRt.holdfixed = 1;
*energyRt.dictfile = 0;
option solvelink = 0;
*option InteractiveSolver = 1;
option iterlim = 1e9;
option reslim = 1e7;
option threads = 12;
*option solvelink = 5;
*option LP = CPLEX;
energyRt.OptFile = 1;
*option savepoint = 1;
*option bRatio = 0;
*execute_loadpoint 'energyRt_p';
$onecho > cplex.opt
*interactive 1
advind 0
* predual 1
* BarStartAlg 4
* tuningtilim 2400
*aggcutlim 3
*aggfill 10
*aggind 25
*bardisplay 2
parallelmode -1
lpmethod 6
*printoptions 1
names no
freegamsmodel 1
*memoryemphasis 1
threads 0
*barepcomp 1e-5
*scaind 1
*predual -1
*solutiontype 2

epopt 1e-1
eprhs 1e-1
barepcomp 1e-5
epmrk 0.1

$offecho
*$exit
"} # GAMS options ####


{gams_options_barrier <- "
*$exit
energyRt.holdfixed = 1;
*energyRt.dictfile = 0;
*option solvelink = 5;
*option InteractiveSolver = 1;
option iterlim = 1e9;
option reslim = 1e7;
option threads = 12;
*option LP = CPLEX;
energyRt.OptFile = 1;
option savepoint = 1;
*option bRatio = 0;
*execute_loadpoint 'energyRt_p';

$onecho > cplex.opt
*interactive 1
advind 0
* predual 1
* BarStartAlg 4
* tuningtilim 2400
*aggcutlim 3
*aggfill 10
*aggind 25

parallelmode -1
threads -1
lpmethod 4
*reinv 1e4

* preind: turn presolver on/off (1/0)
*preind 0
*scaind 1
scaind -1
*predual -1
solutiontype 2

*printoptions 1
names no
*freegamsmodel 1
*memoryemphasis 1

*barcolnz 5
numericalemphasis 1
barepcomp 1e-5
barstartalg 2
*predual 1
*baralg 1

epopt 1e-1
eprhs 1e-1
dpriind 2
ppriind 3
perind 1
epmrk 0.1

tuningdisplay 2
*simdisplay 2
*bardisplay 2

*CraInd 0

$offecho
"} # GAMS barrier ####


{gams_options_simplex <- "
*$exit
energyRt.holdfixed = 1;
*energyRt.dictfile = 0;
option solvelink = 5;
*option InteractiveSolver = 1;
option iterlim = 1e9;
option reslim = 1e7;
option threads = 12;
*option LP = CPLEX;
energyRt.OptFile = 1;
option savepoint = 1;
*option bRatio = 0;
*execute_loadpoint 'energyRt_p';
$onecho > cplex.opt
*interactive 1
*advind 0
* predual 1
* BarStartAlg 4
* tuningtilim 2400
*aggcutlim 3
*aggfill 10
*aggind 25

parallelmode -1
threads 12
lpmethod 6
*reinv 1e4

* preind: turn presolver on/off (1/0)
*preind 0
*scaind 1
* scaind -1
*predual -1
solutiontype 2

*printoptions 1
names no
*freegamsmodel 1
*memoryemphasis 1

*barcolnz 5
barepcomp 1e-1

epopt 1e-1
eprhs 1e-1
dpriind 2
ppriind 3
perind 1
epmrk 0.1

tuningdisplay 2
*simdisplay 2
bardisplay 2

CraInd 0

$offecho
"} # Simplex



# {cplex_options <- "
# interactive 1
# CPXPARAM_Advance 0
# *tuningtilim 2400
# aggcutlim 3
# aggfill 10
# aggind 25
# bardisplay 2
# parallelmode -1
# lpmethod 6
# *printoptions 1
# names no
# *freegamsmodel 1
# *memoryemphasis 1
# threads -1
# "} # CPLEX parameters ####

{inc4 <-
  "parameter zModelStat;
zModelStat = energyRt.ModelStat;
execute_unload 'solved_scenario.gdx';"} # inc4.gms

gams_cmd_line_mac <- "/Applications/GAMS27.2/sysdir/gams energyRt.gms"


GAMS_csv <- list(
  lang = "GAMS",
  export_format = "gdx",
  import_format = "gdx",
  # inc3 = gams_options_parallel,
  cmdline = gams_cmd_line,
  # solver = "cbc",
  solver = "cplex"
)

GAMS_parallel <- list(
  lang = "GAMS",
  export_format = "gdx",
  import_format = "gdx",
  inc3 = gams_options_parallel,
  cmdline = gams_cmd_line,
  # solver = "cbc",
  solver = "cplex"
)

if (Sys.info()["sysname"] == "Darwin") {
  GAMS_parallel$export_format <- "csv"
  GAMS_parallel$import_format <- "csv"
  GAMS_parallel$cmdline <- gams_cmd_line_mac
}

GAMS_barrier <- list(
  lang = "GAMS",
  export_format = "gdx",
  import_format = "gdx",
  inc3 = gams_options_barrier,
  cmdline = gams_cmd_line,
  # solver = "cbc",
  solver = "cplex"
)

GAMS_simplex <- list(
  lang = "GAMS",
  export_format = "gdx",
  import_format = "gdx",
  inc3 = gams_options_simplex,
  cmdline = gams_cmd_line,
  # solver = "cbc",
  solver = "cplex"
)

Pyomo <- list(
  lang = "PYOMO",
  export_format = "SQLite",
  # solver = "cplex"
  # solver = "glpk"
  solver = "cbc"
)

JuMP = list(
  lang = "JuMP"
  # files = list(
  #   cplex.opt = cplex_options
  # )
)

gams_cmd_line <- "/Applications/GAMS25.1/sysdir/gams energyRt.gms"


GAMS_mac <- list(
  lang = "GAMS",
  # export_format = "gdx",
  # import_format = "gdx",
  inc3 = gams_options_parallel,
  cmdline = gams_cmd_line,
  # solver = "cbc",
  solver = "cplex"
)
