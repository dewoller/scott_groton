## library() calls go here
library(conflicted)
library(dotenv)
library(drake)
library(purrrlyr )
library(workflowr)
library(seas)
library(magrittr)
library(stringr)
library(knitr)
library(lubridate)
library(foreign )
library(wrapr )   # for the qc function
library(tidyverse)
library(rmarkdown)
library(janitor)
library(caret)
library(parsnip)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

