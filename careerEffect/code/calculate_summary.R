# This script calculates various summaries of the data. 

# Library
library(tidyverse)

# Paths
path.data.candidate <- "./data/dataverse_files/Reedsmith_complete.csv"
path.data.first.win <- "./careerEffect/data/first_win.RDS"
path.data.first.run <- "./careerEffect/data/first_run.RDS"

# Data
data.candidate <- read_csv(path.data.candidate) %>% 
  filter(
    year <= 2017
  )
data.winner <- data.candidate %>% 
  filter(
    result != 0
  )
data.first.win <- read_rds(path.data.first.win) %>% 
  filter(
    year <= 2017
  )
data.first.run <- read_rds(path.data.first.run) %>% 
  filter(
    year <= 2017
  )

# Calculate various summaries of data: first run
# 1. Age of entry to parliament
data.ts <- aggregate(
  data = data.candidate %>% 
    filter(
      byelection == 0, 
      result != 0
    ), 
  age ~ year, 
  FUN = mean
) %>% 
  left_join(
    count(data.candidate, year)
  ) 
data.firstwin.ts <-  aggregate(
  data = data.first.win %>% 
    filter(
      # only general elections
      byelection == 0
    ), 
  age ~ year, 
  FUN = mean
) %>% 
  left_join(
    count(data.first.win, year)
  ) %>% 
  merge(
    as.data.frame(table(data.winner$year)), 
    by.x = "year", 
    by.y = "Var1"
  ) %>% 
  mutate(
    prop = n / Freq
  ) %>% 
  select(
    -Freq
  )

# 2. Summary by professions previously held
data.first.run.open <- data.first.run %>% 
  filter(
    kobo == 1
  ) %>% 
  mutate(
    juku = ifelse(
      juku == 0, 
      0, 
      1
    )
  )
data.first.run.closed <- data.first.run %>% 
  filter(
    kobo == 0
  ) %>% 
  mutate(
    juku = ifelse(
      juku == 0, 
      0, 
      1
    )
  )
data.first.run.open$name_jp[
  is.na(data.first.run.open$ken)
]

# Open recruitment
# total
length(
  data.first.run.open$name_jp[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 1
  ]
)
length(
  data.first.run.open$name_jp[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 0
  ]
)
length(
  data.first.run.open$name_jp[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 1
  ]
)
length(
  data.first.run.open$name_jp[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 0
  ]
)

# local assembly
sum(  # male, dynasty
  data.first.run.open$assy[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.open$assy[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.open$assy[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.open$assy[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
# bureaucrat
sum(  # male, dynasty
  data.first.run.open$bcrat[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.open$bcrat[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.open$bcrat[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.open$bcrat[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
# secretary
sum(  # male, dynasty
  data.first.run.open$sec[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.open$sec[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.open$sec[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.open$sec[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
# Lawyer
sum(  # male, dynasty
  data.first.run.open$law[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.open$law[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.open$law[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.open$law[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
# juku
sum(  # male, dynasty
  data.first.run.open$juku[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.open$juku[
    data.first.run.open$female == 0 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.open$juku[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.open$juku[
    data.first.run.open$female == 1 &
      data.first.run.open$dynasty == 0
  ], 
  na.rm = TRUE
)

# Closed recruitment
# total
length(
  data.first.run.closed$name_jp[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 1
  ]
)
length(
  data.first.run.closed$name_jp[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 0
  ]
)
length(
  data.first.run.closed$name_jp[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 1
  ]
)
length(
  data.first.run.closed$name_jp[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 0
  ]
)

# local assembly
sum(  # male, dynasty
  data.first.run.closed$assy[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.closed$assy[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.closed$assy[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.closed$assy[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
# bureaucrat
sum(  # male, dynasty
  data.first.run.closed$bcrat[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.closed$bcrat[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.closed$bcrat[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.closed$bcrat[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
# secretary
sum(  # male, dynasty
  data.first.run.closed$sec[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.closed$sec[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.closed$sec[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.closed$sec[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
# Lawyer
sum(  # male, dynasty
  data.first.run.closed$law[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.closed$law[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.closed$law[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.closed$law[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
# juku
sum(  # male, dynasty
  data.first.run.closed$juku[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.run.closed$juku[
    data.first.run.closed$female == 0 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.run.closed$juku[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.run.closed$juku[
    data.first.run.closed$female == 1 &
      data.first.run.closed$dynasty == 0
  ], 
  na.rm = TRUE
)

sum(is.na(data.first.run$kobo))
sum(is.na(data.first.run$female))
summary(as.factor(data.first.run$kobo))
summary(as.factor(data.first.run$juku))
summary(as.factor(data.first.run$year))
summary(as.factor(data.first.run$year[
  data.first.run$kobo == 0
]))

# Calculate various summaries of data: first win
data.first.win.open <- data.first.win %>% 
  filter(
    kobo == 1
  ) %>% 
  mutate(
    juku = ifelse(
      juku == 0, 
      0, 
      1
    )
  )
data.first.win.closed <- data.first.win %>% 
  filter(
    kobo == 0
  ) %>% 
  mutate(
    juku = ifelse(
      juku == 0, 
      0, 
      1
    )
  )

# Open recruitment
# total
length(
  data.first.win.open$name_jp[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 1
  ]
)
length(
  data.first.win.open$name_jp[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 0
  ]
)
length(
  data.first.win.open$name_jp[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 1
  ]
)
length(
  data.first.win.open$name_jp[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 0
  ]
)

# local assembly
sum(  # male, dynasty
  data.first.win.open$assy[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.open$assy[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.open$assy[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.open$assy[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
# bureaucrat
sum(  # male, dynasty
  data.first.win.open$bcrat[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.open$bcrat[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.open$bcrat[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.open$bcrat[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
# secretary
sum(  # male, dynasty
  data.first.win.open$sec[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.open$sec[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.open$sec[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.open$sec[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
# Lawyer
sum(  # male, dynasty
  data.first.win.open$law[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.open$law[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.open$law[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.open$law[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
# juku
sum(  # male, dynasty
  data.first.win.open$juku[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.open$juku[
    data.first.win.open$female == 0 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.open$juku[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.open$juku[
    data.first.win.open$female == 1 &
      data.first.win.open$dynasty == 0
  ], 
  na.rm = TRUE
)

# Closed recruitment
# total
length(
  data.first.win.closed$name_jp[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 1
  ]
)
length(
  data.first.win.closed$name_jp[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 0
  ]
)
length(
  data.first.win.closed$name_jp[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 1
  ]
)
length(
  data.first.win.closed$name_jp[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 0
  ]
)

# local assembly
sum(  # male, dynasty
  data.first.win.closed$assy[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.closed$assy[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.closed$assy[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.closed$assy[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
# bureaucrat
sum(  # male, dynasty
  data.first.win.closed$bcrat[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.closed$bcrat[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.closed$bcrat[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.closed$bcrat[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
# secretary
sum(  # male, dynasty
  data.first.win.closed$sec[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.closed$sec[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.closed$sec[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.closed$sec[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
# Lawyer
sum(  # male, dynasty
  data.first.win.closed$law[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.closed$law[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.closed$law[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.closed$law[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
# juku
sum(  # male, dynasty
  data.first.win.closed$juku[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # male, non-dynasty
  data.first.win.closed$juku[
    data.first.win.closed$female == 0 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)
sum(  # female, dynasty
  data.first.win.closed$juku[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 1
  ], 
  na.rm = TRUE
)
sum(  # female, non-dynasty
  data.first.win.closed$juku[
    data.first.win.closed$female == 1 &
      data.first.win.closed$dynasty == 0
  ], 
  na.rm = TRUE
)

