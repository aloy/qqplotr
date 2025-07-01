set.seed(1)

if (requireNamespace('ggplot2', quietly = TRUE)) {
  library(ggplot2)
}

testLongData = generateLongData(
  sizes = c(20L, 30L),
  fixed = Value ~ 1 + time,
  cluster = ~ 1 + time,
  random = ~ 1,
  id = 'id',
  data = data.frame(time = seq(0, 1, by = .1)),
  fixedCoefs = c(0, 0),
  clusterCoefs = cbind(c(-2, 1), c(2, -1)),
  randomScales = cbind(.1, .1),
  noiseScales = c(.1, .1),
  clusterNames = c('A', 'B'),
  shuffle = TRUE
) %>%
  .[, .(id, time, Value, Class)] %>%
  .[, Constant := 1] %>%
  .[, Cluster := Class] %>%
  .[, id := factor(id)] %>%
  .[]

# set up capture functions to test for wrong column name handling
response = function(...) stop('response column mishandling')
cluster = function(...) stop('cluster column mishandling')
id = function(...) stop('id column mishandling')

Time = function(...) stop('Time name evaluation')
Value = function(...) stop('Value name evaluation')
Assessment = function(...) stop('Assessment name evaluation')
Cluster = function(...) stop('Cluster name evaluation')
Traj = function(...) stop('Traj name evaluation')
