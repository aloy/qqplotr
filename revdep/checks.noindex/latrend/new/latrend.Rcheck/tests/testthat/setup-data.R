set.seed(1)
testLongData = generateLongData(
  sizes = c(20, 30),
  fixed = Value ~ 1 + Assessment,
  cluster = ~ 1 + Assessment,
  random = ~ 1,
  id = 'Traj',
  data = data.frame(Assessment = seq(0, 1, by = .1)),
  fixedCoefs = c(0, 0),
  clusterCoefs = cbind(c(-2, 1), c(2, -1)),
  randomScales = cbind(.1, .1),
  noiseScales = c(.1, .1),
  clusterNames = c('A', 'B'),
  shuffle = TRUE) %>%
  .[, .(Traj, Assessment, Value, Class)]
