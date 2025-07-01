context('lcMethod')
rngReset()
setClass('lcMethodTest', contains = 'lcMethod')

m = new(
  'lcMethodTest',
  null = NULL,
  vNA = NA,
  vNaN = NaN,
  logical = TRUE,
  int = -3L,
  num = -2.5,
  char = 'a',
  fac = factor('b', levels = c('a', 'b')),
  form = A~B,
  call = 1 + 2 * 3,
  name = xvar
)

# Core ####
test_that('name', {
  name = getName(m)
  expect_equal(name, 'undefined')
})

test_that('shortname', {
  name = getShortName(m)
  expect_equal(name, 'undefined')
})

test_that('length', {
  expect_length(new('lcMethodTest'), 0) #default time & id arguments
  expect_length(new('lcMethodTest', a = 1), 1)
  expect_length(new('lcMethodTest', a = 1, b = NULL), 2)
})


# Argument handling ####
test_that('method argument values', {
  expect_equivalent(m$null, NULL)
  expect_equivalent(m[['vNA']], NA)
  expect_equivalent(m$vNA, NA)
  expect_equivalent(m$vNaN, NaN)
  expect_equivalent(m$logical, TRUE)
  expect_equivalent(m$int, -3L)
  expect_equivalent(m$num, -2.5)
  expect_equivalent(m$char, 'a')
  expect_equivalent(m$fac, factor('b', levels = c('a', 'b')))
  expect_equivalent(m$form, A ~ B)
  expect_equivalent(m$call, 1 + 2 * 3)
  expect_equivalent(m[['name', eval = FALSE]], quote(xvar))
  expect_error(m$name, 'xvar')
  expect_error(m$missing)
  expect_error(m[['missing']])

  xvar = 2
  expect_equivalent(m$name, xvar)
})

test_that('take local over global scope', {
  m = new('lcMethodTest', a = 1, var = globalVar)
  assign('globalVar', value = 5, envir = .GlobalEnv)
  globalVar = 2

  expect_equal(m$var, 2)

  rm('globalVar', envir = .GlobalEnv)
})

test_that('argument value in global scope', {
  m = new('lcMethodTest', a = 1, var = globalVar)
  assign('globalVar', value = 7, envir = .GlobalEnv)
  expect_equal(m$var, 7)
  rm('globalVar', envir = .GlobalEnv)
})

test_that('unevaluated values', {
  expect_null(m[['null', eval = FALSE]])
  expect_true(is.na(m[['vNA', eval = FALSE]]))
  expect_true(is.nan(m[['vNaN', eval = FALSE]]))
  expect_true(isTRUE(m[['logical', eval = FALSE]]))
  expect_is(m[['int', eval = FALSE]], 'call')
  expect_is(m[['num', eval = FALSE]], 'call')
  expect_is(m[['char', eval = FALSE]], 'character')
  expect_is(m[['fac', eval = FALSE]], 'call')
  expect_is(m[['form', eval = FALSE]], 'call')
  expect_is(m[['form', eval = FALSE]], 'call')
  expect_equivalent(deparse(m[['call', eval = FALSE]]), '1 + 2 * 3')
  expect_is(m[['name', eval = FALSE]], 'name')
  expect_error(m[['missing', eval = FALSE]])
})

test_that('time function confusion', {
  m = new('lcMethodTest', time = 'time')
  expect_is(m[['time', eval = FALSE]], 'character')
  expect_is(m[['time', eval = TRUE]], 'character')
  expect_equal(m$time, 'time')
})

test_that('dependency function evaluation', {
  method = lcMethodTestLMKM(fun = mean)
  expect_is(method$fun, 'function')
})

test_that('local variables', {
  f = function() {
    xvar = 2
    new('lcMethodTest', name = xvar)
  }
  expect_error(f()$name) # value of xvar is not defined

  g = function() {
    xvar = 2
    method = new('lcMethodTest', name = xvar)
    xvar = 3
    method$name #should be 3
  }
  expect_equal(g(), 3)
})

test_that('internal variable reference', {
  method = new('lcMethodTest', iter = 1e3, warmup = floor(iter / 2))
  expect_equal(method$warmup, floor(method$iter / 2))
})

test_that('variable of argument name', {
  warmup = 3
  method = new('lcMethodTest', iter = 1e3, warmup = warmup)
  expect_equal(method$warmup, warmup)
})

test_that('formula', {
  method = new('lcMethodTest', formula = A ~ B, formula.sigma = ~C)
  expect_is(formula(method), 'formula')
  expect_error(formula(method, 'missing'))
  expect_equal(formula(method), A ~ B)
  expect_equal(formula(method, 'sigma'), ~ C)
})


# Update ####
test_that('empty update', {
  m2 = update(m)
  expect_equal(names(m2), names(m)) #ensure arg ordering
})

test_that('update add single argument', {
  srcNames = names(m)
  m2 = update(m, new = 1)
  expect_equal(names(m), srcNames) # ensure that m was not modified
  expect_equal(m2$new, 1)
  expect_equal(setdiff(names(m2), 'new'), names(m)) #ensure arg ordering

  expect_equal(update(m, a = 2)$a, 2)
  expect_null(update(m, a = NULL)$a)
  expect_equal(update(m, c = 2)$c, 2)

  m3 = update(m, newf = A ~ B)
  expect_named(m3, c('newf', names(m)), ignore.order = TRUE)
  expect_equal(m3$newf, A ~ B)
})

test_that('update with eval', {
  xvar = 2
  m0 = new('lcMethodTest', a = 1, b = 'a', c = NULL, d = NA, e = xvar)
  m1 = update(m0, new = xvar, .eval = TRUE)
  expect_equal(deparse(m0[['e', eval = FALSE]]), 'xvar')
  expect_equal(m1$new, xvar)
})

test_that('update with formula eval', {
  xvar = ~ 1
  m0 = new('lcMethodTest', a = 1, f = A ~ 0)
  m1 = update(m0, f = xvar, .eval = TRUE)
  expect_equal(m1$f, A ~ 1)
})

test_that('update formula', {
  method = new('lcMethodTest', a = 1, f = A ~ 1)
  expect_equal(update(method, f = . ~ B)$f, A ~ B)
})

test_that('update.lcMethod with local variables', {
  xvar = 2
  method = new('lcMethodTest', e = xvar)
  u = update(method, e = xvar)
  xvar = 3
  expect_equal(u$e, 3)
})


# Environments ####
test_that('environment()', {
  envir = new.env()
  envir$xvar = 3
  expect_error(m$name)
  environment(m) = envir
  expect_equal(m$name, envir$xvar)
})

test_that('variable from custom environment', {
  method = new('lcMethodTest', name = xvar)
  expect_error(method$name)

  envir = new.env()
  envir$xvar = 5
  expect_error(method$name)
  expect_equal(method[['name', envir = envir]], 5)
})


# Evaluate ####
test_that('evaluate', {
  expect_error(m$name)
  m2 = evaluate(m)
  expect_equal(names(m2), names(m))
  expect_error(m$name)
  envir = new.env()
  envir$xvar = 9
  m3 = evaluate(m, envir = envir)
  expect_equal(m3$name, 9)
  expect_error(m$name)
})

test_that('substitute', {
  xvar = 2
  method = new('lcMethodTest', a = 1, b = 'a', c = NULL, d = NA, e = xvar)
  method2 = evaluate.lcMethod(method)

  expect_equal(method2[['a', eval = FALSE]], 1)
  expect_null(method2[['c', eval = FALSE]])
  expect_equal(method2[['e', eval = FALSE]], 2)
})


# Validation ####
test_that('.arg error', {
  expect_error(new('lcMethodTest', a = 1, .b = 'a'), '\\.')
})


test_that('negative nClusters error', {
  expect_error(new('lcMethodTest', nClusters = -1))
})


# Output ####
test_that('print', {
  expect_output(print(m))
})

test_that('show', {
  expect_output(show(m))
})


# Data.frame ####
test_that('as.data.frame(eval=FALSE)', {
  refDf = data.frame(
    null = NA,
    vNA = NA,
    vNaN = NaN,
    logical = TRUE,
    int = '-3L',
    num = '-2.5',
    char = 'a',
    fac = 'factor(\"b\", levels = c(\"a\", \"b\"))',
    form = 'A ~ B',
    call = '1 + 2 * 3',
    name = 'xvar',
    stringsAsFactors = FALSE
  )

  df = as.data.frame(m, eval = FALSE)

  expect_length(df, length(m))
  expect_named(df, names(m))
  expect_equal(df, refDf)
})

test_that('as.data.frame(eval=TRUE)', {
  refDf = data.frame(
    null = NA,
    vNA = NA,
    vNaN = NaN,
    logical = TRUE,
    int = -3L,
    num = -2.5,
    char = 'a',
    fac = factor('b', levels = c('a', 'b')),
    form = 'A ~ B',
    call = 7,
    name = 'xvar',
    stringsAsFactors = FALSE
  )

  df = as.data.frame(m, eval = TRUE)

  expect_length(df, length(m))
  expect_named(df, names(m))
  expect_equal(df, refDf)
})

test_that('as.data.frame with symbols', {
  xvar = 2
  df = as.data.frame(m, eval = TRUE)

  expect_length(df, length(m))
  expect_named(df, names(m))
  expect_equal(df$name, xvar)
})

test_that('as.data.frame with vector arguments', {
  m = new(
    'lcMethodTest',
    null = NULL,
    vec = c('a', 'b'),
    ab = LETTERS[1:26]
  )

  df = as.data.frame(m, eval = FALSE)
  refDf = data.frame(
    null = NA,
    vec = 'c("a", "b")',
    ab = 'LETTERS[1:26]',
    stringsAsFactors = FALSE
  )
  expect_equal(df, refDf)

  df2 = as.data.frame(m, eval = TRUE)
  refDf2 = data.frame(
    null = NA,
    vec = 'c("a", "b")',
    ab = sprintf('c(%s)', paste0('"', LETTERS[1:26], '"', collapse = ', ')),
    stringsAsFactors = FALSE
  )
  expect_equal(df2, refDf2)
})


# List ####
test_that('as.list', {
  xvar = 2
  method = new('lcMethodTest', a = 1, b = 'a', c = NULL, d = NA, e = xvar)
  xvar = 3
  expect_equal(as.list(method), list(a = 1, b = 'a', c = NULL, d = NA, e = xvar))

  expect_length(as.list(method, eval = FALSE), length(method))
})

test_that('as.list with function', {
  skip_if_not_installed('kml')

  method = lcMethodTestKML()
  lis = as.list(method, args = kml::parALGO)
  expect_length(setdiff(names(lis), formalArgs(kml::parALGO)), 0)
})

test_that('as.list with two functions', {
  skip_if_not_installed('kml')

  method = lcMethodTestKML()
  funs = c(kml::parALGO, kml::kml)
  lis = as.list(method, args = funs)
  expect_length(setdiff(names(lis), union(formalArgs(funs[[1]]), formalArgs(funs[[2]]))), 0)
})
