context('lcMethod')
rngReset()
setClass('lcMethodTest', contains='lcMethod')

test_that('new clmethod', {
  m = lcMethod('lcMethodTest', `NA`=NULL, log=TRUE, int=3L, num=2.5, char='a',
                                        fac=factor('b', levels=c('a', 'b')),
                                        form=A~B,
                                        call=1 + 2 * 3,
                                        name=xvar)

  expect_equivalent(m[['NA']], NULL)
  expect_equivalent(m$`NA`, NULL)
  expect_equivalent(m$log, TRUE)
  expect_equivalent(m$int, 3)
  expect_equivalent(m$num, 2.5)

  expect_equivalent(m$char, 'a')
  expect_equivalent(m$fac, factor('b', levels=c('a', 'b')))
  expect_equivalent(m$form, A ~ B)
  expect_equivalent(m$call, 1 + 2 * 3)
  expect_equivalent(m[['name', eval=FALSE]], quote(xvar))
})


test_that('as.data.frame', {
  # lcMethod.call('lcMethodTest', call=call('lcMethod')) %>%
  #   as.data.frame %>%
  #   expect_length(0) %T>%
  #   {expect_equal(nrow(.), 0)}

  m = lcMethod.call('lcMethodTest', call=call('lcMethod',
                                null=NULL, log=TRUE, int=3L, num=2.5, char='a',
                                fac=factor('b', levels=c('a', 'b')),
                                form=A~B,
                                call=quote(1 + 2 * 3),
                                name=quote(xvar)))

  as.data.frame(m) %>%
    expect_length(length(m)) %>%
    expect_named(names(m)) %T>%
    {expect_equal(., data.frame(null=NA, log=TRUE, int=3L, num=2.5, char='a',
                                fac=factor('b', levels=c('a', 'b')),
                                form='A ~ B', call='1 + 2 * 3', name='xvar', stringsAsFactors=FALSE))}

  as.data.frame(m, eval=TRUE) %>%
    expect_length(length(m)) %>%
    expect_named(names(m)) %T>%
    {expect_equal(., data.frame(null=NA, log=TRUE, int=3L, num=2.5, char='a',
                                fac=factor('b', levels=c('a', 'b')),
                                form='A ~ B', call=7, name='xvar', stringsAsFactors=FALSE))}

  xvar = 2
  as.data.frame(m, eval=TRUE) %>%
    expect_length(length(m)) %>%
    expect_named(names(m)) %T>%
    {expect_equal(., data.frame(null=NA, log=TRUE, int=3L, num=2.5, char='a',
                                fac=factor('b', levels=c('a', 'b')),
                                form='A ~ B', call=7, name=2, stringsAsFactors=FALSE))}

  m2 = lcMethod.call('lcMethodTest', call=call('lcMethod', vec=LETTERS[1:2]))
  expect_output(print(m2))
})


test_that('creation', {
  xvar = 2
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, b='a', c=NULL, d=NA, e=quote(xvar)))

  expect_is(getCall(method), 'call')
  expect_equal(names(method), letters[1:5])
  expect_output(show(method))
  expect_output(print(method))
})

test_that('unevaluated creation', {
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', e=quote(xvar)))
  expect_error(method$e)
  expect_output(show(method))
  expect_output(print(method))
})

test_that('length', {
  lcMethod.call('lcMethodTest', call=call('lcMethod')) %>%
    expect_length(0)

  lcMethod.call('lcMethodTest', call=call('lcMethod', a=1)) %>%
    expect_length(1)

  lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, e=quote(xvar))) %>%
    expect_length(2)
})

test_that('argument retrieval', {
  xvar = 2
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, b='a', c=NULL, d=NA, e=quote(xvar)))

  expect_equal(method$a, 1)
  expect_equal(method$b, 'a')
  expect_null(method$c)
  expect_true(is.na(method$d))
  expect_equal(method$e, xvar)
  expect_error(method$missing)
  expect_equal(method[['a']], method$a)
  expect_error(method[['missing']])

  expect_is(method[['e', eval=FALSE]], 'name')
})

test_that('environment()', {
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', e=quote(xvar)))
  e = new.env()
  e$xvar = 3

  expect_error(method$e)
  environment(method) = e
  expect_equal(method$e, e$xvar)
})

test_that('local variables', {
  f = function() {
    xvar = 2
    lcMethod.call('lcMethodTest', call=call('lcMethod', e=quote(xvar)))
  }
  expect_error(f()$e) # value of xvar is not defined

  g = function() {
    xvar = 2
    m = lcMethod.call('lcMethodTest', call=call('lcMethod', e=quote(xvar)))
    xvar = 3
    m$e #should be 3
  }
  expect_equal(g(), 3)
})

test_that('variable from custom environment', {
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', e=quote(xvar)))
  expect_error(method$e)

  e = new.env()
  e$xvar = 2
  expect_error(method$e)
  expect_equal(method[['e', envir=e]], 2)
})

test_that('internal variable reference', {
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', iter=1e3, warmup = quote(floor(iter / 2))))
  expect_equal(method$warmup, floor(method$iter / 2))
})

test_that('formula', {
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', formula=A~B, formula.sigma=~C))
  expect_is(formula(method), 'formula')
  expect_error(formula(method, 'missing'))
  expect_equal(formula(method), A~B)
  expect_equal(formula(method, 'sigma'), ~C)
})

test_that('update', {
  xvar = 2
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, b='a', c=NULL, d=NA, e=xvar))

  expect_equal(update(method, a=2)$a, 2)
  expect_null(update(method, a=NULL)$a)
  expect_equal(update(method, c=2)$c, 2)

  # update with new arguments
  m2 = update(method, new=1)
  expect_named(m2, c('new', names(method)), ignore.order=TRUE)
  expect_equal(m2$new, 1)

  m3 = update(method, newf=A~B)
  expect_named(m3, c('newf', names(method)), ignore.order=TRUE)
  expect_equal(m3$newf, A~B)
})

test_that('update with eval', {
  xvar = 2
  m0 = lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, b='a', c=NULL, d=NA, e=xvar))
  m1 = update(m0, new=xvar, .eval=TRUE)
  expect_equal(m1$new, xvar)
})

test_that('update with formula eval', {
  xvar = ~ 1
  m0 = lcMethod('lcMethodTest', a=1, f = A ~ 0)
  m1 = update(m0, f=xvar, .eval=TRUE)
  expect_equal(m1$f, A ~ 1)
})

test_that('update formula', {
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, f=A~1))
  update(method, f=.~B) %T>%
    {expect_equal(.$f, A~B)}
})

test_that('update.lcMethod with local variables', {
  xvar = 2
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', e=quote(xvar)))
  u = update(method, e=xvar)
  xvar = 3
  expect_equal(u$e, 3)
})

test_that('dependency function evaluation', {
  method = lcMethodTestKML()
  expect_is(method$centerMethod, 'function')
})

test_that('as.list', {
  xvar = 2
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, b='a', c=NULL, d=NA, e=quote(xvar)))
  xvar = 3
  expect_equal(as.list(method), list(a=1, b='a', c=NULL, d=NA, e=xvar))

  as.list(method, eval=FALSE) %>%
    expect_length(length(method))
})

test_that('as.list with function', {
  m = lcMethodTestKML()
  lis = as.list(m, args=kml::parALGO)
  expect_length(setdiff(names(lis), formalArgs(kml::parALGO)), 0)
})

test_that('as.list with two functions', {
  m = lcMethodTestKML()
  funs = c(kml::parALGO, kml::kml)
  lis = as.list(m, args=funs)
  expect_length(setdiff(names(lis), union(formalArgs(funs[[1]]), formalArgs(funs[[2]]))), 0)
})

test_that('substitute', {
  xvar = 2
  method = lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, b='a', c=NULL, d=NA, e=quote(xvar)))
  method2 = evaluate.lcMethod(method)

  expect_equal(method2[['a', eval=FALSE]], 1)
  expect_null(method2[['c', eval=FALSE]])
  expect_equal(method2[['e', eval=FALSE]], 2)
})

test_that('.arg error', {
  expect_error(lcMethod.call('lcMethodTest', call=call('lcMethod', a=1, .b='a')))
})

test_that('negative nClusters error', {
  expect_error(lcMethod.call('lcMethodTest', call=call('lcMethod', nClusters=-1, .b='a')))
})

test_that('lcMethod function', {
  lcMethodTestKML2 = function(time='Traj', id='Traj', response = 'Value', nClusters=2) {
    lcMethod.call('lcMethodKML', call=match.call.defaults(),
             defaults=c(kml::kml, kml::parALGO),
             excludeArgs=c('object', 'nbClusters', 'parAlgo', 'toPlot', 'saveFreq'))
  }
  m = lcMethodTestKML2(nClusters=3)

  expect_true(all(formalArgs(lcMethodTestKML2) %in% names(m)))
})

test_that('lcMethod function with default NULL argument', {
  lcMethodTestKML2 = function(time=NULL, id='Traj', response = 'Value', nClusters=2) {
    lcMethod.call('lcMethodKML', call=match.call.defaults(),
             defaults=c(kml::kml, kml::parALGO),
             excludeArgs=c('object', 'nbClusters', 'parAlgo', 'toPlot', 'saveFreq'))
  }
  m = lcMethodTestKML2(nClusters=3)

  expect_true(all(formalArgs(lcMethodTestKML2) %in% names(m)))
})
