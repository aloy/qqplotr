context('formula')

test_that('is.formula', {
  expect_true(is.formula(~1))
  expect_false(is.formula('~1'))
})

test_that('hasResponse', {
  expect_true(hasResponse(A~0))
  expect_true(hasResponse(I(log(A))~B))
  expect_false(hasResponse(~A))
  expect_false(hasResponse(~0))
})

test_that('hasIntercept', {
  expect_true(hasIntercept(~1))
  expect_true(hasIntercept(A~1))
  expect_false(hasIntercept(A~-1))
  expect_false(hasIntercept(~0))
  expect_false(hasIntercept(~1 - 1))
  expect_false(hasIntercept(~A - 1))
})

test_that('hasSingleResponse', {
  expect_true(hasSingleResponse(A~0))
  expect_true(hasSingleResponse(A~B))
  expect_false(hasSingleResponse(~1))
  expect_false(hasSingleResponse(~A))
  expect_false(hasSingleResponse(I(A+B)~0))
  expect_false(hasSingleResponse(A + B ~ C))
  expect_false(hasSingleResponse(cbind(A, B) ~ C))
})

test_that('hasResponse', {
  expect_null(getResponse(~1))
  expect_equal(getResponse(A~1), 'A')
  expect_equal(getResponse(A~B), 'A')
})

test_that('getREterms', {
  expect_null(getREterms(~0))
  expect_null(getREterms(A~B))
  expect_length(getREterms(A~B + (1 | C)), 1)
  expect_length(getREterms(A~B + (1 | C) + (1 | D)), 2)
  expect_is(getREterms(A~B + (1 | C)), 'list')
  expect_is(getREterms(A~B + (1 | C))[[1]], 'call')
  expect_equal(getREterms(A~(1 | C))[[1]], quote(1 | C))
})

test_that('hasRE', {
  expect_false(hasRE(A ~ 0))
  expect_false(hasRE(A ~ 1))
  expect_false(hasRE(A ~ B))
  expect_true(hasRE(A ~ (1 | B)))
  expect_true(hasRE(A ~ B + (1 | C)))
})

test_that('REtermAsFormula', {
  expect_equal(getREterms(A~B + (1 | C))[[1]] %>% REtermAsFormula, ~1)
  expect_equal(getREterms(A~B + (D | C))[[1]] %>% REtermAsFormula, ~D)
  expect_equal(getREterms(A~B + (-1 + D | C))[[1]] %>% REtermAsFormula, ~-1 + D)
})

test_that('getREGroupName', {
  expect_equal(getREterms(A~B + (1 | C))[[1]] %>% getREGroupName, 'C')
  expect_equal(getREterms(A~B + (1 | C) + (1 | Group))[[2]] %>% getREGroupName, 'Group')
})

test_that('hasCovariates', {
  expect_true(hasCovariates(A~B))
  expect_true(hasCovariates(A~B+C))
  expect_true(hasCovariates(A~poly(A, 2)))
  expect_false(hasCovariates(A~0))
  expect_false(hasCovariates(A~1))
  expect_false(hasCovariates(A + B ~ 0))
})

test_that('getCovariates', {
  expect_is(getCovariates(A~B), 'character')
  expect_length(getCovariates(A~0), 0)
  expect_length(getCovariates(A~1), 0)
  expect_equal(getCovariates(A~B), 'B')
  expect_equal(getCovariates(A~B + C), c('B', 'C'))
  expect_equal(getCovariates(A~B + poly(C, 2)), c('B', 'C'))
})

test_that('addInteraction', {
  expect_equal(addInteraction(~0, 'A'), ~0)
  expect_equal(addInteraction(~1, 'A'), ~A)
  expect_equal(addInteraction(~1 + A, 'A'), ~A * A)
  expect_equal(addInteraction(~1 + B, 'A'), ~B * A)
  expect_equal(addInteraction(D ~1 + B, 'A'), D ~ B * A)
})

test_that('merge.formula', {
  expect_equal(merge.formula(~A, ~B), ~A + B)
  expect_equal(merge.formula(Z~A, ~B), Z~A + B)
  expect_equal(merge.formula(Z~A + B, ~B), Z~A + B)
  expect_equal(merge.formula(Z~A + C, ~B), Z~A + C + B)
  expect_error(merge.formula(~A, Z~B))
})

test_that('dropResponse', {
  expect_equal(dropResponse(~0), ~0)
  expect_equal(dropResponse(~1), ~1)
  expect_equal(dropResponse(~A), ~A)
  expect_false(hasResponse(dropResponse(A~0)))
  expect_false(hasIntercept(dropResponse(A~0)))
  expect_equal(dropResponse(A~1), ~1)
  expect_equal(dropResponse(A~B), ~B)
  expect_equal(dropResponse(A + B ~C), ~C)
})

test_that('dropIntercept', {
  expect_false(hasIntercept(dropIntercept(~0)))
  expect_false(hasIntercept(dropIntercept(A~0)))
  expect_false(hasIntercept(dropIntercept(A~B)))
  expect_false(hasIntercept(dropIntercept(~B)))
  expect_equal(dropIntercept(A~B), A ~ B -1)
})

test_that('dropRE', {
  expect_equal(dropRE(~0), ~0)
  expect_equal(dropRE(~1), ~1)
  expect_equal(dropRE(A ~ 0), A ~ 0)
  expect_equal(dropRE(A ~ B), A ~ B)
  expect_equal(dropRE(A ~ (1 | B)), A ~ 1)
  expect_equal(dropRE(A ~ -1 + (1 | B)), A ~ 1 - 1)
  expect_equal(dropRE(A ~ B + (1 | C)), A ~ B)
  expect_equal(dropRE(A ~ B + (1 | C) + (D | E)), A ~ B)
})

test_that('getSpecialTerms', {
  f = Value ~ 0 + time(1) + time(B + C) + D + time + time(I(A^2)) + I(time(Z))
  expect_equal(getSpecialTerms(A~0, special='time'), character())
  expect_equal(getSpecialTerms(A~1, special='time'), character())
  expect_equal(getSpecialTerms(A~B, special='time'), character())
  expect_equal(getSpecialTerms(A~time, special='time'), character())
  expect_equal(getSpecialTerms(A~time(1), special='time'), '1')
  expect_equal(getSpecialTerms(A~time(0), special='time'), '0')
  expect_equal(getSpecialTerms(A~time(-1), special='time'), '-1')
  expect_equal(getSpecialTerms(A~I(time(B)), special='time'), character())
  expect_equal(getSpecialTerms(A~B + time(C), special='time'), 'C')
  expect_equal(getSpecialTerms(~B + time(C), special='time'), 'C')
  expect_equal(getSpecialTerms(A~B + time(C) + time(C + D), special='time'), c('C', 'C + D'))
  expect_equal(getSpecialTerms(A~B + time(I(B^2)), special='time'), 'I(B^2)')
  expect_equal(getSpecialTerms(f, special='time'), c('1', 'B + C', 'I(A^2)'))
})

test_that('getSpecialFormula', {
  f = Value ~ 0 + time(1) + time(B + C) + D + time + time(I(A^2)) + I(time(Z))
  expect_equal(getSpecialFormula(A~0, special='time'), A~1)
  expect_equal(getSpecialFormula(A~1, special='time'), A~1)
  expect_equal(getSpecialFormula(A~B, special='time'), A~1)
  expect_equal(getSpecialFormula(A~B + time(0), special='time'), A~0)
  expect_equal(getSpecialFormula(A~B + time(1), special='time'), A~1)
  expect_equal(getSpecialFormula(A~B + time(B), special='time'), A~B)
  expect_equal(getSpecialFormula(A~B + time(C), special='time'), A~C)
  expect_equal(getSpecialFormula(~B + time(C), special='time'), ~C)
  expect_equal(getSpecialFormula(A~B + time(0) + time(B), special='time'), A~0 + B)
  expect_equal(getSpecialFormula(A~B + time(I(A^2)), special='time'), A~I(A^2))
})

test_that('dropSpecial', {
  f = Value ~ 0 + time(1) + time(B + C) + D + time + time(I(A^2)) + I(time(Z))
  expect_equal(dropSpecial(A~B, special='time'), A~B)
  expect_equal(dropSpecial(A~1, special='time'), A~1)
  expect_equal(dropSpecial(A~0, special='time'), A~0)
  expect_equal(dropSpecial(A~B - 1, special='time'), A~B - 1)
  expect_equal(dropSpecial(A~B + time, special='time'), A~B + time)
  expect_equal(dropSpecial(A~B + time(C), special='time'), A~B)
  expect_equal(dropSpecial(A~B + I(time(C)), special='time'), A~B + I(time(C)))
  expect_equal(dropSpecial(~B + I(time(C)), special='time'), ~B + I(time(C)))
})
