test_that( "Check symmetric output looks okay", {
  output = check_symmetric(direct_from)

  expect_equal(
    output$symm,
    TRUE
  )

  expect_equal(
    length(output$conn),
    0
  )

})
