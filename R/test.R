context("Testing SW_test context")

## invalid inputs 
test_that("function SW_test gives helpful errors", 
          {
            expect_error(SW_test(data3), "Input data contains NA values.")
            
            expect_error(SW_test(data4), "Input data contains infinite values.")
            
            expect_error(SW_test(data5), "Input data must be numeric.")
            
            expect_error(SW_test(data6), "Input data must have at least 3 values.")
            
          })

## valid inputs

test_that("function SW_test works with numeric dataframes", {
  
  expect_silent(SW_test(data1))
  
  expect_silent(SW_test(milk$Cost))
  
})