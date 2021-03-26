test_that("message_verbose returns a message when asked", {
  expect_equal(
    capture.output(message_verbose(TRUE, "hi there"), type = "message"),
    "hi there"
  )
})

test_that("message_verbose does not return a message when asked", {
  l <- suppressMessages(
    capture.output(message_verbose(FALSE, "hi there"), type = "message")
  )
  expect_equal(l, character(0))
})
