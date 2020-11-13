test_that("missing files are marked as missing, and existing files as confirmed", {


    # only missing
    fake_meta <- data.frame(path_x = c("./does/not/exist.R",
                                       "also/does/not/exist.R"),
                            path_y = c("./not/yet/here",
                                       NA),
                            stringsAsFactors = FALSE)
    fake_meta <- transform(fake_meta,
                           oid = seq_len(nrow(fake_meta)))

    checks <- check_meta_paths(fake_meta)

    compare_with <- structure(
        list(
            oid = 1:2,
            path_x = c("issue", "issue"),
            path_y = c("issue", NA)),
        class = "data.frame",
        row.names = c(NA, -2L))



  expect_identical(checks,compare_with)


  # one existing file
  fake_meta[1,1] <- tempfile("existing_file",fileext = ".R")
  file.create(fake_meta[1,1])

  checks <- check_meta_paths(fake_meta)

  compare_with <- structure(
      list(
          oid = 1:2,
          path_x = c("confirmed", "issue"),
          path_y = c("issue", NA)),
      class = "data.frame",
      row.names = c(NA, -2L))

  expect_identical(checks,compare_with)
  unlink(fake_meta[1,1])


})
