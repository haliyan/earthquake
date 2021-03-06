test_that("geom_timeline returns the correct object",{
  expect_s3_class(ggplot2::ggplot(data=
                                    eq_raw_cleaner(tail(eq,100)),
                                  ggplot2::aes(x=Date,
                                               xmin=as.Date("2022-01-14"),
                                               xmax=as.Date("2022-05-26"))) +
                    geom_timeline(size=5), "ggplot")
  expect_s3_class(ggplot2::ggplot(data=
                                    eq_raw_cleaner(tail(eq,100)),
                                  ggplot2::aes(x=Date,
                                               xmin=as.Date("2022-01-14"),
                                               xmax=as.Date("2022-05-26"),
                                               y=Country)) +
                    geom_timeline(size=5), "ggplot")
})

test_that("stat_timeline returns the correct object",{
  expect_s3_class(ggplot2::ggplot(data=
                                    eq_raw_cleaner(tail(eq,100)),
                                  ggplot2::aes(x=Date,
                                               xmin=as.Date("2022-01-14"),
                                               xmax=as.Date("2022-05-26"))) +
                    stat_timeline(size=5), "ggplot")
  expect_s3_class(ggplot2::ggplot(data=
                                    eq_raw_cleaner(tail(eq,100)),
                                  ggplot2::aes(x=Date,
                                               xmin=as.Date("2022-01-14"),
                                               xmax=as.Date("2022-05-26"),
                                               y=Country)) +
                    stat_timeline(size=5), "ggplot")
})

test_that("geom_timeline_label returns the correct object",{
  expect_s3_class(ggplot2::ggplot(data=
                                    eq_raw_cleaner(tail(eq,100)),
                                  ggplot2::aes(x=Date,
                                               xmin=as.Date("2022-01-14"),
                                               xmax=as.Date("2022-05-26"),
                                               max_by=Mag,
                                               txt=Country,
                                               n_max=3)) +
                    geom_timeline_label(), "ggplot")
})

test_that("stat_timeline_label returns the correct object", {
  expect_s3_class(ggplot2::ggplot(data=
                                    eq_raw_cleaner(tail(eq,100)),
                                  ggplot2::aes(x=Date,
                                               xmin=as.Date("2022-01-14"),
                                               xmax=as.Date("2022-05-26"),
                                               max_by=Mag,
                                               txt=Country,
                                               n_max=3)) +
                    stat_timeline_label(), "ggplot")
})

test_that("auto_maxer throws correct error",{
  expect_error(auto_maxer(tail(eq,1)),
               "Please enter larger x range or specify n_max")
  expect_s3_class(auto_maxer(tail(eq,2)), "data.frame")
})

test_that("auto_maxer produces message correctly",{
  expect_message(auto_maxer(tail(eq,2)),
                 "n_max has been automatically set to 1")
  expect_message(auto_maxer(tail(eq,3)),
                 "n_max has been automatically set to 1")
})

test_that("clipper clips data frame correctly",{
 expect_equal(nrow(clipper(data.frame(x=c(4,7,12),
                                      xmin=c(6,6,6),
                                      xmax=c(15,15,15)))),2)
 expect_equal(clipper(data.frame(x=c(4,7,12),
                                   xmin=c(6,6,6),
                                   xmax=c(15,15,15)))$x, c(7,12))
})

test_that("clipper plus prepares data frame correctly",{
  expect_equal(nrow(clipper_plus(data.frame(x=c(4,7,12),
                                            xmin=c(6,6,6),
                                            xmax=c(15,15,15),
                                            txt=c("red","yellow","blue"),
                                            max_by=c(1,2,3)))),2)
  expect_false(clipper_plus(data.frame(x=c(4,7,12),
                                       xmin=c(6,6,6),
                                       xmax=c(15,15,15),
                                       txt=c("red","yellow","blue"),
                                       max_by=c(1,2,3)))$max_yn[1])
  expect_true(clipper_plus(data.frame(x=c(4,7,12),
                                      xmin=c(6,6,6),
                                      xmax=c(15,15,15),
                                      txt=c("red","yellow","blue"),
                                      max_by=c(1,2,3)))$max_yn[2])
})

test_that("timeline_draw produces an object of the correct class",{
  expect_s3_class(timeline_draw(data.frame(x=c(7,12),
                                           xmin=c(6,6),
                                           xmax=c(15,15),
                                           y=c(3,6),
                                           size=c(5,5),
                                           colour=c(1,1),
                                           fill=c(1,1),
                                           alpha=c(1,1))),"gTree")
})

test_that("timeline_label_draw produces an object of the correct class",{
  expect_s3_class(timeline_label_draw(data.frame(x=c(7,12),
                                           xmin=c(6,6),
                                           xmax=c(15,15),
                                           max_by=c(1,2),
                                           n_max=c(1,1),
                                           max_yn=c(FALSE,TRUE),
                                           txt=c("a","b"),
                                           size=c(5,5),
                                           colour=c(1,1),
                                           fill=c(1,1),
                                           alpha=c(1,1))),"gTree")
})
