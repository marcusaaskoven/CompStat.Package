for (s in 95:105) {
  for (k in 95:105) {
    for (r in 1:5) {
      expect_true(BS_option(s, k, 0.1/r, 1, 0.2, 'C')$delta<1)
      expect_true(BS_option(s, k, 0.1/r, 1, 0.2, 'C')$delta>0)
      expect_true(BS_option(s, k, 0.1/r, 1, 0.2, 'C')$gamma>0)
      expect_true(BS_option(s, k, 0.1/r, 1, 0.2, 'C')$gamma<1)
    }
  }
}

