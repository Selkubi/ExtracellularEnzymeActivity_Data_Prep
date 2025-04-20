create_test_data <- function() {
  list(
    S09 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      estimate = c(-0.2103, -0.4177, -0.2074),
      SE = c(0.0650, 0.0650, 0.0650),
      df = c(22, 22, 22),
      t.ratio = c(-3.233, -6.421, -3.188),
      p.value = c(0.0103, 0.0001, 0.0114),
      stringsAsFactors = FALSE
    ),
    S13 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      estimate = c(-0.1269, -0.0608, 0.0661),
      SE = c(0.0650, 0.0650, 0.0650),
      df = c(22, 22, 22),
      t.ratio = c(-1.951, -0.934, 1.017),
      p.value = c(0.1484, 0.6249, 0.5744),
      stringsAsFactors = FALSE
    ),
    S16 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      estimate = c(0.0258, 0.2164, 0.1906),
      SE = c(0.0650, 0.0650, 0.0650),
      df = c(22, 22, 22),
      t.ratio = c(0.397, 3.328, 2.931),
      p.value = c(0.9171, 0.0082, 0.0203),
      stringsAsFactors = FALSE
    ),
    S19 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      estimate = c(0.0566, 0.1935, 0.1369),
      SE = c(0.0751, 0.0751, 0.0751),
      df = c(22, 22, 22),
      t.ratio = c(0.754, 2.577, 1.823),
      p.value = c(0.7345, 0.0437, 0.1855),
      stringsAsFactors = FALSE
    )
  )
}



# Create sample data
create_sample_data <- function() {
  list(
    emmGrid1 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      p.value = c(0.0103, 0.0001, 0.0114)
    ),
    emmGrid2 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      p.value = c(0.1484, 0.6249, 0.5744)
    ),
    emmGrid3 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      p.value = c(0.9171, 0.0082, 0.0203)
    )
  )
}
