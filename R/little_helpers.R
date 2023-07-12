### Mode of vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

### not in
`%nin%` <- function (x, table) is.na(match(x, table))



## 04: Create scores long.
carry_event_forward <- function(event_t0, event_t1, event_t3, data)
{
  v0 <- which(names(data) == event_t0)
  v1 <- which(names(data) == event_t1)
  v3 <- which(names(data) == event_t3)

  na1   <- which(is.na(data[, v1]) & data[, v0] == 1)
  na3_1 <- which(is.na(data[, v3]) & data[, v1] == 1)
  na3_2 <- which(is.na(data[, v3]) & is.na(data[, v1]) & data[, v0] == 1)

  if(length(na1) == 0 & length(na3_1) == 0 & length(na3_2) == 0){
    return(daten)
  }

  tmp <- daten %>% mutate(.,
                          !!event_t1 :=
                            if_else(.id %in% na1 & .imp > 0, data[, v0], data[, v1]),
                          !!event_t3 :=
                            if_else(.id %in% na3_1 & .imp > 0, data[, v1], data[, v3]))

  tmp <- tmp %>% mutate(.,
                        !!event_t3 :=
                          if_else(.id %in% na3_2 & .imp > 0, tmp[, v0], tmp[, v3]))
  tmp
}


# bnlear
matrix2df <- function(mat){
  diag(mat) <- 0
  tmp <- data.frame(from = rownames(mat)[row(mat)],
                    to = colnames(mat)[col(mat)],
                    values = c(mat))
  tmp[tmp$values == 1, -3]
}



# IDA Ergebnis in gt Table ändern
make_my_table <- function(local, optimal,
                          namen = names(daten.pa.mids$data),
                          local_as = local_adj_sets,
                          optimal_as = optimal_adj_sets,
                          labeldiscrete = NULL){

  requireNamespace("gt", quietly = TRUE)

  kurzeFunktion <- function(x) paste(x, collapse = ", ")

  out <- rbind(
    data.frame(local, type = "local",
               "adjset" = sapply(lapply(local_as, function(x) namen[x]), kurzeFunktion)),
    data.frame(optimal, type = "optimal",
               "adjset" = sapply(lapply(optimal_as, function(x) namen[x]), kurzeFunktion)))

  if(ncol(out) == 4){

  gt_tbl <- out %>% gt %>%
    fmt_number(
      columns = 1:2,
      decimals = 3
    ) %>%
    cols_label(
      type = md("**Type**"),
      est = md("**Est.**"),
      se = md("**S.E.**"),
      adjset = md("**Adjustment sets**")) %>%
    cols_align(
      align = "center", columns = c("est", "se", "type")) %>%
    tab_footnote(
      footnote = "all adjustment sets include additionally country, sex, age.0",
      locations = cells_column_labels(columns = vars(adjset))
    )
  gt_tbl

  } else {

    gt_tbl <- out %>% gt %>%
      fmt_number(
        columns = 1:4,
        decimals = 3
      ) %>%
      tab_spanner(
        label = labeldiscrete[1],
        columns = c("est", "se")
      ) %>%
      tab_spanner(
        label = labeldiscrete[2],
        columns = c("est.1", "se.1")
      ) %>%
      cols_align(
        align = "center", columns = c("est", "se", "est.1", "se.1", "type")) %>%
      cols_label(
        est = md("**Est.**"),
        se = md("**S.E.**"),
        est.1 = md("**Est.**"),
        se.1 = md("**S.E.**"),
        type = md("**Type**"),
        adjset = md("**Adjustment sets**")) %>%
      tab_footnote(
        footnote = "all adjustment sets include additionally country, sex, age.0",
        locations = cells_column_labels(columns = vars(adjset))
      )
    gt_tbl
  }
}

