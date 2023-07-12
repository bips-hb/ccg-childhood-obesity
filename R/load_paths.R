### Path IDEFICS & I.Family ----------------------------------------------------
load_IDEFICS <- function(pfad, folder, datensaetze, version, times, nott0t1)
{
  pfad <- c(rep(pfad[1], times[1]),
            rep(pfad[2], times[2]),
            rep(pfad[3], times[3]))

  idefics <- vector(length(datensaetze), mode = "list")
  namen <- datensaetze
  namen[1:times[1]] <- paste0(namen[1:times[1]], "_t0")
  namen[(times[1] + 1): (times[1] + times[2])] <-
    paste0(namen[(times[1] + 1): (times[1] + times[2])], "_t1")
  names(idefics) <- namen

  for(i in 1:length(idefics)){
    ifelse(i < (length(idefics) - nott0t1 + 1),
        daten <- haven::read_sas(data_file =
                             file.path(pfad[i], folder[i],
                             version[i],"SAS", paste0(datensaetze[i],".sas7bdat"))),
        daten <- haven::read_sas(data_file =
                             file.path(pfad[i], folder[i],
                                  paste0(datensaetze[i],".sas7bdat"))))
    names(daten) <- tolower(names(daten))
    idefics[[i]] <- daten
  }

  return(idefics)
}


