# Read metadata annotations from an Excel template workbook.
read_metadata_excel <- function(path, sheet = 1)
{
  if(!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' is required to read Excel metadata templates.")
  
  metadata <- readxl::read_excel(path, sheet = sheet, col_types = "text", .name_repair = "minimal")
  required_columns <- c("level_1", "level_2", "level_3", "value")
  missing_columns <- setdiff(required_columns, colnames(metadata))
  if(length(missing_columns) > 0)
    stop("Excel metadata template is missing required column(s): ", paste(missing_columns, collapse = ", "))
  
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  metadata[] <- lapply(metadata, function(column)
  {
    column <- trimws(column)
    column[column == ""] <- NA_character_
    column
  })
  metadata <- metadata[!is.na(metadata[["level_1"]]) & !is.na(metadata[["value"]]), required_columns, drop = FALSE]
  
  info <- list(
    participantID = NULL,
    enrollment = list(),
    baseline = list(),
    intervention = NULL,
    outcome = list(),
    correlated = list(),
    unexpected = list()
  )
  
  for(i in seq_len(nrow(metadata)))
  {
    level_1 <- metadata[["level_1"]][i]
    level_2 <- metadata[["level_2"]][i]
    level_3 <- metadata[["level_3"]][i]
    value <- metadata[["value"]][i]
    
    if(level_1 == "participantID")
    {
      info[["participantID"]] <- value
    } else if(level_1 == "intervention") {
      info[["intervention"]] <- value
    } else if(level_1 == "enrollment") {
      if(is.na(level_2))
        stop("Row ", i + 1, " in Excel metadata template is missing level_2 for enrollment.")
      info[["enrollment"]][[level_2]] <- value
    } else if(level_1 == "baseline") {
      if(is.na(level_2))
        stop("Row ", i + 1, " in Excel metadata template is missing level_2 for baseline.")
      info[["baseline"]][[level_2]] <- c(info[["baseline"]][[level_2]], value)
    } else if(level_1 == "outcome") {
      if(is.na(level_2) || is.na(level_3))
        stop("Row ", i + 1, " in Excel metadata template must include level_2 and level_3 for outcomes.")
      if(is.null(info[["outcome"]][[level_2]]))
        info[["outcome"]][[level_2]] <- list()
      info[["outcome"]][[level_2]][[level_3]] <- c(info[["outcome"]][[level_2]][[level_3]], value)
    } else if(level_1 == "correlated") {
      if(is.na(level_2))
        stop("Row ", i + 1, " in Excel metadata template is missing level_2 for correlated variables.")
      info[["correlated"]][[level_2]] <- c(info[["correlated"]][[level_2]], value)
    } else if(level_1 == "unexpected") {
      if(is.na(level_2))
        stop("Row ", i + 1, " in Excel metadata template is missing level_2 for unexpected values.")
      if(level_2 == "days")
      {
        if(is.na(level_3))
          stop("Row ", i + 1, " in Excel metadata template is missing level_3 for unexpected days.")
        if(is.null(info[["unexpected"]][["days"]]))
          info[["unexpected"]][["days"]] <- list()
        if(level_3 == "locale")
        {
          info[["unexpected"]][["days"]][["locale"]] <- value
        } else if(level_3 == "names") {
          info[["unexpected"]][["days"]][["names"]] <- c(info[["unexpected"]][["days"]][["names"]], value)
        } else {
          stop("Row ", i + 1, " in Excel metadata template has unsupported unexpected/days level_3 value: ", level_3)
        }
      } else {
        info[["unexpected"]][[level_2]] <- c(info[["unexpected"]][[level_2]], value)
      }
    } else {
      stop("Row ", i + 1, " in Excel metadata template has unsupported level_1 value: ", level_1)
    }
  }
  
  .simplify_metadata_vectors(info)
}

# Read metadata annotations from an R script template.
read_metadata_r <- function(path, object_name = "dataset_info")
{
  environment <- new.env(parent = baseenv())
  sys.source(path, envir = environment)
  
  if(!exists(object_name, envir = environment, inherits = FALSE))
    stop("Object ", object_name, " was not found in ", path, ".")
  
  info <- get(object_name, envir = environment, inherits = FALSE)
  if(!is.list(info))
    stop("Object ", object_name, " in ", path, " must be a list.")
  
  info
}

.simplify_metadata_vectors <- function(x)
{
  if(!is.list(x))
    return(x)
  
  if(is.null(names(x)))
  {
    x <- unlist(lapply(x, .simplify_metadata_vectors), use.names = FALSE)
    if(length(x) == 1)
      return(x[[1]])
    return(x)
  }
  
  x <- lapply(x, .simplify_metadata_vectors)
  for(name in names(x))
  {
    if(is.list(x[[name]]) && is.null(names(x[[name]])))
    {
      vectorised <- unlist(x[[name]], use.names = FALSE)
      x[[name]] <- if(length(vectorised) == 1) vectorised[[1]] else vectorised
    }
  }
  x
}
