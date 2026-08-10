## Strict JSON-row parsing for systemctl machine output, on janssonr.
##
## janssonr maps a JSON array to an unnamed list and a JSON object to a named
## list; it never simplifies an array-of-objects into a data.frame the way
## jsonlite did. So these helpers validate structure and per-field scalar types
## explicitly and fail closed -- they never rectangularize malformed input into
## something plausible. Errors carry class "runix_parse_error".

## Parse `txt` as a JSON array of objects and return the list of row objects.
## Fail-closed: unparseable input, a non-array top level, or a non-object row is
## a parse error.
.json_object_rows <- function(txt, label) {
    parsed <- tryCatch(janssonr::from_json(txt), error = function(e) {
        stop_rsystemd("unparseable ", label, " JSON: ", conditionMessage(e),
                      class = "runix_parse_error")
    })
    ## a JSON array is an unnamed list; a JSON object is a named list. systemctl
    ## machine output is an array of objects.
    if (!is.list(parsed) || !is.null(names(parsed))) {
        stop_rsystemd(label, " JSON is not an array of objects",
                      class = "runix_parse_error")
    }
    for (i in seq_along(parsed)) {
        nm <- names(parsed[[i]])
        if (!is.list(parsed[[i]]) || is.null(nm) || anyNA(nm) ||
            any(!nzchar(nm))) {
            stop_rsystemd(label, " row ", i, " is not a named object",
                          class = "runix_parse_error")
        }
    }
    parsed
}

## Extract one contracted scalar field from a parsed row (named list). Validates
## presence, that the value is a non-container scalar, and its type, before
## coercion. A JSON null becomes a typed NA only where the field is nullable; an
## array/object where a scalar is contracted is refused.
.json_field <- function(row, key, type, nullable, label) {
    na <- switch(type, character = NA_character_, number = NA_real_)
    if (!(key %in% names(row))) {
        stop_rsystemd(label, ": missing field '", key, "'",
                      class = "runix_parse_error")
    }
    v <- row[[key]]
    if (is.null(v)) {
        if (isTRUE(nullable)) {
            return(na)
        }
        stop_rsystemd(label, ": field '", key, "' is null",
                      class = "runix_parse_error")
    }
    ## janssonr: an array is an unnamed list, an object a named list -- either is
    ## a container where a scalar is contracted.
    if (is.list(v) || length(v) != 1L || is.na(v)) {
        stop_rsystemd(label, ": field '", key, "' is not a scalar ", type,
                      class = "runix_parse_error")
    }
    if (identical(type, "character")) {
        if (!is.character(v)) {
            stop_rsystemd(label, ": field '", key, "' is not a string",
                          class = "runix_parse_error")
        }
        return(v)
    }
    if (!is.numeric(v)) {
        stop_rsystemd(label, ": field '", key, "' is not a number",
                      class = "runix_parse_error")
    }
    as.numeric(v)
}

## Build a named list of column vectors from `txt`, one per field spec
## (list(key=, type = "character"|"number", nullable=)). Row count is the array
## length; an empty array yields correctly-typed zero-length columns.
.json_columns <- function(txt, fields, label) {
    rows <- .json_object_rows(txt, label)
    out <- vector("list", length(fields))
    names(out) <- vapply(fields, function(f) f$key, character(1))
    for (j in seq_along(fields)) {
        f <- fields[[j]]
        tmpl <- switch(f$type, character = character(1), number = numeric(1))
        out[[j]] <- vapply(rows, .json_field, tmpl, key = f$key,
                           type = f$type, nullable = isTRUE(f$nullable),
                           label = label)
    }
    out
}

## Decode one journal field value (janssonr) to a character scalar. journalctl
## emits a non-UTF-8 field as a JSON array of byte integers; janssonr gives an
## unnamed list of length-1 integers, so validate each element is a byte in
## 0:255 before assembling, rather than coercing junk. Missing/null -> NA.
.journal_chr <- function(row, key) {
    v <- row[[key]]
    if (is.null(v)) {
        return(NA_character_)
    }
    if (is.character(v) && length(v) == 1L && !is.na(v)) {
        return(v)
    }
    if (is.list(v)) {
        bytes <- vapply(v, function(b) {
            if (!(is.numeric(b) && length(b) == 1L && !is.na(b) &&
                        b >= 0 && b <= 255 && b == as.integer(b))) {
                stop_rsystemd("journal field '", key,
                              "' has a non-byte array element",
                              class = "runix_parse_error")
            }
            as.integer(b)
        }, integer(1))
        return(rawToChar(as.raw(bytes)))
    }
    NA_character_
}
