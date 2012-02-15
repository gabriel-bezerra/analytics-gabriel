
regionOf <- function(uf) {
    if (uf == "AC" | uf == "AP" | uf == "AM" | uf == "PA" | uf == "RO" | uf == "RR" | uf == "TO") {
        "Norte"
    } else if (uf == "DF" | uf == "GO" | uf == "MS" | uf == "MT") {
        "Centro-oeste"
    } else if (uf == "AL" | uf == "BA" | uf == "CE" | uf == "MA" | uf == "PB" | uf == "PE"
               | uf == "PI" | uf == "RN" | uf == "SE") {
        "Nordeste"
    } else if (uf == "ES" | uf == "MG" | uf == "RJ" | uf == "SP") {
        "Sudeste"
    } else if (uf == "PR" | uf == "RS" | uf == "SC") {
        "Sul"
    } else {
        ""
    }
}

# Tests

stopifnot(regionOf("AC") == "Norte")
stopifnot(regionOf("AL") == "Nordeste")
stopifnot(regionOf("AP") == "Norte")
stopifnot(regionOf("AM") == "Norte")
stopifnot(regionOf("BA") == "Nordeste")
stopifnot(regionOf("CE") == "Nordeste")
stopifnot(regionOf("DF") == "Centro-oeste")
stopifnot(regionOf("ES") == "Sudeste")
stopifnot(regionOf("GO") == "Centro-oeste")
stopifnot(regionOf("MA") == "Nordeste")
stopifnot(regionOf("MT") == "Centro-oeste")
stopifnot(regionOf("MS") == "Centro-oeste")
stopifnot(regionOf("MG") == "Sudeste")
stopifnot(regionOf("PA") == "Norte")
stopifnot(regionOf("PB") == "Nordeste")
stopifnot(regionOf("PR") == "Sul")
stopifnot(regionOf("PE") == "Nordeste")
stopifnot(regionOf("PI") == "Nordeste")
stopifnot(regionOf("RJ") == "Sudeste")
stopifnot(regionOf("RN") == "Nordeste")
stopifnot(regionOf("RS") == "Sul")
stopifnot(regionOf("RO") == "Norte")
stopifnot(regionOf("RR") == "Norte")
stopifnot(regionOf("SC") == "Sul")
stopifnot(regionOf("SP") == "Sudeste")
stopifnot(regionOf("SE") == "Nordeste")
stopifnot(regionOf("TO") == "Norte")

stopifnot(regionOf("") == "")

