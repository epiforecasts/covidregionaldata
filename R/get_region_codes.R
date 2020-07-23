# Mains -------------------------------------------------------------------------------------

#' Get a table of region codes for a specified country
#' @param country a string with a country specified
#' @return a tibble of regions and their corresponding region codes
#' @importFrom tibble tibble
get_region_codes <- function(country) {

  region_code_fun <- switch(country,
                         "afghanistan" = get_afghan_region_codes,
                         "belgium" = get_belgium_region_codes,
                         "brazil" = get_brazil_region_codes,
                         "canada" = get_canada_region_codes,
                         "colombia" = get_colombia_region_codes,
                         "germany" = get_germany_region_codes,
                         "india" = get_india_region_codes,
                         "italy" = get_italy_region_codes,
                         "russia" = get_russia_region_codes,
                         "uk" = get_uk_region_codes,
                         "usa" = get_us_region_codes)

  region_codes_table <- do.call(region_code_fun, list())

  return(region_codes_table)
}

#' Get a table of level 2 region codes (FIPS, ONS, region) for a specified country
#' @param country a string with a country specified
#' @return a tibble of regions and their corresponding level 2 region codes
#' @importFrom tibble tibble
get_level_2_region_codes <- function(country) {
  
  level_2_code_fun <- switch(country,
                             "belgium" = get_belgium_level_2_codes,
                             "brazil" = get_brazil_level_2_codes,
                             "germany" = get_germany_level_2_codes,
                             "uk" = get_uk_level_2_codes,
                             "usa" = get_us_level_2_codes)
  
  level_2_codes_table <- do.call(level_2_code_fun, list())
  
  return(level_2_codes_table)
}

# Level 1 regions -------------------------------------------------------------------------------------

#' Afghan region codes
#' @importFrom tibble tibble
#' 
get_afghan_region_codes <- function() {
  region_codes <- tibble::tibble(
    level_1_region_code = c("AF-BAL", "AF-BAM", "AF-BDG", "AF-BDS", "AF-BGL", "AF-DAY", "AF-FRA", "AF-FYB",
             "AF-GHA", "AF-GHO", "AF-HEL", "AF-HER", "AF-JOW", "AF-KAB", "AF-KAN", "AF-KAP",
             "AF-KDZ", "AF-KHO", "AF-KNR", "AF-LAG", "AF-LOG", "AF-NAN", "AF-NIM", "AF-NUR",
             "AF-PAN", "AF-PAR", "AF-PIA", "AF-PKA", "AF-SAM", "AF-SAR", "AF-TAK", "AF-URU",
             "AF-WAR", "AF-ZAB"),
    region = c("Balkh", "Badghis", "Baghlan", "Badakhshan", "Bamyan", "Daykundi", "Farah", "Faryab", "Ghazni", "Ghor", "Helmand", "Herat",
               "Jowzjan", "Kabul", "Kandahar", "Kapisa", "Kunduz", "Khost", "Kunar", "Laghman", "Logar", "Nangarhar", "Nimruz", "Nuristan",
               "Panjshir", "Parwan", "Paktia", "Paktika", "Samangan", "Sar-e Pol", "Takhar", "Urozgan", "Wardak", "Zabul"))
  return(region_codes)
}

#' Belgian region codes
#' @importFrom tibble tibble
#' 
get_belgium_region_codes <- function() {
  region_codes <- tibble::tibble(
    level_1_region_code = c("BE-BRU", "BE-VLG", "BE-WAL"),
    region = c("Brussels", "Flanders", "Wallonia"))
  return(region_codes)
}

#' Brazilian region codes
#' @importFrom tibble tibble
#' 
get_brazil_region_codes <- function() {
  region_codes <- tibble::tibble(
    level_1_region_code = c("BR-AC", "BR-AL", "BR-AM", "BR-AP", "BR-BA", "BR-CE", "BR-DF", "BR-ES", "BR-FN",
             "BR-GO", "BR-MA", "BR-MG", "BR-MS", "BR-MT", "BR-PA", "BR-PB", "BR-PE", "BR-PI",
             "BR-PR", "BR-RJ", "BR-RN", "BR-RO", "BR-RR", "BR-RS", "BR-SC", "BR-SE", "BR-SP",
             "BR-TO"),
    region = c("Acre", "Alagoas", "Amazonas", "Amap\u00E1", "Bahia", "Cear\u00E1", "Distrito Federal",
               "Espirito Santo", "Fernando de Noronha", "Goi\u00E1s", "Maranh\u00E3o", "Minas Gerais", "Mato Grosso do Sul", "Mato Grosso",
               "Par\u00E1", "Para\u00EDba", "Pernambuco", "Piau\u00ED", "Paran\u00E1", "Rio de Janeiro", "Rio Grande do Norte",
               "Rond\u00F4nia", "Roraima", "Rio Grande do Sul", "Santa Catarina", "Sergipe", "S\u00E3o Paulo", "Tocantins"))
  return(region_codes)
}

#' Canadian region codes
#' @importFrom tibble tibble
#' 
get_canada_region_codes <- function() {
  region_codes <- tibble::tibble(
    level_1_region_code = c("CA-AB", "CA-BC", "CA-MB", "CA-NB", "CA-NL", "CA-NS", "CA-NT", "CA-NU", "CA-ON", "CA-PE", "CA-QC", "CA-SK", "CA-YT"),
    region = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador",
               "Nova Scotia", "Northwest Territories", "Nunavut", "Ontario", "Prince Edward Island",
               "Quebec", "Saskatchewan", "Yukon"))
    return(region_codes)
}

#' German region codes
#' @importFrom tibble tibble
#' 
get_germany_region_codes <- function() {
  region_codes <- tibble::tibble(
    level_1_region_code = c("DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HB", "DE-HE", "DE-HH", "DE-MV",
             "DE-NI", "DE-NW", "DE-RP", "DE-SH", "DE-SL", "DE-SN", "DE-ST", "DE-TH"),
    region = c("Brandenburg", "Berlin", "Baden-W\u00FCrttemberg", "Bayern", "Bremen", "Hessen",
               "Hamburg", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen",
               "Rheinland-Pfalz", "Schleswig-Holstein", "Saarland", "Sachsen", "Sachsen-Anhalt", "Th\u00FCringen"))
  return(region_codes)
}

#' Indian region codes
#' @importFrom tibble tibble
#' 
get_india_region_codes <- function() {
  region_codes <- tibble::tibble(
    level_1_region_code = c("IN-AN", "IN-AP", "IN-AR", "IN-AS", "IN-BR", "IN-CH", "IN-CT", "IN-DD", "IN-DL",
             "IN-DN", "IN-GA", "IN-GJ", "IN-HP", "IN-HR", "IN-JH", "IN-JK", "IN-KA", "IN-KL",
             "IN-LA", "IN-LD", "IN-MH", "IN-ML", "IN-MN", "IN-MP", "IN-MZ", "IN-NL", "IN-OR",
             "IN-PB", "IN-PY", "IN-RJ", "IN-SK", "IN-TG", "IN-TN", "IN-TR", "IN-UP", "IN-UT", "IN-WB"),
    region = c("Andaman and Nicobar", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar",
               "Chandigarh", "Chhattisgarh", "Daman and Diu", "NCT of Delhi", "Dadra and Nagar Haveli",
               "Goa", "Gujarat", "Himachal Pradesh", "Haryana", "Jharkhand",
               "Jammu and Kashmir", "Karnataka", "Kerala", "Ladakh", "Lakshadweep", "Maharashtra",
               "Meghalaya", "Manipur", "Madhya Pradesh", "Mizoram", "Nagaland",
               "Odisha", "Punjab", "Puducherry", "Rajasthan", "Sikkim",
               "Telangana", "Tamil Nadu", "Tripura", "Uttar Pradesh", "Uttarakhand",
               "West Bengal"))
  return(region_codes)
}

#' Italian region codes
#' @importFrom tibble tibble
#' 
get_italy_region_codes <- function() {
  region_codes <- tibble::tibble(
    level_1_region_code = c("IT-21", "IT-23", "IT-25", "IT-32", "IT-34", "IT-36", "IT-42", "IT-45", "IT-52",
             "IT-55", "IT-57", "IT-62", "IT-65", "IT-67", "IT-72", "IT-75", "IT-77", "IT-78",
             "IT-82", "IT-88"),
    region = c("Piemonte", "Valle d'Aosta", "Lombardia", "Trentino-Alto Adige", "Veneto", "Friuli Venezia Giulia",
               "Liguria", "Emilia-Romagna", "Toscana", "Umbria", "Marche", "Lazio",
               "Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria",
               "Sicilia", "Sardegna"))
  return(region_codes)
}

#' Russian region codes
#' @importFrom tibble tibble
#' 
get_russia_region_codes <- function() {
  region_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:RU"
  region_table <- region_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
    rvest::html_table(fill=TRUE)
  region_code <- region_table[[1]][-1,]$Code
  
  region_codes <- tibble::tibble(
    level_1_region_code = c(region_code, "UA-40", "UA-43"), 
    region = c("Adygea Republic", "Altai Republic", "Bashkortostan Republic", "Buryatia Republic",
    "Chechen Republic", "Chuvashia Republic", "Dagestan Republic", "Ingushetia Republic", 
    "Kabardino-Balkarian Republic", "Kalmykia Republic", "Karachay-Cherkess Republic", "Karelia Republic",
    "Khakassia Republic", "Komi Republic", "Mari El Republic", "Mordovia Republic",
    "Sakha (Yakutiya) Republic", "North Ossetia - Alania Republic", "Tatarstan Republic",
    "Tyva Republic",  "Udmurt Republic",  "Altai Krai", "Kamchatka Krai",
    "Khabarovsk Krai",  "Krasnodar Krai", "Krasnoyarsk Krai", "Perm Krai", "Primorsky Krai", "Stavropol Krai",
    "Zabaykalsky Krai", "Amur Oblast", "Arkhangelsk Oblast", "Astrakhan Oblast", "Belgorod Oblast",
    "Bryansk Oblast", "Chelyabinsk Oblast",  "Irkutsk Oblast", "Ivanovo Oblast",
    "Kaliningrad Oblast", "Kaluga Oblast", "Kemerovo Oblast", "Kirov Oblast",
    "Kostroma Oblast", "Kurgan Oblast", "Kursk Oblast", "Leningrad Oblast",
    "Lipetsk Oblast", "Magadan Oblast", "Moscow Oblast", "Murmansk Oblast",
    "Nizhny Novgorod Oblast", "Novgorod Oblast", "Novosibirsk Oblast", "Omsk Oblast",
    "Orenburg Oblast",  "Orel Oblast",  "Penza Oblast", "Pskov Oblast",
    "Rostov Oblast", "Ryazan Oblast", "Sakhalin Oblast",  "Samara Oblast",
    "Saratov Oblast", "Smolensk Oblast", "Sverdlovsk Oblast",  "Tambov Oblast",
    "Tomsk Oblast", "Tula Oblast", "Tver Oblast",  "Tyumen Oblast",  "Ulyanovsk Oblast",
    "Vladimir Oblast", "Volgograd Oblast",  "Vologda Oblast",  "Voronezh Oblast",
    "Yaroslavl Oblast", "Moscow", "Saint Petersburg", "Jewish Autonomous Okrug", "Chukotka Autonomous Okrug",
    "Khanty-Mansi Autonomous Okrug", "Nenets Autonomous Okrug", "Yamalo-Nenets Autonomous Okrug", "Sevastopol",
    "Republic of Crimea"))
  
  return(region_codes)
}

#' US region codes
#' @importFrom tibble tibble
#' 
get_us_region_codes <- function() {
  region_codes <- tibble::tibble(
    level_1_region_code = c("US-AL", "US-AK", "US-AZ", "US-AR", "US-CA", "US-CO", "US-CT", "US-DE", "US-FL", "US-GA",
                 "US-HI", "US-ID", "US-IL", "US-IN", "US-IA", "US-KS", "US-KY", "US-LA", "US-ME", "US-MD",
                 "US-MA", "US-MI", "US-MN", "US-MS", "US-MO", "US-MT", "US-NE", "US-NV", "US-NH", "US-NJ",
                 "US-NM", "US-NY", "US-NC", "US-ND", "US-OH", "US-OK", "US-OR", "US-PA", "US-RI", "US-SC",
                 "US-SD", "US-TN", "US-TX", "US-UT", "US-VE", "US-VA", "US-WA", "US-WV", "US-WI", "US-WY",
                 "US-DC", "US-AS", "US-GU", "US-MP", "US-PR", "US-UM", "US-VI"),
    region = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware",
               "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
               "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
               "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
               "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
               "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
               "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia",
               "American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico", "Minor Outlying Islands",
               "Virgin Islands"))
  return(region_codes)
}

#' UK region codes (NULL - they're in the raw data already)
#' 
get_uk_region_codes <- function() {
  return(NULL)
}

#' Colombia region codes
#' 
get_colombia_region_codes <- function() {
  region_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:CO"
  region_table <- region_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
    rvest::html_table()
  region <- region_table[[1]] %>%
    dplyr::select(level_1_region_code = Code, region_level_1 = 2) %>%
    dplyr::mutate(region_level_1 = iconv(x = region_level_1, from = "UTF-8", to = "ASCII//TRANSLIT"),
                  region_level_1 = stringr::str_replace_all(region_level_1, "Distrito Capital de ", ""),
                  region_level_1 = stringr::str_to_sentence(region_level_1))
  return(region)
}

# Level 2 regions -------------------------------------------------------------------------------------

#' Belgian Provincial region codes
#' @importFrom tibble tibble
#' 
get_belgium_level_2_codes <- function() {
  region_codes <- tibble::tibble(
    level_2_region_code = c("BE-VAN", "BE-BRU", "BE-WLG", "BE-VLI", "BE-VOV", "BE-VBR",
                            "BE-VWV", "BE-WBR", "BE-WHT", "BE-WNA", NA, "BE-WLX"),
    region = c("Antwerpen", "Brussels", "Li\u00E8ge", "Limburg", "OostVlaanderen", "VlaamsBrabant", 
               "WestVlaanderen", "BrabantWallon", "Hainaut", "Namur", "Unknown", "Luxembourg"))
  return(region_codes)
}

#' Brazilian level 2 codes (not available currently)
#' @importFrom tibble tibble
#' 
get_brazil_level_2_codes <- function() {
  region_codes <- tibble::tibble(
    level_2_region_code = NA,
    region = NA)
  return(region_codes)
}

#' German level 2 codes (not available currently)
#' @importFrom tibble tibble
#' 
get_germany_level_2_codes <- function() {
  region_codes <- tibble::tibble(
    level_2_region_code = NA,
    region = NA)
  return(region_codes)
}

#' US level 2 codes (FIPS) (Included in original function)
#' @importFrom tibble tibble
#' 
get_us_level_2_codes <- function() {
  return(NULL)
}

#' UK level 2 codes (ONS) (Included in original function)
#' @importFrom tibble tibble
#' 
get_uk_level_2_codes <- function() {
  return(NULL)
}
