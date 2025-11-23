library(xml2)
library(dplyr)
library(stringr)
library(purrr)

# --------------------------------------------------
# 1. Read XML + strip namespaces
# --------------------------------------------------

xml_file <- "org_990.xml"  # change path/name if needed
doc <- read_xml(xml_file)

# Strip namespaces so we can use simple XPaths
xml_ns_strip(doc)

# --------------------------------------------------
# 2. Helper: safely get text from first existing node
# --------------------------------------------------

get_node_text <- function(doc, paths) {
  for (p in paths) {
    node <- xml_find_first(doc, p)
    if (!inherits(node, "xml_missing")) {
      txt <- xml_text(node)
      if (!is.na(txt) && nzchar(txt)) return(txt)
    }
  }
  return(NA_character_)
}

num_or_na <- function(x) suppressWarnings(as.numeric(x))

# --------------------------------------------------
# 3. Basic header info
# --------------------------------------------------

org_name <- get_node_text(doc, c(
  ".//ReturnHeader/Filer/BusinessName/BusinessNameLine1Txt",
  ".//ReturnHeader/Filer/BusinessName/BusinessNameLine1"
))

ein <- get_node_text(doc, c(
  ".//ReturnHeader/Filer/EIN",
  ".//ReturnHeader/Filer/EINTxt"
))

tax_yr <- get_node_text(doc, c(
  ".//ReturnHeader/TaxYr",
  ".//ReturnHeader/TaxYear"
))

# --------------------------------------------------
# 4. Core 990 node (IRS990, IRS990EZ, etc.)
# --------------------------------------------------

core_node  <- xml_find_first(doc, ".//ReturnData/*[1]")
core_name  <- xml_name(core_node)
core_xpath <- paste0(".//ReturnData/", core_name)

message("Core form node detected: ", core_name)

# --------------------------------------------------
# 5. Top-line revenue, expenses, net assets
# --------------------------------------------------

total_revenue <- num_or_na(get_node_text(doc, c(
  paste0(core_xpath, "/CYTotalRevenueAmt"),
  paste0(core_xpath, "/TotalRevenueCurrentYear"),
  paste0(core_xpath, "/TotalRevenueAmt")
)))

total_expenses <- num_or_na(get_node_text(doc, c(
  paste0(core_xpath, "/CYTotalExpensesAmt"),
  paste0(core_xpath, "/TotalFunctionalExpensesCurrentYear"),
  paste0(core_xpath, "/TotalExpensesAmt")
)))

net_assets_beg <- num_or_na(get_node_text(doc, c(
  paste0(core_xpath, "/NetAssetsOrFundBalancesBOYAmt"),
  paste0(core_xpath, "/NetAssetsOrFundBalancesBeginningYearAmt"),
  paste0(core_xpath, "/NetAssetsOrFundBalancesBOY")
)))

net_assets_end <- num_or_na(get_node_text(doc, c(
  paste0(core_xpath, "/NetAssetsOrFundBalancesEOYAmt"),
  paste0(core_xpath, "/NetAssetsOrFundBalancesEndYearAmt"),
  paste0(core_xpath, "/NetAssetsOrFundBalancesEOY")
)))

summary_df <- tibble(
  org_name,
  ein,
  tax_yr,
  total_revenue,
  total_expenses,
  surplus_deficit      = total_revenue - total_expenses,
  net_assets_beg,
  net_assets_end,
  change_in_net_assets = net_assets_end - net_assets_beg
)

print(summary_df)

# --------------------------------------------------
# 6. Functional expenses: program vs management vs fundraising
# --------------------------------------------------

program_exp <- num_or_na(get_node_text(doc, c(
  paste0(core_xpath, "/TotalProgramServiceExpensesAmt"),
  paste0(core_xpath, "/CYTotalProgramServiceExpensesAmt"),
  paste0(core_xpath, "//TotalProgramServiceExpensesAmt")
)))

mgmt_exp <- num_or_na(get_node_text(doc, c(
  paste0(core_xpath, "//TotalManagementAndGeneralExpensesAmt"),
  paste0(core_xpath, "//ManagementAndGeneralExpensesAmt"),
  paste0(core_xpath, "//ManagementAndGeneralAmt")
)))

fundraising_exp <- num_or_na(get_node_text(doc, c(
  paste0(core_xpath, "//TotalFundraisingExpensesAmt"),
  paste0(core_xpath, "//FundraisingExpensesAmt")
)))

overhead_exp <- mgmt_exp + fundraising_exp

expense_ratios <- tibble(
  program_exp,
  mgmt_exp,
  fundraising_exp,
  total_expenses,
  program_ratio  = ifelse(!is.na(total_expenses) & total_expenses > 0,
                          program_exp / total_expenses, NA_real_),
  overhead_ratio = ifelse(!is.na(total_expenses) & total_expenses > 0 &
                            !is.na(overhead_exp),
                          overhead_exp / total_expenses, NA_real_)
)

print(expense_ratios)

# --------------------------------------------------
# 7. Officers / key employees compensation (Part VII)
# --------------------------------------------------

# Most common modern schema: group entries
people_nodes <- xml_find_all(doc, paste0(core_xpath, "//Form990PartVIISectionAGrp"))

# Fallbacks
if (length(people_nodes) == 0) {
  people_nodes <- xml_find_all(doc, paste0(core_xpath, "//Form990PartVIISectionA"))
}
if (length(people_nodes) == 0) {
  people_nodes <- xml_find_all(doc, paste0(core_xpath, "//OffcrDirTrstKeyEmpl"))
}
if (length(people_nodes) == 0) {
  people_nodes <- xml_find_all(doc, paste0(core_xpath, "//OfficerDirectorTrusteeKeyEmployee"))
}

officers_df <- if (length(people_nodes) > 0) {
  map_df(people_nodes, function(n) {
    tibble(
      name = get_node_text(n, c(".//PersonNm", ".//NamePersonTxt")),
      title = get_node_text(n, c(".//TitleTxt")),
      avg_hours_per_week = num_or_na(get_node_text(n, c(".//AverageHoursPerWeekRt"))),
      reportable_comp_org = num_or_na(get_node_text(n, c(".//ReportableCompFromOrgAmt"))),
      reportable_comp_related = num_or_na(get_node_text(n, c(".//ReportableCompFromRltdOrgAmt"))),
      other_comp = num_or_na(get_node_text(n, c(".//OtherCompensationAmt")))
    )
  })
} else {
  tibble()
}

if (nrow(officers_df) > 0 && "reportable_comp_org" %in% names(officers_df)) {
  top_officers <- officers_df %>%
    filter(!is.na(reportable_comp_org)) %>%
    arrange(desc(reportable_comp_org)) %>%
    head(25)

  print(top_officers)
} else {
  message("No officer compensation records found (even after Group fallback).")
}
# 7. Officers / key employees compensation (Part VII)
# --------------------------------------------------

# Most common modern schema: group entries
people_nodes <- xml_find_all(doc, paste0(core_xpath, "//Form990PartVIISectionAGrp"))

# Fallbacks
if (length(people_nodes) == 0) {
  people_nodes <- xml_find_all(doc, paste0(core_xpath, "//Form990PartVIISectionA"))
}
if (length(people_nodes) == 0) {
  people_nodes <- xml_find_all(doc, paste0(core_xpath, "//OffcrDirTrstKeyEmpl"))
}
if (length(people_nodes) == 0) {
  people_nodes <- xml_find_all(doc, paste0(core_xpath, "//OfficerDirectorTrusteeKeyEmployee"))
}

officers_df <- if (length(people_nodes) > 0) {
  map_df(people_nodes, function(n) {
    tibble(
      name = get_node_text(n, c(".//PersonNm", ".//NamePersonTxt")),
      title = get_node_text(n, c(".//TitleTxt")),
      avg_hours_per_week = num_or_na(get_node_text(n, c(".//AverageHoursPerWeekRt"))),
      reportable_comp_org = num_or_na(get_node_text(n, c(".//ReportableCompFromOrgAmt"))),
      reportable_comp_related = num_or_na(get_node_text(n, c(".//ReportableCompFromRltdOrgAmt"))),
      other_comp = num_or_na(get_node_text(n, c(".//OtherCompensationAmt")))
    )
  })
} else {
  tibble()
}

# Print ALL officers, sorted by compensation
if (nrow(officers_df) > 0 && "reportable_comp_org" %in% names(officers_df)) {

  all_officers <- officers_df %>%
    filter(!is.na(reportable_comp_org)) %>%
    arrange(desc(reportable_comp_org))

  print(all_officers, n = nrow(all_officers))

} else {
  message("No officer compensation records found (even after Group fallback).")
}
# Export ALL officers to CSV
write.csv(all_officers,
          file = "depaul_990_officers.csv",
          row.names = FALSE)

message("Exported: depaul_990_officers.csv")

# --------------------------------------------------
# End
# --------------------------------------------------
