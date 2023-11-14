#' transformNationalHudData
#'
#' @param hud_data raw hud data
#'
#' @return transformed hud data
#'
#' @import dplyr
transformNationalHudData <- function(hud_data) {

  # TODO: Update states to have their abbreviation and name as separate columns

  hud_data_transformed <- hud_data %>%
    dplyr::mutate(total_pct_under62 = rowSums(select(., c(pct_lt24_head, pct_age25_50, pct_age51_61)), na.rm = TRUE),
                 total_pct_disabled = (
                   ifelse(is.na(pct_disabled_lt62), 0, pct_disabled_lt62) *
                     total_pct_under62 +
                     ifelse(is.na(pct_disabled_ge62), 0, pct_disabled_ge62 * pct_age62plus)
                 ) %>% replace_na(0)
    ) %>%
    dplyr::filter(program_label %in% c("Housing Choice Vouchers", "Public Housing")) %>%
    dplyr::select(States, program_label, total_pct_disabled) %>%
    dplyr::group_by(States, program_label) %>%
    dplyr::summarise(total_pct_disabled = sum(ifelse(is.na(total_pct_disabled), 0, total_pct_disabled), na.rm = TRUE)) %>%
    tidyr::pivot_wider(names_from = program_label, values_from = total_pct_disabled) %>%
    dplyr::rename(pwd_housing_choice_voucher_pct = `Housing Choice Vouchers`, pwd_pubhousing_pct = `Public Housing`) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!States %in% c("GU Guam", "MP Northern Mariana Islands", "VI U.S. Virgin Islands"))

  return(hud_data_transformed)
}
