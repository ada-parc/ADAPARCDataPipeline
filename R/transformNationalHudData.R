#' transformNationalHudData
#'
#' @param hud_data raw hud data
#'
#' @return transformed hud data
#'
#' @import dplyr
#' @import tidyr
transformNationalHudData <- function(hud_data) {

  # TODO: Update states to have their abbreviation and name as separate columns

  hud_data_transformed <- hud_data %>%
    dplyr::filter(program_label %in% c("Housing Choice Vouchers", "Public Housing") &
                  !States %in% c("GU Guam", "MP Northern Mariana Islands", "VI U.S. Virgin Islands")) %>%
    dplyr::mutate(total_pct_under62 = rowSums(select(., c(pct_lt24_head, pct_age25_50, pct_age51_61)), na.rm = TRUE),
                 total_pct_disabled = ((
                   ifelse(is.na(pct_disabled_lt62), 0, pct_disabled_lt62) *
                     total_pct_under62 +
                     ifelse(is.na(pct_disabled_ge62), 0, pct_disabled_ge62 * pct_age62plus)
                 ) %>% replace_na(0)),
                 NAME = sub("^[A-Z]{2} |^[A-Z]{2}$", "", States) # This allows for the name of the state to be extracted; will be used to join to other data later on
    ) %>%
    dplyr::select(NAME, program_label, total_pct_disabled) %>%
    dplyr::group_by(NAME, program_label) %>%
    dplyr::summarise(total_pct_disabled = (sum(ifelse(is.na(total_pct_disabled), 0, total_pct_disabled), na.rm = TRUE)) * 100) %>%
    tidyr::pivot_wider(names_from = program_label, values_from = total_pct_disabled) %>%
    dplyr::rename(pwd_housing_choice_voucher_pct = `Housing Choice Vouchers`, pwd_pubhousing_pct = `Public Housing`) %>%
    dplyr::ungroup()

  return(hud_data_transformed)
}
