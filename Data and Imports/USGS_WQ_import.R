# Metadata located in usgs_wq_metadata.txt

# Poughkeepsie ----

urls <- paste0('https://waterdata.usgs.gov/ny/nwis/dv?cb_00010=on&cb_00095=on&cb_62619=on&cb_63680=on&cb_72137=on&format=rdb&site_no=01372058&referred_module=sw&period=&begin_date=',
               paste0(2016:2018, '-03-01'),
               '&end_date=',
               paste0(2016:2018, '-07-31'))

wq_list <- lapply(urls, read.delim, comment.char = "#", stringsAsFactors = F)
wq <- do.call(rbind, wq_list)

# remove checksum header and data-quality descriptor columns
wq <- wq[wq$agency_cd == 'USGS', !grepl('_cd', names(wq))]
# rename according to metadata

wq <- wq[wq$agency_cd == 'USGS', !grepl('_cd', names(wq))]
names(wq)[3:ncol(wq)] <- c(
  'mse_hl', 'mse_l', 'mse_h', 'mse_lh', 'mse',
  'mwt', 'max_wt',
  'min_spc', 'max_spc', 'mspc',
  'min_wt',
  'mdisch',
  'mturb'
)

wq <- type.convert(wq, na.strings = '', as.is = T)
wq$site_name <- 'HR_belowpoughkeepsie'

wq_master <- wq

# Albany ----
urls <- paste0('https://waterdata.usgs.gov/nwis/dv?cb_all_=on&cb_00010=on&cb_62619=on&format=rdb&site_no=01359139&referred_module=sw&period=&begin_date=',
               paste0(2016:2018, '-03-01'),
               '&end_date=',
               paste0(2016:2018, '-07-31'))

wq_list <- lapply(urls, read.delim, comment.char = "#", stringsAsFactors = F)
wq <- do.call(rbind, wq_list)

wq <- wq[wq$agency_cd == 'USGS', !grepl('_cd', names(wq))]
names(wq)[3:ncol(wq)] <- c(
  'max_wt', 'min_wt', 'mwt',
  'mse', 'mse_h', 'mse_lh', 'mse_hl', 'mse_l'
)

wq <- type.convert(wq, na.strings = '', as.is = T)
wq$site_name <- 'HR_albany'


wq_master <- merge(wq_master, wq, all = T)

saveRDS(wq_master, 'usgs_wq.rds')
