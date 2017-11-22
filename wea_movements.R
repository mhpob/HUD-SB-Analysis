library(TelemetryR)

detects <- readRDS('hud_detects.RDS')


spl.dets <- split(detects, detects$Transmitter)

gen.tracks <- lapply(spl.dets, track, dates = 'date.local', ids = 'station')

oc <- '[IAO][NMS]\\d|[TA]-|CS'

wea.tracks <- lapply(gen.tracks, function(x){x[c(min(grep(oc, x$station)) - 1,
                                                 grep(oc, x$station)),]})

# Drop transmitters with no detections in WEA
wea.tracks <- wea.tracks[lapply(lapply(wea.tracks, dim), '[', 1) > 1]


wea.tracks[[40]]
