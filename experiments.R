evtype[grep("tornado",evtype)]
evtype[grep("THUNDERSTORM|TSTM",evtype)]
evtype[grep("astro",evtype)]




events <- list(
    AstronomicalLowTide=evtype[grep("astronomical",evtype)],
    Avalanche=evtype[grep("avalanche",evtype)],
    Blizzard=evtype[grep("blizzard",evtype)],
    CoastalFlood=evtype[grep("coastal flood",evtype)],
    ColdWindChill=evtype[grep("cold/wind chill",evtype)],
    DebrisFlow=evtype[grep("debris flow",evtype)],
    DenseFog=evtype[grep("dense fog",evtype)],
    DenseSmoke=evtype[grep("dense smoke",evtype)],
    Drought=evtype[grep("drought",evtype)],
    DustDevil=evtype[grep("dust devil",evtype)],
    DustStorm=evtype[grep("dust storm",evtype)],
    ExcessiveHeat=evtype[grep("excessive heat",evtype)],
    ExtremeColdWindChill=evtype[grep("extreme cold wind chill",evtype)],
    FlashFlood=evtype[grep("flash flood",evtype)],
    Flood=evtype[grep("flood",evtype)],
    FrostFreeze=evtype[grep("frost|freeze",evtype)],
    FunnelCloud=evtype[grep("funnel cloud",evtype)],
    FreezingFog=evtype[grep("freezing fog",evtype)],
    Hail=evtype[grep("hail",evtype)],
    Heat=evtype[grep("heat",evtype)],
    HeavyRain=evtype[grep("heavy rain",evtype)],
    HeavySnow=evtype[grep("heavy snow",evtype)],
    HighSurf=evtype[grep("high surf",evtype)],
    HighWind=evtype[grep("high wind",evtype)],
    HurricaneTyphoon=evtype[grep("hurricane|typhoon",evtype)],
    IceStorm=evtype[grep("ice storm",evtype)],
    LakeEffectSnow=evtype[grep("lake effect snow",evtype)],
    LakeshoreFlood=evtype[grep("lakeshore flood",evtype)],
    Lightning=evtype[grep("lightning",evtype)],
    MarineHail=evtype[grep("marine hail",evtype)],
    MarineHighWind=evtype[grep("marine high wind",evtype)],
    MarineStrongWind=evtype[grep("marine strong wind",evtype)],
    MarineThunderstormWind=evtype[grep("marine thunderstorm wind",evtype)],
    RipCurrent=evtype[grep("rip current",evtype)],
    Seiche=evtype[grep("seiche",evtype)],
    Sleet=evtype[grep("sleet",evtype)],
    StormSurgeTide=evtype[grep("storm surge/tide",evtype)],
    StrongWind=evtype[grep("strong wind",evtype)],
    ThunderstormWind=evtype[
       grep(
           "thunderstorm|tstm|thunderestorm|thunderstorm|thunerstorm|thuderstorm",
           evtype)],
    Tornado=evtype[grep("tornado|torndao",evtype)],
    TropicalDepression=evtype[grep("tropical depression",evtype)],
    TropicalStorm=evtype[grep("tropical storm",evtype)],
    Tsunami=evtype[grep("tsunami",evtype)],
    VolcanicAsh=evtype[grep("volcanic ash",evtype)],
    Waterspout=evtype[grep("waterspout",evtype)],
    Wildfire=evtype[grep("wildfire",evtype)],
    WinterStorm=evtype[grep("winter storm",evtype)],
    WinterWeather=evtype[grep("winter weather",evtype)],
    Summary=evtype[grep("summary",evtype)]
)
events
sum.events <- Reduce(c, events)
sum.events
evtype[!(evtype %in% sum.events)]
