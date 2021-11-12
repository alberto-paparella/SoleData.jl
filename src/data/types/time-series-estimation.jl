
# -------------------------------------------------------------
# TimeSeries - estimation

"""
    nointerpolation(ts, t)

This is the default method used to interpolate points in a `TimeSeries` when time `t` is not
part of the series `ts`.

It just returns `nothing`.
"""
function nointerpolation(ts::TimeSeries, t::AbstractFloat)
    return missing
end

"""
    noextrapolation(ts, t)

This is the default method used to extrapolate points in a `TimeSeries` when time `t` is not
part of the series `ts`.

It just returns `nothing`.
"""
function noextrapolation(ts::TimeSeries, t::AbstractFloat)
    throw(BoundsError(time(ts), t))
end
