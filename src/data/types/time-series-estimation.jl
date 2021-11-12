
# -------------------------------------------------------------
# TimeSeries - estimation

function nointerpolation(ts::TimeSeries, t::AbstractFloat)
    return missing
end
function noextrapolation(ts::TimeSeries, t::AbstractFloat)
    throw(BoundsError(time(ts), t))
end
