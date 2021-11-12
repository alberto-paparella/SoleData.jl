
# -------------------------------------------------------------
# Abstract types

"""
TODO: docs
"""
abstract type TimeSeries <: TemporalEntity end

# -------------------------------------------------------------
# TimeSeries - accessors

function series(ts::TimeSeries)
    error("`series` accessor not implemented for type " * string(typeof(ts)))
end
function time(ts::TimeSeries)
    error("`time` accessor not implemented for type " * string(typeof(ts)))
end
function interpolation_method(ts::TimeSeries)
    error("`interpolation_method` accessor not implemented for type " * string(typeof(ts)))
end
function extrapolation_method(ts::TimeSeries)
    error("`extrapolation_method` accessor not implemented for type " * string(typeof(ts)))
end
function estimate(ts::TimeSeries, t::AbstractFloat)
    if time(ts)[1] ≤ t ≤ time(ts)[end]
        return interpolation_method(ts)(ts, t)
    else
        return extrapolation_method(ts)(ts, t)
    end
end

# -------------------------------------------------------------
# TimeSeries - iterable interface

getindex(ts::TimeSeries, i::Integer) = series(ts)[i]
getindex(ts::TimeSeries, indices::AbstractVector{<:Integer}) = series(ts)[indices]
function getindex(ts::TimeSeries, t::AbstractFloat)
    # TODO: maybe implement some sort of optimized binary search
    found = findall(x -> isapprox(x, t), time(ts))

    if length(found) == 0
        return estimate(ts, t)
    elseif length(found) == 1
        return ts[found[1]]
    else
        # TODO: what if multiple points are approx `t`?
        throw(ErrorException("still not implemented"))
    end
end
function getindex(ts::TimeSeries, times::AbstractVector{<:AbstractFloat})
    [ts[t] for t in times]
end

length(ts::TimeSeries) = length(series(ts))
ndims(ts::TimeSeries) = 1
isempty(ts::TimeSeries) = length(ts) == 0
firstindex(ts::TimeSeries) = 1
lastindex(ts::TimeSeries) = length(ts)
eltype(ts::TimeSeries) = eltype(series(ts))

Base.@propagate_inbounds function iterate(ts::TimeSeries, i::Integer = 1)
    (i ≤ 0 || i > length(ts)) && return nothing
    (@inbounds series(ts)[i], i+1)
end

show(ts::TimeSeries) = show(series(ts))

# -------------------------------------------------------------
# PointTimeSeries

"""
TODO: docs
"""
struct PointTimeSeries <: TimeSeries
    series::AbstractVector{<:Number}
    interpolation_method::Function
    extrapolation_method::Function
end

function TimeSeries(
    values::AbstractVector{<:Number};
    interpolation_method::Function = nointerpolation,
    extrapolation_method::Function = noextrapolation
)
    return PointTimeSeries(values, interpolation_method, extrapolation_method)
end

# -------------------------------------------------------------
# PointTimeSeries - accessors

series(ts::PointTimeSeries) = ts.series
time(ts::PointTimeSeries) = Float64.(collect(0:(length(series(ts))-1)))
interpolation_method(ts::PointTimeSeries) = ts.interpolation_method
extrapolation_method(ts::PointTimeSeries) = ts.extrapolation_method


# -------------------------------------------------------------
# ConstantRateTimeSeries

"""
TODO: docs
"""
struct ConstantRateTimeSeries <: TimeSeries
    series::AbstractVector{<:Number}
    samplerate::Real
    interpolation_method::Function
    extrapolation_method::Function

    function ConstantRateTimeSeries(
        values::AbstractVector{<:Number},
        samplerate::Real = 1,
        interpolation_method::Function = nointerpolation,
        extrapolation_method::Function = noextrapolation
    )
        if samplerate == 1
            return TimeSeries(
                values;
                interpolation_method = interpolation_method,
                extrapolation_method = extrapolation_method
            )
        else
            return new(values, samplerate, interpolation_method, extrapolation_method)
        end
    end
end

function TimeSeries(
    values::AbstractVector{<:Number},
    samplerate::Real;
    interpolation_method::Function = nointerpolation,
    extrapolation_method::Function = noextrapolation
)
    return ConstantRateTimeSeries(
        values,
        samplerate,
        interpolation_method,
        extrapolation_method
    )
end

# -------------------------------------------------------------
# ConstantRateTimeSeries - accessors

series(ts::ConstantRateTimeSeries) = ts.series
function time(ts::ConstantRateTimeSeries)
    step = 1.0 / ts.samplerate
    return Float64.(collect(0:step:((length(ts)-1) * step)))
end
interpolation_method(ts::ConstantRateTimeSeries) = ts.interpolation_method
extrapolation_method(ts::ConstantRateTimeSeries) = ts.extrapolation_method


# -------------------------------------------------------------
# TimeExplicitTimeSeries

"""
TODO: docs
"""
struct TimeExplicitTimeSeries <: TimeSeries
    series::AbstractVector{<:Number}
    time::AbstractVector{<:AbstractFloat}
    interpolation_method::Function
    extrapolation_method::Function

    function TimeExplicitTimeSeries(
        values::AbstractVector{<:Number},
        time::AbstractVector{<:AbstractFloat} = collect(1:length(values)),
        interpolation_method::Function = nointerpolation,
        extrapolation_method::Function = noextrapolation
    )
        @assert length(values) == length(time) "`values` and `time` mismtaches in length"
        # TODO: assert time is increasing
        # TODO: all time[1] has to be = 0.0

        if collect(time) == collect(1:length(values))
            return TimeSeries(
                values;
                interpolation_method = interpolation_method,
                extrapolation_method = extrapolation_method
            )
        elseif _has_approx_constant_increase(time)
            return ConstantRateTimeSeries(
                values,
                _approx_samplerate(time),
                interpolation_method,
                extrapolation_method
            )
        else
            return new(values, Float64.(time), interpolation_method, extrapolation_method)
        end
    end
end

function TimeSeries(
        values::AbstractVector{<:Number},
        time::AbstractVector{<:Number};
        interpolation_method::Function = nointerpolation,
        extrapolation_method::Function = noextrapolation
    )
    return TimeExplicitTimeSeries(values, time, interpolation_method, extrapolation_method)
end

# -------------------------------------------------------------
# TimeExplicitTimeSeries - accessors

series(ts::TimeExplicitTimeSeries) = ts.series
time(ts::TimeExplicitTimeSeries) = ts.time
interpolation_method(ts::TimeExplicitTimeSeries) = ts.interpolation_method
extrapolation_method(ts::TimeExplicitTimeSeries) = ts.extrapolation_method


# -------------------------------------------------------------
# TimeSeries - conversions

# vectors -> TimeSeries
convert(::Type{T}, x::T) where T<:TimeSeries = x
convert(::Type{T}, v::AbstractVector{<:Number}) where T<:TimeSeries = T(v)

# vectors + samplerate -> TimeSeries
function convert(::Type{ConstantRateTimeSeries}, t::Tuple{<:AbstractVector{<:Number},<:Real})
    return TimeSeries(t[1], t[2])
end
function convert(::Type{ConstantRateTimeSeries}, t::Tuple{<:Real,<:AbstractVector{<:Number}})
    return TimeSeries(t[2], t[1])
end

# vectors + explicit time coordinates -> TimeSeries
function convert(
    ::Type{TimeExplicitTimeSeries},
    t::Tuple{<:AbstractVector{<:Number},<:AbstractVector{<:Number}}
)
    # TODO: implement conversion to TimeExplicitTimeSeries
    throw(Exception("conversion to TimeExplicitTimeSeries still not implementd"))
end

istimeseries(ts::Any) = false
istimeseries(ts::TimeSeries) = true
istimeseries(ts::Tuple{<:AbstractVector{<:Number},<:Real}) = true
istimeseries(ts::Tuple{<:Real,<:AbstractVector{<:Number}}) = true
