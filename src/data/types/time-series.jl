
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
    error("`series` accessor not implemented for type " * string(typeof(ts)))
end

# -------------------------------------------------------------
# TimeSeries - iterable interface

getindex(ts::TimeSeries, i::Integer) = series(ts)[i]
getindex(ts::TimeSeries, indices::AbstractVector{<:Integer}) = series(ts)[indices]
function getindex(ts::TimeSeries, t::AbstractFloat)
    # TODO: interpolate using nearest neighbours
    time(ts)[t]
end
function getindex(ts::TimeSeries, times::AbstractVector{<:AbstractFloat})
    # TODO: interpolate using nearest neighbours
    time(ts)[times]
end

length(ts::TimeSeries) = length(series(ts))
ndims(ts::TimeSeries) = 1
isempty(ts::TimeSeries) = length(ts) == 0
firstindex(ts::TimeSeries) = 1
lastindex(ts::TimeSeries) = length(ts)
eltype(::Type{TimeSeries}) = eltype(series(ts))

Base.@propagate_inbounds function iterate(ts::TimeSeries, i::Integer = 1)
    (i ≤ 0 || i > length(ts)) && return nothing
    (@inbounds series(ts)[i], i+1)
end

# -------------------------------------------------------------
# PointTimeSeries

"""
TODO: docs
"""
struct PointTimeSeries <: TimeSeries
    series::AbstractVector{<:Number}
end

TimeSeries(values::AbstractVector{<:Number}) = PointTimeSeries(values)

# -------------------------------------------------------------
# ConstantRateTimeSeries

"""
TODO: docs
"""
struct ConstantRateTimeSeries <: TimeSeries
    series::AbstractVector{<:Number}
    sr::Real

    function ConstantRateTimeSeries(values::AbstractVector{<:Number}, samplerate::Real = 1)
        return samplerate == 1 ? TimeSeries(values) : new(values, samplerate)
    end
end

function TimeSeries(values::AbstractVector{<:Number}, samplerate::Real)
    return ConstantRateTimeSeries(values, samplerate)
end

# -------------------------------------------------------------
# TimeExplicitTimeSeries

"""
TODO: docs
"""
struct TimeExplicitTimeSeries <: TimeSeries
    series::AbstractVector{<:Number}
    time::AbstractVector{<:AbstractFloat}

    function TimeExplicitTimeSeries(
        values::AbstractVector{<:Number},
        time::AbstractVector{<:Number} = collect(1:length(values))
    )
        @assert length(values) == length(time) "`values` and `time` mismtaches in length"

        if collect(time) == collect(1:length(values))
            return TimeSeries(values)
        elseif _has_approx_constant_increase(time)
            return ConstantRateTimeSeries(values, _approx_samplerate(time))
        else
            return new(values, time)
        end
    end
end

function TimeSeries(
        values::AbstractVector{<:Number},
        time::AbstractVector{<:Number}
    )
    return TimeExplicitTimeSeries(values, time)
end

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

istimeseries(x::Any) = false
istimeseries(x::TimeSeries) = true
istimeseries(x::Tuple{<:AbstractVector{<:Number},<:Real}) = true
istimeseries(x::Tuple{<:Real,<:AbstractVector{<:Number}}) = true
