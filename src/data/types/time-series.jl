
# -------------------------------------------------------------
# Abstract types

"""
Abstract supertype for all TemporalEntities that can be described as values changing over
time.

A concrete TimeSeries should always provide accessors [`series`](@ref), to
access the points of the series, [`time`](@ref), to access the times at which the points
are defined, [`interpolation_method`](@ref) which defines the way the points are interpolated
and [`extrapolation_method`](@ref) which defines the way the points are extrapolated.

A TimeSeries point can be accessed by its index, just like it it was a Vector of Numbers or
by using AbstractFloats indicating the time elapsed from the first point of the series.
"""
abstract type TimeSeries <: TemporalEntity end

# -------------------------------------------------------------
# TimeSeries - accessors

"""
    series(ts)

Get the point of the series as a Vector.
"""
function series(ts::TimeSeries)
    error("`series` accessor not implemented for type " * string(typeof(ts)))
end
"""
    time(ts)

Get the times of the series as a Vector.
"""
function time(ts::TimeSeries)
    error("`time` accessor not implemented for type " * string(typeof(ts)))
end
function interpolation_method(ts::TimeSeries)
    error("`interpolation_method` accessor not implemented for type " * string(typeof(ts)))
end
function extrapolation_method(ts::TimeSeries)
    error("`extrapolation_method` accessor not implemented for type " * string(typeof(ts)))
end

# -------------------------------------------------------------
# TimeSeries - iterable interface

"""
    getindex(timeseries, i)

Get the `i`-th point in TimeSeries `timeseries`.

    getindex(timeseries, time)

Get the point in TimeSeries `timeseries` at `time`.

If point at `time` is not defined then [`estimate`](@ref) is called.

# Examples
```jldoctest
julia> ts = TimeSeries([1,2,3,1,4], [0.1, 0.5, 0.7, 1.3, 1.9])
TimeSeries([(1, 0.0), (2, 0.4), (3, 0.6), (1, 1.2), (4, 1.8)])

julia> ts[1]
1

julia> ts[5]
4

julia> ts[0.4]
2

julia> ts[0.5]
missing

julia> ts[2.5]
ERROR: BoundsError: attempt to access 5-element Vector{Float64} at index [2.5]
Stacktrace: [... omitted ...]
```
"""
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

"""
    estimate(timeseries, time)

Estimate the value of the point at `time` in TimeSeries `timeseries`.

If `time` is inside the time range in which the TimeSeries is defined then the
[`interpolation_method`](@ref) defined for the TimeSeries is called,
[`extrapolation_method`](@ref) otherwise.
"""
function estimate(ts::TimeSeries, t::AbstractFloat)
    if time(ts)[1] ≤ t ≤ time(ts)[end]
        return interpolation_method(ts)(ts, t)
    else
        return extrapolation_method(ts)(ts, t)
    end
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

map(f::Function, ts::TimeSeries) = map(f, series(ts))

function isequal(ts1::TimeSeries, ts2::TimeSeries)
    return series(ts1) == series(ts2) && time(ts1) == time(ts2)
end
function ==(ts1::TimeSeries, ts2::TimeSeries)
    return isequal(ts1, ts2)
end

function isapprox(ts1::TimeSeries, ts2::TimeSeries)
    return series(ts1) ≈ series(ts2) && time(ts1) ≈ time(ts2)
end

function show(io::IO, ts::TimeSeries)
    print(io, "TimeSeries(")
    show(io, series(ts))
    print(io, ")")
end
show(ts::TimeSeries) = show(stdout, ts)

const __default_constructor_msg = """In most cases it is advised to use the more generic
[`TimeSeries`](@ref) constructor because the most compact representation possible will be
used anyway.
"""

const __esimate_msg = """The last two arguments define respectively how to interpolate or
extrapolate values when accessing the series by time.

The default behaviour is to retrun `missing` for interpolation and throw a `BoundsError` for
extrapolation.
"""

# -------------------------------------------------------------
# PointTimeSeries

"""
    PointTimeSeries(series, nointerpolation, noextrapolation)

Create a `PointTimeSeries` with points from `series`.

A `PointTimeSeries` is a `TimeSeries` with all points registered at a constant sampling rate
of 1.

$__default_constructor_msg

# Examples
```jldoctest
julia> ts = TimeSeries([1,2,3,4,1])
TimeSeries([1, 2, 3, 4, 1])

julia> PointTimeSeries([1,2,3,4,1])
TimeSeries([1, 2, 3, 4, 1])

julia> ts[0.0]
1

julia> ts[1.0]
2

julia> ts[1.5]
missing

julia> ts[1]
1

julia> ts[5]
1
```
"""
struct PointTimeSeries <: TimeSeries
    series::AbstractVector{<:Number}
    interpolation_method::Function
    extrapolation_method::Function

    function PointTimeSeries(
        values::AbstractVector{<:Number},
        interpolation_method::Function = nointerpolation,
        extrapolation_method::Function = noextrapolation
    )
        return new(values, interpolation_method, extrapolation_method)
    end
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
    ConstantRateTimeSeries(series, 1, nointerpolation, noextrapolation)

Create a `ConstantRateTimeSeries` with points from `series` sampled at constant rate
`samplerate`.

A `ConstantRateTimeSeries` is a `TimeSeries` with all points registered at a constant
sampling rate of `samplerate`.

$__default_constructor_msg

# Examples
```jldoctest
julia> ts = ConstantRateTimeSeries([1,2,3,1,4], 5)
TimeSeries(@5, [1, 2, 3, 1, 4])


julia> ts isa ConstantRateTimeSeries
true

julia> ts isa PointTimeSeries
false

julia> ts = ConstantRateTimeSeries([1,2,3,1,4])
TimeSeries([1, 2, 3, 1, 4])


julia> ts isa ConstantRateTimeSeries
false

julia> ts isa PointTimeSeries
true
```
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

function show(io::IO, ts::ConstantRateTimeSeries)
    print(io, "TimeSeries(@")
    show(io, ts.samplerate)
    print(io, ", ")
    show(io, series(ts))
    print(io, ")")
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
    TimeExplicitTimeSeries(series, collect(1:length(values)), nointerpolation,
    noextrapolation)

Create a `TimeExplicitTimeSeries` with points from `series` sampled at times `time`.

A `TimeExplicitTimeSeries` is a `TimeSeries` in which each point is associated with a time
explicitly

$__default_constructor_msg

# Examples
```jldoctest
julia> ts = TimeExplicitTimeSeries([1,2,3,1,4], [0.0, 0.5, 0.55, 0.75, 1.2])
TimeSeries([(1, 0.0), (2, 0.5), (3, 0.55), (1, 0.75), (4, 1.2)])

julia> ts = TimeExplicitTimeSeries([1,2,3,1,4], [0.0, 0.1, 0.2, 0.3, 0.4])
TimeSeries(@10.0, [1, 2, 3, 1, 4])

julia> ts = TimeExplicitTimeSeries([1,2,3,1,4], [0.0, 1.0, 2.0, 3.0, 4.0])
TimeSeries([1, 2, 3, 1, 4])

```
"""
struct TimeExplicitTimeSeries <: TimeSeries
    series::AbstractVector{<:Number}
    time::AbstractVector{<:AbstractFloat}
    interpolation_method::Function
    extrapolation_method::Function

    function TimeExplicitTimeSeries(
        series::AbstractVector{<:Number},
        time::AbstractVector{<:Real} = collect(1:length(series)),
        interpolation_method::Function = nointerpolation,
        extrapolation_method::Function = noextrapolation
    )
        @assert length(series) == length(time) "`series` and `time` mismtaches in length"

        @assert isincreasing(time) "`time` should be increasing"

        if time[1] != 0
            correction = time[1]
            time = time .- correction
        end

        if collect(time) == collect(1:length(series))
            return TimeSeries(
                series;
                interpolation_method = interpolation_method,
                extrapolation_method = extrapolation_method
            )
        elseif _has_approx_constant_increase(time)
            return ConstantRateTimeSeries(
                series,
                _approx_samplerate(time),
                interpolation_method,
                extrapolation_method
            )
        else
            return new(series, Float64.(time), interpolation_method, extrapolation_method)
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

function show(io::IO, ts::TimeExplicitTimeSeries)
    print(io, "TimeSeries(")
    show(io, collect(zip(series(ts),time(ts))))
    print(io, ")")
end
show
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
function convert(
    ::Type{T},
    t::Tuple{S,SR}
) where {T<:TimeSeries, S<:AbstractVector{<:Number}, SR<:Real}
    return TimeSeries(t[1], t[2])
end
function convert(
    ::Type{T},
    t::Tuple{SR,S}
) where {T<:TimeSeries, S<:AbstractVector{<:Number}, SR<:Real}
    return TimeSeries(t[2], t[1])
end

# vectors + explicit time coordinates -> TimeSeries
function convert(
    ::Type{T},
    t::Tuple{S1,S2}
) where {T<:TimeSeries, S1<:AbstractVector{<:Number}, S2<:AbstractVector{<:Number}}
    # TODO: implement conversion to TimeExplicitTimeSeries
    throw(Exception("conversion to TimeExplicitTimeSeries still not implemented"))
end

istimeseries(ts::Any) = false
istimeseries(ts::TimeSeries) = true
istimeseries(ts::AbstractVector{<:Number}) = true
istimeseries(ts::Tuple{<:AbstractVector{<:Number},<:Real}) = true
istimeseries(ts::Tuple{<:Real,<:AbstractVector{<:Number}}) = true
