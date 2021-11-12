module SoleType

using Statistics
using ScientificTypes

import Base: eltype, isempty, iterate, map, getindex, length
import Base: firstindex, lastindex, ndims, size, show
import Base: convert

# -------------------------------------------------------------
# exports

# abstract supertype
export AbstractEntity

# dimensional
export DimensionalEntity
export TemporalEntity, SpatialEntity, SpatialTemporalEntity

# graph
export GraphEntity

# textual
export TextualEntity

# time series
export TimeSeries
export PointTimeSeries, ConstantRateTimeSeries, TimeExplicitTimeSeries
export istimeseries

# -------------------------------------------------------------
# Abbreviations
const ST = ScientificTypes

# -------------------------------------------------------------
# Abstract types

"""
Abstract supertype for all Entities.
"""
abstract type AbstractEntity end

"""
Abstract supertype for all Entities that can be described in a table-like form.
"""
abstract type DimensionalEntity{ND} <: AbstractEntity where ND <:UInt end

abstract type TemporalEntity <: DimensionalEntity{1} end
abstract type SpatialEntity{ND} <: DimensionalEntity{ND} end
abstract type SpatialTemporalEntity{ND} <: DimensionalEntity{ND} end

"""
Abstract supertype for all Entities that can be described by a graph.
"""
abstract type GraphEntity{ND} <: AbstractEntity where ND <:UInt end

"""
Abstract supertype for all Entities that can be described by a text.
"""
abstract type TextualEntity{ND} <: AbstractEntity where ND <:UInt end

include("time-series.jl")
include("time-series-estimation.jl")
include("utils.jl")

end # module
