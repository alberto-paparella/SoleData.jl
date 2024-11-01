using SoleData: AbstractUnivariateFeature

using MultiData: AbstractDimensionalDataset

import MultiData: ninstances, nvariables

import SoleData:
    islogiseed, initlogiset, frame,
    featchannel, readfeature, featvalue, vareltype, featvaltype

function islogiseed(dataset::AbstractDimensionalDataset)
    ndims(eltype(dataset)) >= 1
end

"""
    function initlogiset(
        dataset::AbstractDimensionalDataset,
        features::AbstractVector;
        worldtype_by_dim::Union{Nothing,AbstractDict{Int,Type{<:AbstractWorld}}}=nothing
    )::UniformFullDimensionalLogiset

Given an [`AbstractDimensionalDataset`](@ref), build a
[`UniformFullDimensionalLogiset`](@ref).

# Keyword Arguments
- worldtype_by_dim::Union{Nothing,AbstractDict{Int,Type{<:AbstractWorld}}}=nothing:
map between a dimensionality, as integer, and the [`AbstractWorld`](@ref) type associated;
when unspecified, this is defaulted to `0 => OneWorld, 1 => Interval, 2 => Interval2D`.

See also [`AbstractDimensionalDataset`](@ref),
SoleLogics.AbstractWorld,
MultiData.dimensionality,
[`UniformFullDimensionalLogiset`](@ref).
"""
function initlogiset(
    dataset::AbstractDimensionalDataset,
    features::AbstractVector;
    worldtype_by_dim::Union{Nothing,AbstractDict{Int,Type{<:AbstractWorld}}}=nothing
)::UniformFullDimensionalLogiset
    worldtype_by_dim = isnothing(worldtype_by_dim) ? Dict{Int,Type{<:AbstractWorld}}([
        0 => OneWorld, 1 => Interval, 2 => Interval2D]) :
        worldtype_by_dim

    _ninstances = ninstances(dataset)

    # Note: a dimension is for the variables
    _worldtype(instancetype::Type{<:AbstractArray{T,1}}) where {T} = worldtype_by_dim[0]
    _worldtype(instancetype::Type{<:AbstractArray{T,2}}) where {T} = worldtype_by_dim[1]
    _worldtype(instancetype::Type{<:AbstractArray{T,3}}) where {T} = worldtype_by_dim[2]

    function _worldtype(instancetype::Type{<:AbstractArray})
        error("Cannot initialize logiset with dimensional instances of type " *
            "`$(instancetype)`. Please, provide " *
            "instances of size X × Y × ... × nvariables." *
            "Note that, currently, only ndims ≤ 4 (dimensionality ≤ 2) is supported."
        )
    end

    W = _worldtype(eltype(dataset))
    N = dimensionality(dataset)

    @assert all(f->f isa VarFeature, features)
    features = UniqueVector(features)
    nfeatures = length(features)
    FT = eltype(features)

    # @show dataset
    # @show features
    # @show typeof(dataset)
    U = Union{map(f->featvaltype(dataset, f), features)...}

    if allequal(map(i_instance->channelsize(dataset, i_instance), 1:ninstances(dataset)))
        _maxchannelsize = maxchannelsize(dataset)
        featstruct = Array{U,length(_maxchannelsize)*2+2}(
                undef,
                vcat([[s, s] for s in _maxchannelsize]...)...,
                _ninstances,
                length(features)
            )
        # if !isconcretetype(U) # TODO only in this case but this breaks code
            # @warn "Abstract featvaltype detected upon initializing UniformFullDimensionalLogiset logiset: $(U)."
            fill!(featstruct, 0)
        # end
        return UniformFullDimensionalLogiset{U,W,N}(featstruct, features)
    else
        error("Different frames encountered for different dataset instances.")
        # @warn "Different frames encountered for different dataset instances." *
        #     "A generic logiset structure will be used, but be advised that it may be very slow."
        # # SoleData.frame(dataset, i_instance)
        # return ExplicitModalLogiset([begin
        #     fr = SoleData.frame(dataset, i_instance)
        #     (Dict{W,Dict{FT,U}}([w => Dict{FT,U}() for w in allworlds(fr)]), fr)
        #     end for i_instance in 1:ninstances(dataset)])
    end
end

function frame(
    dataset::AbstractDimensionalDataset,
    i_instance::Integer
)
    FullDimensionalFrame(channelsize(dataset, i_instance))
end

function featchannel(
    dataset::AbstractDimensionalDataset,
    i_instance::Integer,
    f::AbstractFeature,
)
    get_instance(dataset, i_instance)
end

function readfeature(
    dataset::AbstractDimensionalDataset,
    featchannel::Any,
    w::W,
    f::VarFeature,
) where {W<:AbstractWorld}
    _interpret_world(::OneWorld, instance::AbstractArray{T,1}) where {T} = instance
    _interpret_world(w::Interval, instance::AbstractArray{T,2}) where {T} = instance[w.x:w.y-1,:]
    _interpret_world(w::Interval2D, instance::AbstractArray{T,3}) where {T} = instance[w.x.x:w.x.y-1,w.y.x:w.y.y-1,:]
    wchannel = _interpret_world(w, featchannel)
    computefeature(f, wchannel)
end

function featvalue(
    feature::AbstractFeature,
    dataset::AbstractDimensionalDataset,
    i_instance::Integer,
    w::W,
) where {W<:AbstractWorld}
    readfeature(dataset, featchannel(dataset, i_instance, feature), w, feature)
end

function vareltype(
    dataset::AbstractDimensionalDataset{T},
    i_variable::VariableId,
) where {T}
    T
end

############################################################################################

function islogiseed(
    dataset::NamedTuple,
)
    true
end

function frame(
    dataset::NamedTuple,
    i_instance::Integer
)
    # dataset_dimensional, varnames = dataframe2dimensional(dataset; dry_run = true)
    # FullDimensionalFrame(channelsize(dataset_dimensional, i_instance))
    column = first(X)
    # frame(column, i_instance)
    v = column[i_instance]
    !(v isa Array) ? OneWorld() : FullDimensionalFrame(size(v))
end

# # Note: used in naturalgrouping.
# function frame(
#     dataset::NamedTuple,
#     column::Vector,
#     i_instance::Integer
# )
#     v = [column[i_instance] for column in values(Tables.columns(X))]
#     !(eltype(v) isa Array) ? OneWorld() : FullDimensionalFrame(size(v))
# end
varnames(::AbstractDimensionalDataset) = nothing
############################################################################################

using DataFrames

using MultiData: dataframe2dimensional

function islogiseed(
    dataset::AbstractDataFrame,
)
    true
end

function initlogiset(
    dataset::AbstractDataFrame,
    features::AbstractVector{<:VarFeature};
    kwargs...,
)
    _ninstances = nrow(dataset)
    dimensional, varnames = dataframe2dimensional(dataset; dry_run = true)

    initlogiset(dimensional, features; kwargs...)
end

function frame(
    dataset::AbstractDataFrame,
    i_instance::Integer
)
    # dataset_dimensional, varnames = dataframe2dimensional(dataset; dry_run = true)
    # FullDimensionalFrame(channelsize(dataset_dimensional, i_instance))
    column = dataset[:,1]
    # frame(column, i_instance)
    v = column[i_instance]
    (v == ()) ? OneWorld() : FullDimensionalFrame(size(v))
end

# Note: used in naturalgrouping.
function frame(
    dataset::AbstractDataFrame,
    column::Vector,
    i_instance::Integer
)
    v = column[i_instance]
    (v == ()) ? OneWorld() : FullDimensionalFrame(size(v))
end

# # Remove!! dangerous
# function frame(
#     column::Vector,
#     i_instance::Integer
# )
#     FullDimensionalFrame(size(column[i_instance]))
# end

function featchannel(
    dataset::AbstractDataFrame,
    i_instance::Integer,
    f::AbstractFeature,
)
    @views dataset[i_instance, :]
end

function readfeature(
    dataset::AbstractDataFrame,
    featchannel::Any,
    w::W,
    f::VarFeature,
) where {W<:AbstractWorld}
    _interpret_world(::OneWorld, instance::DataFrameRow) = instance
    _interpret_world(w::Interval, instance::DataFrameRow) = map(varchannel->varchannel[w.x:w.y-1], instance)
    _interpret_world(w::Interval2D, instance::DataFrameRow) = map(varchannel->varchannel[w.x.x:w.x.y-1,w.y.x:w.y.y-1], instance)
    wchannel = _interpret_world(w, featchannel)
    computefeature(f, wchannel)
end

function featchannel(
    dataset::AbstractDataFrame,
    i_instance::Integer,
    f::AbstractUnivariateFeature,
)
    @views dataset[i_instance, SoleData.i_variable(f)]
end

function readfeature(
    dataset::AbstractDataFrame,
    featchannel::Any,
    w::W,
    f::AbstractUnivariateFeature,
) where {W<:AbstractWorld}
    _interpret_world(::OneWorld, varchannel::T) where {T} = varchannel
    _interpret_world(w::Interval, varchannel::AbstractArray{T,1}) where {T} = varchannel[w.x:w.y-1]
    _interpret_world(w::Interval2D, varchannel::AbstractArray{T,2}) where {T} = varchannel[w.x.x:w.x.y-1,w.y.x:w.y.y-1]
    wchannel = _interpret_world(w, featchannel)
    computeunivariatefeature(f, wchannel)
end

function featvalue(
    feature::AbstractFeature,
    dataset::AbstractDataFrame,
    i_instance::Integer,
    w::W,
) where {W<:AbstractWorld}
    readfeature(dataset, featchannel(dataset, i_instance, feature), w, feature)
end

function vareltype(
    dataset::AbstractDataFrame,
    i_variable::VariableId,
)
    eltype(eltype(dataset[:,i_variable]))
end

varnames(dataset::AbstractDataFrame) = names(dataset)
