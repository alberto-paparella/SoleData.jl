
# -------------------------------------------------------------
# AbstractMultiFrameDataset - describe

const desc_dict = Dict{Symbol,Function}(
    :mean_m => mean,
    :min_m => minimum,
    :max_m => maximum,
    # allow catch22 desc
    (getnames(catch22) .=> catch22)...
)

const auto_desc_by_dim = Dict{Integer,Vector{Symbol}}(
    1 => [:mean_m, :min_m, :max_m]
)

function _describeonm(
    df::AbstractDataFrame;
    descfunction::Function,
    cols::AbstractVector{<:Integer} = 1:ncol(df),
    t::AbstractVector{<:NTuple{3,Integer}} = [(1, 0, 0)]
)
    frame_dim = dimension(df)
    @assert length(t) == 1 || length(t) == frame_dim "`t` length has to be `1` or the " *
        "dimension of the frame ($(frame_dim))"

    if frame_dim > 1 && length(t) == 1
        # if dimension is > 1 but only 1 triple is passed use it for all dimensions
        t = fill(t, frame_dim)
    end

    x = Matrix{AbstractFloat}[]

    for j in cols
        y = Matrix{AbstractFloat}(undef, nrow(df), t[1][1])
        for (i, paa_result) in enumerate(paa.(df[:,j]; f=descfunction, decdigits=4, t=t))
            y[i,:] = paa_result
        end
        push!(x, y)
    end

    return x
end

# TODO: describeonm should have the same interface as the `describe` function from DataFrames
# describe(df::AbstractDataFrame; cols=:)
# describe(df::AbstractDataFrame, stats::Union{Symbol, Pair}...; cols=:)
function describeonm(
    df::AbstractDataFrame;
    desc::AbstractVector{Symbol} = Symbol[],
    t::AbstractVector{<:NTuple{3,Integer}} = [(1, 0, 0)],
)
    for d in desc
        @assert d in keys(desc_dict) "`$(d)` is not a valid descriptor Symbol; available " *
            "descriptors are $(keys(desc_dict))"
    end

    return DataFrame(
        :Attirbutes => Symbol.(propertynames(df)),
        [d => _describeonm(df; descfunction = desc_dict[d], t) for d in desc]...
    )
end

# TODO: same as above
function DF.describe(mfd::AbstractMultiFrameDataset; kwargs...)
    return [DF.describe(mfd, i; kwargs...) for i in 1:nframes(mfd)]
end

# TODO: implement this
# function DF.describe(mfd::MultiFrameDataset, stats::Union{Symbol, Pair}...; cols=:)
#     # TODO: select proper defaults stats based on `dimension` of each frame
# end

function DF.describe(mfd::AbstractMultiFrameDataset, i::Integer; kwargs...)
    frame_dim = dimension(frame(mfd, i))
    if frame_dim == :mixed
        # TODO: implement
        throw(ErrorException("Description for `:mixed` dimension frame not implemented"))
    elseif frame_dim == 0
        return DF.describe(frame(mfd, i))
    else
        desc = haskey(kwargs, :desc) ? kwargs[:desc] : auto_desc_by_dim[frame_dim]
        return describeonm(frame(mfd, i); desc = desc, kwargs...)
    end
end
