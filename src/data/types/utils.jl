
# -------------------------------------------------------------
# SoleType - utils

function _has_approx_constant_increase(time::AbstractVector{<:Number})
    last_diff = time[2] - time[1]
    first_diff = deepcopy(last_diff)

    for i in 2:(length(time)-1)
        new_diff = time[i+1] - time[i]

        if !isapprox(new_diff, last_diff) && isapprox(new_diff, first_diff)
            return false
        end

        last_diff = new_diff
    end

    return true
end

function _approx_samplerate(time::AbstractVector{<:Number})
    return 1.0 / mean([(time[i+1] - time[i]) for i in 1:(length(time)-1)])
end

function _check_trend(v::AbstractVector{<:Real}, f::Function)
    return length(findall(x -> f(x, 0), [v[i] - v[i-1] for i in 2:length(v)])) == length(v)-1
end
function isincreasing(v::AbstractVector{<:Real})
    return _check_trend(v, >)
end
function isdecreasing(v::AbstractVector{<:Real})
    return _check_trend(v, <)
end
function isnonincreasing(v::AbstractVector{<:Real})
    return _check_trend(v, <=)
end
function isnondecreasing(v::AbstractVector{<:Real})
    return _check_trend(v, >=)
end
