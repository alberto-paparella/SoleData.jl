
# -------------------------------------------------------------
# SoleType - utils

function _has_approx_constant_increase(time::AbstractVector{<:Number})
    last_diff = time[2] - time[1]

    for i in 2:(length(time)-1)
        new_diff = time[i+1] - time[i]

        if !isapprox(new_diff, last_diff)
            return false
        end

        last_diff = new_diff
    end

    return true
end

function _approx_samplerate(time::AbstractVector{<:Number})
    return 1.0 / mean([(time[i+1] - time[i]) for i in 1:(length(time)-1)])
end
