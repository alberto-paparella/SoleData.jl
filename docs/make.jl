using SoleData
using Documenter

DocMeta.setdocmeta!(SoleData, :DocTestSetup, :(using SoleData); recursive=true)

makedocs(;
    modules=[SoleData],
    authors="Lorenzo BALBONI, Federico MANZELLA, Giovanni PAGLIARINI, Eduard I. STAN",
    repo="https://github.com/aclai-lab/SoleData.jl/blob/{commit}{path}#{line}",
    sitename="SoleData.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://aclai-lab.github.io/SoleData.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Datasets" => "datasets.md",
        "Manipulation" => "manipulation.md",
        "Description" => "description.md",
        "Utils" => "utils.md",
    ],
)

deploydocs(;
    repo="github.com/aclai-lab/SoleData.jl",
    devbranch="dev",
)
