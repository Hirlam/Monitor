using Documenter

makedocs(
    sitename = "Monitor",
    format = Documenter.HTML(prettyurls = get(ENV, "CI", nothing) == "true"),
    pages = [ 
        "Monitor" => "index.md",
        "WebgraF" => "webgraf.md"
    ],
)

deploydocs(
    repo = "github.com/Hirlam/Monitor.git",
)
