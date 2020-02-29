using Documenter

const PAGES = [ "Monitor" => "index.md",
                "WebgraF" => "webgraf.md"
              ]


makedocs(sitename="Monitor",
         pages=PAGES)



deploydocs(
    repo = "github.com/Hirlam/Monitor.git",
)
