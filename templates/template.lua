require "io"
require "os"


function quit_error(msg)
    msg = msg or ""
    io.stderr:write(string.format("Error: %s\n", msg))
    os.exit(1)
end


function main(args)
    if #args < 1 then
        quit_error("Please provide an input file.")
    end

    local file, maybeError = io.open(args[1])
    if not file then
        quit_error("Error opening file: " .. maybeError)
    end

    for line in file:lines() do
        -- do stuff here
    end

    os.exit(0)
end


main(arg)
