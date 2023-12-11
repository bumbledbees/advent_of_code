require "io"
require "math"
require "os"


function quit_error(msg)
    msg = msg or ""
    io.stderr:write(string.format("Error: %s\n", msg))
    os.exit(1)
end


function print_array(arr)
    local arr_ = {}
    for _, val in ipairs(arr) do
        if type(val) == "table" then
            print_array(val)
        else
            table.insert(arr_, string.format("%d", val))
        end
    end
    print(table.concat(arr_, ", "))
end


function main(args)
    if #args < 1 then
        quit_error("Please provide an input file.")
    end

    local file, maybeError = io.open(args[1])
    if not file then
        quit_error("Error opening file: " .. maybeError)
    end

    function extract_values(line)
        local values = {}
        for word in line:gmatch("%S+") do
            local val = tonumber(word)
            if val ~= nil then
                table.insert(values, val)
            end
        end
        return values
    end

    local times = extract_values(file:read("*line"))
    local distances = extract_values(file:read("*line"))

    local races = {}
    for i = 1, #times, 1 do
        races[i] = { times[i], distances[i] }
    end

    function calc_distance(charge_time, duration)
        -- acceleration increases by 1 mm/ms^2 while charging
        local velocity = charge_time
        local time_moving = duration - charge_time
        local distance = velocity * time_moving

        return distance
    end

    local winning_strategies = {}
    for _, race in ipairs(races) do
        local duration = race[1]
        local record = race[2]

        local winners = {}
        for i = 0, duration, 1 do
            local distance = calc_distance(i, duration)
            if distance > record then
                table.insert(winners, i)
            end
        end
        table.insert(winning_strategies, winners)
    end

    local part1_answer = 1  -- couldn't come up w/ a better variable name
    for _, race_results in ipairs(winning_strategies) do 
        part1_answer = part1_answer * #race_results
    end
    print("Product of the number of ways to beat each race's record: " ..
          part1_answer)

    local time = 0
    local distance = 0
    for _, races in ipairs(races) do
        local t = races[1]
        local d = races[2]

        time = time * 10 ^ math.ceil(math.log10(t)) + t
        distance = distance * 10 ^ math.ceil(math.log10(d)) + d
    end

    local winning_strategies_2 = {}
    for i = 0, time, 1 do
        local attempt = calc_distance(i, time)
        if attempt > distance then
            table.insert(winning_strategies_2, i)
        end
    end
    print("Number of ways to beat the final race's record: " ..
          #winning_strategies_2)

    os.exit(0)
end


main(arg)
