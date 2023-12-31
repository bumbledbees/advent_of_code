#!/usr/bin/env/node

const fs = require('fs/promises');


function error(msg) {
    process.stderr.write(`Error: ${msg}\n`);
    process.exit(1);
}


async function* readLines(path) {
    const file = await fs.open(path);
    for await (const line of file.readLines()) {
        yield line;
    }
}


class Mapping {
    constructor(dest, src, len) {
        this.dest = dest;
        this.src = src;
        this.len = len;
        this.delta = dest - src;
        this.dist = src + this.delta;
    }

    maps_to(num) {
        const upper_bound = this.src + this.len;
        if (num < this.src || num >= upper_bound) {
            return null;
        } else {
            return this.dest + (num - this.src);
        }
    }

    upper_src_bound() {
        return this.src + this.len;
    }

    upper_dest_bound() {
        return this.dest + this.len;
    }
}


async function main(args) {
    if (args.length < 1) {
        error('Please specify an input file.');
    }

    let maps = {};
    let seeds = [];

    {
        let section = '';
        for await (const line of readLines(args[0])) {
            let words = line.split(/\s+/);
            let firstWord = words[0];
            if (!isNaN(parseInt(firstWord))) { 
                const newMap = new Mapping(...words.map(n => Number(n)));
                maps[section].push(newMap);
                continue;
            }
            switch (firstWord) {
                case 'seeds:':
                    seeds = words.slice(1).map(n => Number(n));
                    break;
                case '':
                    continue;
                default:
                    section = firstWord;
                    maps[section] = [];
            }
        }
    }

    function pathfind(seed) {
        const CONVERT_ORDER = [
            'seed-to-soil', 'soil-to-fertilizer', 'fertilizer-to-water',
            'water-to-light', 'light-to-temperature',
            'temperature-to-humidity', 'humidity-to-location'
        ];
        let steps = [seed];

        for (const [idx, step] of CONVERT_ORDER.entries()) {
            for (const m of maps[step]) { 
                const maybePath = m.maps_to(steps[steps.length - 1]);
                if (maybePath !== null) {
                    steps.push(maybePath);
                    break;
                }
            }

            // true if no paths were found in this step
            if (steps.length - 1 === idx) { 
                steps.push(steps[steps.length - 1]);
            }
        }
        return steps;
    }

    const paths = seeds.map(s => pathfind(s));
    const locations = paths.map(p => p[p.length - 1]);

    console.log(`Lowest location: ${Math.min(...locations)}`);

    let seed_pairs = [];
    for (const n of Array(seeds.length / 2).keys()) {
        seed_pairs.push([seeds[n * 2], seeds[n * 2 + 1]]);
    }

    function fill_in_the_blanks(theMaps, upper_bound = 0) {
        theMaps.sort((a, b) => a.src - b.src);

        let lower_bound = 0;
        let maps_new = [];

        for (const m of theMaps) {
            if (lower_bound < m.src) {
                const m_new = new Mapping(
                    lower_bound, lower_bound, m.src - lower_bound);
                maps_new.push(m_new);
                maps_new.push(m);
                lower_bound = m.upper_src_bound();
            } else if (m.src === lower_bound) {
                maps_new.push(m)
                lower_bound = m.upper_src_bound();
            }
        }

        const last_map = maps_new[maps_new.length - 1]
        if (upper_bound > last_map.upper_src_bound()) {
            const last_upper = last_map.upper_src_bound();
            const m_new = new Mapping(
                last_upper, last_upper, upper_bound - last_upper);
            maps_new.push(m_new);
        }

        return maps_new;
    }

    function reduce_maps(m_ab, list_m_bc) {
        // m_ab : mapping from a -> b (bounded)
        // list_m_bc : list of (bounded) mappings from b -> c. must be sorted
        //             by src value in ascending order.
        //
        // returns: list of mappings from a -> c, equivalent to the results of
        //          evaluating a -> b -> c.
        //
        let new_mappings = [];
        const lower_a = m_ab.src;
        const upper_a = m_ab.upper_src_bound();
        const lower_b = m_ab.dest;
        const upper_b = m_ab.upper_dest_bound();

        // console.log(m_ab);
        // console.log('\n');
        for (const [idx, m_bc] of list_m_bc.entries()) {
            // console.log(m_bc);
            if (lower_b < m_bc.upper_src_bound()) { 
                if (upper_b <= m_bc.upper_src_bound()) {
                    // console.log('upper_b <= m_bc.upper_src_bound');
                    const new_dest = m_bc.maps_to(lower_b);
                    new_mappings.push(
                        new Mapping(new_dest, lower_a, m_ab.len));
                }
                else {
                    // console.log('upper_b > m_bc.upper_src_bound');
                    const remain_len = upper_b - m_bc.upper_src_bound();
                    const remain_src = upper_a - remain_len;
                    const remain_dest = m_ab.maps_to(remain_src);
                    const remainder = (
                        new Mapping(remain_dest, remain_src, remain_len));

                    const new_dest = m_bc.maps_to(lower_b);
                    const new_len = m_ab.len - remain_len;

                    new_mappings.push(
                        new Mapping(new_dest, lower_a, new_len));
                    new_mappings.push(
                        ...reduce_maps(remainder, list_m_bc.slice(idx + 1)));
                }
                break;
            }
        }
        return new_mappings;
    }

    let flattened = [];
    const map_types = Object.keys(maps)
    const last_idx = map_types.length - 1;

    for (const [idx, k] of map_types.entries()) {
        let current_maps = maps[k];
        if (idx === 0) {
            flattened = fill_in_the_blanks(current_maps);
        } else {
            let new_flattened = [];
            current_maps.sort((x, y) => x.src - y.src);
            const last_map = flattened[flattened.length - 1];
            current_maps = fill_in_the_blanks(current_maps,
                                              last_map.upper_src_bound());
            for (const m of flattened) {
                new_flattened.push(...reduce_maps(m, current_maps));
            }
            flattened = new_flattened;
        }
    }
    
    flattened.sort((x, y) => x.dist - y.dist);
    let location_of_lowest = -1;
    for (const mapping of flattened) {
        let found = false;
        for (const [start, len] of seed_pairs) {
            if (mapping.src < (start + len) && mapping.src >= start) {
                found = true;
                console.log(`first: ${mapping.dest}`);
                console.log(mapping);
                location_of_lowest = mapping.dest;
            } else if (mapping.upper_src_bound() > start
                       && mapping.upper_src_bound() <= (start + len)) {
                found = true;
                console.log(`second: ${mapping.maps_to(start)}`);
                console.log(mapping);
                location_of_lowest = mapping.maps_to(start);
            }
        }
        if (found) {
            break;
        }
    }

    console.log(`Lowest possible location: ${location_of_lowest}`);
    process.exit(0);
}


main(process.argv.slice(2));
