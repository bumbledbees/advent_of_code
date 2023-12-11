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
                    maps.seeds = words.slice(1).map(n => Number(n));
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

    const paths = maps.seeds.map(s => pathfind(s));
    const locations = paths.map(p => p[p.length - 1]);

    console.log(`Lowest location: ${Math.min(...locations)}`);

    maps.seed_pairs = []
    for (const n of Array(maps.seeds.length / 2).keys()) {
        maps.seed_pairs.push([maps.seeds[n], maps.seeds[n + 1]]);
    }

    function fill_in_the_blanks(theMaps) {
        theMaps.sort((a, b) => a.src - b.src);
        let lower_bound = 0;
        let mapped_ranges = [];
        let unmapped_ranges = [];

        for (const m of theMaps) {
            if (lower_bound < m.src) {
                unmapped_ranges.push([lower_bound, m.src]);
                mapped_ranges.push([m.src, m.upper_src_bound()]);
                lower_bound = m.upper_src_bound();
            } else if (m.src == lower_bound) {
                const last_idx = mapped_ranges.length - 1;
                const last_range = mapped_ranges[last_idx];
                if (last_range && last_range[1] == m.src) {
                    mapped_ranges[last_idx] = (
                        [last_range[0], m.upper_src_bound()]);
                    lower_bound = m.upper_src_bound();
                } else {
                    mapped_ranges.push([m.src, m.upper_src_bound()]);
                    lower_bound = m.upper_src_bound();
                }
            } else {
                error("something weird happened");
            }
        }

        for (const r of unmapped_ranges) {
            theMaps.push(new Mapping(r[0], r[0], r[1] - r[0]));
        }
        return theMaps;
    }

    function reduce_map(m_ab, list_m_bc) {
        // m_ab : mapping from a -> b (bounded)
        // list_m_bc : list of (bounded) mappings from b -> c
        //
        // returns: list of mappings from a -> c, equivalent to the results of
        //          evaluating a -> b -> c.
        list_m_bc.sort((b, c) => b.src - c.src);
        const last_m_bc = list_m_bc[list_m_bc.length - 1];
        if (m_ab.upper_dest_bound() > last_m_bc.upper_src_bound()) {
            const src = m_ab.upper_dest_bound();
            const len = src;
            list_m_bc.push(new Mapping(src, dest, len));
        }
    

        let new_mappings = [];
        const lower_to = m_from.dest;
        const upper_to = m_from.upper_dest_bound();
        const lower_from = m_from.src;
        const upper_from = m_from.upper_src_bound();

        console.log('---');
        console.log(m_from);
        console.log(upper_to);
        console.log('\n');
        for (const [idx, m_to] of ms_to.entries()) {
            console.log(m_to);
            if (lower_to < m_to.src) { 
                const m_to_prev = ms_to[idx - 1];
                if (upper_to < m_to.src) {
                    console.log('upper_to < m_to.src');
                    const new_dest = m_to_prev.maps_to(m_from.dest);
                    new_mappings.push(
                        new Mapping(m_from.src, new_dest, m_from.len));
                } else if (upper_to === m_to.src) {
                    // idk man;
                } else {
                    console.log('upper_to >= m_to.src');
                    const remaining_len = upper_to - m_to.src;
                    const remaining_src = upper_from - remaining_len;
                    const remainder = (
                        new Mapping(remaining_src, m_to.src, remaining_len));
                    new_mappings.push(...reduce_map(remainder, ms_to));

                    const new_dest = m_to.maps_to(m_to.src);
                    const new_len = m_from.len - remaining_len;
                    new_mappings.push(
                        new Mapping(m_from.src, new_dest, new_len));
                }
                break;
            }
        }
        return new_mappings;
    }

    const m = maps['seed-to-soil'][0];
    const ms = fill_in_the_blanks(maps['water-to-light'])
    console.log(ms);
    //console.log(reduce_map(m, ms));

    process.exit(0);
}


main(process.argv.slice(2));
