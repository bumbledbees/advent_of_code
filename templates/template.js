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


async function main(args) {
    if (args.length < 1) {
        error('Please specify an input file.');
    }
    for await (const line of readLines(args[0])) {
        // do stuff here
    }
    process.exit(0);
}


main(process.argv.slice(2));
