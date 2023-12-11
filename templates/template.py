import sys


def error(msg):
    print(f'Error: {msg}', file=sys.stderr) 
    return 1


def main(argv):
    if len(argv) <= 1:
        return error('Please specify an input file.')

    input_filepath = argv[1]
    try:
        with open(input_filepath, 'r') as input_file:
            for line in input_file:
                # do stuff here
                print(line[:-1])
    except FileNotFoundError:
        return error(f'File not found: "{argv[1]}".')
    
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
