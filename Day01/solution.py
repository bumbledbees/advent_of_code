import logging
from string import digits as DIGITS
import sys


def error(msg, rv=1):
    logging.error(msg)
    return rv


def calibration_value_1(string):
    # find the first digit in the string
    num1 = None
    for ch in string:
        if ch in DIGITS:
            num1 = int(ch)
            break

    # num1 will be None if the string contains no digits
    if num1 is None:
        return None

    # find the last digit in the string
    for ch in reversed(string):
        if ch in DIGITS:
            num2 = int(ch)
            break
    
    return num1 * 10 + num2


DIGIT_STRINGS = ['one', 'two', 'three', 'four', 'five', 'six', 'seven',
                 'eight', 'nine']

def calibration_value_2(string):
    num1 = None
    match = ""
    for ch in string:
        logging.debug(f'string: {string}, current ch: {ch}')
        if ch in DIGITS:
            num1 = int(ch)
            logging.debug(f'found num1: {num1}')
            break

        match += ch
        matches = [d for d in DIGIT_STRINGS if d.startswith(match)]
        logging.debug(f'matching "{match}", potential matches: {matches}')
        if len(matches) == 0:
            match = match[1:]
        elif len(matches) == 1:
            if match == matches[0]:
                num1 = DIGIT_STRINGS.index(match) + 1
                logging.debug(f'found num1: {num1}')
                break
    
    if num1 is None:
        return None

    match = ""
    for ch in reversed(string):
        logging.debug(f'string: {string}, current ch: {ch}')
        if ch in DIGITS:
            num2 = int(ch)
            logging.debug(f'found num2: {num2}')
            break

        match = ch + match
        matches = [d for d in DIGIT_STRINGS if d.endswith(match)]
        logging.debug(f'matching "{match}", potential matches: {matches}')
        if len(matches) == 0:
            match = match[:-1]
        elif len(matches) == 1:
            if match == matches[0]:
                num2 = DIGIT_STRINGS.index(match) + 1
                logging.debug(f'found num2: {num2}')
                break
    logging.debug(f'solution: {num1*10+num2}')

    return num1 * 10 + num2


def main(argv):
    if len(argv) <= 1:
        return error('Please specify an input file.')

    input_filepath = argv[1]
    try:
        with open(input_filepath, 'r') as input_file:
            sum_1 = sum([calibration_value_1(line[:-1]) for line in input_file])
            print(f'Part 1 answer (no digit name substitution): {sum_1}')

            input_file.seek(0, 0)

            sum_2 = sum([calibration_value_2(line[:-1]) for line in input_file])
            print(f'Part 2 answer (w/ digit name substitution): {sum_2}')
    except FileNotFoundError:
        return error(f'File not found: "{argv[1]}".')

    return 0


if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    sys.exit(main(sys.argv))
