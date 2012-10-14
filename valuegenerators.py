import functools
import string
import random
import pprint

__author__ = 'Stephen Elliott'

__random_generator = random.Random()

def print_output(pre='', converter=lambda x: x):
    """
    Returns a decorator which prints the output of the decorated function.

    :param pre: Text to be displayed before the output is printed.
    :param converter: Converts function output before it is printed. Useful for converting to a string.
    :return: A decorator
    """
    def decorator(fn):
        @functools.wraps(fn)
        def output_printer(*args, **kwargs):
            output = fn(*args, **kwargs)
            if len(pre):
                print pre
            pprint.pprint(converter(output))
            return output
        return output_printer
    return decorator

#@print_output("Random Integer: ")
def generate_integer(min=-300, max=300):
    """
    Generates a random integer between the given min and max. If omitted, the min
    and max used are the min and max possible integers for the given architecture.

    >>> -300 <= generate_integer() <= 300
    True
    >>> 0 <= generate_integer(0) <= 300
    True
    >>> 50 <= generate_integer(50, 60) <= 60
    True
    """
    return __random_generator.randint(min, max)

#@print_output("Random Float: ")
def generate_float(min=-100, max=100):
    """
    Generates a random floating point number between the given min and max. If
    omitted, the min and max used are the min and max possible floating point
    numbers for the given architecture.

    >>> -100 <= generate_float() <= 100
    True
    >>> 0 <= generate_float(0) <= 100
    True
    >>> 50 <= generate_float(50, 60) <= 60
    True
    """
    return __random_generator.random() * (max - min) + min

def generate_double(min=-100, max=100):
    """
    Generates a random double-precision floating point number between the given
    min and max. If omitted, the min and max used are the min and max possible
    double-precision floating point numbers for the given architecture.

    >>> -100 <= generate_double() <= 100
    True
    >>> 0 <= generate_double(0) <= 100
    True
    >>> 50 <= generate_double(50, 60) <= 60
    True
    """
    return generate_float(min, max)

#@print_output("Random String: ")
def generate_string(length=10):
    """
    Generates a string of the given length containing random letters and numbers.
    If omitted, the length used will be 10.

    >>> len(generate_string())
    10
    >>> len(generate_string(100))
    100
    """
    return ''.join(generate_char() for _ in range(length))

def generate_char():
    """
    Generates a random ASCII character.

    >>> generate_char() in string.ascii_letters + string.digits
    True
    """
    return random.choice(string.ascii_letters + string.digits)

#@print_output("Random Boolean: ")
def generate_bool():
    """
    Generates a boolean value.
    """
    r = __random_generator.randint(0, 1)
    if r == 1:
        return True
    return False