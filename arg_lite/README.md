# Arg lite

basic command line argument parser

## TODO

- align naming for args (option, arg, defined arg etc.)
- can I replace usage of string by something to match &str as well for simplicity?

## Scope

1. Option with a value
    - --file-name example.txt
    - -f example.txt
2. Flag
    - --verbose
    - -v
    - -vkb (equal to -v -k -b)
3. Arguments
    - arg1 arg2
    - arg1 --verbose (the second arg isn't treated as option anymore)

