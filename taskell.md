## Refactoring


## Features

- Should detect if internet connection is down
- Cache length option should accept units: s, m, h, d - default to s
- Should be able to detect links without http:// or https:// prefixes

## Bugs

- Can't parse links containing single quote marks
- Edge case: shouldn't delay if only a single URL being checked

## In Progress


## Done

- Basic link parsing
- Basic link checking
- Should output file that problem occured in
- Should return correct exit code
- Should store recently tested links in .brokdb file
- Option for cache length
- Should space out requests
- Option for interval between requests
- Should be able to ignore specific URLs
    > List of URL prefixes
- --help command
- Use attoparsec
    > Already used by HTTP-conduit, so may as well use it
