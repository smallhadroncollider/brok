## Refactoring

- Use attoparsec
    > Already used by HTTP-conduit, so may as well use it

## Features

- Should space out requests
- Should detect if internet connection is down
- Should be able to ignore specific URLs
    > Could add a URL to .brokdb with a far future timestamp?

## Bugs

- Can't parse links containing single quote marks

## In Progress


## Done

- Basic link parsing
- Basic link checking
- Should output file that problem occured in
- Should return correct exit code
- Should store recently tested links in .brokdb file
