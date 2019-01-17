# brök

Find broken links in text documents

Similar idea to [awesome_bot](https://github.com/dkhamsing/awesome_bot), but with different output options.

Currently only supports `http://` and `https://` prefixed URLs

## Install

```bash
stack build && stack install
```

## Usage

### Basic Usage

Check all links in a single text file:

```bash
brok test.md
```

Or in multiple files:

```bash
brok test.md links.tex
```

If you're using this as part of a test suite, you probably only need the errors:

```
brok text.md links.tex > /dev/null
```

### Options

#### Cache

By default brök will cache successes for a day. It will always recheck errors.

If you want to adjust the cache length, you can enter the number of seconds after which the cache invalidates:

```bash
# cache for a week
brok --cache 604800 test.md links.tex
```

#### Ignore URLs

You can tell brök to ignore URLs with specified prefixes:

```bash
# ignore facebook and amazon URLS
brok --ignore "http://facebook.com" "http://amazon.com" test.md links.tex
```

#### Interval

By default brök waits for 100ms between URL checks. You can change the delay:

```bash
# wait for 1 second between checks
brok --interval 1000 test.md links.tex
```
