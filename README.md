# brök

Find broken links in text documents

![Demo](https://files.smallhadroncollider.com/brok-0.1.gif)

Similar idea to [awesome_bot](https://github.com/dkhamsing/awesome_bot), but with different output options.

Currently only supports `http://` and `https://` prefixed URLs

## Install

[Binaries for Mac and Linux are available](https://github.com/smallhadroncollider/brok/releases). Add the binary to a directory in your path (such as `/usr/local/bin`).

### Cabal

If you have `cabal` installed:

```bash
cabal install brok
```

Make sure you run `cabal update` if you haven't run it recently.

### Building

**Requirements**: [Stack](https://docs.haskellstack.org/en/stable/README/)

The following command will build brök and then install it in `~/.local/bin`:

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
