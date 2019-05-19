# [watchdog][]

Watchdog is a command line utility that runs given command on filesystem changes.

``` sh
# watchdog --help
Execute a command on file system changes in current directory.

Usage: watchdog command

Available options:
  -h,--help                Show this help text

Example: $ watchdog echo There was a change on current directory
```
## How to get it

### Get precompiled binary

You can get precompiled binary from [release][] page.


To compile source code following commands can be used.

``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```

Thanks again, and happy hacking!

[watchdog]: https://github.com/githubuser/watchdog
[release]: https://github.com/huseyinyilmaz/watchdog/releases