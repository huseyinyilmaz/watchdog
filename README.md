# [watchdog][]

Watchdog is a command line utility that runs given command on filesystem changes.

Here is the help output:

``` sh
# watchdog --help
Execute a command on file system changes in current directory.

Usage: watchdog command

Available options:
  -h,--help                Show this help text

Example: $ watchdog echo There was a change on current directory
```

Here are some example usages
```
# Run build every time there is an update in current directory.
watchdog make build

# Copy current directory to server every time there is an update.
watchdog "scp . server.com:project_dir"
```
## How to get it

### Get precompiled binary

You can get precompiled binary from [release][] page.

### Compile source

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