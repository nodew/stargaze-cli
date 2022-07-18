# Stargaze CLI

Stargaze is a CLI tool to help you manage your starred projects on GitHub.

## Build && Install

```bash
$ git clone https://github.com/nodew/stargaze-cli.git

$ cd stargaze-cli

$ stack build

-- Or install by --

$ stack install

```

## Usage

```bash
$ stargaze config --user "YOUR GITHUB USERNAME"

$ stargaze update

$ stargaze list --lang haskell --pattern stargaze

$ stargaze -h

Usage: stargaze COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  config                   Config
  update                   Update your project list from upstream
  list                     List projects
  owners                   List top owners
  tags                     List top tags
  languages                List top languages

```
