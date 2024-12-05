# YAML in Elm

[![Tests Actions Status](https://github.com/MaybeJustJames/yaml/workflows/Tests/badge.svg)](https://github.com/MaybeJustJames/yaml/actions/) [![Elm package](https://img.shields.io/elm-package/v/MaybeJustJames/yaml.svg)](https://package.elm-lang.org/packages/MaybeJustJames/yaml/latest/)

Convert between type-safe Elm values and [YAML](https://yaml.org).

This is forked from the wonderful
[terezka/yaml](https://package.elm-lang.org/packages/terezka/yaml/latest/).

## Install

```bash
$ elm install MaybeJustJames/yaml
```

and import the library in an elm file like this

```elm
import Yaml.Decode -- for decoders
import Yaml.Encode -- for encoders
```

## Documentation

Find the documentation on [Elm's package website](http://package.elm-lang.org/packages/MaybeJustJames/yaml/latest).

## Example Usage

### Decoding YAML into Elm values
Say you have some YAML which looks like this:

```yaml
---
- name:
    first: Marie
    last: Curie
  occupation: [ chemist, physicist ]
  age: 66
  children: [ Irène, Ève ]
- name:
    first: Alva
    last: Myrdal
  occupation: [ sociologist, diplomat, politician ]
  age: 84
  children: []
- name:
    first: Svetlana
    last: Alexievich
  occupation: [ journalist, historian ]
  age: 72
  children: []
...  
```

to decode this, you could write

```elm
import Yaml.Decode exposing (..)

type alias Woman =
  { name : String
  , occupation : List String
  , age : Int
  , children : Int -- number of children
  }

decoder : Decoder Woman
decoder =
  map4 Woman
    (map2 (\first last -> first ++ " " ++ last)
          (at ["name", "first"] string)
          (at ["name", "last"] string))
    (field "occupation" (list string))
    (field "age" int)
    (map List.length (field "children" (list string)))


fromString
  (list decoder)
  yamlString -- The string containing the YAML example above

```

### Encoding your Elm values into YAML

Say you are manipulating Github Actions configuration files which you might model like so:

```elm
import Yaml.Encode exposing (..)

-- Top-level structure
type alias GHActions =
    { name : String
    , trigger : List String
    , jobs : List Action
    }

type alias Action =
    { id : String
    , name : String
    , runsOn : String
    , steps : List Step
    }

type alias Step =
    { name : String
    , uses : String
    , run : String
    }

-- top-level encoder
encodeGHA : GHActions -> Encoder
encodeGHA action =
    record
        [ ("name", string action.name)
        , ("on", list (string action.trigger))
        , ("jobs", encodeJobs action.jobs)
        ]

encodeJobs : List Action -> Encoder
encodeJobs actions =
    actions
        |> List.map (\action -> (action.id, encodeJob action))
        |> record

encodeJob : Action -> Encoder
encodeJob action =
    record
        [ ("name", action.name)
        , ("runs-on", action.runsOn)
        , ("steps", list (encodeStep action.steps))
        ]

encodeStep : Step -> Encoder
    record
        [ ("name", step.name)
        , ("uses", step.uses)
        , ("run", step.run)
        ]
```

Now that you have modelled the data and defined encoders,
encoding into a YAML formatted string is done like:
```elm
-- An example Elm value that we could encode:
myAction : GHActions
myAction =
    { name = "Tests"
    , trigger = ["push", "pull_request"]
    , jobs = [
        { id = "test"
        , name = "Test with elm-test"
        , runsOn = "ubuntu-latest"
        , steps = [ { name = "Test"
                    , uses = ""
                    , run = "npx elm-test"
                    }
                  , { name = "Verify examples"
                    , uses = ""
                    , run = "npm elm-verify-examples --run-tests"
                    }
                  ]
        }
        ]
    }

-- A function to do the encoding / conversion
outputGHAction : String
outputGHAction =
    encodeGHA myAction
        |> document
        |> toString 2

```

This will result in the YAML String:
```yaml
---
name: Tests
on:
  - push
  - pull_request
jobs:
  test:
    name: Test with elm-test
    runs-on: ubuntu-latest
    steps:
      - name: Test
        uses:
        run: npx elm-test
      - name: Verify examples
        uses:
        run: npx elm-verify-examples --run-tests
...
```

## Development

The branch `parser-logging` contains a version of the
[parser logger](https://discourse.elm-lang.org/t/improved-parser-logger/5964)
by @Janiczek.

This, along with writing detailed tests using [elm-test](https://github.com/elm-community/elm-test)
is how I've been developing this package.

Please feel encouraged and welcome to submit bugs, PRs, etc.

## Tests

This package uses [elm-test](https://github.com/elm-explorations/test)
and [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).


## Major Missing Features

- Testing against the official [YAML test suite](https://github.com/yaml/yaml-test-suite). [#7](https://github.com/MaybeJustJames/yaml/issues/7)

## Copying

You are free to copy, modify, and distribute this package with attribution under the terms of the BSD-3-Clause license. See the [LICENSE](LICENSE) file for details.
