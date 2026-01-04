-- Dhall Test File for UAST-Grep
-- Tests: types, functions, records, imports, expressions

-- Constants
let MAX_ITEMS = 100
let DEFAULT_NAME = "UAST-Grep"
let VERSION = "1.0.0"

-- Type definitions
let Status = < OK | NotFound | ServerError >

let Role = < Admin | User | Guest | Moderator >

-- Record types
let Person =
      { name : Text
      , age : Natural
      , email : Optional Text
      , active : Bool
      }

let Address =
      { street : Text
      , city : Text
      , state : Optional Text
      , postalCode : Text
      , country : Text
      }

let Config =
      { name : Text
      , version : Text
      , maxItems : Natural
      , enabled : Bool
      , features :
          { parsing : Bool
          , validation : Bool
          , formatting : Bool
          , caching :
              { enabled : Bool
              , ttl : Natural
              , maxSize : Text
              }
          }
      , thresholds :
          { warning : Double
          , error : Double
          , critical : Double
          }
      }

-- Default values
let defaultPerson
    : Person
    = { name = "Unknown"
      , age = 0
      , email = None Text
      , active = True
      }

let defaultConfig
    : Config
    = { name = DEFAULT_NAME
      , version = VERSION
      , maxItems = MAX_ITEMS
      , enabled = True
      , features =
        { parsing = True
        , validation = True
        , formatting = False
        , caching =
          { enabled = True
          , ttl = 3600
          , maxSize = "100MB"
          }
        }
      , thresholds =
        { warning = 0.75
        , error = 0.90
        , critical = 0.99
        }
      }

-- Functions
let isAdult
    : Person -> Bool
    = \(person : Person) -> Natural/isZero (Natural/subtract 18 person.age) == False

let greet
    : Text -> Text -> Text
    = \(greeting : Text) -> \(name : Text) -> "${greeting}, ${name}!"

let calculateSum
    : Natural -> Natural -> Natural
    = \(a : Natural) -> \(b : Natural) -> a + b

-- Function with optional parameter
let greetWithDefault
    : Text -> Optional Text -> Text
    = \(name : Text) ->
      \(maybeGreeting : Optional Text) ->
        merge
          { Some = \(g : Text) -> "${g}, ${name}!"
          , None = "Hello, ${name}!"
          }
          maybeGreeting

-- List operations
let numbers = [1, 2, 3, 4, 5]

let doubled
    : List Natural -> List Natural
    = \(xs : List Natural) ->
        List/map Natural Natural (\(x : Natural) -> x * 2) xs

let sum
    : List Natural -> Natural
    = \(xs : List Natural) ->
        List/fold
          Natural
          xs
          Natural
          (\(x : Natural) -> \(acc : Natural) -> x + acc)
          0

-- Filter function
let filter
    : forall (a : Type) -> (a -> Bool) -> List a -> List a
    = \(a : Type) ->
      \(predicate : a -> Bool) ->
      \(xs : List a) ->
        List/build
          a
          ( \(list : Type) ->
            \(cons : a -> list -> list) ->
            \(nil : list) ->
              List/fold
                a
                xs
                list
                ( \(x : a) ->
                  \(acc : list) ->
                    if predicate x then cons x acc else acc
                )
                nil
          )

-- Record update function
let updatePersonAge
    : Person -> Natural -> Person
    = \(person : Person) -> \(newAge : Natural) -> person // { age = newAge }

-- Merge records
let extendConfig
    : Config -> { debug : Bool } -> { name : Text, debug : Bool }
    = \(config : Config) ->
      \(extra : { debug : Bool }) ->
        { name = config.name, debug = extra.debug }

-- Union handling
let statusCode
    : Status -> Natural
    = \(status : Status) ->
        merge
          { OK = 200
          , NotFound = 404
          , ServerError = 500
          }
          status

let statusMessage
    : Status -> Text
    = \(status : Status) ->
        merge
          { OK = "OK"
          , NotFound = "Not Found"
          , ServerError = "Server Error"
          }
          status

-- Optional handling
let getEmailOrDefault
    : Person -> Text
    = \(person : Person) ->
        merge
          { Some = \(email : Text) -> email
          , None = "no-email@example.com"
          }
          person.email

-- Boolean operations
let and
    : Bool -> Bool -> Bool
    = \(a : Bool) -> \(b : Bool) -> if a then b else False

let or
    : Bool -> Bool -> Bool
    = \(a : Bool) -> \(b : Bool) -> if a then True else b

let not
    : Bool -> Bool
    = \(a : Bool) -> if a then False else True

-- Conditional expressions
let classify
    : Natural -> Text
    = \(n : Natural) ->
        if    Natural/isZero n
        then  "zero"
        else  if Natural/isZero (Natural/subtract 10 n)
              then "single digit"
              else if Natural/isZero (Natural/subtract 100 n)
              then "double digit"
              else "triple digit or more"

-- Nested records and lists
let Language =
      { id : Natural
      , name : Text
      , extension : Text
      , tier : Natural
      , features : List Text
      , active : Bool
      }

let languages
    : List Language
    = [ { id = 1
        , name = "TypeScript"
        , extension = ".ts"
        , tier = 1
        , features = ["classes", "interfaces", "generics"]
        , active = True
        }
      , { id = 2
        , name = "Python"
        , extension = ".py"
        , tier = 1
        , features = ["classes", "decorators", "async"]
        , active = True
        }
      , { id = 3
        , name = "Rust"
        , extension = ".rs"
        , tier = 1
        , features = ["structs", "traits", "lifetimes"]
        , active = True
        }
      ]

-- Text interpolation
let formatPerson
    : Person -> Text
    = \(person : Person) ->
        "Name: ${person.name}, Age: ${Natural/show person.age}"

-- Assert (for testing)
let _ = assert : isAdult { name = "Alice", age = 30, email = None Text, active = True } === True

let _ = assert : calculateSum 5 3 === 8

let _ = assert : greet "Hello" "World" === "Hello, World!"

-- Sample data
let samplePerson
    : Person
    = { name = "Alice"
      , age = 30
      , email = Some "alice@example.com"
      , active = True
      }

let sampleConfig
    : Config
    = defaultConfig // { name = "CustomApp", maxItems = 200 }

-- Export
in  { Person
    , Config
    , Status
    , Role
    , defaultPerson
    , defaultConfig
    , samplePerson
    , sampleConfig
    , languages
    , isAdult
    , greet
    , calculateSum
    , statusCode
    , statusMessage
    , doubled
    , sum
    }
