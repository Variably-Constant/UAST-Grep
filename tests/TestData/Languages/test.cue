// CUE Test File for UAST-Grep
// Tests: definitions, constraints, schemas, expressions

// Package declaration
package test

// Import
import (
	"strings"
	"time"
	"list"
)

// Constants
#MaxItems:    100
#DefaultName: "UAST-Grep"
#Version:     "1.0.0"

// Basic type definitions
#Status: "ok" | "not_found" | "error"

#Role: "admin" | "user" | "guest" | "moderator"

// Numeric constraints
#Percentage: number & >=0 & <=100

#Port: int & >=1 & <=65535

#PositiveInt: int & >0

// String constraints
#NonEmptyString: string & !=""

#Email: =~"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

#URL: =~"^https?://.*"

// Struct definitions
#Person: {
	name:    #NonEmptyString
	age:     int & >=0 & <=150
	email?:  #Email
	active:  *true | bool
	role:    #Role
	tags:    [...string]
	created: time.Format("2006-01-02T15:04:05Z07:00")
}

#Address: {
	street:     string
	city:       string
	state?:     string
	postalCode: string
	country:    string
}

#UserProfile: {
	person:  #Person
	address: #Address
	settings: {
		theme:         *"light" | "dark" | "system"
		notifications: *true | bool
		language:      *"en" | string
	}
}

// Configuration schema
#Config: {
	name:     *#DefaultName | string
	version:  #Version
	maxItems: *#MaxItems | int & >0

	features: {
		parsing:    *true | bool
		validation: *true | bool
		formatting: *false | bool
		caching?: {
			enabled:  *true | bool
			ttl:      *3600 | int & >0
			maxSize:  *"100MB" | string
			strategy: *"LRU" | "LFU" | "FIFO"
		}
	}

	thresholds: {
		warning:  #Percentage & *75
		error:    #Percentage & *90
		critical: #Percentage & *99
	}

	logging: {
		level:  *"info" | "debug" | "warn" | "error"
		format: *"json" | "text"
		output: *"stdout" | "stderr" | string
	}
}

// Array/list schemas
#ItemList: [...#Item]

#Item: {
	id:       int & >0
	name:     #NonEmptyString
	value:    number
	status:   #Status
	metadata: {...}
}

// Constrained list
#LanguageList: list.MaxItems(10) & [...#Language]

#Language: {
	id:        int
	name:      string
	extension: =~"^\\.[a-z]+$"
	tier:      int & >=1 & <=9
	features:  [...string]
	active:    bool
}

// Disjunction (union types)
#SearchResult: #User | #Post | #Tag

#User: {
	_type:    "user"
	id:       int
	username: string
	email:    #Email
}

#Post: {
	_type:   "post"
	id:      int
	title:   string
	content: string
	status:  "draft" | "published" | "archived"
}

#Tag: {
	_type: "tag"
	id:    int
	name:  string
	slug:  string
}

// Default values and optionals
#ServerConfig: {
	host: *"localhost" | string
	port: *8080 | #Port

	tls?: {
		enabled:  bool
		certFile: string
		keyFile:  string
	}

	timeout: {
		read:    *"30s" | string
		write:   *"30s" | string
		idle:    *"60s" | string
		connect: *"5s" | string
	}

	cors?: {
		origins:         *["*"] | [...string]
		methods:         *["GET", "POST", "PUT", "DELETE"] | [...string]
		allowedHeaders:  [...string]
		exposedHeaders?: [...string]
		credentials:     *false | bool
	}
}

// Expressions and comprehensions
_numbers: [1, 2, 3, 4, 5]

doubled: [for n in _numbers {n * 2}]

filtered: [for n in _numbers if n > 2 {n}]

mapped: {for n in _numbers {"\(n)": n * 10}}

// Conditional expressions
_env: *"development" | string

settings: {
	if _env == "production" {
		debug: false
		minify: true
		caching: enabled: true
	}
	if _env == "development" {
		debug: true
		minify: false
		caching: enabled: false
	}
	if _env == "staging" {
		debug: true
		minify: true
		caching: enabled: true
	}
}

// String interpolation
greeting: "Hello, \(config.name)!"
versionString: "Version: \(#Version)"

// String operations
nameUpper: strings.ToUpper(config.name)
nameLower: strings.ToLower(config.name)
nameTrim:  strings.TrimSpace(config.name)

// Embedding and inheritance
#BaseEntity: {
	id:        int
	createdAt: time.Format("2006-01-02T15:04:05Z07:00")
	updatedAt: time.Format("2006-01-02T15:04:05Z07:00")
}

#ExtendedEntity: {
	#BaseEntity
	name:        string
	description: *"" | string
	metadata: {...}
}

// Closed structs
#ClosedConfig: close({
	name:    string
	version: string
	// No other fields allowed
})

// Hidden fields (for computation)
_computed: {
	total: list.Sum(values)
	avg:   _computed.total / len(values)
}
values: [10, 20, 30, 40, 50]

// Actual data instances
config: #Config & {
	name:     "MyApp"
	maxItems: 200

	features: caching: {
		ttl:     7200
		maxSize: "500MB"
	}

	logging: level: "debug"
}

server: #ServerConfig & {
	port: 3000

	tls: {
		enabled:  true
		certFile: "/etc/ssl/cert.pem"
		keyFile:  "/etc/ssl/key.pem"
	}

	cors: {
		origins:     ["https://example.com", "https://api.example.com"]
		credentials: true
	}
}

users: [...#Person] & [
	{
		name:    "Alice"
		age:     30
		email:   "alice@example.com"
		role:    "admin"
		tags:    ["developer", "lead"]
		created: "2024-01-15T10:30:00Z"
	},
	{
		name:    "Bob"
		age:     25
		email:   "bob@example.com"
		role:    "user"
		tags:    ["developer"]
		created: "2024-01-16T11:00:00Z"
	},
]

languages: #LanguageList & [
	{
		id:        1
		name:      "TypeScript"
		extension: ".ts"
		tier:      1
		features:  ["classes", "interfaces", "generics"]
		active:    true
	},
	{
		id:        2
		name:      "Python"
		extension: ".py"
		tier:      1
		features:  ["classes", "decorators", "async"]
		active:    true
	},
]
