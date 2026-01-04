# Nix Test File for UAST-Grep
# Tests: expressions, functions, sets, lists, let bindings, imports

# =============================================================================
# Basic Expressions
# =============================================================================

{
  # String values
  name = "UAST-Grep";
  version = "1.0.0";
  description = "Nix test file for parser testing";

  # String interpolation
  greeting = "Hello, ${name}!";
  versionString = "Version: ${version}";

  # Multi-line strings
  multiLine = ''
    This is a multi-line string.
    It preserves indentation.
    Interpolation works: ${name}
  '';

  # Numbers
  integer = 42;
  negative = -17;
  float = 3.14159;

  # Booleans
  enabled = true;
  disabled = false;

  # Null
  nullValue = null;

  # Paths
  absolutePath = /etc/nixos/configuration.nix;
  relativePath = ./test.nix;
  homePath = ~/documents;

  # =============================================================================
  # Lists
  # =============================================================================

  simpleList = [ 1 2 3 4 5 ];

  stringList = [ "one" "two" "three" ];

  mixedList = [ 1 "two" true null ];

  nestedList = [
    [ 1 2 3 ]
    [ 4 5 6 ]
    [ 7 8 9 ]
  ];

  # List concatenation
  concatenated = [ 1 2 ] ++ [ 3 4 ] ++ [ 5 ];

  # =============================================================================
  # Attribute Sets
  # =============================================================================

  simpleSet = {
    a = 1;
    b = 2;
    c = 3;
  };

  nestedSet = {
    outer = {
      inner = {
        value = "deep";
      };
    };
  };

  # Recursive set (self-referential)
  recursiveSet = rec {
    x = 1;
    y = 2;
    sum = x + y;
  };

  # Attribute set with dynamic key
  dynamicKey = let key = "myKey"; in {
    ${key} = "value";
  };

  # Inherit attributes
  inheritExample = let
    a = 1;
    b = 2;
  in {
    inherit a b;
    c = 3;
  };

  # Inherit from another set
  sourceSet = { x = 10; y = 20; };
  inheritFromSet = {
    inherit (sourceSet) x y;
    z = 30;
  };

  # =============================================================================
  # Let Expressions
  # =============================================================================

  letExample = let
    x = 10;
    y = 20;
    helper = a: b: a + b;
  in
    helper x y;

  letWithSet = let
    config = {
      port = 8080;
      host = "localhost";
    };
  in
    "http://${config.host}:${toString config.port}";

  # =============================================================================
  # With Expression
  # =============================================================================

  withExample = let
    attrs = { a = 1; b = 2; c = 3; };
  in
    with attrs; a + b + c;

  # =============================================================================
  # Functions
  # =============================================================================

  # Simple function
  double = x: x * 2;

  # Multiple arguments (curried)
  add = a: b: a + b;

  # Function with pattern matching on sets
  greet = { name, greeting ? "Hello" }: "${greeting}, ${name}!";

  # Function with variadic arguments
  sumList = list: builtins.foldl' (a: b: a + b) 0 list;

  # Function with @ pattern
  fullConfig = args@{ name, ... }: {
    inherit name;
    other = args;
  };

  # =============================================================================
  # Conditionals
  # =============================================================================

  ifExample = if true then "yes" else "no";

  nestedIf = x:
    if x > 0 then "positive"
    else if x < 0 then "negative"
    else "zero";

  # Assert
  assertExample = assert 1 + 1 == 2; "math works";

  # =============================================================================
  # Built-in Functions
  # =============================================================================

  builtinsExample = {
    # Type checking
    isString = builtins.isString "hello";
    isInt = builtins.isInt 42;
    isList = builtins.isList [ 1 2 3 ];
    isAttrs = builtins.isAttrs { };

    # Type conversion
    toString = builtins.toString 42;
    fromJSON = builtins.fromJSON ''{ "key": "value" }'';
    toJSON = builtins.toJSON { a = 1; b = 2; };

    # List operations
    length = builtins.length [ 1 2 3 ];
    head = builtins.head [ 1 2 3 ];
    tail = builtins.tail [ 1 2 3 ];
    elemAt = builtins.elemAt [ "a" "b" "c" ] 1;
    filter = builtins.filter (x: x > 2) [ 1 2 3 4 5 ];
    map = builtins.map (x: x * 2) [ 1 2 3 ];
    foldl = builtins.foldl' (a: b: a + b) 0 [ 1 2 3 4 5 ];

    # Set operations
    attrNames = builtins.attrNames { a = 1; b = 2; };
    attrValues = builtins.attrValues { a = 1; b = 2; };
    hasAttr = builtins.hasAttr "a" { a = 1; };
    getAttr = builtins.getAttr "a" { a = 1; };

    # String operations
    stringLength = builtins.stringLength "hello";
    substring = builtins.substring 0 5 "hello world";
    replaceStrings = builtins.replaceStrings ["old"] ["new"] "old text";
  };

  # =============================================================================
  # Derivations (Build Expressions)
  # =============================================================================

  exampleDerivation = derivation {
    name = "example";
    system = builtins.currentSystem;
    builder = "/bin/sh";
    args = [ "-c" "echo hello > $out" ];
  };

  # Using stdenv.mkDerivation pattern
  packageExample = { pkgs }: pkgs.stdenv.mkDerivation {
    pname = "UAST-Grep";
    version = "1.0.0";

    src = pkgs.fetchFromGitHub {
      owner = "example";
      repo = "UAST-Grep";
      rev = "v1.0.0";
      sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
    };

    buildInputs = with pkgs; [
      dotnet-sdk
      nodejs
    ];

    nativeBuildInputs = with pkgs; [
      pkg-config
    ];

    buildPhase = ''
      dotnet build --configuration Release
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp -r bin/Release/* $out/bin/
    '';

    meta = with pkgs.lib; {
      description = "Cross-language AST grep tool";
      homepage = "https://github.com/example/UAST-Grep";
      license = licenses.mit;
      maintainers = [ maintainers.example ];
      platforms = platforms.linux ++ platforms.darwin;
    };
  };

  # =============================================================================
  # Imports
  # =============================================================================

  # Import another file (commented to avoid errors)
  # importedConfig = import ./config.nix;

  # Import with arguments
  # importedWithArgs = import ./package.nix { inherit pkgs; };

  # =============================================================================
  # NixOS Configuration Style
  # =============================================================================

  nixosModule = { config, pkgs, lib, ... }: {
    options = {
      services.uastGrep = {
        enable = lib.mkEnableOption "UAST-Grep service";

        port = lib.mkOption {
          type = lib.types.port;
          default = 8080;
          description = "Port to listen on";
        };

        logLevel = lib.mkOption {
          type = lib.types.enum [ "debug" "info" "warn" "error" ];
          default = "info";
          description = "Logging level";
        };
      };
    };

    config = lib.mkIf config.services.uastGrep.enable {
      systemd.services.UAST-Grep = {
        description = "UAST-Grep Service";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.UAST-Grep}/bin/UAST-Grep --port ${toString config.services.uastGrep.port}";
          Restart = "on-failure";
          User = "uast";
          Group = "uast";
        };
      };

      users.users.uast = {
        isSystemUser = true;
        group = "uast";
        description = "UAST-Grep service user";
      };

      users.groups.uast = { };
    };
  };

  # =============================================================================
  # Flake Style
  # =============================================================================

  flakeExample = {
    description = "UAST-Grep Nix Flake";

    inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
      flake-utils.url = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs, flake-utils }:
      flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          packages.default = pkgs.hello;

          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              dotnet-sdk
              nodejs
              git
            ];

            shellHook = ''
              echo "Welcome to UAST-Grep development shell"
            '';
          };
        }
      );
  };

}
