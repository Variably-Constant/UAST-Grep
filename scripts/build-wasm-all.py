#!/usr/bin/env python3
"""Build all Tier 3 WASM grammars from source."""

import os
import subprocess
import shutil
import tempfile
from pathlib import Path

# Tier 3 grammars with their GitHub repos and optional subdirectories
GRAMMARS = {
    "verilog": "https://github.com/tree-sitter/tree-sitter-verilog",
    "latex": "https://github.com/latex-lsp/tree-sitter-latex",
    "sql": "https://github.com/DerekStride/tree-sitter-sql",
    "fortran": "https://github.com/stadelmanma/tree-sitter-fortran",
    "fsharp": ("https://github.com/ionide/tree-sitter-fsharp", "fsharp"),
    "kotlin": "https://github.com/fwcd/tree-sitter-kotlin",
    "cobol": "https://github.com/yutaro-sakamoto/tree-sitter-cobol",
    "scala": "https://github.com/tree-sitter/tree-sitter-scala",
    "objc": "https://github.com/tree-sitter-grammars/tree-sitter-objc",
    "julia": "https://github.com/tree-sitter/tree-sitter-julia",
    "d": "https://github.com/gdamore/tree-sitter-d",
    "crystal": "https://github.com/keidax/tree-sitter-crystal",
    "cuda": "https://github.com/tree-sitter-grammars/tree-sitter-cuda",
    "haskell": "https://github.com/tree-sitter/tree-sitter-haskell",
    "swift": "https://github.com/alex-pinkus/tree-sitter-swift",
    "perl": "https://github.com/tree-sitter-perl/tree-sitter-perl",
    "arduino": "https://github.com/tree-sitter-grammars/tree-sitter-arduino",
    "agda": "https://github.com/tree-sitter/tree-sitter-agda",
    "ocaml": ("https://github.com/tree-sitter/tree-sitter-ocaml", "grammars/ocaml"),
    "apex": ("https://github.com/aheber/tree-sitter-sfapex", "sfapex"),
    "dart": "https://github.com/UserNobody14/tree-sitter-dart",
    "groovy": "https://github.com/murtaza64/tree-sitter-groovy",
    "commonlisp": "https://github.com/theHamsta/tree-sitter-commonlisp",
    "zig": "https://github.com/tree-sitter-grammars/tree-sitter-zig",
    "awk": "https://github.com/Beaglefoot/tree-sitter-awk",
    "vim": "https://github.com/tree-sitter-grammars/tree-sitter-vim",
    "r": "https://github.com/r-lib/tree-sitter-r",
    "bitbake": "https://github.com/tree-sitter-grammars/tree-sitter-bitbake",
    "ada": "https://github.com/briot/tree-sitter-ada",
    "cairo": "https://github.com/tree-sitter-grammars/tree-sitter-cairo",
    "dhall": "https://github.com/jbellerb/tree-sitter-dhall",
    "cue": "https://github.com/eonpatapon/tree-sitter-cue",
    "doxygen": "https://github.com/tree-sitter-grammars/tree-sitter-doxygen",
    "comment": "https://github.com/stsewd/tree-sitter-comment",
}

def main():
    script_dir = Path(__file__).parent.parent
    output_dir = script_dir / "native" / "uast_core" / "grammars-wasm"
    output_dir.mkdir(parents=True, exist_ok=True)

    succeeded = []
    failed = []

    with tempfile.TemporaryDirectory() as temp_dir:
        for name, spec in GRAMMARS.items():
            if isinstance(spec, tuple):
                repo_url, subdir = spec
            else:
                repo_url = spec
                subdir = None

            print(f"\n{'='*60}")
            print(f"Building {name}...")
            print(f"{'='*60}")

            clone_dir = Path(temp_dir) / name

            # Clone
            result = subprocess.run(
                ["git", "clone", "--depth", "1", repo_url, str(clone_dir)],
                capture_output=True, text=True
            )
            if result.returncode != 0:
                print(f"  ERROR: Clone failed - {result.stderr}")
                failed.append((name, "clone failed"))
                continue

            # Determine grammar directory
            grammar_dir = clone_dir / subdir if subdir else clone_dir

            # Change to grammar directory
            os.chdir(grammar_dir)

            # Check if parser.c exists, if not run generate
            parser_c = grammar_dir / "src" / "parser.c"
            if not parser_c.exists():
                print(f"  Running tree-sitter generate...")
                result = subprocess.run(
                    ["tree-sitter", "generate"],
                    capture_output=True, text=True, timeout=300
                )
                if result.returncode != 0:
                    print(f"  ERROR: Generate failed - {result.stderr[:500]}")
                    failed.append((name, "generate failed"))
                    continue

            # Build WASM
            print(f"  Running tree-sitter build --wasm...")
            result = subprocess.run(
                ["tree-sitter", "build", "--wasm"],
                capture_output=True, text=True, timeout=300
            )
            if result.returncode != 0:
                print(f"  ERROR: WASM build failed - {result.stderr[:500]}")
                failed.append((name, "wasm build failed"))
                continue

            # Find generated WASM file
            wasm_files = list(grammar_dir.glob("*.wasm"))
            if not wasm_files:
                print(f"  ERROR: No WASM file generated")
                failed.append((name, "no wasm file"))
                continue

            # Copy to output
            wasm_file = wasm_files[0]
            dest_file = output_dir / f"{name}.wasm"
            shutil.copy(wasm_file, dest_file)
            size_kb = dest_file.stat().st_size / 1024
            print(f"  SUCCESS: {name}.wasm ({size_kb:.1f} KB)")
            succeeded.append(name)

    # Summary
    print(f"\n{'='*60}")
    print(f"BUILD COMPLETE")
    print(f"{'='*60}")
    print(f"Succeeded: {len(succeeded)}/{len(GRAMMARS)}")
    print(f"Failed: {len(failed)}/{len(GRAMMARS)}")

    if failed:
        print(f"\nFailed grammars:")
        for name, reason in failed:
            print(f"  - {name}: {reason}")

    print(f"\nOutput: {output_dir}")

if __name__ == "__main__":
    main()
