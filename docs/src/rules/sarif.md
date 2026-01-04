# SARIF Output

SARIF (Static Analysis Results Interchange Format) is the industry standard for static analysis tool output, supported by GitHub, GitLab, Azure DevOps, and many CI/CD platforms.

## Generating SARIF Output

```bash
# Generate SARIF file
uast-grep scan -r rules/ -f sarif ./src > results.sarif

# With explicit output file
uast-grep scan -r rules/ -f sarif -o results.sarif ./src
```

## SARIF Structure

UAST-Grep generates SARIF 2.1.0 compliant output:

```json
{
  "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
  "version": "2.1.0",
  "runs": [
    {
      "tool": {
        "driver": {
          "name": "uast-grep",
          "version": "0.1.0",
          "informationUri": "https://github.com/Variably-Constant/UAST-Grep",
          "rules": [
            {
              "id": "sql-injection",
              "name": "SQL Injection",
              "shortDescription": {
                "text": "Possible SQL injection vulnerability"
              },
              "defaultConfiguration": {
                "level": "error"
              },
              "properties": {
                "tags": ["security", "injection", "CWE-89"]
              }
            }
          ]
        }
      },
      "results": [
        {
          "ruleId": "sql-injection",
          "level": "error",
          "message": {
            "text": "Possible SQL injection vulnerability"
          },
          "locations": [
            {
              "physicalLocation": {
                "artifactLocation": {
                  "uri": "src/database.py"
                },
                "region": {
                  "startLine": 42,
                  "startColumn": 5,
                  "endLine": 42,
                  "endColumn": 55
                }
              }
            }
          ],
          "fixes": [
            {
              "description": {
                "text": "Use parameterized query"
              },
              "artifactChanges": [
                {
                  "artifactLocation": {
                    "uri": "src/database.py"
                  },
                  "replacements": [
                    {
                      "deletedRegion": {
                        "startLine": 42,
                        "startColumn": 5,
                        "endLine": 42,
                        "endColumn": 55
                      },
                      "insertedContent": {
                        "text": "cursor.execute(query, (param,))"
                      }
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}
```

## Severity Mapping

| UAST-Grep | SARIF Level |
|-----------|-------------|
| `error` | `error` |
| `warning` | `warning` |
| `info` | `note` |
| `hint` | `note` |

## CI/CD Integration

### GitHub Actions

```yaml
name: Security Scan

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  security-scan:
    runs-on: ubuntu-latest
    permissions:
      security-events: write
    steps:
      - uses: actions/checkout@v4

      - name: Download UAST-Grep
        run: |
          curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-linux-x64.tar.gz
          tar xzf uast-grep-linux-x64.tar.gz
          chmod +x uast-grep

      - name: Run Security Scan
        run: ./uast-grep scan -r rules/security/ -f sarif ./src > results.sarif

      - name: Upload SARIF
        uses: github/codeql-action/upload-sarif@v2
        with:
          sarif_file: results.sarif
```

### GitHub Code Scanning API

```bash
# Upload SARIF directly
uast-grep scan -r rules/ -f sarif ./src > results.sarif

# Compress and encode
gzip -c results.sarif | base64 > results.sarif.gz.b64

# Upload via API
curl -X POST \
  -H "Authorization: token $GITHUB_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/repos/OWNER/REPO/code-scanning/sarifs \
  -d "{
    \"commit_sha\": \"$(git rev-parse HEAD)\",
    \"ref\": \"refs/heads/$(git branch --show-current)\",
    \"sarif\": \"$(cat results.sarif.gz.b64)\"
  }"
```

### GitLab CI

```yaml
security-scan:
  stage: test
  script:
    - curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-linux-x64.tar.gz
    - tar xzf uast-grep-linux-x64.tar.gz
    - ./uast-grep scan -r rules/ -f sarif ./src > gl-sast-report.json
  artifacts:
    reports:
      sast: gl-sast-report.json
```

### Azure DevOps

```yaml
- task: Bash@3
  displayName: 'Run UAST-Grep'
  inputs:
    targetType: 'inline'
    script: |
      curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-linux-x64.tar.gz
      tar xzf uast-grep-linux-x64.tar.gz
      ./uast-grep scan -r rules/ -f sarif ./src > $(Build.ArtifactStagingDirectory)/results.sarif

- task: PublishBuildArtifacts@1
  inputs:
    PathtoPublish: '$(Build.ArtifactStagingDirectory)/results.sarif'
    ArtifactName: 'CodeAnalysisLogs'
```

### Jenkins

```groovy
pipeline {
    agent any
    stages {
        stage('Security Scan') {
            steps {
                sh '''
                    curl -LO https://github.com/Variably-Constant/UAST-Grep/releases/latest/download/uast-grep-linux-x64.tar.gz
                    tar xzf uast-grep-linux-x64.tar.gz
                    ./uast-grep scan -r rules/ -f sarif ./src > results.sarif
                '''
                recordIssues(
                    tools: [sarif(pattern: 'results.sarif')]
                )
            }
        }
    }
}
```

## Viewing SARIF Files

### VS Code

Install the [SARIF Viewer](https://marketplace.visualstudio.com/items?itemName=MS-SarifVSCode.sarif-viewer) extension.

### Online

Use [Microsoft SARIF Viewer](https://microsoft.github.io/sarif-web-component/) to view SARIF files in your browser.

### Command Line

```bash
# Pretty print with jq
cat results.sarif | jq '.runs[0].results[] | {rule: .ruleId, file: .locations[0].physicalLocation.artifactLocation.uri, line: .locations[0].physicalLocation.region.startLine}'

# Count by severity
cat results.sarif | jq '[.runs[0].results[].level] | group_by(.) | map({level: .[0], count: length})'

# List affected files
cat results.sarif | jq -r '.runs[0].results[].locations[0].physicalLocation.artifactLocation.uri' | sort -u
```

## Custom Properties

UAST-Grep includes custom properties in SARIF output:

### Rule Properties

```json
{
  "id": "sql-injection",
  "properties": {
    "tags": ["security", "CWE-89"],
    "precision": "high",
    "security-severity": "9.0"
  }
}
```

### Result Properties

```json
{
  "ruleId": "sql-injection",
  "properties": {
    "matchedPattern": "cursor.execute(§QUERY + §INPUT)",
    "metavariables": {
      "QUERY": "\"SELECT * FROM users WHERE id = \"",
      "INPUT": "user_id"
    }
  }
}
```

## Filtering Results

### By Severity

```bash
# Only errors
cat results.sarif | jq '.runs[0].results = [.runs[0].results[] | select(.level == "error")]'

# Errors and warnings
cat results.sarif | jq '.runs[0].results = [.runs[0].results[] | select(.level == "error" or .level == "warning")]'
```

### By Rule

```bash
# Specific rule
cat results.sarif | jq '.runs[0].results = [.runs[0].results[] | select(.ruleId == "sql-injection")]'

# Rules with tag
cat results.sarif | jq --arg tag "security" '.runs[0].results = [.runs[0].results[] | select(.ruleId as $id | .runs[0].tool.driver.rules[] | select(.id == $id) | .properties.tags | contains([$tag]))]'
```

### By Path

```bash
# Specific directory
cat results.sarif | jq '.runs[0].results = [.runs[0].results[] | select(.locations[0].physicalLocation.artifactLocation.uri | startswith("src/api/"))]'
```

## Best Practices

1. **Use semantic versioning** for rule IDs when rules change
2. **Include CWE IDs** in tags for security rules
3. **Provide fixes** for actionable results
4. **Set appropriate severity** to avoid alert fatigue
5. **Test SARIF output** before CI integration
6. **Archive results** for trend analysis
