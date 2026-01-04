/**
 * CodeQL Test File for UAST-Grep
 * Tests: classes, predicates, queries, data flow analysis
 *
 * @name UAST-Grep CodeQL Test
 * @description Test file demonstrating CodeQL language features
 * @kind problem
 * @id test/codeql-test
 * @problem.severity warning
 * @precision high
 * @tags test
 *       security
 *       UAST-Grep
 */

// Import statements
import javascript
import semmle.javascript.security.dataflow.CommandInjectionCustomizations
import DataFlow::PathGraph

// =============================================================================
// Basic Classes
// =============================================================================

/**
 * A class representing a function call in JavaScript.
 */
class FunctionCallExpr extends CallExpr {
  FunctionCallExpr() {
    // This class captures all call expressions
    this instanceof CallExpr
  }

  /**
   * Gets the name of the called function if it's a direct call.
   */
  string getFunctionName() {
    result = this.getCallee().(Identifier).getName()
  }

  /**
   * Holds if this is a call to a function with the given name.
   */
  predicate hasName(string name) {
    this.getFunctionName() = name
  }
}

/**
 * A class representing a dangerous function call.
 */
class DangerousFunctionCall extends FunctionCallExpr {
  DangerousFunctionCall() {
    this.getFunctionName() = ["eval", "exec", "execSync", "spawn", "spawnSync"]
  }

  /**
   * Gets the first argument of the dangerous call.
   */
  Expr getCommandArgument() {
    result = this.getArgument(0)
  }
}

/**
 * A source of user-controlled data.
 */
class UserInputSource extends DataFlow::Node {
  UserInputSource() {
    // HTTP request parameters
    exists(MethodCallExpr call |
      call.getMethodName() = ["param", "query", "body", "params"] and
      this = DataFlow::valueNode(call)
    )
    or
    // Command line arguments
    exists(PropAccess access |
      access.getPropertyName() = "argv" and
      this = DataFlow::valueNode(access)
    )
    or
    // Environment variables
    exists(PropAccess access |
      access.getBase().(PropAccess).getPropertyName() = "env" and
      this = DataFlow::valueNode(access)
    )
  }
}

/**
 * A sink for command injection.
 */
class CommandInjectionSink extends DataFlow::Node {
  DangerousFunctionCall call;

  CommandInjectionSink() {
    this = DataFlow::valueNode(call.getCommandArgument())
  }

  /**
   * Gets the dangerous function call.
   */
  DangerousFunctionCall getCall() {
    result = call
  }
}

// =============================================================================
// Predicates
// =============================================================================

/**
 * Holds if the expression is a literal string.
 */
predicate isLiteralString(Expr e) {
  e instanceof StringLiteral
}

/**
 * Holds if the expression is a template literal.
 */
predicate isTemplateLiteral(Expr e) {
  e instanceof TemplateLiteral
}

/**
 * Holds if the function has the given number of parameters.
 */
predicate hasNParameters(Function f, int n) {
  n = f.getNumParameter()
}

/**
 * Holds if the variable is assigned a value.
 */
predicate isAssigned(Variable v, Expr value) {
  exists(AssignExpr assign |
    assign.getLhs().(Identifier).getName() = v.getName() and
    value = assign.getRhs()
  )
}

/**
 * Gets a function that calls the given function.
 */
Function getCallerOf(Function callee) {
  exists(CallExpr call |
    call.getCallee().(Identifier).getName() = callee.getName() and
    result = call.getEnclosingFunction()
  )
}

/**
 * Gets the depth of the call chain from root to the given function.
 */
int callDepth(Function f) {
  not exists(getCallerOf(f)) and result = 0
  or
  result = 1 + max(callDepth(getCallerOf(f)))
}

// =============================================================================
// Data Flow Configuration
// =============================================================================

/**
 * Configuration for tracking command injection vulnerabilities.
 */
class CommandInjectionConfig extends TaintTracking::Configuration {
  CommandInjectionConfig() {
    this = "CommandInjectionConfig"
  }

  override predicate isSource(DataFlow::Node source) {
    source instanceof UserInputSource
  }

  override predicate isSink(DataFlow::Node sink) {
    sink instanceof CommandInjectionSink
  }

  override predicate isSanitizer(DataFlow::Node node) {
    // Allow list validation
    exists(MethodCallExpr call |
      call.getMethodName() = "includes" and
      node = DataFlow::valueNode(call)
    )
    or
    // Regex validation
    exists(MethodCallExpr call |
      call.getMethodName() = "test" and
      call.getReceiver() instanceof RegExpLiteral and
      node = DataFlow::valueNode(call.getArgument(0))
    )
  }

  override predicate isAdditionalTaintStep(DataFlow::Node pred, DataFlow::Node succ) {
    // String concatenation
    exists(BinaryExpr concat |
      concat.getOperator() = "+" and
      (
        pred = DataFlow::valueNode(concat.getLeftOperand()) or
        pred = DataFlow::valueNode(concat.getRightOperand())
      ) and
      succ = DataFlow::valueNode(concat)
    )
    or
    // Template literal interpolation
    exists(TemplateLiteral template, Expr element |
      element = template.getAnElement() and
      pred = DataFlow::valueNode(element) and
      succ = DataFlow::valueNode(template)
    )
  }
}

// =============================================================================
// Queries
// =============================================================================

/**
 * @name Find all eval calls
 * @description Finds all calls to the eval function
 * @kind problem
 * @id test/eval-usage
 * @problem.severity warning
 */
/*
from FunctionCallExpr call
where call.hasName("eval")
select call, "Eval function used"
*/

/**
 * @name Find functions with many parameters
 * @description Finds functions with more than 5 parameters
 * @kind problem
 * @id test/many-parameters
 */
/*
from Function f
where hasNParameters(f, 6) or hasNParameters(f, 7) or f.getNumParameter() > 7
select f, "Function has " + f.getNumParameter().toString() + " parameters"
*/

/**
 * @name Command injection vulnerability
 * @description Finds paths from user input to command execution
 * @kind path-problem
 * @id test/command-injection
 * @problem.severity error
 * @security-severity 9.0
 * @precision high
 */
from CommandInjectionConfig config, DataFlow::PathNode source, DataFlow::PathNode sink
where config.hasFlowPath(source, sink)
select sink.getNode(), source, sink,
  "This command depends on $@, which may be controlled by an attacker.",
  source.getNode(), "user input"

/**
 * @name Find hardcoded credentials
 * @description Finds potential hardcoded passwords or API keys
 * @kind problem
 * @id test/hardcoded-credentials
 * @problem.severity error
 */
from AssignExpr assign, string name
where
  name = assign.getLhs().(Identifier).getName() and
  (
    name.toLowerCase().matches("%password%") or
    name.toLowerCase().matches("%secret%") or
    name.toLowerCase().matches("%apikey%") or
    name.toLowerCase().matches("%api_key%")
  ) and
  assign.getRhs() instanceof StringLiteral
select assign, "Potential hardcoded credential in variable '" + name + "'"

/**
 * @name Find SQL injection sinks
 * @description Identifies potential SQL injection vulnerabilities
 * @kind problem
 * @id test/sql-injection
 */
from MethodCallExpr call
where
  call.getMethodName() = ["query", "execute", "raw"] and
  exists(Expr arg |
    arg = call.getArgument(0) and
    (
      arg instanceof TemplateLiteral or
      arg.(BinaryExpr).getOperator() = "+"
    )
  )
select call, "Potential SQL injection in " + call.getMethodName() + " call"

// =============================================================================
// Aggregate Predicates
// =============================================================================

/**
 * Gets the total number of function calls in the codebase.
 */
int totalFunctionCalls() {
  result = count(FunctionCallExpr call | | call)
}

/**
 * Gets the average number of parameters per function.
 */
float averageParameterCount() {
  result = avg(Function f | | f.getNumParameter())
}

/**
 * Gets the function with the most calls.
 */
string mostCalledFunction() {
  result = max(string name, int calls |
    calls = count(FunctionCallExpr call | call.getFunctionName() = name | call)
  |
    name order by calls desc
  )
}

// =============================================================================
// Helper Classes
// =============================================================================

/**
 * A module for analyzing JavaScript security issues.
 */
module SecurityAnalysis {
  /**
   * A class representing a sensitive data source.
   */
  class SensitiveData extends DataFlow::Node {
    string category;

    SensitiveData() {
      exists(Variable v, string name |
        name = v.getName().toLowerCase() and
        this = DataFlow::valueNode(v.getAnAccess()) and
        (
          name.matches("%password%") and category = "password"
          or
          name.matches("%credit%card%") and category = "credit_card"
          or
          name.matches("%ssn%") and category = "ssn"
          or
          name.matches("%secret%") and category = "secret"
        )
      )
    }

    /**
     * Gets the category of sensitive data.
     */
    string getCategory() {
      result = category
    }
  }

  /**
   * A class representing a logging sink.
   */
  class LoggingSink extends DataFlow::Node {
    LoggingSink() {
      exists(MethodCallExpr call |
        call.getMethodName() = ["log", "info", "debug", "warn", "error"] and
        this = DataFlow::valueNode(call.getAnArgument())
      )
    }
  }

  /**
   * Configuration for finding sensitive data leaks.
   */
  class SensitiveDataLeakConfig extends TaintTracking::Configuration {
    SensitiveDataLeakConfig() {
      this = "SensitiveDataLeakConfig"
    }

    override predicate isSource(DataFlow::Node source) {
      source instanceof SensitiveData
    }

    override predicate isSink(DataFlow::Node sink) {
      sink instanceof LoggingSink
    }
  }
}
