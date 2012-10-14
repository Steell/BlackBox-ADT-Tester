import ADTSpecScanner
import sys
import itertools
import valuegenerators

__doc__ = 'reStructuredText'
__author__ = 'Stephen Elliott, Kyle Billemeyer, Nicholas Bagley, John Amicangelo'

"""
=-=-=-=-=-=-=-=-=
Type Definitions:
=-=-=-=-=-=-=-=-=
-Key-
  () = tuple
  {} = dictionary
  [] = list
  ... = repeat type in list indefinitely

:Equation:          (S-Expression, S-Expression)
:S-Expression:      Atom | [S-Expression...]
:Grammar:           {Type: [OperationSpec...]}
:PatternMap:        {PatternVariable: Expression}
:Expression:        Atom | [Identifier] + [Test...]
:Test:              (Expression, [Expression...])
:TestDictionary:    {Type: [Test...]}
:Type:              Identifier
:Identifier:        ('ID', string)
:Atom:              AInteger | ABoolean | ACharacter | AString
:AInteger:          ('int', int)
:ABoolean:          ('boolean', boolean)
:ACharacter:        ('character', char)
:AString:           ('string', string)
"""

## Constants
__MIN_EXPRESSIONS = 25
__DEPTH_TARGET = 3
__NUM_PRIMITIVES = 3


# Dictionary containing all primitive operations.
__primitiveOps = {
    'not': lambda boolean: ('boolean', not boolean[1]),
    '+': lambda int1, int2, *args: ('int', int1[1] + int2[1] + sum(arg[1] for arg in args)),
    '-': lambda int1, int2, *args: ('int', int1[1] - (int2[1] + sum(arg[1] for arg in args))),
    '*': lambda int1, int2, *args: ('int', int1[1] * int2[1] + reduce(lambda x, y: x*y, args, 1)),
    '<': lambda bool1, bool2: ('boolean', bool1[1] < bool2[1]),
    '>': lambda bool1, bool2: ('boolean', bool1[1] > bool2[1]),
    '=': lambda bool1, bool2, *args: ('boolean', bool1[1] == bool2[1] and all(bool1[1] == arg[1] for arg in args))
}

# List holding all Equations which have caused a rewrite timeout. Used to prevent the error from being printed more
# than once for any Equation.
__exceptions = []


def generateExpressions(opSpec, equations, prevTestDictionary, newTests, allTestDictionary):
    """
    generateExpressions : OperationSpec Grammar [Equation...] TestDictionary [Test...] TestDictionary -> ((Test...))

    :param opSpec: The OperationSpec that tests will be generated for.
    :param equations: The list of equations to be used to generate tests.
    :param prevTestDictionary: Dictionary mapping types to already generated tests. The expressions used in the left-side of these tests have never been used as a sub-expression.
    :param newTests: List that all new tests will be added to.
    :param allTestDictionary: Dictionary mapping types to already generated tests. The expressions used in the left-side of these tests have been used already as sub-expressions.
    :return: A generator that produces tests where the expression simplifies to a primitive value
    """
    addToResults = opSpec.output in ['int', 'string', 'char', 'boolean'] #Check to see if we will be generating a primitive type
    testCase = any(opSpec.name == left[0] for left, _ in equations) #Check to see if there are any equations defined for this OpSpec
    if len(opSpec.inputs) and all(i in prevTestDictionary for i in opSpec.inputs) and all(i in allTestDictionary for i in opSpec.inputs):
        # For each OperationSpec that takes at least one arguments and has at least one available expression for each
        # argument...
        for inLists in (
            l for l in itertools.product(*[(prevTestDictionary[i], allTestDictionary[i]) for i in opSpec.inputs])
            if l != tuple([allTestDictionary[i] for i in opSpec.inputs])
        ):
            # For each possible combination of used and unused sub expressions, excluding the case where they have
            # all been used (to prevent repeats)...
            for expr in ([opSpec.name] + list(args) for args in itertools.product(*inLists)):
                # For each new expression generated from the inputs...
                test = createTest(expr, equations) if testCase else double(expr)
                if test is not False:
                    #If we were able to simplify, we keep the test.
                    newTests.append(test)
                    if addToResults:
                        yield test


def createTest(expr, equations, checkTimeout=True):
    """
    createTest : Expression [Equation...] -> Test

    :param expr: The Expression to attempt to create a test from.
    :param equations: Equations to be searched for matching rewrite patterns.
    :param checkTimeout: Flag which enables Timeout Error handling. Keep this set to True.
    :return: A generated Test, or False if one could not be generated.


    expr = [("ID", "pred"), ([("ID", "pred"), ([("ID", "zero")], [[("ID", "zero")]])], [[("ID", "zero")], []])]
    """
    result = []
    for e in (eq for eq in equations if eq[0][0] == expr[0]):
        if checkTimeout:
            # If the timeout check is enabled, then setup our try/except.
            try:
                new = __rewriteExpression(expr, e, equations)
            except RuntimeError:
                if e not in __exceptions:
                    sys.stderr.write(
                        "WARNING: Maximum recursion depth exceeded!" +
                        "\n\tEquation:\t" + sExprToString(e[0]) + ' = ' + sExprToString(e[1]) +
                        "Suppressing further warnings from this equation.\n"
                    )
                    __exceptions.append(e)
                new = False
        else:
            # Otherwise, just attempt to make the test
            new = __rewriteExpression(expr, e, equations)
        if new is not False:
#            return expr, new
            result.extend(new)
    return (expr, result) if result else False


def __rewriteExpression(expr, equation, equations):
    """
    __rewriteExpression : Expression Equation [Equation...] -> [Expression...]

    :param expr: Expression to attempt to rewrite.
    :param equation: Equation to attempt to be matched and used to rewrite the given expression.
    :param equations: List of all equations, used to further simplify rewritten expressions and sub expressions.
    :return: The rewritten expression, or False if one could not be generated.
    """
    # Check to make sure this is sane
    if not len(equations):
        raise ValueError("'equations' argument cannot be empty")
    left, right = equation
    patternMap = patternMatch(left, expr) #Attempt to match
    if patternMap is not False:
        # Replace all pattern variables with those found in the match.
        rV = replaceVariables(right, patternMap, equations)
        result = []
        for r in rV:
            if not isTestCase(r, equations): #If we shouldn't be able to simplify
                result.append(r)
            else:
                # Attempt to simplify
                t = createTest(r, equations, False)
                if t is not False: #Could simplify
                    result.extend(t[1])
        return result if result else False
    return False


def replaceVariables(expr, patternMap, equations):
    """
    replaceVariables : S-Expression PatternMap [Equation...] -> [Expression...]

    :param expr: S-Expression where any pattern variables will be replaced.
    :param patternMap: PatternMap which will be used to look up replacements for pattern variables.
    :param equations: List of all equations, used to further simplify rewritten expressions and sub expressions.
    :return: A rewritten Expression.
    """
    if isPatternVariable(expr):
        return patternMap[expr] if expr in patternMap else expr
    elif isinstance(expr, list):
        op = expr[0]
        result = []
        for i in expr[1:]:
            rewritten = replaceVariables(i, patternMap, equations) #r is a list of expressions
            subResult = []
            for r in rewritten:
                t = createTest(r, equations, False)
                if t is False:
                    subResult.append(double(r))
                else:
                    subResult.append(t)
            result.append(subResult)
        return [[op] + list(x) for x in itertools.product(*result)]
    else:
        return [expr]


def __makeTuplePredicate(type):
    """
    __makeTuplePredicate : String -> (X -> boolean)

    :param type: String corresponding to the "tag" of an Atom.
    :return: Predicate which takes anything and returns if it's an Atom whose tag matches type.
    """
    return lambda x: isinstance(x, tuple) and x[0] == type

isInteger = __makeTuplePredicate('int')
isString = __makeTuplePredicate('string')
isCharacter = __makeTuplePredicate('character')
isBoolean = __makeTuplePredicate('boolean')
isPatternVariable = __makeTuplePredicate('ID')


def patternMatch(patternExpr, targetExpr):
    """
    patternMatch : S-Expression Expression -> PatternMap

    :param patternExpr: S-Expression representing a pattern to be matched.
    :param targetExpr: Expression which will attempt to be matched to the pattern
    :return: A PatternMap mapping all pattern variables found in patternExpr to all sub-expressions found in targetExpr, or False if there is no match.
    """
    patternMap = {}
    return patternMap if __patternMatchHelper(patternExpr, targetExpr, patternMap) else False


def __patternMatchHelper(patternExpression, targetExpression, patternMap):
    """
    __patternMatchHelper : S-Expression Expression PatternMap -> boolean

    :param patternExpression: S-Expression representing a pattern to be matched.
    :param targetExpression: Expression which will attempt to be matched to the pattern
    :param patternMap: PatternMap which found pattern variables will be added to.
    :return: True if the match was completed successfully, False otherwise.
    """
    if isInteger(patternExpression):
        return isInteger(targetExpression) and patternExpression == targetExpression
    elif isBoolean(patternExpression):
        return isBoolean(targetExpression) and patternExpression == targetExpression
    elif isCharacter(patternExpression):
        return isCharacter(targetExpression) and patternExpression == targetExpression
    elif isString(patternExpression):
        return isString(targetExpression) and patternExpression == targetExpression
    elif isPatternVariable(patternExpression):
        if patternExpression not in patternMap:
            patternMap[patternExpression] = []
        patternMap[patternExpression].append(targetExpression)
        return True
    elif isinstance(patternExpression, list) and isinstance(targetExpression, list):
        if patternExpression[0] == targetExpression[0]:
            return matchArguments(patternExpression[1:], targetExpression[1:], patternMap)
    else:
        return False


def matchArguments(patterns, expressions, patternMap):
    """
    matchArguments : [S-Expression...] [Test...] PatternMap -> boolean

    :param patterns: List of S-Expressions representing patterns to be matched.
    :param expressions: Expression which will attempt to be matched to the pattern at the corresponding index.
    :param patternMap: PatternMap which found pattern variables will be added to.
    :return: True if the match was completed successfully, False otherwise.
    """
    return len(patterns) == len(expressions) and all(
        any([__patternMatchHelper(pattern, e, patternMap) for e in eRight])
        for (_, eRight), pattern in zip(expressions, patterns)
    )


def toScheme(expression, simplified=False):
    """
    toScheme : Expression boolean -> [String...]

    :param expression: An Expression to be converted.
    :param simplified: If true, always take the right-hand side of sub-expressions. If false, take the left.
    :return: A string containing a valid scheme expression.
    """
    if isinstance(expression, list):
        if simplified:
            return ['(' + " ".join([toScheme(expression[0])][0] + list(argList)) + ')'
                    for argList in itertools.product(
                        *[reduce(extend, (toScheme(x, simplified) for x in s), []) for _, s in expression[1:]]
                    )]
        return ['(' + " ".join([toScheme(expression[0])][0] + list(argList)) + ')'
                    for argList in itertools.product(
                        *[toScheme(e, simplified) for e, s in expression[1:]]
                    )]
    else:
        tag, value = expression
        if tag == 'boolean':
            return ['#t'] if value else ['#f']
        elif tag == 'character':
            return ["#\\" + value]
        elif tag == 'string':
            return ['"' + value + '"']
        else:
            return [str(value)]


def generateBasicExpressions(grammar, exprDict):
    """
    generateBasicExpressions : Grammar TestDictionary -> None

    Generates expressions which require no arguments, and adds them to the expression dictionary.
    :param grammar: The grammar which will be searched for OperationSpecs that take no arguments.
    :param exprDict: An expression dictionary which generated expressions will be added to.
    """
    for type, opSpecs in ((type, opSpecs) for type, opSpecs in grammar.iteritems() if type not in exprDict):
        # For each type and list of OperationSpec in the grammar (key, value pair) where the type is not already in the
        # dictionary...
        exprDict[type] = [double([o.name]) for o in opSpecs if not len(o.inputs)]


def isTestCase(expr, equations):
    """
    isTestCase : Expression [Equations...] -> boolean

    :param expr: Expression to check to see if it is a test case.
    :param equations: List of equations to check against.
    :return: True if it is a test case, False otherwise.
    """
    return isinstance(expr, list) and any(expr[0] == left[0] for left, _ in equations)


double = lambda x: (x, [x])


def sExprToString(sExpr):
    """
    sExprToString : S-Expression -> String

    :param sExpr: S-Expression to be converted
    :return: String representation of the given S-Expression
    """
    if isinstance(sExpr, list):
        return '(' + " ".join([sExprToString(s) for s in sExpr]) + ')'
    else:
        tag, value = sExpr
        if tag == 'boolean':
            return '#t' if value else '#f'
        elif tag == 'character':
            return "#\\" + value
        elif tag == 'string':
            return '"' + value + '"'
        else:
            return str(value)


def extend(x, a):
    x.extend(a)
    return x


__primEquality = {
    "boolean": 'boolean=?',
    "character": 'char=?',
    "string": 'string=?',
    "int": '='
}


def testToString(test, depth, index, outType):
    """
    testToString : Test int int Identifier -> [String...]

    :param test: Test to be converted to a String.
    :param depth: Depth of the expression, used to name the test.
    :param index: Index of this test, used to name the test.
    :param outType: Output type of the expressions in the test. Used to select an appropriate equality procedure.
    :return: A String containing a valid Scheme test.
    """
    left, right = test
    name = left[0][1]
    return ('(test "' + name + str(depth) + '-' + str(index) +
        '" (ormap (lambda (x) (' + __primEquality[outType] + ' x ' + toScheme(left)[0] + ')) (list ' +
        ' '.join(reduce(extend, (toScheme(r, True) for r in right), [])) + ')))')


def checkValidEquations(equations, opSpecs):
    """
    checkValidEquations : [Equation...] [OperationSpec...] -> None

    :param equations: List of equations to check for undefined procedures.
    :param opSpecs: List of defined procedures (represented as Operation Specs).

    Checks to make sure all procedures used in the given equations have been defined. Prints a warning to stderr
    if any undefined procedures are found.
    """
    def checkValidExpression(expression):
        if isinstance(expression, list):
            if expression[0] in [o.name for o in opSpecs] or expression[0][1] in __primitiveOps.iterkeys():
                return all(checkValidExpression(x) for x in expression[1:])
            else:
                sys.stderr.write('WARNING: Procedure "' + expression[0][1] +
                                 '" was found in equations but is not a defined operation.\n')
    for left, right in equations:
        checkValidExpression(left)
        checkValidExpression(right)


def runSanityCheck(spec):
    if not any(o.output in ['int', 'string', 'character', 'boolean'] for o in spec.getAllOpSpecs()):
        sys.stderr.write("ERROR: No provided operations return primitive types. Cannot generate tests.\n")
        return False
    elif not len(spec.equations):
        sys.stderr.write("WARNING: No equations have been provided. Generated tests may be meaningless.\n")
    checkValidEquations(spec.equations, spec.getAllOpSpecs())
    return True


def generateTestsFromSpec(spec, numPrimitive=__NUM_PRIMITIVES, targetAmt=__MIN_EXPRESSIONS, targetDepth=__DEPTH_TARGET):
    # Dictionary containing all generated expressions which have not been used as arguments.
    exprDict = {
        'int':       [double(('int', valuegenerators.generate_integer()))    for _ in range(0, numPrimitive)],
        'string':    [double(('string', valuegenerators.generate_string()))  for _ in range(0, numPrimitive)],
        'character': [double(('character', valuegenerators.generate_char())) for _ in range(0, numPrimitive)],
        'boolean':   [double(('boolean', valuegenerators.generate_bool()))   for _ in range(0, numPrimitive)]
    }

    # Populate TestDictionary with basic expressions.
    generateBasicExpressions(spec.getGrammar(), exprDict)

    # Dictionary containing all generated expressions which have been used as arguments.
    totalDict = dict((key, []) for key in exprDict.iterkeys())

    total = 0 #Total amount of expressions generated.
    depth = 1 #The recursive depth of this iteration of expressions.
    opSpecs = spec.getAllOpSpecs()

    while total < targetAmt or depth < targetDepth:
        # While the total amount of generated expressions is less than the threshold...
        depth += 1

        # Dictionary generated values will be copied into on this iteration.
        newDict = dict((key, []) for key in exprDict.iterkeys())

        for opSpec in opSpecs:
            # For each Type, [OperationSpec...] pair in the grammar...

            # Generate expressions for this type.
            output = generateExpressions(opSpec, spec.equations, exprDict, newDict[opSpec.output], totalDict)

            for i, t in enumerate(output):
                total += 1
                yield testToString(t, depth, i, opSpec.output)

        for key, values in exprDict.iteritems():
            totalDict[key].extend(values)

        exprDict = newDict #Iteration over, update the reference to previously generated
                           #expressions.


def __main():
    numArgs = len(sys.argv)
    if numArgs < 2:
        print "Usage: ./cs4500-ND input-file output-file"
        return
    
    # Parse the spec located at the file given as the first argument to the script.
    with open(sys.argv[1]) as input:
        spec = ADTSpecScanner.parseText('input', input.read())

    # Make sure our input spec is sane
    if not runSanityCheck(spec): return

    # Write expressions to the file specified by the second argument to the script.
    with open(sys.argv[2], 'wb') as outfile:
        outfile.write(
            "(import (rnrs base)\n\t(rnrs exceptions)\n\t(rnrs io simple)\n" +
            "\n".join("\t(testing " + sig.name[1] + ')' for sig in spec.signatures) + ')\n\n' +
            "(define tests-run 0)\n(define tests-passed 0)\n(define tests-failed 0)\n\n" +
            """(define-syntax test
  (syntax-rules ()
    ((_ name expr)
     (begin (set! tests-run (+ tests-run 1))
            (if (guard (exn (else #f))
                  expr)
                (set! tests-passed (+ tests-passed 1))
                (begin (set! tests-failed (+ tests-failed 1))
                       (display "Failed test: ")
                       (display name)
                       (newline)))))))

(define (ormap f xs)
    (cond ((null? xs) #f)
          ((f (car xs)) #t)
          (else (ormap f (cdr xs)))))

"""
        )

        i = 0
        for i, test in enumerate(generateTestsFromSpec(spec)):
            outfile.write(test + '\n')

        outfile.write(
            '\n(display "SUMMARY: failed ")\n(display tests-failed)\n(display " of ")\n' +
            '(display tests-run)\n(display " tests.")\n(newline)'
        )

    print "Successfully generated " + str(i+1) + " tests."


if __name__ == '__main__':
    __main()
