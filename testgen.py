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
:Test:              (Expression, Expression)
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
__DEPTH_TARGET = 4
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

    >>> from ADTSpec import OperationSpec
    >>> oS1 = OperationSpec(("ID", "isEmpty"), [("ID", "StackInt")], "boolean")
    >>> eq1 = [([('ID', 'isEmpty'), [('ID', 'empty')]], ('boolean', True))]
    >>> pTD1 = { 'int': [(('int', 3), ('int', 3))], ('ID', 'StackInt'): [([('ID', 'empty')], [('ID', 'empty')])] }
    >>> nT1 = []
    >>> aTD1 = { "int": [], ('ID', 'StackInt'): [] }

    >>> list(generateExpressions(oS1, eq1, pTD1, nT1, aTD1))
    [([('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])], ('boolean', True))]

    >>> nT1
    [([('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])], ('boolean', True))]

    If the given OperationSpec takes no arguments, nothing is returned.

    >>> oS2 = OperationSpec(("ID", "empty"), [], ("ID", "StackInt"))
    >>> nT1 = []
    >>> list(generateExpressions(oS2, eq1, pTD1, nT1, aTD1))
    []
    >>> nT1
    []

    If there are no matching patterns, generated tests are added to the newTests list, but not to the result generator.
    
    >>> oS3 = OperationSpec(("ID", "push"), [("ID", "StackInt"), "int"], ("ID", "StackInt"))
    >>> nT1 = []
    >>> list(generateExpressions(oS3, eq1, pTD1, nT1, aTD1))
    []
    >>> nT1
    [([('ID', 'push'), ([('ID', 'empty')], [('ID', 'empty')]), (('int', 3), ('int', 3))], [('ID', 'push'), ([('ID', 'empty')], [('ID', 'empty')]), (('int', 3), ('int', 3))])]

    If there is a partial match, but not a complete match, then no test is generated. In the following example, the
    expression generated by the OperationSpec is (top (empty)), where (empty) is the only available StackInt to be
    placed in the expression.

    >>> oS4 = OperationSpec(("ID", "top"), [("ID", "StackInt")], "int")
    >>> eq2 = eq1 + [([("ID", "top"), [("ID", "push"), ("ID", "s"), ("ID", "k")]], ("ID", "k"))]
    >>> nT1 = []
    >>> list(generateExpressions(oS4, eq2, pTD1, nT1, aTD1))
    []
    >>> nT1
    []
    """
    addToResults = opSpec.output in ['int', 'string', 'character', 'boolean'] #Check to see if we will be generating a primitive type
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
                if testCase:
                    test = createTest(expr, equations)
                    if test is not False:
                        #If we were able to simplify, we keep the test.
                        newTests.append(test)
                        if addToResults:
                            yield test
                else:
                     newTests.append(double(expr))


def createTest(expr, equations, checkTimeout=True):
    """
    createTest : Expression [Equation...] -> Test

    :param expr: The Expression to attempt to create a test from.
    :param equations: Equations to be searched for matching rewrite patterns.
    :param checkTimeout: Flag which enables Timeout Error handling. Keep this set to True.
    :return: A generated Test, or False if one could not be generated.

    Match
    >>> exp1 = [('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])]
    >>> eqs = [([('ID', 'isEmpty'), [('ID', 'empty')]], ('boolean', True))]
    >>> createTest(exp1, eqs)
    ([('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])], ('boolean', True))

    No Equations
    >>> createTest(exp1, [])
    False

    No Match
    >>> eqs2 = [([('ID', 'top'), [('ID', 'push'), ('ID', 's'), ('ID', 'k')]], ('ID', 'k'))]
    >>> createTest(exp1, eqs2)
    False

    Partial Match
    >>> eqs3 = [([('ID', 'isEmpty'), [('ID', 'push'), ('ID', 's'), ('ID', 'k')]], ('boolean', False))]
    >>> createTest(exp1, eqs3)
    False

    Multiple Eqs
    >>> eqs4 = eqs3 + eqs2 + eqs
    >>> createTest(exp1, eqs4)
    ([('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])], ('boolean', True))

    Simplify to primitive
    >>> exp2 = [('ID', 'asInt'), ([('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])], [('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])])]
    >>> eqs5 = [([('ID', 'asInt'), [('ID', 'zero')]], ('int', 0)), ([('ID', 'asInt'), [('ID', 'succ'), ('ID', 'm')]], [('ID', '+'), ('int', 1), [('ID', 'asInt'), ('ID', 'm')]])]
    >>> createTest(exp2, eqs5)
    ([('ID', 'asInt'), ([('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])], [('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])])], ('int', 1))

    """
    for e in (eq for eq in equations if eq[0][0] == expr[0]):
        if checkTimeout:
            # If the timeout check is enabled, then setup our try/except.
            try:
                test = __createTestFromEquation(expr, e, equations)
            except RuntimeError:
                if e not in __exceptions:
                    sys.stderr.write(
                        "WARNING: Maximum recursion depth exceeded!" +
                        "\n\tEquation:\t" + sExprToString(e[0]) + ' = ' + sExprToString(e[1]) +
                        "\n\tExpression:\t" + toScheme(expr) + "\n" +
                        "Suppressing further warnings from this equation.\n"
                    )
                    __exceptions.append(e)
                test = False
        else:
            # Otherwise, just attempt to make the test
            test = __createTestFromEquation(expr, e, equations)
        if test is not False:
            return test
    return False


def __createTestFromEquation(expr, equation, equations):
    """
    __createTestFromEquation : Expression Equation [Equation...] -> Test

    :param expr: Expression to attempt to create a test from.
    :param equation: Equation to attempt to be matched and used to rewrite the given expression.
    :param equations: List of all equations, used to further simplify rewritten expressions and sub expressions.
    :return: A generated Test, or False if one could not be generated.

    Match
    >>> exp1 = [('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])]
    >>> eq = ([('ID', 'isEmpty'), [('ID', 'empty')]], ('boolean', True))
    >>> __createTestFromEquation(exp1, eq, [eq])
    ([('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])], ('boolean', True))

    No Equations
    >>> eqs2 = [([('ID', 'top'), [('ID', 'push'), ('ID', 's'), ('ID', 'k')]], ('ID', 'k'))]
    >>> __createTestFromEquation(exp1, eqs2, [])
    Traceback (most recent call last):
        ...
    ValueError: 'equations' argument cannot be empty

    No Match
    >>> eq2 = ([('ID', 'top'), [('ID', 'push'), ('ID', 's'), ('ID', 'k')]], ('ID', 'k'))
    >>> __createTestFromEquation(exp1, eq2, [eq2])
    False

    Partial Match
    >>> eq3 = ([('ID', 'isEmpty'), [('ID', 'push'), ('ID', 's'), ('ID', 'k')]], ('boolean', False))
    >>> __createTestFromEquation(exp1, eq3, [eq3])
    False
    """
    # Check to make sure this is sane
    if not len(equations):
        raise ValueError("'equations' argument cannot be empty")
    left, right = equation
    patternMap = patternMatch(left, expr) #Attempt to match
    if patternMap is not False:
        # Replace all pattern variables with those found in the match.
        rV = replaceVariables(right, patternMap, equations)
        if not isTestCase(rV, equations): #If we shouldn't be able to simplify
            return expr, rV
        else:
            # Attempt to simplify
            t = createTest(rV, equations, False)
            if t is not False: #Could simplify
                return expr, t[1]
    return False


def replaceVariables(expr, patternMap, equations):
    """
    replaceVariables : S-Expression PatternMap [Equation...] -> Expression

    :param expr: S-Expression where any pattern variables will be replaced.
    :param patternMap: PatternMap which will be used to look up replacements for pattern variables.
    :param equations: List of all equations, used to further simplify rewritten expressions and sub expressions.
    :return: A rewritten Expression.

    The expression is an Atom
    >>> expr = ('boolean', True)
    >>> patternMap = { }
    >>> eqs = [([('ID', 'isEmpty'), [('ID', 'empty')]], ('boolean', True))]
    >>> replaceVariables(expr, patternMap, eqs)
    ('boolean', True)

    The expression is a pattern variable
    >>> patternMap[('ID', 'k')] = ('int', 0)
    >>> expr2 = ('ID', 'k')
    >>> replaceVariables(expr2, patternMap, eqs)
    ('int', 0)

    The expression is not an Atom and simplifies to a primitive type
    >>> expr3 = [('ID', '+'), ('int', 1), [('ID', 'asInt'), ('ID', 'm')]]
    >>> eqs2 = [([('ID', 'asInt'), [('ID', 'zero')]], ('int', 0)), ([('ID', 'asInt'), [('ID', 'succ'), ('ID', 'm')]], [('ID', '+'), ('int', 1), [('ID', 'asInt'), ('ID', 'm')]])]
    >>> patternMap2 = {('ID', 'm'):  [('ID', 'zero')]}
    >>> replaceVariables(expr3, patternMap2, eqs2)
    ('int', 1)
    """
    if isPatternVariable(expr):
        return patternMap[expr] if expr in patternMap else expr
    elif isinstance(expr, list):
        op = expr[0]
        result = [op]
        for i in expr[1:]:
            r = replaceVariables(i, patternMap, equations)
            t = createTest(r, equations, False)
            if t is False:
                result.append(double(r))
            else:
                result.append(t)
        if op[1] in __primitiveOps:
            result = __primitiveOps[op[1]](*[x[1] for x in result[1:]])
        return result
    else:
        return expr


def __makeTuplePredicate(type):
    """
    __makeTuplePredicate : String -> (X -> boolean)

    :param type: String corresponding to the "tag" of an Atom.
    :return: Predicate which takes anything and returns if it's an Atom whose tag matches type.

    >>> isInt = __makeTuplePredicate('int')
    >>> isInt(("int", 2))
    True
    >>> isInt(("int", "notAnInteger"))
    True
    >>> isInt(("string", 2))
    False
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

    >>> pE = [('ID', 'isEmpty'), [('ID', 'empty')]]
    >>> tE = [('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])]
    >>> patternMatch(pE, tE)
    {}
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

    >>> pE = [('ID', 'isEmpty'), [('ID', 'empty')]]
    >>> tE = [('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])]
    >>> pM = {}
    >>> __patternMatchHelper(pE, tE, pM)
    True
    >>> pM
    {}
    >>> __patternMatchHelper(("int", 1), ("int", 1), pM)
    True
    >>> pM
    {}
    >>> __patternMatchHelper([('ID', 'isEmpty'), ('ID', 'k')], [('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])], pM)
    True
    >>> pM == { ("ID", "k"): [('ID', 'empty')] }
    True
    >>> pM = { }
    >>> __patternMatchHelper(('ID', 'k'),  [('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])], pM)
    True
    >>> pM == { ('ID', 'k'): [('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])] }
    True

    >>> __patternMatchHelper(pE, ("int", 1), pM)
    False
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
        if patternExpression in patternMap:
            return targetExpression == patternMap[patternExpression]
        else:
            patternMap[patternExpression] = targetExpression
            return True
    elif isinstance(patternExpression, list) and isinstance(targetExpression, list):
        if patternExpression[0] == targetExpression[0]:
            return __matchArguments(patternExpression[1:], targetExpression[1:], patternMap)
    else:
        return False


def __matchArguments(patterns, expressions, patternMap):
    """
    __matchArguments : [S-Expression...] [Test...] PatternMap -> boolean

    :param patterns: List of S-Expressions representing patterns to be matched.
    :param expressions: Expression which will attempt to be matched to the pattern at the corresponding index.
    :param patternMap: PatternMap which found pattern variables will be added to.
    :return: True if the match was completed successfully, False otherwise.

    >>> pats = [[('ID', 'empty')]]
    >>> exps = [([('ID', 'empty')], [('ID', 'empty')])]
    >>> pM = { }
    >>> __matchArguments(pats, exps, pM)
    True
    >>> pM
    {}

    >>> pats2 = [[('ID', 'zero')], [('ID', 'succ'), ('ID', 'm')]]
    >>> exps2 = [([('ID', 'zero')], [('ID', 'zero')]), ([('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])], [('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])])]
    >>> __matchArguments(pats2, exps2, pM)
    True
    >>> pM == {('ID', 'm'): [('ID', 'zero')]}
    True

    >>> pM = {}
    >>> pats2 = [[('ID', 'succ'), ('ID', 'm')], [('ID', 'zero')]]
    >>> exps2 = [([('ID', 'zero')], [('ID', 'zero')]), ([('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])], [('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])])]
    >>> __matchArguments(pats2, exps2, pM)
    False
    >>> pM
    {}
    """
    return len(patterns) == len(expressions) and all(
        __patternMatchHelper(pattern, eRight, patternMap)
        for (_, eRight), pattern in zip(expressions, patterns)
    )


def toScheme(expression, simplified=False):
    """
    toScheme : Expression boolean -> String

    :param expression: An Expression to be converted.
    :param simplified: If true, always take the right-hand side of sub-expressions. If false, take the left.
    :return: A string containing a valid scheme expression.

    Booleans
    >>> toScheme(('boolean', True))
    '#t'
    >>> toScheme(('boolean', False))
    '#f'

    Characters
    >>> toScheme(('character', "k"))
    '#\\\\k'

    Strings
    >>> toScheme(('string', ""))
    '""'
    >>> toScheme(('string', "foo"))
    '"foo"'

    Integers
    >>> toScheme(('int', 5))
    '5'

    Complex Expression
    >>> toScheme([('ID', 'isEmpty'), ([('ID', 'empty')], [('ID', 'empty')])])
    '(isEmpty (empty))'

    >>> expr = [('ID', 'plus'),([('ID', 'plus'),([('ID', 'succ'),([('ID', 'zero')],[('ID', 'zero')])],[('ID', 'succ'),([('ID', 'zero')],[('ID', 'zero')])]),([('ID', 'zero')],[('ID', 'zero')])], [('ID', 'succ'), ([('ID', 'zero')], [('ID', 'zero')])]), ([('ID', 'zero')], [('ID', 'zero')])]
    >>> toScheme(expr, False)
    '(plus (plus (succ (zero)) (zero)) (zero))'
    >>> toScheme(expr, True)
    '(plus (succ (zero)) (zero))'
    """
    if isinstance(expression, list):
        return '(' + " ".join(
            [toScheme(expression[0])] + [toScheme(s, simplified) if simplified else toScheme(e, simplified)
                                         for e, s in expression[1:]]
        ) + ')'
    else:
        tag, value = expression
        if tag == 'boolean':
            return '#t' if value else '#f'
        elif tag == 'character':
            return "#\\" + value
        elif tag == 'string':
            return '"' + value + '"'
        else:
            return str(value)


def generateBasicExpressions(grammar, exprDict):
    """
    generateBasicExpressions : Grammar TestDictionary -> None

    Generates expressions which require no arguments, and adds them to the expression dictionary.
    :param grammar: The grammar which will be searched for OperationSpecs that take no arguments.
    :param exprDict: An expression dictionary which generated expressions will be added to.

    No basic constructors available
    >>> exprDict = {}
    >>> generateBasicExpressions({}, exprDict)
    >>> exprDict
    {}
    >>> from ADTSpec import OperationSpec
    >>> grammar = { ("ID", "IntContainer"): [OperationSpec(("ID", "hold"), ["int"], ("ID", "IntContainer"))] }
    >>> generateBasicExpressions(grammar, exprDict)
    >>> exprDict
    {('ID', 'IntContainer'): []}
    >>> grammar[("ID", "StringContainer")] = [OperationSpec(("ID", "holdString"), ["string"], ("ID", "StringContainer"))]
    >>> exprDict = {}
    >>> generateBasicExpressions(grammar, exprDict)
    >>> exprDict == {('ID', 'IntContainer'): [], ('ID', 'StringContainer'): []}
    True

    Basic constructor available
    >>> exprDict = {}
    >>> grammar2 = { ("ID", "StackInt"): [OperationSpec(("ID", "empty"), [], ("ID", "StackInt"))] }
    >>> generateBasicExpressions(grammar2, exprDict)
    >>> exprDict == { ("ID", "StackInt"): [([("ID", "empty")], [("ID", "empty")])] }
    True
    >>> exprDict = {}
    >>> grammar2[("ID", "StackInt")].append(OperationSpec(("ID", "empty"), [("ID", "StackInt"), "int"], ("ID", "StackInt")))
    >>> generateBasicExpressions(grammar2, exprDict)
    >>> exprDict == { ("ID", "StackInt"): [([("ID", "empty")], [("ID", "empty")])] }
    True
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

    >>> expr = [("ID", "empty")]
    >>> equations = [([("ID", "isEmpty"), [("ID", "empty")]], ("boolean", True))]
    >>> isTestCase(expr, equations)
    False
    >>> expr2 = [("ID", "isEmpty")]
    >>> isTestCase(expr2, equations)
    True
    >>> expr3 = [("ID", "isEmpty"), ([("ID", "empty")], [("ID", "empty")])]
    >>> isTestCase(expr3, equations)
    True
    >>> equations2 = [([("ID", "top"), [("ID", "push"), ("ID", "s"), ("ID", "k")]], ("ID", "k"))] + equations
    >>> isTestCase(expr3, equations)
    True

    Atom
    >>> expr4 = ("boolean", True)
    >>> isTestCase(expr, equations)
    False
    """
    return isinstance(expr, list) and any(expr[0] == left[0] for left, _ in equations)


double = lambda x: (x, x)


def sExprToString(sExpr):
    """
    sExprToString : S-Expression -> String

    :param sExpr: S-Expression to be converted
    :return: String representation of the given S-Expression

    Atom
    >>> sExprToString(("boolean", True))
    '#t'
    >>> sExprToString(("boolean", False))
    '#f'
    >>> sExprToString(("character", 'a'))
    '#\\\\a'
    >>> sExprToString(("string", 'foo'))
    '"foo"'
    >>> sExprToString(("int", 1))
    '1'

    List
    >>> sExprToString([("ID", "empty")])
    '(empty)'
    >>> sExprToString([("ID", "push"), [("ID", "empty")], ("int", 1)])
    '(push (empty) 1)'
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


def testToString(test, depth, index, outType):
    """
    testToString : Test int int Identifier -> String

    :param test: Test to be converted to a String.
    :param depth: Depth of the expression, used to name the test.
    :param index: Index of this test, used to name the test.
    :param outType: Output type of the expressions in the test. Used to select an appropriate equality procedure.
    :return: A String containing a valid Scheme test.

    >>> empty = [("ID", "empty")]
    >>> test = (empty, ('boolean', True))
    >>> testToString(test, 1, 0, 'boolean')
    '(test "empty1-0" (empty))'
    >>> test1 = (empty, ('boolean', False))
    >>> testToString(test1, 1, 0, 'boolean')
    '(test "empty1-0" (not (empty)))'
    >>> one = ("int", 1)
    >>> push = [("ID", "push"), (empty, empty), (one, one)]
    >>> test2 = ([("ID", "top"), (push, push)], one)
    >>> testToString(test2, 3, 100, 'int')
    '(test "top3-100" (= 1 (top (push (empty) 1))))'
    >>> string = ("string", "a")
    >>> push2 = [("ID", "push"), (empty, empty), (string, string)]
    >>> test3 = ([("ID", "top"), (push2, push2)], string)
    >>> testToString(test3, 3, -4, 'string')
    '(test "top3--4" (string=? "a" (top (push (empty) "a"))))'
    >>> testToString(test3, 50, 1000, 'character')
    '(test "top50-1000" (char=? "a" (top (push (empty) "a"))))'
    >>> testToString(test3, 0, 0, "boolean")
    '(test "top0-0" (boolean=? "a" (top (push (empty) "a"))))'
    """
    left, right = test
    name = left[0][1]
    if outType == 'boolean':
        if isBoolean(right):
            if right[1]:
                return '(test "' + name + str(depth) + '-' + str(index) + '" ' + toScheme(left) + ')'
            return '(test "' + name + str(depth) + '-' + str(index) + '" (not ' + toScheme(left) + '))'
        return ('(test "' + name + str(depth) + '-' + str(index) +
                '" (boolean=? ' + toScheme(right, True) + ' ' + toScheme(left) + '))')
    elif outType == 'character':
        return ('(test "' + name + str(depth) + '-' + str(index) +
                '" (char=? ' + toScheme(right, True) + ' ' + toScheme(left) + '))')
    elif outType == 'string':
        return ('(test "' + name + str(depth) + '-' + str(index) +
                '" (string=? ' + toScheme(right, True) + ' ' + toScheme(left) + '))')
    else:
        return ('(test "' + name + str(depth) + '-' + str(index) +
               '" (= ' + toScheme(right, True) + ' ' + toScheme(left) + '))')


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
        sys.stderr.write("ERROR: No equations have been provided.\n")
        return False
    checkValidEquations(spec.equations, spec.getAllOpSpecs())
    return True


def generateTestsFromSpec(spec, numPrimitive=__NUM_PRIMITIVES, targetAmt=__MIN_EXPRESSIONS, targetDepth=__DEPTH_TARGET):
    # Dictionary containing all generated expressions which have not been used as arguments.
    exprDict = {
        'int':       list(set([double(('int', valuegenerators.generate_integer()))    for _ in range(0, numPrimitive)])),
        'string':    list(set([double(('string', valuegenerators.generate_string()))  for _ in range(0, numPrimitive)])),
        'character': list(set([double(('character', valuegenerators.generate_char())) for _ in range(0, numPrimitive)])),
        'boolean':   list(set([double(('boolean', valuegenerators.generate_bool()))   for _ in range(0, numPrimitive)]))
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
        print "Usage: ./cs4500 input-file output-file"
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
