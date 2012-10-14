__author__ = 'Stephen Elliott, Kyle Billemeyer, Nicholas Bagley, John Amicangelo'


def all_contained(seq1, seq2):
    """
    Determines if all elements in seq1 are in seq2, and vice versa.

    >>> all_contained([], [])
    True
    >>> all_contained([], [1])
    False
    >>> all_contained([1], [])
    False
    >>> all_contained([1], [1])
    True
    >>> all_contained(xrange(0, 10), range(0, 10))
    True
    """
    return len(list(seq1)) == len(list(seq2)) and all(x in seq2 and y in seq1 for x, y in zip(seq1, seq2))


def _getAllOpSpecsOfReturnType(sigs, type):
    """
    Helper Function
    Given a sequence of Signatures and a target type, returns a list of OperationSpecs
    whose output matches the target type.

    >>> oS1 = OperationSpec("empty", [], "StackInt")
    >>> oS2 = OperationSpec("push", ["StackInt", "int"], "StackInt")
    >>> sig1 = Signature("StackInt", [oS1, oS2])
    >>> sig2 = Signature("StackInt", [oS2, oS1])

    >>> ospecs = _getAllOpSpecsOfReturnType([sig1, sig2], "StackInt")
    >>> target = [oS1, oS2] * 2
    >>> all_contained(ospecs, target)
    True

    >>> _getAllOpSpecsOfReturnType([sig1, sig2], "Whatever")
    []

    >>> _getAllOpSpecsOfReturnType([], "StackInt")
    []
    """
    def f(a, sig):
        r = [o for o in sig.operationSpecs if o.output == type]
        r.extend(a)
        return r
    return reduce(f, sigs, [])


def _getAllReturnValues(sigs):
    """
    Helper Function
    Given a sequence of Signatures, return a list of unique return types (Strings) found
    in the Signatures.

    >>> oS1 = OperationSpec("empty", [], "StackInt")
    >>> oS2 = OperationSpec("top", ["StackInt"], "int")
    >>> sig1 = Signature("StackInt", [oS1, oS2])
    >>> sig2 = Signature("StackInt", [oS2, oS1])

    >>> all_contained(_getAllReturnValues([sig1, sig2]), ["StackInt", "int"])
    True

    >>> _getAllReturnValues([])
    []

    >>> all_contained(_getAllReturnValues([sig1]), ["StackInt", "int"])
    True
    """
    def f(a, sig):
        r = [o.output for o in sig.operationSpecs]
        r.extend(a)
        return r
    return list(set(reduce(f, sigs, [])))


class Spec(object):
    def __init__(self, sigs, equations):
        self.signatures = sigs
        self.equations = equations

    def __str__(self):
        return "\n\n".join(map(str, self.signatures))  + "\n\n" + str(self.equations)

    def __repr__(self):
        return "Spec(" + repr(self.signatures) + ", " + repr(self.equations) + ")"

    def __eq__(self, other):
        return all_contained(self.signatures, other.signatures) and all_contained(self.equations, other.equations)

    def __hash__(self):
        return sum(map(hash, self.signatures)) + sum(map(hash, self.equations))

    def getGrammar(self):
        """
        :return: A Grammar Dictionary, mapping typenames to OperationSpecs whose output type matches the typename.

        >>> opStackInt = OperationSpec("empty", [], "StackInt")
        >>> opConsInt = OperationSpec("toList", ["StackInt"], "ConsInt")
        >>> s = Spec([Signature("StackInt", [opStackInt])], [])
        >>> s.getGrammar() == {"StackInt": [OperationSpec("empty", [], "StackInt")]}
        True

        >>> s.signatures.append(Signature("ConsInt", [opConsInt]))
        >>> s.getGrammar() == {"StackInt": [opStackInt], "ConsInt": [opConsInt]}
        True
        """
        return dict((type, _getAllOpSpecsOfReturnType(self.signatures, type))
                    for type in _getAllReturnValues(self.signatures))

    def getAllOpSpecs(self):
        return reduce(lambda x, y: x + y.operationSpecs, self.signatures, [])


class Signature(object):
    def __init__(self, name, opSpecs):
        self.name = name
        self.operationSpecs = opSpecs

    def __str__(self):
        return str(self.name) + ":\n\t" + "\n\t".join([str(o) for o in self.operationSpecs])

    def __repr__(self):
        return "Signature(" + repr(self.name) + ", " + repr(self.operationSpecs) + ")"

    def __eq__(self, other):
        """
        Signatures are equal when both :name: fields match and both operationSpecs sequences contain the same elements.

        >>> oS1 = OperationSpec("empty", [], "StackInt")
        >>> oS2 = OperationSpec("push", ["StackInt", "int"], "StackInt")
        >>> sig1 = Signature("StackInt", [oS1, oS2])
        >>> sig2 = Signature("StackInt", [oS2, oS1])
       
        >>> sig1 == sig2
        True

        As expected, if both objects are equal, then so are their hashes.

        >>> hash(sig1) == hash(sig2)
        True

        If the names don't match, or the sequences contain different elements, then the Signatures are not equal.

        >>> sig3 = Signature("ConsInt", [oS1, oS2])
        >>> sig3 == sig1
        False
        >>> sig4 = Signature("StackInt", [oS1])
        >>> sig4 == sig1
        False
        >>> sig5 = Signature("StackInt", [OperationSpec("pop", ["StackInt"], "StackInt")])
        >>> sig5 == sig1
        False
        """
        return self.name == other.name and len(self.operationSpecs) == len(other.operationSpecs) and all(
            x in other.operationSpecs and y in self.operationSpecs
            for x, y in zip(self.operationSpecs, other.operationSpecs)
        )

    def __hash__(self):
        return hash(self.name) * sum(map(hash, self.operationSpecs))


class OperationSpec(object):
    def __init__(self, name, inputs, output):
        self.name = name
        self.inputs = inputs
        self.output = output

    def __str__(self):
        return str(self.name) + ' : ' + " * ".join([str(i) for i in self.inputs]) + ' -> ' + str(self.output)

    def __repr__(self):
        return "OperationSpec(" + repr(self.name) + ", " + repr(self.inputs) + ", " + repr(self.output) + ")"

    def __eq__(self, other):
        return self.name == other.name and self.inputs == other.inputs and self.output == other.output

    def __hash__(self):
        return hash(self.name) * sum(hash(inp) * 13 * i for i, inp in enumerate(self.inputs)) * hash(self.output)



