# Define some auxillary functions

def features(wordline):
    vector = []
    if "ie" in wordline[0]: loc = wordline[0].index("ie")
    else: loc = wordline[0].index("ei")
    # pronounced as one syllable
    vector = vector + [wordline[1][loc] == '-' or wordline[1][loc+1] == '-']
    # silent
    vector = vector + [wordline[1][loc] == '-' and wordline[1][loc+1] == '-']
    # two syllables
    vector = vector + [wordline[1][loc] != '-' and wordline[1][loc+1] != '-']
    # pronunciation
    #    for pro in ["--", "-I", "A-", "E-", "I-", "e-", "ei", "i-", "iA", "iI", "ix", "x-", "-E",
    #                "-a", "-e", "-x", "-y", "AE", "Ai", "Ax", "Ix", "Y-", "iE", "ii", "y-", "yE",
    #                "yI", "ye", "yx"]:
    # two syllable pronunciation
    for pro in ["ei", "iA", "iI", "ix", "AE", "Ai", "Ax", "Ix", "iE", "ii", "yE", "yI", "ye", "yx"]:
        vector = vector + [wordline[1][loc:(loc+2)] == pro]
    # two syllable pronunciation
    for pro in ["I", "A", "E", "e", "i", "x", "a", "y", "Y"]:
        vector = vector + [wordline[1][loc:(loc+2)] == pro+"-" or wordline[1][loc:(loc+2)] == "-"+pro]
    for let in "abcdefghijklmnopqrstuvwxyz":
        # immediate preceeding, before
        if loc > 0: vector = vector + [wordline[0][loc-1] == let]
        else: vector = vector + [False]
        vector = vector + [let in wordline[0][0:loc]]
        # immediate following, after
        if loc < len(wordline[0])-2: vector = vector + [wordline[0][loc+2] == let]
        else: vector = vector + [False]
        vector = vector + [let in wordline[0][(loc+2):]]
        # in word at all
        vector = vector + [let in wordline[0][(loc+2):]]
    return(vector)

def featurenames():
    vector = []
    # pronounced as one syllable
    vector = vector + ["one syllable?", "silent?", "two syllables?"]
    # pronunciation
    for pro in ["ei", "iA", "iI", "ix", "AE", "Ai", "Ax", "Ix", "iE", "ii", "yE", "yI", "ye", "yx"]:
        vector = vector + ["sounds like " + pro + "?"]
    # two syllable pronunciation
    for pro in ["I", "A", "E", "e", "i", "x", "a", "y", "Y"]:
        vector = vector + ["sounds like " + pro + "?"]
    for let in "abcdefghijklmnopqrstuvwxyz":
        # immediate preceeding, before
        vector = vector + ["immediately after " + let + "?", "after " + let + "?"]
        # immediate following, after
        vector = vector + ["immediately before " + let + "?", "before " + let + "?"]
        # in word at all
        vector = vector + ["word contains " + let + "?"]
    return(vector)
