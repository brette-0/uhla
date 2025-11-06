import tomllib

class Memory:
    def __init__(self, offset : int, width : int):
        Offset = offset
        Width = width
        return

    Offset : int
    Width  : int



def __main__():
    toml : dict = tomllib.loads("./Test/link.toml")

    if not toml.__contains__("memory"):
        raise KeyError("Linker Script Requires Memory Region")

    if not toml.__contains__("segments"):
        raise KeyError("Linker Script Requires Segments Region")

    if not toml.__contains__("rules"):
        raise KeyError("Linker Script Requires Rules Region")

    regions  : dict = toml["memory"]
    segments : dict = toml["segments"]
    rules    : dict = toml["rules"]

    globalOffset : int

    for segment in segments:
        globalOffset = regions[segment["region"]]["start"]
        
        pass

    return



if __name__ == "__main__": __main__()