
test "charr1"
    var c = [7, 6, 5, 4, 3]
    c[end+1] = 8
    check all(c == [7, 6, 5, 4, 3, 8])
    check not has(c, value = 65)
end test

test "chdict1"
    var d = []
    d["test"] = 54
    d["whack"] = 77
    check d["whack"] == 77
    check not has(d, key = "whac")
    check not has(d, value = 65)
end test

test "chstr"
    var s = "The wuick brown dox"
    print(len(s))
    var g = s[6:13]
    check g == "uick bro"
    #free g
    var ss = split(s, sep = " ")
    print(len(ss))
    check ss[3] == "brown"
    #free ss
    var m = replace(s, what = "wuick", with = "bhingora")
    check m == "The bhingora brown dox"
    #free m
    var t = replace(s, map = ["wuick" = "quick", "dox" = "fox"])
    check t == "The quick brown fox" #1322
    #free t
    #free s
end test

test "chstrint"
    var m = "The quick"
    var j = "brown fox"
    var u = "jumps over"
    var ret = "{{m}} {{j}} {{u}} the lazy dog."
    print(ret)
end test

test "chtermcolor"
    print("The {r}quick{-} brown fox {u}jumps{-} over the {g}lazy{-} dog.")
end test

test "matliteral"
    var m = [
        1, 2, 3;
        4, 5, 6;
        7, 8, 9
    ]
    var c = [1, 2, 3; 4, 5, 6; 7, 8, 9]
    check all(m == c)
    m[:] = random()
    describe(m)
end test
