namespace csharp;

public class UnitTest1
{
    [Fact]
    public void TestPart1()
    {
        Assert.Equal(2, Part1(File.ReadAllLines("../../../../sample1.txt")));
        Assert.Equal(4, Part1(File.ReadAllLines("../../../../sample2.txt")));
        Assert.Equal(14, Part1(File.ReadAllLines("../../../../sample.txt")));
        Assert.Equal(398, Part1(File.ReadAllLines("../../../../input.txt")));
    }

    private static void AddAntinodes(string[] map, HashSet<(int, int)> antinodes, int ia, int ja, int ib, int jb)
    {
        var ic = 2 * ib - ia;
        var jc = 2 * jb - ja;
        if (ic >= 0 && jc >= 0 && ic < map.Length && jc < map[0].Length)
            antinodes.Add((ic, jc));
    }

    private static int Part1(string[] map)
    {
        if (map.Length == 0) throw new ArgumentException("map should contain at least 1 row");

        var antenas = new Dictionary<char, List<(int, int)>>();
        var antinodes = new HashSet<(int, int)>();
        for (var i = 0; i < map.Length; i++)
        {
            for (var j = 0; j < map[0].Length; j++)
            {
                var c = map[i][j];
                if (c != '.')
                {
                    antenas.TryGetValue(c, out var others);
                    if (others != null)
                    {
                        foreach (var (i2, j2) in others)
                        {
                            AddAntinodes(map, antinodes, i2, j2, i, j);
                            AddAntinodes(map, antinodes, i, j, i2, j2);
                        }
                        others.Add((i, j));
                    }
                    else
                    {
                        antenas.Add(c, [(i, j)]);
                    }
                }
            }
        }
        var result = antinodes.Count;
        return result;
    }

    [Fact]
    public void TestPart2()
    {
        Assert.Equal(9, Part2(File.ReadAllLines("../../../../sample3.txt")));
        Assert.Equal(34, Part2(File.ReadAllLines("../../../../sample.txt")));
        Assert.Equal(34, Part2(File.ReadAllLines("../../../../input.txt")));
    }

    private static void AddAntinodes2(string[] map, HashSet<(int, int)> antinodes, int ia, int ja, int ib, int jb)
    {
        while (ib >= 0 && ib < map.Length && jb >= 0 && jb < map[0].Length)
        {
            var ic = 2 * ib - ia;
            var jc = 2 * jb - ja;
            if (ic >= 0 && jc >= 0 && ic < map.Length && jc < map[0].Length)
                antinodes.Add((ic, jc));
            ia = ib;
            ja = jb;
            ib = ic;
            jb = jc;
        }
    }

    private static int Part2(string[] map)
    {
        if (map.Length == 0) throw new ArgumentException("map should contain at least 1 row");

        var antenas = new Dictionary<char, List<(int, int)>>();
        var antinodes = new HashSet<(int, int)>();
        for (var i = 0; i < map.Length; i++)
        {
            for (var j = 0; j < map[0].Length; j++)
            {
                var c = map[i][j];
                if (c != '.')
                {
                    antenas.TryGetValue(c, out var others);
                    if (others != null)
                    {
                        foreach (var (i2, j2) in others)
                        {
                            AddAntinodes2(map, antinodes, i2, j2, i, j);
                            AddAntinodes2(map, antinodes, i, j, i2, j2);
                        }
                        others.Add((i, j));
                        antinodes.Add((i, j));
                    }
                    else
                    {
                        antenas.Add(c, [(i, j)]);
                        antinodes.Add((i, j));
                    }
                }
            }
        }
        var result = antinodes.Count;
        return result;
    }

}