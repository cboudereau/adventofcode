namespace csharp;

public class UnitTest1
{
    private static long Possibilities(string towel, string[] stripes, Dictionary<string, long> cache)
    {
        if (cache.TryGetValue(towel, out var count))
        {
            return count;
        }
        if (towel == "") return 1;
        var result = stripes.Where(towel.StartsWith).Select(p => Possibilities(towel[p.Length..], stripes, cache)).Sum();
        cache.Add(towel, result);
        return result;
    }

    [Fact]
    public void Test1()
    {
        Assert.Equal((6, 16), Solve(File.ReadAllLines("../../../../sample.txt")));
        Assert.Equal((336, 758890600222015), Solve(File.ReadAllLines("../../../../input.txt")));
    }

    private static (long, long) Solve(string[] input)
    {
        var stripes = input[0].Split(',').Select(x => x.Trim()).ToArray() ?? throw new ArgumentException("stripes not found");
        var towels = input[2..] ?? throw new ArgumentException("towels not found");
        var cache = new Dictionary<string, long>();
        var possibilities = towels.Select(towel => Possibilities(towel, stripes, cache)).ToList();
        var total = possibilities.Where(x => x > 0).Count();
        var solutions = possibilities.Sum();
        return (total, solutions);
    }
}