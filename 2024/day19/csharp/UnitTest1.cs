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
        var input = File.ReadAllLines("../../../../input.txt");
        var stripes = input[0].Split(',').Select(x => x.Trim()).ToArray() ?? throw new ArgumentException("stripes not found");
        var towels = input[2..] ?? throw new ArgumentException("towels not found");
        var cache = new Dictionary<string, long>();
        Assert.Equal(336, towels.Where(towel => Possibilities(towel, stripes, cache) > 0).Count());
    }
}