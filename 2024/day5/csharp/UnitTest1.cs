namespace csharp;

public class UnitTest1
{
    [Fact]
    public void TestPart2()
    {
        Assert.Equal(123, Part2("""../../../../sample.txt"""));
        Assert.Equal(5448, Part2("""../../../../input.txt"""));
    }

    private static long Part2(string filePath)
    {
        var parts = File.ReadAllText(filePath).Split("\n\n");
        var rules = parts[0].Split("\n").Select(x =>
        {
            var a = x.Split('|').Select(long.Parse).ToArray();
            return (a[0], a[1]);
        }).ToHashSet();
        var updates = parts[1].Split("\n").Select(x => x.Split(',').Select(long.Parse).ToArray()).ToArray();

        var fixedUpdates = new List<long[]>();
        // O(m*n^2)
        foreach (var update in updates)
        {
            bool toKeep = false;
            bool canContinue = true;
            // O(n^2)
            while (canContinue)
            {
                canContinue = false;
                // O(n)
                for (var i = 0; i < update.Length - 1; i++)
                {
                    var current = update[i];
                    var next = update[i + 1];
                    if (rules.Contains((next, current)))
                    {
                        toKeep = true;
                        canContinue = true;
                        update[i] = next;
                        update[i + 1] = current;
                    }
                }
            }
            if (toKeep) fixedUpdates.Add(update);
        }

        return fixedUpdates.Select(x => x[x.Length / 2]).Sum();
    }
}