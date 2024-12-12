
namespace csharp;

public class UnitTest1
{

    [Fact]
    public void TestRegion()
    {
        var actual = GetRegions(@"AAAA
BBCD
BBCC
EEEC".Split(Environment.NewLine));

        List<(char, long, long, List<(int, int)>)> expected =
            [('A', 4, 10, [(0, 0), (0, 1), (0, 2), (0, 3)])];

        for (var i = 0; i < expected.Count; i++)
        {
            var (expectedPlant, expectedArea, expectedPerimeter, expectedRegion) = expected[0];
            AssertRegion(actual[0], expectedPlant, expectedArea, expectedPerimeter, expectedRegion);
        }
    }

    private static void AssertRegion((char, long, long, List<(int, int)>) actual, char expectedPlant, long expectedArea, long expectedPerimeter, List<(int, int)> expectedRegion)
    {
        var (plant, area, perimeter, region) = actual;
        Assert.Equal(expectedPlant, plant);
        Assert.Equal(expectedArea, area);
        Assert.Equal(expectedPerimeter, perimeter);
        Assert.Equal(expectedRegion, region);
    }

    private static List<(char, long, long, List<(int, int)>)> GetRegions(string[] input)
    {
        var visited = new HashSet<(int, int)>();
        List<(char, long, long, List<(int, int)>)> result = [];
        for (var i = 0; i < input.Length; i++)
        {
            for (var j = 0; j < input[0].Length; j++)
            {
                var region = GetRegion(visited, input, i, j);
                if (region.HasValue) result.Add(region.Value);
            }
        }
        return result;
    }

    private static (char, long, long, List<(int, int)>)? GetRegion(HashSet<(int, int)> visited, string[] input, int a, int b)
    {
        List<(int, int)> directions = [(0, 1), (0, -1), (1, 0), (-1, 0)];
        List<(int, int, int)> region = [];
        var queue = new Queue<(int, int)>();
        var plant = input[a][b];
        queue.Enqueue((a, b));

        while (queue.Count > 0)
        {
            var (i, j) = queue.Dequeue();
            if (!visited.Add((i, j)) || input[i][j] != plant) continue;
            var perimeter = 0;
            foreach (var (x, y) in directions)
            {
                var i2 = i + x;
                var j2 = j + y;
                if (i2 < 0 || j2 < 0 || i2 >= input.Length || j2 >= input[0].Length || input[i2][j2] != plant)
                {
                    perimeter++;
                }
                else
                {
                    queue.Enqueue((i2, j2));
                }
            }
            region.Add((perimeter, i, j));
        }

        var area = region.Count;
        if (area == 0) return null;
        else
        {
            var perimeter = 0;
            List<(int, int)> plots = [];
            foreach (var (p, i, j) in region)
            {
                perimeter += p;
                plots.Add((i, j));
            }

            return (plant, area, perimeter, plots);
        }
    }

    private static long Part1(string[] input)
    {
        long result = 0;
        foreach (var (plant, area, perimeter, region) in GetRegions(input))
        {
            result += area * perimeter;
        }
        return result;
    }

    [Fact]
    public void TestPart1()
    {
        Assert.Equal(1930, Part1(@"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE".Split(Environment.NewLine)));

        Assert.Equal(1396298, Part1(File.ReadAllLines("../../../../input.txt")));
    }
}