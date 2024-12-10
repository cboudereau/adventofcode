namespace csharp;

public class UnitTest1
{
    private static (int, int) Solve(string[] map)
    {
        if (map.Length == 0) throw new ArgumentException("map should not be empty");
        var (count, totalCount) = (0, 0);
        for (var i = 0; i < map.Length; i++)
        {
            for (var j = 0; j < map[0].Length; j++)
            {
                var (cellCount, cellTotalCount) = Solve(map, i, j);
                count += cellCount;
                totalCount += cellTotalCount;
            }
        }
        return (count, totalCount);
    }

    private static (int, int) Solve(string[] map, int a, int b)
    {
        if (map[a][b] != '0') return (0, 0);

        var visitedEnd = new HashSet<(int, int)>();
        var count = 0;
        (int, int)[] directions = [(-1, 0), (1, 0), (0, -1), (0, 1)];
        var queue = new Queue<(int, int, int)>();
        queue.Enqueue((0, a, b));
        while (queue.Count > 0)
        {
            var (v, i, j) = queue.Dequeue();

            if (i < 0 || j < 0 || i >= map.Length || j >= map[0].Length) continue;
            if (map[i][j] - '0' != v) continue;
            if (map[i][j] == '9')
            {
                visitedEnd.Add((i, j));
                count++;
                continue;
            }
            foreach (var (x, y) in directions)
            {
                queue.Enqueue((v + 1, i + x, j + y));
            }
        }
        return (visitedEnd.Count, count);
    }

    [Fact]
    public void Test1()
    {
        Assert.Equal((36, 81), Solve(File.ReadAllLines("../../../../sample.txt")));
        Assert.Equal((778, 1925), Solve(File.ReadAllLines("../../../../input.txt")));
    }
}