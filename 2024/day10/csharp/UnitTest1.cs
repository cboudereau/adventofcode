namespace csharp;

public class UnitTest1
{
    private static int Part1(string[] map)
    {
        if (map.Length == 0) throw new ArgumentException("map should not be empty");
        var result = 0;
        for (var i = 0; i < map.Length; i++)
        {
            for (var j = 0; j < map[0].Length; j++)
            {
                result += Part1(map, i, j);
            }
        }
        return result;
    }

    private static int Part1(string[] map, int a, int b)
    {
        if (map[a][b] != '0') return 0;

        var visitedEnd = new HashSet<(int, int)>();
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
                continue;
            }
            foreach (var (x, y) in directions)
            {
                queue.Enqueue((v + 1, i + x, j + y));
            }
        }
        return visitedEnd.Count;
    }

    [Fact]
    public void Test1()
    {
        Assert.Equal(36, Part1(File.ReadAllLines("../../../../sample.txt")));
        Assert.Equal(548, Part1(File.ReadAllLines("../../../../input.txt")));
    }
}