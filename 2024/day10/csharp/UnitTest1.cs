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
        Assert.Equal(778, Part1(File.ReadAllLines("../../../../input.txt")));
    }

    private static int Part2(string[] map, int a, int b)
    {
        if (map[a][b] != '0') return 0;
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
                count++;
                continue;
            }
            foreach (var (x, y) in directions)
            {
                queue.Enqueue((v + 1, i + x, j + y));
            }
        }
        return count;
    }

    private static int Part2(string[] map)
    {
        if (map.Length == 0) throw new ArgumentException("map should not be empty");
        var result = 0;
        for (var i = 0; i < map.Length; i++)
        {
            for (var j = 0; j < map[0].Length; j++)
            {
                result += Part2(map, i, j);
            }
        }
        return result;
    }

    [Fact]
    public void Test2()
    {
        Assert.Equal(81, Part2(File.ReadAllLines("../../../../sample.txt")));
        Assert.Equal(1925, Part2(File.ReadAllLines("../../../../input.txt")));
    }
}