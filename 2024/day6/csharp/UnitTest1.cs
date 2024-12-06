namespace csharp;

public class UnitTest1
{
    internal static (int, int) GetPos(char[,] map, char c)
    {
        var l1 = map.GetLength(0);
        var l2 = map.GetLength(1);
        for (var i = 0; i < l1; i++)
        {
            for (var j = 0; j < l2; j++)
            {
                if (map[i, j] == c) return (i, j);
            }
        }
        throw new ArgumentException("map does not contains char");
    }

    private static char TurnRight(char c) => c switch
    {
        '^' => '>',
        '>' => 'v',
        'v' => '<',
        '<' => '^',
        _ => throw new ArgumentException($"unexpected char '{c}'"),
    };

    private static (int, int) Move(char c) => c switch
    {
        '^' => (-1, 0),
        '>' => (0, 1),
        'v' => (1, 0),
        '<' => (0, -1),
        _ => throw new ArgumentException($"unexpected char '{c}'"),
    };

    private static bool IsOutside(int i, int j, int l1, int l2)
    {
        return i < 0 || j < 0 || i >= l1 || j >= l2;
    }

    internal static bool HasCycle(char[,] map, HashSet<((int, int), (int, int))> visited, char dir, int a, int b)
    {
        var stack = new Stack<(char, int, int)>();
        stack.Push((dir, a, b));
        while (stack.Count > 0)
        {
            var (direction, i, j) = stack.Pop();

            var l1 = map.GetLength(0);
            var l2 = map.GetLength(1);

            if (IsOutside(i, j, l1, l2)) return false;
            var (x, y) = Move(direction);
            var i2 = i + x;
            var j2 = j + y;

            var node = ((i, j), (i2, j2));
            if (visited.Contains(node)) return true;
            visited.Add(node);

            if (IsOutside(i2, j2, l1, l2))
            {
                map[i, j] = 'X';
                return false;
            }

            if (map[i2, j2] == '#')
            {
                var d = TurnRight(direction);
                map[i, j] = 'X';
                stack.Push((d, i, j));
            }
            else
            {
                map[i, j] = 'X';
                map[i2, j2] = direction;
                stack.Push((direction, i2, j2));
            }
        }
        return false;
    }

    private static int GetVisitedCount(char[,] map)
    {
        var result = 0;
        var l1 = map.GetLength(0);
        var l2 = map.GetLength(1);
        for (var i = 0; i < l1; i++)
        {
            for (var j = 0; j < l2; j++)
            {
                if (map[i, j] == 'X') result++;
            }
        }
        return result;
    }

    private static int Part1(string filePath)
    {
        char[,] map = GetMap(filePath);
        var (i, j) = GetPos(map, '^');
        HasCycle(map, [], map[i, j], i, j);

        return GetVisitedCount(map);
    }

    internal static char[,] GetMap(string filePath)
    {
        var lines = File.ReadAllLines(filePath);
        var l1 = lines.Length;
        var l2 = lines[0].Length;
        var map = new char[l1, l2];

        for (var i = 0; i < l1; i++)
        {
            for (var j = 0; j < l2; j++)
            {
                map[i, j] = lines[i][j];
            }
        }

        return map;
    }

    [Fact]
    public void TestPart1()
    {
        Assert.Equal(41, Part1("../../../../sample.txt"));
        Assert.Equal(5551, Part1("../../../../input.txt"));
    }

    [Fact(Timeout = 300000000)]
    public void TestPart2EachSample()
    {
        var filePath = "../../../../sample.txt";
        var map = GetMap(filePath);
        var (x, y) = GetPos(map, '^');

        Assert.False(UnitTestPart2(filePath, x, y, 0, 0));
        Assert.True(UnitTestPart2(filePath, x, y, 6, 3));
        Assert.True(UnitTestPart2(filePath, x, y, 7, 6));
        Assert.True(UnitTestPart2(filePath, x, y, 8, 1));
        Assert.True(UnitTestPart2(filePath, x, y, 8, 3));
        Assert.True(UnitTestPart2(filePath, x, y, 9, 7));
    }

    private static bool UnitTestPart2(string filePath, int x, int y, int i, int j)
    {
        var map2 = GetMap(filePath);
        map2[i, j] = '#';
        return HasCycle(map2, [], map2[x, y], x, y);
    }

    [Fact(Timeout = 300000000)]
    public void TestPart2Sample()
    {
        Assert.Equal(6, Part2("../../../../sample.txt"));
    }

    [Fact(Timeout = 300000000)]
    public void TestPart2Input()
    {
        Assert.Equal(1939, Part2("../../../../input.txt"));
    }

    internal static int Part2(string filePath)
    {
        var map = GetMap(filePath);
        var (x, y) = GetPos(map, '^');
        var result = 0;

        for (var i = 0; i < map.GetLength(0); i++)
        {
            for (var j = 0; j < map.GetLength(1); j++)
            {
                if (map[i, j] == '.')
                {
                    var map2 = GetMap(filePath);
                    map2[i, j] = '#';
                    if (HasCycle(map2, [], map2[x, y], x, y)) result++;
                }
            }
        }

        return result;
    }
}