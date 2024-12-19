namespace csharp;

public class Solution2
{
    private static (int, int)? FindChar(string[] map, char c)
    {
        for (var i = 0; i < map.Length; i++)
        {
            for (var j = 0; j < map[0].Length; j++)
            {
                if (map[i][j] == c) return (i, j);
            }
        }
        return null;
    }

    private static (long, int) GetValue((long, HashSet<(int, int)>) v)
    {
        var (d, p) = v;
        return (d, p.Count);
    }

    [Fact]
    public void TestPart1()
    {
        Assert.Equal((7036, 45), GetValue(Part1(File.ReadAllLines("../../../../sample.txt"))));
        Assert.Equal((11048, 64), GetValue(Part1(File.ReadAllLines("../../../../sample2.txt"))));
        Assert.Equal((85432, 465), GetValue(Part1(File.ReadAllLines("../../../../input.txt"))));
    }
    private static (int, int) TurnClockwise((int, int) direction)
    {
        return direction switch
        {
            (0, 1) => (1, 0),
            (1, 0) => (0, -1),
            (0, -1) => (-1, 0),
            (-1, 0) => (0, 1),
            _ => throw new ArgumentException("unexpected direction"),
        };
    }
    private static (int, int) TurnCounterClockwise((int, int) direction)
    {
        return TurnClockwise(TurnClockwise(TurnClockwise(direction)));
    }

    private static (long, HashSet<(int, int)>) Part1(string[] map)
    {
        (int, int)? e = FindChar(map, 'E');
        if (!e.HasValue) throw new ArgumentException("cannot find End 'E'");
        var s = FindChar(map, 'S');
        if (!s.HasValue) throw new ArgumentException("cannot find End 'S'");
        var start = s.Value;

        var distances = new Dictionary<((int, int), (int, int)), (HashSet<(int, int)>, long)>();
        var queue = new Queue<(long, (int, int), HashSet<(int, int)>, (int, int))>();
        var right = (0, 1);
        queue.Enqueue((0, right, [start], start));

        while (queue.Count > 0)
        {
            var (distance, direction, path, (i, j)) = queue.Dequeue();
            if (i < 0 || j < 0 || i >= map.Length || j >= map[0].Length || map[i][j] == '#') continue;
            var position = ((i, j), direction);
            if (distances.TryGetValue(position, out var oldPathAndDistance))
            {
                var (oldPath, oldDistance) = oldPathAndDistance;
                if (distance == oldDistance) distances[position] = ([.. oldPath, .. path], distance);
                else if (distance < oldDistance) distances[position] = (path, distance);
                else continue;
            }
            else distances.Add(position, (path, distance));

            var (oi, oj) = direction;
            var i2 = i + oi;
            var j2 = j + oj;
            queue.Enqueue((distance + 1, direction, [.. path, (i2, j2)], (i2, j2)));
            queue.Enqueue((distance + 1000, TurnClockwise(direction), path, (i, j)));
            queue.Enqueue((distance + 1000, TurnCounterClockwise(direction), path, (i, j)));
        }
        var end = e.Value;
        var minDistance = long.MaxValue;

        (int, int)[] directions = [(-1, 0), (0, 1), (1, 0), (-1, 0)];
        HashSet<(int, int)> paths = [];
        foreach (var direction in directions)
        {
            if (distances.TryGetValue((end, direction), out var pathAndDistance))
            {
                var (p, d) = pathAndDistance;
                if (d < minDistance)
                {
                    minDistance = d;
                    paths = p;
                }
            }
        }
        return (minDistance, paths);
    }
}