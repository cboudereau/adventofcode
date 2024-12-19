namespace csharp;

public class BestPerfButBadCount
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
        Assert.Equal((7036, 37), GetValue(Part1(File.ReadAllLines("../../../../sample.txt"))));
        Assert.Equal((11048, 49), GetValue(Part1(File.ReadAllLines("../../../../sample2.txt"))));
        Assert.Equal((85432, 433), GetValue(Part1(File.ReadAllLines("../../../../input.txt"))));

        // Assert.Equal((7036, 45), GetValue(Part1(File.ReadAllLines("../../../../sample.txt"))));
        // Assert.Equal((11048, 64), GetValue(Part1(File.ReadAllLines("../../../../sample2.txt"))));
        // Assert.Equal((85432, 465), GetValue(Part1(File.ReadAllLines("../../../../input.txt"))));
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
            bool canContinue;
            if (distances.TryAdd(position, (path, distance)))
            {
                canContinue = true;
            }
            else
            {
                var (oldPath, oldDistance) = distances[position];
                if (distance == oldDistance)
                {
                    distances[position] = ([.. oldPath, .. path], distance);
                }
                else
                {
                    distances[position] = (path, distance);
                }
                canContinue = distance < oldDistance;
            }

            if (canContinue)
            {
                {
                    var (oi, oj) = direction;
                    var i2 = i + oi;
                    var j2 = j + oj;
                    queue.Enqueue((distance + 1, direction, [.. path, (i2, j2)], (i2, j2)));
                }

                {
                    var (oi, oj) = TurnClockwise(direction);
                    var i2 = i + oi;
                    var j2 = j + oj;
                    queue.Enqueue((distance + 1001, (oi, oj), [.. path, (i2, j2)], (i2, j2)));
                }

                {
                    var (oi, oj) = TurnClockwise(TurnClockwise(TurnClockwise(direction)));
                    var i2 = i + oi;
                    var j2 = j + oj;
                    queue.Enqueue((distance + 1001, (oi, oj), [.. path, (i2, j2)], (i2, j2)));
                }
            }
        }
        var end = e.Value;
        var minDistance = long.MaxValue;
        foreach (var distance in distances)
        {
            var (d, _) = distance.Key;
            if (d == end)
            {
                var (_, v) = distance.Value;
                if (v < minDistance)
                {
                    minDistance = v;
                }
            }
        }

        HashSet<(int, int)> allTiles = [];
        foreach (var distance in distances)
        {
            var (d, _) = distance.Key;
            if (d == end)
            {
                var (p, v) = distance.Value;
                if (v == minDistance)
                {
                    allTiles = [.. allTiles, .. p];
                }
            }
        }
        return (minDistance, allTiles);
    }
}