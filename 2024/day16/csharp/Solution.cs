namespace csharp;

public class Solution
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

    internal static (long, int) GetValue((long, HashSet<(int, int)>) v)
    {
        var (d, p) = v;
        return (d, p.Count + 1);
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

    private static (int, int)[] Turn((int, int) direction)
    {
        return [TurnClockwise(direction), TurnCounterClockwise(direction)];
    }

    private static IEnumerable<(((int, int), (int, int)), (HashSet<(int, int)>, long))> Move(string[] map, (((int, int), (int, int)), (HashSet<(int, int)>, long)) entry)
    {
        var (((i, j), (oi, oj)), (path, distance)) = entry;
        var i2 = i + oi;
        var j2 = j + oj;
        if (map[i2][j2] != '#') yield return (((i2, j2), (oi, oj)), ([.. path, (i, j)], distance + 1));
        foreach (var direction in Turn((oi, oj)))
        {
            yield return (((i, j), direction), (path, distance + 1000));
        }
    }

    private static void Solve(string[] map, Dictionary<((int, int), (int, int)), (HashSet<(int, int)>, long)> distances, List<(((int, int), (int, int)), (HashSet<(int, int)>, long))> positions)
    {
        if (positions.Count == 0) return;
        foreach (var (position, (path, distance)) in positions)
        {
            if (distances.TryGetValue(position, out var oldPathAndDistance))
            {
                var (oldPath, oldDistance) = oldPathAndDistance;
                if (distance == oldDistance) distances[position] = ([.. oldPath, .. path], distance);
                else distances[position] = (path, distance);
            }
            else distances.Add(position, (path, distance));
        }

        var newPositions = positions.SelectMany(position => Move(map, position)).Where(move =>
        {
            var (position, (_, distance)) = move;
            var isNew = !distances.TryGetValue(position, out var oldPathAndDistance);
            var (_, oldDistance) = oldPathAndDistance;
            return isNew || distance < oldDistance;
        }).ToList();
        Solve(map, distances, newPositions);
    }

    internal static (long, HashSet<(int, int)>) Part1(string[] map)
    {
        (int, int)? e = FindChar(map, 'E');
        if (!e.HasValue) throw new ArgumentException("cannot find End 'E'");
        var s = FindChar(map, 'S');
        if (!s.HasValue) throw new ArgumentException("cannot find End 'S'");
        var start = s.Value;
        var visited = new Dictionary<((int, int), (int, int)), (HashSet<(int, int)>, long)>();
        var right = (0, 1);

        Solve(map, visited, [((start, right), ([], 0))]);

        var end = e.Value;
        var minDistance = long.MaxValue;
        HashSet<(int, int)> paths = [];
        foreach (var entry in visited)
        {
            var (pos, _) = entry.Key;
            var (p, d) = entry.Value;
            if (pos == end && d < minDistance)
            {
                minDistance = d;
                paths = p;
            }
        }
        return (minDistance, paths);
    }
}