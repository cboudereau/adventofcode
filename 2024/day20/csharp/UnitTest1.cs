namespace csharp;

public class UnitTest1
{
    private static readonly (int, int)[] DIRECTIONS = [(-1, 0), (1, 0), (0, -1), (0, 1)];
    private static IEnumerable<((int, int), (int, int)?)> Move(string[] map, ((int, int), (int, int)?) position)
    {
        var ((i, j), cheatPosition) = position;
        foreach (var (oi, oj) in DIRECTIONS)
        {
            var (i2, j2) = (i + oi, j + oj);
            if (i2 < 0 || j2 < 0 || i2 >= map.Length || j2 >= map[0].Length) continue;
            if (map[i2][j2] == '#')
            {
                if (!cheatPosition.HasValue)
                {
                    yield return ((i2, j2), (i2, j2));
                }
            }
            else
            {
                yield return ((i2, j2), cheatPosition);
            }
        }
    }

    [Fact]
    public void TestMove()
    {
        var map = File.ReadAllLines("../../../../sample.txt");
        var cheat = (12, 2);
        var result = Move(map, ((12, 2), cheat)).ToList();
        Assert.Equal([((13, 2), cheat), ((12, 1), cheat), ((12, 3), cheat)], result);
    }

    private static (int, int) FindChar(string[] map, char c)
    {
        for (var i = 0; i < map.Length; i++)
        {
            for (var j = 0; j < map[0].Length; j++)
            {
                if (map[i][j] == c) return (i, j);
            }
        }
        throw new ArgumentException($"cannot find {c}");
    }

    private static List<T> FindDuplicates<T>(List<T> l)
    {
        var h = new HashSet<T>();
        return l.Where(x => !h.Add(x)).ToList();
    }

    [Fact]
    public void TestFindDuplicates()
    {
        Assert.Equal([], FindDuplicates([1]));
        Assert.Equal([1], FindDuplicates([1, 1, 2]));
    }

    [Fact]
    public void TestDfs()
    {
        string[] map = File.ReadAllLines("../../../../sample.txt");
        var start = FindChar(map, 'S');
        var (count, p) = Dfs(map, start);
        var dp = FindDuplicates(p);
        Assert.Empty(dp);
        Assert.Equal(84, count);
    }

    [Fact]
    public void TestBestPathsToEnd()
    {
        string[] map = File.ReadAllLines("../../../../sample.txt");
        var bestPathsToEnd = BestPathsToEnd(map);
        Assert.NotEmpty(bestPathsToEnd);
        var totalDiff = 0;
        var totalDiff2 = 0;
        foreach (var e in bestPathsToEnd)
        {
            var (_, expectedPath) = Dfs(map, e.Key);
            var actualPath = e.Value;
            Assert.Equal(expectedPath, actualPath);
            var diff = expectedPath.Except(actualPath).ToList();
            var diff2 = actualPath.Except(expectedPath).ToList();
            totalDiff += diff.Count;
            totalDiff2 += diff2.Count;
        }
        Assert.Equal(0, totalDiff);
        Assert.Equal(0, totalDiff2);
    }

    [Fact]
    public void TestDebug()
    {
        string[] map = File.ReadAllLines("../../../../sample.txt");
        var (_, endPaths) = Solve(map, false);
        var (_, endPathsCache) = Solve(map, true);
        var diffCount = 0;
        var diff2Count = 0;
        foreach (var e in endPaths)
        {
            Assert.Empty(FindDuplicates(e.Value));
            var pc = endPathsCache[e.Key];
            Assert.Empty(FindDuplicates(pc));

            var diff = e.Value.Except(pc).ToList();
            var diff2 = pc.Except(e.Value).ToList();
            diffCount += diff.Count;
            diff2Count += diff2.Count;
            if (diff.Count > 0 || diff2.Count > 0)
            {

            }
        }
        Assert.Equal(0, diffCount);
        Assert.Equal(0, diff2Count);
    }

    [Fact]
    public void TestSample()
    {
        string[] map = File.ReadAllLines("../../../../sample.txt");
        var (maxLength, endPaths) = Solve(map, true);
        var finalPaths = endPaths.Where(x =>
        {
            var duplicates = FindDuplicates(x.Value);
            Assert.Empty(duplicates);
            return x.Value.Count < maxLength;
        }).ToList();
        Assert.Equal(44, finalPaths.Count);
    }

    [Fact]
    public void TestSample2()
    {
        string[] map = File.ReadAllLines("../../../../sample.txt");
        var (maxLength, endPaths) = Solve2(map, true);
        var finalPaths = endPaths.Where(x =>
        {
            return x.Value < maxLength;
        }).ToList();
        Assert.Equal(44, finalPaths.Count);
    }

    // [Fact(Skip = "Not ready")]
    [Fact]
    public void TestPart1()
    {
        string[] map = File.ReadAllLines("../../../../input.txt");
        var (maxLength, endPaths) = Solve2(map, true);
        var finalPaths = endPaths.Where(x =>
        {
            return x.Value < maxLength;
        }).ToList();
        Assert.Equal(44, finalPaths.Count);
    }

    private static Dictionary<(int, int), List<(int, int)>> BestPathsToEnd(string[] map)
    {
        var bestPathsToEnd = new Dictionary<(int, int), List<(int, int)>>();
        var queue = new Queue<(int, int, List<(int, int)>)>();
        for (var l = 1; l < map.Length - 1; l++)
        {
            for (var c = 1; c < map[0].Length - 1; c++)
            {
                queue.Enqueue((l, c, []));
                while (queue.Count > 0)
                {
                    var (i, j, path) = queue.Dequeue();
                    if (i < 0 || j < 0 || i >= map.Length || j >= map[0].Length || map[i][j] == '#' || bestPathsToEnd.ContainsKey((i, j))) continue;

                    if (map[i][j] == 'E')
                    {
                        for (var k = 0; k < path.Count; k++)
                        {
                            var position = path[k];
                            var candidate = path[k..];
                            if (!bestPathsToEnd.TryAdd(position, candidate))
                            {
                                var oldPath = bestPathsToEnd[position];
                                if (candidate.Count < oldPath.Count)
                                {
                                    bestPathsToEnd[position] = candidate;
                                }
                            }
                        }
                    }
                    else
                    {
                        foreach (var (oi, oj) in DIRECTIONS)
                        {
                            var i2 = i + oi;
                            var j2 = j + oj;
                            if (!path.Contains((i2, j2)))
                            {
                                queue.Enqueue((i2, j2, [.. path, (i, j)]));
                            }
                        }
                    }
                }
            }
        }
        return bestPathsToEnd;
    }

    private static void Add(Dictionary<(int, int), List<(int, int)>> d, (int, int) k, List<(int, int)> v)
    {
        if (!d.TryAdd(k, v))
        {
            var previous = d[k];
            if (v.Count < previous.Count)
            {
                d[k] = v;
            }
        }
    }

    private static void Add2(Dictionary<(int, int), int> d, (int, int) k, int v)
    {
        if (!d.TryAdd(k, v))
        {
            var previous = d[k];
            if (v < previous)
            {
                d[k] = v;
            }
        }
    }

    private static (long, Dictionary<(int, int), List<(int, int)>>) Solve(string[] map, bool isCacheEnabled = false)
    {
        Dictionary<(int, int), List<(int, int)>> bestPathsToEnd = [];
        var start = FindChar(map, 'S');

        var visited = new HashSet<((int, int), (int, int)?)>();
        var queue = new Queue<((int, int), (int, int)?, List<(int, int)>)>();
        queue.Enqueue((start, null, []));
        var endPaths = new Dictionary<(int, int), List<(int, int)>>();
        var maxPath = long.MinValue;

        while (queue.Count > 0)
        {
            var entry = queue.Dequeue();
            var ((i, j), cheatPosition, path) = entry;
            if (i < 0 || j < 0 || i >= map.Length || j >= map[0].Length || !visited.Add(((i, j), cheatPosition))) continue;

            if (map[i][j] == 'E')
            {
                if (cheatPosition.HasValue)
                {
                    Add(endPaths, cheatPosition.Value, path);
                    for (var k = 0; k < path.Count; k++)
                    {
                        var key = path[k];
                        var candidate = path[k..];
                        Add(bestPathsToEnd, key, candidate);
                    }
                }
                else maxPath = Math.Max(maxPath, path.Count);
            }
            else if (isCacheEnabled && cheatPosition.HasValue && bestPathsToEnd.TryGetValue((i, j), out var p))
            {
                List<(int, int)> candidate = [.. path, .. p];
                var key = cheatPosition.Value;
                Add(endPaths, key, candidate);
            }
            else
            {
                foreach (var (newPosition, newCheatPosition) in Move(map, ((i, j), cheatPosition)))
                {
                    if (path.Contains(newPosition)) continue;
                    queue.Enqueue((newPosition, newCheatPosition, [.. path, (i, j)]));
                }
            }
        }
        return (maxPath, endPaths);
    }

    private static (long, Dictionary<(int, int), int>) Solve2(string[] map, bool isCacheEnabled = false)
    {
        Dictionary<(int, int), int> bestPathsToEnd = [];
        var start = FindChar(map, 'S');

        var visited = new HashSet<((int, int), (int, int)?)>();
        var queue = new Queue<((int, int), (int, int)?, List<(int, int)>)>();
        queue.Enqueue((start, null, []));
        var endPaths = new Dictionary<(int, int), int>();
        var maxPath = long.MinValue;

        while (queue.Count > 0)
        {
            var entry = queue.Dequeue();
            var ((i, j), cheatPosition, path) = entry;
            if (i < 0 || j < 0 || i >= map.Length || j >= map[0].Length || !visited.Add(((i, j), cheatPosition))) continue;

            if (map[i][j] == 'E')
            {
                if (cheatPosition.HasValue)
                {
                    Add2(endPaths, cheatPosition.Value, path.Count);
                    for (var k = 0; k < path.Count; k++)
                    {
                        var key = path[k];
                        var candidate = path.Count - k;
                        Add2(bestPathsToEnd, key, candidate);
                    }
                }
                else maxPath = Math.Max(maxPath, path.Count);
            }
            else if (isCacheEnabled && cheatPosition.HasValue && bestPathsToEnd.TryGetValue((i, j), out var p))
            {
                var candidate = path.Count + p;
                var key = cheatPosition.Value;
                Add2(endPaths, key, candidate);
            }
            else
            {
                foreach (var (newPosition, newCheatPosition) in Move(map, ((i, j), cheatPosition)))
                {
                    if (path.Contains(newPosition)) continue;
                    queue.Enqueue((newPosition, newCheatPosition, [.. path, (i, j)]));
                }
            }
        }
        return (maxPath, endPaths);
    }


    private static (long, List<(int, int)>) Dfs(string[] map, (int, int) start)
    {
        var visited = new HashSet<(int, int)>();
        var stack = new Stack<((int, int), List<(int, int)>)>();
        stack.Push((start, []));

        while (stack.Count > 0)
        {
            var entry = stack.Pop();
            var ((i, j), path) = entry;
            if (i < 0 || j < 0 || i >= map.Length || j >= map[0].Length || !visited.Add((i, j))) continue;
            if (map[i][j] == 'E')
            {
                return (path.Count, path);
            }
            else
            {
                foreach (var (oi, oj) in DIRECTIONS)
                {
                    var i2 = i + oi;
                    var j2 = j + oj;
                    if (i2 < 0 || j2 < 0 || i2 >= map.Length || j2 >= map[0].Length || map[i2][j2] == '#') continue;
                    stack.Push(((i2, j2), [.. path, (i, j)]));
                }
            }
        }
        return (0, []);
    }
}