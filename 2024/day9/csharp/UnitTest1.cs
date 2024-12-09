using Microsoft.VisualBasic;

namespace csharp;

public class UnitTest1
{
    [Fact]
    public void TestPart1()
    {
        Assert.Equal(1928, Part1("2333133121414131402"));
        Assert.Equal(6301895872542, Part1(File.ReadAllText("../../../../input.txt")));
    }

    private static long Part1(string input)
    {
        if (input.Length % 2 == 0) throw new ArgumentException("expected an odd input length");
        var counts = new Dictionary<int, int>();
        var leftPointer = 0;
        var rightPointer = input.Length - 1;
        var rightCount = input[rightPointer] - '0';
        var spaceCount = 0;

        List<int> compacted = [];

        while (leftPointer <= rightPointer)
        {
            if (spaceCount > 0)
            {
                if (rightCount > 0)
                {
                    spaceCount--;
                    rightCount--;
                    var fileId = rightPointer / 2;
                    if (!counts.TryAdd(fileId, 1)) counts[fileId]++;
                    compacted.Add(fileId);
                }
                else
                {
                    rightPointer -= 2;
                    if (rightPointer > 0)
                    {
                        var fileId = rightPointer / 2;
                        counts.TryGetValue(fileId, out var count);
                        rightCount = input[rightPointer] - '0' - count;
                    }
                }
            }
            else
            {
                var fileId = leftPointer / 2;

                var n = input[leftPointer] - '0';
                counts.TryGetValue(fileId, out var count);
                n -= count;
                if (!counts.TryAdd(fileId, n)) counts[fileId] += n;

                for (var i = 0; i < n; i++)
                {
                    compacted.Add(leftPointer / 2);
                }
                leftPointer++;

                if (leftPointer < input.Length) spaceCount = input[leftPointer] - '0';

                leftPointer++;
            }
        }

        return compacted.Select((i, x) => i * ((long)x)).Sum();
    }

    [Fact]
    public void TestPart2()
    {
        Assert.Equal(2858, Part2("2333133121414131402"));
        Assert.Equal(6323761685944, Part2(File.ReadAllText("../../../../input.txt")));
    }

    private static long Part2(string input)
    {
        var compacted = new Dictionary<int, List<(int, int)>>();
        var toIgnore = new HashSet<int>();
        var counts = new Dictionary<int, int>();

        for (var right = input.Length - 1; right >= 0; right -= 2)
        {
            var candidate = input[right] - '0';
            for (var i = 1; i < right; i += 2)
            {
                var space = input[i] - '0';
                counts.TryGetValue(i, out var count);

                if (space - count >= candidate)
                {
                    if (!counts.TryAdd(i, candidate)) counts[i] += candidate;
                    var entry = (candidate, right / 2);
                    if (!compacted.TryAdd(i, [entry]))
                    {
                        compacted[i].Add(entry);
                    }
                    toIgnore.Add(right);
                    break;
                }
            }
        }

        var result = new List<int?>();

        for (var i = 0; i < input.Length; i++)
        {
            var length = input[i] - '0';
            if (i % 2 == 0)
            {
                for (var j = 0; j < length; j++)
                {
                    if (!toIgnore.Contains(i)) result.Add(i / 2);
                    else result.Add(null);
                }
            }
            else
            {
                if (compacted.ContainsKey(i))
                {
                    var entries = compacted[i];
                    var filled = 0;
                    foreach (var (count, compactedFileId) in entries)
                    {
                        for (var j = 0; j < count; j++)
                        {
                            result.Add(compactedFileId);
                            filled++;
                        }
                    }
                    if (filled > length) throw new Exception($"failed: {filled} > {length}");
                    for (var j = 0; j < length - filled; j++)
                    {
                        result.Add(null);
                    }

                }
                else
                {
                    for (var j = 0; j < length; j++) result.Add(null);
                }
            }
        }

        return result.Select((x, i) =>
        {
            if (x == null) return 0;
            return (long)x * i;
        }).Sum();
    }
}