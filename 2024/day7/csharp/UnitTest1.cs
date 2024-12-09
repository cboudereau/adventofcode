namespace csharp;

public class UnitTest1
{
    private static bool IsValid(long expected, long[] numbers, Func<long, long, long>[] operations)
    {
        if (numbers.Length < 1) throw new ArgumentException("numbers length should not be empty");

        var queue = new Queue<long>();
        queue.Enqueue(numbers[0]);
        for (var i = 1; i < numbers.Length; i++)
        {
            var n = numbers[i];
            var c = queue.Count;
            for (var j = 0; j < c; j++)
            {
                var v = queue.Dequeue();
                foreach (var op in operations)
                {
                    queue.Enqueue(op(v, n));
                }
            }
        }

        foreach (var actual in queue)
        {
            if (actual == expected) return true;
        }

        return false;
    }

    [Fact]
    public void TestPart1()
    {
        Func<long, long, long>[] operators = [(x, y) => x + y, (x, y) => x * y];
        Assert.Equal(3749, Part1("../../../../sample.txt", operators));
        Assert.Equal(5702958180383, Part1("../../../../input.txt", operators));
    }

    [Fact]
    public void TestPart2()
    {
        Func<long, long, long>[] operators = [(x, y) => {
                checked { return x + y; };
            },
            (x, y) => {
                checked { return x * y; }
                },
            (x, y) =>
        {
            checked
            {
                var l = (int) Math.Log10(y) + 1;
                var m = (long) Math.Pow(10, l);
                return x * m + y;
            }
        }];
        Assert.Equal(11387, Part1("../../../../sample.txt", operators));
        Assert.Equal(92612386119138, Part1("../../../../input.txt", operators));
    }

    private static long Part1(string filePath, Func<long, long, long>[] operators)
    {
        var parts = Parse(filePath);
        var actual = parts.Where(x =>
        {
            var (expected, numbers) = x;
            return IsValid(expected, numbers, operators);
        }).Select(x =>
        {
            var (expected, _) = x;
            return expected;
        }).Sum();
        return actual;
    }

    private static (long expected, long[] numbers)[] Parse(string filePath)
    {
        return File.ReadAllLines(filePath).Select(x =>
        {
            var parts = x.Split(':');
            if (parts.Length != 2) throw new ArgumentException("the length of the array should be 2");
            var (part1, part2) = (parts[0], parts[1]);
            var expected = long.Parse(part1);
            var numbers = part2.Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(long.Parse).ToArray();
            return (expected, numbers);
        }).ToArray();
    }
}