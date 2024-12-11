namespace csharp;

public class UnitTest1
{
    private static int GetNumberOfDigits(long n)
    {
        return (int)Math.Log10(n) + 1;
    }

    private static (long, long) Split(long n)
    {
        var digits = GetNumberOfDigits(n);
        if (digits % 2 != 0) throw new ArgumentException("expected an even number of digits");
        var p = (long)Math.Pow(10, digits / 2);
        var left = n / p;
        var right = n - (left * p);
        return (left, right);
    }

    [Fact]
    public void TestSplit()
    {
        Assert.Equal((10, 10), Split(1010));
        Assert.Equal((9, 9), Split(99));
        Assert.Throws<ArgumentException>(() => Split(1));
    }

    [Fact]
    public void TestGetNumberOfDigits()
    {
        Assert.Equal(1, GetNumberOfDigits(1));
        Assert.Equal(2, GetNumberOfDigits(10));
        Assert.Equal(2, GetNumberOfDigits(11));
        Assert.Equal(2, GetNumberOfDigits(99));
        Assert.Equal(3, GetNumberOfDigits(100));
        Assert.Equal(3, GetNumberOfDigits(987));
    }

    internal static long Solve(string input, int times)
    {
        var inputs = input.Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(long.Parse).ToList();
        long count = 0;
        Dictionary<(long, int), long> cache = [];
        foreach (var n in inputs)
        {
            var l = Solve(cache, n, times);
            count += l;
        }
        return count;
    }

    private static long Solve(Dictionary<(long, int), long> cache, long n, int times)
    {
        if (times == 0) return 1;

        if (cache.TryGetValue((n, times), out var cached)) return cached;

        times--;
        if (n == 0)
        {
            n = 1;
            var r = Solve(cache, n, times);
            cache.TryAdd((n, times), r);
            return r;
        }
        var digits = GetNumberOfDigits(n);
        if (digits % 2 == 0)
        {
            var (left, right) = Split(n);
            var leftCount = Solve(cache, left, times);
            cache.TryAdd((left, times), leftCount);
            var rightCount = Solve(cache, right, times);
            cache.TryAdd((right, times), rightCount);

            return leftCount + rightCount;
        }
        {
            n *= 2024;
            var r = Solve(cache, n, times);
            cache.TryAdd((n, times), r);
            return r;
        }
    }

    [Fact]
    public void TestPart1()
    {
        Assert.Equal(22, Solve("125 17", 6));
        Assert.Equal(56, Solve("125", 11));
        Assert.Equal(191690, Solve("30 71441 3784 580926 2 8122942 0 291", 25));
        Assert.Equal(228651922369703, Solve("30 71441 3784 580926 2 8122942 0 291", 75));
    }
}