namespace csharp;

public class UnitTest1
{
    [Fact]
    public void TestTryWin()
    {
        var times = 100;
        Assert.Equal(280, TryWin((8400, 5400), (94, 34), (22, 67), times));
        Assert.False(TryWin((12748, 12176), (26, 66), (67, 21), times).HasValue);
        Assert.Equal(200, TryWin((7870, 6450), (17, 86), (84, 37), times));
        Assert.False(TryWin((18641, 10279), (69, 23), (27, 71), times).HasValue);
    }

    [Fact]
    public void TestTryWin2()
    {
        Assert.False(TryWin((10000000008400, 10000000005400), (94, 34), (22, 67), null).HasValue);
        Assert.Equal(459236326669, TryWin((10000000012748, 10000000012176), (26, 66), (67, 21), null));
        Assert.False(TryWin((10000000007870, 10000000006450), (17, 86), (84, 37), null).HasValue);
        Assert.Equal(416082282239, TryWin((10000000018641, 10000000010279), (69, 23), (27, 71), null));
    }

    private static long? TryWin((long, long) prize, (long, long) a, (long, long) b, int? times)
    {
        var (xp, yp) = prize;
        var (xa, ya) = a;
        var (xb, yb) = b;
        var d = xa * yb - ya * xb;
        var ta = (xp * yb - yp * xb) / d;
        var tb = (yp * xa - xp * ya) / d;
        if (ta * xa + tb * xb == xp && ta * ya + tb * yb == yp) return 3 * ta + tb;
        return null;
    }

    private static long ParseNumber(char separator, string text)
    {
        var index = text.IndexOf(separator);
        if (index < 0) throw new ArgumentException("no separator found in text");
        if (index == text.Length - 1) throw new ArgumentException("expected an int after the separator");
        return long.Parse(text[(index + 1)..]);
    }
    private static (long, long) ParseLine(char separator, string line)
    {
        var buttons = line.Split(',');
        if (buttons.Length != 2) throw new ArgumentException("expected buttons of length 2");
        var x = ParseNumber(separator, buttons[0]);
        var y = ParseNumber(separator, buttons[1]);
        return (x, y);
    }

    private static long Solve(string[] input, long offset, int? times)
    {
        long tokens = 0;
        for (var i = 0; i < input.Length; i += 4)
        {
            var a = ParseLine('+', input[i]);
            var b = ParseLine('+', input[i + 1]);
            var (xp, yp) = ParseLine('=', input[i + 2]);
            var prize = (xp + offset, yp + offset);
            var fewestToken = TryWin(prize, a, b, times);
            if (fewestToken.HasValue) tokens += fewestToken.Value;
        }

        return tokens;
    }

    [Fact]
    public void TestPart1()
    {
        var times = 100;
        var offset = 0;
        Assert.Equal(480, Solve(File.ReadAllLines("../../../../sample.txt"), offset, times));
        Assert.Equal(36571, Solve(File.ReadAllLines("../../../../input.txt"), offset, times));
    }

    [Fact]
    public void TestPart2()
    {
        var offset = 10000000000000;
        Assert.Equal(875318608908, Solve(File.ReadAllLines("../../../../sample.txt"), offset, null));
        Assert.Equal(85527711500010, Solve(File.ReadAllLines("../../../../input.txt"), offset, null));
    }
}