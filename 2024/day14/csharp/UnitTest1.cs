
using System.Text;

namespace csharp;

public class UnitTest1
{
    private static long GetVelocity(long v, long max)
    {
        return (v < 0) ? max + v : v;
    }

    private static long Move(long i, long v, long t, long max)
    {
        return (i + t * v) % max;
    }
    internal static (long, long) GetPos((long, long) p, (long, long) v, long t, long maxX, long maxY)
    {
        var (xp, yp) = p;
        var (vx, vy) = v;
        vx = GetVelocity(vx, maxX);
        vy = GetVelocity(vy, maxY);
        var x = Move(xp, vx, t, maxX);
        var y = Move(yp, vy, t, maxY);
        return (x, y);
    }

    [Fact]
    public void TestGetPos()
    {
        Assert.Equal((4, 1), GetPos((2, 4), (2, -3), 1, 11, 7));
        Assert.Equal((6, 5), GetPos((2, 4), (2, -3), 2, 11, 7));
        Assert.Equal((8, 2), GetPos((2, 4), (2, -3), 3, 11, 7));
        Assert.Equal((10, 6), GetPos((2, 4), (2, -3), 4, 11, 7));
        Assert.Equal((1, 3), GetPos((2, 4), (2, -3), 5, 11, 7));

        Assert.Equal((3, 5), GetPos((0, 4), (3, -3), 100, 11, 7));
        Assert.Equal((5, 4), GetPos((6, 3), (-1, -3), 100, 11, 7));
    }

    private static (long, long) ParseCoords(string input)
    {
        var index = input.IndexOf('=');
        if (index == -1 || index >= input.Length - 1) throw new ArgumentException("bad input");
        var parts = input[(index + 1)..].Split(',');
        if (parts.Length != 2) throw new ArgumentException("bad parts");
        return (long.Parse(parts[0]), long.Parse(parts[1]));
    }

    [Fact]
    public void TestParseCoords()
    {
        Assert.Throws<ArgumentException>(() => ParseCoords(""));
        Assert.Throws<ArgumentException>(() => ParseCoords("p="));
        Assert.Equal((0, 4), ParseCoords("p=0,4"));
    }
    internal static ((long, long), (long, long)) ParseLine(string input)
    {
        var parts = input.Split(' ');
        if (parts.Length != 2) throw new ArgumentException("input should contains 2 parts delimited by a comma");
        return (ParseCoords(parts[0]), ParseCoords(parts[1]));
    }

    [Fact]
    public void TestParseLine()
    {
        Assert.Equal(((0, 4), (3, -3)), ParseLine("p=0,4 v=3,-3"));
    }

    [Fact]
    public void TestPart1()
    {
        Assert.Equal(12, Solve(11, 7, 100, File.ReadAllLines("../../../../sample.txt")));
        Assert.Equal(226179492, Solve(101, 103, 100, File.ReadAllLines("../../../../input.txt")));
    }

    private static long Solve(int wide, int tall, int times, string[] input)
    {
        var middleWide = (wide - 1) / 2;
        var middleTall = (tall - 1) / 2;
        long[] quadrants = [0, 0, 0, 0];

        var inputs = input.Select(l =>
        {
            var (p, v) = ParseLine(l);
            return GetPos(p, v, times, wide, tall);
        }).Where(p =>
        {
            var (x, y) = p;
            return x != middleWide && y != middleTall;
        });

        foreach (var (x, y) in inputs)
        {
            if (x < middleTall)
            {
                if (y < middleTall)
                {
                    quadrants[0]++;
                }
                else
                {
                    quadrants[1]++;
                }
            }
            else
            {
                if (y < middleTall)
                {
                    quadrants[2]++;
                }
                else
                {
                    quadrants[3]++;
                }
            }
        }

        long result = 1;
        foreach (var q in quadrants)
        {
            result *= q;
        }

        return result;
    }

    [Fact]
    public void TestPart2()
    {
        var input = File.ReadAllLines("../../../../input.txt");

        var wide = 101;
        var tall = 103;

        var times = 0;
        var sb = new StringBuilder();
        var output = "";
        while (!output.Contains("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
        {
            times++;
            sb.Clear();
            var inputs =
                input.Select(l =>
                {
                    var (p, v) = ParseLine(l);
                    return GetPos(p, v, times, wide, tall);

                });
            var h = new HashSet<(long, long)>(inputs);
            for (var y = 0; y < tall; y++)
            {
                for (var x = 0; x < wide; x++)
                {
                    var c = h.Contains((x, y)) ? 'X' : '.';
                    sb.Append(c);
                }
                sb.AppendLine();
            }

            output = sb.ToString();
        }

        Assert.Equal(7502, times);
    }
}