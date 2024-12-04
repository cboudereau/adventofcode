
namespace csharp;

public class UnitTest1
{
    [Fact]
    public void TestPart1Sample()
    {
        var a = Get2DArray("../../../sample.txt");
        int result = Part1(a);
        Assert.Equal(18, result);
    }

    [Fact]
    public void TestPart1Input()
    {
        var a = Get2DArray("../../../input.txt");
        Assert.Equal(2560, Part1(a));
    }

    [Fact]
    public void TestPart2Sample()
    {
        var a = Get2DArray("../../../sample2.txt");
        Assert.Equal(9, Part2(a));
    }

    [Fact]
    public void TestPart2Input()
    {
        var a = Get2DArray("../../../input.txt");
        Assert.Equal(1910, Part2(a)); // not 1831, 1993
    }
    private static int Part2(char[,] a)
    {
        var l1 = a.GetLength(0);
        var l2 = a.GetLength(1);
        var result = 0;
        for (var i = 1; i < l1 - 1; i++)
        {
            for (var j = 1; j < l2 - 1; j++)
            {
                if (a[i, j] == 'A')
                {
                    if ((a[i - 1, j - 1] == 'M' && a[i + 1, j + 1] == 'S') || (a[i - 1, j - 1] == 'S' && a[i + 1, j + 1] == 'M'))
                    {
                        if ((a[i - 1, j + 1] == 'M' && a[i + 1, j - 1] == 'S') || (a[i - 1, j + 1] == 'S' && a[i + 1, j - 1] == 'M'))
                        {
                            result++;
                        }
                    }
                }
            }
        }

        return result;
    }

    private static int Part1(char[,] a)
    {
        var l1 = a.GetLength(0);
        var l2 = a.GetLength(1);
        var result = 0;
        var word = "XMAS";
        for (var i = 0; i < l1; i++)
        {
            for (var j = 0; j < l2; j++)
            {
                result += Dfs(a, word, 0, l1, l2, HorizontalDirections, i, j);
                result += Dfs(a, word, 0, l1, l2, VerticalDirections, i, j);
                result += Dfs(a, word, 0, l1, l2, RightDiagonalDirections, i, j);
                result += Dfs(a, word, 0, l1, l2, LeftDiagonalDirections, i, j);
            }
        }

        return result;
    }

    private static readonly (int, int)[] HorizontalDirections = [(0, -1), (0, 1)];
    private static readonly (int, int)[] VerticalDirections = [(1, 0), (-1, 0)];
    private static readonly (int, int)[] RightDiagonalDirections = [(-1, 1), (1, -1)];
    private static readonly (int, int)[] LeftDiagonalDirections = [(-1, -1), (1, 1)];

    private static int Dfs(char[,] a, string word, int pos, int l1, int l2, (int, int)[] directions, int i, int j)
    {
        if (i < 0 || j < 0 || i >= l1 || j >= l2) return 0;
        char candidate = a[i, j];
        char c = word[pos];
        if (c != candidate) return 0;

        if (pos == word.Length - 1) return 1;

        var result = 0;
        foreach (var (x, y) in directions)
        {
            result += Dfs(a, word, pos + 1, l1, l2, directions, i + x, j + y);
        }
        return result;
    }

    private static char[,] Get2DArray(string Sample)
    {
        var content = File.ReadAllLines(Sample);

        int l1 = content.Length;
        int l2 = content[0].Length;
        var map = new char[l1, l2];

        for (var i = 0; i < l1; i++)
        {
            for (var j = 0; j < l2; j++)
            {
                map[i, j] = content[i][j];
            }
        }
        return map;
    }
}